;;; The dotcl-lsp contrib, driven by an LSP client over loopback: initialize,
;;; didOpen / didChange / didClose with publishDiagnostics, hover, an unknown
;;; request, shutdown and exit. No Coalton here (dotcl-lsp-coalton.lisp has
;;; that); a diagnostic function defined below stands in for a checker, so the
;;; ranges on the wire are checked here: lines, and columns in UTF-16 units
;;; after a BMP character and a surrogate pair.
;;;
;;; Every read has a socket receive timeout, so a regression fails the test
;;; instead of hanging the suite.

(require "dotcl-lsp")
(require "dotcl-bsd-sockets")

(defvar *lsp-test-id* 0)
(defvar *lsp-test-notes* '())

(defun %lsp-send (stream method params &key (id nil idp))
  (dotcl-jsonrpc:write-message
   stream (list* :obj "jsonrpc" "2.0" "method" method "params" params
                 (when idp (list "id" id)))))

(defun %lsp-read (stream)
  (or (dotcl-jsonrpc:read-message stream)
      (error "LSP test: connection closed")))

(defun %lsp-request (stream method params)
  "Send a request and return its response; notifications that arrive first are kept."
  (let ((id (incf *lsp-test-id*)))
    (%lsp-send stream method params :id id)
    (loop for m = (%lsp-read stream)
          do (if (eql (dotcl-jsonrpc:jget m "id") id)
                 (return m)
                 (setf *lsp-test-notes* (append *lsp-test-notes* (list m)))))))

(defun %lsp-diagnostics (stream uri)
  "The next publishDiagnostics for URI: its version and its diagnostics."
  (let ((m (or (find-if (lambda (m)
                          (equal (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget m "params") "uri") uri))
                        *lsp-test-notes*)
               (loop for m = (%lsp-read stream)
                     when (equal (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget m "params") "uri") uri)
                       do (return m)
                     else do (setf *lsp-test-notes* (append *lsp-test-notes* (list m)))))))
    (setf *lsp-test-notes* (remove m *lsp-test-notes*))
    (let ((params (dotcl-jsonrpc:jget m "params")))
      (list (dotcl-jsonrpc:jget m "method")
            (dotcl-jsonrpc:jget params "version")
            (coerce (dotcl-jsonrpc:jget params "diagnostics") 'list)))))

(defun %lsp-range (diagnostic)
  (let ((r (dotcl-jsonrpc:jget diagnostic "range")))
    (flet ((p (key)
             (let ((p (dotcl-jsonrpc:jget r key)))
               (list (dotcl-jsonrpc:jget p "line") (dotcl-jsonrpc:jget p "character")))))
      (list (p "start") (p "end")))))

(defun %lsp-doc (uri text &optional (language "commonlisp") (version 1))
  (list :obj "textDocument"
        (list :obj "uri" uri "languageId" language "version" version "text" text)))

(defun %lsp-position (uri line character)
  (list :obj "textDocument" (list :obj "uri" uri)
        "position" (list :obj "line" line "character" character)))

;;; Stands in for a checker: every BAD in a "test-lsp" document is an error.
(defun %lsp-test-bad-tokens (document)
  (when (search "test-lsp" (dotcl-lsp:document-uri document))
    (let ((text (dotcl-lsp:document-text document)))
      (or (loop for start = (search "BAD" text) then (search "BAD" text :start2 (1+ start))
                while start
                collect (list :start start :end (+ start 3) :severity :warning
                              :message "bad token"))
          :none))))

;;; A Japanese character and a surrogate pair before the token, on one line,
;;; behind a CR LF line end and a lone CR one.
(defvar *lsp-test-wide*
  (coerce (list (code-char #x3042) (code-char #xD83D) (code-char #xDE00)) 'string))

(defvar *lsp-test-text*
  (format nil "(car '(1))~C~C;; ~A BAD~C(cdr '(2))" #\Return #\Newline *lsp-test-wide* #\Return))

(defun %lsp-scenario ()
  ;; Set globally, not bound: the server answers on a thread of its own.
  (push '%lsp-test-bad-tokens dotcl-lsp:*diagnostic-functions*)
  (let* ((server (dotcl-lsp:start-server :port 0))
         (socket (dotcl-bsd-sockets:tcp-connect "127.0.0.1" (dotcl-lsp:server-port server)
                                                :timeout 5))
         (stream (progn
                   (setf (dotcl-bsd-sockets:sockopt-receive-timeout socket) 10)
                   (dotcl-bsd-sockets:socket-make-stream
                    socket :element-type '(unsigned-byte 8))))
         (out '()))
    (flet ((note (key value) (push (cons key value) out))
           (result (m) (dotcl-jsonrpc:jget m "result")))
      (unwind-protect
           (let* ((init (result (%lsp-request stream "initialize"
                                              (list :obj "processId" nil "capabilities" '(:obj)))))
                  (caps (dotcl-jsonrpc:jget init "capabilities")))
             (note :initialize
                   (list (dotcl-jsonrpc:jget caps "hoverProvider")
                         (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget caps "textDocumentSync") "change")
                         (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget init "serverInfo") "name")))
             (%lsp-send stream "initialized" '(:obj))
             ;; A plain Lisp document: nothing to report.
             (%lsp-send stream "textDocument/didOpen"
                        (%lsp-doc "file:///plain.lisp" "(defun f (x) (car x))"))
             (note :plain (%lsp-diagnostics stream "file:///plain.lisp"))
             (note :hover-car
                   (let ((h (result (%lsp-request stream "textDocument/hover"
                                                  (%lsp-position "file:///plain.lisp" 0 15)))))
                     (and h (search "CAR" (string-upcase
                                           (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget h "contents")
                                                               "value")))
                          t)))
             (note :hover-nothing
                   (result (%lsp-request stream "textDocument/hover"
                                         (%lsp-position "file:///plain.lisp" 0 12))))
             ;; The stand-in checker: the range is in UTF-16 units.
             (%lsp-send stream "textDocument/didOpen"
                        (%lsp-doc "file:///test-lsp.lisp" *lsp-test-text*))
             (destructuring-bind (method version diagnostics)
                 (%lsp-diagnostics stream "file:///test-lsp.lisp")
               (note :wide (list method version (length diagnostics)
                                 (%lsp-range (first diagnostics))
                                 (dotcl-jsonrpc:jget (first diagnostics) "severity")
                                 (dotcl-jsonrpc:jget (first diagnostics) "message"))))
             ;; An incremental change after the surrogate pair, then one on the last line.
             (%lsp-send stream "textDocument/didChange"
                        (list :obj "textDocument" (list :obj "uri" "file:///test-lsp.lisp"
                                                        "version" 2)
                              "contentChanges"
                              (vector (list :obj "range"
                                            (list :obj "start" (list :obj "line" 1 "character" 6)
                                                  "end" (list :obj "line" 1 "character" 6))
                                            "text" " X BAD")
                                      (list :obj "range"
                                            (list :obj "start" (list :obj "line" 2 "character" 0)
                                                  "end" (list :obj "line" 2 "character" 0))
                                            "text" "BAD "))))
             (destructuring-bind (method version diagnostics)
                 (%lsp-diagnostics stream "file:///test-lsp.lisp")
               (declare (ignore method))
               (note :changed (list version (mapcar #'%lsp-range diagnostics))))
             ;; A whole-text change that removes it.
             (%lsp-send stream "textDocument/didChange"
                        (list :obj "textDocument" (list :obj "uri" "file:///test-lsp.lisp"
                                                        "version" 3)
                              "contentChanges" (vector (list :obj "text" "(fixed)"))))
             (note :fixed (cdr (%lsp-diagnostics stream "file:///test-lsp.lisp")))
             (%lsp-send stream "textDocument/didClose"
                        (list :obj "textDocument" (list :obj "uri" "file:///test-lsp.lisp")))
             (note :closed (third (%lsp-diagnostics stream "file:///test-lsp.lisp")))
             (note :unknown
                   (let ((m (%lsp-request stream "textDocument/noSuchThing" '(:obj))))
                     (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget m "error") "code")))
             (let ((m (%lsp-request stream "shutdown" nil)))
               (note :shutdown (list (dotcl-jsonrpc:jget m "result" :absent)
                                     (dotcl-jsonrpc:jget m "error"))))
             (%lsp-send stream "exit" nil)
             (note :exit (handler-case (dotcl-jsonrpc:read-message stream)
                           (error () :error))))
        (ignore-errors (close stream))
        (dotcl-lsp:stop-server server)
        (setf dotcl-lsp:*diagnostic-functions*
              (remove '%lsp-test-bad-tokens dotcl-lsp:*diagnostic-functions*))))
    (nreverse out)))

(defvar *lsp-result* (%lsp-scenario))

(defun %lsp-get (key) (cdr (assoc key *lsp-result*)))

(deftest dotcl-lsp.initialize
  (%lsp-get :initialize)
  (t 2 "dotcl-lsp"))

(deftest dotcl-lsp.plain-lisp-has-no-diagnostics
  (%lsp-get :plain)
  ("textDocument/publishDiagnostics" 1 nil))

(deftest dotcl-lsp.hover
  (list (%lsp-get :hover-car) (%lsp-get :hover-nothing))
  (t nil))

;;; Line 1 is ";; <a><pair> BAD": the token starts after 3 + 1 + 2 + 1 units.
(deftest dotcl-lsp.diagnostic-range-utf16
  (%lsp-get :wide)
  ("textDocument/publishDiagnostics" 1 1 ((1 7) (1 10)) 2 "bad token"))

(deftest dotcl-lsp.incremental-change
  (%lsp-get :changed)
  (2 (((1 9) (1 12)) ((1 13) (1 16)) ((2 0) (2 3)))))

(deftest dotcl-lsp.fixed-and-closed
  (list (%lsp-get :fixed) (%lsp-get :closed))
  ((3 nil) nil))

(deftest dotcl-lsp.unknown-method
  (%lsp-get :unknown)
  -32601)

(deftest dotcl-lsp.shutdown-exit
  (list (%lsp-get :shutdown) (%lsp-get :exit))
  ((nil nil) nil))

;;; Offsets and positions in UTF-16 units, both ways.
(deftest dotcl-lsp.offset-position
  (let ((d (dotcl-lsp::make-document "u" "commonlisp" 1 *lsp-test-text*)))
    (list (multiple-value-list (dotcl-lsp:offset-position d 0))
          (multiple-value-list (dotcl-lsp:offset-position d (search "BAD" *lsp-test-text*)))
          (= (dotcl-lsp:position-offset d 1 7) (search "BAD" *lsp-test-text*))
          (multiple-value-list (dotcl-lsp:offset-position d (search "(cdr" *lsp-test-text*)))
          (= (dotcl-lsp:position-offset d 2 0) (search "(cdr" *lsp-test-text*))
          (= (dotcl-lsp:position-offset d 9 0) (length *lsp-test-text*))))
  ((0 0) (1 7) t (2 0) t t))
