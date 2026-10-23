;;; dotcl-lsp-coalton, as an editor uses it: a child dotcl loads Coalton and
;;; serves LSP on its standard input and output, and this file is the client.
;;; didOpen of a .lisp document with a type error gives one diagnostic whose
;;; range is in lines and UTF-16 columns (a Japanese character and a surrogate
;;; pair stand before the error on its line); fixing it with didChange gives
;;; none; hover shows the type of a function the document defines; a second
;;; document does not see that function, so the check did not leave it in the
;;; global environment; a .coal document is checked too.
;;;
;;; Coalton is found through ASDF, with the directory in DOTCL_TEST_COALTON
;;; added when that is set. The child reads Coalton's dependencies through the
;;; ASDF configuration it inherits (CL_SOURCE_REGISTRY). When the child finds
;;; no Coalton the tests are skipped and the run says so.

(require "dotcl-jsonrpc")
(require "dotcl-thread")

(defvar *lspc-id* 0)
(defvar *lspc-notes* '())
(defvar *lspc-in* nil "The server's standard output.")
(defvar *lspc-out* nil "The server's standard input.")

(defparameter *lspc-child-form*
  "(progn
     (let ((dir (dotcl:getenv \"DOTCL_TEST_COALTON\")))
       (when (and dir (plusp (length dir)))
         (push (uiop:ensure-directory-pathname dir) asdf:*central-registry*)))
     (unless (ignore-errors (asdf:find-system \"coalton\" nil))
       (dotcl:quit 3))
     (let ((*standard-output* *error-output*))
       (asdf:load-system \"coalton\"))
     (require \"dotcl-lsp-coalton\"))")

(defun %lspc-send (method params &key (id nil idp))
  (dotcl-jsonrpc:write-message
   *lspc-out* (list* :obj "jsonrpc" "2.0" "method" method "params" params
                 (when idp (list "id" id)))))

(defun %lspc-read ()
  (or (dotcl-jsonrpc:read-message *lspc-in*)
      (error "LSP test: the server closed its output")))

(defun %lspc-request (method params)
  (let ((id (incf *lspc-id*)))
    (%lspc-send method params :id id)
    (loop for m = (%lspc-read)
          do (if (eql (dotcl-jsonrpc:jget m "id") id)
                 (return (dotcl-jsonrpc:jget m "result"))
                 (setf *lspc-notes* (append *lspc-notes* (list m)))))))

(defun %lspc-diagnostics (uri)
  "The ranges and messages of the next publishDiagnostics for URI."
  (flet ((for-uri-p (m)
           (equal (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget m "params") "uri") uri)))
    (let ((m (or (find-if #'for-uri-p *lspc-notes*)
                 (loop for m = (%lspc-read)
                       when (for-uri-p m) do (return m)
                       else do (setf *lspc-notes* (append *lspc-notes* (list m)))))))
      (setf *lspc-notes* (remove m *lspc-notes*))
      (map 'list
           (lambda (d)
             (let ((r (dotcl-jsonrpc:jget d "range")))
               (flet ((p (key)
                        (let ((p (dotcl-jsonrpc:jget r key)))
                          (list (dotcl-jsonrpc:jget p "line")
                                (dotcl-jsonrpc:jget p "character")))))
                 (list (p "start") (p "end")
                       (dotcl-jsonrpc:jget d "severity")
                       (dotcl-jsonrpc:jget d "message")))))
           (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget m "params") "diagnostics")))))

(defun %lspc-open (uri text &optional (language "commonlisp"))
  (%lspc-send "textDocument/didOpen"
              (list :obj "textDocument"
                    (list :obj "uri" uri "languageId" language "version" 1 "text" text))))

(defun %lspc-hover (uri line character)
  (let ((h (%lspc-request "textDocument/hover"
                          (list :obj "textDocument" (list :obj "uri" uri)
                                "position" (list :obj "line" line "character" character)))))
    (and h (dotcl-jsonrpc:jget (dotcl-jsonrpc:jget h "contents") "value"))))

(defvar *lspc-wide*
  (coerce (list (code-char #x3042) (code-char #xD83D) (code-char #xDE00)) 'string))

(defun %lspc-text (argument)
  (format nil "(defpackage #:lsp-coalton-test (:use #:coalton #:coalton-prelude))
(in-package #:lsp-coalton-test)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare add1 (Integer -> Integer))
  (define (add1 x) (+ x 1)))

(coalton-toplevel
  (define (use-it) (let ((_s \"~A\")) (add1 ~A))))
" *lspc-wide* argument))

(defparameter *lspc-other-text*
  "(in-package #:lsp-coalton-test)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define (other) (add1 2)))
")

(defun %lspc-coal-text (call)
  (format nil "(package lsp-coalton-coal-test)

(declare twice ((:a -> :a) * :a -> :a))
(define (twice f x) (f (f x)))

(define (use-it) ~A)
" call))

(defun %lspc-watchdog (process seconds)
  "Kill PROCESS if it is still running after SECONDS, so a hang fails the test."
  (dotcl-thread:make-thread
   (lambda ()
     (loop repeat (* seconds 10)
           while (dotcl:process-alive-p process)
           do (sleep 0.1))
     (when (dotcl:process-alive-p process)
       (ignore-errors (dotcl:process-kill process))))
   :name "lsp-coalton-watchdog"))

(defun %lspc-scenario ()
  (let* ((log (regression-temp-file "lsp-coalton-stderr.txt"))
         (process (dotcl:launch-process
                   (dotnet:static "System.Environment" "ProcessPath")
                   (list "--core" (regression-child-core) "--eval" "(require \"asdf\")"
                         "--eval" *lspc-child-form*
                         "--eval" "(dotcl:quit (dotcl-lsp:serve-stdio))")
                   :element-type '(unsigned-byte 8)
                   :error log :if-error-exists :supersede))
         (*lspc-in* (dotcl:process-output process))
         (*lspc-out* (dotcl:process-input process))
         (result '()))
    (%lspc-watchdog process 600)
    (flet ((note (key value) (push (cons key value) result)))
      (handler-case
          (progn
            (%lspc-request "initialize" (list :obj "processId" nil "capabilities" '(:obj)))
            (%lspc-send "initialized" '(:obj))
            (%lspc-open "file:///a.lisp" (%lspc-text "_s"))
            (note :error (%lspc-diagnostics "file:///a.lisp"))
            (%lspc-send "textDocument/didChange"
                        (list :obj "textDocument" (list :obj "uri" "file:///a.lisp" "version" 2)
                              "contentChanges" (vector (list :obj "text" (%lspc-text "1")))))
            (note :fixed (%lspc-diagnostics "file:///a.lisp"))
            ;; Line 6 is "  (define (add1 x) (+ x 1)))".
            (note :hover-add1 (%lspc-hover "file:///a.lisp" 6 12))
            (note :hover-space (%lspc-hover "file:///a.lisp" 6 1))
            (%lspc-open "file:///b.lisp" *lspc-other-text*)
            (note :other (mapcar (lambda (d) (subseq d 0 3))
                                 (%lspc-diagnostics "file:///b.lisp")))
            (%lspc-open "file:///c.coal" (%lspc-coal-text "(twice \"x\" 2)") "coalton")
            (note :coal (mapcar (lambda (d) (subseq d 0 3))
                                (%lspc-diagnostics "file:///c.coal")))
            (%lspc-send "textDocument/didChange"
                        (list :obj "textDocument" (list :obj "uri" "file:///c.coal" "version" 2)
                              "contentChanges"
                              (vector (list :obj "text"
                                            (%lspc-coal-text "(twice (fn (x) x) 2)")))))
            (note :coal-fixed (%lspc-diagnostics "file:///c.coal"))
            (note :hover-coal (%lspc-hover "file:///c.coal" 3 9))
            (%lspc-request "shutdown" nil)
            (%lspc-send "exit" nil)
            (note :exit (dotcl:process-wait process)))
        (error (e)
          (ignore-errors (dotcl:process-kill process))
          (let ((code (ignore-errors (dotcl:process-wait process))))
            (note :failed (if (eql code 3) :no-coalton (princ-to-string e)))))))
    (ignore-errors (close *lspc-out*))
    (unless (equal (cdr (assoc :failed result)) :no-coalton)
      (when (assoc :failed result)
        (format t "~&;; dotcl-lsp-coalton: the server's standard error:~%~A~%"
                (ignore-errors
                 (with-open-file (s log)
                   (let ((text (make-string (file-length s))))
                     (subseq text 0 (read-sequence text s))))))))
    (nreverse result)))

(defvar *lspc-result* (%lspc-scenario))

(defun %lspc-get (key) (cdr (assoc key *lspc-result*)))

(if (eq (%lspc-get :failed) :no-coalton)
    (format t "~&;; dotcl-lsp-coalton: no Coalton found through ASDF (set DOTCL_TEST_COALTON), tests skipped~%")
    (progn
      (deftest dotcl-lsp-coalton.ran
        (%lspc-get :failed)
        nil)

      ;; Line 9: `  (define (use-it) (let ((_s "<a><pair>")) (add1 _s))))': the
      ;; argument _s is at 2 + 27 + 1 + 2 + 5 + 6 = 43 UTF-16 units.
      (deftest dotcl-lsp-coalton.type-error-range
        (mapcar (lambda (d) (list (first d) (second d) (third d)
                                  (and (search "Type mismatch" (fourth d)) t)))
                (%lspc-get :error))
        (((9 43) (9 45) 1 t)))

      (deftest dotcl-lsp-coalton.fixed
        (%lspc-get :fixed)
        nil)

      (deftest dotcl-lsp-coalton.hover
        (list (%lspc-get :hover-add1) (%lspc-get :hover-space))
        (#.(format nil "```coalton~%add1 :: Integer ~C Integer~%```" (code-char #x2192)) nil))

      (deftest dotcl-lsp-coalton.environment-put-back
        (%lspc-get :other)
        (((4 19) (4 23) 1)))

      (deftest dotcl-lsp-coalton.coal-document
        (list (%lspc-get :coal) (%lspc-get :coal-fixed)
              (and (search "twice ::" (or (%lspc-get :hover-coal) "")) t))
        ((((5 24) (5 27) 1)) nil t))

      (deftest dotcl-lsp-coalton.exit-code
        (%lspc-get :exit)
        0)))
