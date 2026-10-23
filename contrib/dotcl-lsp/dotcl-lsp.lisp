;;; dotcl-lsp.lisp: a Language Server Protocol server inside a running dotcl.
;;;
;;;   dotcl --eval '(require "dotcl-lsp")' --eval '(dotcl:quit (dotcl-lsp:serve-stdio))'
;;;
;;; or, from a REPL, (dotcl-lsp:start-server :port 0) for a client over TCP.
;;; Documents are kept in memory as the client sends them; nothing is loaded
;;; or written. Diagnostics and hover come from the functions on
;;; *DIAGNOSTIC-FUNCTIONS* and *HOVER-FUNCTIONS*, so another contrib can add
;;; a language (dotcl-lsp-coalton does).

(require "dotcl-thread")
(require "dotcl-socket")
(require "dotcl-jsonrpc")
(require "dotcl-code-info")

(defpackage :dotcl-lsp
  (:use :cl :dotcl-jsonrpc)
  (:export #:serve-stdio #:serve-stream #:start-server #:stop-server #:server-port #:*server*
           #:*diagnostic-functions* #:*hover-functions*
           #:document #:document-uri #:document-language-id #:document-version
           #:document-text #:document-property
           #:offset-position #:position-offset))

(in-package :dotcl-lsp)

;;; Documents. Positions are LSP's: a zero-based line and a column counted in
;;; UTF-16 code units. A line ends at LF, CR LF or CR.

(defstruct (document (:constructor %make-document))
  uri language-id version text line-starts (properties '()))

(defun %line-starts (text)
  (let ((starts (list 0)) (len (length text)))
    (loop for i from 0 below len
          for c = (char text i)
          do (cond ((char= c #\Newline) (push (1+ i) starts))
                   ((char= c #\Return)
                    (unless (and (< (1+ i) len) (char= (char text (1+ i)) #\Newline))
                      (push (1+ i) starts)))))
    (coerce (nreverse starts) 'vector)))

(defun make-document (uri language-id version text)
  (%make-document :uri uri :language-id language-id :version version :text text
                  :line-starts (%line-starts text)))

(defun %set-text (document text)
  (setf (document-text document) text
        (document-line-starts document) (%line-starts text)))

(defun document-property (document key &optional default)
  (getf (document-properties document) key default))

(defun (setf document-property) (value document key)
  (setf (getf (document-properties document) key) value))

(defun %units (char)
  (if (> (char-code char) #xFFFF) 2 1))

(defun offset-position (document offset)
  "The (values line character) of the character OFFSET in DOCUMENT."
  (let* ((starts (document-line-starts document))
         (text (document-text document))
         (offset (max 0 (min offset (length text))))
         (line (or (position-if (lambda (s) (<= s offset)) starts :from-end t) 0)))
    (values line
            (loop for i from (aref starts line) below offset
                  sum (%units (char text i))))))

(defun position-offset (document line character)
  "The character offset of LSP position LINE / CHARACTER in DOCUMENT."
  (let* ((starts (document-line-starts document))
         (text (document-text document)))
    (if (>= line (length starts))
        (length text)
        (let ((end (if (< (1+ line) (length starts)) (aref starts (1+ line)) (length text)))
              (units 0))
          (loop for i from (aref starts (max line 0)) below end
                do (when (>= units character) (return i))
                   (incf units (%units (char text i)))
                finally (return end))))))

(defun %range (document start end)
  (flet ((pos (offset)
           (multiple-value-bind (line character) (offset-position document offset)
             (list :obj "line" line "character" character))))
    (list :obj "start" (pos start) "end" (pos end))))

(defun %apply-change (document change)
  (let ((range (jget change "range"))
        (new (jget change "text")))
    (if range
        (flet ((offset (p) (position-offset document (jget p "line") (jget p "character"))))
          (let ((start (offset (jget range "start")))
                (end (offset (jget range "end")))
                (text (document-text document)))
            (%set-text document (concatenate 'string (subseq text 0 start) new
                                             (subseq text end)))))
        (%set-text document new))))

;;; Providers.

(defvar *diagnostic-functions* '()
  "Functions of a document returning its diagnostics, or NIL when the document
is not theirs. A diagnostic is a plist: :start and :end character offsets,
:severity (:error :warning :information :hint), :message, :source, and
:related, a list of (start end message) in the same document. The first
function that returns a non-NIL list, or :NONE for none, decides.")

(defvar *hover-functions* '()
  "Functions of a document and a character offset returning Markdown to show,
or NIL. The first non-NIL answer is shown. The image's own description of a
Lisp name is tried after these.")

(defun %severity-code (severity)
  (case severity (:warning 2) (:information 3) (:hint 4) (t 1)))

(defun %diagnostics (document)
  (let ((found (dolist (f *diagnostic-functions* '())
                 (let ((r (funcall f document)))
                   (when r (return (if (eq r :none) '() r)))))))
    (map 'vector
         (lambda (d)
           (let ((range (%range document (getf d :start) (getf d :end))))
             (list* :obj "range" range
                    "severity" (%severity-code (getf d :severity))
                    "source" (or (getf d :source) "dotcl")
                    "message" (getf d :message)
                    (when (getf d :related)
                      (list "relatedInformation"
                            (map 'vector
                                 (lambda (r)
                                   (destructuring-bind (start end message) r
                                     (list :obj "location"
                                           (list :obj "uri" (document-uri document)
                                                      "range" (%range document start end))
                                           "message" message)))
                                 (getf d :related)))))))
         found)))

(defun %describe-markdown (document offset)
  (let ((d (ignore-errors (dotcl-code-info:describe-at (document-text document) offset))))
    (when d
      (with-output-to-string (out)
        (format out "```lisp~%~{~A~^~%~}~%```" (or (getf d :signatures) (list (getf d :name))))
        (when (getf d :documentation)
          (format out "~%~%~A" (getf d :documentation)))
        (when (getf d :url)
          (format out "~%~%~A" (getf d :url)))))))

(defun %hover (document offset)
  (or (dolist (f *hover-functions*)
        (let ((r (ignore-errors (funcall f document offset))))
          (when r (return r))))
      (%describe-markdown document offset)))

;;; Sessions: one client, one stream pair.

(defstruct session
  input output
  (documents (make-hash-table :test 'equal))
  (lock (dotcl-thread:make-lock "lsp-session"))
  (shutdown nil)
  (exit nil)
  (open t))

(defun %send (session message)
  (dotcl-thread:with-lock-held ((session-lock session))
    (when (session-open session)
      (handler-case (write-message (session-output session) message)
        (error () (setf (session-open session) nil))))))

(defun %notify (session method params)
  (%send session (list :obj "jsonrpc" "2.0" "method" method "params" params)))

(defun %publish (session document)
  (let ((diagnostics (handler-case (%diagnostics document)
                       (error (e)
                         (vector (list :obj "range" (%range document 0 0) "severity" 1
                                       "source" "dotcl-lsp"
                                       "message" (format nil "checking failed: ~A" e)))))))
    (%notify session "textDocument/publishDiagnostics"
             (list :obj "uri" (document-uri document)
                   "version" (document-version document)
                   "diagnostics" diagnostics))))

(defun %capabilities ()
  (list :obj
        "textDocumentSync" (list :obj "openClose" t "change" 2)
        "hoverProvider" t))

(define-condition method-not-found (error)
  ((name :initarg :name :reader method-not-found-method))
  (:report (lambda (c s) (format s "unsupported method ~A" (method-not-found-method c)))))

(defun handle (session method params)
  "Answer METHOD. Returns the result for a request; raises for an error."
  (let ((documents (session-documents session)))
    (flet ((doc ()
             (gethash (jget (jget params "textDocument") "uri") documents)))
      (cond
        ((string= method "initialize")
         (list :obj "capabilities" (%capabilities)
               "serverInfo" (list :obj "name" "dotcl-lsp" "version" "0.1")))
        ((string= method "initialized") nil)
        ((string= method "shutdown") (setf (session-shutdown session) t) nil)
        ((string= method "exit") (setf (session-exit session) t) nil)
        ((string= method "textDocument/didOpen")
         (let* ((td (jget params "textDocument"))
                (document (make-document (jget td "uri") (jget td "languageId")
                                         (jget td "version") (jget td "text"))))
           (setf (gethash (document-uri document) documents) document)
           (%publish session document)
           nil))
        ((string= method "textDocument/didChange")
         (let ((document (doc)))
           (when document
             (map nil (lambda (c) (%apply-change document c))
                  (jget params "contentChanges" #()))
             (setf (document-version document) (jget (jget params "textDocument") "version"))
             (%publish session document))
           nil))
        ((string= method "textDocument/didSave") nil)
        ((string= method "textDocument/didClose")
         (let ((document (doc)))
           (when document
             (remhash (document-uri document) documents)
             (%notify session "textDocument/publishDiagnostics"
                      (list :obj "uri" (document-uri document) "diagnostics" #()))))
         nil)
        ((string= method "textDocument/hover")
         (let ((document (doc)))
           (when document
             (let* ((p (jget params "position"))
                    (text (%hover document (position-offset document (jget p "line")
                                                            (jget p "character")))))
               (when text
                 (list :obj "contents" (list :obj "kind" "markdown" "value" text)))))))
        (t (error 'method-not-found :name method))))))

(defun %dispatch (session message)
  (let ((id (jget message "id" :none))
        (method (jget message "method")))
    (cond
      ((null method) nil)
      ((eq id :none)
       (ignore-errors (handle session method (jget message "params" '(:obj)))))
      ((and (session-shutdown session) (string/= method "shutdown"))
       (%send session (list :obj "jsonrpc" "2.0" "id" id
                            "error" (list :obj "code" -32600 "message" "shut down"))))
      (t
       (handler-case
           (let ((result (handle session method (jget message "params" '(:obj)))))
             (%send session (list :obj "jsonrpc" "2.0" "id" id "result" result)))
         (method-not-found (e)
           (%send session (list :obj "jsonrpc" "2.0" "id" id
                                "error" (list :obj "code" -32601
                                              "message" (princ-to-string e)))))
         (error (e)
           (%send session (list :obj "jsonrpc" "2.0" "id" id
                                "error" (list :obj "code" -32603
                                              "message" (princ-to-string e))))))))))

(defun serve-stream (input output)
  "Serve one client on the binary streams INPUT and OUTPUT until it sends exit
or the input ends. Returns the exit code the protocol asks for: 0 after
shutdown, else 1."
  (let ((session (make-session :input input :output output)))
    (loop until (or (session-exit session) (not (session-open session)))
          do (let ((message (handler-case (read-message input)
                              (error () nil))))
               (unless message (return))
               (%dispatch session message)))
    (setf (session-open session) nil)
    (if (session-shutdown session) 0 1)))

(defun serve-stdio ()
  "Serve the client on standard input and output, as an editor that starts the
server as a child process expects. What Lisp code prints goes to standard
error meanwhile, so it cannot break the protocol."
  (let ((input (dotnet:to-stream (dotnet:static "System.Console" "OpenStandardInput")
                                 :binary t))
        (output (dotnet:to-stream (dotnet:static "System.Console" "OpenStandardOutput")
                                  :binary t))
        (*standard-output* *error-output*)
        (*trace-output* *error-output*))
    (serve-stream input output)))

;;; TCP, one client at a time, in a background thread.

(defstruct server socket thread port)

(defvar *server* nil)

(defun start-server (&key (host "127.0.0.1") (port 0))
  "Listen for an LSP client on HOST:PORT (0 picks a free port) and serve one
connection at a time in a background thread. SERVER-PORT tells the port."
  (when *server* (stop-server))
  (let* ((socket (dotcl-socket:make-server-socket host port))
         (server (make-server :socket socket :port (dotcl-socket:local-port socket))))
    (setf (server-thread server)
          (dotcl-thread:make-thread
           (lambda ()
             (loop
               (let ((stream (handler-case (dotcl-socket:socket-accept socket :binary t)
                               (error () (return)))))
                 (unwind-protect (serve-stream stream stream)
                   (ignore-errors (close stream))))))
           :name "lsp-server"))
    (setf *server* server)))

(defun stop-server (&optional (server *server*))
  (when server
    (dotcl-socket:socket-close (server-socket server))
    (when (eq server *server*) (setf *server* nil))
    t))

(provide "dotcl-lsp")
