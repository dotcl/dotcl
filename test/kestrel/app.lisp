;;; What the dotcl-kestrel contrib has to do: serve a Lack application.
;;;
;;; Kept out of the regression suite because it needs the ASP.NET Core shared
;;; framework, which the suite otherwise never touches. Run it with
;;; `make test-kestrel`.
;;;
;;; Prints one line per case and a final PASS/FAIL count, which check.sh greps.

(require "dotcl-kestrel")

(defvar *port* 8086)
(defvar *checks* 0)
(defvar *failures* 0)

(defun check (label expected actual)
  (incf *checks*)
  (cond ((equal expected actual) (format t "~&ok    ~a~%" label))
        (t (incf *failures*)
           (format t "~&FAIL  ~a~%  expected ~s~%  got      ~s~%" label expected actual))))

(defvar *application*
  (lambda (env)
    (let ((path (getf env :path-info)))
      (cond
        ;; The request as the application sees it.
        ((string= path "/env")
         (list 200 (list :content-type "text/plain")
               (list (format nil "~a|~a|~a|~a|~a"
                             (getf env :request-method)
                             (getf env :path-info)
                             (getf env :query-string)
                             (getf env :server-protocol)
                             (gethash "user-agent" (getf env :headers))))))
        ;; The body, read back through :raw-body.
        ((string= path "/echo")
         (let* ((bytes (make-array 256 :element-type '(unsigned-byte 8)))
                (n (read-sequence bytes (getf env :raw-body))))
           (list 200 (list :content-type "text/plain")
                 (list (map 'string #'code-char (subseq bytes 0 n))))))
        ;; A status other than 200, and a header the application sets.
        ((string= path "/missing")
         (list 404 (list :content-type "text/plain" :x-reason "gone") (list "nope")))
        ;; A single string rather than a list.
        ((string= path "/string") (list 200 (list :content-type "text/plain") "plain"))
        (t (list 200 (list :content-type "text/plain") (list "one " "two " "three")))))))

(defvar *server* (dotcl-kestrel:run *application* :port *port*))
(defvar *client* (dotnet:new "System.Net.Http.HttpClient"))
(dotnet:invoke (dotnet:invoke *client* "DefaultRequestHeaders") "Add" "User-Agent" "kestrel-check")

(defun url (path) (format nil "http://127.0.0.1:~a~a" *port* path))

(defun response-text (response)
  (dotnet:await (dotnet:invoke (dotnet:invoke response "Content") "ReadAsStringAsync")))

(defun status (response)
  (dotnet:invoke (dotnet:invoke response "StatusCode") "GetHashCode"))

(defun header (response name)
  (handler-case
      (let ((values (dotnet:invoke (dotnet:invoke response "Headers") "GetValues" name)))
        (dotnet:invoke (dotnet:static "System.Linq.Enumerable" "First" values) "ToString"))
    (error () nil)))

(defun fetch (path)
  (dotnet:await (dotnet:invoke *client* "GetAsync" (url path))))

(let ((r (fetch "/")))
  (check "body as a list of strings" "one two three" (response-text r))
  (check "status 200" 200 (status r)))

(let ((r (fetch "/string")))
  (check "body as a single string" "plain" (response-text r)))

(let ((r (fetch "/env?a=1&b=2")))
  (check "request as the application sees it"
         "GET|/env|a=1&b=2|HTTP/1.1|kestrel-check"
         (response-text r)))

(let ((r (fetch "/missing")))
  (check "application status" 404 (status r))
  (check "application header" "gone" (header r "X-Reason")))

(let ((r (dotnet:await (dotnet:invoke *client* "PostAsync" (url "/echo")
                                      (dotnet:new "System.Net.Http.StringContent"
                                                  "body through raw-body")))))
  (check "request body" "body through raw-body" (response-text r)))

;;; A Lack application, built the way Lack applications are built. LACK does not
;;; exist when this file is read, so the form is made at run time.
(require "quicklisp")
(funcall (find-symbol "QUICKLOAD" "QL") "lack" :silent t)

(defvar *lack-application*
  (eval (read-from-string
         "(lack:builder
            (lambda (app)
              (lambda (env)
                (let ((response (funcall app env)))
                  (list (first response)
                        (append (second response) (list :x-middleware \"ran\"))
                        (third response)))))
            (lambda (env)
              (list 200 (list :content-type \"text/plain\")
                    (list (format nil \"lack ~a ~a\"
                                  (getf env :request-method) (getf env :path-info))))))")))

(dotcl-kestrel:stop *server*)
(setf *server* (dotcl-kestrel:run *lack-application* :port *port*))

(let ((r (fetch "/hello")))
  (check "lack:builder application" "lack GET /hello" (response-text r))
  (check "lack middleware" "ran" (header r "X-Middleware")))

(dotcl-kestrel:stop *server*)

(format t "~&~a checks, ~a failed~%" *checks* *failures*)
(format t "~&~a~%" (if (zerop *failures*) "KESTREL-OK" "KESTREL-FAILED"))
(dotcl:quit (if (zerop *failures*) 0 1))
