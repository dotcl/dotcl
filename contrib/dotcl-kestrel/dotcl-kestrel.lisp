;;; dotcl-kestrel.lisp -- an HTTP server for dotcl, on ASP.NET Core's Kestrel.
;;;
;;; Usage: (require "dotcl-kestrel")
;;;
;;;   (dotcl-kestrel:run (lambda (env)
;;;                        (declare (ignore env))
;;;                        (list 200 (list :content-type "text/plain") (list "hello")))
;;;                      :port 5000)
;;;
;;; The application is a function of one argument, the request as a property
;;; list, returning (status headers body) -- the Lack/Clack shape, so a Lack
;;; application runs here unchanged. BODY is a list of strings, a single string,
;;; a pathname, or a CL input stream.
;;;
;;; The request body is read before the application is called and the response is
;;; written after it returns, both through .NET's asynchronous API. Kestrel
;;; refuses synchronous stream operations by default and it is right to: a
;;; handler waiting on a socket holds a thread pool thread. Because a Lack
;;; application hands back its body as a value rather than writing as it goes,
;;; neither direction has to happen while the application runs, so the
;;; prohibition never comes up. :raw-body is therefore a stream over bytes
;;; already in memory. Pass :allow-synchronous-io t to hand over Kestrel's own
;;; stream instead, which an application streaming a large upload will want.

(defpackage :dotcl-kestrel
  (:use :cl)
  (:export #:run
           #:stop))

(in-package :dotcl-kestrel)

(dotnet:load-assembly "Microsoft.AspNetCore")

(defvar *header-dictionary* "Microsoft.AspNetCore.Http.IHeaderDictionary"
  "Headers are reached through this interface: the concrete types implement it
and IDictionary separately, so the one meant has to be named.")

(defun %headers (collection)
  (dotnet:cast collection *header-dictionary*))

(defun %header-value (headers name)
  "The value of NAME as a string, or NIL when the header is absent."
  (let ((value (dotnet:invoke headers "Item" name)))
    (when value
      (let ((text (dotnet:invoke value "ToString")))
        (unless (string= text "") text)))))

(defun %request-headers (request)
  "A hash table of every request header, keyed by lower-case name."
  (let ((table (make-hash-table :test #'equal))
        (iterator (dotnet:invoke (dotnet:invoke request "Headers") "GetEnumerator")))
    (loop while (dotnet:invoke iterator "MoveNext")
          do (let ((pair (dotnet:invoke iterator "Current")))
               (setf (gethash (string-downcase (dotnet:invoke pair "Key")) table)
                     (dotnet:invoke (dotnet:invoke pair "Value") "ToString"))))
    table))

(defun %keyword (string)
  (intern (string-upcase string) :keyword))

(defun %read-body (context allow-synchronous-io)
  "The request body as a CL input stream of bytes.

Copied into memory first, so the application reads without touching Kestrel's
stream. With ALLOW-SYNCHRONOUS-IO that stream is handed over instead."
  (let ((body (dotnet:invoke (dotnet:invoke context "Request") "Body")))
    (cond
      (allow-synchronous-io
       (let ((feature (dotnet:invoke (dotnet:invoke context "Features") "Item"
                                     (dotnet:resolve-type
                                      "Microsoft.AspNetCore.Http.Features.IHttpBodyControlFeature"))))
         (when feature
           (setf (dotnet:invoke feature "AllowSynchronousIO") t)))
       (dotnet:to-stream body :binary t))
      (t
       (let ((buffer (dotnet:new "System.IO.MemoryStream")))
         (dotnet:await (dotnet:invoke body "CopyToAsync" buffer))
         (setf (dotnet:invoke buffer "Position") 0)
         (dotnet:to-stream buffer :binary t))))))

(defun %environment (context allow-synchronous-io)
  "The request as a Lack environment plist."
  (let* ((request (dotnet:invoke context "Request"))
         (connection (dotnet:invoke context "Connection"))
         (headers (%headers (dotnet:invoke request "Headers")))
         (path (dotnet:invoke (dotnet:invoke request "Path") "ToString"))
         (query (dotnet:invoke (dotnet:invoke request "QueryString") "ToString"))
         (length (%header-value headers "content-length"))
         (address (dotnet:invoke connection "RemoteIpAddress")))
    (list :request-method (%keyword (dotnet:invoke request "Method"))
          :script-name ""
          :path-info path
          :query-string (if (and (plusp (length query)) (char= (char query 0) #\?))
                            (subseq query 1)
                            query)
          :request-uri (concatenate 'string path query)
          :server-protocol (%keyword (dotnet:invoke request "Protocol"))
          :url-scheme (%keyword (dotnet:invoke request "Scheme"))
          :server-name (or (%header-value headers "host") "")
          :server-port (or (dotnet:invoke (dotnet:invoke request "Host") "Port") 0)
          :remote-addr (if address (dotnet:invoke address "ToString") "")
          :remote-port (dotnet:invoke connection "RemotePort")
          :content-type (%header-value headers "content-type")
          :content-length (when length (parse-integer length :junk-allowed t))
          :headers (%request-headers request)
          :raw-body (%read-body context allow-synchronous-io))))

(defun %write-headers (response headers)
  "Apply a Lack header plist. A name given twice accumulates, as Set-Cookie needs."
  (let ((collection (%headers (dotnet:invoke response "Headers"))))
    (loop for (name value) on headers by #'cddr
          for text = (string-downcase (string name))
          do (if (string= text "content-type")
                 (setf (dotnet:invoke response "ContentType") value)
                 (let ((existing (%header-value collection text)))
                   (setf (dotnet:invoke collection "Item" text)
                         (if existing
                             (concatenate 'string existing "," value)
                             value)))))))

(defun %write-body (response body)
  "Send BODY: a list of strings, a single string, a pathname, or a CL stream."
  (etypecase body
    (null nil)
    (string (dotnet:await (dotnet:invoke response "WriteAsync" body)))
    (cons (dolist (chunk body)
            (dotnet:await (dotnet:invoke response "WriteAsync" chunk))))
    (pathname
     (dotnet:load-assembly "Microsoft.AspNetCore.Http.Extensions")
     (dotnet:await (dotnet:static "Microsoft.AspNetCore.Http.SendFileResponseExtensions"
                                  "SendFileAsync" response (namestring (truename body)))))
    (stream
     (loop for line = (read-line body nil nil)
           while line
           do (dotnet:await (dotnet:invoke response "WriteAsync" line))))))

(defun %respond (context result)
  (destructuring-bind (status headers body) result
    (let ((response (dotnet:invoke context "Response")))
      (setf (dotnet:invoke response "StatusCode") status)
      (%write-headers response headers)
      (%write-body response body))))

(defun %handle (context application allow-synchronous-io)
  (%respond context (funcall application (%environment context allow-synchronous-io)))
  (dotnet:static "System.Threading.Tasks.Task" "CompletedTask"))

(defun run (application &key (port 5000) (address "127.0.0.1") allow-synchronous-io)
  "Serve APPLICATION on ADDRESS:PORT and return the server, which STOP takes.

APPLICATION takes a Lack environment plist and returns (status headers body).
Returns once the server is listening; requests are served on their own threads."
  (let* ((builder (dotnet:static "Microsoft.AspNetCore.Builder.WebApplication" "CreateBuilder"))
         (server (dotnet:invoke builder "Build")))
    (dotnet:invoke (dotnet:invoke server "Urls") "Add"
                   (format nil "http://~a:~a" address port))
    (dotnet:static "Microsoft.AspNetCore.Builder.RunExtensions" "Run" server
                   (dotnet:make-delegate "Microsoft.AspNetCore.Http.RequestDelegate"
                                         (lambda (context)
                                           (%handle context application allow-synchronous-io))))
    (dotnet:await (dotnet:invoke server "StartAsync"))
    server))

(defun stop (server)
  "Stop SERVER and wait for it to finish."
  (dotnet:await (dotnet:invoke server "StopAsync"))
  t)

(provide "dotcl-kestrel")
