;;;; http-json.lisp -- calling a .NET library from Lisp.
;;;;
;;;; Run with:  dotcl examples/http-json.lisp [owner/repo]
;;;;
;;;; The other examples put Lisp *inside* a .NET host: a framework drives the
;;;; program and Lisp supplies the types. This one goes the other way. Lisp is
;;;; the program, and it reaches for .NET the way it would reach for a Lisp
;;;; library -- here to make an HTTP request and read the JSON that comes back.
;;;;
;;;;   - a NuGet package resolved at run time, used in the next form
;;;;   - an async .NET method awaited from ordinary Lisp code
;;;;   - IDisposable through dotnet:using, an indexer through dotnet:ref
;;;;
;;;; Needs a network connection: it downloads the package on first run and
;;;; queries api.github.com.

(require "dotnet-class")                ; dotnet:using and dotnet:ref live here
(require "nuget")                       ; NuGet resolution (ships with dotcl)

;;; Resolves the package and its dependencies, then registers the assemblies
;;; with dotcl's resolver -- after this the types are simply visible.
(nuget:require "Newtonsoft.Json")

(defparameter *repo*
  ;; command-line-arguments has the shape uiop expects -- ("dotcl" "--" args...)
  ;; -- so a script's own arguments are what follows the delimiter.
  (or (second (member "--" (dotcl:command-line-arguments) :test #'string=))
      "dotcl/dotcl"))

(defun fetch (url)
  "GET URL and return the response body as a string."
  ;; using disposes the client on the way out, including when the body signals.
  (dotnet:using ((client (dotnet:new "System.Net.Http.HttpClient")))
    ;; The GitHub API refuses a request that carries no User-Agent.
    (dotnet:-> client "DefaultRequestHeaders" ("Add" "User-Agent" "dotcl-example"))
    ;; GetStringAsync hands back a Task<string>. dotnet:await waits for it
    ;; without blocking a thread, and yields the string itself.
    (dotnet:await (dotnet:-> client ("GetStringAsync" url)))))

(defun parse (json)
  "Parse JSON text into a Newtonsoft JObject."
  (dotnet:static "Newtonsoft.Json.JsonConvert" "DeserializeObject" json))

;;; A JObject indexes by key, so dotnet:ref reads a field. An absent key comes
;;; back as NIL -- a .NET null arrives as Lisp NIL, so no special test is
;;; needed. The value itself is a JToken that still knows its own JSON type:
;;; ToString for text, ToObject for a number that should arrive as an integer.
(defun field (object name)
  (let ((token (dotnet:ref object name)))
    (when token
      (let ((text (dotnet:invoke token "ToString")))
        (unless (string= text "") text)))))       ; JSON null stringifies empty

(defun int-field (object name)
  (let ((token (dotnet:ref object name)))
    (when token
      (dotnet:invoke token "ToObject" (dotnet:resolve-type "System.Int32")))))

(handler-case
    (let ((repo (parse (fetch (format nil "https://api.github.com/repos/~a" *repo*)))))
      (format t "~&~a~%" (field repo "full_name"))
      (format t "  ~a~%" (or (field repo "description") "(no description)"))
      (format t "  language ~a, ~:d star~:p, ~:d open issue~:p~%"
              (or (field repo "language") "unknown")
              (or (int-field repo "stargazers_count") 0)
              (or (int-field repo "open_issues_count") 0)))
  (error (condition)
    (format *error-output* "~&Could not read ~a: ~a~%" *repo* condition)
    (dotcl:quit 1)))
