;;; main.lisp — ASP.NET Core controller in Lisp.
;;;
;;; Defines a Microsoft.AspNetCore.Mvc.ControllerBase subclass via
;;; dotnet:define-class. Program.cs's ApplicationPart adoption picks
;;; up the dynamically-emitted assembly and MVC's routing serves it.

(in-package :cl-user)

(format *error-output* "[main.lisp] loading in package ~S~%" *package*)

(require :dotnet-class)

;; Type aliases must take effect at compile time too: dotnet:define-class
;; resolves short names while macroexpanding, so plain SETF GETHASH would
;; only run at load time and the macros below would fail to look up
;; CONTROLLERBASE / IACTIONRESULT during compile-file. eval-when makes the
;; registration happen in both compile and load phases.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash "CONTROLLERBASE" dotnet::*type-aliases*)
        "Microsoft.AspNetCore.Mvc.ControllerBase")
  (setf (gethash "IACTIONRESULT" dotnet::*type-aliases*)
        "Microsoft.AspNetCore.Mvc.IActionResult"))

;; Minimal "hello" controller. Route attribute on the class makes
;; the action available at /api/hello (combined with HttpGet on the method).
;; HelloController: class-level [Route("api/hello")] sets the base URL,
;; method-level [HttpGet] picks up the verb. dotnet:define-class supports
;; :attributes both at class level (above) and per-method.
(dotnet:define-class "Demo.HelloController" (ControllerBase)
  (:attributes
    ("Microsoft.AspNetCore.Mvc.RouteAttribute" "api/hello"))
  (:methods
    ("Get" () :returns IActionResult
       :attributes (("Microsoft.AspNetCore.Mvc.HttpGetAttribute"))
      (dotnet:new "Microsoft.AspNetCore.Mvc.OkObjectResult" "hello from lisp"))))

(format *error-output* "[main.lisp] HelloController defined: ~S~%"
        (find-class 'demo.hello-controller nil))

;;; --- Async endpoint (Task producer side) -----------------------
;;; A Lisp async handler that returns a .NET Task<LispObject>. Program.cs maps it
;;; to a Minimal API route and awaits the Task, writing its result to the response.
;;; This exercises the Task-producing side of dotcl:async: an (async ...) block
;;; awaits a real .NET Task (Task.Delay) then yields a value, all off the request
;;; thread — no thread-per-request blocking.
(defun async-hello ()
  "Return a Task<LispObject> that completes (after a simulated async delay) with a
   greeting string. The C# host awaits it as the request handler's result."
  (dotcl:async
    (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 20))
    "hello from async lisp"))

(format *error-output* "[main.lisp] async-hello defined~%")
