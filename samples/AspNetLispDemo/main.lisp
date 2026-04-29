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
