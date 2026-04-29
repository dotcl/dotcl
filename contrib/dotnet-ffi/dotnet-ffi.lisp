;;; dotnet-ffi.lisp — dotnet:define-ffi macro
;;; Wraps dotnet:%ffi-call for declarative native function bindings.
;;;
;;; Usage:
;;;   (require "dotnet-ffi")
;;;   (dotnet:define-ffi set-console-mode "kernel32.dll" "SetConsoleMode"
;;;                      :args '(:ptr :uint32) :ret :bool)
;;;   (set-console-mode handle mode)  ; => T or NIL

;;; Export define-ffi before using it with single-colon accessor
(export 'dotnet::define-ffi (find-package :dotnet))

;;; (dotnet:define-ffi name dll func :args '(type ...) :ret type)
;;; Defines a named Lisp function wrapping a native call.
(defmacro dotnet:define-ffi (name dll func &rest options)
  (let ((arg-form nil) (ret-form nil))
    (do ((rest options (cddr rest)))
        ((null rest))
      (case (car rest)
        (:args (setq arg-form (cadr rest)))
        (:ret  (setq ret-form (cadr rest)))))
    (let* ((arg-list (if (and (consp arg-form) (eq (car arg-form) 'quote))
                         (cadr arg-form)
                         nil))
           (params (let ((r nil))
                     (dotimes (i (length arg-list) (nreverse r))
                       (push (intern (format nil "ARG~D" i)) r)))))
      `(defun ,name ,params
         (dotnet:%ffi-call ,dll ,func ,arg-form ,ret-form ,@params)))))

(provide "dotnet-ffi")
