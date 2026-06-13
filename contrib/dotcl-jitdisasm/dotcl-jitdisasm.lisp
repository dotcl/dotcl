;;; dotcl-jitdisasm.lisp — JIT native disassembly for dotcl
;;;
;;; Usage: (require "dotcl-jitdisasm")
;;;        (dotcl:jit-disassemble #'some-fn)
;;;
;;; Loads DotCL.Contrib.JitDisasm.dll (built via `make contrib-dotcl-jitdisasm`)
;;; and wires it into the dotcl:jit-disassemble hook.

(defpackage :dotcl-jitdisasm
  (:use :cl))

(in-package :dotcl-jitdisasm)

(defvar *contrib-dir*
  (when *load-pathname*
    (make-pathname :directory (pathname-directory *load-pathname*)
                   :defaults *load-pathname*)))

(defvar *lib-loaded* nil)

(defun ensure-lib-loaded ()
  (unless *lib-loaded*
    (unless *contrib-dir*
      (error "dotcl-jitdisasm: *contrib-dir* not captured — loaded outside of require?"))
    (let ((dll (namestring
                (merge-pathnames "lib/DotCL.Contrib.JitDisasm.dll" *contrib-dir*))))
      (dotnet:load-assembly dll))
    (dotnet:static "DotCL.Contrib.JitDisasm.JitDisasmContrib" "Initialize")
    (setf *lib-loaded* t)))

(ensure-lib-loaded)
