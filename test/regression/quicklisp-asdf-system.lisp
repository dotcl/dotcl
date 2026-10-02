;;; The bundled Quicklisp client is one file with no quicklisp.asd beside it, so
;;; a system with "quicklisp" in :depends-on (Quicklisp tooling such as
;;; quickdist and cl-brewer) failed with "System quicklisp not found", while a
;;; stock Quicklisp install finds its .asd. (require "quicklisp") now registers
;;; the system with ASDF as already present in the image.

(require "quicklisp")

(deftest quicklisp-asdf-system.registered
  (let ((s (asdf:find-system "quicklisp" nil)))
    (list (and s t)
          (and s (asdf:component-loaded-p s) t)))
  (t t))

(deftest quicklisp-asdf-system.dependents-resolve
  (progn
    ;; Defined with no source file, so ASDF does not try to re-read this file
    ;; as the system's definition.
    (let ((*load-pathname* nil) (*load-truename* nil))
      (eval '(asdf:defsystem "qas-uses-quicklisp" :depends-on ("quicklisp"))))
    (handler-case (progn (asdf:load-system "qas-uses-quicklisp") :loaded)
      (asdf:missing-dependency (e)
        (list :missing (asdf/find-component:missing-requires e)))))
  :loaded)
