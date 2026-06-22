;;; project-core CONCATENATED build (dotcl build <asd> --output) evaluates
;;; toplevel module/package setup forms at compile time, so a macro provided by a
;;; bare (load ...) / (require ...) in an earlier part of the concatenated unit is
;;; available when a later part compiles — instead of being miscompiled as an
;;; undefined function call (the symptom when ASDF :components were split).
;;;
;;; compile-file-concatenated is the entry the C# build driver calls; it binds
;;; *concatenate-build* (from cross-compiled code, so the dynamic binding shares
;;; symbol identity with compile-form's read). We exercise it directly here with a
;;; hand-written concatenated source, so the test needs no asdf and no .asd.
;;;
;;; NOTE: stay in the default load package (CL-USER) — an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro and makes the test
;;; name read as an unbound variable. Helpers are pc325- prefixed to avoid clashes.

(defvar *pc325-tmp-dir* "test/regression/.tmp-pc325/")
(defvar *pc325-result* :unset)

(defun pc325-write-text (path text)
  (with-open-file (s path :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun pc325-build-and-load-concat ()
  "Write a macro-provider file + a concatenated source that loads it (bare, no
   eval-when) then uses its macro at top level. Build via compile-file-concatenated,
   load the fasl, return the value the macro produced. With the fix the macro
   expands at compile time; without it the load errors (undefined function)."
  (ensure-directories-exist *pc325-tmp-dir*)
  (let* ((mod  (namestring (merge-pathnames "pc325-mod.lisp"    (truename *pc325-tmp-dir*))))
         (src  (namestring (merge-pathnames "pc325-concat.lisp" (truename *pc325-tmp-dir*))))
         (fasl (namestring (merge-pathnames "pc325.fasl"        (truename *pc325-tmp-dir*))))
         (modf (substitute #\/ #\\ mod)))
    (setf *pc325-result* :unset)
    (pc325-write-text mod
      "(in-package :cl-user)
       (defmacro pc325-mac () 4242)")
    ;; Bare (load ...) — NOT wrapped in eval-when. The fix must make this take
    ;; compile-time effect within the concatenated unit.
    (pc325-write-text src
      (format nil "(load ~s)~%(defparameter cl-user::*pc325-result* (pc325-mac))~%" modf))
    (dotcl.cil-compiler:compile-file-concatenated src fasl)
    (load fasl)
    *pc325-result*))

(deftest project-core-concat-toplevel-load-macro
  ;; 4242 proves pc325-mac expanded (macro available at compile time); a plain
  ;; function call would have signalled "Undefined function: PC325-MAC" on load.
  (pc325-build-and-load-concat)
  4242)
