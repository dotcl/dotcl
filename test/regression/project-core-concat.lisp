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

(deftest-compiled-only project-core-concat-toplevel-load-macro
  ;; 4242 proves pc325-mac expanded (macro available at compile time); a plain
  ;; function call would have signalled "Undefined function: PC325-MAC" on load.
  (pc325-build-and-load-concat)
  4242)

;;; --- fasl serialization of class-object constants ---
;;; A class object used as a compile-time literal (#.(find-class 'foo) /
;;; #.(class-of x)) must be emitted as a FIND-CLASS load-form so the fasl is
;;; cross-process safe, not a per-process constant-pool reference. We can't spawn
;;; a second process here, but compiling to a fasl and loading it exercises the
;;; LispClass emission path; the class must round-trip to the same registry object.
(defun cc469-build-and-load ()
  (ensure-directories-exist *pc325-tmp-dir*)
  (let* ((src  (namestring (merge-pathnames "cc469-src.lisp" (truename *pc325-tmp-dir*))))
         (fasl (namestring (merge-pathnames "cc469.fasl"     (truename *pc325-tmp-dir*)))))
    (pc325-write-text src
      "(in-package :cl-user)
       (defclass cc469-myc () ())
       (defun cc469-builtin () #.(find-class 'cons))
       (defun cc469-user () #.(find-class 'cc469-myc))")
    (compile-file src :output-file fasl)
    (load fasl)
    (list (eq (funcall (find-symbol "CC469-BUILTIN")) (find-class 'cons))
          (eq (funcall (find-symbol "CC469-USER")) (find-class 'cc469-myc)))))

(deftest-compiled-only fasl-class-constant-roundtrip
  ;; Both a built-in class (CONS) and a user class resolve back to the live
  ;; registry object after compile-file + load (find-class load-form, not pool).
  (cc469-build-and-load)
  (t t))

;;; --- compile-file of a source whose basename has an AssemblyName-reserved
;;; char --- e.g. serapeum's vector=.lisp. The fasl module name derived
;;; from the basename ("vector=_<guid>") must be sanitized before new
;;; AssemblyName, which otherwise throws "The given assembly name was invalid.".
(defun cc471-build-and-load ()
  (ensure-directories-exist *pc325-tmp-dir*)
  (let* ((src  (namestring (merge-pathnames "cc471=eq.lisp" (truename *pc325-tmp-dir*))))
         (fasl (namestring (merge-pathnames "cc471=eq.fasl" (truename *pc325-tmp-dir*)))))
    (pc325-write-text src "(in-package :cl-user)(defun cc471-fn () 471)")
    (compile-file src :output-file fasl)   ; must not signal "assembly name invalid"
    (load fasl)
    (funcall (find-symbol "CC471-FN"))))

(deftest-compiled-only fasl-equals-in-filename
  (cc471-build-and-load)
  471)
