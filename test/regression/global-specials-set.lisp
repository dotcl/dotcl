;;; The compiler keeps the globally special variables it has seen (DEFVAR,
;;; DEFPARAMETER, DECLAIM SPECIAL) in a hash set instead of a list, so the
;;; lookup does not get slower with the number of special variables. These
;;; check that what is special, and where, is unchanged.

(defvar *gss-dv* :global)
(defun gss-read-dv () *gss-dv*)

(deftest global-specials-set.defvar-let-is-dynamic
  (let ((*gss-dv* :bound)) (gss-read-dv))
  :bound)

(declaim (special *gss-declaimed*))
(setf (symbol-value '*gss-declaimed*) :global)
(defun gss-read-declaimed () (symbol-value '*gss-declaimed*))

(deftest global-specials-set.declaim-let-is-dynamic
  (let ((*gss-declaimed* :bound)) (gss-read-declaimed))
  :bound)

(defun gss-read-local () (symbol-value 'gss-local-x))

(deftest global-specials-set.local-declare-binds-dynamically
  (let ((gss-local-x :bound))
    (declare (special gss-local-x))
    (gss-read-local))
  :bound)

;; An outer local special declaration does not make an inner binding special.
(deftest global-specials-set.local-declare-not-inherited-by-inner-let
  (let ((gss-local-y :outer))
    (declare (special gss-local-y))
    (let ((gss-local-y :inner))
      (list gss-local-y (symbol-value 'gss-local-y))))
  (:inner :outer))

(defvar *gss-unbound* 1)

(deftest global-specials-set.special-after-makunbound
  (progn
    (makunbound '*gss-unbound*)
    (let ((*gss-unbound* :bound))
      (symbol-value '*gss-unbound*)))
  :bound)

;; Specialness is per symbol: another package's symbol of the same name is not.
(deftest global-specials-set.other-package-same-name
  (let* ((pkg (or (find-package "GSS-OTHER") (make-package "GSS-OTHER" :use '())))
         (other (intern "*GSS-DV*" pkg)))
    (list (dotcl.cil-compiler::global-special-p '*gss-dv*)
          (dotcl.cil-compiler::global-special-p other)
          (dotcl.cil-compiler::special-var-p other)))
  (t nil nil))

;; Known to the compiler only (the image is not asked while cross-compiling):
;; a LET of a variable the compiler saw DEFVARed binds it dynamically, and one
;; it did not see binds it lexically.
(deftest global-specials-set.compiler-only-registration
  (let* ((flag (find-symbol "*CROSS-COMPILING*" "DOTCL-INTERNAL"))
         (seen (make-symbol "GSS-SEEN"))
         (unseen (make-symbol "GSS-UNSEEN")))
    (dotcl.cil-compiler::note-global-special seen)
    (flet ((dynamic-p (var)
             (progv (list flag) '(t)
               (and (search "DynamicBindings.Push"
                            (prin1-to-string
                             (dotcl.cil-compiler::compile-toplevel
                              `(defun gss-probe (v) (let ((,var v)) (gss-callee))))))
                    t))))
      (list (dynamic-p seen) (dynamic-p unseen)
            (and (dotcl.cil-compiler::special-var-p seen) t))))
  (t nil t))
