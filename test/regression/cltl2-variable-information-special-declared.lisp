;;; VARIABLE-INFORMATION of a special variable reports the type declared where the
;;; query is made, merged with the proclaimed one, not the proclamation alone.

(defvar *cvisd-g* 0)
(declaim (type integer *cvisd-g*))
(defvar *cvisd-r* nil)

(defmacro cvisd-probe (name &environment env)
  (setf *cvisd-r* (multiple-value-list (dotcl-cltl2:variable-information name env)))
  nil)

(deftest cltl2-variable-information-special-declared.augment
  (let ((env (dotcl-cltl2:augment-environment
              nil :variable '(v) :declare '((special v) (fixnum v)))))
    (multiple-value-list (dotcl-cltl2:variable-information 'v env)))
  (:special nil ((type . fixnum))))

(deftest-compiled-only cltl2-variable-information-special-declared.let
  (progn
    (funcall (compile nil '(lambda ()
                            (let ((v 1))
                              (declare (special v) (fixnum v))
                              (cvisd-probe v)))))
    *cvisd-r*)
  (:special nil ((type . fixnum))))

(deftest-compiled-only cltl2-variable-information-special-declared.global-narrowed
  (progn
    (funcall (compile nil '(lambda ()
                            (locally (declare (type (integer 0 5) *cvisd-g*))
                              (cvisd-probe *cvisd-g*)))))
    *cvisd-r*)
  (:special nil ((type integer 0 5))))

(deftest cltl2-variable-information-special-declared.global-only
  (multiple-value-list (dotcl-cltl2:variable-information '*cvisd-g*))
  (:special nil ((type . integer))))
