;;; NIL and T print with their COMMON-LISP prefix in a package that does not use COMMON-LISP, also when
;;; the name needs escaping (readtable-case :downcase).

(defun %pnq (rc form-fn)
  (let ((pkg (or (find-package "PNQ-EMPTY") (make-package "PNQ-EMPTY" :use nil)))
        (rt (copy-readtable nil)))
    (setf (readtable-case rt) rc)
    (let ((*package* pkg) (*readtable* rt))
      (funcall form-fn))))

(deftest print-nil-t-qualified-when-escaped.downcase
  (list (%pnq :downcase (lambda () (prin1-to-string nil)))
        (%pnq :downcase (lambda () (prin1-to-string t)))
        (%pnq :downcase (lambda () (prin1-to-string '(nil t car)))))
  ("|COMMON-LISP|:|NIL|" "|COMMON-LISP|:|T|" "(|COMMON-LISP|:|NIL| |COMMON-LISP|:|T| |COMMON-LISP|:|CAR|)"))

(deftest print-nil-t-qualified-when-escaped.cl-user-unchanged
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :downcase)
    (let ((*readtable* rt) (*package* (find-package "CL-USER")))
      (list (prin1-to-string nil) (prin1-to-string t))))
  ("|NIL|" "|T|"))

(deftest print-nil-t-qualified-when-escaped.base-30
  (let ((*print-base* 30) (*package* (or (find-package "PNQ-EMPTY") (make-package "PNQ-EMPTY" :use nil))))
    (list (prin1-to-string t) (let ((*package* (find-package "CL-USER"))) (prin1-to-string t))))
  ("|COMMON-LISP|:|T|" "|T|"))
