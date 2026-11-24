;;; SBCL signals a package-locked-error (a PACKAGE-ERROR) for UNINTERN and RENAME-PACKAGE on the
;;; COMMON-LISP package; dotcl used to do both and break CAR for the rest of the session.

(deftest package-lock-unintern-rename
  (list (handler-case (progn (unintern 'car "CL") :no-error) (package-error () :package-error))
        (handler-case (progn (rename-package "CL" "FOO") :no-error) (package-error () :package-error))
        (eq (find-symbol "CAR" "CL") 'car)
        (package-name (find-package "CL")))
  (:package-error :package-error t "COMMON-LISP"))

(deftest package-lock-unintern-rename.unlocked
  (let ((p (make-package "PLUR-1")))
    (list (unintern (intern "Q" p) p)
          (package-name (rename-package p "PLUR-2"))
          (progn (delete-package "PLUR-2") t)))
  (t "PLUR-2" t))

;;; Only a change the operation would actually make touches the locked package (SBCL).
(deftest package-lock-unintern-rename.no-change
  (list (unintern (make-symbol "NOT-IN-CL") "CL")
        (unintern 'plur-not-there "CL")
        (package-name (rename-package "COMMON-LISP" "COMMON-LISP" (package-nicknames "COMMON-LISP")))
        (handler-case (progn (unintern 'car "CL") :no-error) (package-error () :package-error))
        (handler-case (progn (rename-package "COMMON-LISP" "COMMON-LISP" '("CL" "PLUR-NICK")) :no-error)
          (package-error () :package-error))
        (package-nicknames "COMMON-LISP"))
  (nil nil "COMMON-LISP" :package-error :package-error ("CL")))
