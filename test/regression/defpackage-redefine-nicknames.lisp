;;; Redefining a package with DEFPACKAGE makes its nicknames exactly the new list.

(defun %drn-nicknames (name)
  (sort (copy-list (package-nicknames name)) #'string<))

(defun %drn-error-p (form)
  (handler-case (progn (eval form) nil) (error () t)))

(defpackage :drn-pkg (:nicknames :drn-one :drn-two))
(defpackage :drn-other (:nicknames :drn-taken))
(defpackage :drn-own (:nicknames :drn-own :drn-own-nick))
(defpackage :drn-rb (:nicknames :drn-rb-old) (:use :cl) (:export #:drn-keep))

(deftest defpackage-redefine-nicknames.remove-one
  (progn
    (eval '(defpackage :drn-pkg (:nicknames :drn-one)))
    (list (%drn-nicknames :drn-pkg)
          (find-package :drn-two)
          (eq (find-package :drn-one) (find-package :drn-pkg))))
  (("DRN-ONE") nil t))

(deftest defpackage-redefine-nicknames.remove-all
  (progn
    (eval '(defpackage :drn-pkg))
    (list (%drn-nicknames :drn-pkg)
          (find-package :drn-one)
          (not (null (find-package :drn-pkg)))))
  (nil nil t))

(deftest defpackage-redefine-nicknames.add
  (progn
    (eval '(defpackage :drn-pkg (:nicknames :drn-three)))
    (let ((one (%drn-nicknames :drn-pkg)))
      (eval '(defpackage :drn-pkg (:nicknames :drn-three :drn-four)))
      (list one (%drn-nicknames :drn-pkg)
            (eq (find-package :drn-four) (find-package :drn-pkg)))))
  (("DRN-THREE") ("DRN-FOUR" "DRN-THREE") t))

(deftest defpackage-redefine-nicknames.same-list
  (progn
    (eval '(defpackage :drn-pkg (:nicknames :drn-four :drn-three)))
    (eval '(defpackage :drn-pkg (:nicknames :drn-three :drn-four)))
    (%drn-nicknames :drn-pkg))
  ("DRN-FOUR" "DRN-THREE"))

(deftest defpackage-redefine-nicknames.conflict-keeps-both
  (let ((before (%drn-nicknames :drn-pkg)))
    (list (%drn-error-p '(defpackage :drn-pkg (:nicknames :drn-taken)))
          (equal before (%drn-nicknames :drn-pkg))
          (%drn-nicknames :drn-other)
          (eq (find-package :drn-taken) (find-package :drn-other))))
  (t t ("DRN-TAKEN") t))

(deftest defpackage-redefine-nicknames.valid-after-conflict
  (progn
    (eval '(defpackage :drn-pkg (:nicknames :drn-free)))
    (%drn-nicknames :drn-pkg))
  ("DRN-FREE"))

(deftest defpackage-redefine-nicknames.rollback-existing
  (let ((failed (%drn-error-p
                 '(defpackage :drn-rb (:nicknames :drn-rb-new)
                   (:use :cl :drn-no-such-package)))))
    (list failed
          (%drn-nicknames :drn-rb)
          (find-package :drn-rb-new)
          (eq (find-package :drn-rb-old) (find-package :drn-rb))
          (multiple-value-bind (s status) (find-symbol "DRN-KEEP" :drn-rb)
            (list (and s (symbol-name s)) status))))
  (t ("DRN-RB-OLD") nil t ("DRN-KEEP" :external)))

(deftest defpackage-redefine-nicknames.rollback-new
  (list (%drn-error-p
         '(defpackage :drn-rb-new (:nicknames :drn-rb-new-nick)
           (:use :drn-no-such-package)))
        (find-package :drn-rb-new)
        (find-package :drn-rb-new-nick))
  (t nil nil))

(deftest defpackage-redefine-nicknames.own-name-ignored
  (progn
    (eval '(defpackage :drn-own (:nicknames :drn-own :drn-own-nick)))
    (let ((before (%drn-nicknames :drn-own)))
      (eval '(defpackage :drn-own (:nicknames :drn-own)))
      (list before (%drn-nicknames :drn-own)
            (not (null (find-package :drn-own))))))
  (("DRN-OWN-NICK") nil t))
