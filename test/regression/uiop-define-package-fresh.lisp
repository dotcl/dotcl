;;; UIOP:DEFINE-PACKAGE on a package that does not exist yet only has to make it,
;;; use its :use packages, intern and export what it lists and re-export what it
;;; names.  Its result has to be the one the full reconciliation gives (the path
;;; a redefinition and :mix, :shadow, :import-from and :unintern still take).
;;;
;;; Hermetic: the packages are uniquely named and deleted at the end of each
;;; test.  UIOP is reached through read-from-string so that loading this file
;;; does not need asdf up front.

(defun %dpf-def (form-string)
  (require "asdf")
  (eval (let ((*package* (find-package "CL-USER"))) (read-from-string form-string))))

(defun %dpf-kill (&rest names)
  (dolist (n names) (let ((p (find-package n))) (when p (delete-package p)))))

(defun %dpf-state (name)
  "Use list, external names with their home packages, and the number of internal and inherited symbols."
  (let ((p (find-package name)) (ext nil) (int 0) (inh 0))
    (do-symbols (s p)
      (ecase (nth-value 1 (find-symbol (symbol-name s) p))
        (:external (push (cons (symbol-name s) (and (symbol-package s) (package-name (symbol-package s)))) ext))
        (:internal (incf int))
        (:inherited (incf inh))))
    (list (sort (mapcar #'package-name (package-use-list p)) #'string<)
          (sort ext #'string< :key #'car)
          int
          (plusp inh))))

(deftest uiop-define-package-fresh.use-and-export
  (progn
    (%dpf-kill "%DPF-A")
    (%dpf-def "(uiop:define-package :%dpf-a (:use :cl) (:export #:foo #:bar))")
    (prog1 (list (%dpf-state "%DPF-A")
                 (eq (find-symbol "CAR" "%DPF-A") 'car)
                 (eq (nth-value 1 (find-symbol "CAR" "%DPF-A")) :inherited)
                 (nth-value 1 (find-symbol "FOO" "%DPF-A")))
      (%dpf-kill "%DPF-A")))
  ((("COMMON-LISP") (("BAR" . "%DPF-A") ("FOO" . "%DPF-A")) 0 t) t t :external))

(deftest uiop-define-package-fresh.use-reexport
  (progn
    (%dpf-kill "%DPF-B" "%DPF-C")
    (%dpf-def "(uiop:define-package :%dpf-b (:use :cl) (:export #:one #:two))")
    (%dpf-def "(uiop:define-package :%dpf-c (:use :cl) (:use-reexport :%dpf-b) (:export #:three))")
    (prog1 (list (%dpf-state "%DPF-C")
                 (eq (find-symbol "ONE" "%DPF-C") (find-symbol "ONE" "%DPF-B"))
                 (nth-value 1 (find-symbol "TWO" "%DPF-C")))
      (%dpf-kill "%DPF-C" "%DPF-B")))
  ((("%DPF-B" "COMMON-LISP")
    (("ONE" . "%DPF-B") ("THREE" . "%DPF-C") ("TWO" . "%DPF-B"))
    0 t)
   t :external))

(deftest uiop-define-package-fresh.redefinition-is-idempotent
  (progn
    (%dpf-kill "%DPF-D")
    (let ((form "(uiop:define-package :%dpf-d (:use :cl) (:export #:x #:y))"))
      (%dpf-def form)
      (let ((first (%dpf-state "%DPF-D")))
        (%dpf-def form)
        (prog1 (list (equal first (%dpf-state "%DPF-D")))
          (%dpf-kill "%DPF-D")))))
  (t))

(deftest uiop-define-package-fresh.redefinition-drops-unwanted-use
  (progn
    (%dpf-kill "%DPF-E")
    (%dpf-def "(uiop:define-package :%dpf-e (:use :cl) (:export #:x))")
    (%dpf-def "(uiop:define-package :%dpf-e (:use) (:export #:x))")
    (prog1 (list (mapcar #'package-name (package-use-list "%DPF-E"))
                 (nth-value 1 (find-symbol "CAR" "%DPF-E")))
      (%dpf-kill "%DPF-E")))
  (nil nil))

(deftest uiop-define-package-fresh.shadow-and-import-from
  (progn
    (%dpf-kill "%DPF-F" "%DPF-G")
    (%dpf-def "(uiop:define-package :%dpf-g (:use :cl) (:export #:thing))")
    (%dpf-def "(uiop:define-package :%dpf-f (:use :cl) (:shadow #:car) (:import-from :%dpf-g #:thing) (:export #:car #:thing))")
    (prog1 (list (eq (find-symbol "CAR" "%DPF-F") 'car)
                 (nth-value 1 (find-symbol "CAR" "%DPF-F"))
                 (eq (find-symbol "THING" "%DPF-F") (find-symbol "THING" "%DPF-G"))
                 (nth-value 1 (find-symbol "THING" "%DPF-F")))
      (%dpf-kill "%DPF-F" "%DPF-G")))
  (nil :external t :external))

(deftest uiop-define-package-fresh.mix
  (progn
    (%dpf-kill "%DPF-H" "%DPF-I" "%DPF-J")
    (%dpf-def "(uiop:define-package :%dpf-h (:use :cl) (:export #:dup #:only-h))")
    (%dpf-def "(uiop:define-package :%dpf-i (:use :cl) (:export #:dup #:only-i))")
    (%dpf-def "(uiop:define-package :%dpf-j (:use :cl) (:mix :%dpf-h :%dpf-i))")
    (prog1 (list (eq (find-symbol "DUP" "%DPF-J") (find-symbol "DUP" "%DPF-H"))
                 (eq (find-symbol "ONLY-I" "%DPF-J") (find-symbol "ONLY-I" "%DPF-I")))
      (%dpf-kill "%DPF-J" "%DPF-I" "%DPF-H")))
  (t t))
