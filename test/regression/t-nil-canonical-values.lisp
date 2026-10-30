;;; Every path that hands out the symbols T and NIL gives the same objects the
;;; reader gives for the tokens T and NIL: TYPE-OF is BOOLEAN / NULL, EQ and NULL
;;; agree, and NIL is false in a test position.

(defun %tn-shape (x)
  (list (type-of x) (eq x t) (eq x nil) (null x) (if x :true :false)))

(defparameter *tn-t-shape* (%tn-shape t))
(defparameter *tn-nil-shape* (%tn-shape nil))

(defun %tn-ok (x expected) (equal (%tn-shape x) expected))

(defun %tn-find (name list)
  (find-if (lambda (s)
             (and (string= (symbol-name s) name)
                  (eq (symbol-package s) (find-package "COMMON-LISP"))))
           list))

(deftest t-nil-canonical-values.shapes
  (list *tn-t-shape* *tn-nil-shape*)
  ((boolean t nil nil :true) (null nil t t :false)))

(deftest t-nil-canonical-values.do-symbols
  (let (te ne)
    (do-symbols (s "COMMON-LISP")
      (cond ((string= (symbol-name s) "T") (setq te s))
            ((string= (symbol-name s) "NIL") (setq ne (list s)))))
    (list (%tn-ok te *tn-t-shape*) (%tn-ok (car ne) *tn-nil-shape*)))
  (t t))

(deftest t-nil-canonical-values.do-external-symbols
  (let (te ne)
    (do-external-symbols (s "COMMON-LISP")
      (cond ((string= (symbol-name s) "T") (setq te s))
            ((string= (symbol-name s) "NIL") (setq ne (list s)))))
    (list (%tn-ok te *tn-t-shape*) (%tn-ok (car ne) *tn-nil-shape*)))
  (t t))

(deftest t-nil-canonical-values.do-symbols-inherited
  (let (te ne)
    (do-symbols (s "COMMON-LISP-USER")
      (cond ((string= (symbol-name s) "T") (setq te s))
            ((string= (symbol-name s) "NIL") (setq ne (list s)))))
    (list (%tn-ok te *tn-t-shape*) (%tn-ok (car ne) *tn-nil-shape*)))
  (t t))

(deftest t-nil-canonical-values.do-all-symbols
  (let (te ne)
    (do-all-symbols (s)
      (when (eq (symbol-package s) (find-package "COMMON-LISP"))
        (cond ((string= (symbol-name s) "T") (setq te s))
              ((string= (symbol-name s) "NIL") (setq ne (list s))))))
    (list (%tn-ok te *tn-t-shape*) (%tn-ok (car ne) *tn-nil-shape*)))
  (t t))

(deftest t-nil-canonical-values.with-package-iterator
  (let (te ne)
    (with-package-iterator (next "COMMON-LISP" :external)
      (loop (multiple-value-bind (more s) (next)
              (unless more (return))
              (cond ((string= (symbol-name s) "T") (setq te s))
                    ((string= (symbol-name s) "NIL") (setq ne (list s)))))))
    (list (%tn-ok te *tn-t-shape*) (%tn-ok (car ne) *tn-nil-shape*)))
  (t t))

(deftest t-nil-canonical-values.apropos-list
  (list (%tn-ok (%tn-find "T" (apropos-list "T" "COMMON-LISP")) *tn-t-shape*)
        (%tn-ok (%tn-find "NIL" (apropos-list "NIL" "COMMON-LISP")) *tn-nil-shape*)
        (%tn-ok (%tn-find "T" (apropos-list "T")) *tn-t-shape*))
  (t t t))

(deftest t-nil-canonical-values.find-intern-read
  (list (%tn-ok (find-symbol "T" "COMMON-LISP") *tn-t-shape*)
        (%tn-ok (find-symbol "NIL" "COMMON-LISP") *tn-nil-shape*)
        (%tn-ok (intern "NIL" "COMMON-LISP") *tn-nil-shape*)
        (%tn-ok (read-from-string "cl:nil") *tn-nil-shape*)
        (%tn-ok (read-from-string "cl::t") *tn-t-shape*)
        (%tn-ok (%tn-find "NIL" (find-all-symbols "NIL")) *tn-nil-shape*))
  (t t t t t t))

(deftest t-nil-canonical-values.class-name
  (list (%tn-ok (class-name (find-class t)) *tn-t-shape*)
        (%tn-ok (class-name (car (last (dotcl-mop:class-precedence-list (find-class 'integer)))))
                *tn-t-shape*))
  (t t))

(deftest t-nil-canonical-values.array-element-types
  (list (%tn-ok (upgraded-array-element-type t) *tn-t-shape*)
        (%tn-ok (upgraded-array-element-type '(complex rational)) *tn-t-shape*)
        (%tn-ok (array-element-type (make-array 2)) *tn-t-shape*)
        (%tn-ok (array-element-type (make-array '(2 2))) *tn-t-shape*)
        (%tn-ok (array-element-type (make-array 2 :adjustable t)) *tn-t-shape*)
        (%tn-ok (second (type-of (make-array '(2 2)))) *tn-t-shape*)
        (%tn-ok (second (type-of (make-array 3 :adjustable t))) *tn-t-shape*)
        (%tn-ok (upgraded-array-element-type nil) *tn-nil-shape*))
  (t t t t t t t t))

(deftest t-nil-canonical-values.package-shadowing-symbols
  (let ((p (make-package "T-NIL-CANONICAL-SHADOW" :use nil)))
    (unwind-protect
         (progn
           (shadowing-import '(t nil) p)
           (let ((syms (package-shadowing-symbols p)))
             (list (length syms)
                   (%tn-ok (%tn-find "T" syms) *tn-t-shape*)
                   (%tn-ok (%tn-find "NIL" syms) *tn-nil-shape*))))
      (delete-package p)))
  (2 t t))

(deftest t-nil-canonical-values.restart-and-cell-error-names
  (list (%tn-ok (restart-case (restart-name (first (compute-restarts)))
                  (nil () nil))
                *tn-nil-shape*)
        (%tn-ok (handler-case (symbol-function nil)
                  (undefined-function (c) (cell-error-name c)))
                *tn-nil-shape*))
  (t t))
