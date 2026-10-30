;;; The CL package table holds plain symbol entries for T and NIL next to the
;;; objects the reader gives for the tokens T and NIL. Some paths still hand the
;;; table entries out (package iteration, CLASS-NAME of the class T), and EQ has
;;; to treat each pair as one symbol, in either argument order, in value and in
;;; test position, and through FUNCALL.

(defun %eq-entry (name)
  (do-symbols (s "COMMON-LISP")
    (when (string= (symbol-name s) name) (return s))))

(defun %eq-value (a b) (eq a b))
(defun %eq-test (a b) (if (eq a b) :same :different))

(deftest eq-t-nil-entries.value-position
  (let ((te (%eq-entry "T")) (ne (%eq-entry "NIL")))
    (list (%eq-value te t) (%eq-value t te) (%eq-value ne nil) (%eq-value nil ne)
          (%eq-value te nil) (%eq-value ne t) (%eq-value te ne) (%eq-value te 'car)))
  (t t t t nil nil nil nil))

(deftest eq-t-nil-entries.test-position
  (let ((te (%eq-entry "T")) (ne (%eq-entry "NIL")))
    (list (%eq-test te t) (%eq-test t te) (%eq-test ne nil) (%eq-test nil ne)
          (%eq-test te nil) (%eq-test nil te) (%eq-test ne 'car)))
  (:same :same :same :same :different :different :different))

(deftest eq-t-nil-entries.funcall-and-member
  (let ((te (%eq-entry "T")) (ne (%eq-entry "NIL")))
    (list (funcall #'eq te t) (funcall #'eq nil ne)
          (and (member te (list 1 t) :test #'eq) t)
          (and (member nil (list ne 2) :test #'eq) t)))
  (t t t t))

(deftest eq-t-nil-entries.class-name-of-t
  (list (eq (class-name (find-class t)) t)
        (if (eq t (class-name (find-class t))) :same :different))
  (t :same))

(deftest eq-t-nil-entries.distinct-objects
  (let ((a (list 1)) (b (list 1)) (s (make-symbol "T")))
    (list (%eq-value a b) (%eq-value a a) (%eq-value s t) (%eq-value t s)
          (%eq-test 'foo 'foo) (%eq-test 'foo 'bar) (%eq-value 3 3)))
  (nil t nil nil :same :different t))
