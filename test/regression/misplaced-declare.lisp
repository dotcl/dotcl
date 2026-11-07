;;; A (DECLARE ...) after a form in the body of LET, LET*, LOCALLY,
;;; MULTIPLE-VALUE-BIND, FLET or LABELS is a PROGRAM-ERROR; declarations at the
;;; head (and a documentation string among them in FLET / LABELS) are fine.

(defmacro %md-err (form)
  `(handler-case (progn (eval ',form) :no-error) (program-error () :program-error) (error () :other-error)))

(deftest misplaced-declare.invalid
  (values (%md-err (let () 1 (declare) 2))
          (%md-err (let ((x 1)) x (declare (ignorable x))))
          (%md-err (let* ((x 1)) x (declare (ignorable x))))
          (%md-err (locally 1 (declare (special zz))))
          (%md-err (multiple-value-bind (x) 1 nil (declare) nil))
          (%md-err (flet ((foo () "x" "y" (declare))) (foo)))
          (%md-err (labels ((foo () 1 (declare))) (foo))))
  :program-error :program-error :program-error :program-error :program-error :program-error :program-error)

(deftest misplaced-declare.valid
  (values (eval '(let () (declare) 1))
          (eval '(let* ((x 1)) (declare (ignorable x)) (declare (special zz)) x))
          (eval '(locally (declare) 1))
          (eval '(multiple-value-bind (x) 1 (declare (ignorable x)) 3))
          (eval '(flet ((foo () "doc" (declare) 5)) (foo)))
          (eval '(flet ((foo () (declare) "doc" 6)) (foo)))
          (eval '(flet ((foo () "doc")) (foo)))
          (eval '(let () "str" 7)))
  1 1 1 3 5 6 "doc" 7)
