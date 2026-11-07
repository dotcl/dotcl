;;; LET, LET*, SETQ, PROGV, LOAD-TIME-VALUE, EVAL-WHEN, MULTIPLE-VALUE-CALL and
;;; MULTIPLE-VALUE-PROG1 with a bad shape signal PROGRAM-ERROR; every valid
;;; shape still works.

(defmacro %sfl-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape-let.invalid
  (values (%sfl-pe (let)) (%sfl-pe (let*)) (%sfl-pe (let 1)) (%sfl-pe (let (1)))
          (%sfl-pe (let ((x 1 2)))) (%sfl-pe (let* ((x 1 2)))) (%sfl-pe (let ((1 2))))
          (%sfl-pe (let (x . y))) (%sfl-pe (let ((x . 1))))
          (%sfl-pe (let ((x 1) (y 2)) (setq x 1 y)))
          (%sfl-pe (setq x)) (%sfl-pe (setq 1 2)) (%sfl-pe (setq nil 1 . x))
          (%sfl-pe (progv)) (%sfl-pe (progv nil)) (%sfl-pe (load-time-value))
          (%sfl-pe (load-time-value 1 nil 3)) (%sfl-pe (eval-when))
          (%sfl-pe (multiple-value-call)) (%sfl-pe (multiple-value-prog1)))
  :program-error :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error
  :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error
  :program-error :program-error)

(defvar *sfl-special* 0)

(deftest special-form-shape-let.valid
  (values (eval '(let ())) (eval '(let () 1 2)) (eval '(let (x) x)) (eval '(let ((x)) x))
          (eval '(let ((x 1) y) (list x y)))
          (eval '(let* ((x 1) (y (1+ x))) y))
          (eval '(let ((x 1)) (declare (ignorable x)) 7))
          (eval '(let (x) (setq x 3))) (eval '(setq)) (eval '(let (x y) (setq x 1 y 2)))
          (eval '(progv '(*sfl-special*) '(5) *sfl-special*))
          (eval '(progv nil nil))
          (eval '(load-time-value 8)) (eval '(load-time-value (+ 1 2) t))
          (eval '(eval-when (:execute) 9)) (eval '(eval-when ()))
          (eval '(multiple-value-call #'list 1 2))
          (eval '(multiple-value-call #'list))
          (eval '(multiple-value-prog1 6)))
  nil 2 nil nil (1 nil) 2 7 3 nil 2 5 nil 8 3 9 nil (1 2) nil 6)
