;;; Forms SBCL rejects are rejected at macroexpansion: a DO / DO* variable spec longer than
;;; (var init step), a DOTIMES / DOLIST clause with the wrong number of parts, a PROG binding with three
;;; parts, PROG1 / PROG2 without their forms, a CCASE / CTYPECASE whose key place is a constant. Every COND
;;; clause must be a list (checked in the compiler, so emitting-only here). A non-integer DOTIMES count
;;; stays accepted (its ceiling is used).

(defmacro %mik-expands-p (form)
  `(handler-case (progn (macroexpand-1 ',form) :ok) (program-error () :program-error)))

(deftest malformed-iteration-and-keyplace.macroexpansion-errors
  (list (%mik-expands-p (do ((i 0 1 2)) (t i)))
        (%mik-expands-p (do* ((i 0 1 2)) (t i)))
        (%mik-expands-p (do ((1 2)) (t nil)))
        (%mik-expands-p (dotimes (i 3 i i)))
        (%mik-expands-p (dotimes (i)))
        (%mik-expands-p (dolist (x)))
        (%mik-expands-p (dolist (x '(1) :done :extra)))
        (%mik-expands-p (prog ((x 1 2)) (return x)))
        (%mik-expands-p (prog* ((x 1 2)) (return x)))
        (%mik-expands-p (prog1))
        (%mik-expands-p (prog2 1))
        (%mik-expands-p (ccase 1 (2 :two)))
        (%mik-expands-p (ctypecase 1.5 (integer :int))))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error))

(deftest malformed-iteration-and-keyplace.valid-forms-unchanged
  (list (eval '(do ((i 0 (1+ i))) ((= i 3) i)))
        (eval '(do ((i 0 (1+ i)) j (k 5)) ((= i 2) (list i j k))))
        (eval '(do* ((i 0 (1+ i)) (j i)) ((= i 2) (list i j))))
        (eval '(dotimes (i 3 i)))
        (eval '(dotimes (i (+ 1 2) :r) nil))
        (eval '(dotimes (i 3)))
        (eval '(dolist (x '(1 2) :done)))
        (eval '(dolist (x '(1 2))))
        (eval '(prog ((x 1) y z) (return (list x y z))))
        (eval '(prog* ((x 1) (y x)) (return (list x y))))
        (eval '(prog1 1 2)) (eval '(prog2 1 2 3)) (eval '(prog1 1))
        (eval '(let ((x 1)) (ccase x (1 :one))))
        (eval '(let ((l (list 1))) (ccase (car l) (1 :one))))
        (eval '(let ((x 1.5)) (ctypecase x (real :real)))))
  (3 (2 nil 5) (2 0) 3 :r nil :done nil (1 nil nil) (1 1) 1 2 1 :one :one :real))

(deftest-emitting-only malformed-iteration-and-keyplace.compiler-checks
  (flet ((err (form) (handler-case (eval form) (program-error () :program-error)
                       (type-error () :type-error))))
    (list (err '(cond ()))
          (err '(cond (t :ok) 1))
          (err '(cond (nil) (t :b)))
          (err '(cond ((= 1 1) :a)))))
  (:type-error :type-error :b :a))
