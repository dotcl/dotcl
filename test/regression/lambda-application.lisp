;;; (funcall (lambda (a b) ...) x y) and ((lambda (a b) ...) x y) bind the parameters in place; the
;;; values, the order of evaluation and the scope are those of a call.

(defvar cl-user::*lap-special* :global)

(deftest lap.values
  (list (funcall (lambda (a b) (list a b)) 1 2)
        ((lambda (a b) (+ a b)) 3 4)
        (funcall #'(lambda (a) (* a a)) 5)
        (funcall (lambda ()) )
        (funcall (lambda () 7))
        (multiple-value-list (funcall (lambda (a) (values a (1+ a) (+ a 2))) 1))
        (multiple-value-list ((lambda (a) (values)) 1)))
  ((1 2) 7 25 nil 7 (1 2 3) nil))

(deftest lap.argument-order-and-scope
  (let ((log '()) (x 10))
    (list (funcall (lambda (a b) (list a b x))
                   (progn (push :first log) (incf x))
                   (progn (push :second log) (incf x)))
          (reverse log)
          ;; the arguments see the outer X, not the parameter of the same name
          (funcall (lambda (x y) (list x y)) (+ x 1) (* x 2))
          (let ((x 1)) ((lambda (x) (+ x 100)) (+ x 1)))))
  ((11 12 12) (:first :second) (13 24) 102))

(deftest lap.special-parameter-and-closures
  (list (funcall (lambda (cl-user::*lap-special*) (symbol-value 'cl-user::*lap-special*)) :bound)
        cl-user::*lap-special*
        (let ((fs (list (funcall (lambda (n) (lambda () n)) 1) (funcall (lambda (n) (lambda () (incf n))) 10))))
          (list (funcall (first fs)) (funcall (second fs)) (funcall (second fs)))))
  (:bound :global (1 11 12)))

(deftest lap.declarations-and-types
  (list (funcall (lambda (r) (declare (fixnum r)) (> r 5)) 9)
        (handler-case (funcall (lambda (r) (declare (type fixnum r)) r) "x") (type-error () :type-error))
        (funcall (lambda (r) (declare (ignore r)) :ignored) 1))
  (t :type-error :ignored))

(deftest lap.other-lambda-lists-still-work
  (list (funcall (lambda (a &optional (b 2)) (list a b)) 1)
        (funcall (lambda (a &rest r) (list a r)) 1 2 3)
        (funcall (lambda (&key k) k) :k 5)
        (handler-case (funcall (lambda (a b) (list a b)) 1) (error () :arg-count-error))
        (handler-case (eval '((lambda (a) a) 1 2)) (error () :arg-count-error)))
  ((1 2) (1 (2 3)) 5 :arg-count-error :arg-count-error))

(defun %lap-loop (n)
  (declare (fixnum n))
  (let ((k 0) (lim 5))
    (declare (fixnum k))
    (dotimes (i n) (when (funcall (lambda (r) (> r lim)) (mod i 10)) (incf k)))
    k))

(deftest lap.loop-value
  (%lap-loop 1000) 400)

(deftest-compiled-only lap.no-closure-per-call
  (progn (%lap-loop 100)
         (list (< (min-allocated-bytes (lambda () (%lap-loop 100000))) 100000)))
  (t))

;;; Declarations that belong to the lambda's own body keep the lambda: NOTINLINE (a compiler macro it
;;; suppresses), INLINE, OPTIMIZE (the safety its stores are compiled at), FTYPE, SPECIAL.
(defun %lap-cm-fn (x) (* x 1000))
(define-compiler-macro %lap-cm-fn (x) `(* ,x 2))

(deftest-compiled-only lap.notinline-in-the-lambda-body
  (list (funcall (lambda (x) (%lap-cm-fn x)) 5)
        (funcall (lambda (x) (declare (notinline %lap-cm-fn)) (%lap-cm-fn x)) 5)
        ((lambda (x) (declare (notinline %lap-cm-fn)) (%lap-cm-fn x)) 5)
        (locally (declare (notinline funcall)) (funcall (lambda (x) (%lap-cm-fn x)) 5)))
  (10 5000 5000 10))

(defstruct lap-box (n 0 :type fixnum))

(defun %lap-safety (v level)
  (handler-case
      (ecase level
        (0 (funcall (lambda (v) (declare (optimize (safety 0))) (let ((b (make-lap-box))) (setf (lap-box-n b) v) (lap-box-n b))) v))
        (1 (funcall (lambda (v) (declare (optimize (safety 1))) (let ((b (make-lap-box))) (setf (lap-box-n b) v) (lap-box-n b))) v))
        (3 (funcall (lambda (v) (declare (optimize (safety 3))) (let ((b (make-lap-box))) (setf (lap-box-n b) v) (lap-box-n b))) v)))
    (type-error () :type-error)))

(deftest lap.safety-declared-in-the-lambda
  (list (%lap-safety 42 0) (%lap-safety 42 1) (%lap-safety 42 3)
        (%lap-safety "x" 1) (%lap-safety "x" 3))
  (42 42 42 :type-error :type-error))

(deftest lap.inline-ftype-special-keep-the-lambda
  (list (funcall (lambda (x) (declare (inline car)) (car x)) '(1 2))
        (funcall (lambda (x) (declare (ftype (function (t) t) %lap-cm-fn) (ignorable x)) x) 4)
        (funcall (lambda (cl-user::*lap-special*) (declare (special cl-user::*lap-special*)) (symbol-value 'cl-user::*lap-special*)) :s))
  (1 4 :s))

(deftest lap.type-violating-arguments
  (list (handler-case (funcall (lambda (n) (declare (fixnum n)) (1+ n)) "x") (type-error () :type-error))
        (handler-case ((lambda (n) (declare (type (integer 0 10) n)) n) 11) (type-error () :type-error))
        (funcall (lambda (n) (declare (type (integer 0 10) n)) n) 7)
        (handler-case (funcall (lambda (s) (declare (type simple-string s)) (length s)) 5) (type-error () :type-error)))
  (:type-error :type-error 7 :type-error))

;;; A lambda that declares (optimize (speed 3) (safety 0)) (closure-common's READ-RUNE and friends)
;;; is applied in place too, at that safety.
(defstruct lap-str (s "" :type string))

(defun %lap-opt (v level)
  (handler-case
      (ecase level
        (:unsafe ((lambda (v) (declare (optimize (speed 3) (safety 0)))
                    (let ((b (make-lap-str))) (setf (lap-str-s b) v) (lap-str-s b))) v))
        (:safe ((lambda (v) (declare (optimize (speed 3) (safety 1)))
                  (let ((b (make-lap-str))) (setf (lap-str-s b) v) (lap-str-s b))) v))
        (:plain ((lambda (v) (declare (optimize (speed 3)))
                   (let ((b (make-lap-str))) (setf (lap-str-s b) v) (lap-str-s b))) v)))
    (type-error () :type-error)))

(deftest lap.optimize-declaration-keeps-its-safety
  (list (%lap-opt 42 :unsafe) (%lap-opt 42 :safe) (%lap-opt 42 :plain) (%lap-opt "ok" :unsafe))
  (42 :type-error :type-error "ok"))

(defun %lap-fast-loop (n)
  (declare (fixnum n))
  (let ((k 0) (v (make-array 4 :element-type '(unsigned-byte 8) :initial-element 7)))
    (declare (fixnum k))
    (dotimes (i n)
      (incf k (the fixnum ((lambda (x) (declare (type (simple-array (unsigned-byte 8) (*)) x) (optimize (speed 3) (safety 0)))
                              (aref x 2)) v))))
    k))

(deftest-compiled-only lap.fast-lambda-no-closure
  (progn (%lap-fast-loop 100)
         (list (%lap-fast-loop 1000)
               (< (min-allocated-bytes (lambda () (%lap-fast-loop 100000))) 100000)))
  (7000 t))
