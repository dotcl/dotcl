;;; A LABELS of one self-tail-recursive function, called once from where it stands (not the tail of
;;; the function), is compiled as a loop with no closure. Results must be those of the closure form.

(defun %ltl-count-down (n)
  (let ((acc 0))
    (dotimes (i 3)
      (incf acc (labels ((f (k a) (if (<= k 0) a (f (1- k) (+ a k))))) (f n 0))))
    acc))

(deftest labels-tail-loop.in-loop-body (%ltl-count-down 10) 165)

(defun %ltl-string-walk (strings)
  (let ((n 0))
    (loop for s in strings
          do (labels ((rec (x)
                        (etypecase x
                          (string (incf n (length x)))
                          (symbol (rec (symbol-name x))))))
               (rec s)))
    n))

(deftest labels-tail-loop.etypecase-macro (%ltl-string-walk (list "ab" 'cde "f" 'gh)) 8)

(defun %ltl-as-argument (n) (+ 1 (labels ((f (k) (cond ((zerop k) 100) (t (f (1- k)))))) (f n)) 1))
(deftest labels-tail-loop.as-argument (%ltl-as-argument 5) 102)

(defun %ltl-values (n)
  (list (multiple-value-list (labels ((f (k a) (if (zerop k) (values a k 'done) (f (1- k) (cons k a))))) (f n nil)))))
(deftest labels-tail-loop.multiple-values (%ltl-values 3) (((1 2 3) 0 done)))

(defun %ltl-return-from (n)
  (list (labels ((f (k) (when (> k 5) (return-from f :big)) (if (zerop k) :zero (f (1- k))))) (f n))
        (labels ((f (k) (when (> k 5) (return-from f :big)) (if (zerop k) :zero (f (1- k))))) (f 20))))
(deftest labels-tail-loop.return-from (%ltl-return-from 3) (:zero :big))

;; Each iteration has its own binding of the parameter: closures made in different iterations
;; keep different values.
(defun %ltl-closures ()
  (let ((fns '()))
    (list (progn (labels ((f (i) (if (< i 3) (progn (push (lambda () i) fns) (f (1+ i))) i))) (f 0))
                 (mapcar #'funcall (reverse fns))))))
(deftest labels-tail-loop.fresh-binding (%ltl-closures) ((0 1 2)))

(defun %ltl-declared (n)
  (let ((r 0))
    (dotimes (i 2) (setq r (labels ((f (k) (declare (fixnum k)) (if (zerop k) :ok (f (1- k))))) (f n))))
    r))
(deftest labels-tail-loop.declared-param (%ltl-declared 4) :ok)

;; Shapes that stay closures.
(defun %ltl-escape (n)
  (let ((r (labels ((f (k) (if (zerop k) #'f (f (1- k))))) (f n))))
    (functionp r)))
(deftest labels-tail-loop.escape-function-value (%ltl-escape 3) t)

(defun %ltl-fib (n) (let ((r 0)) (dotimes (i 1) (setq r (labels ((f (k) (if (< k 2) k (+ (f (- k 1)) (f (- k 2)))))) (f n)))) r))
(deftest labels-tail-loop.non-tail-self-call (%ltl-fib 10) 55)

(defun %ltl-funcall (n) (let ((r 0)) (dotimes (i 1) (setq r (labels ((f (k) (if (zerop k) :end (funcall #'f (1- k))))) (f n)))) r))
(deftest labels-tail-loop.funcall-self (%ltl-funcall 4) :end)

(defun %ltl-in-lambda (n)
  (let ((r 0)) (dotimes (i 1) (setq r (labels ((f (k) (if (zerop k) 0 (funcall (lambda () (f (1- k))))))) (f n)))) r))
(deftest labels-tail-loop.self-call-in-lambda (%ltl-in-lambda 4) 0)

(defun %ltl-unwind (n)
  (let ((log '()))
    (dotimes (i 1)
      (labels ((f (k) (if (zerop k) :end (unwind-protect (f (1- k)) (push k log))))) (f n)))
    (reverse log)))
(deftest labels-tail-loop.self-call-in-unwind-protect (%ltl-unwind 3) (1 2 3))

(defmacro %ltl-recur (x) `(%ltl-hidden ,x))
(defun %ltl-hidden-user (n)
  (let ((r 0)) (dotimes (i 1) (setq r (labels ((%ltl-hidden (k) (if (zerop k) :hidden (%ltl-recur (1- k))))) (%ltl-hidden n)))) r))
(deftest labels-tail-loop.macro-hidden-self-call (%ltl-hidden-user 3) :hidden)

(defun %ltl-mutual (n)
  (let ((r 0)) (dotimes (i 1) (setq r (labels ((ev (k) (if (zerop k) :even (od (1- k)))) (od (k) (if (zerop k) :odd (ev (1- k))))) (ev n)))) r))
(deftest labels-tail-loop.mutual (%ltl-mutual 5) :odd)

(defvar *%ltl-special* 0)
(defun %ltl-special-param (n)
  (let ((r 0)) (dotimes (i 1) (setq r (labels ((f (*%ltl-special*) (declare (special *%ltl-special*)) (if (> *%ltl-special* 2) *%ltl-special* (f (1+ *%ltl-special*))))) (f n)))) r))
(deftest labels-tail-loop.special-param (%ltl-special-param 0) 3)

(defun %ltl-no-leak-of-name (n)
  (let ((r 0)) (dotimes (i 1) (setq r (labels ((f (k) (if (zerop k) 'f (f (1- k))))) (f n)))) r))
(deftest labels-tail-loop.symbol-named-like-function (%ltl-no-leak-of-name 2) f)

;; A tail call made under a special binding is no tail call: the binding is still in force while the
;; callee runs, so it must not be taken for a jump.
(defvar *%ltl-dyn* 0)
(defun %ltl-dynamic (n)
  (let ((r 0))
    (dotimes (i 1)
      (setq r (labels ((f (k) (if (zerop k) *%ltl-dyn* (let ((*%ltl-dyn* (1+ *%ltl-dyn*))) (f (1- k))))))
                (f n))))
    r))
(deftest labels-tail-loop.special-binding-around-self-call (%ltl-dynamic 3) 3)

(defun %ltl-dynamic-declared (n)
  (let ((r 0))
    (dotimes (i 1)
      (setq r (labels ((f (k) (if (zerop k) *%ltl-dyn* (locally (declare (special *%ltl-dyn*)) (let ((*%ltl-dyn* (+ 2 *%ltl-dyn*))) (f (1- k)))))))
                (f n))))
    r))
(deftest labels-tail-loop.special-binding-in-locally (%ltl-dynamic-declared 2) 4)
