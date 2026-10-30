;;; A non-tail call a DEFUN makes to itself, when every value its body can
;;; return is a single value, takes the result as it is (no MultipleValues.Reset
;;; before it, no value mode, no UnwrapMv after it). A body that can return
;;; several values, or return early from its block, keeps them. The truth test
;;; of a value that has been through UnwrapMv compares with NIL directly.

(setf dotcl:*save-sil* t)

(defun %svs-fib (n) (if (< n 2) n (+ (%svs-fib (- n 1)) (%svs-fib (- n 2)))))
(defun %svs-tak (x y z)
  (if (not (< y x)) z (%svs-tak (%svs-tak (1- x) y z) (%svs-tak (1- y) z x) (%svs-tak (1- z) x y))))
(defun %svs-cond (n)
  (cond ((< n 1) 0) ((evenp n) (let ((r (%svs-cond (1- n)))) (+ r 2))) (t (1+ (%svs-cond (1- n))))))
;; One branch returns two values: the self-call result is unwrapped.
(defun %svs-mv (n) (if (< n 1) (values 10 20) (+ 1 (%svs-mv (1- n)))))
;; The tail is an unknown call: may be several values.
(defun %svs-other () (values 5 6))
(defun %svs-unknown (n) (if (< n 1) (%svs-other) (+ 1 (%svs-unknown (1- n)))))
;; An early RETURN-FROM with two values.
(defun %svs-early (n)
  (when (< n 1) (return-from %svs-early (values 7 8)))
  (+ 1 (%svs-early (1- n))))
;; The same through a macro.
(defmacro %svs-bail (name v) `(return-from ,name (values ,v :extra)))
(defun %svs-early-macro (n)
  (if (< n 1) (%svs-bail %svs-early-macro 3) nil)
  (+ 1 (%svs-early-macro (1- n))))
;; The self-call's values are asked for.
(defun %svs-mvl (n) (if (< n 1) 0 (car (multiple-value-list (%svs-mvl (1- n))))))

(defun %svs-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))
;; A call through LispFunction.InvokeN followed by Runtime.UnwrapMv.
(defun %svs-unwraps-call (fn)
  (loop for (a b) on (dotcl:function-sil fn)
        thereis (and (consp a) (consp b)
                     (string= (symbol-name (car a)) "CALLVIRT")
                     (stringp (cadr a)) (search "LispFunction.Invoke" (cadr a))
                     (string= (symbol-name (car b)) "CALL")
                     (equal (cadr b) "Runtime.UnwrapMv"))))

(deftest single-value-self-call.values
  (list (%svs-fib 15) (%svs-tak 12 8 4) (%svs-cond 5)
        (%svs-mv 3) (multiple-value-list (%svs-mv 0))
        (%svs-unknown 2) (multiple-value-list (%svs-unknown 0))
        (%svs-early 2) (multiple-value-list (%svs-early 0))
        (%svs-early-macro 2) (multiple-value-list (%svs-early-macro 0))
        (%svs-mvl 3))
  (610 5 7 13 (10 20) 7 (5 6) 9 (7 8) 5 (3 :extra) 0))

(deftest-compiled-only single-value-self-call.sil
  (list (%svs-has "UnwrapMv" #'%svs-fib) (%svs-has "Invoke1M" #'%svs-fib)
        (%svs-unwraps-call #'%svs-tak) (%svs-unwraps-call #'%svs-cond)
        (%svs-unwraps-call #'%svs-mv) (%svs-unwraps-call #'%svs-unknown)
        (%svs-unwraps-call #'%svs-early))
  (nil nil nil nil t t t))

;; Redefined while it runs: the running body's self-calls stay on that body.
(defun %svs-redef (n)
  (if (< n 1)
      (progn (eval '(defun %svs-redef (n) (values n :new))) 0)
      (+ 1 (%svs-redef (1- n)))))

(deftest-emitting-only single-value-self-call.redefined-while-running
  (let ((r (%svs-redef 3)))
    (list r (multiple-value-list (%svs-redef 4))))
  (3 (4 :new)))

(defun %svs-nil-t () (values nil t))
(defun %svs-truth (x)
  (list (if (%svs-nil-t) :yes :no)
        (or (%svs-nil-t) :or-next)
        (cond ((%svs-nil-t) :c1) (t :c2))
        (when (identity x) :when)
        (and (%svs-nil-t) :and-next)))

(deftest single-value-self-call.truth-of-primary
  (list (%svs-truth 1) (%svs-truth nil))
  ((:no :or-next :c2 :when nil) (:no :or-next :c2 nil nil)))

(setf dotcl:*save-sil* nil)
