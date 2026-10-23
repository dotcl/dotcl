;;; COMPILE evaluates the LOAD-TIME-VALUE forms of the lambda it compiles, once
;;; each and in the order they appear, and treats the values as literals (CLHS
;;; LOAD-TIME-VALUE). They were evaluated on the first call instead.

(defvar *ltv-compile-count* 0)
(defvar *ltv-compile-order* '())

(deftest-emitting-only load-time-value-compile.compile
  (progn
    (setf *ltv-compile-count* 0 *ltv-compile-order* '())
    (let ((f (compile nil '(lambda ()
                             (list (load-time-value
                                    (progn (push :first *ltv-compile-order*)
                                           (incf *ltv-compile-count*)))
                                   (load-time-value
                                    (progn (push :second *ltv-compile-order*)
                                           (incf *ltv-compile-count* 10))
                                    t))))))
      (let ((at-compile (list (reverse *ltv-compile-order*) *ltv-compile-count*)))
        (list at-compile (funcall f) (funcall f) *ltv-compile-count*))))
  (((:first :second) 11) (1 11) (1 11) 11))

(deftest-emitting-only load-time-value-compile.named
  (progn
    (setf *ltv-compile-count* 0 *ltv-compile-order* '())
    (compile 'ltv-compile-named
             '(lambda ()
                (load-time-value
                 (progn (push :named *ltv-compile-order*)
                        (incf *ltv-compile-count* 100)))))
    (list (reverse *ltv-compile-order*) *ltv-compile-count*
          (ltv-compile-named) *ltv-compile-count*))
  ((:named) 100 100 100))

;;; EVAL of a DEFUN or LAMBDA compiles in a build that compiles what it
;;; evaluates, and evaluates their LOAD-TIME-VALUE forms then too.
(deftest-compiled-only load-time-value-compile.eval-lambda
  (progn
    (setf *ltv-compile-count* 0)
    (let ((f (eval '(lambda () (load-time-value (incf *ltv-compile-count*))))))
      (list *ltv-compile-count* (funcall f) *ltv-compile-count*)))
  (1 1 1))

(deftest-compiled-only load-time-value-compile.eval-defun
  (progn
    (setf *ltv-compile-count* 0)
    (eval '(defun ltv-compile-eval-defun ()
             (load-time-value (incf *ltv-compile-count*))))
    (list *ltv-compile-count* (ltv-compile-eval-defun) *ltv-compile-count*))
  (1 1 1))

(deftest-compiled-only load-time-value-compile.eval-direct
  (progn
    (setf *ltv-compile-count* 0)
    (list (eval '(load-time-value (incf *ltv-compile-count*)))
          *ltv-compile-count*))
  (1 1))
