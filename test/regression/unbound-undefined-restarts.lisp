;;; SBCL's restarts around an unbound variable and an undefined function.

(defvar *udr-u* 0)
(defun %udr-read () *udr-u*)
(defun %udr-mark () (makunbound '*udr-u*))
(defun %udr-unmark () (setq *udr-u* 0))

(deftest udr-unbound-use-value
  (unwind-protect
       (progn
         (%udr-mark)
         (handler-bind ((unbound-variable
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'use-value 42))))
           (%udr-read)))
    (%udr-unmark))
  42)

(deftest udr-unbound-store-value
  (unwind-protect
       (progn
         (%udr-mark)
         (list (handler-bind ((unbound-variable
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'store-value 43))))
                 (%udr-read))
               *udr-u*))
    (%udr-unmark))
  (43 43))

(deftest udr-unbound-value-functions
  (unwind-protect
       (progn
         (%udr-mark)
         (list (handler-bind ((unbound-variable
                                (lambda (c) (use-value 51 c))))
                 (%udr-read))
               (handler-bind ((unbound-variable
                                (lambda (c) (declare (ignore c))
                                  (store-value 52))))
                 (%udr-read))
               *udr-u*))
    (%udr-unmark))
  (51 52 52))

(deftest udr-unbound-symbol-value
  (unwind-protect
       (progn
         (%udr-mark)
         (handler-bind ((unbound-variable
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'use-value 61))))
           (symbol-value '*udr-u*)))
    (%udr-unmark))
  61)

(deftest udr-unbound-eval-use-value
  (unwind-protect
       (progn
         (%udr-mark)
         (handler-bind ((unbound-variable
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'use-value 71))))
           (eval '*udr-u*)))
    (%udr-unmark))
  71)

(deftest udr-unbound-restarts
  (unwind-protect
       (progn
         (%udr-mark)
         (let (found)
           (handler-bind ((unbound-variable
                            (lambda (c)
                              (setq found
                                    (loop for r in (compute-restarts c)
                                          for n = (restart-name r)
                                          when (member n '(use-value store-value))
                                            collect (list (symbol-name n)
                                                          (princ-to-string r))))
                              (invoke-restart 'use-value 1))))
             (%udr-read))
           found))
    (%udr-unmark))
  (("USE-VALUE" "Use specified value.")
   ("STORE-VALUE" "Set specified value and use it.")))

(defun %udr-missing-call (x) (udr-no-such-function x))

(deftest udr-undefined-use-value
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'use-value (lambda (x) (+ x 100))))))
    (%udr-missing-call 1))
  101)

(deftest udr-undefined-use-value-symbol
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (setf (symbol-function 'udr-use-late)
                           (lambda (x) (* x 2)))
                     (invoke-restart 'use-value 'udr-use-late))))
    (%udr-missing-call 21))
  42)

(deftest udr-undefined-return-value
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'return-value 7 8))))
    (multiple-value-list (%udr-missing-call 1)))
  (7 8))

(deftest udr-undefined-return-nothing
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'return-nothing))))
    (multiple-value-list (%udr-missing-call 1)))
  nil)

(defun %udr-continue-call (x) (udr-continue-late x))

(deftest udr-undefined-continue
  (let ((done nil))
    (list
     (handler-bind ((undefined-function
                      (lambda (c)
                        (declare (ignore c))
                        (unless done
                          (setf (symbol-function 'udr-continue-late)
                                (lambda (x) (list :late x)))
                          (setq done t))
                        (invoke-restart 'continue))))
       (%udr-continue-call 5))
     done))
  ((:late 5) t))

(deftest udr-undefined-eval-use-value
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'use-value (lambda (x) (+ x 100))))))
    (eval '(udr-no-such-eval-function 3)))
  103)

(deftest udr-undefined-restarts
  (let (found)
    (handler-bind ((undefined-function
                     (lambda (c)
                       (setq found
                             (loop for r in (compute-restarts c)
                                   for n = (restart-name r)
                                   when (member (symbol-name n)
                                                '("USE-VALUE" "RETURN-VALUE"
                                                  "RETURN-NOTHING" "CONTINUE")
                                                :test #'string=)
                                     collect (list (symbol-name n)
                                                   (princ-to-string r))))
                       (invoke-restart 'use-value #'identity))))
      (%udr-missing-call 5))
    found)
  (("CONTINUE" "Retry calling UDR-NO-SUCH-FUNCTION.")
   ("USE-VALUE" "Call specified function.")
   ("RETURN-VALUE" "Return specified values.")
   ("RETURN-NOTHING" "Return zero values.")))

(defvar *udr-setf-value* nil)
(defun %udr-setf-call (v) (setf (udr-no-such-setf) v))

(deftest udr-undefined-setf-use-value
  (handler-bind ((undefined-function
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'use-value
                                     (lambda (v) (setq *udr-setf-value* v))))))
    (%udr-setf-call 9))
  9)
