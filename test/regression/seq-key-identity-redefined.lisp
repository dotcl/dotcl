;;; :KEY #'IDENTITY is no key only while IDENTITY is the function the system defined. A program
;;; that redefines CL:IDENTITY gets its function called, as it does in other implementations.

(deftest seq-key-identity-redefined.calls-the-new-function
  (let ((old (fdefinition 'identity)))
    (unwind-protect
         (progn
           (dotcl:without-package-locks
             (setf (fdefinition 'identity) (lambda (x) (if (numberp x) (* x 10) x))))
           (list (position 20 '(1 2 3) :key #'identity)
                 (position 20 '(1 2 3) :key 'identity)
                 (find 30 '(1 2 3) :key (function identity))
                 (count 20 '(1 2 3) :key (symbol-function 'identity))
                 (remove 10 '(1 2 3) :key #'identity)
                 (position 2 '(1 2 3) :key #'identity)))
      (dotcl:without-package-locks (setf (fdefinition 'identity) old))))
  (1 1 3 1 (2 3) nil))

(deftest seq-key-identity-redefined.restored
  (list (position 2 '(1 2 3) :key #'identity) (position 20 '(1 2 3) :key #'identity))
  (1 nil))
