;;; TRACE works on a COMMON-LISP function (SBCL encapsulates it; dotcl raised a PACKAGE-ERROR
;;; from the package lock) and the exit line shows every value the function returned.

(defun tcv-mv (x) (values x 2 3))
(defun tcv-one (x) x)

(defun %tcv-trace-output (thunk)
  (let ((*trace-output* (make-string-output-stream)))
    (funcall thunk)
    (get-output-stream-string *trace-output*)))

(deftest trace-cl-function-and-values.cl-function
  (let ((result nil))
    (let ((out (%tcv-trace-output
                (lambda ()
                  (trace cdddr)
                  (unwind-protect (setq result (funcall 'cdddr (list 1 2 3 4)))
                    (untrace cdddr))))))
      (list result (and (search "Calling CDDDR" out) t)
            (and (search "=> (4)" out) t)
            (funcall 'cdddr (list 1 2 3 4))
            (null (search "Calling" (%tcv-trace-output (lambda () (funcall 'cdddr (list 1 2 3 4)))))))))
  ((4) t t (4) t))

(deftest trace-cl-function-and-values.multiple-values
  (let ((out (%tcv-trace-output
              (lambda ()
                (trace tcv-mv tcv-one)
                (unwind-protect (progn (tcv-mv 1) (tcv-one 5))
                  (untrace tcv-mv tcv-one))))))
    (list (and (search "=> 1 2 3" out) t) (and (search "=> 5" out) t)
          (multiple-value-list (tcv-mv 1))))
  (t t (1 2 3)))
