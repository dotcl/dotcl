;;; Two LOAD-TIME-VALUE forms written separately each get a value of their own, as in SBCL.
;;; (The same cons used twice shares one: the sites are kept by form identity, so that a form the
;;; compiler walks twice is evaluated once. CLHS does not say what EQ forms do.)

(defun %ltv-two-forms ()
  (let ((f (compile nil '(lambda ()
                          (list (let ((xv (load-time-value (vector 1 2) nil)))
                                  (incf (svref xv 0)) (svref xv 0))
                                (let ((xv (load-time-value (vector 1 2) nil)))
                                  (incf (svref xv 0)) (svref xv 0)))))))
    (list (funcall f) (funcall f))))

(defun %ltv-copied-form ()
  (let* ((sub '(let ((xv (load-time-value (vector 1 2) nil)))
                (incf (svref xv 0)) (svref xv 0)))
         (f (compile nil `(lambda () (list ,sub ,(copy-tree sub))))))
    (list (funcall f) (funcall f))))

(deftest-compiled-only ltv-separate-forms.written-twice
  (%ltv-two-forms)
  ((2 2) (3 3)))

(deftest-compiled-only ltv-separate-forms.copy-of-the-same-form
  (%ltv-copied-form)
  ((2 2) (3 3)))
