;;; MULTIPLE-VALUE-BIND as MACROEXPAND-1 shows it.
;;;
;;; The expansion used to be the compiler's own
;;;   (let* ((#:p (%mv-capture form)) (a (%mv-nth 0)) (b (%mv-nth 1))) ...)
;;; where %MV-NTH reads the values of the most recent %MV-CAPTURE from a
;;; per-thread snapshot. A code walker that rewrites the expansion (cl-cont's
;;; CPS transform puts a continuation call between the two) lost every value
;;; but the first. MACROEXPAND-1 now gives the portable form, which passes the
;;; values through MULTIPLE-VALUE-CALL, and the compiler lowers that form back
;;; to the snapshot binding, so it costs the same as MULTIPLE-VALUE-BIND.

(deftest mvbp-expansion-is-mv-call
  (let ((exp (macroexpand-1 '(multiple-value-bind (a b) (f) (list a b)))))
    (list (car exp)
          (car (cadr exp))
          (car (cadr (cadr exp)))
          (subseq (cadr (cadr (cadr exp))) 0 3)
          (car (last exp))))
  (multiple-value-call function lambda (&optional a b) (f)))

(deftest mvbp-expansion-evaluates
  (list (eval (macroexpand-1 '(multiple-value-bind (a b c) (values 1 2) (list a b c))))
        (eval (macroexpand-1 '(multiple-value-bind (q r) (floor 17 5)
                               (declare (fixnum q))
                               (list q r))))
        (eval (macroexpand-1 '(multiple-value-bind () (values 1 2) :none))))
  ((1 2 nil) (3 2) :none))

;; The values survive something running between the values form and the binding.
(defun mvbp-noise () (multiple-value-bind (x y) (floor 7 2) (list x y)))
(deftest mvbp-values-survive-intervening-call
  (let ((exp (macroexpand-1 '(multiple-value-bind (a b) (floor 17 5) (list a b)))))
    (eval `(multiple-value-call ,(cadr exp)
             (multiple-value-prog1 ,(caddr exp) (mvbp-noise)))))
  (3 2))

;; The compiler takes the portable shape as a binding, not a closure call.
(defun mvbp-portable (x)
  (multiple-value-call #'(lambda (&optional q r &rest ig) (declare (ignore ig)) (+ q r))
    (floor x 3)))
(defun mvbp-direct (x)
  (multiple-value-bind (q r) (floor x 3) (+ q r)))

(deftest mvbp-portable-value
  (list (mvbp-portable 10) (mvbp-direct 10)
        (multiple-value-call #'(lambda (&optional a (b) &rest r) (declare (ignore r)) (list a b))
          (values 1))
        ;; &rest that is used is still a real call
        (multiple-value-call #'(lambda (&optional a &rest r) (list a r)) (values 1 2 3)))
  (4 4 (1 nil) (1 (2 3))))

(deftest mvbp-portable-setq-outer
  (let ((s 0))
    (dotimes (i 3)
      (multiple-value-call #'(lambda (&optional q r &rest ig) (declare (ignore ig))
                               (setq s (+ s q r)))
        (floor (+ 10 i) 3)))
    s)
  13)

(deftest mvbp-portable-closure-capture
  (let ((fns (multiple-value-call #'(lambda (&optional q r &rest ig) (declare (ignore ig))
                                      (list (lambda () q) (lambda () (setq r (1+ r)) r)))
               (floor 17 5))))
    (list (funcall (first fns)) (funcall (second fns)) (funcall (second fns))))
  (3 3 4))

(setf dotcl:*save-sil* t)
(defun mvbp-portable-sil (x)
  (multiple-value-call #'(lambda (&optional q r &rest ig) (declare (ignore ig)) (+ q r))
    (floor x 3)))
(deftest-emitting-only mvbp-portable-is-snapshot-bind
  (let ((sil (princ-to-string (dotcl:function-sil #'mvbp-portable-sil))))
    (list (and (search "CaptureForBind" sil) t)
          (and (search "MultipleValuesList" sil) t)))
  (t nil))
