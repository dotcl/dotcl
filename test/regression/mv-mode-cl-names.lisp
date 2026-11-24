;;; A DEFMETHOD on a COMMON-LISP function does not change the code compiled for
;;; the calls of that function that come after it.

(setf dotcl:*save-sil* t)

(defclass %mmc-thing () ())

(defun %mmc-before (x) (let ((d (documentation x 'function))) (list d)))

(defmethod documentation ((x %mmc-thing) (y (eql 'function))) "thing")

(defun %mmc-after (x) (let ((d (documentation x 'function))) (list d)))

(defun %mmc-mode-calls (fn)
  (let ((text (%flat-text (dotcl:function-sil fn))) (n 0) (start 0))
    (loop (let ((p (search "Invoke2M" text :start2 start)))
            (if p (setq n (1+ n) start (1+ p)) (return n))))))

(deftest-emitting-only mv-mode-cl-names.same-code
  (list (%mmc-mode-calls #'%mmc-before) (%mmc-mode-calls #'%mmc-after))
  (0 0))

(deftest mv-mode-cl-names.value
  (list (documentation (make-instance '%mmc-thing) 'function)
        (car (%mmc-after (make-instance (quote %mmc-thing)))))
  ("thing" "thing"))
