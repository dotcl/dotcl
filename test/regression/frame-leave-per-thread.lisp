;;; A function's debugger frame keeps where its thread's frame list lives, and
;;; unlinks itself through that on return instead of looking the thread-static
;;; up again. Each thread must unlink from its own list: calls on several
;;; threads at once, then the backtrace on each, show only that thread's frames.

(defun %flpt-leaf (tag) (list tag (loop for f in (dotcl:backtrace)
                                         when (member f '("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")
                                                      :test #'string=)
                                           collect f)))
(defun %flpt-mid (tag) (let ((r (%flpt-leaf tag))) (if r r nil)))
(defun %flpt-bad (x) (let ((r (car x))) (if r r nil)))
(defun %flpt-mid-bad (x) (let ((r (%flpt-bad x))) (if r r nil)))
(defun %flpt-top (tag n)
  (let ((last nil))
    (dotimes (i n) (setf last (%flpt-mid tag)))
    (list last (%flpt-mid tag))))

(deftest frame-leave-per-thread.threads
  (let* ((threads (loop for k below 4
                        collect (let ((k k))
                                  (dotcl:make-thread (lambda () (%flpt-top k 20000))))))
         (results (mapcar #'dotcl:thread-join threads)))
    (list results (%flpt-top :main 3)))
  ((((0 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")) (0 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")))
    ((1 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")) (1 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")))
    ((2 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")) (2 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")))
    ((3 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")) (3 ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP"))))
   ((:main ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")) (:main ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")))))

(deftest frame-leave-per-thread.after-error
  (list (handler-case (%flpt-mid-bad 1) (type-error () :caught))
        (%flpt-top :again 2))
  (:caught ((:again ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP"))
            (:again ("%FLPT-LEAF" "%FLPT-MID" "%FLPT-TOP")))))
