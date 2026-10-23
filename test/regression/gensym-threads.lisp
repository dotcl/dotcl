;;; GENSYM from several threads that share the global *GENSYM-COUNTER*: every
;;; name is different and the counter advances once per call. The counter was
;;; read and written back without a lock, so threads handed out the same number
;;; and increments were lost.

(require "dotcl-thread")

(defun %gst-global-counter ()
  (let ((v nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread (lambda () (setf v *gensym-counter*))))
    v))

(deftest gensym-threads.distinct-names
  (let* ((nthreads 4) (per 20000) (go nil)
         (names (make-array nthreads))
         (start (%gst-global-counter))
         (threads (loop for i below nthreads
                        collect (let ((i i))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (loop until go do (dotcl-thread:thread-yield))
                                     (setf (aref names i)
                                           (loop repeat per collect (symbol-name (gensym "GST"))))))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)
    (let ((h (make-hash-table :test 'equal)) (dups 0))
      (loop for v across names
            do (dolist (n v) (if (gethash n h) (incf dups) (setf (gethash n h) t))))
      ;; At least: interpreted code expanding macros meanwhile takes gensyms too.
      (list dups (>= (- (%gst-global-counter) start) 80000))))
  (0 t))
