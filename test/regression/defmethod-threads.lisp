;;; DEFMETHOD of a generic function that does not exist yet creates it. Two
;;; threads doing that at once both saw no generic function, both made one, and
;;; the second registration replaced the first, so the method the first thread
;;; added to its own was lost. Run with parallel EVAL, as a host that lifts the
;;; EVAL lock does.

(require "dotcl-thread")

(deftest defmethod-threads.auto-created-generic-function
  (let* ((rounds 60)
         (classes '(integer string symbol cons))
         (names (loop for r below rounds
                      collect (intern (format nil "%DMT-GF-~D" r))))
         (was-parallel (dotcl:parallel-eval-p)))
    (dotcl:set-parallel-eval t)
    (unwind-protect
         (let ((threads
                 (loop for c in classes
                       collect (let ((c c))
                                 (dotcl-thread:make-thread
                                  (lambda ()
                                    (dolist (g names)
                                      (eval `(defmethod ,g ((x ,c)) ',c)))))))))
           (mapc #'dotcl-thread:thread-join threads))
      (dotcl:set-parallel-eval was-parallel))
    (loop for g in names
          count (notevery (lambda (x c) (eq (ignore-errors (funcall g x)) c))
                          '(1 "s" a (1)) classes)))
  0)
