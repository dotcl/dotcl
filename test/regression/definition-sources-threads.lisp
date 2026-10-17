;;; Definitions recorded from several threads at once all land in the table,
;;; also when their symbols share a name.

(require "dotcl-thread")

(deftest definition-sources-threads.all-recorded
  (let* ((pkgs (loop for i below 4
                     collect (let ((n (format nil "DS-THR-~d" i)))
                               (when (find-package n) (delete-package n))
                               (make-package n :use nil))))
         (threads (loop for p in pkgs
                        collect (let ((p p))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (dotimes (j 300)
                                       (dotcl:record-definition-sources
                                        (list 'defun (intern (format nil "F~d" j) p))
                                        "threads.lisp" (1+ j)))))))))
    (mapc #'dotcl-thread:thread-join threads)
    (prog1
        (loop for p in pkgs
              always (loop for j below 300
                           always (equal (dotcl:definition-sources (find-symbol (format nil "F~d" j) p))
                                         `((:kind :function :file "threads.lisp" :line ,(1+ j))))))
      (mapc #'delete-package pkgs)))
  t)
