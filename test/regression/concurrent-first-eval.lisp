;;; Several threads evaluating (compiling and assembling) forms at the same time:
;;; the assembler's lazily built tables must be built once and never seen half made.

(require "dotcl-thread")

(defun cfe-work (i)
  (list (eval `(let ((x ,i)) (declare (type fixnum x)) (+ x 1)))
        (eval `(funcall (lambda (a b) (* a b)) ,i 10))
        (eval `(length (make-array 3 :initial-element ,i)))))

(deftest-emitting-only concurrent-first-eval.four-threads
  (let* ((results (make-array 4 :initial-element nil))
         (threads (loop for i below 4
                        collect (let ((i i))
                                  (dotcl-thread:make-thread
                                   (lambda () (setf (aref results i) (cfe-work i))))))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    (coerce results 'list))
  ((1 0 3) (2 10 3) (3 20 3) (4 30 3)))
