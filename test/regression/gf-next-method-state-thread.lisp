;;; The next-method state of a method chain is per thread. A thread whose first
;;; generic function call runs a chain, a chain that calls another chain from the
;;; middle of a method, and CALL-NEXT-METHOD after that inner call returns or
;;; unwinds through a non-local exit.

(require "dotcl-thread")

(defclass %gnmt-a () ())
(defclass %gnmt-b (%gnmt-a) ())
(defclass %gnmt-c (%gnmt-b) ())

(defgeneric %gnmt-in (x y))
(defmethod %gnmt-in ((x %gnmt-a) y) (list :in-a y))
(defmethod %gnmt-in ((x %gnmt-b) y) (cons :in-b (call-next-method)))

(defgeneric %gnmt-out (x y))
(defmethod %gnmt-out ((x %gnmt-a) y) (list :out-a y))
(defmethod %gnmt-out ((x %gnmt-b) y)
  (let ((inner (%gnmt-in x (1+ y))))
    (list* :out-b inner (call-next-method))))
(defmethod %gnmt-out ((x %gnmt-c) y)
  (catch 'skip (%gnmt-in x (throw 'skip nil)))
  (cons :out-c (call-next-method)))

(defgeneric %gnmt-and (x) (:method-combination and))
(defmethod %gnmt-and and ((x %gnmt-a)) (%gnmt-out x 0))
(defmethod %gnmt-and and ((x %gnmt-b)) (next-method-p))

(defun %gnmt-run ()
  (let ((c (make-instance '%gnmt-c)))
    (list (%gnmt-out c 1)
          (%gnmt-and c)
          (%gnmt-out (make-instance '%gnmt-b) 5))))

(defparameter *%gnmt-expected*
  '((:out-c :out-b (:in-b :in-a 2) :out-a 1)
    nil
    (:out-b (:in-b :in-a 6) :out-a 5)))

(deftest gf-next-method-state-thread.main
  (equal (%gnmt-run) *%gnmt-expected*)
  t)

(deftest gf-next-method-state-thread.fresh-threads
  (let* ((results (make-array 4 :initial-element nil))
         (threads (loop for i below 4
                        collect (let ((i i))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (setf (aref results i)
                                           (loop repeat (emit-free-scale 2000 100)
                                                 always (equal (%gnmt-run) *%gnmt-expected*)))))))))
    (mapc #'dotcl-thread:thread-join threads)
    (every #'identity results))
  t)
