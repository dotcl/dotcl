;;; A lambda built by one EVAL must keep its compilation unit alive: while the
;;; lambda is reachable its body can run, and a body can load a constant from
;;; the unit that produced it. Collecting the unit first turns a later call into
;;; "compilation-unit constant was collected while still loadable".
;;;
;;; These exercise the shape -- many units' worth of lambdas kept alive across
;;; collections, then called -- but do not by themselves force the collection
;;; that exposed the bug; that needed a small nursery and the memory pressure of
;;; a full benchmark run. They guard against over-collection generally.

(defun %uhl-build (n)
  (eval `(progn
           ,@(loop for i below n
                   collect `(defclass ,(intern (format nil "UHL-C-~D" i)) ()
                              ((a :initform (list ,i ,(* i 2))
                                  :accessor ,(intern (format nil "UHL-A-~D" i)))
                               (b :initform (lambda () ,i)
                                  :accessor ,(intern (format nil "UHL-B-~D" i)))))))))

(deftest unit-holder.classes-usable-after-gc
  (progn
    (%uhl-build 40)
    (dotimes (k 3) (dotcl:gc))
    (let ((bad 0))
      (dotimes (i 40)
        (let ((o (make-instance (intern (format nil "UHL-C-~D" i)))))
          (unless (and (equal (funcall (intern (format nil "UHL-A-~D" i)) o) (list i (* i 2)))
                       (eql (funcall (funcall (intern (format nil "UHL-B-~D" i)) o)) i))
            (incf bad))))
      bad))
  0)

(deftest unit-holder.eval-lambdas-callable-after-gc
  (let ((fns (loop for i below 60 collect (eval `(lambda () (lambda () (list ,i :ok)))))))
    (dotimes (k 3) (dotcl:gc))
    (count-if-not (lambda (f) (equal (funcall (funcall f)) (list (position f fns) :ok))) fns))
  0)
