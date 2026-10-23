;;; A closure captures a block's tag from a local function body that is never
;;; compiled (an unused function, deferred for inlining and then dropped), so no
;;; RETURN-FROM was compiled, yet making the closure reads the tag. The compile
;;; used to fail with "Undeclared local: BTAG_n". Expected values are SBCL's.

(defmacro %btcu (name expected form &rest args)
  `(deftest-emitting-only ,(intern (format nil "BLOCK-TAG-CAPTURED-UNUSED-LOCAL-FN.~a" name))
     (let ((*error-output* (make-broadcast-stream)))
       (funcall (compile nil ',form) ,@args))
     ,expected))

(%btcu "RANDOM-FORM" 0
  (lambda (a b c d)
    (declare (ignorable a b c d))
    (declare (optimize (compilation-speed 0) (safety 2) (speed 1) (debug 0) (space 0)))
    (flet ((%f18 (f18-1)
             (progn (labels ((%f5 (f5-1)
                               (labels ((%f17 (f17-1 f17-2 f17-3 &optional &key (key1 0))
                                          (flet ((%f5 nil (return-from %f18 0))) 0)))
                                 0)))
                      0)
                    0)))
      0))
  1 2 3 4)

(%btcu "OPTIONAL" 1
  (lambda () (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "REST" 1
  (lambda () (block b1 (flet ((%f (&rest k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "KEY" 1
  (lambda () (block b1 (labels ((%f (&key k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "OPTIONAL-CALLED" 5
  (lambda () (block b1 (flet ((%f (&optional (k 5)) (flet ((%g () (return-from b1 0))) k))) (%f)))))

(%btcu "NOTINLINE" 3
  (lambda ()
    (block b1
      (flet ((%f (k) (flet ((%g () (return-from b1 0))) k)))
        (declare (notinline %f))
        (+ (%f 1) (%f 2))))))

(%btcu "LAMBDA-ARG" 3
  (lambda () (block b1 (funcall (lambda (k) (flet ((%g () (return-from b1 0))) k)) 3))))

(%btcu "LAMBDA-VAR" 7
  (lambda (x) (block b1 (let ((f (lambda (k) (flet ((%g () (return-from b1 0))) k)))) (funcall f x))))
  7)

(%btcu "ESCAPE-USED" :out
  (lambda (x)
    (block b1
      (flet ((%f (&optional k) (flet ((%g () (return-from b1 :out))) (if k (%g) 1))))
        (list (%f x) 2))))
  t)

(%btcu "ESCAPE-NOT-TAKEN" (1 2)
  (lambda (x)
    (block b1
      (flet ((%f (&optional k) (flet ((%g () (return-from b1 :out))) (if k (%g) 1))))
        (list (%f x) 2))))
  nil)

(%btcu "GO-OPTIONAL" 3
  (lambda ()
    (let ((r 1))
      (tagbody (flet ((%f (&optional k) (flet ((%g () (go end))) k))) (setq r (%f 3)))
       end)
      r)))

(%btcu "GO-LAMBDA" 3
  (lambda ()
    (let ((r 1))
      (tagbody (let ((f (lambda (k) (flet ((%g () (go end))) k)))) (setq r (funcall f 3)))
       end)
      r)))

(%btcu "SAFETY0-DEBUG0" 1
  (lambda ()
    (declare (optimize (safety 0) (debug 0)))
    (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "SAFETY3-DEBUG0" 1
  (lambda ()
    (declare (optimize (safety 3) (debug 0)))
    (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "SAFETY0-DEBUG3" 1
  (lambda ()
    (declare (optimize (safety 0) (debug 3)))
    (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "SAFETY3-DEBUG3" 1
  (lambda ()
    (declare (optimize (safety 3) (debug 3)))
    (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))

(%btcu "SPEED3" 1
  (lambda ()
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (block b1 (flet ((%f (&optional k) (flet ((%g () (return-from b1 0))) k))) 1))))
