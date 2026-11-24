;;; A RETURN-FROM inside a local function that the enclosing BLOCK calls in tail position,
;;; in an interpreted function. The BLOCK body ends in a tail call, which the evaluator
;;; hands to the function's trampoline; the RETURN-FROM then has to go to the trampoline's
;;; catch, not to the block's own (already left) one. Fiasco's PARSE-LAMBDA-LIST, which has
;;; this shape, failed with "Attempt to THROW to tag (%MINI-BLOCK ...)" on an emit-free build.

(defun %irf-run (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(defparameter *irf-forms*
  '((progn (defun irf-a () (block b (flet ((f () (return-from b :ok))) (f)))) (irf-a))
    (progn (defun irf-b (args)
             (labels ((p1 () (pop args) (if args (p2) (done)))
                      (p2 () (pop args) (p1))
                      (done () (return-from irf-b :done)))
               (when args (p1))))
           (list (irf-b '(1 2 3)) (irf-b '(1)) (irf-b nil)))
    (progn (defun irf-c (n)
             (block outer
               (labels ((loop1 (i) (if (> i n) (return-from outer i) (loop1 (1+ i)))))
                 (loop1 0))))
           (irf-c 5))))

(deftest interp-return-from-tail-local-function.interpret
  (mapcar (lambda (f) (%irf-run :interpret f)) *irf-forms*)
  (:ok (:done :done nil) 6))
