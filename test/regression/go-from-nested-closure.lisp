;;; GO from a closure nested two lambdas deep inside the TAGBODY. The compiled
;;; function failed at compile time with "go: no tagbody tag named 6".
;;;
;;; The free-variable walk memoizes each lambda's capture candidates per
;;; top-level form, but the candidate for a GO (the target tagbody's id
;;; variable) was resolved against the go tags in scope during the walk. A walk
;;; of the whole function made before the TAGBODY was compiled saw no tags and
;;; cached "captures nothing" for the inner lambda, so the outer lambda never
;;; captured the tagbody id and the inner one could not find the tag.
;;; Found by the random integer form test (make test-random-forms).
;;; Top-level DEFUNs rather than COMPILE, so the file also runs on an
;;; emit-free build (there it exercises the interpreter).

(defun %gfnc-app (f) (funcall f 0))

(defun %gfnc-two-levels ()
  (let ((r :done))
    (tagbody (%gfnc-app (lambda (x) (declare (ignore x))
                          (%gfnc-app (lambda (p) (declare (ignore p)) (go 6)))))
       (setq r :fell-through)
     6)
    r))

(deftest go-from-nested-closure.two-levels
  (%gfnc-two-levels)
  :done)

(defun %gfnc-flet ()
  (let ((n 0))
    (tagbody
       (flet ((f () (%gfnc-app (lambda (p) (declare (ignore p)) (go end)))))
         (f))
       (setq n 99)
     end)
    n))

(deftest go-from-nested-closure.flet
  (%gfnc-flet)
  0)

(defun %gfnc-random-form (a b c d)
  ;; The shape the random form test reported.
  (declare (ignore c))
  (unwind-protect 0
    (complex (progn (tagbody
                       (reduce #'(lambda (lmv6 lmv5)
                                   (declare (ignore lmv5))
                                   (reduce #'(lambda (lmv4 lmv6)
                                               (declare (ignore lmv4 lmv6))
                                               (go 6))
                                           (vector d 0 0 a 0 lmv6 0 0)
                                           :end 8 :start 5 :from-end t))
                               (list 0 0 a 0 b 0 0) :end 7 :start 0 :from-end t)
                       0 6)
                     a)
             0)))

(deftest go-from-nested-closure.random-form
  (%gfnc-random-form 46611695438154349 1679440588238076 -519860868 -4)
  0)
