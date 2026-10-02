;;; A circular literal ('#1=(1 2 3 . #1#)) inside a TAGBODY, or inside a
;;; LABELS function the compiler tries to compile as a direct loop, made the
;;; compiler loop forever: two scans of the generated instructions looked for a
;;; symbol by walking every cons, including the quoted literal. Found by the
;;; random integer form test with literal-object shapes.

(defun %clit-tagbody (x)
  (let ((r 0))
    (tagbody (setq r (+ x (nth 4 '#1=(1 2 3 . #1#)))))
    r))

(defun %clit-labels (x)
  (labels ((f (n a) (if (<= n 0) (+ a (nth 4 '#2=(1 2 3 . #2#))) (f (1- n) (+ a 1)))))
    (f x 0)))

(deftest circular-literal-in-tagbody
  (list (%clit-tagbody 1) (%clit-labels 3))
  (3 5))
