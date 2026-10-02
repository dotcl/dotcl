;;; LOOP expands its internal macros with a walk over the whole expansion. The
;;; walk went into QUOTE forms, so a circular literal anywhere in a LOOP made
;;; the macroexpansion run forever. Found by the random integer form test with
;;; literal-object shapes.

(defun %clil-sum (x) (loop for i below 2 sum (+ i x (nth 4 '#1=(1 2 3 . #1#)))))
(defun %clil-with () (loop with a = '#2=(5 6 . #2#) repeat 1 return (nth 3 a)))

(deftest circular-literal-in-loop
  (list (%clil-sum 1) (%clil-with)
        (loop repeat 1 collect '(loop-collect-answer x)))
  (7 6 ((loop-collect-answer x))))
