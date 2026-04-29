;;; test8: tagbody/go (count down from 5)
;;; Expected output: (5 4 3 2 1)
(progn
  (defun countdown ()
    (let ((result nil)
          (i 5))
      (tagbody
       loop
        (if (= i 0) (go end))
        (setq result (cons i result))
        (setq i (- i 1))
        (go loop)
       end)
      result))
  (print (countdown)))
