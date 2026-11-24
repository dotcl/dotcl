;;; A FLET or LABELS whose body holds a circular quoted literal compiles and runs
;;; (the walk for NOTINLINE declarations used to follow the circular list forever).
(defun %fcl-flet ()
  (flet ((f (x) (car x)))
    (f '#1=(3 . #1#))))

(defun %fcl-labels ()
  (labels ((g (x) (if (consp x) (cadr x) x)))
    (let ((c '#2=(4 5 . #2#)))
      (list (g c) (car c)))))

(deftest flet-circular-literal.flet
  (%fcl-flet)
  3)

(deftest flet-circular-literal.labels
  (%fcl-labels)
  (5 4))
