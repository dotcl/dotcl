;;; A free SPECIAL declaration at the head of an FLET or LABELS body applies to
;;; the body forms only; the local functions keep the lexical variable.

(deftest flet-free-special.body-sees-dynamic
  (let ((x :special))
    (declare (special x))
    (let ((x :lexical))
      (flet ((f () x))
        (declare (special x))
        (list x (f)))))
  (:special :lexical))

(deftest labels-free-special.body-sees-dynamic
  (let ((x :special))
    (declare (special x))
    (let ((x :lexical))
      (labels ((f () x))
        (declare (special x))
        (list x (f)))))
  (:special :lexical))

(deftest labels-free-special.no-functions
  (let ((x :special))
    (declare (special x))
    (let ((x :lexical))
      (labels ()
        (declare (special x))
        x)))
  :special)

(deftest flet-free-special.other-declarations-kept
  (let ((x :special))
    (declare (special x))
    (let ((x :lexical))
      (flet ((f () x))
        (declare (special x) (ignorable #'f))
        x)))
  :special)
