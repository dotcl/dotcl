;;; A child structure whose :CONC-NAME gives an inherited slot the same
;;; accessor name as its parent's keeps the parent's accessor, as SBCL does,
;;; so the accessor still accepts the parent and the parent's other children.
;;; (runes' YSTREAM- structures and cl-yacc's ITEM- structures are written so.)

(defstruct siasn-item (pos 0) (tag nil))
(defstruct (siasn-lr0 (:include siasn-item) (:conc-name siasn-item-)) (look nil))
(defstruct (siasn-lr1 (:include siasn-item) (:conc-name siasn-item-)) (ahead nil))
(defstruct (siasn-lr2 (:include siasn-lr1) (:conc-name siasn-item-)) (more nil))
(defstruct (siasn-n (:conc-name "SIASN-N.")) (i 0 :type fixnum))
(defstruct (siasn-n2 (:include siasn-n) (:conc-name "SIASN-N.")))
(defstruct (siasn-n3 (:include siasn-n) (:conc-name "SIASN-N.")))

(defun %siasn-pos (o) (siasn-item-pos o))
(defun %siasn-set-pos (o v) (setf (siasn-item-pos o) v))
(defun %siasn-i (o) (the fixnum (+ 1 (siasn-n.i o))))

(defmacro %siasn-err (form)
  `(handler-case (progn ,form)
     (type-error (e) (list :type-error (type-error-expected-type e)))))

(deftest struct-inherited-accessor-same-name.siblings
  (let ((a (make-siasn-lr0 :pos 1)) (b (make-siasn-lr1 :pos 2))
        (c (make-siasn-lr2 :pos 3)) (d (make-siasn-item :pos 4)))
    (list (%siasn-pos a) (%siasn-pos b) (%siasn-pos c) (%siasn-pos d)
          (siasn-item-pos a) (funcall 'siasn-item-pos b)
          (%siasn-set-pos a 10) (setf (siasn-item-tag b) :x)
          (funcall #'(setf siasn-item-pos) 11 c)
          (siasn-item-pos a) (siasn-item-tag b) (siasn-item-pos c)))
  (1 2 3 4 1 2 10 :x 11 10 :x 11))

(deftest struct-inherited-accessor-same-name.raw-slot
  (list (%siasn-i (make-siasn-n2 :i 1)) (%siasn-i (make-siasn-n3 :i 2))
        (%siasn-i (make-siasn-n :i 3)))
  (2 3 4))

(deftest struct-inherited-accessor-same-name.own-slots-still-checked
  (list (%siasn-err (siasn-item-look (make-siasn-lr1)))
        (%siasn-err (siasn-item-ahead (make-siasn-lr0)))
        (%siasn-err (siasn-item-pos (make-siasn-n))))
  ((:type-error siasn-lr0) (:type-error siasn-lr1) (:type-error siasn-item)))
