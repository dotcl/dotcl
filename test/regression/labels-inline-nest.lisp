;;; A LABELS group declared INLINE whose definitions call each other without
;;; recursion, and INLINE local functions called from inside a closure.

(setf dotcl:*save-sil* t)

(defun %lin-b () :global-b)

;;; A definition names one written after it.
(deftest labels-inline-nest.forward-reference
  (labels ((%lin-a (x) (list :a (%lin-b) x))
           (%lin-b () :local-b))
    (declare (inline %lin-a %lin-b))
    (%lin-a 1))
  (:a :local-b 1))

;;; A macro expanding into a call of a later definition.
(deftest labels-inline-nest.macro-call
  (macrolet ((call-b () '(%lin-b)))
    (labels ((%lin-a () (list :a (call-b)))
             (%lin-b () :local-b))
      (declare (inline %lin-a %lin-b))
      (%lin-a)))
  (:a :local-b))

;;; Mutual recursion is not substituted, and still works.
(deftest labels-inline-nest.mutual-recursion
  (labels ((ev (n) (if (zerop n) t (od (1- n))))
           (od (n) (if (zerop n) nil (ev (1- n)))))
    (declare (inline ev od))
    (list (ev 10) (od 7)))
  (t t))

;;; NOTINLINE of one member reaches the other definitions, as in LABELS.
(deftest labels-inline-nest.notinline
  (labels ((f (x) (* x 2))
           (g (x) (+ (f x) 1)))
    (declare (inline g) (notinline f))
    (list (g 3) (funcall #'f 4)))
  (7 8))

;;; Called from inside a closure, the body still sees the variables it was
;;; written with.
(deftest labels-inline-nest.closure-free-variable
  (let ((z 5))
    (flet ((f (x) (* x z)))
      (declare (inline f))
      (flet ((g (y) (+ 1 (f y))))
        (mapcar #'g '(1 2)))))
  (6 11))

;;; A closure parameter with the name of the body's free variable is not that
;;; variable.
(deftest labels-inline-nest.closure-parameter-shadows
  (let ((z 7))
    (flet ((f (x) (* x z)))
      (declare (inline f))
      (mapcar (lambda (z) (f z)) '(1 2))))
  (7 14))

(defun %lin-shape (px n)
  (declare (type (simple-array (unsigned-byte 8) (*)) px) (fixnum n))
  (labels ((cubic (x p0 p1)
             (declare (type single-float x p0 p1))
             (+ p0 (* x (- p1 p0))))
           (two (x a)
             (declare (type single-float x) (type function a))
             (cubic x (float (the (unsigned-byte 8) (funcall a (aref px 0))) 1f0)
                    (float (the (unsigned-byte 8) (funcall a (aref px 1))) 1f0))))
    (declare (inline cubic two))
    (flet ((access (a) (round (two 0.5f0 a))))
      (loop repeat n collect (access #'identity)))))

(deftest labels-inline-nest.shape-value
  (%lin-shape (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(10 21)) 2)
  (16 16))

(deftest-compiled-only labels-inline-nest.shape-no-local-calls
  (search "Invoke3" (%flat-text (dotcl:function-sil #'%lin-shape)))
  nil)

(setf dotcl:*save-sil* nil)
