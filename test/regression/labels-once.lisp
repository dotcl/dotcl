;;; A LABELS group whose functions capture nothing but each other is built once
;;; and reused by every entry. These pin that only such groups are: a group
;;; that returns from an enclosing block, goes to an enclosing tag, reads a
;;; lexical variable through a symbol macro, or reads a special keeps the
;;; answers it had, entry after entry. And a local macro in scope still expands.

(defun lo-rf (n)
  (block outer
    (labels ((walk (k) (if (= k 0) (return-from outer (list :escaped n)) (walk (1- k)))))
      (walk n)
      :no)))

(defun lo-tg (n)
  (let ((r nil))
    (tagbody
       (labels ((walk (k) (push k r) (if (= k 0) (go done) (walk (1- k)))))
         (walk n))
     done)
    r))

(defun lo-ab (n)
  (labels ((a (x) (if (> x 0) (b x 1) :a))
           (b (x y) (a (- x y))))
    (a n)))

(defun lo-sm (v)
  (symbol-macrolet ((q v))
    (labels ((f (k) (if (= k 0) q (f (1- k))))
             (g (a b) (list a b)))
      (f 2))))

(defvar *lo-special* 7)
(defun lo-dyn ()
  (labels ((f (k) (if (= k 0) *lo-special* (f (1- k))))
           (g (a b) (list a b)))
    (f 3)))

(defmacro lo-with-macro (&body b) `(macrolet ((m () ''mac)) ,@b))
(defun lo-mm ()
  (lo-with-macro (labels ((f (k) (if (= k 0) (m) (f (1- k))))
                          (g (a b) (list a b)))
                   (f 1))))

(defun lo-tree (form)
  (labels ((rcr (x) (if (consp x) (cons (rcr (car x)) (rcr (cdr x))) x)))
    (rcr form)))

(deftest labels-once.escapes
  (list (lo-rf 3) (lo-rf 5) (lo-tg 2) (lo-tg 1))
  ((:escaped 3) (:escaped 5) (0 1 2) (0 1)))

(deftest labels-once.closed-groups
  (list (lo-ab 3) (lo-ab 0) (lo-tree '(1 (2 3) . 4)) (lo-mm))
  (:a :a (1 (2 3) . 4) mac))

(deftest labels-once.environment
  (list (lo-sm 1) (lo-sm 2) (lo-dyn) (let ((*lo-special* 8)) (lo-dyn)))
  (1 2 7 8))
