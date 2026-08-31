;;; A &key function gets a typed entry for its required-only arity.
;;;
;;; A variadic LispFunction has one entry, taking LispObject[], so InvokeN
;;; builds an array on every call -- including the calls that pass no keywords
;;; at all, which is most of them. That was 40 bytes and a 1.6x slowdown on
;;; every &key call: (f 1 2) plain 16622 units, the same call to a &key f 26911.
;;;
;;; The mechanism already existed for &optional (one typed arity per concrete
;;; argument count, absent optionals bound by a wrapping LET*). &key adds the
;;; one arity that can be typed: the required-only call, with every key param
;;; bound to its default -- and its supplied-p, when it has one, to NIL.
;;;
;;; Defaults stay unrestricted. The LET* evaluates them at call time exactly
;;; where the array entry would, so a default that reads an earlier parameter,
;;; or has a side effect, behaves the same.
;;;
;;; Only the --asm load path installs these; the fasl path stays array-only.
;;;
;;; Every expected value here was taken from SBCL.

(defvar *kde-n* 0)

(defun kde-a (x &key (c 1) (d "s") (e :kw) f) (list x c d e f))

(deftest kde.constant-defaults
  (list (kde-a 9)
        (kde-a 9 :c 2)
        (kde-a 9 :f 3 :c 4)
        (kde-a 9 :d "z")
        (funcall #'kde-a 7)
        (multiple-value-list (kde-a 7)))
  ((9 1 "s" :kw nil) (9 2 "s" :kw nil) (9 4 "s" :kw 3) (9 1 "z" :kw nil)
   (7 1 "s" :kw nil) ((7 1 "s" :kw nil))))

;;; A default may read an earlier parameter, or an earlier key.
(defun kde-b (x &key (c (* x 2)) (d (+ c 1))) (list x c d))

(deftest kde.defaults-read-earlier-params
  (list (kde-b 5) (kde-b 5 :c 100) (kde-b 5 :d 7))
  ((5 10 11) (5 100 101) (5 10 7)))

;;; A default with a side effect runs once per call that omits it, and never
;;; when it is supplied.
(defun kde-c (&key (c (incf *kde-n*))) c)

(deftest kde.default-side-effects
  (list (progn (setq *kde-n* 0) (list (kde-c) (kde-c) *kde-n*))
        (progn (setq *kde-n* 0) (list (kde-c :c 42) (kde-c :c 42) *kde-n*)))
  ((1 2 2) (42 42 0)))

(defun kde-d (x &key (c 1 c-p) (d 2 d-p)) (list x c c-p d d-p))

(deftest kde.supplied-p
  (list (kde-d 9) (kde-d 9 :c 5) (kde-d 9 :d 6) (kde-d 9 :c 5 :d 6))
  ((9 1 nil 2 nil) (9 5 t 2 nil) (9 1 nil 6 t) (9 5 t 6 t)))

;;; A parameter the typed entry would bind with a LET* loses a SPECIAL
;;; declaration, so such a function keeps the array entry only. &optional was
;;; already wired this way and had exactly that bug: (o-e 1) below answered
;;; UNBOUND-VARIABLE where SBCL answers (1 :inner :inner).
(defvar *kde-dummy* nil)
(defun kde-e (x &key (c :inner)) (declare (special c)) (list x c (symbol-value 'c)))
(defun kde-o (x &optional (c :inner)) (declare (special c)) (list x c (symbol-value 'c)))

(deftest kde.special-declaration
  (list (kde-e 1) (kde-e 1 :c :given) (kde-o 1) (kde-o 1 :given))
  ((1 :inner :inner) (1 :given :given) (1 :inner :inner) (1 :given :given)))

;;; Arity and keyword errors read the same as before: the typed entry is only
;;; reached by a call that already has the right number of arguments.
(defun kde-f (x &key c) (list x c))

(deftest kde.errors
  (flet ((kind (thunk)
           (handler-case (funcall thunk) (program-error () :program-error))))
    (list (kind (lambda () (kde-f)))
          (kind (lambda () (kde-f 1 2)))
          (kind (lambda () (kde-f 1 :bogus 2)))
          (kde-f 1 :bogus 2 :allow-other-keys t)
          (kind (lambda () (kde-f 1 :c)))))
  (:program-error :program-error :program-error (1 nil) :program-error))

(defun kde-g (&key a b) (list a b))
(defun kde-h (x y z &key (w 4)) (list x y z w))
(defun kde-i (x &key c &allow-other-keys) (list x c))

(deftest kde.other-shapes
  (list (kde-g)
        (kde-g :b 1)
        (kde-h 1 2 3)
        (kde-h 1 2 3 :w 9)
        (apply #'kde-h '(1 2 3))
        (apply #'kde-h 1 2 3 '(:w 8))
        (kde-i 1)
        (kde-i 1 :zz 5))
  ((nil nil) (nil 1) (1 2 3 4) (1 2 3 9) (1 2 3 4) (1 2 3 8) (1 nil) (1 nil)))

;;; Recursion reaches the typed entry too, both when the recursive call supplies
;;; a keyword and when it does not.
(defun kde-j (n &key (acc 0)) (if (= n 0) acc (kde-j (1- n) :acc (+ acc n))))
(defun kde-k (n &key (acc 0)) (if (= n 0) acc (kde-k (1- n))))

(deftest kde.recursion
  (list (kde-j 5) (kde-k 3))
  (15 0))

;;; One keyword pair also gets a typed arity.
;;;
;;; (position x l :test #'eq), (sort l #'< :key #'car) -- passing exactly one
;;; keyword is the shape most calls that pass any have, and it was still going
;;; through the array entry: 64 bytes and 2.0x a plain four-argument call.
;;;
;;; A single body serves it: which key was named is a per-binding IF, not a copy
;;; of the body per key. 64 -> 0 bytes, 2.0x -> 1.56x.
;;;
;;; Only implicit keywords qualify -- an explicit ((:kw var) default) names a
;;; symbol whose package the generated comparison would have to reconstruct, so
;;; such a function keeps the array entry.

(defvar *kde-m* 0)
(defun kde-p (x &key c) (list x c))
(defun kde-q (x &key c &allow-other-keys) (list x c))
(defun kde-r (&key (a (incf *kde-m*)) (b (incf *kde-m*))) (list a b))
(defun kde-s (&key (a 1) (b (* a 10))) (list a b))
(defun kde-t (x &key ((:kw v) 3)) (list x v))

(deftest kde.one-keyword-pair
  (list (kde-p 1 :c 2)
        (kde-p 1 :allow-other-keys t)
        (kde-p 1 :allow-other-keys nil)
        (kde-q 1 :zz 5)
        (kde-q 1 :c 5)
        (handler-case (kde-p 1 :bogus 2) (program-error () :program-error)))
  ((1 2) (1 nil) (1 nil) (1 nil) (1 5) :program-error))

;;; The default of the key that IS supplied must not run; the others must.
(deftest kde.one-keyword-pair-default-side-effects
  (list (progn (setq *kde-m* 0) (list (kde-r :a 9) *kde-m*))
        (progn (setq *kde-m* 0) (list (kde-r :b 9) *kde-m*))
        (progn (setq *kde-m* 0) (list (kde-r) *kde-m*)))
  (((9 1) 1) ((1 9) 1) ((1 2) 2)))

;;; A later default reading an earlier key still sees the supplied value.
(deftest kde.one-keyword-pair-default-reads-earlier-key
  (list (kde-s) (kde-s :a 5) (kde-s :b 7))
  ((1 10) (5 50) (1 7)))

;;; An explicitly named keyword declines the typed arity and must still work.
(deftest kde.explicit-keyword-name
  (list (kde-t 1) (kde-t 1 :kw 8))
  ((1 3) (1 8)))
