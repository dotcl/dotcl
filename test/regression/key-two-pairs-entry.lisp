;;; A call with exactly two keyword pairs takes a typed entry of the &key function
;;; (no argument array). Every result and diagnostic equals the array entry's, which
;;; APPLY reaches.

(defun %ktp-f (a &key (k 1) (m (+ k 10)) (p nil p-p))
  (list a k m p p-p))
(defun %ktp-g (a b &key x y) (list a b x y))
(defun %ktp-open (a &key k &allow-other-keys) (list a k))
(defun %ktp-five (a b c d e &key k m) (list a b c d e k m))

(defmacro %ktp-both (fn &rest args)
  "The literal call and the APPLY call, each reduced to its value or :ERROR."
  `(list (handler-case (,fn ,@args) (program-error () :error))
         (handler-case (apply #',fn (list ,@args)) (program-error () :error))))

(defmacro %ktp-same (fn &rest args)
  `(let ((r (%ktp-both ,fn ,@args))) (list (equal (first r) (second r)) (first r))))

(deftest key-two-pairs.both-keys
  (list (%ktp-same %ktp-f 0 :k 5 :m 6)
        (%ktp-same %ktp-f 0 :m 6 :k 5)
        (%ktp-same %ktp-f 0 :k 5 :p 7)
        (%ktp-same %ktp-f 0 :p 7 :m 8))
  ((t (0 5 6 nil nil)) (t (0 5 6 nil nil)) (t (0 5 15 7 t)) (t (0 1 8 7 t))))

(deftest key-two-pairs.duplicate-takes-first
  (list (%ktp-same %ktp-f 0 :k 3 :k 4)
        (%ktp-same %ktp-g 1 2 :x :a :x :b))
  ((t (0 3 13 nil nil)) (t (1 2 :a nil))))

(deftest key-two-pairs.default-sees-earlier-key
  (%ktp-same %ktp-f 9 :k 2 :p nil)
  (t (9 2 12 nil t)))

(deftest key-two-pairs.unknown-key-is-an-error
  (list (%ktp-same %ktp-f 0 :k 1 :bogus 2)
        (%ktp-same %ktp-f 0 :bogus 2 :k 1)
        (%ktp-same %ktp-g 1 2 :x 1 :z 2))
  ((t :error) (t :error) (t :error)))

(deftest key-two-pairs.allow-other-keys
  (list (%ktp-same %ktp-f 0 :bogus 1 :allow-other-keys t)
        (%ktp-same %ktp-f 0 :allow-other-keys t :bogus 1)
        (%ktp-same %ktp-f 0 :allow-other-keys nil :bogus 1)
        (%ktp-same %ktp-f 0 :allow-other-keys nil :allow-other-keys t)
        (%ktp-same %ktp-f 0 :allow-other-keys t :allow-other-keys nil)
        (%ktp-same %ktp-open 3 :zzz 1 :k 4))
  ((t (0 1 11 nil nil)) (t (0 1 11 nil nil)) (t :error) (t (0 1 11 nil nil))
   (t (0 1 11 nil nil)) (t (3 4))))

(deftest key-two-pairs.non-symbol-keyword-is-an-error
  (list (%ktp-same %ktp-f 0 :k 1 5 2)
        (%ktp-same %ktp-f 0 "k" 1 :m 2))
  ((t :error) (t :error)))

(deftest key-two-pairs.too-many-required-for-an-entry
  (%ktp-same %ktp-five 1 2 3 4 5 :k 1 :m 2)
  (t (1 2 3 4 5 1 2)))

(deftest key-two-pairs.one-pair-and-none-unchanged
  (list (%ktp-same %ktp-f 0 :k 5) (%ktp-same %ktp-f 0) (%ktp-same %ktp-g 1 2 :y 3))
  ((t (0 5 15 nil nil)) (t (0 1 11 nil nil)) (t (1 2 nil 3))))

(deftest key-two-pairs.three-pairs
  (%ktp-same %ktp-f 0 :k 5 :m 6 :p 7)
  (t (0 5 6 7 t)))

;;; Three and four pairs (as many as the 8-argument entry allows).
(defun %ktp-h (a &key (k 1) (m 2) (p 3) (q 4) (r 5)) (list a k m p q r))
(defun %ktp-z (&key (k 1) (m 2) (p 3) (q 4) (r 5)) (list k m p q r))

(deftest key-three-four-pairs.values-and-order
  (list (%ktp-same %ktp-h 0 :r 9 :p 8 :k 7)
        (%ktp-same %ktp-h 0 :k 1 :m 2 :p 3 :q 4)
        (%ktp-same %ktp-h 0 :q 9 :q 8 :r 7 :k 6)
        (%ktp-same %ktp-z :p 1 :q 2 :r 3)
        (%ktp-same %ktp-z :r 5 :q 4 :p 3 :m 2)
        (%ktp-same %ktp-z :r 5 :q 4 :p 3 :m 2 :k 1))
  ((t (0 7 2 8 4 9)) (t (0 1 2 3 4 5)) (t (0 6 2 3 9 7)) (t (1 2 1 2 3))
   (t (1 2 3 4 5)) (t (1 2 3 4 5))))

(deftest key-three-four-pairs.diagnostics
  (list (%ktp-same %ktp-h 0 :k 1 :m 2 :bogus 3)
        (%ktp-same %ktp-h 0 :bogus 1 :k 2 :m 3)
        (%ktp-same %ktp-h 0 :k 1 :m 2 :p 3 :bogus 4)
        (%ktp-same %ktp-h 0 :bogus 1 :k 2 :m 3 :allow-other-keys t)
        (%ktp-same %ktp-h 0 :allow-other-keys t :k 2 :m 3 :bogus 4)
        (%ktp-same %ktp-h 0 :k 2 :allow-other-keys nil :m 3 :bogus 4)
        (%ktp-same %ktp-h 0 :allow-other-keys nil :allow-other-keys t :bogus 1)
        (%ktp-same %ktp-h 0 :k 1 :m 2 5 3)
        (%ktp-same %ktp-z :k 1 :m 2 :nope 3 :allow-other-keys t))
  ((t :error) (t :error) (t :error) (t (0 2 3 3 4 5)) (t (0 2 3 3 4 5)) (t :error)
   (t :error) (t :error) (t (1 2 3 4 5))))

;;; The array entry (a function with &optional or &rest as well as &key, or a call
;;; with more pairs than a typed entry takes) finds a keyword argument by the
;;; keyword symbol itself.
(defun %ktp-ok (a &optional b &key (k 1) (m 2)) (list a b k m))
(defun %ktp-rk (a &rest r &key (k 1) (m 2)) (list a r k m))

(deftest key-array-entry.finds-keywords
  (list (%ktp-ok 1)
        (%ktp-ok 1 2)
        (%ktp-ok 1 2 :m 9)
        (%ktp-ok 1 2 :m 9 :k 8)
        (%ktp-ok 1 2 :k 8 :k 7)
        (%ktp-rk 1 :m 5)
        (apply #'%ktp-ok 1 2 '(:k 4 :m 3)))
  ((1 nil 1 2) (1 2 1 2) (1 2 1 9) (1 2 8 9) (1 2 8 2) (1 (:m 5) 1 5) (1 2 4 3)))

(deftest key-array-entry.non-keyword-symbol-of-the-same-name-is-an-error
  ;; Only the keyword symbol names the key; a CL-USER symbol K is not a keyword.
  (list (handler-case (%ktp-ok 1 2 'k 5) (error () :error))
        (handler-case (%ktp-ok 1 2 :bogus 5) (error () :error))
        (handler-case (%ktp-ok 1 2 :k) (error () :error)))
  (:error :error :error))

;;; The unknown-keyword check of the array entry.
(defun %ktp-ck (a &optional b &key (k 1) (m 2) ((:other o) 3)) (list a b k m o))

(deftest key-array-check.accepts-and-rejects
  (list (handler-case (%ktp-ck 1 2 :k 5 :m 6 :other 7) (error () :error))
        (handler-case (%ktp-ck 1 2 :other 7) (error () :error))
        (handler-case (%ktp-ck 1 2 (intern "K" :keyword) 9) (error () :error))
        (handler-case (%ktp-ck 1 2 (intern "k" :keyword) 9) (error () :error))
        (handler-case (%ktp-ck 1 2 (make-symbol "K") 9) (error () :error))
        (handler-case (%ktp-ck 1 2 :k 1 :zz 2) (error () :error))
        (handler-case (%ktp-ck 1 2 :k 1 :zz 2 :allow-other-keys t) (error () :error))
        (handler-case (%ktp-ck 1 2 :k 1 :m) (error () :error))
        (handler-case (%ktp-ck 1 2 "K" 1) (error () :error))
        (handler-case (%ktp-ck 1 2 7 1) (error () :error)))
  ((1 2 5 6 7) (1 2 1 2 7) (1 2 9 2 3) :error :error :error (1 2 1 2 3) :error :error :error))

(deftest key-array-check.symbol-created-late
  ;; A keyword interned after the function was compiled is the same symbol.
  (let ((kw (read-from-string ":m")))
    (list (eq kw :m) (%ktp-ck 1 2 kw 4)))
  (t (1 2 1 4 3)))
