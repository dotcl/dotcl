;;; A structure with one to four raw slots (double-float, single-float, fixnum,
;;; (unsigned-byte 64)) keeps them in the instance; five or more keep them in a
;;; long[]. Boxed slots beside them stay in the slot array. Every way in and out
;;; of a slot must behave the same for both shapes.

(setf dotcl:*save-sil* t)

(defstruct (%sri-v3 (:constructor %sri-v3 (x y z))
                    (:constructor make-%sri-v3))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float))
(defstruct %sri-mix (n 0 :type fixnum) (o nil) (d 0d0 :type double-float))
(defstruct %sri-sf (f 0f0 :type single-float) (u 0 :type (unsigned-byte 64)))
(defstruct %sri-4 (a 0 :type fixnum) (b 0 :type fixnum) (c 0d0 :type double-float)
  (d 0 :type fixnum))
(defstruct %sri-5 (a 0 :type fixnum) (b 0 :type fixnum) (c 0d0 :type double-float)
  (d 0 :type fixnum) (e 0d0 :type double-float))
(defstruct (%sri-mixc (:include %sri-mix)) (e 0d0 :type double-float) (q nil))
(defstruct (%sri-4c (:include %sri-4)) (e 0 :type fixnum))

(defun %sri-make-all ()
  (list (%sri-v3 1d0 2d0 3d0)
        (make-%sri-mix :n 1 :o "o" :d 2.5d0)
        (make-%sri-sf :f 1.5f0 :u (1- (expt 2 64)))
        (make-%sri-4 :a 1 :b 2 :c 3d0 :d 4)
        (make-%sri-5 :a 1 :b 2 :c 3d0 :d 4 :e 5d0)
        (make-%sri-mixc :n 1 :o :o :d 2d0 :e 3d0 :q :q)
        (make-%sri-4c :a 1 :b 2 :c 3d0 :d 4 :e 5)))

(defun %sri-slot-names (s)
  (mapcar #'dotcl-mop:slot-definition-name
          (dotcl-mop:class-slots (class-of s))))

(defun %sri-values (s)
  (mapcar (lambda (n) (slot-value s n)) (%sri-slot-names s)))

(deftest struct-raw-inline.construct-and-read
  (mapcar #'%sri-values (%sri-make-all))
  ((1d0 2d0 3d0) (1 "o" 2.5d0) (1.5f0 18446744073709551615) (1 2 3d0 4)
   (1 2 3d0 4 5d0) (1 :o 2d0 3d0 :q) (1 2 3d0 4 5)))

(defun %sri-dot (a b)
  (declare (type %sri-v3 a b))
  (+ (* (%sri-v3-x a) (%sri-v3-x b)) (* (%sri-v3-y a) (%sri-v3-y b))
     (* (%sri-v3-z a) (%sri-v3-z b))))

(defun %sri-scale (a k)
  (declare (type %sri-v3 a) (double-float k))
  (setf (%sri-v3-x a) (* k (%sri-v3-x a))
        (%sri-v3-y a) (* k (%sri-v3-y a))
        (%sri-v3-z a) (* k (%sri-v3-z a)))
  a)

(defun %sri-sum-loop (v n)
  (declare (type %sri-v3 v) (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n acc)
      (setq acc (+ acc (%sri-v3-x v) (%sri-v3-z v)))
      (setf (%sri-v3-y v) (+ (%sri-v3-y v) 1d0)))))

(defun %sri-mix-loop (m n)
  (declare (type %sri-mix m) (fixnum n))
  (dotimes (i n (list (%sri-mix-n m) (%sri-mix-o m) (%sri-mix-d m)))
    (setf (%sri-mix-n m) (the fixnum (+ (%sri-mix-n m) 2)))
    (setf (%sri-mix-d m) (* (%sri-mix-d m) 2d0))))

(defun %sri-new (x)
  (declare (double-float x))
  (%sri-v3 x (+ x 1d0) (+ x 2d0)))

(deftest struct-raw-inline.typed-access
  (let ((a (%sri-v3 1d0 2d0 3d0)) (m (make-%sri-mix :n 1 :o :k :d 1d0))
        (c (make-%sri-mixc :n 1 :d 1d0)))
    (list (%sri-dot a a)
          (%sri-values (%sri-scale (%sri-v3 1d0 2d0 3d0) 2d0))
          (%sri-sum-loop a 3) (%sri-v3-y a)
          (%sri-mix-loop m 3) (%sri-mix-loop c 2)
          (%sri-values (%sri-new 1d0))))
  (14d0 (2d0 4d0 6d0) 12d0 5d0 (7 :k 8d0) (5 nil 4d0) (1d0 2d0 3d0)))

(deftest struct-raw-inline.accessors-and-include
  (let ((c (make-%sri-mixc :n 1 :o 2 :d 3d0 :e 4d0 :q 5))
        (k (make-%sri-4c :a 1 :b 2 :c 3d0 :d 4 :e 5)))
    (list (%sri-mix-n c) (%sri-mix-o c) (%sri-mix-d c) (%sri-mixc-d c) (%sri-mixc-e c)
          (%sri-mixc-q c) (%sri-4-c k) (%sri-4c-c k) (%sri-4-d k) (%sri-4c-e k)))
  (1 2 3d0 3d0 4d0 5 3d0 3d0 4 5))

(deftest struct-raw-inline.setf-accessor
  (let ((all (%sri-make-all)))
    (destructuring-bind (v m sf s4 s5 mc s4c) all
      (setf (%sri-v3-z v) 9d0
            (%sri-mix-n m) 10 (%sri-mix-o m) :x (%sri-mix-d m) -1d0
            (%sri-sf-f sf) 2f0 (%sri-sf-u sf) 7
            (%sri-4-d s4) 40 (%sri-4-c s4) 30d0
            (%sri-5-e s5) 50d0
            (%sri-mix-d mc) 20d0 (%sri-mixc-e mc) 30d0 (%sri-mixc-q mc) :r
            (%sri-4c-e s4c) 50 (%sri-4-a s4c) 10))
    (mapcar #'%sri-values all))
  ((1d0 2d0 9d0) (10 :x -1d0) (2f0 7) (1 2 30d0 40) (1 2 3d0 4 50d0)
   (1 :o 20d0 30d0 :r) (10 2 3d0 4 50)))

(deftest struct-raw-inline.setf-slot-value
  (mapcar (lambda (s)
            (loop for n in (%sri-slot-names s)
                  for v = (slot-value s n)
                  do (setf (slot-value s n)
                           (typecase v (double-float (+ v 10d0)) (single-float (+ v 10f0))
                             (integer (logand (+ v 10) #xFFFFFFFFFFFFFFFF)) (t :new))))
            (%sri-values s))
          (%sri-make-all))
  ((11d0 12d0 13d0) (11 :new 12.5d0) (11.5f0 9)
   (11 12 13d0 14) (11 12 13d0 14 15d0) (11 :new 12d0 13d0 :new) (11 12 13d0 14 15)))

(deftest struct-raw-inline.mop-slot-functions
  (let ((v (%sri-v3 1d0 2d0 3d0)) (m (make-%sri-mix :n 1 :o 2 :d 3d0)))
    (flet ((slotd (o n) (find n (dotcl-mop:class-slots (class-of o))
                              :key #'dotcl-mop:slot-definition-name)))
      (setf (dotcl-mop:slot-value-using-class (class-of v) v (slotd v 'y)) 20d0
            (dotcl-mop:slot-value-using-class (class-of m) m (slotd m 'n)) 10
            (dotcl-mop:slot-value-using-class (class-of m) m (slotd m 'o)) :o)
      (list (dotcl-mop:slot-value-using-class (class-of v) v (slotd v 'x))
            (%sri-v3-y v) (%sri-mix-n m) (%sri-mix-o m)
            (slot-exists-p v 'z) (slot-exists-p v 'w) (slot-boundp m 'd)
            (and (dotcl-mop:slot-boundp-using-class (class-of v) v (slotd v 'z)) t))))
  (1d0 20d0 10 :o t nil t t))

(deftest struct-raw-inline.copy-structure
  (mapcar (lambda (s)
            (let* ((n (first (%sri-slot-names s)))
                   (c (copy-structure s))
                   (before (%sri-values c)))
              (setf (slot-value c n) (if (floatp (slot-value s n)) (float 9 (slot-value s n)) 99))
              (when (slot-exists-p c 'o) (setf (slot-value c 'o) :changed))
              (list (equalp before (%sri-values s))
                    (eq (type-of c) (type-of s))
                    (slot-value s n) (slot-value c n)
                    (and (slot-exists-p s 'o) (slot-value s 'o)))))
          (%sri-make-all))
  ((t t 1d0 9d0 nil) (t t 1 99 "o") (t t 1.5f0 9f0 nil) (t t 1 99 nil) (t t 1 99 nil)
   (t t 1 99 :o) (t t 1 99 nil)))

(deftest struct-raw-inline.copier
  (let* ((o (%sri-v3 1d0 2d0 3d0)) (c (copy-%sri-v3 o))
         (m (make-%sri-mix :n 1 :o (list 1))) (mc (copy-%sri-mix m)))
    (setf (%sri-v3-x c) 5d0 (%sri-mix-n mc) 2 (%sri-mix-o mc) :x)
    (list (%sri-v3-x o) (%sri-v3-x c) (%sri-mix-n m) (%sri-mix-o m) (%sri-mix-n mc)
          (%sri-mix-o mc)))
  (1d0 5d0 1 (1) 2 :x))

(deftest struct-raw-inline.equalp
  (let ((a (%sri-make-all)) (b (%sri-make-all)))
    (list (every #'equalp a b)
          (notany #'equal a b)
          (equalp (%sri-v3 1d0 2d0 3d0) (%sri-v3 1d0 2d0 3.5d0))
          (equalp (make-%sri-mix :o "X") (make-%sri-mix :o "x"))
          (equalp (make-%sri-mix :n 1) (make-%sri-mixc :n 1))
          (let ((h (make-hash-table :test 'equalp)))
            (setf (gethash (%sri-v3 1d0 2d0 3d0) h) :v3
                  (gethash (make-%sri-mix :n 1 :o "k") h) :mix
                  (gethash (make-%sri-5 :a 1) h) :five)
            (list (gethash (%sri-v3 1d0 2d0 3d0) h)
                  (gethash (make-%sri-mix :n 1 :o "K") h)
                  (gethash (make-%sri-5 :a 1) h)
                  (gethash (%sri-v3 1d0 2d0 4d0) h)))))
  (t t nil t nil (:v3 :mix :five nil)))

(deftest struct-raw-inline.print-and-read
  (let ((*package* (find-package "CL-USER")) (*print-readably* nil))
    (mapcar (lambda (s)
              (let* ((text (prin1-to-string s))
                     (back (read-from-string text)))
                (setf (slot-value back (first (%sri-slot-names back)))
                      (slot-value s (first (%sri-slot-names s))))
                (list text (equalp back s) (eq (type-of back) (type-of s)))))
            (list (%sri-v3 1d0 2d0 3d0) (make-%sri-mix :n 1 :o "s" :d 2d0)
                  (make-%sri-sf :f 1f0 :u 5) (make-%sri-5 :e 5d0))))
  (("#S(%SRI-V3 :X 1.0d0 :Y 2.0d0 :Z 3.0d0)" t t)
   ("#S(%SRI-MIX :N 1 :O \"s\" :D 2.0d0)" t t)
   ("#S(%SRI-SF :F 1.0 :U 5)" t t)
   ("#S(%SRI-5 :A 0 :B 0 :C 0.0d0 :D 0 :E 5.0d0)" t t)))

(deftest struct-raw-inline.read-then-write
  (let ((v (read-from-string "#S(%SRI-V3 :X 1d0 :Y 2d0 :Z 3d0)"))
        (m (read-from-string "#S(%SRI-MIX :N 1 :O :O :D 2d0)")))
    (setf (%sri-v3-y v) 20d0 (%sri-mix-d m) 4d0 (%sri-mix-o m) :p)
    (list (%sri-values v) (%sri-values m) (%sri-dot v v)))
  ((1d0 20d0 3d0) (1 :p 4d0) 410d0))

(deftest struct-raw-inline.typep
  (let ((v (%sri-v3 0d0 0d0 0d0)) (c (make-%sri-mixc)) (m (make-%sri-mix)) (k (make-%sri-4c)))
    (list (typep v '%sri-v3) (%sri-v3-p v) (%sri-v3-p m) (typep c '%sri-mix) (%sri-mix-p c)
          (%sri-mixc-p m) (typep k '%sri-4) (typep v 'structure-object) (type-of v)
          (typecase v (%sri-mix :mix) (%sri-v3 :v3) (t :other))
          (typecase c (%sri-v3 :v3) (%sri-mix :mix) (t :other))
          (typep 3 '%sri-v3) (typep (list 1) '%sri-mix)))
  (t t nil t t nil t t %sri-v3 :v3 :mix nil nil))

;; A value of the wrong type is an error, through the accessor, the constructor
;; and SLOT-VALUE alike, and the instance keeps its old value.
(deftest struct-raw-inline.type-errors
  (flet ((te (f) (handler-case (progn (funcall f) :no-error) (type-error () :type-error))))
    (let ((v (%sri-v3 1d0 2d0 3d0)) (m (make-%sri-mix)))
      (list (te (lambda () (setf (%sri-v3-x v) 1)))
            (te (lambda () (setf (%sri-mix-n m) 1.5d0)))
            (te (lambda () (setf (slot-value v 'y) "no")))
            (te (lambda () (setf (slot-value m 'n) :no)))
            (te (lambda () (%sri-v3 1 2d0 3d0)))
            (te (lambda () (make-%sri-mix :d 1)))
            (te (lambda () (%sri-v3-x m)))
            (te (lambda () (%sri-v3-x 5)))
            (te (lambda () (%sri-mixc-e m)))
            (te (lambda () (%sri-dot v m)))
            (%sri-values v) (%sri-values m))))
  (:type-error :type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error (1d0 2d0 3d0) (0 nil 0d0)))

(defun %sri-redefine (from to)
  (eval from)
  (let ((old (eval '(make-%sri-rd :a 1d0 :b 2))))
    (eval to)
    (let ((new (eval '(make-%sri-rd :a 10d0 :b 20))))
      (list (eval `(%sri-rd-a ,new)) (eval `(%sri-rd-b ,new))
            (handler-case (eval `(%sri-rd-a ',old)) (error () :error))
            (%sri-values new)
            (%sri-values (copy-structure new))))))

(deftest struct-raw-inline.redefine
  (list (%sri-redefine '(defstruct %sri-rd (a 0d0 :type double-float) (b 0 :type fixnum))
                       '(defstruct %sri-rd (a 0d0 :type double-float) (b 0 :type fixnum)))
        (%sri-redefine '(defstruct %sri-rd (a 0d0 :type double-float) (b 0 :type fixnum))
                       '(defstruct %sri-rd (b 0 :type fixnum) (a 0d0 :type double-float)))
        (%sri-redefine '(defstruct %sri-rd (a 0d0 :type double-float) (b 0 :type fixnum))
                       '(defstruct %sri-rd a b))
        (%sri-redefine '(defstruct %sri-rd a b)
                       '(defstruct %sri-rd (a 0d0 :type double-float) b (c 0 :type fixnum)
                         (d 0 :type fixnum) (e 0 :type fixnum) (f 0 :type fixnum))))
  ((10d0 20 1d0 (10d0 20) (10d0 20))
   (10d0 20 :error (20 10d0) (20 10d0))
   (10d0 20 :error (10d0 20) (10d0 20))
   (10d0 20 :error (10d0 20 0 0 0 0) (10d0 20 0 0 0 0))))

;; A copy of a raw-inline instance is one object of the same size.
(defun %sri-copy-many (s n)
  (let ((r nil))
    (dotimes (i n r) (setq r (copy-structure s)))))

(deftest-compiled-only struct-raw-inline.copy-is-one-object
  (let* ((v (%sri-v3 1d0 2d0 3d0)) (w (make-%sri-5)))
    (%sri-copy-many v 10) (%sri-copy-many w 10)
    (let ((bv (min-allocated-bytes (lambda () (%sri-copy-many v 1000))))
          (bw (min-allocated-bytes (lambda () (%sri-copy-many w 1000)))))
      (list (<= bv (* 1000 88)) (< bv bw))))
  (t t))

(defun %sri-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %sri-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %sri-5-loop (s n)
  (declare (type %sri-5 s) (fixnum n))
  (dotimes (i n (%sri-5-a s))
    (setf (%sri-5-a s) (the fixnum (+ (%sri-5-a s) (%sri-5-d s))))))

(defun %sri-v3-x-of (v) (declare (type %sri-v3 v)) (+ (%sri-v3-x v) 1d0))

(declaim (inline %sri-iv))
(defstruct (%sri-iv (:constructor %sri-iv (a b))) (a 0d0 :type double-float) (b 0 :type fixnum))
(defun %sri-iv-new (x) (declare (double-float x)) (%sri-iv x 3))

;; The structure's own accessors take the one-compare entry, its constructor
;; passes the raw values as arguments, and a loop hoists the instance itself. A
;; structure of five raw slots keeps the long[] and its hoist.
(deftest-emitting-only struct-raw-inline.emitted-code
  (let ((rd (%sri-sil #'%sri-v3-x-of)) (mk (%sri-sil #'%sri-iv-new))
        (lp (%sri-sil #'%sri-sum-loop)) (w (%sri-sil #'%sri-5-loop)))
    (list (%sri-count "Runtime.StructRefDK" rd)
          (%sri-count "Runtime.MakeStructRawInline" mk)
          (%sri-count "Runtime.StructRawInline" lp)
          (%sri-count "Runtime.StructRawBacking" w)
          (%sri-count "LDELEM-I8" w)))
  (1 1 1 1 4))

(setf dotcl:*save-sil* nil)

(deftest struct-raw-inline.wide-loop-and-inline-constructor
  (list (%sri-5-loop (make-%sri-5 :a 1 :d 3) 4) (%sri-values (%sri-iv-new 2d0))
        (%sri-values (copy-structure (%sri-iv-new 3d0))))
  (13 (2d0 3) (3d0 3)))
