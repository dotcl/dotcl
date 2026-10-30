;;; A structure of up to four boxed slots keeps them in the instance; a longer
;;; one, or one with a raw slot, keeps them in an array. Both shapes must behave
;;; the same through every way in and out of a slot.

(defstruct %sis-1 a)
(defstruct %sis-2 (a 0) (b nil))
(defstruct %sis-4 a b c d)
(defstruct %sis-6 a b c d e f)
(defstruct %sis-d (x 0d0 :type double-float) y)
(defstruct (%sis-2c (:include %sis-2)) c)
(defstruct (%sis-4c (:include %sis-4)) e f)
(defstruct (%sis-1c (:include %sis-1 (a 5))) b c d)
(defstruct %sis-ty (s nil :type (or null string)) (l nil :type list))
(defstruct (%sis-bc (:constructor %sis-bc-mk (a &optional (b 2) &key c))) a b c)

(defun %sis-make-all ()
  (list (make-%sis-1 :a 1)
        (make-%sis-2 :a 1 :b 2)
        (make-%sis-4 :a 1 :b 2 :c 3 :d 4)
        (make-%sis-6 :a 1 :b 2 :c 3 :d 4 :e 5 :f 6)
        (make-%sis-d :x 1.5d0 :y 2)
        (make-%sis-2c :a 1 :b 2 :c 3)
        (make-%sis-4c :a 1 :b 2 :c 3 :d 4 :e 5 :f 6)
        (make-%sis-1c :b 2 :c 3 :d 4)))

(defun %sis-slot-names (s)
  (mapcar #'dotcl-mop:slot-definition-name
          (dotcl-mop:class-slots (class-of s))))

(defun %sis-values (s)
  (mapcar (lambda (n) (slot-value s n)) (%sis-slot-names s)))

(deftest struct-inline-slots.construct-and-read
  (mapcar #'%sis-values (%sis-make-all))
  ((1) (1 2) (1 2 3 4) (1 2 3 4 5 6) (1.5d0 2) (1 2 3) (1 2 3 4 5 6) (5 2 3 4)))

(deftest struct-inline-slots.accessors
  (let ((c (make-%sis-2c :a 1 :b 2 :c 3))
        (k (make-%sis-4c :a 1 :b 2 :c 3 :d 4 :e 5 :f 6))
        (o (make-%sis-1c :b 2 :c 3 :d 4)))
    (list (%sis-2-a c) (%sis-2-b c) (%sis-2c-a c) (%sis-2c-c c)
          (%sis-4-d k) (%sis-4c-d k) (%sis-4c-f k)
          (%sis-1-a o) (%sis-1c-a o) (%sis-1c-d o)))
  (1 2 1 3 4 4 6 5 5 4))

(deftest struct-inline-slots.setf-accessor
  (let ((s1 (make-%sis-1)) (s4 (make-%sis-4)) (s6 (make-%sis-6))
        (d (make-%sis-d)) (c (make-%sis-2c)) (k (make-%sis-4c)))
    (setf (%sis-1-a s1) :a
          (%sis-4-a s4) 1 (%sis-4-d s4) 4
          (%sis-6-a s6) 1 (%sis-6-f s6) 6
          (%sis-d-x d) 2d0 (%sis-d-y d) :y
          (%sis-2-a c) :pa (%sis-2c-c c) :cc
          (%sis-4-d k) :pd (%sis-4c-f k) :cf)
    (mapcar #'%sis-values (list s1 s4 s6 d c k)))
  ((:a) (1 nil nil 4) (1 nil nil nil nil 6) (2d0 :y) (:pa nil :cc)
   (nil nil nil :pd nil :cf)))

(deftest struct-inline-slots.setf-slot-value
  (mapcar (lambda (s)
            (let ((names (%sis-slot-names s)))
              (loop for n in names for i from 10
                    unless (eq n 'x) do (setf (slot-value s n) i))
              (%sis-values s)))
          (%sis-make-all))
  ((10) (10 11) (10 11 12 13) (10 11 12 13 14 15) (1.5d0 11) (10 11 12)
   (10 11 12 13 14 15) (10 11 12 13)))

(deftest struct-inline-slots.mop-slot-functions
  (let ((s (make-%sis-2 :a 1 :b 2)) (b (make-%sis-6)))
    (flet ((slotd (o n) (find n (dotcl-mop:class-slots (class-of o))
                              :key #'dotcl-mop:slot-definition-name)))
      (setf (dotcl-mop:slot-value-using-class (class-of s) s (slotd s 'b)) :w
            (dotcl-mop:slot-value-using-class (class-of b) b (slotd b 'e)) :e)
      (list (dotcl-mop:slot-value-using-class (class-of s) s (slotd s 'a))
            (%sis-2-b s) (%sis-6-e b)
            (slot-exists-p s 'b) (slot-exists-p s 'c) (slot-boundp s 'a)
            (and (dotcl-mop:slot-boundp-using-class (class-of b) b (slotd b 'f)) t))))
  (1 :w :e t nil t t))

(deftest struct-inline-slots.copy-structure
  (mapcar (lambda (s)
            (let* ((n (first (%sis-slot-names s)))
                   (c (copy-structure s))
                   (before (%sis-values c)))
              (setf (slot-value c n) (if (eq n 'x) 9d0 :new))
              (list (equalp before (%sis-values s))
                    (eq (type-of c) (type-of s))
                    (slot-value s n) (slot-value c n))))
          (%sis-make-all))
  ((t t 1 :new) (t t 1 :new) (t t 1 :new) (t t 1 :new) (t t 1.5d0 9d0)
   (t t 1 :new) (t t 1 :new) (t t 5 :new)))

(deftest struct-inline-slots.copier
  (let* ((o (make-%sis-2 :a 1 :b 2)) (c (copy-%sis-2 o))
         (o6 (make-%sis-6 :a 1)) (c6 (copy-%sis-6 o6)))
    (setf (%sis-2-b c) 3 (%sis-6-a c6) 3)
    (list (%sis-2-b o) (%sis-2-b c) (%sis-6-a o6) (%sis-6-a c6)))
  (2 3 1 3))

(deftest struct-inline-slots.equalp
  (let ((a (%sis-make-all)) (b (%sis-make-all)))
    (list (every #'equalp a b)
          (notany #'equal a b)
          (equalp (make-%sis-2 :a 1 :b "X") (make-%sis-2 :a 1 :b "x"))
          (equalp (make-%sis-2 :a 1 :b 2) (make-%sis-2 :a 1 :b 3))
          (equalp (make-%sis-6 :f 1) (make-%sis-6 :f 2))
          (equalp (make-%sis-2 :a 1 :b 2) (make-%sis-2c :a 1 :b 2))
          (let ((h (make-hash-table :test 'equalp)))
            (setf (gethash (make-%sis-4 :a 1 :b "k") h) :four
                  (gethash (make-%sis-6 :a 1 :b "k") h) :six)
            (list (gethash (make-%sis-4 :a 1 :b "K") h)
                  (gethash (make-%sis-6 :a 1 :b "K") h)
                  (gethash (make-%sis-4 :a 2 :b "k") h)))))
  (t t t nil nil nil (:four :six nil)))

(deftest struct-inline-slots.print-and-read
  (let ((*package* (find-package "CL-USER")) (*print-readably* nil))
    (mapcar (lambda (s)
              (let* ((text (prin1-to-string s))
                     (back (read-from-string text)))
                (list text (equalp back s) (eq (type-of back) (type-of s)))))
            (list (make-%sis-1 :a 1) (make-%sis-2 :a "s" :b '(1))
                  (make-%sis-4 :d 4) (make-%sis-6 :f 6) (make-%sis-2c :c 3)
                  (make-%sis-d :x 1d0))))
  (("#S(%SIS-1 :A 1)" t t)
   ("#S(%SIS-2 :A \"s\" :B (1))" t t)
   ("#S(%SIS-4 :A NIL :B NIL :C NIL :D 4)" t t)
   ("#S(%SIS-6 :A NIL :B NIL :C NIL :D NIL :E NIL :F 6)" t t)
   ("#S(%SIS-2C :A 0 :B NIL :C 3)" t t)
   ("#S(%SIS-D :X 1.0d0 :Y NIL)" t t)))

(deftest struct-inline-slots.read-then-write
  (let ((s (read-from-string "#S(%SIS-2 :A 1 :B 2)"))
        (b (read-from-string "#S(%SIS-6 :A 1 :F 6)")))
    (setf (%sis-2-a s) 10 (%sis-6-f b) 60)
    (list (%sis-2-a s) (%sis-2-b s) (%sis-6-a b) (%sis-6-f b)))
  (10 2 1 60))

(deftest struct-inline-slots.boa-constructor
  (mapcar #'%sis-values
          (list (%sis-bc-mk 1) (%sis-bc-mk 1 3 :c 4)
                (funcall #'%sis-bc-mk 5 6) (apply #'make-%sis-4 '(:a 1 :d 4))))
  ((1 2 nil) (1 3 4) (5 6 nil) (1 nil nil 4)))

(deftest struct-inline-slots.accessor-type-check
  (flet ((te (f) (handler-case (progn (funcall f) :no-error) (type-error () :type-error))))
    (list (te (lambda () (%sis-2-a 5)))
          (te (lambda () (%sis-2-a (make-%sis-4))))
          (te (lambda () (%sis-2c-c (make-%sis-2))))
          (te (lambda () (%sis-6-a (make-%sis-4))))
          (te (lambda () (%sis-4c-e (make-%sis-4))))
          (te (lambda () (setf (%sis-2-a (make-%sis-1)) 1)))
          (te (lambda () (setf (%sis-ty-s (make-%sis-ty)) 5)))
          (te (lambda () (setf (%sis-ty-l (make-%sis-ty)) "x")))
          (te (lambda () (make-%sis-ty :s 5)))
          (%sis-2-a (make-%sis-2c :a :ok))))
  (:type-error :type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :ok))

(deftest struct-inline-slots.typep
  (let ((c (make-%sis-2c)) (p (make-%sis-2)) (k (make-%sis-4c)))
    (list (typep c '%sis-2) (typep c '%sis-2c) (typep p '%sis-2c)
          (%sis-2-p c) (%sis-2c-p p) (typep k '%sis-4) (typep k 'structure-object)
          (type-of c) (typep c 'structure-object)
          (typecase c (%sis-4 :four) (%sis-2c :two-c) (t :other))
          (typecase k (%sis-2 :two) (%sis-4 :four) (t :other))))
  (t t nil t nil t t %sis-2c t :two-c :four))

(defun %sis-redefine (from to)
  (eval from)
  (let ((old (eval '(make-%sis-rd :a 1 :b 2))))
    (eval to)
    (let ((new (eval '(make-%sis-rd :a 10 :b 20))))
      (list (eval `(%sis-rd-a ,new)) (eval `(%sis-rd-b ,new))
            (handler-case (eval `(%sis-rd-a ',old)) (error () :error))
            (%sis-values new)))))

(deftest struct-inline-slots.redefine
  (list (%sis-redefine '(defstruct %sis-rd a b) '(defstruct %sis-rd a b))
        (%sis-redefine '(defstruct %sis-rd a b) '(defstruct %sis-rd b a))
        (%sis-redefine '(defstruct %sis-rd a b) '(defstruct %sis-rd a b c d e f))
        (%sis-redefine '(defstruct %sis-rd a b c d e f) '(defstruct %sis-rd a b)))
  ((10 20 1 (10 20))
   (10 20 :error (20 10))
   (10 20 :error (10 20 nil nil nil nil))
   (10 20 :error (10 20))))

(deftest-emitting-only struct-inline-slots.fasl
  (let ((src "sis-fasl-tmp.lisp") (fasl "sis-fasl-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(defstruct %sis-f2 a b)~%")
            (format s "(defstruct %sis-f6 a b c d e f)~%")
            (format s "(defun %sis-f-lit () (list '#S(%sis-f2 :a 1 :b (x)) '#S(%sis-f6 :f 6)))~%")
            (format s "(defun %sis-f-new (x) (make-%sis-f2 :a x :b (1+ x)))~%")
            (format s "(defun %sis-f-get (s) (%sis-f2-b s))~%")
            (format s "(defun %sis-f-set (s v) (setf (%sis-f2-a s) v) s)~%"))
          (compile-file src :output-file fasl)
          (load fasl)
          (let* ((lits (funcall '%sis-f-lit))
                 (n (funcall '%sis-f-new 4)))
            (list (%sis-values (first lits)) (%sis-values (second lits))
                  (funcall '%sis-f-get n)
                  (%sis-values (funcall '%sis-f-set (copy-structure n) :z))
                  (%sis-values n)
                  (eq (first lits) (first (funcall '%sis-f-lit)))
                  (funcall '%sis-f-get (first lits)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  ((1 (x)) (nil nil nil nil nil 6) 5 (:z 5) (4 5) t (x)))
