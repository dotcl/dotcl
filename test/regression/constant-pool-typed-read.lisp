;;; Literals in compiled code read back as the objects they were (symbols,
;;; lists, strings, structures, numbers), one value each, and a structure
;;; accessor in a loop still rejects an unrelated structure with TYPE-ERROR.

(defstruct cptr-a x)
(defstruct cptr-b y)
(defstruct (cptr-c (:include cptr-a)) z)

(defun %cptr-literals ()
  (list 'cptr-sym :cptr-key '(1 cptr-sym "s") "str" #\c 3/4 2.5d0 nil t
        #S(cptr-a :x 5)))

(defun %cptr-count-sym (l)
  (let ((n 0)) (dolist (x l n) (when (eq x 'cptr-sym) (incf n)))))

(defun %cptr-sum-x (objs)
  (let ((s 0)) (dolist (o objs s) (incf s (cptr-a-x o)))))

(defun %cptr-two () (values 'cptr-sym '(a b)))

(deftest constant-pool-typed-read.literals
  (let ((l (%cptr-literals)))
    (list (eq (first l) 'cptr-sym) (eq (second l) :cptr-key)
          (eq (second (third l)) 'cptr-sym) (fourth l) (fifth l) (sixth l)
          (seventh l) (eighth l) (ninth l) (cptr-a-x (tenth l))
          (eq (first (%cptr-literals)) (first l))
          (eq (third (%cptr-literals)) (third l))))
  (t t t "str" #\c 3/4 2.5d0 nil t 5 t t))

(deftest constant-pool-typed-read.values
  (list (multiple-value-list (%cptr-two))
        (multiple-value-list (%cptr-count-sym '(a cptr-sym b cptr-sym))))
  ((cptr-sym (a b)) (2)))

(deftest constant-pool-typed-read.struct-check-in-loop
  (list (%cptr-sum-x (list (make-cptr-a :x 1) (make-cptr-c :x 2 :z 0)))
        (handler-case (%cptr-sum-x (list (make-cptr-a :x 1) (make-cptr-b :y 2)))
          (type-error (e) (list :type-error (type-error-expected-type e)))))
  (3 (:type-error cptr-a)))

;;; The other constant-pool reads of compiled code: keywords, symbols of
;;; another package, global symbols, string literals, &key name vectors,
;;; slot accessor and generic function call caches, LOAD-TIME-VALUE cells,
;;; closures, and a function re-registered after FMAKUNBOUND.

(defpackage #:cptr-other (:use #:cl) (:export #:*v* #:f))
(defvar cptr-other:*v* 7)
(defun cptr-other:f (x) (* x 2))

(defclass cptr-cls () ((s :initarg :s :accessor cptr-cls-s)))
(defgeneric cptr-gf (x))
(defmethod cptr-gf ((x cptr-cls)) (list :cls (cptr-cls-s x)))
(defmethod cptr-gf ((x integer)) (list :int x))

(defun %cptr-key (&key (alpha 1) (beta 2)) (list alpha beta))

(defun %cptr-other-reads ()
  (list :kw (eq (car (list :cptr-kw)) :cptr-kw)
        cptr-other:*v* (cptr-other:f 3)
        (symbol-package 'cptr-other:f)
        "lit" (%cptr-key :beta 5)
        (let ((o (make-instance 'cptr-cls :s 1)))
          (setf (cptr-cls-s o) (+ (cptr-cls-s o) 10))
          (list (cptr-cls-s o) (cptr-gf o) (cptr-gf 4)))
        (load-time-value (list 'cptr-ltv))
        (funcall (let ((n 3)) (lambda (x) (+ x n))) 4)))

(deftest constant-pool-typed-read.other-reads
  (let ((r (%cptr-other-reads)))
    (list (eq (load-time-value (find-package "CPTR-OTHER")) (fifth r))
          (subseq r 0 4) (sixth r) (seventh r) (eighth r) (ninth r) (tenth r)
          (eq (ninth r) (ninth (%cptr-other-reads)))))
  (t (:kw t 7 6) "lit" (1 5) (11 (:cls 11) (:int 4)) (cptr-ltv) 7 t))

(progn
  (fmakunbound '%cptr-rereg)
  (defun %cptr-rereg () :cptr-rereg))

(deftest constant-pool-typed-read.reregister
  (list (%cptr-rereg) (fboundp '%cptr-rereg) (functionp #'%cptr-rereg))
  (:cptr-rereg t t))
