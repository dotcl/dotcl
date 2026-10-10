;;; The table behind a generic function's front dispatch cache is looked up with the
;;; call's own arguments. A call whose classes were in the table but not among the
;;; last few in the front cache used to build an argument array and a key array to
;;; look the entry up (Coalton's type checker spent a fifth of its allocation there),
;;; and the one-argument entry point did the same for EQL-specialized and slot reader
;;; entries. These check the values on the table path, that it allocates nothing, and
;;; that the table still follows method and class changes.

(defstruct (gft-ty (:constructor nil)))
(defstruct (gft-a (:include gft-ty)))
(defstruct (gft-b (:include gft-ty)))
(defstruct (gft-c (:include gft-ty)))
(defstruct (gft-d (:include gft-ty)))
(defstruct (gft-e (:include gft-ty)))
(defstruct (gft-f (:include gft-ty)))

(defvar *gft-structs*
  (vector (make-gft-a) (make-gft-b) (make-gft-c) (make-gft-d) (make-gft-e) (make-gft-f)))

;; Two arguments, both specialized: 36 class pairs in rotation.
(defgeneric gft-eq (x y))
(defmethod gft-eq ((x gft-a) (y gft-a)) :aa)
(defmethod gft-eq ((x gft-b) (y gft-b)) :bb)
(defmethod gft-eq ((x gft-c) (y gft-c)) :cc)
(defmethod gft-eq ((x gft-ty) (y gft-ty)) nil)

;; One argument, six classes.
(defgeneric gft-kind (x))
(defmethod gft-kind ((x gft-ty)) :ty)
(defmethod gft-kind ((x gft-a)) :a)
(defmethod gft-kind ((x gft-d)) (list :d (call-next-method)))

;; One argument with EQL methods, six classes.
(defgeneric gft-eql (x))
(defmethod gft-eql ((x (eql 3))) :three)
(defmethod gft-eql ((x (eql :k))) :k)
(defmethod gft-eql ((x integer)) :int)
(defmethod gft-eql ((x symbol)) :sym)
(defmethod gft-eql ((x string)) :str)
(defmethod gft-eql ((x cons)) :cons)
(defmethod gft-eql ((x character)) :char)
(defmethod gft-eql ((x gft-ty)) :ty)

(defvar *gft-eql-args* (vector 3 4 :k :j "s" '(1) #\c (make-gft-a)))

(defun %gft-pairs ()
  (let ((out '()))
    (dotimes (i 6) (dotimes (j 6) (push (cons (aref *gft-structs* i) (aref *gft-structs* j)) out)))
    (coerce (nreverse out) 'vector)))
(defvar *gft-pairs* (%gft-pairs))

(deftest gf-dispatch-table-loose.two-args
  (let ((out '()))
    (dotimes (k 72)
      (let ((p (aref *gft-pairs* (mod k 36))))
        (push (gft-eq (car p) (cdr p)) out)))
    (subseq (nreverse out) 36))
  (:aa nil nil nil nil nil
   nil :bb nil nil nil nil
   nil nil :cc nil nil nil
   nil nil nil nil nil nil
   nil nil nil nil nil nil
   nil nil nil nil nil nil))

(deftest gf-dispatch-table-loose.one-arg
  (let ((out '()))
    (dotimes (k 18) (push (gft-kind (aref *gft-structs* (mod k 6))) out))
    (subseq (nreverse out) 12))
  (:a :ty :ty (:d :ty) :ty :ty))

(deftest gf-dispatch-table-loose.eql
  (let ((out '()))
    (dotimes (k 24) (push (gft-eql (aref *gft-eql-args* (mod k 8))) out))
    (subseq (nreverse out) 16))
  (:three :int :k :sym :str :cons :char :ty))

;; A slot reader whose generic function also has a hand-written method, over more
;; classes than the front cache holds.
(defclass gft-r1 () ((v :initarg :v :accessor gft-v)))
(defclass gft-r2 () ((v :initarg :v :accessor gft-v)))
(defclass gft-r3 () ((v :initarg :v :accessor gft-v)))
(defclass gft-r4 () ((v :initarg :v :accessor gft-v)))
(defclass gft-r5 () ((v :initarg :v :accessor gft-v)))
(defclass gft-r6 () ((v :initarg :v :accessor gft-v)))
(defmethod gft-v ((x gft-ty)) :struct)
(defvar *gft-readers*
  (vector (make-instance 'gft-r1 :v 1) (make-instance 'gft-r2 :v 2) (make-instance 'gft-r3 :v 3)
          (make-instance 'gft-r4 :v 4) (make-instance 'gft-r5 :v 5) (make-instance 'gft-r6 :v 6)
          (make-gft-b)))

(deftest gf-dispatch-table-loose.reader
  (let ((out '()))
    (dotimes (k 21) (push (gft-v (aref *gft-readers* (mod k 7))) out))
    (subseq (nreverse out) 14))
  (1 2 3 4 5 6 :struct))

;; A method added after the table is warm is seen at once on the table path, and so is
;; one removed.
(deftest gf-dispatch-table-loose.method-added-and-removed
  (let ((objs (coerce *gft-structs* 'list)))
    (dotimes (k 3) (mapc #'gft-kind objs))
    (let ((m (defmethod gft-kind ((x gft-b)) :b)))
      (prog1 (list (mapcar #'gft-kind objs)
                   (progn (remove-method #'gft-kind m) (mapcar #'gft-kind objs)))
        nil)))
  ((:a :b :ty (:d :ty) :ty :ty)
   (:a :ty :ty (:d :ty) :ty :ty)))

;; A class redefined while its entry is in the table but not in the front cache.
(defclass gft-p () ())
(defclass gft-q () ())
(defclass gft-k1 (gft-p) ())
(defclass gft-k2 (gft-p) ())
(defclass gft-k3 (gft-p) ())
(defclass gft-k4 (gft-p) ())
(defclass gft-k5 (gft-p) ())
(defclass gft-k6 (gft-p) ())
(defgeneric gft-side (x))
(defmethod gft-side ((x gft-p)) :p)
(defmethod gft-side ((x gft-q)) :q)

(deftest gf-dispatch-table-loose.class-redefined
  (let ((objs (mapcar #'make-instance '(gft-k1 gft-k2 gft-k3 gft-k4 gft-k5 gft-k6))))
    (dotimes (k 3) (mapc #'gft-side objs))
    (eval '(defclass gft-k1 (gft-q) ()))
    (prog1 (list (gft-side (first objs)) (gft-side (second objs)))
      (eval '(defclass gft-k1 (gft-p) ()))))
  (:q :p))

;; CHANGE-CLASS moves an instance to a class whose entry is in the table.
(deftest gf-dispatch-table-loose.change-class
  (let ((objs (mapcar #'make-instance '(gft-k1 gft-k2 gft-k3 gft-k4 gft-k5 gft-k6)))
        (x (make-instance 'gft-k2)))
    (dotimes (k 3) (mapc #'gft-side objs))
    (let ((before (gft-side x)))
      (change-class x 'gft-q)
      (list before (gft-side x))))
  (:p :q))

(defun %gft-bytes () (nth 4 (dotcl:gc-stats)))

(defun %gft-run-pairs (n)
  (let ((v *gft-pairs*) (c 0))
    (dotimes (k n) (let ((p (aref v (mod k 36)))) (when (gft-eq (car p) (cdr p)) (incf c))))
    c))
(defun %gft-run-one (n)
  (let ((v *gft-structs*)) (dotimes (k n) (gft-kind (aref v (mod k 6))))))
(defun %gft-run-eql (n)
  (let ((v *gft-eql-args*)) (dotimes (k n) (gft-eql (aref v (mod k 8))))))
(defun %gft-run-reader (n)
  (let ((v *gft-readers*)) (dotimes (k n) (gft-v (aref v (mod k 7))))))

(deftest-compiled-only gf-dispatch-table-loose.no-allocation
  (flet ((per-call (f)
           (funcall f 1000)
           (let ((b0 (%gft-bytes)))
             (funcall f 36000)
             (/ (- (%gft-bytes) b0) 36000.0))))
    ;; The one-argument chain with CALL-NEXT-METHOD (GFT-D) builds its closure
    ;; lazily only when asked; the LIST it returns (64 B, one call in six) is the
    ;; method's own.
    (list (< (per-call #'%gft-run-pairs) 1)
          (< (per-call #'%gft-run-one) 16)
          (< (per-call #'%gft-run-eql) 1)
          (< (per-call #'%gft-run-reader) 1)))
  (t t t t))
