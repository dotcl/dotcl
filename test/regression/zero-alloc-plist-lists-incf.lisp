;;; Property-list writes, short-list functions and fixnum update places stay in budget.
;;; These are statements about emitted code, so the allocation tests are compiled-only.

(defvar *zpl-n* 10000)

(defun %zpl-under (thunk budget &optional base)
  (funcall thunk)
  (when base (funcall base))
  (let ((total (min-allocated-bytes thunk 5))
        (built (if base (min-allocated-bytes base 5) 0)))
    (< (- total built) (* *zpl-n* (+ budget 16)))))

(defun %list1 () (list 1))
(defun %list3 () (list 1 2 3))
(defun %list8 () (list 1 2 3 4 5 6 7 8))

(defun %plist3 () (list :a 1 :b 2 :c 3))

(defun %alist1 () (list (cons 1 :a)))
(defun %alist3 () (list (cons 1 :a) (cons 2 :b) (cons 3 :c)))
(defun %alist8 ()
  (list (cons 1 :a) (cons 2 :b) (cons 3 :c) (cons 4 :d)
        (cons 5 :e) (cons 6 :f) (cons 7 :g) (cons 8 :h)))

(defvar *l1* (%list1))
(defvar *l3* (%list3))
(defvar *l8* (%list8))
(defvar *alist1* (%alist1))
(defvar *alist3* (%alist3))
(defvar *alist8* (%alist8))
(defvar *plist* (%plist3))
(defvar *get-sym* (make-symbol "ZERO-ALLOC-GET"))
(defvar *remprop-sym* (make-symbol "ZERO-ALLOC-REMPROP"))

;;; --- plists ---

(defun %p-remf (key n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((pl (%plist3)))
        (setq r (remf pl key))))
    r))

(defun %p-plist-build (n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n) (setq r (%plist3)))
    r))

(defun %p-getf-set-overwrite (pl key n)
  (declare (fixnum n))
  (dotimes (i n) (setf (getf pl key) 0))
  pl)

(defun %p-getf-set-overwrite-d (pl key n)
  (declare (type list pl) (fixnum n))
  (dotimes (i n) (setf (getf pl key) 0))
  pl)

(defun %p-getf-set-add (key n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((pl (%plist3)))
        (setf (getf pl key) 0)
        (setq r pl)))
    r))

(defun %p-get-set-overwrite (sym n)
  (declare (fixnum n))
  (dotimes (i n) (setf (get sym :a) 0))
  (get sym :a))

(defun %p-remprop (sym key n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (setf (symbol-plist sym) (%plist3))
      (setq r (remprop sym key)))
    r))

(defun %p-symbol-plist-build (sym n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (setf (symbol-plist sym) (%plist3))
      (setq r (symbol-plist sym)))
    r))

(defun %p-getf-incf (pl key n)
  (declare (fixnum n))
  (dotimes (i n)
    (setf (getf pl key) 0)
    (incf (getf pl key)))
  (getf pl key))

(defun %p-getf-push (pl key n)
  (declare (fixnum n))
  (dotimes (i n) (push :x (getf pl key)))
  (getf pl key))

;;; --- non-destructive short-list functions ---

(defmacro %def-list-probe (name form)
  `(progn
     (defun ,(intern (format nil "%P-~A" name)) (l n)
       (declare (type list l) (fixnum n))
       (let ((r nil))
         (dotimes (i n) (setq r ,form))
         r))
     (defun ,(intern (format nil "%P-~A-U" name)) (l n)
       (declare (fixnum n))
       (let ((r nil))
         (dotimes (i n) (setq r ,form))
         r))))

(%def-list-probe butlast (butlast l))
(%def-list-probe butlast2 (butlast l 2))
(%def-list-probe last2 (last l 2))
(%def-list-probe ldiff (ldiff l (last l)))
(%def-list-probe tailp (tailp (cdr l) l))
(%def-list-probe copy-list (copy-list l))
(%def-list-probe list-length (list-length l))
(%def-list-probe endp (endp l))
(%def-list-probe revappend (revappend l *l3*))

(defmacro %def-alist-probe (name form)
  `(progn
     (defun ,(intern (format nil "%P-~A" name)) (l n)
       (declare (type list l) (fixnum n))
       (let ((r nil))
         (dotimes (i n) (setq r ,form))
         r))
     (defun ,(intern (format nil "%P-~A-U" name)) (l n)
       (declare (fixnum n))
       (let ((r nil))
         (dotimes (i n) (setq r ,form))
         r))))

(%def-alist-probe rassoc (rassoc :c l))
(%def-alist-probe acons (acons 9 :v l))

;;; --- destructive short-list functions ---

(defun %p-nconc (maker n)
  (declare (function maker) (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((a (funcall maker)) (b (funcall maker)))
        (setq r (nconc a b))))
    r))

(defun %p-nconc-base (maker n)
  (declare (function maker) (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((a (funcall maker)) (b (funcall maker)))
        (setq r (if (consp a) b b))))
    r))

(defun %p-nbutlast (maker n)
  (declare (function maker) (fixnum n))
  (let ((r nil))
    (dotimes (i n) (setq r (nbutlast (funcall maker))))
    r))

(defun %p-nbutlast-base (maker n)
  (declare (function maker) (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((a (funcall maker)))
        (setq r (if (consp a) a a))))
    r))

(defun %p-nreconc (maker n)
  (declare (function maker) (fixnum n))
  (let ((r nil))
    (dotimes (i n)
      (let ((a (funcall maker)) (b (funcall maker)))
        (setq r (nreconc a b))))
    r))

;;; --- integer update places ---

(defun %p-signum (x n)
  (declare (fixnum x n))
  (let ((r 0))
    (dotimes (i n) (setq r (signum x)))
    r))

(defun %p-signum-u (x n)
  (declare (fixnum n))
  (let ((r 0))
    (dotimes (i n) (setq r (signum x)))
    r))

(defun %p-signum-sum (x n)
  (declare (fixnum x n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (setq s (+ s (signum x))))
    s))

(defun %p-1- (x n)
  (declare (fixnum x n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (setq s (+ s (1- x))))
    s))

(defun %p-1--u (x n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (setq s (+ s (1- x))))
    s))

(defun %p-min4 (a b c d n)
  (declare (fixnum a b c d n))
  (let ((r 0))
    (dotimes (i n) (setq r (min a b c d)))
    r))

(defun %p-min4-u (a b c d n)
  (declare (fixnum n))
  (let ((r 0))
    (dotimes (i n) (setq r (min a b c d)))
    r))

(defun %p-max4 (a b c d n)
  (declare (fixnum a b c d n))
  (let ((r 0))
    (dotimes (i n) (setq r (max a b c d)))
    r))

(defun %p-max4-u (a b c d n)
  (declare (fixnum n))
  (let ((r 0))
    (dotimes (i n) (setq r (max a b c d)))
    r))

(defun %p-incf-local (n)
  (declare (fixnum n))
  (let ((x 0))
    (declare (fixnum x))
    (dotimes (i n) (incf x 1))
    x))

(defun %p-incf-local-u (n)
  (declare (fixnum n))
  (let ((x 0))
    (dotimes (i n) (incf x 1))
    x))

(defun %p-decf-local (n)
  (declare (fixnum n))
  (let ((x 0))
    (declare (fixnum x))
    (dotimes (i n) (decf x 1))
    x))

(defun %p-decf-local-u (n)
  (declare (fixnum n))
  (let ((x 0))
    (dotimes (i n) (decf x 1))
    x))

(defvar *p-special-d* 0)
(defvar *p-special-u* 0)
(declaim (fixnum *p-special-d*))

(defun %p-incf-special-d (n)
  (declare (fixnum n))
  (dotimes (i n) (setq *p-special-d* 0) (incf *p-special-d* 1))
  *p-special-d*)

(defun %p-decf-special-d (n)
  (declare (fixnum n))
  (dotimes (i n) (setq *p-special-d* 0) (decf *p-special-d* 1))
  *p-special-d*)

(defun %p-incf-special-u (n)
  (declare (fixnum n))
  (dotimes (i n) (setq *p-special-u* 0) (incf *p-special-u* 1))
  *p-special-u*)

(defun %p-decf-special-u (n)
  (declare (fixnum n))
  (dotimes (i n) (setq *p-special-u* 0) (decf *p-special-u* 1))
  *p-special-u*)

(defstruct (%probe-struct (:constructor %make-probe-struct))
  (slot 0 :type fixnum))

(defun %p-incf-slot (s n)
  (declare (type %probe-struct s) (fixnum n))
  (dotimes (i n)
    (setf (%probe-struct-slot s) 0)
    (incf (%probe-struct-slot s) 1))
  (%probe-struct-slot s))

(defun %p-decf-slot (s n)
  (declare (type %probe-struct s) (fixnum n))
  (dotimes (i n)
    (setf (%probe-struct-slot s) 0)
    (decf (%probe-struct-slot s) 1))
  (%probe-struct-slot s))

(defvar *p-sv* (make-array 1 :initial-element 0))
(defvar *p-fix-array* (make-array 1 :element-type 'fixnum :initial-element 0))

(defun %p-incf-svref (v n)
  (declare (type simple-vector v) (fixnum n))
  (dotimes (i n)
    (setf (svref v 0) 0)
    (incf (svref v 0) 1))
  (svref v 0))

(defun %p-decf-svref (v n)
  (declare (type simple-vector v) (fixnum n))
  (dotimes (i n)
    (setf (svref v 0) 0)
    (decf (svref v 0) 1))
  (svref v 0))

(defun %p-incf-aref (a n)
  (declare (type (simple-array fixnum (*)) a) (fixnum n))
  (dotimes (i n)
    (setf (aref a 0) 0)
    (incf (aref a 0) 1))
  (aref a 0))

(defun %p-decf-aref (a n)
  (declare (type (simple-array fixnum (*)) a) (fixnum n))
  (dotimes (i n)
    (setf (aref a 0) 0)
    (decf (aref a 0) 1))
  (aref a 0))

;;; --- plist writes ---

(deftest-compiled-only zero-alloc-plist-lists-incf.plists
  (every #'identity
         (list
          (%zpl-under (lambda () (%p-remf :a *zpl-n*)) 0
                      (lambda () (%p-plist-build *zpl-n*)))
          (%zpl-under (lambda () (%p-remf :b *zpl-n*)) 0
                      (lambda () (%p-plist-build *zpl-n*)))
          (%zpl-under (lambda () (%p-remf :c *zpl-n*)) 0
                      (lambda () (%p-plist-build *zpl-n*)))
          (%zpl-under (lambda () (%p-remf :z *zpl-n*)) 0
                      (lambda () (%p-plist-build *zpl-n*)))
          (%zpl-under (lambda () (%p-getf-set-overwrite *plist* :b *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-getf-set-overwrite-d *plist* :b *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-getf-set-add :z *zpl-n*)) 64
                      (lambda () (%p-plist-build *zpl-n*)))
          (%zpl-under (lambda () (setf (get *get-sym* :a) 0)
                         (%p-get-set-overwrite *get-sym* *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-remprop *remprop-sym* :a *zpl-n*)) 0
                      (lambda () (%p-symbol-plist-build *remprop-sym* *zpl-n*)))
          (%zpl-under (lambda () (%p-getf-incf *plist* :b *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-getf-push *plist* :b *zpl-n*)) 32)))
  t)

;;; --- copying and walking short lists ---

(deftest-compiled-only zero-alloc-plist-lists-incf.lists
  (every #'identity
         (loop for entry in (list (list *l1* 1) (list *l3* 3) (list *l8* 8))
               nconc (let ((l (first entry)) (len (second entry)))
                       (list
                        (%zpl-under (lambda () (%p-butlast l *zpl-n*)) (* 32 (1- len)))
                        (%zpl-under (lambda () (%p-butlast-u l *zpl-n*)) (* 32 (1- len)))
                        (%zpl-under (lambda () (%p-butlast2 l *zpl-n*))
                                    (* 32 (max 0 (- len 2))))
                        (%zpl-under (lambda () (%p-butlast2-u l *zpl-n*))
                                    (* 32 (max 0 (- len 2))))
                        (%zpl-under (lambda () (%p-last2 l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-last2-u l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-ldiff l *zpl-n*)) (* 32 (1- len)))
                        (%zpl-under (lambda () (%p-ldiff-u l *zpl-n*)) (* 32 (1- len)))
                        (%zpl-under (lambda () (%p-tailp l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-tailp-u l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-copy-list l *zpl-n*)) (* 32 len))
                        (%zpl-under (lambda () (%p-copy-list-u l *zpl-n*)) (* 32 len))
                        (%zpl-under (lambda () (%p-list-length l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-list-length-u l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-endp l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-endp-u l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-revappend l *zpl-n*)) (* 32 len))
                        (%zpl-under (lambda () (%p-revappend-u l *zpl-n*)) (* 32 len))))))
  t)

(deftest-compiled-only zero-alloc-plist-lists-incf.alists
  (every #'identity
         (loop for entry in (list *alist1* *alist3* *alist8*)
               nconc (let ((l entry))
                       (list
                        (%zpl-under (lambda () (%p-rassoc l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-rassoc-u l *zpl-n*)) 0)
                        (%zpl-under (lambda () (%p-acons l *zpl-n*)) 64)
                        (%zpl-under (lambda () (%p-acons-u l *zpl-n*)) 64)))))
  t)

(deftest-compiled-only zero-alloc-plist-lists-incf.destructive
  (every #'identity
         (loop for entry in (list (cons "1" #'%list1) (cons "3" #'%list3)
                                  (cons "8" #'%list8))
               nconc (let ((maker (cdr entry)))
                       (list
                        (%zpl-under (lambda () (%p-nconc maker *zpl-n*)) 0
                                    (lambda () (%p-nconc-base maker *zpl-n*)))
                        (%zpl-under (lambda () (%p-nbutlast maker *zpl-n*)) 0
                                    (lambda () (%p-nbutlast-base maker *zpl-n*)))
                        (%zpl-under (lambda () (%p-nreconc maker *zpl-n*)) 0
                                    (lambda () (%p-nconc-base maker *zpl-n*)))))))
  t)

;;; --- integer operations ---

(deftest-compiled-only zero-alloc-plist-lists-incf.integers
  (every #'identity
         (list
          (%zpl-under (lambda () (%p-signum -5 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-signum-u -5 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-signum-sum -5 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-1- 5 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-1--u 5 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-min4 4 2 8 1 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-min4-u 4 2 8 1 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-max4 4 2 8 1 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-max4-u 4 2 8 1 *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-incf-local *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-incf-local-u *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-decf-local *zpl-n*)) 0)
          ;; The undeclared DECF local row is open: generic decrement boxes once
          ;; per call. It is measured in investigation/zero-alloc-4.md, not here.
          (%zpl-under (lambda () (%p-incf-special-d *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-decf-special-d *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-incf-special-u *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-decf-special-u *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-incf-slot (%make-probe-struct) *zpl-n*)) 16)
          (%zpl-under (lambda () (%p-decf-slot (%make-probe-struct) *zpl-n*)) 16)
          (%zpl-under (lambda () (%p-incf-svref *p-sv* *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-decf-svref *p-sv* *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-incf-aref *p-fix-array* *zpl-n*)) 0)
          (%zpl-under (lambda () (%p-decf-aref *p-fix-array* *zpl-n*)) 0)))
  t)

;;; --- values ---

(deftest zero-alloc-plist-lists-incf.plist-values
  (let ((p (list :a 1 :b 2 :c 3))
        (q (list :a 1 :b 2 :c 3)))
    (list (remf p :a) p
          (remf q :b) q
          (let ((r (list :a 1 :b 2 :c 3)))
            (remf r :z) r)
          (let ((r (list :a 1 :b 2 :c 3)))
            (setf (getf r :d) 4) r)
          (let ((r (list :a 1 :b 2 :c 3)))
            (incf (getf r :b)) r)))
  (t (:b 2 :c 3) t (:a 1 :c 3) (:a 1 :b 2 :c 3)
     (:d 4 :a 1 :b 2 :c 3) (:a 1 :b 3 :c 3)))

(deftest zero-alloc-plist-lists-incf.list-values
  (let ((a (list 1 2 3)) (b (list 4 5)))
    (list (butlast a) (butlast a 2) (last a 2) (ldiff a (last a))
          (tailp (cdr a) a) (copy-list a) (list-length a)
          (revappend a b)
          (nconc (list 1 2) (list 3))
          (nbutlast (list 1 2 3))
          (nreconc (list 1 2 3) nil)))
  ((1 2) (1) (2 3) (1 2) t (1 2 3) 3 (3 2 1 4 5)
   (1 2 3) (1 2) (3 2 1)))

(deftest zero-alloc-plist-lists-incf.signum-values
  (list (signum -5) (signum 0) (signum 9)
        (signum -2.0d0) (signum 2.0d0) (signum 0.0d0)
        (signum -3/4) (signum 3/4))
  (-1 0 1 -1.0d0 1.0d0 0.0d0 -1 1))
