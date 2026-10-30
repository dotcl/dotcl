;;; (THE <array type> X) and a declared array parameter, above SAFETY 0: a value
;;; that is not of the type signals a TYPE-ERROR, also where the access reads
;;; the array on a specialized path. At SAFETY 0 THE is trusted.

(setf dotcl:*save-sil* t)

(defmacro %tatc-define-cases ()
  (let ((defs '()) (cases '()))
    (dolist (et '(fixnum (signed-byte 32) (unsigned-byte 8) (unsigned-byte 16)
                  (unsigned-byte 32) (unsigned-byte 64) double-float single-float t))
      (dolist (rank '(1 2 3))
        (let* ((ty `(simple-array ,et ,(make-list rank :initial-element '*)))
               (zs (make-list rank :initial-element 0))
               (the-fn (intern (format nil "%TATC-THE-~a-~d" et rank)))
               (decl-fn (intern (format nil "%TATC-DECL-~a-~d" et rank))))
          (push `(defun ,the-fn (a) (aref (the ,ty a) ,@zs)) defs)
          (push `(defun ,decl-fn (a) (declare (type ,ty a)) (aref a ,@zs)) defs)
          (push `(list ',et ,rank #',the-fn #',decl-fn) cases))))
    `(progn ,@(nreverse defs) (defparameter *tatc-cases* (list ,@(nreverse cases))))))

(%tatc-define-cases)

(defun %tatc-outcome (fn arg)
  (handler-case (funcall fn arg)
    (type-error () :type-error)))

(defun %tatc-array (et rank)
  (make-array (make-list rank :initial-element 2)
              :element-type et
              :initial-element (cond ((eq et 'double-float) 2d0)
                                     ((eq et 'single-float) 2f0)
                                     (t 2))))

;; Each case: the value of the right array, then the outcome for an array of the
;; same rank with another element type, a simple-vector, and a list. SBCL 2.6.8
;; gives the same table; the one value read is the simple-vector against
;; (SIMPLE-ARRAY T (*)), which it is.
(defun %tatc-table (pick)
  (loop for (et rank the-fn decl-fn) in *tatc-cases*
        for fn = (if (eq pick :the) the-fn decl-fn)
        collect (list (%tatc-outcome fn (%tatc-array et rank))
                      (%tatc-outcome fn (%tatc-array (if (eq et t) 'double-float t) rank))
                      (%tatc-outcome fn (vector 1 2 3))
                      (%tatc-outcome fn (list 1 2 3)))))

(defun %tatc-expected ()
  (loop for (et rank) in *tatc-cases*
        collect (list (cond ((eq et 'double-float) 2d0)
                            ((eq et 'single-float) 2f0)
                            (t 2))
                      :type-error
                      (if (and (eq et t) (= rank 1)) 1 :type-error)
                      :type-error)))

(deftest the-array-type-check.the
  (equal (%tatc-table :the) (%tatc-expected))
  t)

(deftest the-array-type-check.declare
  (equal (%tatc-table :declare) (%tatc-expected))
  t)

;; The issue's shape: rank 3 (unsigned-byte 64), a simple-vector.
(defun %tatc-u64-3d (a i j k)
  (aref (the (simple-array (unsigned-byte 64) (* * *)) a) i j k))

(deftest the-array-type-check.u64-3d-simple-vector
  (list (%tatc-outcome (lambda (a) (%tatc-u64-3d a 0 0 0)) (vector 1 2 3))
        (%tatc-u64-3d (make-array '(1 1 2) :element-type '(unsigned-byte 64)
                                           :initial-element #xFFFFFFFFFFFFFFFF)
                      0 0 1))
  (:type-error #xFFFFFFFFFFFFFFFF))

;; Other uses of the asserted value.
(defstruct %tatc-holder a)

(defun %tatc-setf (a) (setf (aref (the (simple-array (unsigned-byte 8) (*)) a) 0) 7))
(defun %tatc-let (a) (let ((b (the (simple-array fixnum (* *)) a))) (aref b 0 0)))
(defun %tatc-row-major (a) (row-major-aref (the (simple-array double-float (*)) a) 0))
(defun %tatc-dim (a) (array-dimension (the (simple-array (unsigned-byte 64) (* * *)) a) 0))
(defun %tatc-slot (h) (aref (the (simple-array fixnum (* *)) (%tatc-holder-a h)) 0 0))
(defun %tatc-loop (a)
  (let ((s 0))
    (dotimes (i 2 s) (incf s (aref (the (simple-array (unsigned-byte 32) (*)) a) i)))))

(deftest the-array-type-check.other-uses
  (let ((w1 (make-array 2 :initial-element 1))
        (w2 (make-array '(2 2) :initial-element 1)))
    (list (%tatc-outcome #'%tatc-setf w1)
          (%tatc-outcome #'%tatc-let w2)
          (%tatc-outcome #'%tatc-row-major w1)
          (%tatc-outcome #'%tatc-dim (vector 1 2 3))
          (%tatc-outcome #'%tatc-slot (make-%tatc-holder :a w2))
          (%tatc-outcome #'%tatc-loop w1)
          (%tatc-setf (make-array 2 :element-type '(unsigned-byte 8)))
          (%tatc-let (make-array '(2 2) :element-type 'fixnum :initial-element 5))
          (%tatc-row-major (make-array 2 :element-type 'double-float :initial-element 1.5d0))
          (%tatc-dim (make-array '(3 1 1) :element-type '(unsigned-byte 64)))
          (%tatc-slot (make-%tatc-holder :a (make-array '(2 2) :element-type 'fixnum
                                                               :initial-element 6)))
          (%tatc-loop (make-array 2 :element-type '(unsigned-byte 32) :initial-element 4))))
  (:type-error :type-error :type-error :type-error :type-error :type-error 7 5 1.5d0 3 6 8))

;; The check is emitted above SAFETY 0 and not at SAFETY 0.
(defun %tatc-s1 (a) (aref (the (simple-array (unsigned-byte 64) (* * *)) a) 0 0 0))
(defun %tatc-s0 (a)
  (declare (optimize (safety 0)))
  (aref (the (simple-array (unsigned-byte 64) (* * *)) a) 0 0 0))

(deftest-compiled-only the-array-type-check.sil
  (list (and (search "Runtime.CheckTheArray" (%flat-text (dotcl:function-sil #'%tatc-s1))) t)
        (search "Runtime.CheckArg" (%flat-text (dotcl:function-sil #'%tatc-s0))))
  (t nil))

;; A parameter declared with the same or a narrower array type and never
;; assigned was checked on entry: THE over it emits no second check. A wider
;; declaration, or an assigned variable, keeps the check.
(defun %tatc-decl-same (a n)
  (declare (type (simple-array fixnum (* *)) a) (fixnum n))
  (let ((s 0))
    (dotimes (i n s) (incf s (aref (the (simple-array fixnum (* *)) a) i 0)))))
(defun %tatc-decl-narrower (a)
  (declare (type (simple-array (unsigned-byte 8) (4)) a))
  (aref (the (simple-array (unsigned-byte 8) (*)) a) 0))
(defun %tatc-decl-wider (a)
  (declare (type (array fixnum (*)) a))
  (aref (the (simple-array fixnum (*)) a) 0))
(defun %tatc-decl-assigned (a b)
  (declare (type (simple-array fixnum (*)) a))
  (setq a b)
  (aref (the (simple-array fixnum (*)) a) 0))

(deftest the-array-type-check.declared
  (list (%tatc-decl-same (make-array '(2 2) :element-type 'fixnum :initial-element 3) 2)
        (%tatc-decl-narrower (make-array 4 :element-type '(unsigned-byte 8) :initial-element 9))
        (%tatc-outcome #'%tatc-decl-wider (make-array 1 :element-type 'fixnum :adjustable t))
        (%tatc-outcome (lambda (b) (%tatc-decl-assigned (make-array 1 :element-type 'fixnum) b))
                       (vector 1)))
  (6 9 :type-error :type-error))

(deftest-compiled-only the-array-type-check.declared-sil
  (mapcar (lambda (f) (and (search "Runtime.CheckTheArray" (%flat-text (dotcl:function-sil f))) t))
          (list #'%tatc-decl-same #'%tatc-decl-narrower #'%tatc-decl-wider #'%tatc-decl-assigned))
  (nil nil t t))

;; THE passes on every value of its form.
(defun %tatc-two () (values (vector 1) 2))
(defun %tatc-mv-tail () (the simple-vector (%tatc-two)))
(defun %tatc-mv-if (x) (if x (the simple-vector (%tatc-two)) nil))

(deftest the-array-type-check.values
  (equalp (list (multiple-value-list (%tatc-mv-tail))
                (multiple-value-list (the simple-vector (%tatc-two)))
                (multiple-value-call #'list (the simple-vector (%tatc-two)) 3)
                (multiple-value-list (%tatc-mv-if t)))
          '((#(1) 2) (#(1) 2) (#(1) 2 3) (#(1) 2)))
  t)
