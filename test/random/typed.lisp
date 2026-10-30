;;;; Typed shapes for the random integer form generator.
;;;;
;;;; The shapes below reach the parts of the compiler that act on declarations:
;;;;   - integer arithmetic on variables declared (unsigned-byte 8/16/32/64),
;;;;     (signed-byte 32/64), fixnum, bit and (integer lo hi), with THE
;;;;   - double-float and single-float arithmetic on declared variables
;;;;   - specialized arrays of rank 1 to 3: MAKE-ARRAY with an element type,
;;;;     AREF / SETF AREF, ROW-MAJOR-AREF, declared array types, arrays passed
;;;;     to local functions that declare their parameter, fill-pointer vectors
;;;;     and SVREF
;;;;   - FLET / LABELS with typed parameters, &optional / &key, INLINE and
;;;;     NOTINLINE, closures, functions nobody calls, RETURN-FROM and GO out of
;;;;     a local function, and recursive LABELS entered through FUNCALL with a
;;;;     RETURN-FROM out of an enclosing BLOCK
;;;;   - multiple values: VALUES in the tail of local functions and blocks,
;;;;     MULTIPLE-VALUE-BIND with declared variables, MULTIPLE-VALUE-CALL
;;;;   - fixnum arithmetic next to the fixnum boundary (results overflow into
;;;     bignums), THE FIXNUM around sums that stay in range, and calls of
;;;     functions with a float parameter given an integer
;;;   - VALUES, NTH-VALUE, MULTIPLE-VALUE-CALL / -PROG1 / -BIND through BLOCK,
;;;     RETURN-FROM, LABELS, UNWIND-PROTECT, CATCH and HANDLER-CASE
;;;   - local OPTIMIZE declarations (speed / safety / debug)
;;;;
;;;; Every declaration is true by construction: a value bound to a typed
;;;; variable is first brought into the type's range (LDB or MOD), and a THE is
;;;; only put around an expression whose range is known (RF-INT-EXPR tracks an
;;;; interval for each expression). Forms that should signal an error (an index
;;;; out of range, a value outside a typed parameter's type, a store of the
;;;; wrong type into a specialized array) are generated only where every
;;;; enclosing safety is at least 1 (*RF-SAFE*), and are wrapped in HANDLER-CASE.
;;;; Typed LET variables and THE are not used for such errors: their checks are
;;;; up to the implementation (dotcl does not check them at safety 1).
;;;;
;;;; Loading this file wraps MAKE-RANDOM-INTEGER-FORM: with probability
;;;; cl-user::*rf-typed-rate* a form of size 4 or more is one of these shapes.

(in-package :cl-test)

(defvar cl-user::*rf-typed-rate* 0.2)

(defvar *rf-safe* t
  "True while every safety in effect, including the case's two OPTIMIZE
   declarations, is at least 1.")

(defvar *rf-yn* 0)

(defvar *rf-typed-next-make-random-integer-form*
  (fdefinition 'make-random-integer-form))

(defun rf-ysym (prefix)
  (intern (format nil "~A~D" prefix (incf *rf-yn*)) :cl-test))

(defun rf-yparts (size n)
  (mapcar #'make-random-integer-form (random-partition (max n (- size 2)) n)))

(defun rf-pick (&rest choices) (random-from-seq choices))

;;; Integer types

(defparameter *rf-int-types*
  `(((unsigned-byte 8) 0 255)
    ((unsigned-byte 16) 0 65535)
    ((unsigned-byte 32) 0 ,(1- (expt 2 32)))
    ((unsigned-byte 64) 0 ,(1- (expt 2 64)))
    ((signed-byte 8) -128 127)
    ((signed-byte 32) ,(- (expt 2 31)) ,(1- (expt 2 31)))
    ((signed-byte 64) ,(- (expt 2 63)) ,(1- (expt 2 63)))
    ;; Both implementations' fixnums hold (signed-byte 62).
    (fixnum ,(- (expt 2 61)) ,(1- (expt 2 61)))
    (bit 0 1)))

(defun rf-pick-int-type ()
  "A list (TYPE LO HI): the declared type and the range of values given to it."
  (if (coin 5)
      (let* ((lo (rf-pick 0 -1 (- (random 1000)) (- (random (expt 2 40)))))
             (hi (+ lo (random (rf-pick 2 100 70000 (expt 2 33))))))
        (list `(integer ,lo ,hi) lo hi))
      (let ((e (random-from-seq *rf-int-types*)))
        (if (and (eq (first e) 'fixnum) (coin 3))
            ;; A fixnum whose value is small: the fixnum lanes without overflow.
            (list 'fixnum -100000 100000)
            e))))

(defun rf-to-range (form lo hi)
  "FORM (an integer) brought into [LO, HI]."
  (let* ((n (- hi lo -1))
         (k (1- (integer-length n))))
    (cond ((and (= n (ash 1 k)) (= lo 0) (coin 2)) `(ldb (byte ,k 0) ,form))
          ((and (= n (ash 1 k)) (= lo (- (ash 1 (1- k)))) (> k 0) (coin 2))
           `(- (ldb (byte ,k 0) (+ ,form ,(ash 1 (1- k)))) ,(ash 1 (1- k))))
          ((= lo 0) `(mod ,form ,n))
          (t `(+ ,lo (mod ,form ,n))))))

(defun rf-covering-type (lo hi)
  "A type that contains every integer in [LO, HI]."
  (let ((named (find-if (lambda (e) (and (<= (second e) lo) (<= hi (third e))))
                        (if (coin 2) *rf-int-types* (reverse *rf-int-types*)))))
    (rcase
      (3 `(integer ,lo ,hi))
      (3 (if named (first named) `(integer ,lo ,hi)))
      (1 (if (coin 2) `(integer ,lo *) `(integer * ,hi)))
      (1 'integer))))

(defun rf-int-vars ()
  "The integer variables in scope with a known range, as (NAME LO HI)."
  (loop for d in *vars*
        for tp = (var-desc-type d)
        when (and (consp tp) (eq (first tp) 'integer)
                  (integerp (second tp)) (integerp (third tp)))
          collect (list (var-desc-name d) (second tp) (third tp))))

(defun rf-int-leaf (vars)
  (if (and vars (coin 4))
      (destructuring-bind (name lo hi) (random-from-seq vars)
        (values name lo hi))
      (let ((c (rcase (3 (- (random 200) 100)) (1 (random 65536))
                      (1 (- (random (expt 2 40)) (expt 2 39)))
                      (1 (rf-pick 0 1 -1 255 256 65535 (1- (expt 2 31)) (- (expt 2 31))
                                  (1- (expt 2 32)) (1- (expt 2 61)) (1- (expt 2 63))
                                  (1- (expt 2 64)))))))
        (values c c c))))

(defun rf-int-expr (vars depth)
  "A random integer expression over VARS ((NAME LO HI) ...), and its range."
  (if (or (<= depth 0) (coin 4))
      (rf-int-leaf vars)
      (multiple-value-bind (a alo ahi) (rf-int-expr vars (1- depth))
        (multiple-value-bind (b blo bhi) (rf-int-expr vars (1- depth))
          (flet ((corners (f)
                   (let ((l (list (funcall f alo blo) (funcall f alo bhi)
                                  (funcall f ahi blo) (funcall f ahi bhi))))
                     (values (reduce #'min l) (reduce #'max l)))))
            (rcase
              (3 (values `(+ ,a ,b) (+ alo blo) (+ ahi bhi)))
              (2 (values `(- ,a ,b) (- alo bhi) (- ahi blo)))
              (2 (multiple-value-bind (lo hi) (corners #'*) (values `(* ,a ,b) lo hi)))
              (1 (values `(- ,a) (- ahi) (- alo)))
              (1 (values `(1+ ,a) (1+ alo) (1+ ahi)))
              (1 (values `(lognot ,a) (- -1 ahi) (- -1 alo)))
              (1 (if (and (>= alo 0) (>= blo 0))
                     (values `(logand ,a ,b) 0 (min ahi bhi))
                     (values `(logand ,a 255) 0 255)))
              (1 (if (and (>= alo 0) (>= blo 0))
                     (values `(,(rf-pick 'logior 'logxor) ,a ,b)
                             0 (1- (ash 1 (max (integer-length ahi) (integer-length bhi)))))
                     (values `(logand ,a ,(1- (expt 2 32))) 0 (1- (expt 2 32)))))
              (1 (let ((m (rf-pick 255 65535 (1- (expt 2 32)) (1- (expt 2 62)) (1- (expt 2 64)))))
                   (values `(logand (,(rf-pick '+ '* '-) ,a ,b) ,m) 0 m)))
              (1 (let ((s (rf-pick 1 8 16 32 61 64)) (p (rf-pick 0 0 3 8)))
                   (values `(ldb (byte ,s ,p) ,a) 0 (1- (ash 1 s)))))
              (1 (let ((k (- (random 9) 4)))
                   (values `(ash ,a ,k) (ash alo k) (ash ahi k))))
              (1 (let ((k (1+ (random 9))) (op (rf-pick 'floor 'truncate 'ceiling)))
                   (values `(values (,op ,a ,k)) (funcall op alo k) (funcall op ahi k))))
              (1 (let ((k (1+ (random 50))))
                   (values `(mod ,a ,k) 0 (1- k))))
              (1 (let ((k (1+ (random 50))))
                   (values `(rem ,a ,k) (if (>= alo 0) 0 (- 1 k)) (if (<= ahi 0) 0 (1- k)))))
              (1 (values `(min ,a ,b) (min alo blo) (min ahi bhi)))
              (1 (values `(max ,a ,b) (max alo blo) (max ahi bhi)))
              (1 (values `(abs ,a)
                         (cond ((>= alo 0) alo) ((<= ahi 0) (- ahi)) (t 0))
                         (max (abs alo) (abs ahi))))
              (1 (values `(if (,(rf-pick '< '<= '= '> '/=) ,a ,b) ,a ,b)
                         (min alo blo) (max ahi bhi)))
              (1 (values `(integer-length ,a) 0
                         (max (integer-length alo) (integer-length ahi))))
              (1 (values `(logcount ,a) 0
                         (max (integer-length alo) (integer-length ahi))))
              (2 (values `(the ,(rf-covering-type alo ahi) ,a) alo ahi))
              (1 (let ((v (rf-ysym "YI")))
                   (multiple-value-bind (body lo hi)
                       (rf-int-expr (cons (list v alo ahi) vars) (1- depth))
                     (values `(let ((,v ,a)) (declare (type ,(rf-covering-type alo ahi) ,v)) ,body)
                             lo hi))))))))))

;;; Local OPTIMIZE

(defun rf-optimize-decl ()
  (let ((safety (random 4)))
    (values `(optimize (speed ,(random 4)) (safety ,safety) (debug ,(random 4))
                       ,@(when (coin 3) `((space ,(random 4)))))
            safety)))

(defmacro rf-with-local-optimize ((decl-var) &body body)
  "Bind DECL-VAR to a random OPTIMIZE declaration (or NIL) and run BODY with
   *RF-SAFE* false if it says safety 0."
  (let ((safety (gensym)))
    `(multiple-value-bind (,decl-var ,safety)
         (if (coin 3) (rf-optimize-decl) (values nil 3))
       (let ((*rf-safe* (and *rf-safe* (> ,safety 0))))
         ,@body))))

(defun rf-locally-form (size)
  (rf-with-local-optimize (decl)
    (let ((body (make-random-integer-form (1- size))))
      (if decl `(locally (declare ,decl) ,body) body))))

;;; Typed integer variables

(defun rf-typed-let-form (size)
  (rf-with-local-optimize (opt)
    (let* ((n (1+ (random 3)))
           (inits (rf-yparts size n))
           (specs (loop repeat n collect (rf-pick-int-type)))
           (names (loop repeat n collect (rf-ysym "YV")))
           (bindings (loop for name in names for (nil lo hi) in specs for init in inits
                           collect `(,name ,(rf-to-range init lo hi))))
           (decls (loop for name in names for (tp) in specs
                        collect `(type ,tp ,name)))
           (ivars (loop for name in names for (nil lo hi) in specs
                        collect (list name lo hi)))
           (*vars* (append (loop for (name lo hi) in ivars
                                 collect (make-var-desc :name name :type `(integer ,lo ,hi)))
                           *vars*))
           (body
             (rcase
               (3 (rf-int-expr (rf-int-vars) (+ 2 (random 3))))
               (2 (make-random-integer-form (max 1 (floor size 2))))
               (2 `(+ ,(make-random-integer-form (max 1 (floor size 3)))
                      ,(rf-int-expr ivars 3)))
               (2 (rf-typed-assign-body ivars specs size))
               (2 (rf-typed-loop-body ivars)))))
      `(,(rf-pick 'let 'let*) ,bindings
        (declare ,@decls ,@(when opt (list opt)))
        ,body))))

(defun rf-typed-assign-body (ivars specs size)
  "Assign each typed variable a value in its range, then combine them."
  (let ((setqs (loop for (name lo hi) in ivars
                     for (tp) in specs
                     collect (let ((v (make-random-integer-form (max 1 (floor size 4)))))
                               (rcase
                                 (2 `(setq ,name ,(rf-to-range v lo hi)))
                                 (1 `(setf ,name (the ,tp ,(rf-to-range v lo hi))))
                                 (1 (multiple-value-bind (e elo ehi) (rf-int-expr ivars 2)
                                      (declare (ignore elo ehi))
                                      `(setq ,name ,(rf-to-range e lo hi)))))))))
    `(progn ,@setqs ,(rf-int-expr ivars 2))))

(defun rf-typed-loop-body (ivars)
  "A loop accumulating a typed expression into a declared accumulator."
  (let* ((i (rf-ysym "YI"))
         (acc (rf-ysym "YACC"))
         (n (1+ (random 6))))
    (multiple-value-bind (e elo ehi) (rf-int-expr (cons (list i 0 (1- n)) ivars) 3)
      (let ((alo (min 0 (* n elo))) (ahi (max 0 (* n ehi))))
        (rcase
          (2 `(let ((,acc 0))
                (declare (type ,(rf-covering-type alo ahi) ,acc))
                (dotimes (,i ,n) (declare (type (integer 0 ,n) ,i))
                  (setq ,acc (+ ,acc ,e)))
                ,acc))
          (1 `(loop for ,i of-type (integer 0 ,n) from 0 below ,n
                    sum (the ,(rf-covering-type elo ehi) ,e)))
          (1 `(let ((,acc 0))
                (declare (type (unsigned-byte 64) ,acc))
                (dotimes (,i ,n)
                  (setq ,acc (logand (+ (* ,acc 31) (logand ,e ,(1- (expt 2 64))))
                                     ,(1- (expt 2 64)))))
                ,acc)))))))

(defun rf-typed-mv-form (size)
  "MULTIPLE-VALUE-BIND of FLOOR and friends on typed operands, with the
   quotient and remainder declared."
  (destructuring-bind (x y) (rf-yparts size 2)
    (destructuring-bind (tp lo hi) (rf-pick-int-type)
      (let* ((k (rf-pick 1 2 3 7 10 255 256 (expt 2 32)))
             (op (rf-pick 'floor 'truncate 'ceiling 'round))
             (a (rf-ysym "YA")) (q (rf-ysym "YQ")) (r (rf-ysym "YR"))
             (qs (list (funcall op lo k) (funcall op hi k)))
             (qlo (reduce #'min qs)) (qhi (reduce #'max qs)))
        `(let ((,a ,(rf-to-range x lo hi)))
           (declare (type ,tp ,a))
           (multiple-value-bind (,q ,r) (,op ,a ,k)
             (declare (type ,(rf-covering-type (1- qlo) (1+ qhi)) ,q)
                      (type (integer ,(- k) ,k) ,r))
             (+ ,q (* ,r ,(rf-pick 1 3 -1)) ,(if (coin 2) y 0))))))))

;;; Floats

(defun rf-clamp-int (form bits)
  `(max ,(- (expt 2 bits)) (min ,(expt 2 bits) ,form)))

(defun rf-proto (fmt) (if (eq fmt 'double-float) 1d0 1f0))

(defun rf-float-expr (fvars fmt depth)
  "A random float expression over FVARS ((NAME FORMAT) ...) of format FMT
   (DOUBLE-FLOAT or SINGLE-FLOAT), a bound on its absolute value, and its
   format when it is known (a SINGLE-FLOAT variable may appear in a
   DOUBLE-FLOAT expression, and MAX of the two returns either)."
  (let ((max-bound (if (eq fmt 'double-float) (expt 2 200) (expt 2 100)))
        (proto (rf-proto fmt)))
    (flet ((leaf ()
             (let ((usable (remove-if-not (lambda (v) (or (eq (second v) fmt)
                                                          (eq fmt 'double-float)))
                                          fvars)))
               (if (and usable (coin 3))
                   (let ((v (random-from-seq usable)))
                     (values (first v) (expt 2 40) (second v)))
                   (let ((c (rcase (3 (/ (- (random 2000) 1000) 8))
                                   (1 (rf-pick 1/10 1/3 -7/10 0 1/2 1000001/7)))))
                     (values (float c proto) (1+ (abs c)) fmt)))))
           (arith (af bf)
             (cond ((or (eq af 'double-float) (eq bf 'double-float)) 'double-float)
                   ((and (eq af 'single-float) (eq bf 'single-float)) 'single-float)
                   (t nil)))
           (same (af bf) (and (eq af bf) af)))
      (if (or (<= depth 0) (coin 4))
          (leaf)
          (multiple-value-bind (a ab af) (rf-float-expr fvars fmt (1- depth))
            (multiple-value-bind (b bb bf) (rf-float-expr fvars fmt (1- depth))
              (rcase
                (3 (values `(,(rf-pick '+ '-) ,a ,b) (+ ab bb 1) (arith af bf)))
                (2 (if (< (* ab bb) max-bound)
                       (values `(* ,a ,b) (+ 1 (* ab bb)) (arith af bf))
                       (values `(+ ,a ,b) (+ ab bb 1) (arith af bf))))
                (1 (values `(/ ,a ,(float (rf-pick 2 4 -8 1/2) proto)) (* 2 ab)
                           (arith af fmt)))
                (1 (values `(- ,a) ab af))
                (1 (values `(abs ,a) ab af))
                (1 (values `(,(rf-pick 'max 'min) ,a ,b) (max ab bb) (same af bf)))
                (1 (values `(sqrt (abs ,a)) (1+ (isqrt (ceiling ab))) af))
                (1 (values `(,(rf-pick 'ffloor 'fceiling 'ftruncate 'fround) ,a) (1+ ab) af))
                (1 (values `(the ,(or af 'float) ,a) ab af))
                (1 (values `(if (,(rf-pick '< '<= '= '>) ,a ,b) ,a ,b) (max ab bb) (same af bf)))
                (1 (values `(float ,(rf-clamp-int (rf-int-expr (rf-int-vars) 2) 40) ,proto)
                           (1+ (expt 2 40)) fmt)))))))))

(defun rf-float-to-int (form)
  ;; One value: the float remainder would reach a MULTIPLE-VALUE-CALL of #'+.
  (rcase
    (3 `(values (,(rf-pick 'floor 'ceiling 'truncate 'round) ,form)))
    (1 `(values (floor ,form ,(rf-pick 1 2 3))))
    (1 `(if (minusp ,form) 1 0))))

(defun rf-typed-float-form (size)
  (rf-with-local-optimize (opt)
    (let* ((n (1+ (random 3)))
           (inits (rf-yparts size n))
           (vars (loop for init in inits
                       collect (list (rf-ysym "YF") (rf-pick 'double-float 'single-float) init)))
           (fmt (rf-pick 'double-float 'single-float))
           (proto (rf-proto fmt))
           (body (rcase
                   (4 (rf-float-to-int (rf-float-expr vars fmt (+ 2 (random 3)))))
                   (1 `(if (,(rf-pick '< '<= '= '>)
                            ,(rf-float-expr vars fmt 2) ,(rf-float-expr vars fmt 2))
                           1 0))
                   (1 (let ((acc (rf-ysym "YACC")) (i (rf-ysym "YI")))
                        `(let ((,acc ,(float 0 proto)))
                           (declare (type ,fmt ,acc))
                           (dotimes (,i ,(1+ (random 5)))
                             (setq ,acc (+ ,acc ,(rf-float-expr vars fmt 2))))
                           ,(rf-float-to-int acc))))
                   (1 (let ((v (rf-ysym "YF")))
                        `(let ((,v (float ,(rf-float-expr vars fmt 2) ,proto)))
                           (declare (type ,fmt ,v))
                           (setq ,v (* ,v ,(float 1/2 proto)))
                           ,(rf-float-to-int v)))))))
      `(let ,(loop for (name ftype init) in vars
                   collect `(,name (float ,(rf-clamp-int init 40) ,(rf-proto ftype))))
         (declare ,@(loop for (name ftype) in vars collect `(type ,ftype ,name))
                  ,@(when opt (list opt)))
         ,body))))

;;; Arrays

(defparameter *rf-elt-types*
  '((unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 64)
    (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64)
    fixnum bit t double-float single-float))

(defun rf-elt-range (et)
  (let ((e (find et *rf-int-types* :key #'first :test #'equal)))
    (cond (e (values (second e) (third e)))
          ((equal et '(signed-byte 16)) (values -32768 32767))
          (t nil))))

(defun rf-elt-value (et form)
  "FORM (an integer) made into a value of element type ET."
  (multiple-value-bind (lo hi) (rf-elt-range et)
    (cond (lo (rf-to-range form lo hi))
          ((member et '(double-float single-float))
           `(float ,(rf-clamp-int form 40) ,(rf-proto et)))
          (t form))))

(defun rf-elt-int (et form)
  "An element of type ET made into an integer."
  (if (member et '(double-float single-float))
      `(values (,(rf-pick 'floor 'round 'truncate) ,form))
      form))

(defun rf-index-form (n)
  "A form whose value is an index below N."
  (rcase
    (2 (random n))
    (3 `(mod ,(make-random-integer-form (rf-pick 1 2 4)) ,n))
    (1 `(min ,(1- n) (max 0 ,(make-random-integer-form 2))))))

(defun rf-array-decl-type (et dims)
  (let ((ds (loop for d in dims collect (if (coin 3) '* d))))
    (rcase
      (4 `(simple-array ,et ,ds))
      (1 `(array ,et ,ds))
      (1 (if (= (length dims) 1) `(simple-array ,et (*)) `(simple-array ,et ,(length dims))))
      (1 (if (= (length dims) 1) `(vector ,et) `(array ,et ,(length dims)))))))

(defun rf-typed-array-form (size)
  (rf-with-local-optimize (opt)
    (let* ((et (random-from-seq *rf-elt-types*))
           (rank (rcase (4 1) (2 2) (1 3)))
           (dims (loop repeat rank collect (1+ (random 4))))
           (total (reduce #'* dims))
           (v (rf-ysym "YARR")))
      (destructuring-bind (x y z) (rf-yparts size 3)
        (let* ((make
                 (if (and (= rank 1) (coin 4))
                     `(make-array ,(first dims) :element-type ',et
                                  :initial-contents
                                  (list ,@(loop repeat (first dims)
                                                collect (rf-elt-value et (- (random 2000) 1000)))))
                     `(make-array ',dims :element-type ',et
                                         :initial-element ,(rf-elt-value et x))))
                 (stores
                   (loop repeat (random 3)
                         collect (rcase
                                   (3 `(setf (aref ,v ,@(loop for d in dims collect (rf-index-form d)))
                                             ,(rf-elt-value et y)))
                                   (1 `(setf (row-major-aref ,v ,(rf-index-form total))
                                             ,(rf-elt-value et z)))
                                   (1 (let ((i (rf-ysym "YI")))
                                        `(dotimes (,i ,total)
                                           (setf (row-major-aref ,v ,i)
                                                 ,(rf-elt-value et `(+ ,i ,y))))))
                                   (1 (let ((is (loop repeat rank collect (rf-ysym "YI"))))
                                        `(dotimes (,(first is) ,(first dims))
                                           ,(rf-nested-store v et is dims
                                                             (make-random-integer-form 2))))))))
                 (result (rf-array-result et dims v)))
          `(let ((,v ,make))
             (declare (type ,(rf-array-decl-type et dims) ,v)
                      ,@(when opt (list opt)))
             ,@stores
             ,(if (and *rf-safe* (coin 6))
                  `(+ ,result ,(rf-array-error-form et dims v))
                  result)))))))

(defun rf-fill-pointer-form (size)
  (rf-with-local-optimize (opt)
    (let* ((et (rf-pick '(unsigned-byte 8) 'fixnum 't 'double-float 'single-float))
           (cap (1+ (random 5)))
           (n (1+ (random 4)))
           (v (rf-ysym "YV"))
           (i (rf-ysym "YI"))
           (x (make-random-integer-form (max 1 (floor size 2)))))
      `(let ((,v (make-array ,cap :element-type ',et :adjustable t :fill-pointer 0)))
         (declare (type (vector ,et) ,v)
                  ,@(when opt (list opt)))
         (dotimes (,i ,n)
           (vector-push-extend ,(rf-elt-value et `(+ ,i ,x)) ,v))
         (+ (fill-pointer ,v) (length ,v)
            ,(rf-elt-int et `(aref ,v (mod ,x (fill-pointer ,v))))
            ,(rf-elt-int et `(aref ,v ,(random n))))))))

(defun rf-svref-form (size)
  (rf-with-local-optimize (opt)
    (let* ((n (1+ (random 5)))
           (v (rf-ysym "YV"))
           (x (make-random-integer-form (max 1 (floor size 2))))
           (y (make-random-integer-form (max 1 (floor size 2)))))
      `(let ((,v (make-array ,n :initial-element ,x)))
         (declare (type simple-vector ,v)
                  ,@(when opt (list opt)))
         (setf (svref ,v ,(random n)) ,y)
         (+ (length ,v) (svref ,v (mod ,x ,n)) (svref ,v ,(random n)))))))

(defun rf-recursive-local-form (size)
  (rf-with-local-optimize (opt)
    (destructuring-bind (x y z) (rf-yparts size 3)
      (let* ((blk (rf-ysym "YB"))
             (g (rf-ysym "YFN"))
             (rec (rf-ysym "YFNR"))
             (nn (rf-ysym "YN"))
             (aa (rf-ysym "YACC"))
             (limit (random 60)))
        `(block ,blk
           (labels ((,g (,nn ,aa)
                      (declare (type (integer 0 100) ,nn)
                               (type fixnum ,aa)
                               ,@(when opt (list opt)))
                      (if (<= ,nn 0)
                          (values ,aa ,nn)
                          (progn
                            (when (and (> ,nn ,limit) (> ,x ,y))
                              (return-from ,blk (+ ,aa ,z)))
                            (funcall #',g (1- ,nn)
                                     (logand (+ ,aa 31 (mod ,z 1000))
                                             2305843009213693951)))))
                    (,rec (,nn)
                      (declare (type (integer 0 100) ,nn))
                      (if (<= ,nn 0)
                          ,(make-random-integer-form (max 1 (floor size 3)))
                          (funcall #',rec (1- ,nn)))))
             (multiple-value-bind (,aa ,nn)
                 (funcall #',g (mod ,x 80) (mod ,y 5))
               (+ ,aa (or ,nn 0) (funcall #',rec (mod ,z 20))))))))))

(defun rf-nested-store (v et is dims w)
  "The body of the outermost DOTIMES over IS: stores into every element."
  (labels ((nest (rest-is rest-dims)
             (if rest-is
                 `(dotimes (,(car rest-is) ,(car rest-dims))
                    ,(nest (cdr rest-is) (cdr rest-dims)))
                 `(setf (aref ,v ,@is) ,(rf-elt-value et `(+ (* ,(first is) ,w) ,@(rest is)))))))
    (nest (rest is) (rest dims))))

(defun rf-array-sum-form (et total a)
  (let ((s (rf-ysym "YS")) (i (rf-ysym "YI")))
    `(let ((,s 0))
       (dotimes (,i ,total) (setq ,s (+ ,s ,(rf-elt-int et `(row-major-aref ,a ,i)))))
       ,s)))

(defun rf-array-result (et dims v)
  (let ((total (reduce #'* dims))
        (rank (length dims)))
    (rcase
      (3 (rf-elt-int et `(aref ,v ,@(loop for d in dims collect (rf-index-form d)))))
      (2 (rf-array-sum-form et total v))
      (1 `(+ (array-total-size ,v) (array-dimension ,v ,(random rank)) (array-rank ,v)))
      (2 (let ((g (rf-ysym "YFN")) (a (rf-ysym "YP")) (inline (rf-pick nil 'inline 'notinline)))
           `(flet ((,g (,a) (declare (type ,(rf-array-decl-type et dims) ,a))
                     ,(rf-array-sum-form et total a)))
              ,@(when inline `((declare (,inline ,g))))
              (,g ,v))))
      (2 (let ((a (rf-ysym "YP")) (is (loop repeat rank collect (rf-ysym "YJ"))))
           `(funcall (lambda (,a ,@is)
                       (declare (type ,(rf-array-decl-type et dims) ,a)
                                ,@(loop for i in is for d in dims
                                        collect `(type (integer 0 (,d)) ,i)))
                       ,(rf-elt-int et `(aref ,a ,@is)))
                     ,v ,@(loop for d in dims collect (rf-index-form d)))))
      (1 (if (= rank 1)
             (rf-elt-int et `(reduce #'max ,v))
             (rf-elt-int et `(row-major-aref ,v ,(rf-index-form total))))))))

(defun rf-array-error-form (et dims v)
  "A form signalling an error at safety 1 or more, caught: a subscript out of
   range or a value of the wrong type stored."
  (let ((rank (length dims)) (total (reduce #'* dims)))
    `(handler-case
         ,(rcase
            (2 (let ((k (random rank)))
                 (rf-elt-int et `(aref ,v ,@(loop for d in dims for j from 0
                                                  collect (if (= j k)
                                                              (rf-pick d -1 `(+ ,d ,(random 3)))
                                                              (random d)))))))
            (1 (rf-elt-int et `(row-major-aref ,v ,(rf-pick total -1 `(+ ,total ,(random 3))))))
            (1 (let ((bad (cond ((member et '(double-float single-float)) 1)
                                ((eq et t) nil)
                                (t (multiple-value-bind (lo hi) (rf-elt-range et)
                                     (rf-pick (1+ hi) (1- lo) 1.5d0))))))
                 (if bad
                     `(progn (setf (aref ,v ,@(loop for d in dims collect (random d))) ,bad) 0)
                     `(progn (aref ,v ,@dims) 0)))))
       (error () -1))))

;;; Local functions

(defun rf-bad-value (tp lo hi)
  "A value outside TP. FIXNUM's range is not the same in every implementation
   (dotcl's holds (signed-byte 64), SBCL's (signed-byte 62)), so for FIXNUM
   only a value outside both."
  (if (eq tp 'fixnum)
      (rf-pick (expt 2 64) (- (expt 2 64)) 1/2)
      (rf-pick (1+ hi) (1- lo) (expt 2 64) 1/2)))

(defun rf-typed-param (size)
  "A typed parameter: (NAME TYPE LO HI ARG-FORM)."
  (destructuring-bind (tp lo hi) (rf-pick-int-type)
    (list (rf-ysym "YP") tp lo hi (rf-to-range (make-random-integer-form size) lo hi))))

(defun rf-make-local-fn (fsize exit blk tag)
  "One local function: (NAME REQ OPT KEY KEYP DECLS BODY BAD-CALL-P)."
  (rf-with-local-optimize (o)
    (let* ((name (rf-ysym "YFN"))
           (typed (coin 2))
           (req (loop repeat (random 3)
                      collect (if typed
                                  (rf-typed-param 2)
                                  (list (rf-ysym "YP") nil nil nil (make-random-integer-form 2)))))
           (opt (when (coin 3) (rf-typed-param 2)))
           (key (when (coin 3) (rf-typed-param 2)))
           (keyp (when key (rf-ysym "YSP")))
           (params (append req (when opt (list opt)) (when key (list key))))
           (bad (and *rf-safe* typed req (coin 4)))
           (body (let ((*vars* (append (loop for (p tp lo hi) in params
                                             collect (make-var-desc
                                                      :name p
                                                      :type (if tp `(integer ,lo ,hi) '(integer * *))))
                                       *vars*))
                       (*random-int-form-blocks* (cons name *random-int-form-blocks*)))
                   (rcase
                     (3 (make-random-integer-form fsize))
                     (2 (rf-int-expr (rf-int-vars) 3))
                     (1 (if (eq exit :tagbody)
                            `(progn (go ,tag) ,(make-random-integer-form 1))
                            `(return-from ,name ,(make-random-integer-form fsize))))
                     (1 (if (eq exit :block)
                            `(if (> ,(make-random-integer-form 2) 0)
                                 (return-from ,blk ,(make-random-integer-form 2))
                                 ,(make-random-integer-form fsize))
                            (make-random-integer-form fsize)))
                     (2 `(values ,(make-random-integer-form fsize) ,(make-random-integer-form 1)))))))
      (when bad
        ;; Whether the required parameter's type is checked before a later
        ;; initform runs is implementation-dependent, so those initforms stay
        ;; plain values.
        (dolist (p (list opt key))
          (when p (setf (fifth p) (third p))))
        ;; The call passes a value outside the last required parameter's type.
        (let ((p (car (last req))))
          (setf (fifth p) (rf-bad-value (second p) (third p) (fourth p)))))
      (list name req opt key keyp
            (append (loop for (p tp) in params when tp collect `(type ,tp ,p))
                    (when keyp `((ignorable ,keyp)))
                    (when o (list o)))
            body bad))))

(defun rf-lambda-list (req opt key keyp)
  `(,@(mapcar #'first req)
    ,@(when opt `(&optional (,(first opt) ,(fifth opt))))
    ,@(when key `(&key ((,(intern (symbol-name (first key)) :keyword) ,(first key))
                        ,(fifth key) ,keyp)))))

(defun rf-local-call (name req opt key)
  (let* ((args (mapcar #'fifth req))
        (optargs (when (and opt (coin 2)) (list (fifth opt))))
        (keyargs (when (and key (or (null opt) optargs) (coin 2))
                   (list (intern (symbol-name (first key)) :keyword) (fifth key)))))
    (rcase
      (3 `(,name ,@args ,@optargs ,@keyargs))
      (1 `(funcall #',name ,@args ,@optargs ,@keyargs))
      (1 `(apply #',name (list ,@args ,@optargs ,@keyargs)))
      (1 `(multiple-value-call #',name ,@(mapcar (lambda (a) `(values ,a))
                                                 (append args optargs keyargs)))))))

(defun rf-local-fn-form (size)
  "FLET / LABELS of functions with typed and untyped parameters, &optional and
   &key, an INLINE or NOTINLINE declaration, closures over the enclosing
   variables, and RETURN-FROM / GO out of a function, possibly one nobody
   calls. Functions with untyped required parameters only are also offered to
   the generator (*FLET-NAMES*), so that generated forms call them."
  (let* ((op (rf-pick 'flet 'labels))
         (blk (rf-ysym "YB"))
         (tag (rf-ysym "YT"))
         (exit (rf-pick :block :tagbody nil))
         (nfns (1+ (random 3)))
         (fsize (max 2 (floor size (+ nfns 2))))
         (fns (let ((*random-int-form-blocks*
                      (if (eq exit :block) (cons blk *random-int-form-blocks*)
                          *random-int-form-blocks*)))
                (loop repeat nfns collect (rf-make-local-fn fsize exit blk tag)))))
    (let* ((*flet-names* (append (loop for (name req opt key nil nil nil nil) in fns
                                       unless (or opt key (some #'second req))
                                         collect (list name (length req) (length req) nil))
                                 *flet-names*))
           (*random-int-form-blocks* (if (eq exit :block) (cons blk *random-int-form-blocks*)
                                         *random-int-form-blocks*))
           (defs (loop for (name req opt key keyp decls body) in fns
                       collect `(,name ,(rf-lambda-list req opt key keyp)
                                       ,@(when decls `((declare ,@decls)))
                                       ,body)))
           (calls (loop for (name req opt key nil nil nil bad) in fns
                        when (or bad (not (coin 4)))   ; else nobody calls it
                          collect (rf-local-call name req opt key)))
           (fdecls (loop for (name) in fns
                         for d = (rcase (2 nil) (1 'inline) (1 'notinline))
                         when d collect `(,d ,name)))
           (use (rcase
                  (3 `(+ ,@calls ,(make-random-integer-form 2)))
                  (1 (if calls `(multiple-value-call #'+ ,@calls) 0))
                  (1 (if calls
                         (let ((a (rf-ysym "YM")) (b (rf-ysym "YM")))
                           `(multiple-value-bind (,a ,b) ,(first calls)
                              (+ ,a (if ,b 1 0) ,@(rest calls))))
                         0))
                  (1 `(length (list ,@calls)))))
           (use (if (some #'eighth fns) `(handler-case ,use (error () -1)) use))
           (form `(,op ,defs ,@(when fdecls `((declare ,@fdecls))) ,use)))
      (case exit
        (:block `(block ,blk ,form))
        (:tagbody (let ((r (rf-ysym "YR")))
                    `(let ((,r 0))
                       (tagbody (setq ,r ,form) ,tag)
                       ,r)))
        (t form)))))

;;; Typed parameter errors

(defun rf-typed-arg-error-form (size)
  "A call with an argument outside its parameter's type, caught. Only where
   every safety in effect is at least 1."
  (if (not *rf-safe*)
      (make-random-integer-form size)
      (destructuring-bind (x) (rf-yparts size 1)
        (destructuring-bind (tp lo hi) (loop for e = (rf-pick-int-type)
                                             unless (eq (first e) 'fixnum) return e)
          (let ((p (rf-ysym "YP")) (g (rf-ysym "YFN")))
            `(handler-case
                 ,(rcase
                    (2 `(flet ((,g (,p) (declare (type ,tp ,p)) (+ ,p 1)))
                          ,@(rcase (1 nil) (1 `((declare (inline ,g)))) (1 `((declare (notinline ,g)))))
                          (,g ,x)))
                    (1 `(funcall (lambda (,p) (declare (type ,tp ,p)) (+ ,p 1)) ,x))
                    (1 `(labels ((,g (,p) (declare (type ,tp ,p)) (+ ,p 1))) (,g ,x)))
                    (1 (let ((et (rf-pick '(unsigned-byte 8) 'double-float 'fixnum)))
                         `(flet ((,g (,p) (declare (type (simple-array ,et (*)) ,p)) (length ,p)))
                            (,g (if (> ,x 0) (vector 1 2) (list 1 2)))))))
               (error () ,(if (coin 2) -1 (+ lo hi 7)))))))))

;;; Multiple values

(defun rf-mv2-form (size)
  (destructuring-bind (x y z) (rf-yparts size 3)
    (let ((a (rf-ysym "YM")) (b (rf-ysym "YM")) (c (rf-ysym "YM"))
          (g (rf-ysym "YFN")) (blk (rf-ysym "YB")))
      (rcase
        (2 `(flet ((,g (,a) (if (> ,a 0) (values ,a ,y) (values ,z ,a 1))))
              ,@(when (coin 2) `((declare (,(rf-pick 'inline 'notinline) ,g))))
              (multiple-value-bind (,a ,b ,c) (,g ,x) (+ ,a ,b (or ,c 0)))))
        (1 `(multiple-value-bind (,a ,b)
                (block ,blk
                  (when (> ,x ,y) (return-from ,blk (values ,y ,z)))
                  (values ,z ,x))
              (- ,a ,b)))
        (1 `(multiple-value-call #'+ (values ,x ,y) (values) (values ,z)))
        (1 `(nth-value ,(random 3) (values ,x ,y ,z)))
        (1 `(let ((,a 0))
              (+ (multiple-value-prog1 (values ,x (setq ,a ,y)) (setq ,a (+ ,a 1))) ,a)))
        (1 `(length (multiple-value-list (if (> ,x 0) (values ,y ,z) (values)))))
        (1 `(multiple-value-bind (,a ,b) (catch ',blk (throw ',blk (values ,x ,y)))
              (+ ,a ,b)))
        (1 (multiple-value-bind (e lo hi) (rf-int-expr (rf-int-vars) 3)
             `(multiple-value-bind (,a ,b) (values ,e ,x)
                (declare (type ,(rf-covering-type lo hi) ,a))
                (+ ,a ,b))))
        (1 (destructuring-bind (tp lo hi) (rf-pick-int-type)
             `(flet ((,g (,a) (declare (type ,tp ,a))
                       (values (the ,tp ,a) (1+ ,a))))
                (multiple-value-call #'+ (,g ,(rf-to-range x lo hi))
                  (,g ,(rf-to-range y lo hi))))))))))

;;; Fixnum boundary

;;; The range of SBCL's fixnum; dotcl's holds it, so a value in this range is a
;;; valid FIXNUM in both.
(defparameter *rf-fix-lo* (- (expt 2 62)))
(defparameter *rf-fix-hi* (1- (expt 2 62)))

(defun rf-fixnum-edge-value (form lo hi)
  "FORM (an integer) moved next to a boundary of [LO, HI], clamped into it."
  (let ((edge (rf-pick hi lo (- hi 1000) (+ lo 1000) (ash hi -1) (ash lo -1))))
    `(max ,lo (min ,hi (+ ,edge (- (mod ,form 7) 3))))))

(defun rf-fixnum-edge-form (size)
  "Arithmetic on FIXNUM variables next to the fixnum boundary, whose results
   overflow into bignums, THE FIXNUM around sums that stay in range, and
   accumulation loops that cross the boundary."
  (destructuring-bind (p q) (rf-yparts size 2)
    (let* ((x (rf-ysym "YX")) (y (rf-ysym "YX"))
           (h1 (rf-ysym "YH")) (h2 (rf-ysym "YH"))
           (i (rf-ysym "YI")) (acc (rf-ysym "YACC"))
           (half (1- (expt 2 61)))
           (opt (when (coin 2)
                  `(optimize (speed ,(rf-pick 0 1 3)) (safety ,(rf-pick 0 1 3)))))
           (k (rf-pick 2 3 -1 7 255))
           (wide (list `(+ ,x ,y) `(- ,x ,y) `(* ,x ,y) `(* ,x ,k) `(1+ ,x) `(1- ,y)
                       `(- ,x) `(abs ,x) `(ash ,x ,(rf-pick 1 2 -1 -3)) `(logxor ,x ,y)
                       `(floor ,x 3) `(integer-length (* ,x ,y))
                       `(if (> (+ ,x ,y) ,*rf-fix-hi*) 1 0)
                       `(if (< (- ,x ,y) ,*rf-fix-lo*) 1 0)
                       `(max (+ ,x 1) (- ,y 1))))
           (narrow (list `(the fixnum (+ ,h1 ,h2)) `(the fixnum (- ,h1 ,h2))
                         `(the fixnum (ash ,h1 1)) `(the (signed-byte 63) (* ,h1 2))
                         `(the fixnum (- ,h1)) `(the fixnum (logxor ,h1 ,h2))))
           (n (1+ (random 80)))
           (body
             (rcase
               (4 `(+ ,(random-from-seq wide) (* 3 ,(random-from-seq wide))
                      ,(random-from-seq wide)))
               (2 `(+ ,(random-from-seq narrow) ,(random-from-seq wide)))
               (1 `(let ((,acc ,x))
                     (declare (type integer ,acc))
                     (dotimes (,i ,n) (setq ,acc (+ ,acc (ash ,acc -1) ,y)))
                     ,acc))
               (1 `(let ((,acc ,x))
                     (declare (type integer ,acc))
                     (dotimes (,i ,n)
                       (setq ,acc (logand (+ (* ,acc 3) ,y) ,(1- (expt 2 100)))))
                     ,acc))
               (1 `(let ((,acc ,h1))
                     (declare (type fixnum ,acc))
                     (dotimes (,i ,(random 3))
                       (setq ,acc (the fixnum (ash ,acc 0)))
                       (setq ,acc (ash (if (> ,acc 0) (- ,acc ,h2) (+ ,acc ,h2)) -1)))
                     (+ ,acc ,(random-from-seq wide)))))))
      `(let ((,x ,(rf-fixnum-edge-value p *rf-fix-lo* *rf-fix-hi*))
             (,y ,(rf-fixnum-edge-value q *rf-fix-lo* *rf-fix-hi*))
             (,h1 ,(rf-fixnum-edge-value q (- half) half))
             (,h2 ,(rf-fixnum-edge-value p (- half) half)))
         (declare (type fixnum ,x ,y)
                  (type (integer ,(- half) ,half) ,h1 ,h2)
                  (ignorable ,x ,y ,h1 ,h2))
         ,(if opt `(locally (declare ,opt) ,body) body)))))

;;; Float parameter errors

(defun rf-float-arg-error-form (size)
  "A call of a function with a float parameter, mostly with an integer
   argument, caught. Only where every safety in effect is at least 1."
  (if (not *rf-safe*)
      (make-random-integer-form size)
      (destructuring-bind (x) (rf-yparts size 1)
        (let ((p (rf-ysym "YP")) (g (rf-ysym "YFN"))
              (ft (rf-pick 'double-float 'single-float)))
          `(handler-case
               (flet ((,g (,p) (declare (type ,ft ,p)) (truncate (* ,p 2))))
                 ,@(rcase (1 nil) (1 `((declare (inline ,g)))) (1 `((declare (notinline ,g)))))
                 ,(rcase
                    (3 `(,g ,x))
                    (1 `(,g 1/2))
                    (2 `(,g (float (mod ,x 1000) ,(rf-proto ft))))))
             (error () -1))))))

;;; Multiple values across non-local exits

(defun rf-mv3-form (size)
  "VALUES, MULTIPLE-VALUE-BIND / -CALL / -PROG1, NTH-VALUE through BLOCK,
   RETURN-FROM, LABELS, UNWIND-PROTECT, CATCH and HANDLER-CASE. Cleanup forms
   only count in U, which is added to the result."
  (destructuring-bind (x y z w) (rf-yparts size 4)
    (let ((a (rf-ysym "YM")) (b (rf-ysym "YM")) (c (rf-ysym "YM"))
          (u (rf-ysym "YU")) (g (rf-ysym "YFN")) (n (rf-ysym "YN"))
          (p (rf-ysym "YP")) (blk (rf-ysym "YB")) (blk2 (rf-ysym "YB")))
      `(let ((,u 0))
         (declare (type (integer 0 1000) ,u) (ignorable ,u))
         (+ ,(rcase
               (2 `(multiple-value-bind (,a ,b ,c)
                       (block ,blk
                         (unwind-protect (return-from ,blk (values ,x ,y ,z))
                           (setq ,u (+ ,u 1))))
                     (+ ,a ,b ,c)))
               (2 `(multiple-value-bind (,a ,b) (unwind-protect (values ,x ,y) (incf ,u))
                     (+ ,a (or ,b 0))))
               (2 `(or (nth-value ,(random 4)
                                  (block ,blk
                                    (unwind-protect (return-from ,blk (values ,x ,y ,z))
                                      (incf ,u))))
                       0))
               (2 `(labels ((,g (,n)
                              (declare (type (integer 0 10) ,n))
                              (if (<= ,n 0)
                                  (values ,x ,y ,z)
                                  (multiple-value-bind (,a ,b ,c) (,g (1- ,n))
                                    (values ,b ,c (+ ,a ,n))))))
                     (multiple-value-call #'+ (,g (mod ,w 6)))))
               (2 `(multiple-value-bind (,a ,b)
                       (block ,blk
                         (labels ((,g () (unwind-protect (return-from ,blk (values ,x ,y))
                                           (incf ,u))))
                           (,g)
                           (values ,z ,w)))
                     (+ ,a ,b)))
               (2 `(multiple-value-call #'+ (values ,x ,y)
                     (block ,blk (return-from ,blk (values ,y ,z)))
                     (values)))
               (2 `(multiple-value-bind (,a ,b ,c)
                       (multiple-value-prog1 (values ,x ,y ,z)
                         (incf ,u)
                         (unwind-protect (values ,w) (incf ,u)))
                     (+ ,a ,b ,c)))
               (1 `(+ (values ,x ,y) (if (values ,z ,w) 1 0)))
               (1 `(multiple-value-bind (,a ,b) (values (values ,x ,y ,z) ,w) (+ ,a ,b)))
               (1 `(multiple-value-bind (,a ,b ,c) (values ,x)
                     (+ ,a (or ,b 1) (or ,c 2))))
               (1 `(reduce #'+ (multiple-value-list
                                (block ,blk (return-from ,blk (values ,x ,y ,z))))))
               (2 `(multiple-value-bind (,a ,b) (catch ',blk
                                                  (unwind-protect (throw ',blk (values ,x ,y))
                                                    (incf ,u)))
                     (+ ,a ,b)))
               (1 `(multiple-value-bind (,a ,b ,c) (ignore-errors (values ,x ,y ,z))
                     (+ ,a (or ,b 0) (or ,c 0))))
               (1 `(multiple-value-bind (,a ,b) (handler-case (values ,x ,y)
                                                  (error () (values 1 2)))
                     (+ ,a ,b)))
               (1 `(multiple-value-bind (,a ,b) (the (values integer integer) (values ,x ,y))
                     (+ ,a ,b)))
               (2 `(flet ((,g (,p) (block ,blk2
                                     (unwind-protect
                                          (if (> ,p 0)
                                              (return-from ,blk2 (values ,p ,y))
                                              (values ,z ,p 7))
                                       (incf ,u)))))
                     ,@(when (coin 2) `((declare (,(rf-pick 'inline 'notinline) ,g))))
                     (multiple-value-call #'+ (funcall #',g ,x) (,g ,w)))))
            ,u)))))

(defun make-random-integer-form (size)
  (if (and (>= size 4) (< (random 1.0) cl-user::*rf-typed-rate*))
      (rcase
        (4 (rf-typed-let-form size))
        (2 (rf-typed-mv-form size))
        (3 (rf-typed-float-form size))
        (4 (rf-typed-array-form size))
        (3 (rf-fill-pointer-form size))
        (2 (rf-svref-form size))
        (4 (rf-local-fn-form size))
        (3 (rf-recursive-local-form size))
        (1 (rf-typed-arg-error-form size))
        (2 (rf-mv2-form size))
        (3 (rf-mv3-form size))
        (3 (rf-fixnum-edge-form size))
        (1 (rf-float-arg-error-form size))
        (1 (rf-locally-form size)))
      (funcall *rf-typed-next-make-random-integer-form* size)))
