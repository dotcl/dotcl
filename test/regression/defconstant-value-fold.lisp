;;; A reference to a constant variable whose value is a number or a character
;;; compiles as that literal.
;;;
;;; CLHS 3.2.2.3 lets a reference to a constant variable be replaced by its
;;; value, and DEFCONSTANT requires the value to be known at compile time and
;;; never to change to a non-EQL one. The symbol used to be read through the
;;; dynamic binding lookup instead, so the typed paths did not recognize it:
;;; (setf (aref a i) +k+) on a declared (simple-array (unsigned-byte 8) (*))
;;; fell to the generic store, while (setf (aref a i) 1) took the raw one.
;;;
;;; Only numbers and characters are folded. Their identity is EQL, so a fresh
;;; literal is indistinguishable from the value the symbol holds. A list
;;; constant keeps its one object and is still read from the symbol.

(setf dotcl:*save-sil* t)

(defconstant +dvf-one+ 1)
(defconstant +dvf-big+ 1000000007)
(defconstant +dvf-half+ 0.5d0)
(defconstant +dvf-space+ #\Space)
(defconstant +dvf-list+
  (if (boundp '+dvf-list+) (symbol-value '+dvf-list+) (list 1 2 3)))

(defun %dvf-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %dvf-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %dvf-set-u8 (a i)
  (declare (type (simple-array (unsigned-byte 8) (*)) a) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref a i) +dvf-one+))

(defun %dvf-set-u8-literal (a i)
  (declare (type (simple-array (unsigned-byte 8) (*)) a) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref a i) 1))

(defun %dvf-set-fix (a i)
  (declare (type (simple-array fixnum (*)) a) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref a i) +dvf-big+))

(defun %dvf-set-double (a i)
  (declare (type (simple-array double-float (*)) a) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref a i) +dvf-half+))

(defun %dvf-add (x)
  (declare (fixnum x) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ x +dvf-big+)))

(defun %dvf-space-p (s i)
  (declare (simple-string s) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (char= (schar s i) +dvf-space+))

(defun %dvf-list () +dvf-list+)

;;; ---- shapes ----
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL.

;; The constant takes the literal's path: an immediate, the width check, and a
;; bare STELEM against the hoisted buffer. No dynamic lookup, no ArefSetL.
(deftest-emitting-only defconstant-value-fold.u8-store-is-raw
  (let ((d (%dvf-sil #'%dvf-set-u8)))
    (list (%dvf-count "DynamicBindings.Get" d)
          (%dvf-count "Runtime.ArefSetL" d)
          (%dvf-count "(STELEM-U1)" d)
          (%dvf-count "Runtime.CheckStoreU8" d)))
  (0 0 1 1))

;; Same instructions as the literal written in place of the constant. Only the
;; opcodes are compared: local and label names carry per-function counters.
(defun %dvf-ops (fn)
  (mapcar (lambda (ins) (if (consp ins) (car ins) ins)) (dotcl:function-sil fn)))

(deftest-emitting-only defconstant-value-fold.u8-store-matches-literal
  (equal (%dvf-ops #'%dvf-set-u8) (%dvf-ops #'%dvf-set-u8-literal))
  t)

(deftest-emitting-only defconstant-value-fold.fixnum-store-is-raw
  (let ((d (%dvf-sil #'%dvf-set-fix)))
    (list (%dvf-count "DynamicBindings.Get" d)
          (%dvf-count "Runtime.ArefSetL" d)
          (%dvf-count "(STELEM-I8)" d)))
  (0 0 1))

(deftest-emitting-only defconstant-value-fold.double-store-is-raw
  (let ((d (%dvf-sil #'%dvf-set-double)))
    (list (%dvf-count "DynamicBindings.Get" d)
          (%dvf-count "Runtime.ArefSetL" d)
          (%dvf-count "(LDC-R8" d)))
  (0 0 1))

(deftest-emitting-only defconstant-value-fold.arith-and-char-no-lookup
  (list (%dvf-count "DynamicBindings.Get" (%dvf-sil #'%dvf-add))
        (%dvf-count "DynamicBindings.Get" (%dvf-sil #'%dvf-space-p)))
  (0 0))

;; A list constant is not folded: it is still read from the symbol.
(deftest-emitting-only defconstant-value-fold.list-constant-not-folded
  (%dvf-count "DynamicBindings.Get" (%dvf-sil #'%dvf-list))
  1)

;;; ---- values ----

(deftest defconstant-value-fold.store-values
  (let ((u8 (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (fx (make-array 4 :element-type 'fixnum :initial-element 0))
        (df (make-array 4 :element-type 'double-float :initial-element 0d0)))
    (list (%dvf-set-u8 u8 2) (aref u8 2)
          (%dvf-set-fix fx 1) (aref fx 1)
          (%dvf-set-double df 3) (aref df 3)))
  (1 1 1000000007 1000000007 0.5d0 0.5d0))

(deftest defconstant-value-fold.arith-and-char-values
  (list (%dvf-add 3) (%dvf-space-p "a b" 1) (%dvf-space-p "a b" 0))
  (1000000010 t nil))

(deftest defconstant-value-fold.list-constant-identity
  (list (eq (%dvf-list) +dvf-list+) (eq (%dvf-list) (%dvf-list)))
  (t t))
