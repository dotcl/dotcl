;;;; IL-verifiability stress fixture (compiled, then checked with ilverify).
;;;;
;;;; Purpose: dotcl's emitter must produce VERIFIABLE CIL: no covariant calls
;;;; (passing a base LispObject where a derived type like Symbol is required
;;;; without a castclass), no stack-type mismatches, etc. CoreCLR's JIT tolerates
;;;; such IL, but strict AOT C++ codegens (Unity IL2CPP / WebGL) reject it. This
;;;; file exercises the emit-heavy paths so a regression surfaces here in seconds
;;;; rather than in a 25-minute IL2CPP build. See scripts/ilverify-check.sh and
;;;; `make ilverify`.
(defpackage :dotcl-ilverify (:use :cl))
(in-package :dotcl-ilverify)

;; Global special bindings (defvar/defparameter/defconstant init = ModuleInit;
;; the historical covariant-IL site, see the DynamicBindings.Set(Symbol,...) calls).
(defvar *a* 0)
(defparameter *b* 1)
(defconstant +c+ 2)

;; defstruct + setf places + read-modify-write + rotatef.
(defstruct pt x y)
(defun places (lst v)
  (setf (car lst) v
        (cdr lst) (list v))
  (incf (car lst))
  (push 9 (cdr lst))
  (let ((p (make-pt :x 1 :y 2)))
    (setf (pt-x p) 10)
    (incf (pt-y p))
    (rotatef (pt-x p) (pt-y p))
    (list (pt-x p) (pt-y p))))

;; CLOS: inheritance, generic dispatch, call-next-method, setf on accessor.
(defclass shape () ((name :initarg :name :accessor shape-name)))
(defclass circ (shape) ((r :initarg :r :accessor circ-r)))
(defgeneric area (s))
(defmethod area ((s circ)) (* 3 (circ-r s) (circ-r s)))
(defgeneric describe-it (x))
(defmethod describe-it ((s shape)) (list :shape (shape-name s)))
(defmethod describe-it ((s circ)) (cons :circ (call-next-method)))
(defun rename (c) (setf (shape-name c) "z"))

;; Control flow: dotimes, handler-case, multiple-value, tagbody/go,
;; non-local exit across flet/labels, loop, hash iteration.
(defun ctrl (n)
  (let ((acc 0))
    (dotimes (i n) (incf acc i))
    (handler-case (if (> acc 5) (error "big") acc)
      (error (e) (declare (ignore e)) -1))))
(defun mv () (multiple-value-bind (q r) (floor 17 5) (+ q r)))
(defun tags (n)
  (let ((s 0))
    (tagbody
     top (when (>= s n) (go end)) (incf s) (go top)
     end)
    s))
(defun nle (n)
  (block done
    (flet ((bail (v) (return-from done v)))
      (labels ((rec (k acc) (if (zerop k) (bail acc) (rec (1- k) (+ acc k)))))
        (rec n 0)))))
(defun lp (xs)
  (loop for x in xs
        for i from 0
        when (evenp x) collect (cons i x) into evens
        else sum x into odds
        finally (return (values evens odds))))
(defun fmt (n) (format nil "~r ~:d ~5,'0d ~x" n (* n 1000) n n))
(defun hashing (kvs)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (kv kvs) (setf (gethash (car kv) h) (cdr kv)))
    (loop for k being the hash-keys of h using (hash-value v) collect (cons k v))))

;; A closure capturing a mutated variable.
(defun adder (n) (lambda (x) (incf n x)))

;; A DEFUN that is not at top level: registration happens at run time through
;; CilAssembler.RegisterFunctionOnSymbol, which takes a Symbol. The symbol
;; arrives widened to LispObject, so the call needs a narrowing castclass to
;; stay verifiable. Real code hits this constantly (asdf's guarded defuns).
(defun define-nested (flag)
  (when flag
    (defun nested-defun (x) (* x 2)))
  flag)

;; The same, for a closure defun (free variable captured) and a (setf ...) name.
(defun define-closure-defun (n)
  (defun closure-defun (x) (+ x n))
  (defun (setf nested-place) (v x) (list v x))
  n)

;; Builtins with a defaulted numeric argument: the default has to be pushed as
;; an i8 (Fixnum.Make takes a long). DIGIT-CHAR-P without a radix used to push
;; an i4, which runs but is not verifiable.
(defun digits-only-p (s)
  (loop :for c :across s :always (digit-char-p c)))

;; A struct constructor whose slot value contains a branch. The type name is
;; pushed before the slot values are compiled, so a slot holding a LOOP (or any
;; branching form) left it pending across a join label -- IL the verifier rejects
;; and the JIT refuses at the first call ("invalid program"). Found in coalton,
;; where (make-node-body :nodes (loop ...) :last-node ...) is ordinary style; ten
;; of its ninety-five fasls carried it.
(defstruct nb nodes last-node)

(defun make-nb-from (list ctx)
  (make-nb :nodes (loop :for e :in list :collect (list e ctx))
           :last-node (if (consp list) (car list) ctx)))

;; The same shape with the branch in the second slot, and with the constructor
;; nested inside VALUES (how coalton's renamer writes it).
(defun make-nb-second (list ctx)
  (values (make-nb :nodes ctx
                   :last-node (loop :for e :in list :count (consp e)))
          ctx))

;; INVOKE-RESTART has the same operand shape (designator, then an args array).
(defun invoke-with-branching-arg (r list)
  (restart-case (invoke-restart r (loop :for e :in list :collect e))
    (continue () :ok)))

;; An (unsigned-byte 64) local keeps its bits in an Int64 slot. When it is also
;; known to be a fixnum (here from its init's range), a fixnum operation on it
;; must read the slot as the int64 it is, not unbox it as a Fixnum object.
(defun u64-local-with-fixnum-range (k)
  (declare (type (integer 0 10) k))
  (let ((n k))
    (declare (type (unsigned-byte 64) n))
    (ldb (byte 64 0) (ash (logxor #x70 (logand n #xff)) 56))))

;; DOTCL-SIMD operations compiled to Vector128 values: struct locals, closed
;; generic calls, array buffers read and written as packs.
(eval-when (:compile-toplevel :load-toplevel :execute) (require "dotcl-simd"))

(defun simd-kernel (x y z n)
  (declare (type (simple-array double-float (*)) x y z)
           (type (integer 0 100000) n))
  (do ((i 0 (+ i 2))) ((>= i n) z)
    (declare (type (integer 0 100002) i))
    (setf (dotcl-simd:f64.2-aref z i)
          (dotcl-simd:f64.2+ (dotcl-simd:f64.2-aref x i)
                             (dotcl-simd:f64.2* (dotcl-simd:f64.2-aref y i) 2d0)))))

(defun simd-kernel-unsafe (x z n)
  (declare (type (simple-array single-float (*)) x z)
           (type (integer 0 100000) n)
           (optimize (speed 3) (safety 0)))
  (do ((i 0 (+ i 4))) ((>= i n) z)
    (declare (type (integer 0 100004) i))
    (setf (dotcl-simd:f32.4-aref z i)
          (dotcl-simd:f32.4-sqrt (dotcl-simd:f32.4-max (dotcl-simd:f32.4-aref x i) 0f0)))))

(defun simd-tree (a b f)
  (dotcl-simd:u32.4-xor (dotcl-simd:u32.4-andc1 a (funcall f b))
                        (dotcl-simd:u32.4-not (dotcl-simd:u32.4+ a b))))

(defun simd-int-kernel (x y z n)
  (declare (type (simple-array (unsigned-byte 8) (*)) x y z)
           (type (integer 0 100000) n))
  (do ((i 0 (+ i 16))) ((>= i n) z)
    (declare (type (integer 0 100016) i))
    (setf (dotcl-simd:u8.16-aref z i)
          (dotcl-simd:u8.16-if (dotcl-simd:u8.16< (dotcl-simd:u8.16-aref x i) (dotcl-simd:u8.16-aref y i))
                               (dotcl-simd:u8.16-shiftr (dotcl-simd:u8.16-aref x i) 2)
                               (dotcl-simd:u8.16-shiftl (dotcl-simd:u8.16-aref y i) 1)))))

(defun simd-u64-kernel (x z n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x z)
           (type (integer 0 100000) n)
           (optimize (speed 3) (safety 0)))
  (do ((i 0 (+ i 2))) ((>= i n) z)
    (declare (type (integer 0 100002) i))
    (setf (dotcl-simd:u64.2-aref z i) (dotcl-simd:u64.2-xor (dotcl-simd:u64.2-aref x i) 7))))

(defun simd-dot-local (x y n)
  (declare (type (simple-array double-float (*)) x y)
           (type (integer 0 100000) n))
  (let ((acc (dotcl-simd:f64.2 0d0)) (k (dotcl-simd:s32.4 1)))
    (declare (type dotcl-simd:f64.2 acc) (type dotcl-simd:s32.4 k))
    (do ((i 0 (+ i 2))) ((>= i n))
      (declare (type (integer 0 100002) i))
      (setq acc (dotcl-simd:f64.2+ acc (dotcl-simd:f64.2* (dotcl-simd:f64.2-aref x i)
                                                         (dotcl-simd:f64.2-aref y i)))
            k (dotcl-simd:s32.4+ k k)))
    (values (dotcl-simd:f64.2-horizontal+ acc) k (setq k n))))

(defun simd-offset-kernel (x z n)
  (declare (type (simple-array double-float (*)) x z)
           (type (integer 0 100000) n))
  (let ((acc (dotcl-simd:f64.2 0d0)))
    (declare (type dotcl-simd:f64.2 acc))
    (do ((i 0 (+ i 4))) ((>= i n))
      (declare (type (integer 0 100004) i))
      (setf acc (the dotcl-simd:f64.2
                     (dotcl-simd:f64.2+ acc (dotcl-simd:f64.2-aref x (+ i 2))))
            (dotcl-simd:f64.2-aref z i) acc))
    (dotcl-simd:f64.2-horizontal-max acc)))

(defun simd-convert (a b)
  (dotcl-simd:f32.4+ (dotcl-simd:f32.4-from-s32.4 a) (dotcl-simd:f32.4-from-u32.4 b)))

(defun simd-convert-down (a)
  (dotcl-simd:s64.2-from-f64.2 (dotcl-simd:f64.2* a 2d0)))
