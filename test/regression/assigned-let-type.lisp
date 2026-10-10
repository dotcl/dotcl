;;; A LET variable the body assigns gets the integer type that its init and
;;; every assigned value stay in, solved together (a value may depend on the
;;; variable itself). It used to be typed from the init only when never
;;; assigned, so a hash register like A = (LDB (BYTE 32 0) (+ A X)) stayed a
;;; LispObject and was boxed at every assignment.

(setf dotcl:*save-sil* t)

(defun %alt-slot-type (fn name)
  "The local type string of the first binding of NAME in FN's SIL, or NIL."
  (let ((prefix (concatenate 'string name "_")))
    (labels ((walk (x)
               (cond ((atom x) nil)
                     ((and (eq (car x) :declare-local)
                           (symbolp (cadr x))
                           (let ((n (symbol-name (cadr x))))
                             (and (> (length n) (length prefix))
                                  (string= prefix n :end2 (length prefix))
                                  (every #'digit-char-p (subseq n (length prefix))))))
                      (return-from %alt-slot-type (caddr x)))
                     (t (walk (car x)) (walk (cdr x))))))
      (walk (dotcl:function-sil fn))
      nil)))

;; Fixed point: the value depends on the variable itself.
(defun %alt-fix (w)
  (declare (type (simple-array (unsigned-byte 32) (*)) w))
  (let ((acc (aref w 0)))
    (dotimes (i (length w))
      (setq acc (ldb (byte 32 0) (+ acc (aref w i)))))
    (logxor acc 1)))

;; Self-dependent without a mask: LOGXOR of two (unsigned-byte 32) values.
(defun %alt-xor (w)
  (declare (type (simple-array (unsigned-byte 32) (*)) w))
  (let ((acc 0))
    (dotimes (i (length w))
      (setq acc (logxor acc (aref w i))))
    (+ acc 1)))

;; The union of the ranges of several assignments.
(defun %alt-union (p)
  (let ((x 0))
    (if p (setq x 5) (setq x 300))
    (+ x 1)))

;; (unsigned-byte 64): the bits slot.
(defun %alt-u64 (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w))
  (let ((acc (aref w 0)))
    (dotimes (i (length w))
      (setq acc (ldb (byte 64 0) (+ acc (aref w i)))))
    (logand acc #xffff)))

;; Not determined: a sum of unknown values, and a counter without a bound.
(defun %alt-sum (l)
  (let ((n 0))
    (dolist (x l) (setq n (+ n x)))
    n))

(defun %alt-double (k)
  (let ((x 1))
    (dotimes (i k) (setq x (* x 2)))
    x))

;; Captured by a closure that assigns it.
(defun %alt-closure (w)
  (declare (type (simple-array (unsigned-byte 32) (*)) w))
  (let ((a (aref w 0)))
    (flet ((bump () (setq a (ldb (byte 32 0) (+ a #xffffffff)))))
      (bump) (bump))
    (logand a 7)))

;; A special variable.
(defvar *alt-special* 0)
(defun %alt-special-reader () *alt-special*)
(defun %alt-special ()
  (let ((*alt-special* 1))
    (setq *alt-special* (logand (+ *alt-special* 6) 7))
    (list (%alt-special-reader) (+ *alt-special* 1))))

;; Assigned through a symbol macro, and through an inner binding of the same
;; name that holds something else.
(defun %alt-symbol-macro ()
  (let ((a 0))
    (symbol-macrolet ((b a))
      (setq b 1000))
    (let ((a (list 1 2)))
      (setq a (cdr a))
      (setq a (car a)))
    (+ a 1)))

(defun %alt-shadow (n)
  (let ((a 0))
    (let ((a (expt 2 n)))
      (setq a (* a a)))
    (setq a (logand (+ a 3) 7))
    (+ a 1)))

;; An assignment whose value is not an integer.
(defun %alt-mixed (p)
  (let ((a 0))
    (when p (setq a :key))
    (if (integerp a) (+ a 1) a)))

;; A free SPECIAL declaration: N inside the LOCALLY is the dynamic variable.
(defun %alt-free-special ()
  (let ((n 1) (m 0))
    (progv '(n) (list (expt 2 80))
      (locally (declare (special n))
        (setq m n)))
    (setq n (logand (+ n 2) 7))
    (+ m n 1)))

;; MULTIPLE-VALUE-SETQ and INCF of a value the walk does not know.
(defun %alt-mvsetq (k)
  (let ((a 0) (b 0))
    (multiple-value-setq (a b) (floor k 3))
    (incf a (expt 2 k))
    (+ a b)))

(deftest-compiled-only alt-slots
  (list (%alt-slot-type #'%alt-fix "ACC")
        (%alt-slot-type #'%alt-xor "ACC")
        (%alt-slot-type #'%alt-union "X")
        (%alt-slot-type #'%alt-u64 "ACC")
        (%alt-slot-type #'%alt-sum "N")
        (%alt-slot-type #'%alt-double "X")
        (%alt-slot-type #'%alt-closure "A")
        (%alt-slot-type #'%alt-mixed "A"))
  ("Int64" "Int64" "Int64" "Int64" "LispObject" "LispObject" "LispObject[]" "LispObject"))

(deftest alt-values
  (let ((w32 (make-array 5 :element-type '(unsigned-byte 32)
                           :initial-contents '(#xffffffff #xfffffffe 7 #x80000000 1)))
        (w64 (make-array 3 :element-type '(unsigned-byte 64)
                           :initial-contents '(#xffffffffffffffff #x8000000000000001 5))))
    (list (%alt-fix w32) (%alt-xor w32)
          (%alt-union t) (%alt-union nil)
          (%alt-u64 w64)
          (%alt-sum '(1 2 3)) (%alt-sum (list most-positive-fixnum most-positive-fixnum))
          (%alt-double 3) (%alt-double 70)
          (%alt-closure w32)
          (%alt-special)
          (%alt-symbol-macro) (%alt-shadow 40)
          (%alt-mixed nil) (%alt-mixed t)
          (%alt-mvsetq 7)
          (%alt-free-special)))
  (#x80000005 #x80000008 6 301 4 6 #xfffffffffffffffe 8 #x400000000000000000 5
   (7 8) 1001 4 1 :key 131 #x100000000000000000004))

;; A raw slot boxes the value at every read that is not a raw integer use (an
;; argument of an ordinary call); a LispObject slot boxes it once per
;; assignment. With more such reads than assignments the variable stays boxed
;; (ironclad's SHA-256 passes its registers to local functions 8 times a round
;; and assigns them twice).
(declaim (notinline %alt-sink))
(defun %alt-sink (&rest xs) (length xs))

(defun %alt-boxed-reads (w)
  (declare (type (simple-array (unsigned-byte 32) (*)) w))
  (let ((a (aref w 0)))
    (setq a (ldb (byte 32 0) (+ a 1)))
    (list (%alt-sink a a a) (logand a 7))))

(defun %alt-few-boxed-reads (w)
  (declare (type (simple-array (unsigned-byte 32) (*)) w))
  (let ((a (aref w 0)))
    (setq a (ldb (byte 32 0) (+ a 1)))
    (setq a (ldb (byte 32 0) (* a 3)))
    (list (%alt-sink a) (logand a 7))))

(deftest-compiled-only alt-boxed-reads-slots
  (list (%alt-slot-type #'%alt-boxed-reads "A")
        (%alt-slot-type #'%alt-few-boxed-reads "A"))
  ("LispObject" "Int64"))

(deftest alt-boxed-reads-values
  (let ((w (make-array 1 :element-type '(unsigned-byte 32) :initial-element #xffffffff)))
    (list (%alt-boxed-reads w) (%alt-few-boxed-reads w)))
  ((3 0) (1 0)))
