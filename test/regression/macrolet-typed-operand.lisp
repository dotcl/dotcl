;;; A call to a MACROLET-bound macro in an integer operand position is looked
;;; at through its expansion. A local macro is how a library names an element
;;; of the state inside a hot function -- ironclad's Salsa20 core has
;;; (macrolet ((ref (i) `(aref x ,i))) ...) -- and (logxor (ref 0) ...) was not
;;; known to be an integer even though (aref x 0) on a (unsigned-byte 32) array
;;; is, so every step went through the generic LOGXOR and a boxed store.

(setf dotcl:*save-sil* t)

(defun %mto-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %mto-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %mto-mix (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (macrolet ((ref (i) `(aref v ,i)))
    (setf (ref 0) (logxor (ref 0) (ref 1) (ref 2)))
    (ref 0)))

;; A local macro of the same name as a global function: the expansion, not
;; the function, is what the operand means.
(defun %mto-shadow (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (macrolet ((logand (a b) `(logior ,a ,b)))
    (logxor 1 (logand (aref v 0) (aref v 1)))))

(defun %mto-vec (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 32) :initial-contents xs))

(deftest macrolet-typed-operand.values
  (list (%mto-mix (%mto-vec 1 2 4))
        (%mto-mix (%mto-vec #xffffffff #xf0f0f0f0 0))
        (%mto-shadow (%mto-vec 8 4)))
  (7 #x0f0f0f0f 13))

(deftest-emitting-only macrolet-typed-operand.native
  (let ((d (%mto-sil #'%mto-mix)))
    (list (%mto-count "Runtime.Logxor" d)
          (%mto-count "Runtime.ArefSetL" d)))
  (0 0))
