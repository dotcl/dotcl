;;; A declaration in a function's head that names an &AUX variable is a bound declaration of it:
;;; the type reaches the variable's binding, so an &aux counter declared (integer 0 23) is an
;;; int64 slot like the same variable in a LET. Declarations of the parameters stay where they
;;; were, a spec naming both kinds of variable is split, and IGNORE / SPECIAL of an &aux
;;; variable mean what they mean for a LET.

(setf dotcl:*save-sil* t)

(defun adt-aux (buf flag &aux (pos 0) (ds 5))
  (declare (type (simple-array character (24)) buf) (type (integer 0 23) pos) (type fixnum ds))
  (when flag (setf (char buf pos) #\-) (incf pos))
  (setf (char buf pos) #\a) (incf pos)
  (setf (char buf pos) (code-char (+ 48 (mod ds 10)))) (incf pos)
  (+ pos 3))
(defun adt-let (buf flag)
  (declare (type (simple-array character (24)) buf))
  (let ((pos 0) (ds 5))
    (declare (type (integer 0 23) pos) (type fixnum ds))
    (when flag (setf (char buf pos) #\-) (incf pos))
    (setf (char buf pos) #\a) (incf pos)
    (setf (char buf pos) (code-char (+ 48 (mod ds 10)))) (incf pos)
    (+ pos 3)))
;; One spec naming a parameter and an &aux variable; the shorthand (type-name var...) form.
(defun adt-mixed (n &aux (k 3) (m 4.5d0))
  (declare (type fixnum n k) (double-float m))
  (+ (* n k) (floor (* m 2))))
(defun adt-shorthand (n &aux (k 7))
  (declare (fixnum n k))
  (logand (* n k) 255))
;; IGNORE of an &aux variable: no warning, and the variable is bound.
(defun adt-ignore (n &aux (unused 1))
  (declare (ignore unused))
  (* n 2))
;; SPECIAL of an &aux variable is a dynamic binding.
(defvar *adt-special* :outer)
(defun adt-read-special () *adt-special*)
(defun adt-special (&aux (*adt-special* :aux))
  (declare (special *adt-special*))
  (adt-read-special))
;; An init that does not fit the declared type, as for a LET.
(defun adt-bad-init (&aux (x "not a fixnum"))
  (declare (type fixnum x))
  x)
(defun adt-bad-let ()
  (let ((x "not a fixnum"))
    (declare (type fixnum x))
    x))
;; The aux variable is assigned and read in a loop.
(defun adt-loop (n &aux (s 0) (i 0))
  (declare (type fixnum n s i))
  (loop (when (>= i n) (return s)) (incf s (* i i)) (incf i)))

(setf dotcl:*save-sil* nil)

(deftest aux-declared-types.same-values-as-the-let
  (let ((a (make-string 24 :initial-element #\space)) (b (make-string 24 :initial-element #\space)))
    (list (adt-aux a t) (adt-let b t) (string= a b)
          (progn (fill a #\space) (fill b #\space) (adt-aux a nil))
          (adt-let b nil) (string= a b)))
  (6 6 t 5 5 t))

(deftest aux-declared-types.mixed-and-shorthand-specs
  (list (adt-mixed 10) (adt-shorthand 100) (adt-shorthand 5) (adt-ignore 21) (adt-loop 10))
  (39 188 35 42 285))

(deftest aux-declared-types.special-aux-variable-is-dynamic
  (list (adt-special) *adt-special*)
  (:aux :outer))

(deftest aux-declared-types.a-bad-init-does-what-the-let-does
  (equal (handler-case (adt-bad-init) (error () :error))
         (handler-case (adt-bad-let) (error () :error)))
  t)

(deftest-emitting-only aux-declared-types.the-aux-counter-is-an-int64-slot
  (flet ((sil (fn) (%flat-text (dotcl:function-sil fn))))
    (list (and (search "Runtime.Increment" (sil #'adt-aux)) t)
          (and (search "Runtime.Increment" (sil #'adt-let)) t)
          (and (search "Runtime.Add\"" (sil #'adt-aux)) t)))
  (nil nil nil))
