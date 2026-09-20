;;; A binding type declaration in a LET* body applies to the later inits too.
;;;
;;; CLHS 3.3.4: the scope of a binding type declaration for a LET* variable
;;; starts at that variable's binding. In a LET* the later bindings' init forms
;;; are inside that scope, so
;;;
;;;   (let* ((x ...) (y ...) (len (+ (* x x) (* y y))))
;;;     (declare (double-float x y len))
;;;     ...)
;;;
;;; must compile the multiplies knowing X and Y are doubles -- exactly as the
;;; same code written as two nested LETs does. The name-keyed declaration tables
;;; used to be established only on the way into the BODY, so a sibling init saw
;;; neither this LET*'s declarations nor the shadowing they imply.
;;;
;;; Both halves are asserted here. The first is a speed question with no visible
;;; answer, so it is covered by value: the same computation has to give the same
;;; number whichever way it is written. The second is a correctness question --
;;; an inner binding of a declared name is a DIFFERENT variable, and reading it
;;; through the outer declaration reads the wrong type out of the slot.

;;; --- the declaration reaches a later init ---

(defun lds-letstar (a b)
  (declare (double-float a b) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x a)
         (y b)
         (len (the double-float (+ (the double-float (* x x))
                                   (the double-float (* y y))))))
    (declare (double-float x y len))
    len))

(defun lds-nested-lets (a b)
  (declare (double-float a b) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x a)
        (y b))
    (declare (double-float x y))
    (let ((len (the double-float (+ (the double-float (* x x))
                                    (the double-float (* y y))))))
      (declare (double-float len))
      len)))

(deftest letstar-scope-double-agrees-with-nested-lets
  (list (lds-letstar 3.0d0 4.0d0)
        (lds-nested-lets 3.0d0 4.0d0)
        (= (lds-letstar 1.5d0 2.5d0) (lds-nested-lets 1.5d0 2.5d0)))
  (25.0d0 25.0d0 t))

(deftest letstar-scope-double-result-type
  (let ((r (lds-letstar 3.0d0 4.0d0)))
    (list (typep r 'double-float) (typep r 'single-float)))
  (t nil))

;;; SINGLE-FLOAT is a separate table and gets the same rule.

(defun lds-letstar-single (a b)
  (declare (single-float a b) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x a)
         (y b)
         (s (the single-float (+ (the single-float (* x x))
                                 (the single-float (* y y))))))
    (declare (single-float x y s))
    s))

(deftest letstar-scope-single
  (let ((r (lds-letstar-single 3.0f0 4.0f0)))
    (list r (typep r 'single-float) (typep r 'double-float)))
  (25.0f0 t nil))

;;; Both formats declared in one LET*, with the later init reading both.

(defun lds-letstar-mixed (a b)
  (declare (double-float a) (single-float b)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((d a)
         (s b)
         (r (the double-float (+ (the double-float (* d d))
                                 (float (the single-float (* s s)) 1.0d0)))))
    (declare (double-float d r) (single-float s))
    (list r (typep r 'double-float) (typep s 'single-float))))

(deftest letstar-scope-mixed-formats
  (lds-letstar-mixed 3.0d0 4.0f0)
  (25.0d0 t t))

;;; FIXNUM was already right and must stay right.

(defun lds-letstar-fixnum (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((a (the fixnum (* n 2)))
         (b (the fixnum (* a a))))
    (declare (fixnum a b))
    b))

(deftest letstar-scope-fixnum
  (list (lds-letstar-fixnum 3) (lds-letstar-fixnum 0))
  (36 0))

;;; A declared binding whose init is not itself statically a double: the slot
;;; stays boxed, so this is the by-name table rather than the native one, and it
;;; is the half a slot-keyed fix alone would not reach.

(defun lds-opaque () 2.5d0)

(defun lds-letstar-boxed-init ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((x (lds-opaque))
         (y (the double-float (* x 2.0d0))))
    (declare (double-float x y))
    (list x y)))

(deftest letstar-scope-boxed-init
  (lds-letstar-boxed-init)
  (2.5d0 5.0d0))

;;; --- shadowing: the inner binding is a different variable ---
;;;
;;; An inner LET* rebinding a name the outer scope declared DOUBLE-FLOAT, with a
;;; later init in that same inner LET* reading it. The outer declaration must
;;; have stopped applying, or the arithmetic reads a Fixnum slot as a double.

(defun lds-shadow-double-by-fixnum (a)
  (declare (double-float a) (optimize (speed 3) (safety 1) (debug 0)))
  (let ((x a))
    (declare (double-float x))
    (let* ((x 3)
           (y (* x x)))
      (list x y (+ x y)))))

(deftest letstar-scope-shadow-double-by-fixnum
  (lds-shadow-double-by-fixnum 9.5d0)
  (3 9 12))

;;; The same the other way round: an inner LET* declares DOUBLE-FLOAT for a name
;;; the outer scope declared FIXNUM, and a later inner init reads it.

(defun lds-shadow-fixnum-by-double (n)
  (declare (fixnum n) (optimize (speed 3) (safety 1) (debug 0)))
  (let ((v n))
    (declare (fixnum v))
    (let* ((v (float n 1.0d0))
           (w (the double-float (* v 2.0d0))))
      (declare (double-float v w))
      (list v w))))

(deftest letstar-scope-shadow-fixnum-by-double
  (lds-shadow-fixnum-by-double 3)
  (3.0d0 6.0d0))

;;; A shadow that is not declared at all in the inner LET*: the outer entry has
;;; to stop applying on the strength of the binding alone.

(defun lds-shadow-undeclared (a)
  (declare (double-float a) (optimize (speed 3) (safety 1) (debug 0)))
  (let ((c a))
    (declare (double-float c))
    (let* ((c "abc")
           (n (length c)))
      (list c n))))

(deftest letstar-scope-shadow-undeclared
  (lds-shadow-undeclared 1.0d0)
  ("abc" 3))

;;; --- a special binding in the middle of a LET* ---
;;;
;;; A dynamic binding carries no lexical type declaration, and after it the name
;;; must not resolve to an outer lexical one either.

(defvar *lds-level* 0.0d0)

(defun lds-letstar-special (a)
  (declare (double-float a) (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((*lds-level* a)
         (r (the double-float (* *lds-level* 2.0d0))))
    (declare (double-float r))
    (list *lds-level* r)))

(deftest letstar-scope-special-binding
  (list (lds-letstar-special 3.0d0) *lds-level*)
  ((3.0d0 6.0d0) 0.0d0))

;;; A lexical DOUBLE-FLOAT shadowed by a special binding of the same name, with
;;; a later init reading it: the read is dynamic, not a slot read.

(defvar *lds-shadowed* 7.0d0)

(defun lds-special-shadows-lexical ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((*lds-shadowed* 2.0d0)
         (r (the double-float (+ *lds-shadowed* 1.0d0))))
    (declare (double-float r))
    r))

(deftest letstar-scope-special-shadows-lexical
  (list (lds-special-shadows-lexical) *lds-shadowed*)
  (3.0d0 7.0d0))
