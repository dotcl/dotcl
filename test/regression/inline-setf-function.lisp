;;; Regression tests for (DECLAIM (INLINE (SETF f))): a SETF of (f args...)
;;; whose (SETF F) is a DEFUN proclaimed INLINE substitutes the definition, the
;;; same as a call to an ordinary INLINE function. It used to stay a generic
;;; call of the setf function on every store (ironclad's (SETF UB32REF/LE)).

(setf dotcl:*save-sil* t)

(defun %isf-calls-setf-fn-p (fn)
  "True when FN's SIL still fetches a setf function to call it."
  (and (search "GetSetfFunctionBySymbol" (%flat-text (dotcl:function-sil fn))) t))

(declaim (inline %isf-ref (setf %isf-ref)))
(defun %isf-ref (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (aref v i))
(defun (setf %isf-ref) (val v i)
  (declare (type (unsigned-byte 8) val)
           (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (setf (aref v i) val))

(defun %isf-use (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (setf (%isf-ref v 1) 7)
  (%isf-ref v 1))

(deftest-compiled-only isf-substituted
  (%isf-calls-setf-fn-p #'%isf-use)
  nil)

(deftest isf-value
  (%isf-use (make-array 4 :element-type '(unsigned-byte 8)))
  7)

;; The SETF form returns what the setf function returns.
(defun %isf-use-value (v)
  (setf (%isf-ref v 2) 9))

(deftest isf-setf-form-value
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (list (%isf-use-value v) (aref v 2)))
  (9 9))

;;; ---- evaluation order: place arguments left to right, then the value ----

(defvar *isf-log* nil)
(defun %isf-note (x) (push x *isf-log*) x)

(defun %isf-order (v)
  (setf (%isf-ref (%isf-note v) (%isf-note 0)) (%isf-note 5)))

(deftest isf-evaluation-order
  (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
    (setq *isf-log* nil)
    (list (%isf-order v) (aref v 0) (length *isf-log*)
          (cdr (reverse *isf-log*))))
  (5 5 3 (0 5)))

;; The value form changes a variable that is a place argument: the argument is
;; the value it had before the value form ran.
(defun %isf-order-setq (v)
  (let ((i 0))
    (setf (%isf-ref v i) (progn (setq i 1) 3))
    (list i (aref v 0) (aref v 1))))

(deftest isf-place-argument-before-value
  (%isf-order-setq (make-array 2 :element-type '(unsigned-byte 8)))
  (1 3 0))

;;; ---- several values from the setf function ----

(declaim (inline (setf %isf-mv)))
(defun (setf %isf-mv) (val cell)
  (setf (car cell) val)
  (values val :second))

(defun %isf-use-mv (cell)
  (multiple-value-list (setf (%isf-mv cell) 4)))

(deftest-compiled-only isf-mv-substituted
  (%isf-calls-setf-fn-p #'%isf-use-mv)
  nil)

(deftest isf-mv-values
  (let ((cell (list 0)))
    (list (%isf-use-mv cell) cell))
  ((4 :second) (4)))

;;; ---- NOTINLINE turns it off ----

(defun %isf-local-notinline (v)
  (declare (notinline (setf %isf-ref)))
  (setf (%isf-ref v 0) 6)
  (aref v 0))

(deftest-compiled-only isf-local-notinline-not-substituted
  (%isf-calls-setf-fn-p #'%isf-local-notinline)
  t)

(deftest isf-local-notinline-value
  (%isf-local-notinline (make-array 2 :element-type '(unsigned-byte 8)))
  6)

(declaim (inline (setf %isf-global-off)))
(defun (setf %isf-global-off) (val cell) (setf (car cell) val))
(declaim (notinline (setf %isf-global-off)))
(defun %isf-use-global-off (cell) (setf (%isf-global-off cell) 2) cell)

(deftest-compiled-only isf-global-notinline-not-substituted
  (%isf-calls-setf-fn-p #'%isf-use-global-off)
  t)

(deftest isf-global-notinline-value
  (%isf-use-global-off (list 0))
  (2))

;; Without any INLINE proclamation it stays a call.
(defun (setf %isf-plain) (val cell) (setf (car cell) val))
(defun %isf-use-plain (cell) (setf (%isf-plain cell) 3) cell)

(deftest-compiled-only isf-no-proclamation-not-substituted
  (%isf-calls-setf-fn-p #'%isf-use-plain)
  t)

(deftest isf-no-proclamation-value
  (%isf-use-plain (list 0))
  (3))

;;; ---- redefinition ----

;; A redefinition while still INLINE is what later call sites substitute.
(declaim (inline (setf %isf-redef)))
(defun (setf %isf-redef) (val cell) (setf (car cell) val))
(defun (setf %isf-redef) (val cell) (setf (car cell) (list :new val)))
(defun %isf-use-redef (cell) (setf (%isf-redef cell) 1) cell)

(deftest isf-redefinition-value
  (%isf-use-redef (list 0))
  ((:new 1)))

;; A redefinition after NOTINLINE drops the recorded body: later call sites
;; call the current definition.
(declaim (inline (setf %isf-redef2)))
(defun (setf %isf-redef2) (val cell) (setf (car cell) (list :old val)))
(declaim (notinline (setf %isf-redef2)))
(defun (setf %isf-redef2) (val cell) (setf (car cell) (list :new val)))
(declaim (inline (setf %isf-redef2)))
(defun %isf-use-redef2 (cell) (setf (%isf-redef2 cell) 1) cell)

(deftest isf-redefinition-after-notinline
  (%isf-use-redef2 (list 0))
  ((:new 1)))

;;; ---- a local (SETF F) is a different function ----

(defun %isf-flet-shadow (v)
  (flet (((setf %isf-ref) (val v i) (declare (ignore v i)) (list :local val)))
    (setf (%isf-ref v 0) 1)))

(deftest isf-flet-shadow
  (%isf-flet-shadow (make-array 2 :element-type '(unsigned-byte 8)))
  (:local 1))

;;; ---- a setf function whose body stores through another INLINE setf ----

(declaim (inline (setf %isf-ref2)))
(defun (setf %isf-ref2) (val v i)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (setf (%isf-ref v i) (logand val #xff)
        (%isf-ref v (1+ i)) (ldb (byte 8 8) val))
  val)

(defun %isf-use-ref2 (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (setf (%isf-ref2 v 0) #x1234)
  (list (aref v 0) (aref v 1)))

(deftest-compiled-only isf-nested-substituted
  (%isf-calls-setf-fn-p #'%isf-use-ref2)
  nil)

(deftest isf-nested-value
  (%isf-use-ref2 (make-array 2 :element-type '(unsigned-byte 8)))
  (#x34 #x12))

;;; ---- a recursive setf function terminates ----

(declaim (inline (setf %isf-rec)))
(defun (setf %isf-rec) (val cell)
  (if (consp (cdr cell))
      (setf (%isf-rec (cdr cell)) val)
      (setf (car cell) val)))

(defun %isf-use-rec (cell) (setf (%isf-rec cell) :end) cell)

(deftest isf-recursive
  (%isf-use-rec (list 1 2 3))
  (1 2 :end))

;;; ---- a constant place argument needs no temp ----

(deftest isf-expansion-constant-argument
  (let ((form (macroexpand-1 '(setf (%isf-plain 3) (foo)))))
    form)
  (funcall #'(setf %isf-plain) (foo) 3))
