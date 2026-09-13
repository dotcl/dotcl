;;; REINITIALIZE-INSTANCE does not pay for validation it cannot need.
;;;
;;; Three costs, all per call and none of them required:
;;;   - a HashSet<string> for the &key names of applicable methods, built even when
;;;     no applicable method has a &key (the same empty-set waste SHARED-INITIALIZE
;;;     had), and built even when no initargs were supplied so there is nothing to
;;;     validate against it;
;;;   - the walk over two generic functions' methods to fill that set, likewise run
;;;     with no initargs to check;
;;;   - an argument array built only to insert NIL for SLOT-NAMES at index 1.
;;;
;;; The method walk went through the IReadOnlyList face of the method snapshot,
;;; which allocates an enumerator per loop -- two per call.

(defclass ric-plain ()
  ((a :initarg :a :initform 0 :accessor ric-a)
   (b :initarg :b :initform 0 :accessor ric-b)))

(defclass ric-keyed ()
  ((a :initarg :a :initform 0 :accessor ric-keyed-a)))

(defmethod reinitialize-instance :after ((x ric-keyed) &key extra)
  (when extra (setf (ric-keyed-a x) extra)))

(defclass ric-aok ()
  ((a :initarg :a :initform 0)))

(defmethod reinitialize-instance :after ((x ric-aok) &key &allow-other-keys) nil)

;;; Behaviour first: the fast exits must not change what the call does.

(deftest reinitialize-instance-consing.returns-the-instance
  (let ((o (make-instance 'ric-plain :a 1)))
    (eq (reinitialize-instance o) o))
  t)

;;; SLOT-NAMES is NIL, so a bound slot keeps its value and no initform re-runs.
(deftest reinitialize-instance-consing.no-initargs-keeps-slots
  (let ((o (make-instance 'ric-plain :a 7 :b 8)))
    (reinitialize-instance o)
    (list (ric-a o) (ric-b o)))
  (7 8))

(deftest reinitialize-instance-consing.initarg-is-applied
  (let ((o (make-instance 'ric-plain :a 1 :b 2)))
    (reinitialize-instance o :a 42)
    (list (ric-a o) (ric-b o)))
  (42 2))

;;; Validation still rejects an initarg no slot and no method accepts.
(deftest reinitialize-instance-consing.invalid-initarg-signals
  (let ((o (make-instance 'ric-plain)))
    (handler-case (progn (reinitialize-instance o :nope 1) :no-error)
      (error () :error)))
  :error)

;;; A method's &key name is a valid initarg (CLHS 7.1.2), and the method runs.
(deftest reinitialize-instance-consing.method-key-is-valid
  (let ((o (make-instance 'ric-keyed)))
    (reinitialize-instance o :extra 5)
    (ric-keyed-a o))
  5)

;;; &allow-other-keys on an applicable method turns validation off.
(deftest reinitialize-instance-consing.method-allow-other-keys
  (let ((o (make-instance 'ric-aok)))
    (handler-case (progn (reinitialize-instance o :anything 1) :no-error)
      (error () :error)))
  :no-error)

;;; The initarg count crosses the positional/array boundary in the SHARED-INITIALIZE
;;; call (1 pair and 2 pairs go positionally, 3 pairs falls back to the array).
(deftest reinitialize-instance-consing.many-initargs
  (let ((o (make-instance 'ric-plain)))
    (reinitialize-instance o :a 1 :b 2)
    (let ((r (list (ric-a o) (ric-b o))))
      (reinitialize-instance o :a 3)
      (append r (list (ric-a o) (ric-b o)))))
  (1 2 3 2))

;;; The point of the change.

(defparameter *ric-obj* (make-instance 'ric-plain))

(defun %ric-none (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (reinitialize-instance *ric-obj*)))))

(defun %ric-one (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (reinitialize-instance *ric-obj* :a 1)))))

;; Ceilings rather than zero: the call still allocates (the GF's own &rest array,
;; and the slot write for the initarg case). These are set between what it costs
;; now (72 and 104 B per call) and what it cost before (200 and 232), so a
;; reappearance of any of the three is caught while ordinary noise is not.
;; Compiled-only, like the other consing assertions.
(deftest-compiled-only reinitialize-instance-consing.no-initargs-ceiling
  (< (bytes-per-op #'%ric-none) 120)
  t)

(deftest-compiled-only reinitialize-instance-consing.one-initarg-ceiling
  (< (bytes-per-op #'%ric-one) 150)
  t)
