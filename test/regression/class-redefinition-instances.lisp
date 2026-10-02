;;; Redefining a class updates the instances made before the redefinition
;;; (CLHS 4.3.6). The update is lazy: an obsolete instance is brought to the
;;; new layout the next time one of its slots is read or written, and
;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS is called with the added local slots,
;;; the discarded slots and a property list of the discarded values. The
;;; runtime used to leave the old slot vector in place, so a slot added by the
;;; redefinition read past its end ("Index was outside the bounds of the
;;; array") and a removed slot shifted the others.

;;; A slot added: initialized from its initform, the old value kept.
(defclass cri-add () ((x :initform 1)))
(defvar *cri-add* (make-instance 'cri-add))
(setf (slot-value *cri-add* 'x) 10)
(defclass cri-add () ((x :initform 1) (y :initform 2)))

(deftest class-redefinition.slot-added
  (list (slot-value *cri-add* 'x) (slot-value *cri-add* 'y))
  (10 2))

;;; A slot removed: the remaining slots keep their values even though their
;;; positions moved.
(defclass cri-del () ((a :initform 1) (b :initform 2) (c :initform 3)))
(defvar *cri-del* (make-instance 'cri-del))
(setf (slot-value *cri-del* 'c) 30)
(defclass cri-del () ((a :initform 1) (c :initform 3)))

(deftest class-redefinition.slot-removed
  (list (slot-value *cri-del* 'a) (slot-value *cri-del* 'c)
        (slot-exists-p *cri-del* 'b))
  (1 30 nil))

;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS sees what changed. A slot renamed is a
;;; slot discarded plus a slot added; a user method can carry the value over.
(defvar *cri-log* nil)
(defclass cri-ren () ((old-name :initarg :v)))
(defvar *cri-ren* (make-instance 'cri-ren :v 7))
(defvar *cri-ren-unbound* (make-instance 'cri-ren))
(defmethod update-instance-for-redefined-class :after
    ((inst cri-ren) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (push (list added discarded plist) *cri-log*)
  (when (getf plist 'old-name)
    (setf (slot-value inst 'new-name) (getf plist 'old-name))))
(defclass cri-ren () ((new-name :initform :default)))

(deftest class-redefinition.slot-renamed
  (list (slot-value *cri-ren* 'new-name) (first *cri-log*))
  (7 ((new-name) (old-name) (old-name 7))))

;;; An unbound discarded slot is listed as discarded but has no plist entry,
;;; and the added slot takes its initform (the standard method runs
;;; SHARED-INITIALIZE on the added slots before the :after method).
(deftest class-redefinition.unbound-discarded
  (list (slot-value *cri-ren-unbound* 'new-name) (first *cri-log*))
  (:default ((new-name) (old-name) nil)))

;;; The update happens once: touching the instance again calls nothing.
(deftest class-redefinition.updated-once
  (let ((n (length *cri-log*)))
    (slot-value *cri-ren* 'new-name)
    (setf (slot-value *cri-ren* 'new-name) 8)
    (- (length *cri-log*) n))
  0)

;;; :instance -> :class. The local value is discarded (and reported), and the
;;; instance sees the shared slot, initialized from its initform.
(defvar *cri-log2* nil)
(defclass cri-alloc () ((s :initform 3)))
(defvar *cri-alloc* (make-instance 'cri-alloc))
(setf (slot-value *cri-alloc* 's) 99)
(defmethod update-instance-for-redefined-class :before
    ((inst cri-alloc) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (push (list added discarded plist) *cri-log2*))
(defclass cri-alloc () ((s :initform 3 :allocation :class)))

(deftest class-redefinition.instance-to-class
  (list (slot-value *cri-alloc* 's) (first *cri-log2*))
  (3 (nil (s) (s 99))))

;;; :class -> :instance. The shared value becomes the instance's own value and
;;; the slot is not reported as added.
(defclass cri-shared () ((s :initform 3 :allocation :class) (z :initform 0)))
(defvar *cri-shared* (make-instance 'cri-shared))
(setf (slot-value *cri-shared* 's) 42)
(defvar *cri-log3* nil)
(defmethod update-instance-for-redefined-class :before
    ((inst cri-shared) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (push (list added discarded plist) *cri-log3*))
(defclass cri-shared () ((s :initform 3) (z :initform 0)))

(deftest class-redefinition.class-to-instance
  (list (slot-value *cri-shared* 's) (slot-value *cri-shared* 'z)
        (slot-value (make-instance 'cri-shared) 's) (first *cri-log3*))
  (42 0 3 (nil nil nil)))

;;; MAKE-INSTANCES-OBSOLETE without a redefinition: the instances keep their
;;; values and UPDATE-INSTANCE-FOR-REDEFINED-CLASS runs once for each.
(defclass cri-mio () ((v :initarg :v :accessor cri-mio-v)))
(defvar *cri-mio* (list (make-instance 'cri-mio :v 1) (make-instance 'cri-mio :v 2)))
(defvar *cri-mio-count* 0)
(defmethod update-instance-for-redefined-class :before
    ((inst cri-mio) added discarded plist &rest initargs)
  (declare (ignore added discarded plist initargs))
  (incf *cri-mio-count*))

(deftest class-redefinition.make-instances-obsolete
  (let ((r (make-instances-obsolete 'cri-mio)))
    (list (eq r (find-class 'cri-mio))
          *cri-mio-count*
          (mapcar #'cri-mio-v *cri-mio*)
          *cri-mio-count*
          (mapcar #'cri-mio-v *cri-mio*)
          *cri-mio-count*))
  (t 0 (1 2) 2 (1 2) 2))

(deftest class-redefinition.make-instances-obsolete-class-object
  (eq (make-instances-obsolete (find-class 'cri-mio)) (find-class 'cri-mio))
  t)

;;; Accessor call sites keep an inline cache keyed on the instance layout. Warm
;;; it on a current instance, then hand it an obsolete one: it must miss and
;;; update rather than index the old slot vector.
(defclass cri-ic () ((p :initform 1 :accessor cri-ic-p)
                     (q :initform 2 :accessor cri-ic-q)))
(defun cri-ic-read-q (o) (cri-ic-q o))
(defun cri-ic-write-q (o v) (setf (cri-ic-q o) v))
(defvar *cri-ic-old* (make-instance 'cri-ic))
(setf (slot-value *cri-ic-old* 'q) 20)
(cri-ic-read-q *cri-ic-old*)
(defclass cri-ic () ((n :initform 0 :accessor cri-ic-n)
                     (q :initform 2 :accessor cri-ic-q)))
(defvar *cri-ic-new* (make-instance 'cri-ic))

(deftest class-redefinition.accessor-inline-cache
  (list (cri-ic-read-q *cri-ic-new*)   ; warm on the new layout
        (cri-ic-read-q *cri-ic-old*)   ; obsolete: must miss and update
        (cri-ic-read-q *cri-ic-new*)
        (progn (make-instances-obsolete 'cri-ic)
               (cri-ic-write-q *cri-ic-new* 5)
               (cri-ic-read-q *cri-ic-new*))
        (slot-value *cri-ic-old* 'n))
  (2 20 2 5 0))

;;; Instances held only inside collections are updated when reached.
(defclass cri-coll () ((k :initarg :k :reader cri-coll-k)))
(defvar *cri-coll-list* (list (make-instance 'cri-coll :k 1) (make-instance 'cri-coll :k 2)))
(defvar *cri-coll-vec* (vector (make-instance 'cri-coll :k 3)))
(defvar *cri-coll-hash* (let ((h (make-hash-table)))
                          (setf (gethash :a h) (make-instance 'cri-coll :k 4))
                          h))
(defclass cri-coll () ((extra :initform :e :reader cri-coll-extra)
                       (k :initarg :k :reader cri-coll-k)))

(deftest class-redefinition.collections
  (list (mapcar #'cri-coll-k *cri-coll-list*)
        (mapcar #'cri-coll-extra *cri-coll-list*)
        (cri-coll-k (aref *cri-coll-vec* 0))
        (cri-coll-extra (aref *cri-coll-vec* 0))
        (cri-coll-k (gethash :a *cri-coll-hash*))
        (cri-coll-extra (gethash :a *cri-coll-hash*)))
  ((1 2) (:e :e) 3 :e 4 :e))

;;; Redefining a superclass updates instances of a subclass.
(defclass cri-base () ((b1 :initform 1)))
(defclass cri-sub (cri-base) ((s1 :initform 10)))
(defvar *cri-sub* (make-instance 'cri-sub))
(setf (slot-value *cri-sub* 's1) 11)
(defclass cri-base () ((b0 :initform 0) (b1 :initform 1)))

(deftest class-redefinition.superclass
  (list (slot-value *cri-sub* 'b0) (slot-value *cri-sub* 'b1)
        (slot-value *cri-sub* 's1))
  (0 1 11))

;;; SLOT-BOUNDP and SLOT-MAKUNBOUND on an obsolete instance.
(defclass cri-bound () ((u) (w :initform 1)))
(defvar *cri-bound* (make-instance 'cri-bound))
(defclass cri-bound () ((new :initform 5) (u) (w :initform 1)))

(deftest class-redefinition.boundp
  (list (slot-boundp *cri-bound* 'u) (slot-boundp *cri-bound* 'w)
        (slot-boundp *cri-bound* 'new))
  (nil t t))

;;; A redefinition that changes nothing about the slots does not make the
;;; instances obsolete.
(defvar *cri-same-count* 0)
(defclass cri-same () ((a :initform 1)))
(defvar *cri-same* (make-instance 'cri-same))
(defmethod update-instance-for-redefined-class :before
    ((inst cri-same) added discarded plist &rest initargs)
  (declare (ignore added discarded plist initargs))
  (incf *cri-same-count*))
(defclass cri-same () ((a :initform 1 :reader cri-same-a)))

(deftest class-redefinition.same-shape
  (list (cri-same-a *cri-same*) *cri-same-count*)
  (1 0))
