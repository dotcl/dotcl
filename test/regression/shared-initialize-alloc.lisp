;;; SHARED-INITIALIZE does not allocate a duplicate-tracking set it never uses.
;;;
;;; It built a HashSet<int> on every call to record which slots an initarg had
;;; already written -- so that the leftmost initarg wins for a duplicated key.
;;; With no initargs the loop that fills it never runs, and the set was ~64 B
;;; allocated to stay empty. Every instance the initialization protocol touches
;;; goes through here, so it was 64 B on each one:
;;;
;;;   shared-initialize <inst> t      120.1 -> 56.1 B/op
;;;   initialize-instance <inst>      112.1 -> 48.2 B/op
;;;   (make-instance 'c) with an
;;;     (initialize-instance :after)  208.2 -> 144.2 B/op  (= the no-method cost)
;;;
;;; The behaviour it guards is what these tests pin: the set is now built on the
;;; first initarg that writes a slot, so the duplicate rule has to keep working
;;; exactly as before.

(defclass sia-c ()
  ((a :initarg :a :initarg :also-a :initform :unset :accessor sia-a)
   (b :initarg :b :initform :unset :accessor sia-b)))

;;; The rule the set exists for: with two initargs naming the SAME slot, the
;;; leftmost wins (CLHS 7.1.4).
(deftest shared-initialize-alloc.leftmost-duplicate-initarg-wins
  (list (sia-a (make-instance 'sia-c :a 1 :also-a 2))
        (sia-a (make-instance 'sia-c :also-a 2 :a 1))
        ;; the same key repeated is the same rule
        (sia-a (make-instance 'sia-c :a :first :a :second)))
  (1 2 :first))

;;; Initargs still override, and initforms still fill what no initarg named.
(deftest shared-initialize-alloc.initargs-and-initforms
  (let ((i (make-instance 'sia-c :b 7)))
    (list (sia-a i) (sia-b i)))
  (:unset 7))

;;; SHARED-INITIALIZE called directly with no initargs -- the path that used to
;;; allocate the set with nothing to put in it.
;;;
;;; CLHS 7.1.4: SLOT-NAMES selects slots to fill from their initforms, and only
;;; slots that are still UNBOUND are filled. A slot MAKE-INSTANCE already gave a
;;; value keeps it; this is not SBCL-style reinitialisation.
(deftest shared-initialize-alloc.direct-call-with-no-initargs
  (let ((i (make-instance 'sia-c :a 1 :b 2)))
    (shared-initialize i t)                 ; t = every slot, no initargs
    ;; both slots are bound, so both keep their values
    (list (sia-a i) (sia-b i)))
  (1 2))

;;; ...and an unbound slot named by SLOT-NAMES does get its initform.
(deftest shared-initialize-alloc.unbound-slot-gets-its-initform
  (let ((i (make-instance 'sia-c :a 1 :b 2)))
    (slot-makunbound i 'a)
    (shared-initialize i t)
    (list (sia-a i) (sia-b i)))
  (:unset 2))

(deftest shared-initialize-alloc.direct-call-with-initargs
  (let ((i (make-instance 'sia-c)))
    (shared-initialize i nil :a 3 :also-a 4 :b 5)
    (list (sia-a i) (sia-b i)))
  (3 5))

;;; SLOT-NAMES selects WHICH unbound slots are filled; one left out stays
;;; unbound. The loop the set guards runs either way.
(deftest shared-initialize-alloc.slot-names-selects
  (let ((i (make-instance 'sia-c :a 1 :b 2)))
    (slot-makunbound i 'a)
    (slot-makunbound i 'b)
    (shared-initialize i '(a))
    (list (sia-a i) (notnot (slot-boundp i 'b))))
  (:unset nil))

;;; An :after method on initialize-instance is the shape that made this visible;
;;; it must still run, and see the slots already filled.
(defclass sia-d () ((v :initarg :v :initform 0 :accessor sia-v)))
(defvar *sia-seen* nil)
(defmethod initialize-instance :after ((s sia-d) &key)
  (push (sia-v s) *sia-seen*))

(deftest shared-initialize-alloc.after-method-sees-filled-slots
  (progn (setq *sia-seen* nil)
         (make-instance 'sia-d :v 9)
         (make-instance 'sia-d)
         (reverse *sia-seen*))
  (9 0))
