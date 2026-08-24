;;; A DEFSTRUCT keyword constructor called with constant keywords is built at the
;;; call site (%MAKE-STRUCT of the slot values) instead of being called: the &key
;;; call is what costs, in an argument array plus the keyword scan (~140 B a call,
;;; make-pt :x 1 :y 2 went 208 -> 112 B). The constructor keeps its ordinary &key
;;; definition, so everything the rewrite declines still works through it.
;;;
;;; What must not change is when initforms run and in what order argument forms
;;; are evaluated, which is what most of these assert.

(defstruct skc a b)

(deftest struct-kw-ctor-basic
  (let ((s (make-skc :a 1 :b 2)))
    (list (skc-a s) (skc-b s) (typep s 'skc)))
  (1 2 t))

(deftest struct-kw-ctor-omitted
  (let ((s (make-skc :b 2)))
    (list (skc-a s) (skc-b s)))
  (nil 2))

(deftest struct-kw-ctor-none
  (let ((s (make-skc)))
    (list (skc-a s) (skc-b s)))
  (nil nil))

;;; Argument forms are evaluated left to right as written, even when the keywords
;;; are not in slot order (CLHS 3.1.2.1.2.3).
(defvar *skc-log* nil)
(deftest struct-kw-ctor-evaluation-order
  (let ((*skc-log* nil))
    (let ((s (make-skc :b (progn (push :b *skc-log*) 1)
                       :a (progn (push :a *skc-log*) 2))))
      (list (reverse *skc-log*) (skc-a s) (skc-b s))))
  ((:b :a) 2 1))

;;; A non-constant initform must keep running in the constructor, once per call
;;; that omits the slot.
(defvar *skc-counter* 0)
(defstruct skc-nc (v (incf *skc-counter*)) (w 7))

(deftest struct-kw-ctor-nonconstant-initform
  (let ((*skc-counter* 0))
    (let ((a (make-skc-nc))
          (b (make-skc-nc :w 1))
          (c (make-skc-nc :v 99 :w 2)))
      (list (skc-nc-v a) (skc-nc-v b) (skc-nc-v c) (skc-nc-w b) *skc-counter*)))
  (1 2 99 1 2))

;;; A repeated keyword: the leftmost value wins and every value form is still
;;; evaluated (CLHS 3.4.1.4).
(defvar *skc-dups* nil)
(deftest struct-kw-ctor-duplicate-keyword
  (let ((*skc-dups* nil))
    (let ((s (make-skc :a (progn (push 1 *skc-dups*) 1)
                       :a (progn (push 2 *skc-dups*) 2))))
      (list (skc-a s) (reverse *skc-dups*))))
  (1 (1 2)))

;;; Keywords that are not constant at the call site, and APPLY, go through the
;;; constructor itself.
(defun %skc-apply (k v) (apply #'make-skc (list k v)))
(deftest struct-kw-ctor-runtime-keyword
  (list (skc-a (%skc-apply :a 5)) (skc-b (%skc-apply :b 6)))
  (5 6))

(deftest struct-kw-ctor-unknown-keyword
  (handler-case (progn (funcall #'make-skc :zz 1) :no-error)
    (error () :error))
  :error)

;;; :include — the inherited slots are part of the same slot order.
(defstruct (skc-child (:include skc)) c)
(deftest struct-kw-ctor-include
  (let ((s (make-skc-child :a 1 :c 3)))
    (list (skc-a s) (skc-b s) (skc-child-c s) (typep s 'skc)))
  (1 nil 3 t))

;;; Typed structures build a list/vector rather than a structure object; they are
;;; not rewritten, and must keep working.
(defstruct (skc-vec (:type vector)) p q)
(deftest struct-kw-ctor-typed
  (coerce (make-skc-vec :p 1 :q 2) (quote list))
  (1 2))

;;; A named keyword constructor is rewritten like the default one; a BOA
;;; constructor of the same structure is untouched.
(defstruct (skc-cc (:constructor make-skc-kw)
                   (:constructor make-skc-boa (a b)))
  a b)
(deftest struct-kw-ctor-custom-names
  (list (skc-cc-a (make-skc-kw :a 1 :b 2))
        (skc-cc-b (make-skc-boa 1 2)))
  (1 2))

;;; Redefining the constructor decides what the call means from then on.
(defstruct skc-rd x)
(defun make-skc-rd (&key x) (list :redefined x))
(deftest struct-kw-ctor-redefined
  (make-skc-rd :x 1)
  (:redefined 1))

;;; A local function of the same name shadows the constructor.
(deftest struct-kw-ctor-flet-shadow
  (flet ((make-skc (&key a b) (list :flet a b)))
    (make-skc :a 1 :b 2))
  (:flet 1 2))

;;; NOTINLINE is the standard way to ask for the call itself (CLHS 3.2.2.1.1).
(deftest struct-kw-ctor-notinline
  (locally (declare (notinline make-skc))
    (let ((s (make-skc :a 1 :b 2)))
      (list (skc-a s) (skc-b s))))
  (1 2))

;;; The structure a rewritten call builds is an ordinary instance: copier,
;;; setters and printing all work on it.
(deftest struct-kw-ctor-instance-is-ordinary
  (let* ((s (make-skc :a 1 :b 2))
         (c (copy-skc s)))
    (setf (skc-a c) 9)
    (list (skc-a s) (skc-a c) (skc-b c) (equalp s (make-skc :a 1 :b 2))))
  (1 9 2 t))

;;; Each instance owns its slot storage. The values handed to the constructor
;;; become that storage rather than being copied into it, so two instances built
;;; from equal arguments must still be independent — through the rewritten call
;;; site and through the constructor itself.
(deftest struct-slot-storage-not-shared
  (let ((a (make-skc :a 1 :b 2))
        (b (make-skc :a 1 :b 2)))
    (setf (skc-a a) 9)
    (list (skc-a a) (skc-a b)))
  (9 1))

(deftest struct-slot-storage-not-shared-dynamic
  (let ((a (apply #'make-skc (list :a 1 :b 2)))
        (b (apply #'make-skc (list :a 1 :b 2))))
    (setf (skc-b a) 9)
    (list (skc-b a) (skc-b b)))
  (9 2))
