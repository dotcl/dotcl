;;; A compiled structure TYPEP tests only the representations the type can
;;; have: the array-backed LispStruct, LispStruct1 .. LispStruct4, or a set of
;;; them for an :INCLUDE parent. Anything the compile-time subtree does not
;;; name -- a child defined later, a redefined structure -- still reaches the
;;; type-name walk through the general fallback.

(setf dotcl:*save-sil* t)

(defstruct %sts-0)
(defstruct %sts-1 a)
(defstruct %sts-2 a b)
(defstruct %sts-3 a b c)
(defstruct %sts-4 a b c d)
(defstruct %sts-5 a b c d e)
(defstruct %sts-raw (n 0 :type fixnum) x)

(defun %sts-0p (x) (typep x '%sts-0))
(defun %sts-1p (x) (typep x '%sts-1))
(defun %sts-2p (x) (typep x '%sts-2))
(defun %sts-3p (x) (typep x '%sts-3))
(defun %sts-4p (x) (typep x '%sts-4))
(defun %sts-5p (x) (typep x '%sts-5))
(defun %sts-rawp (x) (typep x '%sts-raw))
(defun %sts-1-read (x) (%sts-1-a x))
(defun %sts-raw-read (x) (%sts-raw-n x))

(deftest struct-typep-specialize.leaves
  (list (%sts-1p (make-%sts-1 :a 1))
        (%sts-1p (make-%sts-2))
        (%sts-2p (make-%sts-2))
        (%sts-3p (make-%sts-3))
        (%sts-4p (make-%sts-4))
        (%sts-5p (make-%sts-5))
        (%sts-rawp (make-%sts-raw :n 1))
        (%sts-0p (make-%sts-0))
        (%sts-1p 3)
        (%sts-1p (list 1)))
  (t nil t t t t t t nil nil))

;;; A parent whose children have different slot counts.

(defstruct %sts-p a)
(defstruct (%sts-p2 (:include %sts-p)) b)
(defstruct (%sts-p3 (:include %sts-p)) b c)
(defstruct (%sts-p5 (:include %sts-p)) b c d e)

(defun %sts-pp (x) (typep x '%sts-p))
(defun %sts-p2p (x) (typep x '%sts-p2))
(defun %sts-p-read (x) (%sts-p-a x))

(deftest struct-typep-specialize.parent
  (let ((p (make-%sts-p :a 1))
        (p2 (make-%sts-p2 :a 2 :b 3))
        (p3 (make-%sts-p3 :a 4 :b 5 :c 6))
        (p5 (make-%sts-p5 :a 7 :b 8 :c 9 :d 10 :e 11)))
    (list (mapcar #'%sts-pp (list p p2 p3 p5 (make-%sts-2) 4))
          (mapcar #'%sts-p2p (list p p2 p3 p5))
          (mapcar #'%sts-p-read (list p p2 p3 p5))))
  ((t t t t nil nil) (nil t nil nil) (1 2 4 7)))

;;; The child is defined after the function that tests the parent was compiled:
;;; its representation is not in the compiled mask, so the general fallback has
;;; to find it. The typed accessor takes its slow path for the same reason.

(defstruct %sts-late a)

(defun %sts-latep (x) (typep x '%sts-late))
(defun %sts-late-read (x) (%sts-late-a x))

(defstruct (%sts-late-child (:include %sts-late)) b c)

(deftest struct-typep-specialize.child-defined-later
  (let ((c (make-%sts-late-child :a 1 :b 2 :c 3)))
    (list (%sts-latep c) (%sts-late-read c) (%sts-latep 4)))
  (t 1 nil))

;;; The same, through the typed entry's argument check.

(defstruct %sts-arg a)

(defun %sts-arg-read (x)
  (declare (type %sts-arg x))
  (%sts-arg-a x))

(defstruct (%sts-arg-child (:include %sts-arg)) b c)

(deftest struct-typep-specialize.arg-check-child-defined-later
  (list (%sts-arg-read (make-%sts-arg :a 1))
        (%sts-arg-read (make-%sts-arg-child :a 2 :b 3 :c 4)))
  (1 2))

;;; Redefining after the TYPEP was compiled leaves inline instances from the
;;; old definition and array-backed ones from the new one.

(defstruct %sts-redef a)

(defvar *%sts-redef-old* (make-%sts-redef :a 1))
(defun %sts-redefp (x) (typep x '%sts-redef))

(eval '(defstruct %sts-redef a b))

(defvar *%sts-redef-new* (make-%sts-redef :a 2 :b 3))

(deftest struct-typep-specialize.redefined-after-compile
  (list (%sts-redefp *%sts-redef-old*)
        (%sts-redefp *%sts-redef-new*)
        (%sts-redefp (make-%sts-1 :a 1)))
  (t t nil))

;;; A structure already redefined when the TYPEP is compiled has no known
;;; representation for its older instances, so the general test is emitted.

(eval '(defstruct %sts-redef2 a))
(eval '(defstruct %sts-redef2 a b))

(defun %sts-redef2p (x) (typep x '%sts-redef2))

(deftest struct-typep-specialize.redefined-before-compile
  (%sts-redef2p (make-%sts-redef2 :a 1 :b 2))
  t)

;;; Raw slots keep the whole subtree in the array-backed representation.

(defstruct %sts-raw-p (n 0 :type fixnum) a)
(defstruct (%sts-raw-child (:include %sts-raw-p)) b)

(defun %sts-raw-pp (x) (typep x '%sts-raw-p))

(deftest struct-typep-specialize.raw-parent
  (list (%sts-raw-pp (make-%sts-raw-p :n 1 :a 2))
        (%sts-raw-pp (make-%sts-raw-child :n 3 :a 4 :b 5))
        (%sts-raw-pp (make-%sts-1 :a 6)))
  (t t nil))

;;; An array-backed type redefined before a later child can have an inline child.

(defstruct %sts-arr (n 0 :type fixnum) (n2 0 :type fixnum) (n3 0 :type fixnum)
  (n4 0 :type fixnum) (n5 0 :type fixnum) a)
(defun %sts-arrp (x) (typep x '%sts-arr))
(defvar *%sts-arr-old* (make-%sts-arr :n 1 :a 2))

(eval '(defstruct %sts-arr a))
(defstruct (%sts-arr-child (:include %sts-arr)) b)

(deftest struct-typep-specialize.array-parent-redefined-then-child
  (list (%sts-arrp *%sts-arr-old*)
        (%sts-arrp (make-%sts-arr :a 3))
        (%sts-arrp (make-%sts-arr-child :a 4 :b 5))
        (%sts-arrp (make-%sts-1 :a 6)))
  (t t t nil))

;;; The same for a long structure redefined to a short one.

(defstruct %sts-many a b c d e)
(defun %sts-manyp (x) (typep x '%sts-many))
(defvar *%sts-many-old* (make-%sts-many :a 1))

(eval '(defstruct %sts-many a b))
(defstruct (%sts-many-child (:include %sts-many)) c)

(deftest struct-typep-specialize.long-parent-redefined-then-child
  (list (%sts-manyp *%sts-many-old*)
        (%sts-manyp (make-%sts-many :a 2 :b 3))
        (%sts-manyp (make-%sts-many-child :a 4 :b 5 :c 6)))
  (t t t))

;;; An inline type redefined to a raw one leaves a later child outside the mask.

(defstruct %sts-mask a b)
(defun %sts-maskp (x) (typep x '%sts-mask))
(defvar *%sts-mask-old* (make-%sts-mask :a 1 :b 2))

(eval '(defstruct %sts-mask (n 0 :type fixnum) a b))
(defstruct (%sts-mask-child (:include %sts-mask)) c)

(deftest struct-typep-specialize.inline-parent-redefined-then-child
  (list (%sts-maskp *%sts-mask-old*)
        (%sts-maskp (make-%sts-mask :n 3 :a 4 :b 5))
        (%sts-maskp (make-%sts-mask-child :n 6 :a 7 :b 8 :c 9)))
  (t t t))

;;; Typed accessors and argument checks use the same masks.

(defstruct %sts-rarg (n 0 :type fixnum) a)
(defun %sts-rarg-read (x)
  (declare (type %sts-rarg x))
  (%sts-rarg-a x))
(defstruct (%sts-rarg-child (:include %sts-rarg)) b c)

(deftest struct-typep-specialize.raw-masked-read-child
  (list (%sts-rarg-read (make-%sts-rarg :n 1 :a 2))
        (%sts-rarg-read (make-%sts-rarg-child :n 3 :a 4 :b 5 :c 6)))
  (2 4))

;;; TYPEP evaluated as a form answers the same compiled and interpreted.

(defvar *%sts-eval-obj* nil)

(defun %sts-eval-typep (obj type)
  (let ((*%sts-eval-obj* obj))
    (eval `(typep *%sts-eval-obj* ',type))))

(deftest struct-typep-specialize.evaluated
  (let ((p (make-%sts-p :a 1))
        (p2 (make-%sts-p2 :a 2 :b 3))
        (late (make-%sts-late-child :a 4 :b 5 :c 6))
        (raw (make-%sts-raw :n 7 :x 8)))
    (list (%sts-eval-typep p '%sts-p)
          (%sts-eval-typep p2 '%sts-p)
          (%sts-eval-typep p2 '%sts-p2)
          (%sts-eval-typep late '%sts-late)
          (%sts-eval-typep raw '%sts-raw)
          (%sts-eval-typep p '%sts-p2)
          (%sts-eval-typep 3 '%sts-p)))
  (t t t t t nil nil))

;;; What the emitted code asks: one class compare for a leaf, the exact array
;;; compare when the subtree is array-backed, and the general test when the
;;; structure was redefined before compilation.

(defun %sts-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %sts-mentioned-p (needle fn)
  (and (search needle (%sts-sil fn)) t))

(deftest-emitting-only struct-typep-specialize.emits-class-compares
  (list (%sts-mentioned-p "LispStruct.TypeNameOfKnown" #'%sts-1p)
        (%sts-mentioned-p "LispStruct.TypeNameOfKnown" #'%sts-pp)
        (%sts-mentioned-p "LispStruct.TypeNameOfRawInline" #'%sts-rawp)
        (%sts-mentioned-p "LispStruct.TypeNameOfStruct" #'%sts-5p)
        (%sts-mentioned-p "LispStruct.TypeNameOfStruct" #'%sts-arrp)
        (and (%sts-mentioned-p "LispStruct.TypeNameOf)" #'%sts-redef2p)
             (not (%sts-mentioned-p "TypeNameOfKnown" #'%sts-redef2p))
             (not (%sts-mentioned-p "TypeNameOfStruct" #'%sts-redef2p))
             t))
  (t t t t t t))

(deftest-emitting-only struct-typep-specialize.emits-masked-reads
  (list (%sts-mentioned-p "Runtime.StructRefITMasked" #'%sts-1-read)
        (%sts-mentioned-p "Runtime.StructRefITMasked" #'%sts-raw-read)
        (%sts-mentioned-p "Runtime.StructRefITMasked" #'%sts-rarg-read)
        (%sts-mentioned-p "Runtime.CheckArgStructSymKnown" #'%sts-arg-read)
        (%sts-mentioned-p "Runtime.CheckArgStructSymKnown" #'%sts-rarg-read))
  (t t t t t))
