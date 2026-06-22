;;;; CLOS evaluated by the tree-walk interpreter (%mini-eval) — no compilation.
;;;; defclass / defgeneric / defmethod / defstruct expand to runtime CLOS
;;;; intrinsics that are registered as callable functions; method bodies become
;;;; interpreted closures. Run under :interpret.
;;;; Run: dotnet run ... -- --asm compiler/cil-out.sil test/interp/clos.lisp

(defpackage :dotcl-interp-clos (:use :cl))
(in-package :dotcl-interp-clos)

(setq dotcl:*evaluator-mode* :interpret)

(defvar *pass* 0)
(defvar *fail* 0)
(defmacro expect (form expected)
  `(let ((got (eval ',form)))
     (if (equal got ,expected)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "FAIL: ~S~%  got ~S, want ~S~%" ',form got ,expected)))))

;; --- basic class + slots + accessors ---
(eval '(defclass point ()
         ((x :initarg :x :initform 0 :accessor px)
          (y :initarg :y :initform 0 :accessor py))))
(expect (px (make-instance 'point :x 3 :y 4)) 3)
(expect (py (make-instance 'point)) 0)                       ; initform
(expect (let ((p (make-instance 'point))) (setf (px p) 9) (px p)) 9)  ; setf accessor

;; --- generic dispatch ---
(eval '(defgeneric area (s)))
(eval '(defmethod area ((p point)) (* (px p) (py p))))
(expect (area (make-instance 'point :x 3 :y 4)) 12)

;; --- inheritance + call-next-method ---
(eval '(defclass point3 (point) ((z :initarg :z :initform 0 :accessor pz))))
(eval '(defmethod area ((p point3)) (+ (call-next-method) (pz p))))
(expect (area (make-instance 'point3 :x 2 :y 3 :z 100)) 106) ; 6 + 100
(expect (px (make-instance 'point3 :x 7)) 7)                  ; inherited accessor

;; --- :before / :after / :around (effective method ordering) ---
(eval '(defvar *log* nil))
(eval '(defgeneric step1 (o)))
(eval '(defmethod step1 ((o point)) (push :primary *log*) :done))
(eval '(defmethod step1 :before ((o point)) (push :before *log*)))
(eval '(defmethod step1 :after ((o point)) (push :after *log*)))
(eval '(defmethod step1 :around ((o point)) (push :around-in *log*)
         (let ((r (call-next-method))) (push :around-out *log*) r)))
(expect (progn (setf *log* nil) (list (step1 (make-instance 'point)) (reverse *log*)))
        '(:done (:around-in :before :primary :after :around-out)))

;; --- EQL specializer ---
(eval '(defgeneric classify (n)))
(eval '(defmethod classify ((n integer)) :int))
(eval '(defmethod classify ((n (eql 0))) :zero))
(expect (classify 5) :int)
(expect (classify 0) :zero)

;; --- multiple dispatch ---
(eval '(defgeneric combine (a b)))
(eval '(defmethod combine ((a integer) (b integer)) :int-int))
(eval '(defmethod combine ((a integer) (b string)) :int-str))
(expect (combine 1 2) :int-int)
(expect (combine 1 "x") :int-str)

;; --- defstruct ---
(eval '(defstruct pt3 a b c))
(expect (let ((s (make-pt3 :a 1 :b 2 :c 3))) (list (pt3-a s) (pt3-b s) (pt3-c s))) '(1 2 3))
(expect (let ((s (make-pt3 :a 1 :b 2 :c 3))) (setf (pt3-b s) 20) (pt3-b s)) 20)
(expect (pt3-p (make-pt3)) t)

;; --- next-method-p ---
(eval '(defgeneric nmp (o)))
(eval '(defmethod nmp ((o point)) (list :point (if (next-method-p) :has :none))))
(eval '(defmethod nmp ((o point3)) (list :point3 (next-method-p) (call-next-method))))
(expect (nmp (make-instance 'point3)) '(:point3 t (:point :none)))
(expect (nmp (make-instance 'point)) '(:point :none))

;; --- call-next-method with explicit args ---
(eval '(defgeneric cnm-args (a b)))
(eval '(defmethod cnm-args ((a integer) (b integer))
         (list :int (call-next-method (* a 10) (* b 10)))))
(eval '(defmethod cnm-args ((a number) (b number)) (list :num a b)))
(expect (cnm-args 1 2) '(:int (:num 10 20)))

;; --- multi-level :around (around at base + derived) ---
(eval '(defvar *al* nil))
(eval '(defgeneric ar (o)))
(eval '(defmethod ar ((o point)) (push :p-prim *al*) :v))
(eval '(defmethod ar :around ((o point)) (push :p-around *al*) (call-next-method)))
(eval '(defmethod ar :around ((o point3)) (push :p3-around *al*) (call-next-method)))
(expect (progn (setf *al* nil) (list (ar (make-instance 'point3)) (reverse *al*)))
        '(:v (:p3-around :p-around :p-prim)))

;; --- slot-value / slot-boundp / with-slots / with-accessors ---
(eval '(defclass cell () ((v :initarg :v :initform 0))))
(expect (slot-value (make-instance 'cell :v 7) 'v) 7)
(expect (let ((c (make-instance 'cell))) (setf (slot-value c 'v) 5) (slot-value c 'v)) 5)
(expect (slot-boundp (make-instance 'cell) 'v) t)
(expect (let ((c (make-instance 'cell :v 3))) (with-slots (v) c (setf v (* v v)) v)) 9)
(eval '(defclass acc () ((n :initarg :n :accessor an :initform 0))))
(expect (let ((o (make-instance 'acc :n 4)))
          (with-accessors ((nn an)) o (setf nn (+ nn 1)) nn)) 5)

;; --- initialize-instance :after ---
(eval '(defclass auto () ((raw :initarg :raw :initform 0) (doubled :accessor doubled))))
(eval '(defmethod initialize-instance :after ((o auto) &key)
         (setf (doubled o) (* 2 (slot-value o 'raw)))))
(expect (doubled (make-instance 'auto :raw 21)) 42)

;; --- custom print-object (dispatched through interpreted method) ---
(eval '(defclass named () ((nm :initarg :nm))))
(eval '(defmethod print-object ((o named) stream)
         (format stream "#<N:~A>" (slot-value o 'nm))))
(expect (with-output-to-string (s) (print-object (make-instance 'named :nm 'bob) s))
        "#<N:BOB>")

;; --- defstruct :include / BOA constructor / :conc-name ---
(eval '(defstruct base-s (a 1) (b 2)))
(eval '(defstruct (derived-s (:include base-s)) (c 3)))
(expect (let ((d (make-derived-s :a 10 :c 30)))
          (list (base-s-a d) (base-s-b d) (derived-s-c d))) '(10 2 30))
(expect (base-s-p (make-derived-s)) t)
(eval '(defstruct (widget (:conc-name w-) (:constructor build-widget (size))) size))
(expect (w-size (build-widget 99)) 99)

;; --- multiple inheritance / CPL ordering ---
(eval '(defclass a-cls () ()))
(eval '(defclass b-cls () ()))
(eval '(defclass ab-cls (a-cls b-cls) ()))
(eval '(defgeneric who (o)))
(eval '(defmethod who ((o a-cls)) :a))
(eval '(defmethod who ((o b-cls)) :b))
(expect (who (make-instance 'ab-cls)) :a)

;; --- :default-initargs ---
(eval '(defclass with-di () ((s :initarg :s :accessor di-s)) (:default-initargs :s 77)))
(expect (di-s (make-instance 'with-di)) 77)
(expect (di-s (make-instance 'with-di :s 5)) 5)

;; --- class-of / class-name / find-class / typep ---
(expect (class-name (class-of (make-instance 'point))) 'point)
(expect (eq (find-class 'point) (class-of (make-instance 'point))) t)
(expect (typep (make-instance 'point3) 'point) t)

;; --- slot introspection / allocation / copy intrinsics (%-intrinsics) ---
(eval '(defclass islot () ((a :initarg :a :initform 0) (b :initarg :b))))
(expect (list (slot-exists-p (make-instance 'islot) 'a)
              (slot-exists-p (make-instance 'islot) 'zzz)) '(t nil))
(expect (slot-boundp (make-instance 'islot :a 1 :b 2) 'b) t)
(expect (slot-boundp (make-instance 'islot) 'b) nil)        ; no initform on b
(expect (let ((o (make-instance 'islot :a 5)))
          (slot-makunbound o 'a) (slot-boundp o 'a)) nil)
(expect (slot-boundp (allocate-instance (find-class 'islot)) 'a) nil) ; no initforms run
;; copy-structure / struct typep (slot names avoid the <name>-P predicate clash)
(eval '(defstruct istr xx yy))
(expect (let* ((s (make-istr :xx 1 :yy 2)) (c (copy-structure s)))
          (list (istr-xx c) (istr-yy c) (eq s c))) '(1 2 nil))
(expect (list (typep (make-istr) 'istr) (typep 7 'istr)) '(t nil))

(format t "~%=== interp CLOS: ~D PASSED  ~D FAILED ===~%" *pass* *fail*)
(dotcl:quit (if (zerop *fail*) 0 1))
