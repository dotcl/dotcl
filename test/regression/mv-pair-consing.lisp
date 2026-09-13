;;; Publishing two values does not build an array.
;;;
;;; MULTIPLE-VALUES.SETPAIR keeps a two-value result in two fields instead of a
;;; LispObject[2], and MULTIPLE-VALUES.GET materialises an array only when
;;; something actually asks for one. SETPAIR existed but 71 runtime entries still
;;; called the PARAMS overload, so every SUBTYPEP, INTERN, FIND-SYMBOL, READ-LINE
;;; and most of TYPE.cs consed 40 bytes per call to publish a value the caller
;;; usually discards.
;;;
;;; The tests that matter are the ones that ask for the second value: an entry
;;; converted by mistake would still return the right primary and be invisible.

(deftest mv-pair.subtypep-two-values
  (list (multiple-value-list (subtypep 'integer 'number))
        (multiple-value-list (subtypep 'integer 'string))
        ;; The undecidable answer is (NIL NIL), which is the pair whose second
        ;; value carries all the information.
        (multiple-value-list (subtypep 'nonexistent-type-xyz 'integer)))
  ((t t) (nil t) (nil nil)))

(deftest mv-pair.intern-two-values
  (let ((p (or (find-package "MV-PAIR-TEST")
               (make-package "MV-PAIR-TEST" :use nil))))
    (list (multiple-value-list (nth-value 1 (intern "FRESH-ONE" p)))
          (progn (intern "FRESH-ONE" p)
                 (nth-value 1 (intern "FRESH-ONE" p)))
          (progn (export (intern "SHOWN" p) p)
                 (nth-value 1 (intern "SHOWN" p)))))
  ((nil) :internal :external))

(deftest mv-pair.find-symbol-two-values
  (let ((p (find-package "MV-PAIR-TEST")))
    (list (multiple-value-list (find-symbol "NO-SUCH-SYMBOL-HERE" p))
          (nth-value 1 (find-symbol "SHOWN" p))
          (nth-value 1 (find-symbol "FRESH-ONE" p))))
  ((nil nil) :external :internal))

(deftest mv-pair.read-line-two-values
  (with-input-from-string (s "a
b")
    (list (multiple-value-list (read-line s))
          (multiple-value-list (read-line s))
          (multiple-value-list (read-line s nil :eof))))
  (("a" nil) ("b" t) (:eof t)))

;;; SLOT-BOUNDP publishes exactly one value, and it has to stay one: SLOT-MISSING
;;; may legally return (values nil x), and those extra values must not leak out.
(defclass mvp-c () ((a :initarg :a)))

(deftest mv-pair.slot-boundp-one-value
  (let ((o (make-instance 'mvp-c)))
    (list (multiple-value-list (slot-boundp o 'a))
          (progn (setf (slot-value o 'a) 1)
                 (multiple-value-list (slot-boundp o 'a)))))
  ((nil) (t)))

;;; A pair state has to survive the things that read it in other shapes.
(deftest mv-pair.other-readers-see-the-pair
  (list (multiple-value-bind (a b) (subtypep 'integer 'number) (list a b))
        (multiple-value-call #'list (subtypep 'integer 'number))
        (nth-value 1 (subtypep 'integer 'number))
        ;; Single-value position keeps only the primary.
        (let ((x (subtypep 'integer 'number))) x))
  ((t t) (t t) t t))

;;; The point of the change.
(defvar *mvp-sink* nil)

(defun %mvp-subtypep (n)
  (declare (fixnum n))
  (do ((i 0 (1+ i))) ((= i n) *mvp-sink*)
    (declare (fixnum i))
    (setq *mvp-sink* (subtypep 'integer 'number))))

;; Compiled-only, like the other consing assertions: an emit-free build has no
;; emitted call site for the measurement to be about.
(deftest-compiled-only mv-pair.subtypep-allocates-nothing
  (< (bytes-per-op #'%mvp-subtypep) 1)
  t)
