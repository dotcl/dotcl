;;; A LET binding a variable to a structure built in place, whose slots are
;;; only read (or set) and whose type is only tested, keeps the slot values in
;;; variables and builds no structure. Anything else the variable is used for
;;; keeps the structure.

(setf dotcl:*save-sil* t)

(declaim (inline %sls-pair))
(defstruct (%sls-base (:constructor nil) (:predicate nil) (:copier nil)))
(defstruct (%sls-pair (:include %sls-base) (:constructor %sls-pair (a b)) (:copier nil))
  (a nil :type t :read-only t)
  (b nil :type t :read-only t))
(defstruct (%sls-other (:constructor %sls-other (a))) a)
(defstruct %sls-kw x (y 10))
(declaim (inline %sls-fix))
(defstruct (%sls-fix (:constructor %sls-fix (n))) (n 0 :type fixnum :read-only t))

(defun %sls-match (x y)
  (let ((g (%sls-pair x y)))
    (declare (dynamic-extent g))
    (let ((m g))
      (declare (ignorable m) (type %sls-base m))
      (cond ((and (typep m '%sls-pair) (eql :lt (%sls-pair-a m)) (eql :lt (%sls-pair-b m))) 1)
            ((and (typep m '%sls-pair) (eql :gt (%sls-pair-a m)) (eql :gt (%sls-pair-b m))) 2)
            ((typep m '%sls-other) 3)
            (t 0)))))

(defun %sls-kw-sum (x) (let ((p (make-%sls-kw :x x))) (+ (%sls-kw-x p) (%sls-kw-y p))))
(defun %sls-kw-set (x) (let ((p (make-%sls-kw :x x))) (setf (%sls-kw-x p) 5) (incf (%sls-kw-y p)) (list (%sls-kw-x p) (%sls-kw-y p))))
(defun %sls-order ()
  (let ((log '()))
    (let ((p (%sls-pair (progn (push 1 log) :a) (progn (push 2 log) :b))))
      (list (%sls-pair-b p) (%sls-pair-a p) (reverse log)))))
(defun %sls-escape () (let ((p (%sls-pair 1 2))) p))
(defun %sls-escape-list () (let ((p (%sls-pair 1 2))) (list (%sls-pair-a p) (%sls-pair-p p))))
(defun %sls-closure () (let ((p (%sls-pair 1 2))) (lambda () (%sls-pair-b p))))
(defun %sls-fix-read (v) (let ((p (%sls-fix v))) (%sls-fix-n p)))
(defun %sls-mv (x) (let ((p (%sls-pair x (floor 7 2)))) (values (%sls-pair-a p) (%sls-pair-b p))))

(deftest struct-let-scalar.values
  (list (%sls-match :lt :lt) (%sls-match :gt :gt) (%sls-match :lt :gt)
        (%sls-kw-sum 5) (%sls-kw-set 1) (%sls-order)
        (%sls-pair-p (%sls-escape)) (%sls-escape-list)
        (funcall (%sls-closure)) (%sls-fix-read 7)
        (multiple-value-list (%sls-mv 3)))
  (1 2 0 15 (5 11) (:b :a (1 2)) t (1 t) 2 7 (3 3)))

(deftest struct-let-scalar.type-check
  (handler-case (%sls-fix-read "x") (type-error () :type-error))
  :type-error)

(deftest-compiled-only struct-let-scalar.no-struct
  (flet ((has (fn) (and (search "MakeStruct" (%flat-text (dotcl:function-sil fn))) t)))
    (list (has #'%sls-match) (has #'%sls-kw-sum) (has #'%sls-order)
          (has #'%sls-escape) (has #'%sls-escape-list)))
  (nil nil nil t t))

;;; A slot declared :TYPE T needs no check where it is stored.
(defstruct (%sls-tslot (:constructor %sls-tslot (a))) (a nil :type t))
(defun %sls-tslot-set (o x) (setf (%sls-tslot-a o) x))
(defun %sls-tslot-match (x)
  (let ((p (%sls-tslot x))) (list (%sls-tslot-a p) (typep p '%sls-tslot))))

(deftest struct-let-scalar.t-slot
  (list (%sls-tslot-match 4)
        (let ((o (%sls-tslot 1))) (%sls-tslot-set o :x) (%sls-tslot-a o)))
  ((4 t) :x))

(deftest-compiled-only struct-let-scalar.t-slot-unchecked
  (list (search "CheckSlotType" (%flat-text (dotcl:function-sil #'%sls-tslot-set)))
        (search "CheckSlotType" (%flat-text (dotcl:function-sil #'%sls-tslot-match))))
  (nil nil))
