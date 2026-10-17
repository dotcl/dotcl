;;; (TYPEP x 'SPEC) with a constant OR / AND / NOT / EQL / MEMBER specifier, or
;;; a DEFTYPE known when the call is compiled, is compiled as tests of the
;;; parts. The answers have to match TYPEP of the same specifier given at run
;;; time, in value and in branch position.

(setf dotcl:*save-sil* t)

(defstruct tcc-s a)
(defstruct (tcc-sub (:include tcc-s)) b)
(deftype tcc-designator () '(or (eql t) tcc-s))
(deftype tcc-inner () '(member :a :b 3))
(deftype tcc-nested () '(or tcc-inner (and integer (not (eql 3)))))
(deftype tcc-small () '(integer 0 (10)))
(deftype tcc-alias () 'tcc-s)

(defparameter *tcc-objects*
  (list nil t :a :b :c 3 4 -1 3.0 "x" #\a (make-tcc-s) (make-tcc-sub) (list 1)))

(defmacro tcc-pair (spec)
  `(cons (lambda (o) (typep o ',spec))
         (lambda (o) (if (typep o ',spec) :yes :no))))

(defparameter *tcc-cases*
  (list (cons 'tcc-designator (tcc-pair tcc-designator))
        (cons 'tcc-inner (tcc-pair tcc-inner))
        (cons 'tcc-nested (tcc-pair tcc-nested))
        (cons 'tcc-small (tcc-pair tcc-small))
        (cons 'tcc-alias (tcc-pair tcc-alias))
        (cons '(or null tcc-s) (tcc-pair (or null tcc-s)))
        (cons '(and tcc-s (not tcc-sub)) (tcc-pair (and tcc-s (not tcc-sub))))
        (cons '(eql 3) (tcc-pair (eql 3)))
        (cons '(member nil 3 #\a) (tcc-pair (member nil 3 #\a)))
        (cons '(not tcc-designator) (tcc-pair (not tcc-designator)))))

(defun tcc-runtime-typep (o spec) (typep o spec))

(defun %tcc-mismatches ()
  (let ((bad '()))
    (dolist (c *tcc-cases*)
      (destructuring-bind (spec value . branch) c
        (dolist (o *tcc-objects*)
          (let ((want (tcc-runtime-typep o spec)))
            (unless (and (eq (funcall value o) (and want t))
                         (eq (funcall branch o) (if want :yes :no)))
              (push (list spec o) bad))))))
    bad))

(deftest typep-constant-compound.answers
  (%tcc-mismatches)
  nil)

(deftest typep-constant-compound.value-is-boolean
  (list (typep t 'tcc-designator) (typep 3 'tcc-inner) (typep :c 'tcc-inner)
        (typep 5 '(and integer (not (eql 3)))))
  (t t nil t))

(defun %tcc-dp (o) (if (typep o 'tcc-designator) 1 0))

(deftest-emitting-only typep-constant-compound.deftype-not-asked-at-run-time
  (let ((sil (%flat-text (dotcl:function-sil #'%tcc-dp))))
    (list (and (search "TCC-DESIGNATOR" sil) t)
          (and (search "Runtime.Typep" sil) t)))
  (nil nil))

(deftest typep-constant-compound.evaluates-object-once
  (let ((n 0))
    (list (typep (progn (incf n) 3) '(or (eql 1) (eql 2) (member 4 3)))
          n))
  (t 1))
