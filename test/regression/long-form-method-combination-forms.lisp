;;; The effective method form a long-form DEFINE-METHOD-COMBINATION returns is
;;; Lisp code (CLHS 7.6.6.2): any form may surround the CALL-METHOD forms, and
;;; the variables of the :ARGUMENTS option stand for forms that evaluate to the
;;; generic function's arguments. dotcl used to interpret only PROGN, VECTOR,
;;; QUOTE and CALL-METHOD and return any other form as data, so a body of
;;; `(list ,@...) made the generic function return (LIST (CALL-METHOD ...)).

;;; A plain LIST around the methods.
(define-method-combination lfmc-list ()
  ((plain ()))
  `(list ,@(mapcar (lambda (m) `(call-method ,m)) plain)))
(defgeneric lfmc-list-gf (x) (:method-combination lfmc-list))
(defmethod lfmc-list-gf ((x integer)) (list :plain x))

(deftest long-form-mc-list
  (lfmc-list-gf 5)
  ((:plain 5)))

;;; Constants and calls mixed with CALL-METHOD, IF and LET.
(define-method-combination lfmc-mixed (&optional (kind :list))
  ((plain ()))
  (ecase kind
    (:list `(list 1 (call-method ,(first plain))))
    (:if `(if t (call-method ,(first plain)) nil))
    (:let `(let ((v (call-method ,(first plain))))
             (list :got v (length v))))))
(defgeneric lfmc-mixed-a (x) (:method-combination lfmc-mixed :list))
(defmethod lfmc-mixed-a ((x integer)) (list :plain x))
(defgeneric lfmc-mixed-b (x) (:method-combination lfmc-mixed :if))
(defmethod lfmc-mixed-b ((x integer)) (list :plain x))
(defgeneric lfmc-mixed-c (x) (:method-combination lfmc-mixed :let))
(defmethod lfmc-mixed-c ((x integer)) (list :plain x))

(deftest long-form-mc-mixed-forms
  (values (lfmc-mixed-a 5) (lfmc-mixed-b 6) (lfmc-mixed-c 7))
  (1 (:plain 5)) (:plain 6) (:got (:plain 7) 2))

;;; The standard method combination written as a long form (the example in
;;; CLHS DEFINE-METHOD-COMBINATION): MULTIPLE-VALUE-PROG1, MAKE-METHOD and
;;; CALL-NEXT-METHOD through :AROUND and primary methods.
(define-method-combination lfmc-standard ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method) `(call-method ,method)) methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary) ,(rest primary)))
                       ,@(reverse (call-methods after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around) (make-method ,form)))
          form))))

(defvar *lfmc-log* nil)
(defgeneric lfmc-std (x) (:method-combination lfmc-standard))
(defmethod lfmc-std ((x t)) (push :primary-t *lfmc-log*) (values :t x))
(defmethod lfmc-std ((x integer))
  (push :primary-integer *lfmc-log*)
  (multiple-value-bind (a b) (call-next-method)
    (values (list :integer a) b)))
(defmethod lfmc-std :before ((x integer)) (push :before *lfmc-log*))
(defmethod lfmc-std :after ((x integer)) (push :after-integer *lfmc-log*))
(defmethod lfmc-std :after ((x t)) (push :after-t *lfmc-log*))
(defmethod lfmc-std :around ((x integer))
  (push :around *lfmc-log*)
  (multiple-value-list (call-next-method)))

(deftest long-form-mc-standard
  (let ((*lfmc-log* nil))
    (let ((r (lfmc-std 3)))
      (list r (reverse *lfmc-log*))))
  (((:integer :t) 3)
   (:around :before :primary-integer :primary-t :after-t :after-integer)))

(deftest long-form-mc-standard-only-primary
  (let ((*lfmc-log* nil))
    (multiple-value-list (lfmc-std "s")))
  (:t "s"))

;;; The AND example from CLHS DEFINE-METHOD-COMBINATION, with a combination
;;; argument that is evaluated for :ORDER.
(define-method-combination lfmc-and (&optional (order :most-specific-first))
  ((around (:around))
   (primary (lfmc-and) :order order :required t))
  (let ((form (if (rest primary)
                  `(and ,@(mapcar #'(lambda (method) `(call-method ,method))
                                  primary))
                  `(call-method ,(first primary)))))
    (if around
        `(call-method ,(first around) (,@(rest around) (make-method ,form)))
        form)))

(defgeneric lfmc-and-first (x) (:method-combination lfmc-and))
(defmethod lfmc-and-first lfmc-and ((x integer)) (push :integer *lfmc-log*) x)
(defmethod lfmc-and-first lfmc-and ((x number)) (push :number *lfmc-log*) (plusp x))
(defgeneric lfmc-and-last (x) (:method-combination lfmc-and :most-specific-last))
(defmethod lfmc-and-last lfmc-and ((x integer)) (push :integer *lfmc-log*) x)
(defmethod lfmc-and-last lfmc-and ((x number)) (push :number *lfmc-log*) (plusp x))
(defmethod lfmc-and-last :around ((x integer)) (list :around (call-next-method)))

(deftest long-form-mc-and
  (let ((*lfmc-log* nil))
    (list (lfmc-and-first 4) (lfmc-and-first -4) (reverse *lfmc-log*)))
  (t nil (:integer :number :integer :number)))

(deftest long-form-mc-and-most-specific-last
  (let ((*lfmc-log* nil))
    (list (lfmc-and-last 4) (reverse *lfmc-log*)))
  ((:around 4) (:number :integer)))

;;; :ARGUMENTS variables are forms, evaluated on each call: the effective
;;; method may be reused across calls, but it must see each call's arguments.
(define-method-combination lfmc-with-lock ()
  ((methods ()))
  (:arguments object)
  `(unwind-protect
        (progn (push (list :lock ,object) *lfmc-log*)
               ,@(mapcar #'(lambda (method) `(call-method ,method)) methods))
     (push (list :unlock ,object) *lfmc-log*)))
(defgeneric lfmc-locked (obj n) (:method-combination lfmc-with-lock))
(defmethod lfmc-locked ((obj symbol) n) (push (list :body obj n) *lfmc-log*) n)

(deftest long-form-mc-arguments
  (let ((*lfmc-log* nil))
    (list (lfmc-locked 'a 1) (lfmc-locked 'b 2) (reverse *lfmc-log*)))
  (1 2 ((:lock a) (:body a 1) (:unlock a) (:lock b) (:body b 2) (:unlock b))))

;;; An :ARGUMENTS form is evaluated where the effective method places it, and
;;; not while the body builds the form.
(define-method-combination lfmc-arg-count ()
  ((methods ()))
  (:arguments &whole whole x &optional (y (list :default x) y-p))
  `(list (length ,whole) ,x ,y ,y-p
         ,@(mapcar #'(lambda (method) `(call-method ,method)) methods)))
(defgeneric lfmc-arg-count-gf (x &optional y) (:method-combination lfmc-arg-count))
(defmethod lfmc-arg-count-gf (x &optional y) (declare (ignore y)) (list :m x))

(deftest long-form-mc-arguments-whole-optional
  (values (lfmc-arg-count-gf 1) (lfmc-arg-count-gf 1 2))
  (1 1 (:default 1) nil (:m 1))
  (2 1 2 t (:m 1)))

;;; :GENERIC-FUNCTION binds a variable to the generic function object.
(define-method-combination lfmc-gf ()
  ((methods ()))
  (:generic-function gf)
  `(list (eq ',gf #'lfmc-gf-name)
         ,@(mapcar #'(lambda (method) `(call-method ,method)) methods)))
(defgeneric lfmc-gf-name (x) (:method-combination lfmc-gf))
(defmethod lfmc-gf-name ((x t)) x)

(deftest long-form-mc-generic-function
  (lfmc-gf-name 9)
  (t 9))
