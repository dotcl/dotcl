;;; A malformed special form written inside a HANDLER-CASE is a run-time PROGRAM-ERROR
;;; of that form (SBCL: a warning, then the same), not a failure to compile the
;;; whole toplevel form. The forms are literal in this file on purpose.

(deftest malformed-form.lambda-dup-optional
  (handler-case (funcall (lambda (&optional a &optional b) a)) (program-error () :pe))
  :pe)

(deftest malformed-form.lambda-rest-without-var
  (handler-case (funcall (lambda (&rest) 1)) (program-error () :pe))
  :pe)

(deftest malformed-form.lambda-key-optional-order
  (handler-case (funcall (lambda (&key &optional) 1)) (program-error () :pe))
  :pe)

(deftest malformed-form.lambda-duplicate-variable
  (handler-case (funcall (lambda (a a) 1) 1 2) (program-error () :pe))
  :pe)

(deftest malformed-form.lambda-non-symbol-variable
  (handler-case (funcall (lambda (1) 1) 1) (error () :err))
  :err)

(deftest malformed-form.labels-duplicate-variable
  (handler-case (labels ((f (a a) 1)) (f 1 2)) (program-error () :pe))
  :pe)

(deftest malformed-form.quote-arity
  (list (handler-case (quote) (program-error () :pe))
        (handler-case (quote 1 2) (program-error () :pe)))
  (:pe :pe))

(deftest malformed-form.cond-clause
  (list (handler-case (cond (1 2) 3) (error () :err))
        (handler-case (cond ()) (error () :err)))
  (:err :err))

(deftest malformed-form.defun-lambda-list
  (list (handler-case (progn (defun mf-bad-1 1) :defined) (error () :err))
        (handler-case (progn (defun mf-bad-2 (a a) 1) :defined) (error () :err)))
  (:err :err))

(deftest malformed-form.rest-of-the-form-still-runs
  (list (handler-case (funcall (lambda (&optional a &optional b) a)) (program-error () :pe))
        (+ 1 2))
  (:pe 3))

;;; Every shape CLHS 3.4.1 allows in an ordinary lambda list still compiles.
(deftest malformed-form.valid-lambda-lists
  (loop for ll in '(()
                    (a)
                    (a b)
                    (&optional)
                    (&optional a)
                    (&optional (a))
                    (&optional (a 1))
                    (&optional (a 1 ap))
                    (&optional a (b 2 bp) c)
                    (&rest r)
                    (a &rest r)
                    (&key)
                    (&key a)
                    (&key (a))
                    (&key (a 1))
                    (&key (a 1 ap))
                    (&key ((:k a)))
                    (&key ((:k a) 1))
                    (&key ((:k a) 1 ap))
                    (&key a &allow-other-keys)
                    (&key &allow-other-keys)
                    (&aux)
                    (&aux a)
                    (&aux (a))
                    (&aux (a 1))
                    (&aux (a 1) (b (+ a 1)))
                    (a &aux (a 2))
                    (a b &optional c d &rest r &key e f &allow-other-keys &aux g h)
                    (&optional a &key b)
                    (&optional a &rest r)
                    (&rest r &key k)
                    (&rest r &key k &allow-other-keys &aux z)
                    (&optional (a 1) (b a))
                    (&key (a 1) (b a)))
        always (functionp (eval `(function (lambda ,ll nil)))))
  t)

(deftest malformed-form.valid-lambda-lists-run
  (list (funcall (lambda (a &optional (b 2 bp) &rest r &key (k 3 kp) &allow-other-keys &aux (z (+ a 1)))
                   (list a b bp r k kp z))
                 1 5 :k 9 :other 1)
        (funcall (lambda (&key ((:kw v) 5)) v) :kw 8)
        (labels ((f (a &optional (b a)) (list a b))) (f 1))
        (flet ((g (&aux (a 7)) a)) (g)))
  ((1 5 T (:K 9 :OTHER 1) 9 T 2) 8 (1 1) 7))
