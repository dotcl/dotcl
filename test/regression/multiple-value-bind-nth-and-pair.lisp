;;; MULTIPLE-VALUE-BIND past index 1000 and the VALUES2 pair path.

(setf dotcl:*save-sil* t)

(defmacro %mvbn-bind-many (n value-form &body body)
  (let ((vars (loop for i below n
                    collect (intern (format nil "MVBN~D" i) "COMMON-LISP-USER"))))
    `(multiple-value-bind ,vars ,value-form
       (declare (ignorable ,@vars))
       ,@body)))

(defun %mvbn-long ()
  (%mvbn-bind-many 1002
      (apply #'values (loop for i below 1002 collect i))
    (list mvbn0 mvbn1000 mvbn1001)))
(defun %mvbn-two () (values 1 2))
(declaim (notinline %mvbn-two))
(defun %mvbn-short ()
  ;; The values come from a call: a literal (values 1 2) is bound as two LETs, with no capture.
  (%mvbn-bind-many 1002 (%mvbn-two)
    (list mvbn0 mvbn1 mvbn2 mvbn1000 mvbn1001)))

(defun %mvbn-build-pair ()
  (let ((old dotcl-internal::*mv-mode-elision*))
    (setq dotcl-internal::*mv-mode-elision* nil)
    (unwind-protect
        ;; (PROGN (VALUES ...)): a bare (VALUES ...) there is bound as two LETs, with no capture
        (eval '(defun %mvbn-pair (a b)
                 (multiple-value-bind (x y) (progn (values a (+ b 1)))
                   (list x y))))
      (setq dotcl-internal::*mv-mode-elision* old))))
(%mvbn-build-pair)

(defun %mvbn-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(deftest-emitting-only multiple-value-bind-nth.values
  (list (%mvbn-long) (%mvbn-short))
  ((0 1000 1001) (1 2 nil nil nil)))

(deftest-emitting-only multiple-value-bind-nth.uses-bindnth
  (list (%mvbn-has "MultipleValues.BindNth)" #'%mvbn-long)
        (%mvbn-has "MultipleValues.BindNth)" #'%mvbn-short))
  (t t))

(deftest multiple-value-bind-nth.pair-values
  (%mvbn-pair 10 20)
  (10 21))

(deftest-compiled-only multiple-value-bind-nth.uses-the-pair-entry
  (list (%mvbn-has "Runtime.Values2Pair" #'%mvbn-pair)
        (%mvbn-has "MultipleValues.CapturePair" #'%mvbn-pair))
  (t t))
