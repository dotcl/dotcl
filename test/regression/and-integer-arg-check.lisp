;;; A parameter declared an AND of integer types (directly or through a
;;; DEFTYPE) is checked inline against the range the type denotes. An AND with
;;; any other kind of component keeps the general check.

(setf dotcl:*save-sil* t)

(deftype %aiac-ufix () '(and fixnum unsigned-byte))
(deftype %aiac-even () '(and integer (satisfies evenp)))

(defun %aiac-u (x) (declare (type %aiac-ufix x)) (list x))
(defun %aiac-b (x) (declare (type (and (integer 0 100) (unsigned-byte 4)) x)) (list x))
(defun %aiac-e (x) (declare (type %aiac-even x)) (list x))

(defun %aiac-te (fn v)
  (handler-case (funcall fn v) (type-error () :type-error)))

(deftest and-integer-arg-check.values
  (list (mapcar (lambda (v) (%aiac-te #'%aiac-u v))
                (list 0 5 most-positive-fixnum -1 (1+ most-positive-fixnum) 1.0 nil))
        (mapcar (lambda (v) (%aiac-te #'%aiac-b v)) (list 0 15 16 -1))
        (mapcar (lambda (v) (%aiac-te #'%aiac-e v)) (list 2 3 4.0)))
  (((0) (5) (#.most-positive-fixnum) :type-error :type-error :type-error :type-error)
   ((0) (15) :type-error :type-error)
   ((2) :type-error :type-error)))

(deftest-compiled-only and-integer-arg-check.inline
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "CheckArgType" #'%aiac-u) (has "CheckArgType" #'%aiac-b)
          (has "CheckArgType" #'%aiac-e)))
  (nil nil t))

(setf dotcl:*save-sil* nil)
