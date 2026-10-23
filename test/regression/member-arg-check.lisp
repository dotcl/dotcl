;;; A parameter declared (MEMBER sym ...) is accepted by comparing it against
;;; the symbols inline; anything else still goes through the full check, which
;;; signals the TYPE-ERROR.

(setf dotcl:*save-sil* t)

(declaim (ftype (function ((member %mac-lt %mac-eq %mac-gt)) t) %mac-f))
(defun %mac-f (x) (list x))
(defun %mac-n (x) (declare (type (member nil :a t) x)) (list x))
(defun %mac-k (x) (declare (type (member 1 :a) x)) (list x))
(defun %mac-e (x) (declare (type (eql :only) x)) (list x))

(defun %mac-te (fn v)
  (handler-case (funcall fn v) (type-error () :type-error)))

(deftest member-arg-check.values
  (list (mapcar (lambda (v) (%mac-te #'%mac-f v)) (list '%mac-lt '%mac-gt '%mac-other nil 1))
        (mapcar (lambda (v) (%mac-te #'%mac-n v)) (list nil t :a :b))
        (mapcar (lambda (v) (%mac-te #'%mac-k v)) (list 1 :a 2))
        (mapcar (lambda (v) (%mac-te #'%mac-e v)) (list :only :other)))
  (((%mac-lt) (%mac-gt) :type-error :type-error :type-error)
   ((nil) (t) (:a) :type-error)
   ((1) (:a) :type-error)
   ((:only) :type-error)))

(deftest-compiled-only member-arg-check.inline
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "BEQ" #'%mac-f) (has "BEQ" #'%mac-e) (has "BEQ" #'%mac-k)))
  (t t nil))

(setf dotcl:*save-sil* nil)
