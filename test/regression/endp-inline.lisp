;;; ENDP compiled in place (a branch test, or a value) keeps the function's
;;; meaning: true for NIL, false for a cons, TYPE-ERROR for anything else.

(setf dotcl:*save-sil* t)

(defun %ei-value (x) (endp x))
(defun %ei-branch (x) (if (endp x) :end :more))
(defun %ei-loop-count (l) (loop for x in l count x))

(deftest endp-inline.values
  (list (%ei-value nil) (%ei-value '(1)) (%ei-branch nil) (%ei-branch '(1 . 2)))
  (t nil :end :more))

(deftest endp-inline.value-type-error
  (handler-case (%ei-value 5)
    (type-error (c) (list (type-error-datum c) (type-error-expected-type c))))
  (5 list))

(deftest endp-inline.branch-type-error
  (handler-case (%ei-branch "x")
    (type-error (c) (type-error-datum c)))
  "x")

(deftest endp-inline.loop-dotted-list
  (list (%ei-loop-count '(1 nil 3))
        (handler-case (%ei-loop-count '(1 2 . 3))
          (type-error (c) (list :error (type-error-datum c)))))
  (2 (:error 3)))

(deftest endp-inline.funcall
  (handler-case (funcall #'endp 5)
    (type-error () :error))
  :error)

(deftest-compiled-only endp-inline.shape
  (list (and (search "Runtime.Endp" (%flat-text (dotcl:function-sil #'%ei-value))) t)
        (and (search "Runtime.IsTrueEndp" (%flat-text (dotcl:function-sil #'%ei-branch))) t))
  (t t))
