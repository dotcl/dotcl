;;; A tag that appears twice in one TAGBODY is a PROGRAM-ERROR; the same tag in different
;;; (nested) tagbodies is fine.

(defun %tdt-class (thunk) (handler-case (progn (funcall thunk) :ok) (program-error () :program-error)))

(deftest tagbody-duplicate-tag.eval
  (list (%tdt-class (lambda () (eval '(tagbody a (progn) a))))
        (%tdt-class (lambda () (eval '(tagbody 0 (progn) 0))))
        (%tdt-class (lambda () (eval '(tagbody nil nil)))))
  (:program-error :program-error :program-error))

(deftest-emitting-only tagbody-duplicate-tag.compiled
  (list (%tdt-class (lambda () (funcall (compile nil '(lambda () (tagbody a (progn) a))))))
        (%tdt-class (lambda () (funcall (compile nil '(lambda () (tagbody 0 (progn) 0))))))
        (%tdt-class (lambda () (funcall (compile nil '(lambda () (tagbody nil nil)))))))
  (:program-error :program-error :program-error))

(deftest tagbody-duplicate-tag.valid
  (list (tagbody)
        (tagbody nil)
        (let ((n 0)) (tagbody a (incf n) (go b) b) n)
        (let ((n 0)) (tagbody 0 (incf n) 1 (incf n)) n)
        (let ((n 0))
          (tagbody
             a (incf n)
             (tagbody a (incf n) (go b) b)
             (when (< n 3) (go a))
             b)
          n))
  (nil nil 1 2 4))
