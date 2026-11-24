;;; A &rest list that a function edits in place (mito's helpers do (PUSH x (GETF initargs k)) on
;;; the INITARGS they are given) is the list a later (APPLY f ... list) sees. Forwarding the
;;; frame's argument array to APPLY instead is only for a rest list used for nothing else.

(defun %rl-edit (plist) (setf (getf plist :k) (list :edited)) plist)
(defun %rl-push (plist) (push :pushed (getf plist :k)) plist)

(defun %rl-defun (&rest r) (%rl-edit r) (apply #'list r))
(defun %rl-defun-key (&rest r &key k &allow-other-keys) (declare (ignore k)) (%rl-push r) (apply #'list r))
(defun %rl-defun-prefix (a &rest r) (%rl-edit r) (apply #'list a r))
(defun %rl-lambda (&rest r) (funcall (lambda (&rest r2) (%rl-edit r2) (apply #'list r2)) :k 2))

(defgeneric %rl-gf (x &rest r))
(defmethod %rl-gf :around ((x t) &rest r) (%rl-edit r) (apply #'call-next-method x r))
(defmethod %rl-gf ((x t) &rest r) r)

(defun %rl-untouched (&rest r) (apply #'list r))
(defun %rl-read-only (&rest r) (list (length r) (getf r :k) (apply #'list r)))
(defun %rl-plain-copy (&rest r) (let ((c (copy-list r))) (%rl-edit c) (list r (apply #'list c))))

(deftest rest-edit.defun
  (list (%rl-defun :k 1 :z 2) (%rl-defun-key :k (list 1)) (%rl-defun-prefix 0 :k 1))
  ((:k (:edited) :z 2) (:k (:pushed 1)) (0 :k (:edited))))

(deftest rest-edit.lambda
  (%rl-lambda)
  (:k (:edited)))

(deftest rest-edit.method
  (%rl-gf 1 :k (list 1))
  (:k (:edited)))

(deftest rest-edit.untouched-and-copies-unchanged
  (list (%rl-untouched 1 2 3) (%rl-read-only :k 5) (%rl-plain-copy :k 1))
  ((1 2 3) (2 5 (:k 5)) ((:k 1) (:k (:edited)))))
