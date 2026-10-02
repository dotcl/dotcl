;;; A supplied-p variable that is assigned and also captured by a closure.
;;; Such a variable lives in a box, like any assigned captured parameter, but
;;; its binding stored the bare T/NIL; the closure then failed with an invalid
;;; cast, or read the wrong slot. series' LATCH does (SETQ POST-P T) and then
;;; closes over POST-P.

(defun spc-key (items &key (pre nil pre-p) (post nil post-p))
  (when (null pre-p) (setq post-p t))
  (funcall (lambda () (list items pre pre-p post post-p))))

(defun spc-opt (&optional (a nil a-p))
  (setq a-p (list a-p))
  (funcall (lambda () (list a a-p))))

(defun spc-set-in-closure (&key (k 0 k-p))
  (funcall (lambda () (setq k-p :changed)))
  (list k k-p))

(deftest supplied-p-closure-key-supplied
  (spc-key 1 :pre 'a)
  (1 a t nil nil))

(deftest supplied-p-closure-key-assigned
  (spc-key 1 :post 'b)
  (1 nil nil b t))

(deftest supplied-p-closure-optional
  (list (spc-opt) (spc-opt 5))
  ((nil (nil)) (5 (t))))

(deftest supplied-p-closure-set-inside
  (spc-set-in-closure :k 2)
  (2 :changed))
