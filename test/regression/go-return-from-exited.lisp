;;; A GO to a tag of a TAGBODY, or a RETURN-FROM a BLOCK, that a closure does after
;;; that TAGBODY or BLOCK has exited signals CONTROL-ERROR (CLHS 5.2 leaves it undefined;
;;; SBCL signals CONTROL-ERROR). It used to throw a transfer nothing caught, which ended
;;; the whole process. Transfers to a tagbody / block that is still active are unchanged.

(defvar *gre-go* nil)
(defvar *gre-block* nil)

(deftest-compiled-only go-return-from-exited.go
  (progn
    (eval '(tagbody (setq *gre-go* (lambda () (go a))) (go b) a (print :in-a) b))
    (handler-case (funcall *gre-go*) (control-error () :control-error)))
  :control-error)

(deftest-compiled-only go-return-from-exited.return-from
  (progn
    (eval '(block gre-blk (setq *gre-block* (lambda () (return-from gre-blk 1)))))
    (handler-case (funcall *gre-block*) (control-error () :control-error)))
  :control-error)

(deftest-compiled-only go-return-from-exited.active-ones-work
  (list (let ((n 0))
          (tagbody
             (funcall (lambda () (go done)))
           again (incf n) (go again)
           done)
          n)
        (block outer (funcall (lambda () (return-from outer :out))) :not-reached)
        (let ((acc '()) (i 0))
          (tagbody
           top (when (< i 3)
                 (push (funcall (lambda () i)) acc)
                 (incf i)
                 (go top)))
          acc))
  (0 :out (2 1 0)))

(deftest-compiled-only go-return-from-exited.again-after-an-error
  (progn
    (eval '(block gre-blk2 (setq *gre-block* (lambda () (return-from gre-blk2 1)))))
    (list (handler-case (funcall *gre-block*) (control-error () :control-error))
          (handler-case (funcall *gre-block*) (control-error () :control-error))))
  (:control-error :control-error))
