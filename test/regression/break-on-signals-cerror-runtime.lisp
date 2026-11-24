;;; *BREAK-ON-SIGNALS* applies to every signalled condition: CERROR, and the errors the
;;; runtime itself raises (a missing restart in INVOKE-RESTART and so on), as for ERROR.

(defvar *bos-seen* nil)

(defun %bos-hook (c old)
  (declare (ignore old))
  (push (type-of c) *bos-seen*)
  (let ((r (or (find-restart 'continue c) (find-restart 'continue))))
    (when r (invoke-restart r))))

(defun %bos-run (thunk)
  (setq *bos-seen* nil)
  (let ((*break-on-signals* 'error)
        (*debugger-hook* #'%bos-hook)
        (sym (find-symbol "*INVOKE-DEBUGGER-HOOK*" "DOTCL")))
    (progv (list sym) (list #'%bos-hook)
      (handler-case (funcall thunk) (condition () nil))))
  (and *bos-seen* t))

(deftest break-on-signals-cerror-runtime.cerror
  (%bos-run (lambda () (cerror "go on" "a continuable error")))
  t)

(deftest break-on-signals-cerror-runtime.missing-restart
  (list (%bos-run (lambda () (invoke-restart 'bos-no-such-restart)))
        (%bos-run (lambda () (invoke-restart-interactively 'bos-no-such-restart))))
  (t t))

(deftest break-on-signals-cerror-runtime.error-still-works
  (list (%bos-run (lambda () (error "plain")))
        (%bos-run (lambda () (car 5))))
  (t t))

(deftest break-on-signals-cerror-runtime.not-matching-type
  (let ((*break-on-signals* nil) (n 0))
    (handler-case (cerror "go on" "x") (condition () (incf n)))
    (handler-case (invoke-restart 'bos-no-such-restart) (condition () (incf n)))
    n)
  2)
