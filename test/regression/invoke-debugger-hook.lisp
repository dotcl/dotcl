;;; dotcl:*INVOKE-DEBUGGER-HOOK*, as sb-ext:*invoke-debugger-hook*: run by
;;; INVOKE-DEBUGGER and by BREAK, before *DEBUGGER-HOOK*, with itself bound to
;;; NIL, and with the same condition and hook arguments. If it returns, the
;;; debugger still runs.

(defun idh-break-with-hook ()
  "Run BREAK with a hook that continues; return what the hook saw."
  (let ((seen nil) (inner :unset))
    (let ((dotcl:*invoke-debugger-hook*
            (lambda (condition hook)
              (declare (ignore hook))
              (setf seen (type-of condition)
                    inner dotcl:*invoke-debugger-hook*)
              (invoke-restart 'continue))))
      (break "caught by invoke-debugger-hook"))
    (list seen inner)))

(deftest invoke-debugger-hook.break-continue
  (idh-break-with-hook)
  (simple-condition nil))

(deftest invoke-debugger-hook.break-continue-eval
  (eval '(let ((seen nil) (inner :unset))
           (let ((dotcl:*invoke-debugger-hook*
                   (lambda (condition hook)
                     (declare (ignore hook))
                     (setf seen (type-of condition)
                           inner dotcl:*invoke-debugger-hook*)
                     (invoke-restart 'continue))))
             (break "caught by eval"))
           (list seen inner)))
  (simple-condition nil))

(deftest invoke-debugger-hook.before-debugger-hook
  (let ((log nil))
    (catch 'idh-out
      (let ((dotcl:*invoke-debugger-hook*
              (lambda (condition hook)
                (declare (ignore condition hook))
                (push :invoke-debugger-hook log)))
            (*debugger-hook*
              (lambda (condition hook)
                (declare (ignore condition hook))
                (push :debugger-hook log)
                (throw 'idh-out nil))))
        (invoke-debugger (make-condition 'simple-error :format-control "order"))))
    (nreverse log))
  (:invoke-debugger-hook :debugger-hook))

;;; DEFTEST's own HANDLER-CASE would take these errors before the debugger
;;; could, which is the point of them.
(let ((*deftest-contain-errors* nil))

  (deftest invoke-debugger-hook.debugger-hook-called-when-new-hook-nil
    (let ((seen nil))
      (catch 'idh-out
        (let ((dotcl:*invoke-debugger-hook* nil)
              (*debugger-hook*
                (lambda (condition hook)
                  (declare (ignore hook))
                  (setf seen (type-of condition))
                  (throw 'idh-out nil))))
          (error "for the debugger hook")))
      seen)
    simple-error)

  ;; A runtime-signalled error that nothing handles reaches the new hook just
  ;; as an unhandled ERROR does, even when *DEBUGGER-HOOK* is NIL.
  (deftest invoke-debugger-hook.runtime-error-seen
    (let ((seen nil))
      (catch 'idh-out
        (let ((dotcl:*invoke-debugger-hook*
                (lambda (condition hook)
                  (declare (ignore hook))
                  (setf seen (type-of condition))
                  (throw 'idh-out nil)))
              (*debugger-hook* nil))
          (car 1)))
      seen)
    type-error))

;;; With no hooks, BREAK still goes straight to the debugger and does not call
;;; *DEBUGGER-HOOK*, as CLHS requires. Non-interactively the debugger declines.
(deftest invoke-debugger-hook.break-no-hooks-still-declines
  (let ((dotcl:*invoke-debugger-hook* nil)
        (*debugger-hook*
          (lambda (condition hook)
            (declare (ignore condition hook))
            (error "BREAK called *DEBUGGER-HOOK*"))))
    (handler-case (progn (break "nobody is listening") :returned)
      (error (e)
        (if (search "Debugger: non-interactive session" (princ-to-string e))
            :debugger
            (list :unexpected (princ-to-string e))))))
  :debugger)
