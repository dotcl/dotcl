;;; WARN establishes MUFFLE-WARNING before *BREAK-ON-SIGNALS* looks at the warning, so the
;;; break hook can muffle it.

(defvar *wmb-seen* nil)

(defun %wmb-hook (c old)
  (declare (ignore old))
  (push (type-of c) *wmb-seen*)
  (let ((r (find-restart 'muffle-warning c)))
    (when r (invoke-restart r))))

(deftest warn-muffle-restart-in-break-hook.hook-can-muffle
  (progn
    (setq *wmb-seen* nil)
    (let ((*break-on-signals* 'warning)
          (*debugger-hook* #'%wmb-hook)
          (sym (find-symbol "*INVOKE-DEBUGGER-HOOK*" "DOTCL")))
      (progv (list sym) (list #'%wmb-hook)
        (let ((*error-output* (make-broadcast-stream)))
          (list (warn "muffled by the hook") (and *wmb-seen* t))))))
  (nil t))
