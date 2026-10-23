;;; DOTCL:WATCH-FDEFINITION calls its function after each change to the
;;; symbol's global function definition, until it is given NIL.

(defvar *wfd-log* '())
(defun wfd-target (x) x)

(defun wfd-run ()
  (setf *wfd-log* '())
  (dotcl:watch-fdefinition 'wfd-target
                           (lambda (sym def)
                             (push (list sym (if (functionp def) (funcall def 1) def))
                                   *wfd-log*)))
  (unwind-protect
       (progn
         (eval '(defun wfd-target (x) (+ x 10)))
         (setf (fdefinition 'wfd-target) (lambda (x) (+ x 20)))
         (fmakunbound 'wfd-target))
    (dotcl:watch-fdefinition 'wfd-target nil))
  (defun wfd-target (x) (+ x 30))
  (list (remove-duplicates (reverse *wfd-log*) :test #'equal :from-end t)
        (wfd-target 1)))

(deftest watch-fdefinition.changes
  (wfd-run)
  (((wfd-target 11) (wfd-target 21) (wfd-target nil)) 31))
