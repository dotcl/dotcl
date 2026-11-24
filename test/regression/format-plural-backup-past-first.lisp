;;; ~:P backs up one argument first, so with no argument before it there is nothing to back up to:
;;; an error, as for ~:*. It used to take the first argument as the one to pluralise.

(defvar *fpb-fn* #'format)

(defun fpb-error-p (control &rest args)
  (handler-case (progn (apply *fpb-fn* nil control args) nil)
    (error () t)))

(deftest format-plural-backup-past-first.errors
  (list (fpb-error-p "~:p" "x") (fpb-error-p "~:p" 1) (fpb-error-p "~:@p" 1)
        (fpb-error-p "~:*~a" 1))
  (t t t t))

(deftest format-plural-backup-past-first.after-an-argument
  (list (apply *fpb-fn* nil "~a~:p" '(1)) (apply *fpb-fn* nil "~a~:p" '(2))
        (apply *fpb-fn* nil "~d file~:p" '(3)) (apply *fpb-fn* nil "~d famil~:@p" '(1))
        (apply *fpb-fn* nil "~d famil~:@p" '(2)))
  ("1" "2s" "3 files" "1 family" "2 families"))
