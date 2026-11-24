;;; The selector of ~[ is an integer. Anything else is an error (it used to select nothing and
;;; print nothing); ~:[ and ~@[ take any object.

(defvar *fcs-fn* #'format)

(defun fcs-run (control &rest args)
  (handler-case (apply *fcs-fn* nil control args)
    (type-error () :type-error)
    (error () :error)))

(deftest format-conditional-selector-type.non-integer
  (list (fcs-run "~[a~;b~]" t) (fcs-run "~[a~;b~]" "s") (fcs-run "~[a~;b~]" 1.5)
        (fcs-run "~[a~;b~]" nil) (fcs-run "~[a~;b~]" 'x))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest format-conditional-selector-type.valid
  (list (fcs-run "~[a~;b~]" 1) (fcs-run "~[a~;b~:;c~]" 7) (fcs-run "~[a~;b~]" 5) (fcs-run "~[a~;b~]" -1)
        (fcs-run "~:[no~;yes~]" 'x) (fcs-run "~:[no~;yes~]" nil) (fcs-run "~@[~a~]" 3) (fcs-run "~2[a~;b~;c~]")
        (fcs-run "~[a~;b~]" (expt 10 30)))
  ("b" "c" "" "" "yes" "no" "3" "c" ""))
