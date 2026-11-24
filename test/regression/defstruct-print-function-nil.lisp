;;; (:print-function nil) and (:print-object nil) name no printer: the default one is
;;; used. They used to expand into (function nil) and fail when the object was printed.

(deftest defstruct-print-function-nil.uses-default-printer
  (progn
    (eval '(defstruct (dpfn-a (:print-function nil)) x))
    (eval '(defstruct (dpfn-b (:print-object nil)) x))
    (list (search "DPFN-A" (prin1-to-string (funcall (intern "MAKE-DPFN-A") :x 1)))
          (search "DPFN-B" (prin1-to-string (funcall (intern "MAKE-DPFN-B") :x 1)))))
  (3 3))
