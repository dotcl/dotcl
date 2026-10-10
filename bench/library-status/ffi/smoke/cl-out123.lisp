;; out123_new, then out123_drivers: char** results read back with MEM-AREF.
(let ((o (cl-out123:make-output nil)))
  (smoke "drivers" (mapcar #'first (cl-out123:drivers o))))
