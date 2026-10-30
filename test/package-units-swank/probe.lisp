(let ((*package* (find-package "COMMON-LISP-USER")))
  (flet ((fn (name) (let ((s (find-symbol name "SWANK"))) (and s (fboundp s) t))))
    (format nil "swank=~A version=~A find-cycle=~A casify-char=~A swanks=~D read-back=~A"
            (package-name (find-package "SWANK"))
            (symbol-value (read-from-string "swank::*swank-wire-protocol-version*"))
            (fn "FIND-CYCLE") (fn "CASIFY-CHAR")
            (count "SWANK" (list-all-packages) :key #'package-name :test #'string=)
            (let ((s (read-from-string "swank::connection-info")))
              (eq s (read-from-string (prin1-to-string s)))))))
