;;; SXHASH of strings, symbols, characters and numbers must agree between Lisp
;;; images (CLHS 18.2.3.2). .NET's String.GetHashCode and BigInteger.GetHashCode
;;; are seeded per process, so two children used to print different values.

(defvar *sxsi-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *sxsi-core* (regression-child-core))

(defparameter *sxsi-form*
  "(dolist (x (list 12345678901234567890
                    (/ (expt 2 100) (1+ (expt 2 100)))
                    1.5f0 1.5d0 #\\a \"abc\"
                    \"the quick brown fox jumps over the lazy dog\"
                    'sxsi-symbol :sxsi-keyword (list 1 \"abc\") #*101))
     (format t \"~D~%\" (sxhash x)))")

(defun %sxsi-run ()
  (dotcl:run-process *sxsi-exe*
                     (list "--asm" *sxsi-core*
                           "--eval" *sxsi-form*
                           "--eval" "(dotcl:quit 0)")))

(deftest sxhash-stable-across-images.sxhash
  (let ((a (%sxsi-run))
        (b (%sxsi-run)))
    (list (first a) (first b)
          (and (string= (second a) (second b)) t)
          (count #\Newline (second a))))
  (0 0 t 11))
