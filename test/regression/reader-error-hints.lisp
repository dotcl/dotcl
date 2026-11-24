;;; A reader error at the end of the input says where the unclosed "(" or string
;;; began, and a package named before its module is loaded says which module to
;;; REQUIRE. (A first-day script with a missing paren used to report only
;;; "Unexpected end of input in list".)

(defun %reh-message (string)
  (handler-case (progn (read-from-string string) nil)
    (error (e) (princ-to-string e))))

(deftest reader-error-hints.unclosed-list-names-its-start
  (let ((m (%reh-message (format nil "(defun f (x)~%  (+ x 1)"))))
    (and m (search "Unexpected end of input in list" m)
         (search "opened at line 1, column 1" m)
         (search "opened at line 3, column 3" (%reh-message (format nil "(a~%  (b c)~%  (d e"))) t))
  t)

(deftest reader-error-hints.unclosed-string-names-its-start
  (let ((m (%reh-message "(print \"abc")))
    (and m (search "Unterminated string" m) (search "opened at line 1, column 8" m) t))
  t)

(deftest reader-error-hints.unloaded-quicklisp-names-the-module
  (if (find-package "QL")
      t
      (let ((m (%reh-message "(ql:quickload \"x\")")))
        (and m (search "Package \"QL\" not found" m) (search "(require \"quicklisp\")" m) t)))
  t)

(deftest reader-error-hints.dotnet-ref-names-the-module
  (if (eq (nth-value 1 (find-symbol "REF" "DOTNET")) :external)
      t
      (let ((m (%reh-message "(dotnet:ref l 0)")))
        (and m (search "Symbol \"REF\" is not external in package \"DOTNET\"" m)
             (search "(require \"dotnet-class\")" m) t)))
  t)
