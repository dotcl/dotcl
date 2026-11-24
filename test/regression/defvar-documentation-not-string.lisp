;;; The documentation of DEFVAR, DEFPARAMETER and DEFCONSTANT must be a string;
;;; another form is a program error rather than an evaluated extra form.

(deftest defvar-doc.not-string-is-error
  (list (handler-case (eval '(defvar *dvd-1* 10 (print "x"))) (program-error () :error))
        (handler-case (eval '(defparameter *dvd-2* 10 3)) (program-error () :error))
        (handler-case (eval '(defconstant +dvd-3+ 10 (print "x"))) (program-error () :error)))
  (:error :error :error))

(deftest defvar-doc.string-and-absent-unchanged
  (list (eval '(defvar *dvd-4* 10 "doc")) (eval '(defvar *dvd-5* 10)) (eval '(defvar *dvd-6*)))
  (*dvd-4* *dvd-5* *dvd-6*))
