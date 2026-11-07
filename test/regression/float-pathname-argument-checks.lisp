;;; FLOAT's prototype has to be a float; MAKE-PATHNAME components cannot be numbers or
;;; characters (the version alone may be an integer). Valid forms are unchanged.

(defmacro %fpac-type-error-p (form)
  `(handler-case (progn ,form nil) (type-error () t)))

(deftest float-pathname-argument-checks.float-prototype
  (list (%fpac-type-error-p (float 1 0))
        (%fpac-type-error-p (float 1 'single-float))
        (float 1 1.0) (float 1 1.0d0) (float 1/2) (float 2 0.5d0))
  (t t 1.0 1.0d0 0.5 2.0d0))

(deftest float-pathname-argument-checks.make-pathname-types
  (list (%fpac-type-error-p (make-pathname :name 5))
        (%fpac-type-error-p (make-pathname :type 5))
        (%fpac-type-error-p (make-pathname :host 5))
        (%fpac-type-error-p (make-pathname :device #\a))
        (%fpac-type-error-p (make-pathname :directory 5))
        (%fpac-type-error-p (make-pathname :version "x")))
  (t t t t t nil))

(deftest float-pathname-argument-checks.make-pathname-valid
  (list (namestring (make-pathname :name "a" :type "b"))
        (pathname-name (make-pathname :name :wild))
        (pathname-version (make-pathname :name "a" :version 3))
        (pathname-version (make-pathname :name "a" :version :newest))
        (pathname-directory (make-pathname :directory '(:absolute "x" "y")))
        (pathname-name (make-pathname)))
  ("a.b" :wild 3 :newest (:absolute "x" "y") nil))
