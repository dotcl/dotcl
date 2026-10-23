;;; A bundled contrib with no .asd of its own (quicklisp, dotnet-ffi) has to win
;;; over a same-named .asd that ASDF finds elsewhere, the way the contribs that do
;;; carry an .asd win through the central registry. With a source registry over a
;;; whole dotcl checkout, quicklisp-client/quicklisp/quicklisp.asd was found by
;;; (require "quicklisp") through ASDF's module provider and loaded as source,
;;; which stopped on an undefined QMERGE.
;;;
;;; The .asd files below signal an error if ASDF ever reads them.

(require "asdf")

(defun %crs-with-fake-asds (names thunk)
  (let ((dir (format nil "~a/dotcl-crs-~a/" (regression-temp-dir) (get-internal-real-time))))
    (ensure-directories-exist dir)
    (dolist (name names)
      (with-open-file (s (concatenate 'string dir name ".asd")
                         :direction :output :if-exists :supersede)
        (format s "(error \"read the registry's ~a.asd instead of the bundled one\")" name)))
    (push (pathname dir) asdf:*central-registry*)
    (unwind-protect
         (handler-case (funcall thunk)
           (error (e) (list :error (princ-to-string e))))
      (setf asdf:*central-registry*
            (remove (pathname dir) asdf:*central-registry* :test #'equal)))))

(deftest contrib-registry-shadow.find-system-dotnet-ffi
  (%crs-with-fake-asds '("dotnet-ffi")
    (lambda ()
      (typep (asdf:find-system "dotnet-ffi") 'asdf:require-system)))
  t)

(when (probe-file "contrib/quicklisp/quicklisp.lisp")
  (require "quicklisp")

  ;; Taking "quicklisp" off *MODULES* sends REQUIRE through the providers again,
  ;; ASDF's first: what a first (require "quicklisp") after (require "asdf") does.
  (deftest contrib-registry-shadow.require-quicklisp
    (%crs-with-fake-asds '("quicklisp")
      (lambda ()
        (let ((*modules* (remove "quicklisp" *modules* :test #'equal)))
          (require "quicklisp")
          (and (member "quicklisp" *modules* :test #'equal) t))))
    t)

  (deftest contrib-registry-shadow.depends-on-quicklisp
    (%crs-with-fake-asds '("quicklisp")
      (lambda ()
        (let ((*load-pathname* nil) (*load-truename* nil))
          (eval '(asdf:defsystem "crs-uses-quicklisp" :depends-on ("quicklisp"))))
        (asdf:load-system "crs-uses-quicklisp")
        :loaded))
    :loaded))
