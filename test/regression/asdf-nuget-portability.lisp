;;; asdf-nuget's portability check: finds reader conditionals and compile-time
;;; code that tie a fasl to the platform it was compiled on, by reading the
;;; source text of the files ASDF lists for a system.
(require "asdf-nuget")

(defun %fc-write (name text)
  (let ((path (regression-temp-file name)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(defun %fc-kinds (findings)
  (mapcar (lambda (f) (list (asdf-nuget:finding-line f) (asdf-nuget:finding-kind f)))
          findings))

(defparameter *fc-sample*
  (format nil "~{~A~%~}"
          '("(in-package :cl-user)"
            "#+windows (defun a () 1)"
            "#-(or win32 mswindows) (defun a () 2)"
            "#+64-bit (defun b () 3)"
            "#+sbcl (progn #+windows (x) #+darwin (y))"
            "(defconstant +sep+ (if (uiop:os-windows-p) #\\\\ #\\/))"
            "(defmacro m () (if (uiop:os-windows-p) 'w 'u))"
            "(defmacro ok () `(if (uiop:os-windows-p) 'w 'u))"
            "(eval-when (:compile-toplevel :load-toplevel :execute)"
            "  (when (member :x86-64 *features*) (pushnew :my-x64 *features*))"
            "  (pushnew :my-lib *features*)"
            "  (defun helper () (machine-type)))"
            "(defparameter *here* #.(or *compile-file-truename* *load-truename*))"
            "(cffi:defcstruct foo (a :long) (b :int))"
            "(defun run () (when (uiop:os-windows-p) 1))"
            "#\\( \"str ( #+windows\" ; #+windows"
            "#| #+windows |#"
            "(defun z () #+arm64 1 #-arm64 2)"
            "(defmacro s () (cffi:foreign-type-size :pointer))"
            "(defmacro l () (cffi:foreign-type-size :long))")))

(deftest asdf-nuget-portability.file-kinds
  (%fc-kinds (asdf-nuget::check-file (%fc-write "fc/sample.lisp" *fc-sample*)))
  ((2 :read-platform) (3 :read-platform) (4 :platform-constant)
   (6 :compile-probe) (7 :compile-probe) (8 :runtime-check)
   (10 :compile-probe) (13 :compile-environment) (13 :compile-environment)
   (14 :c-layout)
   (15 :runtime-check) (18 :read-platform) (18 :read-platform)
   (20 :compile-probe)))

(deftest asdf-nuget-portability.read-platform-targets
  (asdf-nuget:finding-detail
   (first (asdf-nuget::check-file
           (%fc-write "fc/one.lisp" (format nil "#-windows (defun f ())~%")))))
  "#-windows is true on linux-x64 linux-arm64 osx-x64 osx-arm64 only")

;;; The files are the ones ASDF lists for the system. A component for another
;;; implementation is not read; a component chosen by platform is noted without
;;; making the other files' fasls platform-dependent; a system of the same name
;;; that the system depends on is checked with it, a test system it does not
;;; depend on is not; a component that runs C on the build machine is reported
;;; from its class.

(defclass cl-user::grovel-file (asdf:cl-source-file) ())

(defparameter *fc-system*
  (progn
    (%fc-write "fclib/pc-lib.asd"
               (format nil "~{~A~%~}"
                       '("(defsystem \"pc-lib\" :depends-on (\"pc-lib/extra\")"
                         "  :components ((:file \"a\")"
                         "  (:file \"b\" :if-feature :sbcl)"
                         "  (:file \"w\" :if-feature :windows)"
                         "  (cl-user::grovel-file \"g\")"
                         "  (:module \"src\" :components ((:file \"c\")))))"
                         "(defsystem \"pc-lib/extra\" :components ((:file \"e\")))"
                         "(defsystem \"pc-lib/tests\" :components ((:file \"t\")))")))
    (%fc-write "fclib/a.lisp" "(defun a () 1)")
    (%fc-write "fclib/b.lisp" "#+darwin (defun b ())")
    (%fc-write "fclib/w.lisp" "(defun w () 1)")
    (%fc-write "fclib/g.lisp" "#+darwin (defun g ())")
    (%fc-write "fclib/e.lisp" "(defun e () (uiop:os-windows-p))")
    (%fc-write "fclib/t.lisp" "#+darwin (defun tt ())")
    (%fc-write "fclib/src/c.lisp" "(defmacro c () (machine-type))")
    (asdf:load-asd (regression-temp-file "fclib/pc-lib.asd"))
    "pc-lib"))

(deftest asdf-nuget-portability.system-follows-asdf
  (multiple-value-bind (findings files) (asdf-nuget:check-portability *fc-system*)
    (list (mapcar #'file-namestring files)
          (mapcar #'asdf-nuget:finding-kind findings)
          (asdf-nuget::summarize findings)))
  (("a.lisp" "w.lisp" "c.lisp" "e.lisp")
   (:asd-platform-component :c-probe :compile-probe :runtime-check)
   ((:compile-probe . 1) (:c-probe . 1) (:asd-platform-component . 1)
    (:runtime-check . 1))))

(deftest asdf-nuget-portability.report-result
  (let ((s (make-string-output-stream)))
    (list (asdf-nuget:report-portability
           (asdf-nuget::check-file (%fc-write "fc/clean.lisp" "(defun f () (uiop:os-windows-p))"))
           :stream s)
          (asdf-nuget:report-portability
           (asdf-nuget::check-file (%fc-write "fc/dirty.lisp" "#+unix (defun f ())"))
           :stream s)))
  (t nil))

;;; The operation prints the report, and runs again when asked again: the
;;; sources may have changed, and a check is never done.
(deftest asdf-nuget-portability.operation-reports-every-time
  (let ((out (with-output-to-string (*standard-output*)
               (asdf:operate 'asdf-nuget:check-portability-op *fc-system*)
               (asdf:operate 'asdf-nuget:check-portability-op *fc-system*))))
    (list (loop with start = 0 for p = (search "Counts:" out :start2 start)
                while p count t do (setf start (1+ p)))
          (and (search "compile-probe: MACHINE-TYPE in DEFMACRO C" out) t)))
  (2 t))
