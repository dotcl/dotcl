;;; DotclHost.Call — the host API a C# program uses to call into Lisp.
;;;
;;; The string it takes is a SYMBOL NAME, matched exactly, and an unqualified
;;; one means the current package. Neither half is obvious, and each replaced a
;;; rule that read as convenience and behaved as a surprise:
;;;
;;;   * Case folding ("greet" finding GREET) would be a second naming rule
;;;     beside the reader's. Whichever way it leaned, one of GREET and |greet|
;;;     becomes unreachable, or changes meaning the day the other is defined.
;;;   * Searching every package for an unqualified name made a working call
;;;     start failing as ambiguous the day an unrelated library defined the same
;;;     name, and hid which package had answered.
;;;
;;; Both survive as hints in the error instead: a miss says which spelling and
;;; which package would have worked. Neither participates in resolution, so what
;;; a host string means does not depend on what else is loaded -- nor on the
;;; readtable, since no reader runs over it.

(defpackage :hostcall-a (:use :cl) (:export #:entry))
(defpackage :hostcall-b (:use :cl))

(defun hostcall-a::entry (x) (format nil "a:~a" x))
(defun hostcall-b::other (x) (format nil "b:~a" x))

(defun %host-call (name &rest args)
  (apply #'dotnet:static "DotCL.DotclHost" "Call" name args))

(defun %host-call-error (name &rest args)
  "The error message, so the test can assert what the caller is told."
  (handler-case (progn (apply #'%host-call name args) :no-error)
    (error (e) (princ-to-string e))))

;;; A name is matched exactly: the reader upcased GREET, so that is its name.
(deftest host-call-name-is-exact
  (%host-call "STRING-UPCASE" "abc")
  "ABC")

;;; The source spelling does not resolve -- and the message names the one that
;;; does, because that is nearly always what the caller meant.
(deftest host-call-lowercase-spelling-is-told-what-to-write
  (let ((msg (%host-call-error "string-upcase" "abc")))
    (list (and (search "no function named string-upcase" msg) t)
          (and (search "\"STRING-UPCASE\" does exist" msg) t)))
  (t t))

;;; An unqualified name means the current package and nothing else. ENTRY lives
;;; in HOSTCALL-A, which CL-USER does not use.
(deftest host-call-unqualified-is-current-package
  (let ((msg (%host-call-error "ENTRY" "x")))
    (list (and (search "no function named ENTRY" msg) t)
          (and (search "defined in HOSTCALL-A" msg) t)))
  (t t))

;;; Qualified names work, and one colon means the exported surface.
(deftest host-call-qualified
  (list (%host-call "HOSTCALL-A:ENTRY" 1)
        (%host-call "HOSTCALL-B::OTHER" 2))
  ("a:1" "b:2"))

;;; A single colon on an internal symbol is refused, with the spelling that
;;; reaches it anyway.
(deftest host-call-single-colon-wants-an-external-symbol
  (let ((msg (%host-call-error "HOSTCALL-B:OTHER" 2)))
    (list (and (search "does not export OTHER" msg) t)
          (and (search "HOSTCALL-B::OTHER" msg) t)))
  (t t))

;;; Setting the current package is how a host reaches a library's names without
;;; qualifying every call. It reads and writes the same *PACKAGE* Lisp sees.
(deftest host-call-current-package-round-trip
  (let ((before (dotnet:static "DotCL.DotclHost" "CurrentPackage")))
    (unwind-protect
         (progn
           (setf (dotnet:static "DotCL.DotclHost" "CurrentPackage") "HOSTCALL-A")
           (list (dotnet:static "DotCL.DotclHost" "CurrentPackage")
                 (%host-call "ENTRY" "x")))
      (setf (dotnet:static "DotCL.DotclHost" "CurrentPackage") before)))
  ("HOSTCALL-A" "a:x"))

;;; A name nothing defines still reports the missing binding.
(deftest host-call-undefined
  (handler-case (progn (%host-call "NO-SUCH-HOST-ENTRY-POINT") :no-error)
    (error () :error))
  :error)
