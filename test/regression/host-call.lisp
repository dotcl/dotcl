;;; DotclHost.Call: the host API a C# program uses to call into Lisp.
;;;
;;; The string it takes is read the way the Lisp reader reads a symbol token:
;;; unescaped letters follow the current readtable's case, |...| and \ escape,
;;; and PKG:NAME / PKG::NAME qualify, the package name read the same way. That
;;; is one rule, the reader's, so a string means one thing: "fact" calls what
;;; (defun fact ...) defined, "|fact|" a symbol whose name is lowercase.
;;;
;;; An unqualified name means the current package and nothing else: searching
;;; every package made a working call start failing as ambiguous the day an
;;; unrelated library defined the same name, and hid which package had
;;; answered. That survives as a hint in the error instead: a miss names the
;;; packages that do have the function, and a symbol whose name differs only in
;;; case, with the spelling that reaches it.

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

;;; Lower and upper case both read as the reader reads them: STRING-UPCASE.
(deftest host-call-name-is-read
  (list (%host-call "STRING-UPCASE" "abc")
        (%host-call "string-upcase" "abc")
        (%host-call "String-Upcase" "abc"))
  ("ABC" "ABC" "ABC"))

;;; |...| keeps the case, so a lowercase symbol and its upcased namesake are
;;; both reachable, each by the spelling the reader gives it.
(defun |hostcall-lower| () :lower)
(defun hostcall-lower () :upper)
(deftest host-call-bars-keep-case
  (list (%host-call "|hostcall-lower|")
        (%host-call "hostcall-lower")
        (%host-call "HOSTCALL-LOWER")
        (%host-call "|HOSTCALL-LOWER|")
        (%host-call "\\h\\o\\s\\t\\c\\a\\l\\l-\\l\\o\\w\\e\\r"))
  (:lower :upper :upper :upper :lower))

;;; A miss on a symbol whose name differs only in case says how to write it.
(defun |hostcallMixed| () :mixed)
(deftest host-call-miss-names-the-spelling
  (let ((msg (%host-call-error "hostcallmixed")))
    (list (and (search "no function named HOSTCALLMIXED" msg) t)
          (and (search "hostcallMixed is written \"|hostcallMixed|\"" msg) t)
          (%host-call "|hostcallMixed|")))
  (t t :mixed))

;;; An unqualified name means the current package and nothing else. ENTRY lives
;;; in HOSTCALL-A, which CL-USER does not use.
(deftest host-call-unqualified-is-current-package
  (let ((msg (%host-call-error "ENTRY" "x")))
    (list (and (search "no function named ENTRY" msg) t)
          (and (search "defined in HOSTCALL-A" msg) t)))
  (t t))

;;; Qualified names work, the package name read like the symbol name, and one
;;; colon means the exported surface.
(deftest host-call-qualified
  (list (%host-call "HOSTCALL-A:ENTRY" 1)
        (%host-call "HOSTCALL-B::OTHER" 2)
        (%host-call "hostcall-a:entry" 3)
        (%host-call "hostcall-b::other" 4)
        (%host-call "|HOSTCALL-A|:|ENTRY|" 5))
  ("a:1" "b:2" "a:3" "b:4" "a:5"))

;;; Under a readtable whose case is not :UPCASE, the host string is read by that
;;; readtable, as source text would be.
(deftest host-call-readtable-case
  (flet ((with-case (case &rest names)
           (let ((*readtable* (copy-readtable nil)))
             (setf (readtable-case *readtable*) case)
             (mapcar #'%host-call names))))
    (list (with-case :preserve "hostcall-lower" "HOSTCALL-LOWER")
          (with-case :invert "hostcall-lower" "HOSTCALL-LOWER")
          (with-case :downcase "HOSTCALL-LOWER" "|HOSTCALL-LOWER|")))
  ((:lower :upper) (:upper :lower) (:lower :upper)))

;;; Text that is not a symbol token is refused as such.
(deftest host-call-not-a-symbol-name
  (mapcar (lambda (name) (and (search "is not a symbol name" (%host-call-error name)) t))
          '("a:b:c" "|open" "pkg:" ""))
  (t t t t))

;;; GetSpecial reads the name the same way.
(deftest host-call-get-special-is-read
  (list (dotnet:static "DotCL.DotclHost" "GetSpecial" "*print-base*")
        (dotnet:static "DotCL.DotclHost" "GetSpecial" "cl:*print-base*"))
  (10 10))

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
           (setf (dotnet:static "DotCL.DotclHost" "CurrentPackage") "hostcall-a")
           (list (dotnet:static "DotCL.DotclHost" "CurrentPackage")
                 (%host-call "entry" "x")))
      (setf (dotnet:static "DotCL.DotclHost" "CurrentPackage") before)))
  ("HOSTCALL-A" "a:x"))

;;; A name nothing defines still reports the missing binding.
(deftest host-call-undefined
  (handler-case (progn (%host-call "NO-SUCH-HOST-ENTRY-POINT") :no-error)
    (error () :error))
  :error)
