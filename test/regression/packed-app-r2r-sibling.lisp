;;; A packed application loads its own fasl by path, so its ReadyToRun sibling
;;; counts.
;;;
;;; The launcher used to read dotcl.user.fasl into a byte array and hand that to
;;; Assembly.Load. An assembly loaded that way is not file-backed, so .NET
;;; ignores any ReadyToRun code in it: the runtime and the core underneath a
;;; packed tool were native while the tool itself was JITted at every start.
;;; LOAD already had the answer (<name>.fasl.r2r-<rid> beside the fasl, taken by
;;; path); the launcher now asks the same question.
;;;
;;; Proving that native code ran would mean calling crossgen2 from the suite,
;;; which is minutes of work for a fact this test does not need. What it checks
;;; is the choice: give the two files *different programs* and let the child say
;;; which one it ran. The staleness rule comes along for free -- a sibling older
;;; than its fasl is left over from an earlier build and must not win.
;;;
;;; The child runs as `dotnet <stem>.dll` out of a copy holding nothing but the
;;; launcher: no execute bit has to survive the copy, and --core points at this
;;; tree so the copy needs no core of its own.

(defvar *par-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *par-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun %par-tmp (name)
  (concatenate 'string
               (substitute #\/ (code-char 92)
                           (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp"))
               "/" name))

(defun %par-portable-rid ()
  "The os-arch spelling the tooling writes siblings under.
Derived rather than asked for: there is no Lisp-visible accessor, and a wrong
answer here makes the fresh-sibling case fail rather than quietly pass."
  (concatenate 'string
               (cond ((dotnet:static "System.OperatingSystem" "IsWindows") "win")
                     ((dotnet:static "System.OperatingSystem" "IsMacOS") "osx")
                     (t "linux"))
               "-"
               (string-downcase
                (dotnet:invoke
                 (dotnet:static "System.Runtime.InteropServices.RuntimeInformation"
                                "get_ProcessArchitecture")
                 "ToString"))))

(defun %par-combine (dir name) (dotnet:static "System.IO.Path" "Combine" dir name))

(defun %par-compile-printer (dir stem text)
  "Compile a fasl whose whole job is to print TEXT, and return its path."
  (let ((source (concatenate 'string dir "/" stem ".lisp"))
        (fasl (concatenate 'string dir "/" stem ".fasl")))
    (with-open-file (s source :direction :output :if-exists :supersede)
      (format s "(format t \"~~&~a~~%\")" text))
    (compile-file source :output-file fasl)
    fasl))

(defun %par-layout ()
  "Lay out a launcher-only copy with a plain fasl and a differing sibling.
Returns (dll fasl sibling)."
  (let* ((dir (%par-tmp "dotcl-packed-r2r"))
         (src (dotnet:static "System.IO.Path" "GetDirectoryName" *par-exe*))
         (stem (dotnet:static "System.IO.Path" "GetFileNameWithoutExtension" *par-exe*))
         (fasl (concatenate 'string dir "/dotcl.user.fasl"))
         (sibling (concatenate 'string dir "/dotcl.user.fasl.r2r-" (%par-portable-rid))))
    (dotnet:static "System.IO.Directory" "CreateDirectory" dir)
    (dolist (name (list (dotnet:static "System.IO.Path" "GetFileName" *par-exe*)
                        (concatenate 'string stem ".dll")
                        (concatenate 'string stem ".deps.json")
                        (concatenate 'string stem ".runtimeconfig.json")
                        "DotCL.Runtime.dll"))
      (dotnet:static "System.IO.File" "Copy" (%par-combine src name)
                     (%par-combine dir name) t))
    (dotnet:static "System.IO.File" "Copy"
                   (%par-compile-printer dir "par-plain" "PLAIN-IMAGE") fasl t)
    (dotnet:static "System.IO.File" "Copy"
                   (%par-compile-printer dir "par-sib" "SIBLING-IMAGE") sibling t)
    (list (concatenate 'string dir "/" stem ".dll") fasl sibling)))

(defun %par-run (dll)
  (second (dotcl:run-process "dotnet" (list dll "--core" *par-core*))))

(defun %par-which (stale)
  "Run the packed copy and say which image it chose.
STALE backdates the sibling first."
  (destructuring-bind (dll fasl sibling) (%par-layout)
    (when stale
      (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" sibling
                     (dotnet:invoke
                      (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" fasl)
                      "AddDays" -1.0d0)))
    (let ((out (%par-run dll)))
      (cond ((search "SIBLING-IMAGE" out) :sibling)
            ((search "PLAIN-IMAGE" out) :plain)
            (t out)))))

(deftest packed-app-r2r-sibling.sibling-is-preferred
  (%par-which nil)
  :sibling)

;;; Left over from an earlier build: the fasl has been recompiled since, so the
;;; sibling no longer describes it.
(deftest packed-app-r2r-sibling.stale-sibling-is-ignored
  (%par-which t)
  :plain)
