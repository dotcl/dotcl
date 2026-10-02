;;; nuget.lisp: transitive NuGet dependency resolution for dotcl
;;;
;;; Usage:
;;;   (require "dotcl-nuget")
;;;   (nuget:require "Avalonia.Desktop" :version "11.3.18")
;;;   (nuget:require "SkiaSharp")                       ; latest stable
;;;   (nuget:require "SkiaSharp" :prerelease t)         ; latest incl. prerelease
;;;   (nuget:require "MyLib" :source "https://my.feed/v3/index.json")
;;;
;;; Identifying axes other than the package name are keywords (:version :source
;;; :prerelease :rid :tfm) so more can be added without positional churn.
;;;
;;; No NuGet client library is linked into the runtime. Resolution is done by the
;;; external `dotnet' CLI in two steps, both on throwaway projects:
;;;
;;;   1. RESTORE decides versions. A project with one PackageReference per request
;;;      and no RuntimeIdentifier is restored with NuGet's own lock file switched
;;;      on, and the lock file's "resolved" entries -- every package of the
;;;      transitive closure, at one version each -- are the answer ("the pins").
;;;   2. LAY OUT puts the files down. A project that references every pin at
;;;      exactly that version, for one RuntimeIdentifier, is built into a
;;;      directory; build flattens the graph (managed and RID-specific native) into
;;;      the output directory, so registering it is a directory scan.
;;;
;;; Splitting the two is what lets one lock file serve every platform: the versions
;;; are decided once without a RID, and each RID lays out the same versions.
;;;
;;; Resolution is per image, not per package. Every package asked for in a session
;;; joins one set, and the set is resolved together, so two packages that depend
;;; on different versions of a third are unified by NuGet (or refused by it)
;;; instead of each registering its own copy. Once a version is registered it
;;; stays: a later resolution that would move it is an error, since the old one
;;; may already be loaded and .NET cannot unload it.
;;;
;;; Declared requests -- the ones a system definition makes, see dotcl-nuget-asdf --
;;; follow a stricter rule than NUGET:REQUIRE typed by hand, because they act on
;;; someone else's machine on some later day:
;;;
;;;   - versions come from the project's lock file, dotcl-nuget.lock.json in
;;;     *PROJECT-DIRECTORY* (default: the current directory), when it records them
;;;   - otherwise an exact version is resolved and recorded there, and the
;;;     packages being fetched are named on *ERROR-OUTPUT*
;;;   - a floating version ("13.*", or none at all) or a range is refused, with a
;;;     pointer to NUGET:RESTORE, which resolves and records it on request
;;;
;;; DOTCL_NUGET_OFFLINE=1 in the environment turns every network-capable step into
;;; an error: only a bundled layout, or a lock file plus an already laid-out cache,
;;; can answer.

(defpackage :nuget
  (:use :cl)
  (:shadow #:require)
  (:export #:require #:resolve #:restore #:*project-directory*
           #:cache-root #:bundled-root #:stage-bundle))

(in-package :nuget)

(defun %current-rid ()
  "The .NET RuntimeIdentifier of the running process (e.g. \"win-arm64\")."
  (dotnet:static "System.Runtime.InteropServices.RuntimeInformation"
                 "get_RuntimeIdentifier"))

(defun %current-tfm ()
  "The target framework moniker for the running runtime (e.g. \"net10.0\")."
  (let ((major (dotnet:invoke
                (dotnet:static "System.Environment" "get_Version") "get_Major")))
    (format nil "net~D.0" major)))

(defun %combine (&rest parts)
  (reduce (lambda (a b) (dotnet:static "System.IO.Path" "Combine" a b)) parts))

(defun %write-text (path text)
  (dotnet:static "System.IO.File" "WriteAllText" path text))

(defun %read-text (path)
  (when (dotnet:static "System.IO.File" "Exists" path)
    (dotnet:static "System.IO.File" "ReadAllText" path)))

(defun %temp-project-dir ()
  "A fresh temp directory for a throwaway project."
  (let* ((base (dotnet:static "System.IO.Path" "GetTempPath"))
         (name (format nil "dotcl-nuget-~A"   ; temp-dir prefix; kept for grep-ability
                       (dotnet:invoke (dotnet:static "System.Guid" "NewGuid") "ToString" "N")))
         (dir (%combine base name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory" dir)
    dir))

(defvar *cache-directory* nil
  "When non-NIL, used instead of CACHE-ROOT's own answer. For tests.")

(defvar *bundle-directory* nil
  "When non-NIL, used instead of BUNDLED-ROOT's own answer. For tests.")

(defvar *project-directory* nil
  "The directory whose dotcl-nuget.lock.json records the versions a project's
declared packages resolved to. NIL means the process's current directory, which
is where `dotnet restore' would look for a project too.")

(defun cache-root ()
  "Where laid-out packages are kept, so a later process can reuse them.

A sibling of the fasl cache rather than a path of its own: that one already
decides where dotcl may write on this platform (XDG_CACHE_HOME, LOCALAPPDATA,
~/.cache), and having two answers to the same question is how they drift apart."
  (or *cache-directory*
      (%combine (dotnet:static "System.IO.Path" "GetDirectoryName"
                               (funcall (find-symbol "%FASL-CACHE-ROOT" "DOTCL")))
                "dotcl-nuget")))

(defun bundled-root ()
  "Where a packaged application carries the packages it was built against.

`dotcl pack --bundle DIR' copies DIR next to the installed executable, so a
layout under DIR/nuget/ arrives as a sibling of the program. Looking there first
is what lets a shipped application start on a machine with no .NET SDK and no
network -- resolving otherwise means running `dotnet build'."
  (or *bundle-directory*
      (let ((exe (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))))
        (when exe
          (%combine (dotnet:static "System.IO.Path" "GetDirectoryName" exe) "nuget")))))

(defun %offline-p ()
  "True when DOTCL_NUGET_OFFLINE asks for no network at all."
  (let ((v (dotcl:getenv "DOTCL_NUGET_OFFLINE")))
    (and v (plusp (length v)) (not (string= v "0")))))

(defun %project-dir ()
  (if *project-directory*
      (namestring *project-directory*)
      (dotnet:static "System.IO.Directory" "GetCurrentDirectory")))

(defun %lock-path ()
  "The project's lock file. Not packages.lock.json: a dotcl project is often also
a .NET project, and that name belongs to its own restore."
  (%combine (%project-dir) "dotcl-nuget.lock.json"))

;;; --- requests ---------------------------------------------------------------

(defstruct (req (:constructor make-req (id spec source origin)))
  id           ; package id as written
  spec         ; NuGet version spec, never NIL ("*" when omitted)
  source       ; extra feed URI or NIL
  origin       ; :declared (a system definition) or :imperative (NUGET:REQUIRE)
  (resolved nil))

(defun %spec (version prerelease)
  ;; A PackageReference requires a version; the SDK errors (NU1604) on a bare Include.
  (or version (if prerelease "*-*" "*")))

(defun %exact-version-p (version)
  "True when VERSION names one release rather than a moving target.

A floating spec (\"*\", \"13.*\", \"*-*\") or a range asks for whatever is newest
(or lowest, for a range, which is still a property of what the feed holds today
rather than of the source), so it is not something a later load can repeat."
  (and (stringp version)
       (plusp (length version))
       (not (find-if (lambda (c) (find c "*[]() ,")) version))))

(defun %split (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defun %normalize-spec (spec)
  "What NuGet writes as \"requested\" in a lock file for SPEC, or NIL when that
is not known here. Used to see whether a lock file already answers a request
without starting NuGet; NIL makes the caller ask NuGet instead."
  (cond ((find-if (lambda (c) (find c "[](), ")) spec) nil)
        ((find #\* spec) (format nil "[~A, )" spec))
        (t
         (let* ((dash (position #\- spec))
                (num (subseq spec 0 dash))
                (parts (%split num #\.)))
           (when (and (<= 1 (length parts) 3)
                      (every (lambda (p) (and (plusp (length p)) (every #'digit-char-p p)))
                             parts))
             (format nil "[~{~A~^.~}~@[~A~], )"
                     (append parts (make-list (- 3 (length parts)) :initial-element "0"))
                     (and dash (subseq spec dash))))))))

;;; --- lock files -------------------------------------------------------------

(defun %json-section (path tfm)
  "The JsonElement for TFM's section of the lock file at PATH, or NIL."
  (let ((text (%read-text path)))
    (when text
      (let* ((doc (dotnet:static "System.Text.Json.JsonDocument" "Parse" text))
             (deps (dotnet:invoke (dotnet:invoke doc "get_RootElement")
                                  "GetProperty" "dependencies"))
             (e (dotnet:invoke deps "EnumerateObject")))
        (loop while (dotnet:invoke e "MoveNext")
              do (let ((p (dotnet:invoke e "get_Current")))
                   (when (string= (dotnet:invoke p "get_Name") tfm)
                     (return (dotnet:invoke p "get_Value")))))))))

(defun %json-string (element name)
  "The string property NAME of the JSON object ELEMENT, or NIL."
  (let ((e (dotnet:invoke element "EnumerateObject")))
    (loop while (dotnet:invoke e "MoveNext")
          do (let ((p (dotnet:invoke e "get_Current")))
               (when (string= (dotnet:invoke p "get_Name") name)
                 (return (dotnet:invoke (dotnet:invoke p "get_Value") "ToString")))))))

(defun %read-lock (path tfm)
  "Read the lock file at PATH for TFM. Returns (values directs pins present-p):
DIRECTS is a list of (id . requested) for the direct references, PINS a list of
(id . resolved) for every package in the closure."
  (let ((section (%json-section path tfm))
        (directs '())
        (pins '()))
    (when section
      (let ((e (dotnet:invoke section "EnumerateObject")))
        (loop while (dotnet:invoke e "MoveNext")
              do (let* ((p (dotnet:invoke e "get_Current"))
                        (id (dotnet:invoke p "get_Name"))
                        (v (dotnet:invoke p "get_Value"))
                        (type (%json-string v "type"))
                        (resolved (%json-string v "resolved"))
                        (requested (%json-string v "requested")))
                   (when resolved (push (cons id resolved) pins))
                   (when (and requested (equal type "Direct"))
                     (push (cons id requested) directs))))))
    (values (nreverse directs) (nreverse pins) (and section t))))

(defun %covered-p (directs req &optional pins)
  "True when the lock answers REQ: its direct references record REQ as written,
or REQ names an exact version and the closure (PINS) already has the package at
exactly that version -- a library's own declaration of a package its consumer
already pulled in transitively."
  (let ((d (assoc (req-id req) directs :test #'string-equal))
        (p (assoc (req-id req) pins :test #'string-equal)))
    (or (and d (equal (%normalize-spec (req-spec req)) (cdr d)))
        (and p (%exact-version-p (req-spec req))
             (string-equal (cdr p) (req-spec req))))))

;;; --- running dotnet ---------------------------------------------------------

(defun %run-dotnet (arg-string working-dir)
  "Run `dotnet ARG-STRING` in WORKING-DIR. Returns (values exit-code stdout+stderr)."
  (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo")))
    (dotnet:invoke psi "set_FileName" "dotnet")
    (dotnet:invoke psi "set_Arguments" arg-string)
    (dotnet:invoke psi "set_WorkingDirectory" working-dir)
    (dotnet:invoke psi "set_UseShellExecute" nil)
    (dotnet:invoke psi "set_RedirectStandardOutput" t)
    (dotnet:invoke psi "set_RedirectStandardError" t)
    (dotnet:invoke psi "set_CreateNoWindow" t)
    (let* ((proc (dotnet:static "System.Diagnostics.Process" "Start" psi))
           (out (dotnet:invoke (dotnet:invoke proc "get_StandardOutput") "ReadToEnd"))
           (err (dotnet:invoke (dotnet:invoke proc "get_StandardError") "ReadToEnd")))
      (dotnet:invoke proc "WaitForExit")
      (values (dotnet:invoke proc "get_ExitCode")
              (concatenate 'string out err)))))

(defun %xml-escape (s)
  (with-output-to-string (o)
    (loop for c across s
          do (case c
               (#\& (write-string "&amp;" o))
               (#\< (write-string "&lt;" o))
               (#\> (write-string "&gt;" o))
               (#\" (write-string "&quot;" o))
               (t (write-char c o))))))

(defun %csproj (refs rid tfm)
  "A project referencing REFS, a list of (id . version-spec), for TFM and, when
RID is non-NIL, that RuntimeIdentifier."
  (format nil "<Project Sdk=\"Microsoft.NET.Sdk\">~%~
  <PropertyGroup>~%~
    <TargetFramework>~A</TargetFramework>~%~
~@[    <RuntimeIdentifier>~A</RuntimeIdentifier>~%~]~
    <Nullable>disable</Nullable>~%~
    <EnableDefaultItems>false</EnableDefaultItems>~%~
    <CopyLocalLockFileAssemblies>true</CopyLocalLockFileAssemblies>~%~
  </PropertyGroup>~%~
  <ItemGroup>~%~
~{    <PackageReference Include=\"~A\" Version=\"~A\" />~%~}~
  </ItemGroup>~%~
</Project>~%"
          tfm rid
          (loop for (id . v) in refs
                append (list (%xml-escape id) (%xml-escape v)))))

(defun %sources-arg (sources)
  ;; Additional, not replacing: a private feed is asked for on top of nuget.org.
  (if sources
      (format nil " \"-p:RestoreAdditionalProjectSources=~{~A~^%3B~}\"" sources)
      ""))

(defun %refuse-offline (what)
  (error "nuget: ~A needs the network, and DOTCL_NUGET_OFFLINE is set." what))

(defun %restore-lock (refs tfm sources lock-path &key locked force)
  "Restore a project referencing REFS with NuGet's lock file at LOCK-PATH.
LOCKED refuses to change the lock file; FORCE ignores the versions it records
(NuGet otherwise keeps them while the references are unchanged).
Returns (values exit-code log)."
  (when (%offline-p) (%refuse-offline "Resolving package versions"))
  (let* ((proj-dir (%temp-project-dir))
         (csproj (%combine proj-dir "proj.csproj")))
    (%write-text csproj (%csproj refs nil tfm))
    (%run-dotnet (format nil "restore \"~A\" -p:RestorePackagesWithLockFile=true ~
\"-p:NuGetLockFilePath=~A\"~:[~; -p:RestoreLockedMode=true~]~:[~; --force-evaluate~]~A"
                         csproj lock-path locked force (%sources-arg sources))
                 proj-dir)))

(defun %restore-or-error (refs tfm sources lock-path &key force note)
  (multiple-value-bind (code log) (%restore-lock refs tfm sources lock-path :force force)
    (unless (zerop code)
      (error "nuget: `dotnet restore' failed (exit ~D) for ~{~A ~A~^, ~}:~%~A~@[~%~A~]"
             code (loop for (id . v) in refs append (list id v)) log note))))

;;; --- layouts ----------------------------------------------------------------

(defun %sort-pins (pins)
  (sort (copy-list pins) #'string< :key (lambda (p) (string-downcase (car p)))))

(defun %layout-key (pins rid tfm)
  "A directory name that stands for exactly this set of package versions on RID
and TFM. The versions are all exact, so the same key is the same bytes."
  (let* ((canon (format nil "~{~A~^;~}|~A|~A"
                        (mapcar (lambda (p) (format nil "~(~A~)/~A" (car p) (cdr p)))
                                (%sort-pins pins))
                        rid tfm))
         (bytes (dotnet:invoke (dotnet:static "System.Text.Encoding" "get_UTF8")
                               "GetBytes" canon))
         (hash (dotnet:static "System.Security.Cryptography.SHA256" "HashData" bytes))
         (hex (string-downcase (dotnet:static "System.Convert" "ToHexString" hash))))
    (format nil "~A_~A_~A" rid tfm (subseq hex 0 16))))

(defun %marker (dir) (%combine dir "dotcl-nuget-complete"))

(defun %layout-complete-p (dir)
  "True when DIR holds a finished layout.

A build that died halfway leaves a directory behind, and reusing that would be
worse than rebuilding: the assemblies that did get copied would register and the
missing ones would surface much later as a type that cannot be resolved. The
marker file is written last, so its presence means the build returned 0."
  (and (dotnet:static "System.IO.Directory" "Exists" dir)
       (dotnet:static "System.IO.File" "Exists" (%marker dir))))

(defun %write-marker (dir rid tfm pins reqs)
  "Record what DIR holds: the versions, and the requests they answer. The
requests are what a bundled copy is matched against."
  (%write-text (%marker dir)
               (with-standard-io-syntax
                 (prin1-to-string
                  (list :rid rid :tfm tfm :pins (%sort-pins pins)
                        :requests (mapcar (lambda (r) (list (req-id r) (req-spec r)))
                                          reqs))))))

(defun %read-marker (dir)
  (let ((text (%read-text (%marker dir))))
    (when text
      (ignore-errors
       (with-standard-io-syntax
         (let ((*read-eval* nil)
               (*package* (find-package "NUGET")))
           (let ((m (read-from-string text)))
             (and (consp m) (getf m :pins) m))))))))

(defun %lay-out (pins rid tfm sources)
  "Put PINS down for RID and TFM under the cache, unless that is already done.
Returns the directory."
  (let ((dir (%combine (cache-root) (%layout-key pins rid tfm))))
    (unless (%layout-complete-p dir)
      (when (%offline-p) (%refuse-offline "Laying out packages"))
      (let* ((proj-dir (%temp-project-dir))
             (csproj (%combine proj-dir "proj.csproj")))
        (%write-text csproj (%csproj (mapcar (lambda (p) (cons (car p) (format nil "[~A]" (cdr p))))
                                             (%sort-pins pins))
                                     rid tfm))
        (multiple-value-bind (code log)
            (%run-dotnet (format nil "build \"~A\" -c Release -o \"~A\"~A"
                                 csproj dir (%sources-arg sources))
                         proj-dir)
          (unless (zerop code)
            (error "nuget: `dotnet build' failed (exit ~D) laying out for ~A:~%~A"
                   code rid log)))
        (%copy-lisp-systems pins dir)))
    dir))

;;; --- Lisp systems carried in packages ---------------------------------------
;;;
;;; A package can carry ASDF systems under dotcl/<system>/ (see dotcl-nuget-asdf).
;;; The build lays out assemblies only, so those trees are copied from NuGet's
;;; package folder into the layout's own dotcl/, which then travels with the
;;; layout into the cache and into a bundle; registering a layout puts each
;;; system directory on ASDF's central registry.

(defvar *packages-directory* nil
  "When non-NIL, used instead of NuGet's global packages folder. For tests.")

(defun %packages-root ()
  "NuGet's global packages folder, where restore extracts every package."
  (or *packages-directory*
      (let ((env (dotcl:getenv "NUGET_PACKAGES")))
        (and env (plusp (length env)) env))
      (%combine (namestring (user-homedir-pathname)) ".nuget" "packages")))

(defun %copy-lisp-systems (pins dir)
  "Copy the dotcl/ tree of every package in PINS that has one into DIR/dotcl/.
Returns how many packages had one."
  (let ((n 0)
        (now (dotnet:static "System.DateTime" "get_UtcNow")))
    (dolist (p pins)
      (let ((src (%combine (%packages-root) (string-downcase (car p))
                           (string-downcase (cdr p)) "dotcl")))
        (when (dotnet:static "System.IO.Directory" "Exists" src)
          (%copy-tree src (%combine dir "dotcl"))
          (incf n))))
    ;; A zip entry's time has no zone, and NuGet extracts it as if it were UTC,
    ;; so east of Greenwich every source arrives dated hours in the future -- and
    ;; ASDF, finding the source newer than any fasl, compiles it again. The copy
    ;; time is the honest date for files that appeared just now.
    (when (plusp n)
      (dolist (f (%dir-files-recursive (%combine dir "dotcl") "*"))
        (%touch f now)))
    n))

(defun %register-lisp-systems (dir)
  "Put each system directory under DIR/dotcl/ on ASDF's central registry, when
ASDF is loaded. Returns the directories."
  (let ((registry (let ((pkg (find-package "ASDF")))
                    (and pkg (find-symbol "*CENTRAL-REGISTRY*" pkg))))
        (dirs (mapcar (lambda (d) (pathname (concatenate 'string (substitute #\/ #\\ d) "/")))
                      (%subdirs (%combine dir "dotcl")))))
    (when (and registry dirs)
      (dolist (d dirs)
        (unless (member d (symbol-value registry) :test #'equal)
          (setf (symbol-value registry) (append (symbol-value registry) (list d))))
        (%place-fasls (namestring d))))
    dirs))

;;; A package may ship the fasls its sources compile to (and R2R siblings), with
;;; dotcl-build.sexp naming the compiler that made them. ASDF looks for a fasl
;;; where its output translations put it, not beside the source, so each shipped
;;; fasl is copied to that place -- where ASDF then finds it newer than the
;;; source and loads it instead of compiling. Only when the compiler matches: a
;;; fasl keeps the code generation of the compiler that made it, so under any
;;; other the sources, which are always shipped too, are compiled as usual.

(defvar *noted-recompiles* '())

(defun %read-build-record (dir)
  (let ((text (%read-text (%combine dir "dotcl-build.sexp"))))
    (when text
      (ignore-errors
       (with-standard-io-syntax
         (let ((*read-eval* nil) (*package* (find-package "NUGET")))
           (read-from-string text)))))))

(defun %touch (path time)
  (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" path time))

(defun %place-fasls (dir)
  "Copy DIR's shipped fasls to where ASDF will look for them. Returns how many,
or NIL when the package ships none or they are not for this compiler."
  (let ((cfp (let ((pkg (find-package "UIOP")))
               (and pkg (find-symbol "COMPILE-FILE-PATHNAME*" pkg))))
        (record (%read-build-record dir)))
    (when (and cfp (fboundp cfp) record)
      (cond
        ((not (equal (getf record :core)
                     (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))))
         (unless (member dir *noted-recompiles* :test #'equal)
           (push dir *noted-recompiles*)
           (format *error-output* "~&; nuget: ~A was compiled by ~A; this is ~A, so its ~
sources are compiled instead~%"
                   dir (getf record :dotcl-version) (lisp-implementation-version)))
         nil)
        (t
         ;; Every placed fasl is dated one second after its source, and every
         ;; sibling one second after that -- not "now". ASDF recompiles a file
         ;; whose dependency's fasl is newer than its own, and placing group after
         ;; group at the time each was placed made a dependency placed later look
         ;; newer: Coalton's library, whose dependencies sort after it, compiled
         ;; again in full. The sources of a layout all carry its copy time, so
         ;; this gives every fasl the same date.
         (let ((n 0))
           ;; The record pairs each shipped fasl with its source: a source need
           ;; not be a .lisp (Coalton's are .ct), so the name alone cannot say.
           (loop for (fasl-rel . src-rel) in (getf record :fasls)
                 for fasl = (%combine dir fasl-rel)
                 for src = (%combine dir src-rel)
                 when (and (dotnet:static "System.IO.File" "Exists" fasl)
                           (dotnet:static "System.IO.File" "Exists" src))
                   do (let ((target (namestring (funcall cfp src))))
                        (unless (and (dotnet:static "System.IO.File" "Exists" target)
                                     (%newer-p target src))
                          (ensure-directories-exist target)
                          (dotnet:static "System.IO.File" "Copy" fasl target t)
                          (%touch target (%after src 1d0))
                          ;; x.fasl.r2r-<rid> rides along under the same suffix,
                          ;; dated after the fasl: the loader ignores a sibling
                          ;; older than it.
                          (dolist (sib (%dir-files
                                        (dotnet:static "System.IO.Path" "GetDirectoryName" fasl)
                                        (concatenate 'string
                                                     (dotnet:static "System.IO.Path" "GetFileName" fasl)
                                                     ".r2r-*")))
                            (let ((to (concatenate 'string target (subseq sib (length fasl)))))
                              (dotnet:static "System.IO.File" "Copy" sib to t)
                              (%touch to (%after src 2d0))))
                          (incf n))))
           n))))))

(defun %dir-files-recursive (dir pattern)
  (let* ((arr (dotnet:static "System.IO.Directory" "GetFiles" dir pattern
                             (dotnet:static "System.IO.SearchOption" "AllDirectories")))
         (n (dotnet:invoke arr "get_Length")))
    (loop for i below n collect (aref arr i))))

(defun %after (path seconds)
  (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" path)
                 "AddSeconds" seconds))

(defun %newer-p (a b)
  (> (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" a) "get_Ticks")
     (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" b) "get_Ticks")))

(defun %dir-files (dir pattern)
  "List files in DIR matching PATTERN as a Lisp list of path strings."
  (let* ((arr (dotnet:static "System.IO.Directory" "GetFiles" dir pattern))
         (n (dotnet:invoke arr "get_Length"))
         (acc '()))
    (dotimes (i n) (push (aref arr i) acc))
    (nreverse acc)))

(defun %subdirs (dir)
  (when (and dir (dotnet:static "System.IO.Directory" "Exists" dir))
    (let* ((arr (dotnet:static "System.IO.Directory" "GetDirectories" dir))
           (n (dotnet:invoke arr "get_Length")))
      (loop for i below n collect (aref arr i)))))

(defun %managed-name (path)
  "If PATH is a managed assembly, return its simple name; else NIL (it's native)."
  (handler-case
      (dotnet:invoke
       (dotnet:static "System.Reflection.AssemblyName" "GetAssemblyName" path) "get_Name")
    (error () nil)))

(defun %file-stem (path)
  (dotnet:static "System.IO.Path" "GetFileNameWithoutExtension" path))

(defun %register-output (out-dir self-stem)
  "Scan OUT-DIR; register each managed assembly and native library with the dotcl
resolver. Skips SELF-STEM (the throwaway project's own assembly). Returns
(values managed-count native-count)."
  (let ((managed 0) (native 0))
    (dolist (pat '("*.dll" "*.so" "*.dylib"))
      (dolist (path (%dir-files out-dir pat))
        (let ((stem (%file-stem path)))
          (unless (string= stem self-stem)
            (let ((mname (and (string= pat "*.dll") (%managed-name path))))
              (cond
                (mname
                 (dotcl:register-assembly-path mname path)
                 (incf managed))
                (t
                 ;; native: register under the bare stem (matches typical
                 ;; [DllImport("libFoo")]); ResolvingUnmanagedDll consults it.
                 (dotcl:register-native-path stem path)
                 (incf native))))))))
    (values managed native)))

;;; --- the image's set --------------------------------------------------------

(defstruct state
  (requests '())   ; every REQ asked for in this image, in order
  (pins '())       ; (id . version) registered so far
  (out-dir nil))   ; the layout last registered

(defvar *states* (make-hash-table :test #'equal)
  "Per (rid tfm): what this image has asked for and what it registered.")

(defun %state (rid tfm)
  (let ((key (list rid tfm)))
    (or (gethash key *states*)
        (setf (gethash key *states*) (make-state)))))

(defun %merge-requests (state new)
  "Add NEW to STATE. The same package asked for with a different version spec is
an error: an image loads one version of a package."
  (dolist (r new)
    (let ((old (find (req-id r) (state-requests state) :key #'req-id :test #'string-equal)))
      (cond ((null old)
             (setf (state-requests state) (append (state-requests state) (list r))))
            ((not (equal (req-spec old) (req-spec r)))
             (error "nuget: ~A is asked for as ~S here and as ~S earlier in this ~
image. One image loads one version of a package; make the two agree."
                    (req-id r) (req-spec r) (req-spec old)))
            ;; A declaration of something already asked for by hand: the lock
            ;; file has not seen it, so let the declared rules look at it.
            ((and (eq (req-origin r) :declared) (eq (req-origin old) :imperative))
             (setf (req-origin old) :declared))))))

(defun %merge-pins (old new &key (on-conflict :error))
  "OLD extended with NEW. A package already registered at another version is a
conflict: the assembly may be loaded, and .NET will not load a second one of the
same name into the same image."
  (let ((out (copy-list old))
        (conflicts '()))
    (dolist (p new)
      (let ((o (assoc (car p) out :test #'string-equal)))
        (cond ((null o) (push p out))
              ((not (string-equal (cdr o) (cdr p)))
               (push (list (car p) (cdr o) (cdr p)) conflicts)))))
    (when conflicts
      (let ((msg (format nil "~:{~A is registered at ~A, and the resolution now wants ~A~:^; ~}"
                         (reverse conflicts))))
        (ecase on-conflict
          (:error (error "nuget: ~A. Start a new image to load the new versions." msg))
          (:warn (warn "nuget: ~A. Start a new image to load the new versions." msg)
           (return-from %merge-pins nil)))))
    (%sort-pins out)))

(defun %sources (reqs)
  (remove-duplicates (remove nil (mapcar #'req-source reqs)) :test #'equal))

(defun %find-bundled (reqs rid tfm)
  "A bundled layout for RID and TFM whose recorded requests include every one of
REQS, or NIL. Returns (values dir marker)."
  (dolist (dir (%subdirs (bundled-root)) nil)
    (when (%layout-complete-p dir)
      (let ((m (%read-marker dir)))
        (when (and m (equal (getf m :rid) rid) (equal (getf m :tfm) tfm)
                   (every (lambda (r)
                            (find-if (lambda (e) (and (string-equal (first e) (req-id r))
                                                      (equal (second e) (req-spec r))))
                                     (getf m :requests)))
                          reqs))
          (return (values dir m)))))))

(defun %fetch-note (reqs lock)
  (format *error-output* "~&; nuget: resolving ~{~A ~A~^, ~}~@[ (recording in ~A)~]~%"
          (loop for r in reqs append (list (req-id r) (req-spec r))) lock)
  (finish-output *error-output*))

(defun %declared-pins (state tfm)
  "Versions for STATE's declared requests, by the project's lock file."
  (let* ((lock (%lock-path))
         (declared (remove :imperative (state-requests state) :key #'req-origin))
         (sources (%sources declared)))
    (when (null declared) (return-from %declared-pins '()))
    (multiple-value-bind (directs pins present) (%read-lock lock tfm)
      ;; 1. The lock answers every declaration as written.
      (when (and present (every (lambda (r) (%covered-p directs r pins)) declared))
        (return-from %declared-pins pins))
      (let* ((uncovered (remove-if (lambda (r) (%covered-p directs r pins)) declared))
             (refs (append (mapcar (lambda (r) (cons (req-id r) (req-spec r))) declared)
                           (remove-if (lambda (d) (find (car d) declared :key #'req-id
                                                                         :test #'string-equal))
                                      directs))))
        ;; 2. The lock may still answer them in a spelling compared here only
        ;; approximately (a range, say). NuGet's locked mode is the judge.
        (when (and present (not (%offline-p))
                   (every (lambda (r) (assoc (req-id r) directs :test #'string-equal))
                          uncovered)
                   (zerop (%restore-lock refs tfm sources lock :locked t)))
          (return-from %declared-pins (nth-value 1 (%read-lock lock tfm))))
        ;; 3. Something new. Only exact versions are resolved implicitly.
        (let ((floating (remove-if (lambda (r) (%exact-version-p (req-spec r))) uncovered)))
          (when floating
            (error "nuget: ~{~A ~S~^, ~} ~:[is~;are~] declared with a version that is not ~
exact, and ~A does not record ~:[it~;them~]. Loading a system does not resolve such ~
a version by itself, since the answer would depend on the day it runs. Pin ~
~:[it~;them~] to an exact version, or run (nuget:restore) once to record what ~
~:[it resolves~;they resolve~] to, then load again."
                   (loop for r in floating append (list (req-id r) (req-spec r)))
                   (cdr floating) lock (cdr floating) (cdr floating) (cdr floating))))
        (let ((moving (remove-if-not (lambda (d) (find #\* (cdr d))) directs)))
          (when moving
            (error "nuget: ~A records floating version~P (~{~A ~A~^, ~}), and resolving ~
~{~A~^, ~} on top of ~:[it~;them~] would let ~:[it~;them~] move. Run (nuget:restore) ~
to update the record, then load again."
                   lock (length moving) (loop for (id . v) in moving append (list id v))
                   (mapcar #'req-id uncovered) (cdr moving) (cdr moving))))
        (when (%offline-p)
          (%refuse-offline (format nil "Resolving ~{~A~^, ~}" (mapcar #'req-id uncovered))))
        (%fetch-note uncovered lock)
        (%restore-or-error refs tfm sources lock)
        (nth-value 1 (%read-lock lock tfm))))))

(defun %imperative-pins (state tfm new)
  "Versions for NEW, asked for by hand, resolved around what STATE has registered.
The project's lock file is neither read nor written."
  (let* ((fixed (mapcar (lambda (p) (cons (car p) (format nil "[~A]" (cdr p))))
                        (state-pins state)))
         (refs (append fixed
                       (loop for r in new
                             unless (assoc (req-id r) fixed :test #'string-equal)
                               collect (cons (req-id r) (req-spec r)))))
         (lock (%combine (%temp-project-dir) "packages.lock.json")))
    (%restore-or-error refs tfm (%sources new) lock
                       :note (and fixed "The versions in brackets are already registered in this image and are held where they are. To resolve everything together, start a new image and ask for all of it before using any of it."))
    (nth-value 1 (%read-lock lock tfm))))

(defun %adopt (state dir pins)
  (%register-output dir "proj")
  (%register-lisp-systems dir)
  (setf (state-pins state) pins
        (state-out-dir state) dir)
  ;; A request counts as answered when its package is among the versions. One
  ;; that was refused (a floating declaration with nothing recorded) is not, even
  ;; though other requests were answered alongside it.
  (dolist (r (state-requests state))
    (when (assoc (req-id r) pins :test #'string-equal)
      (setf (req-resolved r) t)))
  dir)

(defun %ensure (new &key (mode :imperative) rid tfm fresh)
  "Make every request in NEW, together with everything this image asked for
before, resolved and registered for RID and TFM. Returns the layout directory.

MODE :DECLARED follows the project's lock file (see the file header); MODE
:IMPERATIVE resolves NEW as asked. FRESH skips the in-image memo."
  (let* ((rid (or rid (%current-rid)))
         (tfm (or tfm (%current-tfm)))
         (state (%state rid tfm)))
    (%merge-requests state new)
    (let* ((reqs (state-requests state))
           ;; By hand, only what was asked for by hand is wanted: a declaration
           ;; that was refused waits for NUGET:RESTORE, not for the next REQUIRE.
           (wanted (if (eq mode :declared)
                       reqs
                       (remove :declared reqs :key #'req-origin))))
      (when (and (not fresh) (state-out-dir state) (every #'req-resolved wanted))
        (return-from %ensure (state-out-dir state)))
      ;; What the application shipped with wins, whatever the version specs say:
      ;; it is the answer the build already committed to, and a shipped program
      ;; has no business asking the network whether something newer came out.
      (multiple-value-bind (bundled marker) (%find-bundled reqs rid tfm)
        (when bundled
          (return-from %ensure
            (%adopt state bundled (%merge-pins (state-pins state) (getf marker :pins))))))
      (let* ((new-pins
               (ecase mode
                 (:declared
                  (append (%declared-pins state tfm)
                          ;; anything asked for by hand earlier keeps its version
                          (state-pins state)))
                 (:imperative
                  (%imperative-pins state tfm
                                    (remove-if (lambda (r) (and (req-resolved r) (not fresh)))
                                               wanted)))))
             (pins (%merge-pins (state-pins state) new-pins))
             (dir (%lay-out pins rid tfm (%sources reqs))))
        (%write-marker dir rid tfm pins reqs)
        (%adopt state dir pins)))))

;;; --- entry points -----------------------------------------------------------

(defun resolve (package &key version source prerelease rid tfm)
  "Resolve PACKAGE and its transitive dependencies, registering every managed assembly
and RID-specific native library with the dotcl resolver. Returns
(values managed-count native-count output-directory).

A package is identified by several axes besides its name, so they are keywords:
  :version    exact (\"2.88.7\"), range (\"[1.0,2.0)\"), or floating (\"13.*\"). Omitted =
              latest stable (\"*\"), or latest incl. prerelease when :prerelease is true.
  :prerelease when true and :version is omitted, take the latest prerelease (\"*-*\").
  :source     an extra NuGet feed URI (private feed); added to the default sources.
  :rid        target RuntimeIdentifier (default: the running process's RID); selects
              which native assets are laid out.
  :tfm        target framework moniker (default: the running runtime's, e.g. net10.0).

Unlike REQUIRE this asks NuGet again even when this image already resolved the
package, so a floating version is looked up afresh. Whatever this image already
registered keeps its version; a resolution that would move one is an error."
  (let ((dir (%ensure (list (make-req package (%spec version prerelease) source :imperative))
                      :mode :imperative :rid rid :tfm tfm :fresh t)))
    (multiple-value-bind (managed native) (%register-output dir "proj")
      (values managed native dir))))

(defun require (package &key version source prerelease rid tfm)
  "Resolve PACKAGE (see RESOLVE for the keywords) and register its assemblies.
Returns T.

Asking twice for the same package in one image does the work once. This is the
by-hand entry point: a floating version is resolved as asked, and the project's
lock file is neither read nor written -- that file is for what a system
definition declares, see NUGET:RESTORE."
  (%ensure (list (make-req package (%spec version prerelease) source :imperative))
           :mode :imperative :rid rid :tfm tfm)
  t)

(defun restore (&key tfm)
  "Resolve what this image's systems declared, together with what the project's
lock file already records, and write the result to that lock file. Returns its
path.

This is the explicit step a floating declaration waits for: loading a system
resolves an exact version by itself but stops on \"13.*\", and RESTORE is what
decides what \"13.*\" means today and records it, so that every later load --
here or on another machine with the same lock file -- gets the same answer.
Running it again is how a recorded floating version moves forward.

When a version this image has already registered moves, the lock file is still
updated, a warning says so, and nothing new is registered: start a new image to
load the new versions."
  (let* ((tfm (or tfm (%current-tfm)))
         (rid (%current-rid))
         (state (%state rid tfm))
         (lock (%lock-path))
         (declared (remove :imperative (state-requests state) :key #'req-origin))
         (directs (%read-lock lock tfm))
         (refs (append (mapcar (lambda (r) (cons (req-id r) (req-spec r))) declared)
                       (remove-if (lambda (d) (find (car d) declared :key #'req-id
                                                                     :test #'string-equal))
                                  directs))))
    (if (null refs)
        (progn
          (format *error-output* "~&; nuget: nothing is declared and ~A records nothing~%"
                  lock)
          nil)
        (progn
          (%fetch-note (mapcar (lambda (r) (make-req (car r) (cdr r) nil :declared)) refs)
                       lock)
          (%restore-or-error refs tfm (%sources declared) lock :force t)
          (let ((pins (%merge-pins (state-pins state) (nth-value 1 (%read-lock lock tfm))
                                   :on-conflict :warn)))
            (when (and pins declared)
              (let ((dir (%lay-out pins rid tfm (%sources declared))))
                (%write-marker dir rid tfm pins (state-requests state))
                (%adopt state dir pins))))
          lock))))

;;; --- shipping ---------------------------------------------------------------

(defun %copy-tree (src dst)
  "Copy every file under SRC to the same relative place under DST."
  (let* ((arr (dotnet:static "System.IO.Directory" "GetFiles" src "*"
                             (dotnet:static "System.IO.SearchOption" "AllDirectories")))
         (n (dotnet:invoke arr "get_Length"))
         (prefix (length src)))
    (dotnet:static "System.IO.Directory" "CreateDirectory" dst)
    (dotimes (i n n)
      (let* ((from (aref arr i))
             ;; SRC came from Path.Combine, so it is a prefix of every entry;
             ;; +1 drops the separator.
             (to (%combine dst (subseq from (1+ prefix)))))
        (dotnet:static "System.IO.Directory" "CreateDirectory"
                       (dotnet:static "System.IO.Path" "GetDirectoryName" to))
        (dotnet:static "System.IO.File" "Copy" from to t)))))

(defun stage-bundle (dir &optional rid)
  "Copy the layouts resolved in this image into DIR/nuget/, and return how many.

With RID, only the layout for that RuntimeIdentifier is copied. `dotcl pack'
builds one package per RID and each carries its own bundle, so a Windows package
has no use for the Linux assets and should not pay for them.

DIR is what `dotcl pack --bundle' places beside the installed executable, and
BUNDLED-ROOT reads back from there. Staging is therefore the step that lets a
packaged application start on a machine with no .NET SDK and no network.

The copy's marker lists the requests this image made, version specs as written
(\"13.*\" stays \"13.*\"). A shipped program's request is matched against that
list, so the answer the build settled on is the answer, whatever has been
published since."
  (let ((root (%combine dir "nuget"))
        (n 0))
    (maphash
     (lambda (key state)
       (destructuring-bind (entry-rid tfm) key
         (let ((out-dir (state-out-dir state)))
           (when (and out-dir
                      (or (null rid) (equal rid entry-rid))
                      (dotnet:static "System.IO.Directory" "Exists" out-dir))
             (let ((target (%combine root (dotnet:static "System.IO.Path" "GetFileName"
                                                         out-dir))))
               (%copy-tree out-dir target)
               (%write-marker target entry-rid tfm (state-pins state) (state-requests state))
               (incf n))))))
     *states*)
    n))

(provide "dotcl-nuget")
