;;; nuget.lisp — transitive NuGet dependency resolution for dotcl
;;;
;;; Usage:
;;;   (require "nuget")
;;;   (nuget:require "Avalonia.Desktop" :version "11.3.18")
;;;   (nuget:require "SkiaSharp")                       ; latest stable
;;;   (nuget:require "SkiaSharp" :prerelease t)         ; latest incl. prerelease
;;;   (nuget:require "MyLib" :source "https://my.feed/v3/index.json")
;;;
;;; Identifying axes other than the package name are keywords (:version :source
;;; :prerelease :rid :tfm) so more can be added without positional churn.
;;;
;;; Folds the manual "AssemblyResolve + path index" work into one call: generates a
;;; throwaway csproj with a single PackageReference for the current RID, runs
;;; `dotnet build` (which restores + lays out the full version-unified dependency
;;; graph — managed and RID-specific native — into one output directory), then teaches
;;; the runtime every resolved path via dotcl:register-assembly-path /
;;; register-native-path. No NuGet client library is linked into the runtime; the
;;; resolution is done by the external `dotnet` CLI and the BCL only.
;;;
;;; Why `dotnet build` (not bare `dotnet restore` + project.assets.json): build with a
;;; RuntimeIdentifier flattens the entire resolved graph (correct, unified versions +
;;; native assets for this RID) into the output directory, so resolution reduces to a
;;; directory scan + a managed/native split — no assets.json shape to track. The extra
;;; cost over restore is just compiling an empty project (negligible; restore dominates).

(defpackage :nuget
  (:use :cl)
  (:shadow #:require)
  (:export #:require #:resolve #:cache-root #:bundled-root))

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

(defun %temp-project-dir ()
  "A fresh temp directory for the throwaway restore project."
  (let* ((base (dotnet:static "System.IO.Path" "GetTempPath"))
         (name (format nil "dotcl-nuget-~A"   ; temp-dir prefix; kept for grep-ability
                       (dotnet:invoke (dotnet:static "System.Guid" "NewGuid") "ToString" "N")))
         (dir (%combine base name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory" dir)
    dir))

(defun cache-root ()
  "Where laid-out packages are kept, so a later process can reuse them.

A sibling of the fasl cache rather than a path of its own: that one already
decides where dotcl may write on this platform (XDG_CACHE_HOME, LOCALAPPDATA,
~/.cache), and having two answers to the same question is how they drift apart."
  (%combine (dotnet:static "System.IO.Path" "GetDirectoryName"
                           (funcall (find-symbol "%FASL-CACHE-ROOT" "DOTCL")))
            "dotcl-nuget"))

(defun bundled-root ()
  "Where a packaged application carries the packages it was built against.

`dotcl pack --bundle DIR' copies DIR next to the installed executable, so a
layout under DIR/nuget/ arrives as a sibling of the program. Looking there first
is what lets a shipped application start on a machine with no .NET SDK and no
network -- resolving otherwise means running `dotnet build'."
  (let ((exe (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))))
    (when exe
      (%combine (dotnet:static "System.IO.Path" "GetDirectoryName" exe) "nuget"))))

(defun %exact-version-p (version)
  "True when VERSION names one release rather than a moving target.

Only an exact version is worth keeping across processes. A floating spec
(\"*\", \"13.*\", \"*-*\") or a range asks for whatever is newest, and answering
it from a directory laid out days ago would quietly pin what the caller
deliberately left open -- with no way to tell, since finding out means asking
the network, which is the work being skipped."
  (and (stringp version)
       (plusp (length version))
       (not (find-if (lambda (c) (find c "*[]() ,")) version))))

(defun %layout-key (package version rid tfm source)
  "A directory name that stands for exactly this request."
  (let ((raw (format nil "~A_~A_~A_~A~@[_~A~]" package version rid tfm source)))
    (map 'string (lambda (c) (if (or (alphanumericp c) (find c "._-")) c #\_)) raw)))

(defun %layout-complete-p (dir)
  "True when DIR holds a finished layout.

A build that died halfway leaves a directory behind, and reusing that would be
worse than rebuilding: the assemblies that did get copied would register and the
missing ones would surface much later as a type that cannot be resolved. The
marker file is written last, so its presence means the build returned 0."
  (and (dotnet:static "System.IO.Directory" "Exists" dir)
       (dotnet:static "System.IO.File" "Exists" (%combine dir "dotcl-nuget-complete"))))

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

(defun %csproj (package version rid tfm)
  (format nil "<Project Sdk=\"Microsoft.NET.Sdk\">~%~
  <PropertyGroup>~%~
    <TargetFramework>~A</TargetFramework>~%~
    <RuntimeIdentifier>~A</RuntimeIdentifier>~%~
    <Nullable>disable</Nullable>~%~
    <EnableDefaultItems>false</EnableDefaultItems>~%~
    <CopyLocalLockFileAssemblies>true</CopyLocalLockFileAssemblies>~%~
  </PropertyGroup>~%~
  <ItemGroup>~%~
    <PackageReference Include=\"~A\"~@[ Version=\"~A\"~] />~%~
  </ItemGroup>~%~
</Project>~%"
          tfm rid package version))

(defun %dir-files (dir pattern)
  "List files in DIR matching PATTERN as a Lisp list of path strings."
  (let* ((arr (dotnet:static "System.IO.Directory" "GetFiles" dir pattern))
         (n (dotnet:invoke arr "get_Length"))
         (acc '()))
    (dotimes (i n) (push (aref arr i) acc))
    (nreverse acc)))

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

(defun resolve (package &key version source prerelease rid tfm)
  "Resolve PACKAGE and its transitive dependencies, registering every managed assembly
and RID-specific native library with the dotcl resolver. Returns
(values managed-count native-count output-directory).

A package is identified by several axes besides its name, so they are keywords:
  :version    exact (\"2.88.7\"), range (\"[1.0,2.0)\"), or floating (\"13.*\"). Omitted =
              latest stable (\"*\"), or latest incl. prerelease when :prerelease is true.
  :prerelease when true and :version is omitted, take the latest prerelease (\"*-*\").
  :source     an extra NuGet feed URI (private feed); appended to the default sources.
  :rid        target RuntimeIdentifier (default: the running process's RID) — selects
              which native assets are laid out.
  :tfm        target framework moniker (default: the running runtime's, e.g. net10.0)."
  ;; A PackageReference requires a version; the SDK errors (NU1604) on a bare Include.
  (let* ((version (or version (if prerelease "*-*" "*")))
         (rid (or rid (%current-rid)))
         (tfm (or tfm (%current-tfm)))
         (key (%layout-key package version rid tfm source))
         (keep (%exact-version-p version))
         (bundled (let ((root (bundled-root)))
                    (when root
                      (let ((dir (%combine root key)))
                        (when (%layout-complete-p dir) dir)))))
         (out-dir (cond
                    ;; What the application shipped with wins, whatever the
                    ;; version spec says. A floating spec is not reused from the
                    ;; cache because "newest" can still change -- but a bundled
                    ;; layout is the answer the build already committed to, and
                    ;; a shipped program has no business asking the network
                    ;; whether something newer came out.
                    (bundled bundled)
                    (keep (%combine (cache-root) key))
                    (t (%combine (%temp-project-dir) "out")))))
    ;; An exact version laid out by an earlier process is the same bytes this
    ;; build would produce, so registering it is the whole job. `dotnet build'
    ;; costs a second and a half even when every package is already in NuGet's
    ;; own cache, because it is MSBuild starting up, not the download.
    (unless (or bundled (and keep (%layout-complete-p out-dir)))
      (let* ((proj-dir (%temp-project-dir))
             (csproj (%combine proj-dir "proj.csproj")))
        (%write-text csproj (%csproj package version rid tfm))
        (multiple-value-bind (code log)
            (%run-dotnet (format nil "build \"~A\" -c Release -o \"~A\"~@[ --source \"~A\"~]"
                                 csproj out-dir source)
                         proj-dir)
          (unless (zerop code)
            (error "nuget: `dotnet build` failed (exit ~D) for ~A ~A:~%~A"
                   code package version log)))
        ;; Last, so a half-written layout is never mistaken for a usable one.
        (when keep
          (%write-text (%combine out-dir "dotcl-nuget-complete") version))))
    (multiple-value-bind (managed native) (%register-output out-dir "proj")
      (values managed native out-dir))))

(defvar *resolved* (make-hash-table :test #'equal)
  "Package identities already resolved in this session, mapped to their output
directory. REQUIRE consults it; RESOLVE always does the work.")

(defun require (package &rest keys &key version source prerelease rid tfm)
  "Like RESOLVE but intended as the user entry point. Returns T on success.
Accepts the same keywords as RESOLVE (:version :source :prerelease :rid :tfm).

Asking twice for the same package in one session does the work once. RESOLVE runs
`dotnet build' on a throwaway project, which costs over a second even when every
assembly is already in NuGet's cache, and a program that reaches for a package
from several files would otherwise pay that each time. Call RESOLVE directly to
force a fresh restore -- that is the only way the answer changes within a session,
since a floating version can pick up a release published while the process runs."
  (let ((key (list package
                   (or version (if prerelease "*-*" "*"))
                   source
                   (or rid (%current-rid))
                   (or tfm (%current-tfm)))))
    (unless (gethash key *resolved*)
      (setf (gethash key *resolved*) (nth-value 2 (apply #'resolve package keys))))
    t))

(provide "dotcl-nuget")
