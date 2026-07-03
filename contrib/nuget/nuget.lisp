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
  (:export #:require #:resolve))

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
         (proj-dir (%temp-project-dir))
         (csproj (%combine proj-dir "proj.csproj"))
         (out-dir (%combine proj-dir "out")))
    (%write-text csproj (%csproj package version rid tfm))
    (multiple-value-bind (code log)
        (%run-dotnet (format nil "build \"~A\" -c Release -o \"~A\"~@[ --source \"~A\"~]"
                             csproj out-dir source)
                     proj-dir)
      (unless (zerop code)
        (error "nuget: `dotnet build` failed (exit ~D) for ~A ~A:~%~A"
               code package version log)))
    (multiple-value-bind (managed native) (%register-output out-dir "proj")
      (values managed native out-dir))))

(defun require (package &rest keys &key version source prerelease rid tfm)
  "Like RESOLVE but intended as the user entry point. Returns T on success.
Accepts the same keywords as RESOLVE (:version :source :prerelease :rid :tfm)."
  (declare (ignore version source prerelease rid tfm))
  (apply #'resolve package keys)
  t)

(provide "dotcl-nuget")
