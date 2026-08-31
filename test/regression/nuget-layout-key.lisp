;;; What NUGET keeps between processes, and what it refuses to keep.
;;;
;;; Laying a package out means running `dotnet build' on a throwaway project,
;;; which costs about a second and a half even when every package is already in
;;; NuGet's own cache -- that is MSBuild starting, not downloading. The result
;;; used to go to a fresh temp directory, so the next process paid again.
;;;
;;; It is now kept under a stable path, but only for an exact version. A floating
;;; spec ("*", "13.*", "*-*") or a range asks for whatever is newest; answering
;;; that from a directory laid out days ago would pin what the caller deliberately
;;; left open, and finding out whether it is still newest means asking the
;;; network -- which is the work being skipped.
;;;
;;; Only the decisions are tested here. Resolving for real needs the network and
;;; the .NET SDK, so it belongs to a bring-up run rather than this suite.

(require "nuget")

(defun nlk-exact-p (version)
  (funcall (find-symbol "%EXACT-VERSION-P" "NUGET") version))

(defun nlk-key (&key (package "P") (version "1.0.0") (rid "win-x64")
                     (tfm "net10.0") source)
  (funcall (find-symbol "%LAYOUT-KEY" "NUGET") package version rid tfm source))

;;; --- what is worth keeping -------------------------------------------------

(deftest nlk-exact-versions-are-kept
  (mapcar #'nlk-exact-p '("13.0.3" "1.0" "2.88.7-beta1"))
  (t t t))

(deftest nlk-floating-versions-are-not-kept
  (mapcar (lambda (v) (and (nlk-exact-p v) t))
          '("*" "*-*" "13.*" "[1.0,2.0)" "(1.0,)" "1.0, 2.0"))
  (nil nil nil nil nil nil))

(deftest nlk-empty-version-is-not-kept
  (and (nlk-exact-p "") t)
  nil)

;;; --- the key stands for the whole request ----------------------------------

;;; Every axis of the identity moves the key: the same package at a different
;;; version, RID, framework or feed is a different layout.
(deftest nlk-key-varies-with-each-axis
  (let ((base (nlk-key)))
    (list (equal base (nlk-key :version "2.0.0"))
          (equal base (nlk-key :rid "linux-arm64"))
          (equal base (nlk-key :tfm "net9.0"))
          (equal base (nlk-key :source "https://example.invalid/v3/index.json"))
          (equal base (nlk-key :package "Q"))))
  (nil nil nil nil nil))

;;; The same request is the same key, so a second process finds the first one's work.
(deftest nlk-key-is-stable
  (equal (nlk-key) (nlk-key))
  t)

;;; It has to be usable as a directory name: a version range or a feed URI carries
;;; characters a path cannot.
(deftest nlk-key-is-a-safe-directory-name
  (let ((key (nlk-key :version "[1.0,2.0)"
                      :source "https://example.invalid/v3/index.json")))
    (and (every (lambda (c) (or (alphanumericp c) (find c "._-"))) key) t))
  t)

;;; --- where it goes ---------------------------------------------------------

;;; Next to the fasl cache, not somewhere of its own: that one already decides
;;; where dotcl may write on this platform.
(defun nlk-parent (path)
  (dotnet:static "System.IO.Path" "GetDirectoryName" (substitute #\/ #\\ path)))

;;; A packaged application carries its packages next to the executable, which is
;;; where `dotcl pack --bundle' puts them. That copy is the answer the build
;;; already committed to, so it is used even for a floating spec -- a shipped
;;; program has no business asking the network whether something newer came out,
;;; and on the machine it was installed on there may be neither network nor SDK.
(deftest nlk-bundled-root-sits-beside-the-executable
  (let ((root (substitute #\/ #\\ (nuget:bundled-root)))
        (exe (substitute #\/ #\\ (dotnet:static "System.Environment" "ProcessPath"))))
    (list (equal (nlk-parent root)
                 (dotnet:static "System.IO.Path" "GetDirectoryName" exe))
          (equal "nuget" (dotnet:static "System.IO.Path" "GetFileName" root))))
  (t t))

(deftest nlk-cache-root-sits-beside-the-fasl-cache
  (let ((nuget-root (nuget:cache-root))
        (fasl-root (funcall (find-symbol "%FASL-CACHE-ROOT" "DOTCL"))))
    (list (equal (nlk-parent nuget-root) (nlk-parent fasl-root))
          (and (search "dotcl-nuget" (substitute #\/ #\\ nuget-root)) t)))
  (t t))
