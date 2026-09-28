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

(require "dotcl-nuget")

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

;;; --- what `dotcl pack' carries beside the executable ------------------------
;;;
;;; STAGE-BUNDLE copies the layouts this process resolved into the directory pack
;;; places next to the installed program, under the same key RESOLVE will compute
;;; for the same request. That is what lets a packaged application start where
;;; there is no .NET SDK and no network. Resolving for real is not exercised here
;;; (it wants both); a layout is faked and the copy is what gets checked.

(defun nlk-temp-dir (tag)
  (let ((d (funcall (find-symbol "%COMBINE" "NUGET")
                    (regression-temp-dir)
                    (format nil "dotcl-nlk-~a-~a" tag
                            (dotnet:invoke (dotnet:static "System.Guid" "NewGuid")
                                           "ToString" "N")))))
    (dotnet:static "System.IO.Directory" "CreateDirectory" d)
    d))

(defun nlk-file (dir name text)
  (let ((p (funcall (find-symbol "%COMBINE" "NUGET") dir name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory"
                   (dotnet:static "System.IO.Path" "GetDirectoryName" p))
    (dotnet:static "System.IO.File" "WriteAllText" p text)
    p))

(defun nlk-exists (path)
  (and (dotnet:static "System.IO.File" "Exists" path) t))

(defun nlk-stage (id)
  "Stage a faked layout for ID = (package version source rid tfm); return the dir."
  (let* ((out (nlk-temp-dir "layout"))
         (bundle (nlk-temp-dir "bundle"))
         (table (symbol-value (find-symbol "*RESOLVED*" "NUGET")))
         (saved (make-hash-table :test #'equal)))
    (nlk-file out "Some.Package.dll" "not really an assembly")
    (nlk-file out "runtimes/win-arm64/native/libfoo.dll" "native too")
    (maphash (lambda (k v) (setf (gethash k saved) v)) table)
    (clrhash table)
    (setf (gethash id table) out)
    (unwind-protect
         (list (funcall (find-symbol "STAGE-BUNDLE" "NUGET") bundle) bundle)
      (clrhash table)
      (maphash (lambda (k v) (setf (gethash k table) v)) saved))))

;;; The layout arrives under DIR/nuget/<key>, which is where BUNDLED-ROOT looks.
(deftest nlk-stage-bundle-uses-the-layout-key
  (destructuring-bind (n bundle)
      (nlk-stage '("Some.Package" "13.*" nil "win-arm64" "net10.0"))
    (let ((dir (funcall (find-symbol "%COMBINE" "NUGET")
                        (funcall (find-symbol "%COMBINE" "NUGET") bundle "nuget")
                        (nlk-key :package "Some.Package" :version "13.*"
                                 :rid "win-arm64" :tfm "net10.0"))))
      (list n
            (nlk-exists (funcall (find-symbol "%COMBINE" "NUGET") dir "Some.Package.dll"))
            ;; subdirectories come along: the native assets live under runtimes/
            (nlk-exists (funcall (find-symbol "%COMBINE" "NUGET")
                                 dir "runtimes/win-arm64/native/libfoo.dll")))))
  (1 t t))

;;; The completion marker is written, not copied. A floating spec never had one
;;; -- only an exact version is kept between processes -- and without it the
;;; shipped copy would be read as a half-finished layout and ignored.
(deftest nlk-stage-bundle-marks-the-copy-complete
  (destructuring-bind (n bundle)
      (nlk-stage '("Some.Package" "13.*" nil "win-arm64" "net10.0"))
    (declare (ignore n))
    (let ((dir (funcall (find-symbol "%COMBINE" "NUGET")
                        (funcall (find-symbol "%COMBINE" "NUGET") bundle "nuget")
                        (nlk-key :package "Some.Package" :version "13.*"
                                 :rid "win-arm64" :tfm "net10.0"))))
      (and (funcall (find-symbol "%LAYOUT-COMPLETE-P" "NUGET") dir) t)))
  t)

;;; Each RID package carries its own platform's layout and nothing else: pack
;;; builds one package per RID, and the Windows one has no use for the Linux
;;; native assets it would otherwise be carrying.
(defun nlk-stage-two-rids (rid)
  (let* ((win (nlk-temp-dir "win"))
         (lin (nlk-temp-dir "lin"))
         (bundle (nlk-temp-dir "bundle"))
         (table (symbol-value (find-symbol "*RESOLVED*" "NUGET")))
         (saved (make-hash-table :test #'equal)))
    (nlk-file win "Some.Package.dll" "win")
    (nlk-file lin "Some.Package.dll" "linux")
    (maphash (lambda (k v) (setf (gethash k saved) v)) table)
    (clrhash table)
    (setf (gethash '("Some.Package" "1.0.0" nil "win-arm64" "net10.0") table) win)
    (setf (gethash '("Some.Package" "1.0.0" nil "linux-x64" "net10.0") table) lin)
    (unwind-protect
         (let ((n (funcall (find-symbol "STAGE-BUNDLE" "NUGET") bundle rid)))
           (list n
                 (nlk-exists (funcall (find-symbol "%COMBINE" "NUGET")
                                      (funcall (find-symbol "%COMBINE" "NUGET") bundle "nuget")
                                      (concatenate 'string
                                                   (nlk-key :package "Some.Package"
                                                            :rid "win-arm64")
                                                   "/Some.Package.dll")))
                 (nlk-exists (funcall (find-symbol "%COMBINE" "NUGET")
                                      (funcall (find-symbol "%COMBINE" "NUGET") bundle "nuget")
                                      (concatenate 'string
                                                   (nlk-key :package "Some.Package"
                                                            :rid "linux-x64")
                                                   "/Some.Package.dll")))))
      (clrhash table)
      (maphash (lambda (k v) (setf (gethash k table) v)) saved))))

(deftest nlk-stage-bundle-takes-one-rid
  (list (nlk-stage-two-rids "win-arm64")
        (nlk-stage-two-rids "linux-x64"))
  ((1 t nil) (1 nil t)))

;;; Without a RID it stages everything, which is what a build that packs for the
;;; machine it runs on wants.
(deftest nlk-stage-bundle-without-a-rid-takes-all
  (nlk-stage-two-rids nil)
  (2 t t))
