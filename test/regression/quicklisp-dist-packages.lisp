;;; A dist can offer prebuilt NuGet packages (dotcl pack --library) of its
;;; releases, listed in the file its distinfo names with dotcl-packages-url.
;;; Under QL:QUICKLOAD, a system listed for this dotcl and RID, in the release
;;; the client picks, is loaded from the package instead of being compiled.

(require "quicklisp")
(require "asdf-nuget")

(defun qdp-dir (tag)
  (let ((d (format nil "~a/dotcl-qdp-~a-~a/" (regression-temp-dir) tag
                   (dotnet:invoke (dotnet:static "System.Guid" "NewGuid") "ToString" "N"))))
    (ensure-directories-exist d)
    d))

(defun qdp-write (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out))
  path)

(defun qdp-rid () (dotcl:package-rid))

(defun qdp-home (system &key (index t) (dotcl (lisp-implementation-version)) (rid (qdp-rid))
                              (source "/feed/") (stale nil))
  "A quicklisp home with one dist, fake, whose release SYSTEM holds system SYSTEM
and is offered as package qdp.SYSTEM 1.0.0 for DOTCL and RID."
  (let* ((home (qdp-dir "home"))
         (d (format nil "~adists/fake/" home)))
    (qdp-write (format nil "~asystems.txt" d) (format nil "# project system-file system-name dependency1 dependency2 ... dependencyN~%~a ~a ~a~%"
                                                   system system system))
    (qdp-write (format nil "~areleases.txt" d)
               (format nil "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]~%~a http://example.invalid/~a.tgz 1 0 0 ~a-1 ~a.asd~%"
                       system system system system))
    (qdp-write (format nil "~aenabled.txt" d) "1")
    (qdp-write (format nil "~adistinfo.txt" d)
               (format nil "name: fake~%version: 1~%system-index-url: http://example.invalid/s.txt
release-index-url: http://example.invalid/r.txt~%archive-base-url: http://example.invalid/
canonical-distinfo-url: http://example.invalid/d.txt~%distinfo-subscription-url: http://example.invalid/f.txt
dotcl-packages-url: http://example.invalid/p.txt~%dotcl-package-source: ~a~%" source))
    ;; after the distinfo, so the index is never older than it
    (when index
      (qdp-write (format nil "~adotcl-packages.txt" d)
                 (format nil "# release dotcl rid id version system...~%~a ~a ~a qdp.~a 1.0.0 ~a~%"
                         system dotcl rid system system)))
    (when (and index stale)
      (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" (format nil "~adotcl-packages.txt" d)
                     (dotnet:invoke (dotnet:static "System.DateTime" "get_UtcNow") "AddDays" -1d0)))
    (pathname home)))

(defmacro qdp-with-home ((&rest args) &body body)
  `(let ((ql-setup:*quicklisp-home* (qdp-home ,@args)))
     ,@body))

(deftest quicklisp-dist-packages.offered
  (qdp-with-home ("qdp-a")
    (ql-setup:dist-package-for "qdp-a"))
  ("qdp.qdp-a" "1.0.0" "/feed/" "qdp-a"))

(deftest quicklisp-dist-packages.source-setting-wins
  (qdp-with-home ("qdp-a")
    (let ((ql-setup:*dist-package-source* "/elsewhere/"))
      (third (ql-setup:dist-package-for "qdp-a"))))
  "/elsewhere/")

(deftest quicklisp-dist-packages.not-for-others
  (list (qdp-with-home ("qdp-a" :dotcl "0.0.1-other") (ql-setup:dist-package-for "qdp-a"))
        (qdp-with-home ("qdp-a" :rid "other-rid") (ql-setup:dist-package-for "qdp-a"))
        (qdp-with-home ("qdp-a") (ql-setup:dist-package-for "qdp-unlisted"))
        (qdp-with-home ("qdp-a" :index nil) (ql-setup:dist-package-for "qdp-a"))
        ;; an index older than the distinfo belongs to an earlier dist version
        (qdp-with-home ("qdp-a" :stale t) (ql-setup:dist-package-for "qdp-a")))
  (nil nil nil nil nil))

;;; The RID is the portable one, so a package made on one Linux distribution
;;; is found on another; musl is its own RID and finds no glibc package.
(deftest quicklisp-dist-packages.package-rid
  (list (dotcl::%package-rid-for "linux-x64" "ubuntu.24.04-x64" "7f00 r-xp /usr/lib/x86_64-linux-gnu/libc.so.6")
        (dotcl::%package-rid-for "linux-arm64" "linux-arm64" nil)
        (dotcl::%package-rid-for "linux-x64" "alpine.3.20-x64" nil)
        (dotcl::%package-rid-for "linux-arm64" "linux-musl-arm64" nil)
        (dotcl::%package-rid-for "linux-x64" "void-x64" "7f00 r-xp /lib/ld-musl-x86_64.so.1")
        (dotcl::%package-rid-for "osx-arm64" "osx-arm64" nil)
        (dotcl::%package-rid-for "win-x64" "win-x64" nil))
  ("linux-x64" "linux-arm64" "linux-musl-x64" "linux-musl-arm64" "linux-musl-x64"
   "osx-arm64" "win-x64"))

(deftest quicklisp-dist-packages.host-rid-is-portable
  (let ((rid (dotcl:package-rid)))
    (list (and (member rid '("linux-x64" "linux-arm64" "linux-musl-x64" "linux-musl-arm64"
                             "osx-x64" "osx-arm64" "win-x64" "win-arm64" "win-x86")
                       :test #'equal)
               t)
          (subseq rid 0 (position #\- rid))))
  (t #.(cond ((member :windows *features*) "win")
             ((member :darwin *features*) "osx")
             (t "linux"))))

(deftest quicklisp-dist-packages.other-rids-not-offered
  (list (qdp-with-home ("qdp-r" :rid "ubuntu.24.04-x64") (ql-setup:dist-package-for "qdp-r"))
        (qdp-with-home ("qdp-r" :rid (if (search "musl" (dotcl:package-rid))
                                         "linux-x64"
                                         "linux-musl-x64"))
          (ql-setup:dist-package-for "qdp-r")))
  (nil nil))

(defun qdp-search (name require)
  "Run the searcher for NAME under QUICKLOAD with NUGET:REQUIRE replaced by
REQUIRE. Returns the searcher's value, the arguments REQUIRE got, and what was
written to *ERROR-OUTPUT*."
  (let ((old (fdefinition 'nuget:require))
        (calls '())
        (*error-output* (make-string-output-stream)))
    (unwind-protect
         (progn
           (setf (fdefinition 'nuget:require)
                 (lambda (&rest args) (push args calls) (apply require args)))
           (let ((v (let ((ql-setup::*quickloading* t))
                      (ql-setup:dist-package-searcher name))))
             (list v (reverse calls) (get-output-stream-string *error-output*))))
      (setf (fdefinition 'nuget:require) old))))

(deftest quicklisp-dist-packages.searcher-requires-once
  (qdp-with-home ("qdp-b")
    (let ((first (qdp-search "qdp-b" (constantly t)))
          (again (qdp-search "qdp-b" (constantly t))))
      (list (first first) (second first)
            (and (search "qdp-b from the package qdp.qdp-b 1.0.0" (third first)) t)
            (second again))))
  (nil (("qdp.qdp-b" :version "1.0.0" :source "/feed/")) t nil))

(deftest quicklisp-dist-packages.searcher-only-under-quickload
  (qdp-with-home ("qdp-c")
    (list (ql-setup:dist-package-searcher "qdp-c")
          (gethash "qdp.qdp-c" ql-setup::*dist-packages-tried*)))
  (nil nil))

(deftest quicklisp-dist-packages.searcher-off
  (qdp-with-home ("qdp-d")
    (let ((ql-setup:*use-dist-packages* nil))
      (second (qdp-search "qdp-d" (constantly t)))))
  nil)

(deftest quicklisp-dist-packages.failure-leaves-the-source
  (qdp-with-home ("qdp-e")
    (destructuring-bind (v calls out)
        (qdp-search "qdp-e" (lambda (&rest args) (declare (ignore args)) (error "no feed")))
      (list v (length calls)
            (and (search "could not use qdp.qdp-e 1.0.0, so qdp-e is compiled: no feed" out) t))))
  (nil 1 t))

;;; A source with {id} and {version} names one .nupkg: it is fetched into the
;;; dist's dotcl-packages/ directory once, and that directory is the feed.
(deftest quicklisp-dist-packages.url-template
  (qdp-with-home ("qdp-g" :source "qdpfetch://host/d/{id}/{version}/{id}.nupkg")
    (let* ((urls '())
           (ql-http:*fetch-scheme-functions*
             (acons "qdpfetch"
                    (lambda (url file &rest keys)
                      (declare (ignore keys))
                      (push url urls)
                      (qdp-write (namestring file) "nupkg"))
                    ql-http:*fetch-scheme-functions*))
           (dir (namestring (merge-pathnames "dists/fake/dotcl-packages/"
                                             ql-setup:*quicklisp-home*)))
           (first (qdp-search "qdp-g" (constantly t))))
      (clrhash ql-setup::*dist-packages-tried*)
      (qdp-search "qdp-g" (constantly t))
      (destructuring-bind ((id &key version source)) (second first)
        (list id version (string= source dir) urls
              (and (probe-file (format nil "~aqdp.qdp-g.1.0.0.nupkg" dir)) t)))))
  ("qdp.qdp-g" "1.0.0" t ("qdpfetch://host/d/qdp.qdp-g/1.0.0/qdp.qdp-g.nupkg") t))

;;; A real package, laid out the way NUGET:REQUIRE would, without dotnet restore.
;;; WHICH answers :FASL in the packed fasl and :SOURCE in the shipped source,
;;; which is edited after packing.

(defun qdp-pack (name)
  (let ((src (qdp-dir "lib")))
    (qdp-write (format nil "~asrc.lisp" src)
               (format nil "(defpackage :~a (:use :cl) (:export #:which))
(in-package :~a)
(defun which () :fasl)" name name))
    (qdp-write (format nil "~a~a.asd" src name)
               (format nil "(defsystem ~s :version \"1.0.0\" :author \"a\" :description \"d\"
  :components ((:file \"src\")))" name))
    (asdf:load-asd (format nil "~a~a.asd" src name))
    (prog1 (asdf-nuget::%pack-library (asdf:find-system name) (qdp-dir "feed"))
      (asdf:clear-system name))))

(defun qdp-lay-out (name nupkg)
  "What NUGET:REQUIRE does with NUPKG, minus restore: lay it out and register it."
  (let ((root (qdp-dir "packages"))
        (layout (qdp-dir "layout")))
    (dotnet:static "System.IO.Compression.ZipFile" "ExtractToDirectory" nupkg
                   (nuget::%combine root name "1.0.0"))
    (let ((nuget::*packages-directory* root))
      (nuget::%copy-lisp-systems (list (cons name "1.0.0")) layout))
    (qdp-write (nuget::%combine layout "dotcl" name "src.lisp")
               (format nil "(defpackage :~a (:use :cl) (:export #:which))
(in-package :~a)
(defun which () :source)" name name))
    (nuget::%register-lisp-systems layout)
    t))

;;; Compiling would have loaded the edited source, so :FASL also says nothing
;;; was compiled.
(deftest-emitting-only quicklisp-dist-packages.loads-the-shipped-fasls
  (let* ((name "qdp-real")
         (nupkg (qdp-pack name)))
    (setf (fdefinition (find-symbol "WHICH" (string-upcase name))) (lambda () :stale))
    (qdp-with-home (name)
      (let ((asdf:*central-registry* '()))
        (qdp-search name (lambda (id &key version source)
                           (declare (ignore id version source))
                           (qdp-lay-out name nupkg)))
        (let ((ql-setup::*quickloading* t))
          (asdf:load-system name))
        (funcall (find-symbol "WHICH" (string-upcase name))))))
  :fasl)
