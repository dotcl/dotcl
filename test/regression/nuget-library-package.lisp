;;; An ASDF system shipped as a NuGet package, and consumed by naming it in a
;;; (:nuget ...) component.
;;;
;;; The package carries the .asd and the sources under dotcl/<system>/ (not lib/,
;;; which would hand them to any .NET project that referenced the package as if
;;; they were assemblies for it), and the system's own (:nuget ...) components
;;; become the package's dependencies. Resolving a package copies its dotcl/ tree
;;; into the layout and puts each system directory on ASDF's central registry.
;;;
;;; No network and no SDK here: the package is written and read back as a zip,
;;; and NuGet's packages folder is faked. The fixture system is written to a .asd
;;; file rather than defined in this file, because a DEFSYSTEM inside a file the
;;; suite LOADs makes ASDF treat this file as its definition.

(require "asdf-nuget")

(defun nlp-dir (tag)
  (let ((d (nuget::%combine (regression-temp-dir)
                            (format nil "dotcl-nlp-~a-~a" tag
                                    (dotnet:invoke (dotnet:static "System.Guid" "NewGuid")
                                                   "ToString" "N")))))
    (dotnet:static "System.IO.Directory" "CreateDirectory" d)
    d))

(defun nlp-write (dir name text)
  (let ((p (nuget::%combine dir name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory"
                   (dotnet:static "System.IO.Path" "GetDirectoryName" p))
    (dotnet:static "System.IO.File" "WriteAllText" p text)
    p))

(defun nlp-system (name components)
  "Write NAME.asd with COMPONENTS (a string) into a fresh directory, load it, and
return the system."
  (let ((dir (nlp-dir "sys")))
    (nlp-write dir "src/nlp.lisp" "(defpackage :nlp-test (:use :cl))")
    (nlp-write dir (format nil "~a.asd" name)
               (format nil "(defsystem ~s :version \"1.2.3\" :author \"someone\"
  :description \"a test library\" :defsystem-depends-on (\"asdf-nuget\")
  :serial t :components (~a))" name components))
    (asdf:load-asd (nuget::%combine dir (format nil "~a.asd" name)))
    (asdf:find-system name)))

(defun nlp-zip-entries (path)
  (let* ((zip (dotnet:static "System.IO.Compression.ZipFile" "OpenRead" path))
         (entries (dotnet:invoke zip "get_Entries"))
         (n (dotnet:invoke entries "get_Count")))
    (unwind-protect
         (sort (loop for i below n
                     collect (dotnet:invoke (dotnet:invoke entries "get_Item" i) "get_FullName"))
               #'string<)
      (dotnet:invoke zip "Dispose"))))

(defun nlp-zip-text (path entry-name)
  (let ((zip (dotnet:static "System.IO.Compression.ZipFile" "OpenRead" path)))
    (unwind-protect
         (let* ((e (dotnet:invoke zip "GetEntry" entry-name))
                (r (dotnet:new "System.IO.StreamReader" (dotnet:invoke e "Open"))))
           (prog1 (dotnet:invoke r "ReadToEnd") (dotnet:invoke r "Dispose")))
      (dotnet:invoke zip "Dispose"))))

;;; :FASL NIL: compiling would perform the (:nuget ...) component, which resolves
;;; a real package. The compiled shape is checked below on a system without one.
(defparameter *nlp-package*
  (asdf-nuget::%pack-library
   (nlp-system "nlp-lib" "(:nuget \"Newtonsoft.Json\" :nuget-version \"13.0.3\") (:file \"src/nlp\")")
   (nlp-dir "out")
   :fasl nil))

;;; The .asd and the sources under dotcl/<system>/, and the nuspec at the root.
(deftest nuget-library.package-entries
  (nlp-zip-entries *nlp-package*)
  ("dotcl/nlp-lib.systems" "dotcl/nlp-lib/nlp-lib.asd" "dotcl/nlp-lib/src/nlp.lisp"
   "nlp-lib.nuspec"))

(deftest nuget-library.package-file-name
  (dotnet:static "System.IO.Path" "GetFileName" *nlp-package*)
  "nlp-lib.1.2.3.nupkg")

;;; Id, version, authors and description come from the system; its (:nuget ...)
;;; component is the package's dependency.
(deftest nuget-library.nuspec
  (let ((text (nlp-zip-text *nlp-package* "nlp-lib.nuspec")))
    (mapcar (lambda (s) (and (search s text) t))
            '("<id>nlp-lib</id>" "<version>1.2.3</version>" "<authors>someone</authors>"
              "<description>a test library</description>"
              "<dependency id=\"Newtonsoft.Json\" version=\"13.0.3\" />")))
  (t t t t t))

;;; A floating version cannot be a package dependency: nuspec has no syntax for
;;; it, and which version it means is the consumer's lock file's business.
(deftest nuget-library.floating-dependency-is-refused
  (handler-case
      (progn (asdf-nuget::%pack-library
              (nlp-system "nlp-float" "(:nuget \"Newtonsoft.Json\" :nuget-version \"13.*\")")
              (nlp-dir "out") :fasl nil)
             :no-error)
    (error (e) (and (search "Newtonsoft.Json" (princ-to-string e)) t)))
  t)

;;; --- consuming ------------------------------------------------------------

;;; A resolved package's dotcl/ tree is copied into the layout, from NuGet's
;;; packages folder (lower-case id and version, as NuGet extracts them).
(deftest nuget-library.layout-gets-the-systems
  (let* ((root (nlp-dir "packages"))
         (layout (nlp-dir "layout")))
    (nlp-write root "nlp-lib/1.2.3/dotcl/nlp-lib/nlp-lib.asd" "()")
    (nlp-write root "newtonsoft.json/13.0.3/lib/net6.0/x.dll" "not lisp")
    (let ((nuget::*packages-directory* root))
      (list (nuget::%copy-lisp-systems '(("Newtonsoft.Json" . "13.0.3") ("nlp-lib" . "1.2.3"))
                                       layout)
            (and (dotnet:static "System.IO.File" "Exists"
                                (nuget::%combine layout "dotcl/nlp-lib/nlp-lib.asd"))
                 t))))
  (1 t))

;;; Registering the layout puts each system directory on the central registry.
(deftest nuget-library.registered-on-the-central-registry
  (let* ((layout (nlp-dir "layout"))
         (asdf:*central-registry* '()))
    (nlp-write layout "dotcl/one/one.asd" "()")
    (nlp-write layout "dotcl/two/two.asd" "()")
    (let ((dirs (nuget::%register-lisp-systems layout)))
      (list (length dirs)
            (equal (sort (mapcar (lambda (d) (car (last (pathname-directory d))))
                                 asdf:*central-registry*)
                         #'string<)
                   '("one" "two")))))
  (2 t))

;;; A library's own declaration of a package its consumer already pulls in
;;; transitively is answered by the consumer's lock, at exactly that version.
(deftest nuget-library.transitive-exact-is-covered
  (let ((pins '(("Newtonsoft.Json" . "13.0.3"))))
    (list (nuget::%covered-p '() (nuget::make-req "Newtonsoft.Json" "13.0.3" nil :declared) pins)
          (nuget::%covered-p '() (nuget::make-req "Newtonsoft.Json" "13.0.4" nil :declared) pins)
          (nuget::%covered-p '() (nuget::make-req "Newtonsoft.Json" "13.*" nil :declared) pins)))
  (t nil nil))

;;; --- shipping the compiled code --------------------------------------------
;;;
;;; The fasls go in beside their sources, with dotcl-build.sexp naming the
;;; dotcl that made them and its platform. The consumer copies them to where
;;; ASDF looks (its output translation of the source).

;;; Packing compiles, which an emit-free build cannot: those tests are skipped there.
(defparameter *nlp-compiled-system* (nlp-system "nlp-compiled" "(:file \"src/nlp\")"))

(defparameter *nlp-compiled-package*
  (when (emitting-mode-p)
    (asdf-nuget::%pack-library *nlp-compiled-system* (nlp-dir "out"))))

(deftest-emitting-only nuget-library.fasls-ship-beside-sources
  (nlp-zip-entries *nlp-compiled-package*)
  ("dotcl/nlp-compiled.systems" "dotcl/nlp-compiled/dotcl-build.sexp"
   "dotcl/nlp-compiled/nlp-compiled.asd" "dotcl/nlp-compiled/src/nlp.lisp"
   "dotcl/nlp-compiled/src/nlp.lisp.fasl"
   "nlp-compiled.nuspec"))

(deftest-emitting-only nuget-library.build-record-names-this-compiler
  (let ((r (read-from-string
            (nlp-zip-text *nlp-compiled-package* "dotcl/nlp-compiled/dotcl-build.sexp"))))
    (list (equal (getf r :core) (funcall (find-symbol "%CORE-GENERATION" "DOTCL")))
          (equal (getf r :dotcl-version) (lisp-implementation-version))
          ;; each fasl with its source: a source need not be a .lisp
          (getf r :fasls)))
  (t t (("src/nlp.lisp.fasl" . "src/nlp.lisp"))))

;;; A zip time has no zone and NuGet extracts it as UTC, so entries carry the
;;; UTC wall clock of the file. Local time would extract hours off.
(deftest-emitting-only nuget-library.entry-times-are-utc
  (let* ((zip (dotnet:static "System.IO.Compression.ZipFile" "OpenRead" *nlp-compiled-package*))
         (e (dotnet:invoke zip "GetEntry" "dotcl/nlp-compiled/src/nlp.lisp"))
         ;; the stored wall clock, whatever zone the reader attaches to it
         (stored (dotnet:invoke (dotnet:invoke (dotnet:invoke e "get_LastWriteTime") "get_DateTime")
                                "get_Ticks"))
         (file (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc"
                                             (namestring (asdf:component-pathname
                                                          (asdf:find-component *nlp-compiled-system*
                                                                               "src/nlp"))))
                              "get_Ticks")))
    (dotnet:invoke zip "Dispose")
    ;; zip times have a two-second grain
    (< (abs (- stored file)) (* 3 10000000)))
  t)

;;; Fake siblings for this platform and another, dated AGE-SECONDS from the fasl.
;;; They are not images, so packing (which loads the system) runs with
;;; DOTCL:*LOAD-R2R* off.
(defun nlp-system-with-siblings (name age-seconds)
  (let* ((sys (nlp-system name "(:file \"src/nlp\")"))
         (c (asdf:find-component sys "src/nlp")))
    (asdf:compile-system sys)
    (let ((fasl (namestring (first (asdf:output-files 'asdf:compile-op c)))))
      (dolist (rid (list (asdf-nuget::%host-rid) "test-rid"))
        (let ((sib (concatenate 'string fasl ".r2r-" rid)))
          (dotnet:static "System.IO.File" "WriteAllText" sib "not really r2r")
          (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" sib
                         (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" fasl)
                                        "AddSeconds" age-seconds)))))
    sys))

(defun nlp-r2r-entries (path)
  (remove-if-not (lambda (n) (search ".r2r-" n)) (nlp-zip-entries path)))

(deftest-emitting-only nuget-library.no-r2r-ships-no-sibling
  (let ((dotcl:*load-r2r* nil))
    (nlp-r2r-entries
     (asdf-nuget::%pack-library (nlp-system-with-siblings "nlp-no-r2r" 60d0) (nlp-dir "out"))))
  nil)

(deftest-emitting-only nuget-library.r2r-ships-this-platforms-sibling
  (let ((dotcl:*load-r2r* nil))
    (equal (nlp-r2r-entries
            (asdf-nuget::%pack-library (nlp-system-with-siblings "nlp-r2r" 60d0) (nlp-dir "out")
                                       :r2r t))
           (list (concatenate 'string "dotcl/nlp-r2r/src/nlp.lisp.fasl.r2r-"
                              (asdf-nuget::%host-rid)))))
  t)

;;; On the grouping step: :R2R would run crossgen2 to replace a stale sibling.
(deftest-emitting-only nuget-library.stale-sibling-does-not-ship
  (let ((sys (nlp-system-with-siblings "nlp-old-sib" -60d0)))
    (remove-if-not (lambda (e) (search ".r2r-" (cdr e)))
                   (asdf-nuget::%group-fasls
                    (namestring (asdf:system-source-directory sys)) (list sys) "test-rid")))
  nil)

(deftest nuget-library.sibling-entry-name
  (list (asdf-nuget::%sibling-entry
         "src/nlp.lisp.fasl" "C:/Users/u/src/nlp.lisp.fasl"
         "C:/Users/u/src/nlp.lisp.fasl.r2r-win-arm64")
        ;; the sibling as found without the drive: the name must not shift
        (asdf-nuget::%sibling-entry
         "src/nlp.lisp.fasl" "C:/Users/u/src/nlp.lisp.fasl"
         "/Users/u/src/nlp.lisp.fasl.r2r-win-arm64")
        (asdf-nuget::%sibling-entry
         "src/nlp.lisp.fasl" "/home/u/src/nlp.lisp.fasl"
         "/home/u/src/nlp.lisp.fasl.r2r-linux-x64"))
  ("src/nlp.lisp.fasl.r2r-win-arm64"
   "src/nlp.lisp.fasl.r2r-win-arm64"
   "src/nlp.lisp.fasl.r2r-linux-x64"))

(defun nlp-shipped-tree (core)
  "A system directory as a layout holds it: source, fasl, a sibling, and a build
record naming CORE."
  (let ((dir (nlp-dir "shipped")))
    (nlp-write dir "src/x.lisp" "(defun nlp-x () 1)")
    (nlp-write dir "src/x.lisp.fasl" "pretend fasl")
    (nlp-write dir "src/x.lisp.fasl.r2r-test-rid" "pretend sibling")
    (nlp-write dir "dotcl-build.sexp"
               (format nil "(:dotcl-version ~s :core ~s :platform ~s :fasls ((\"src/x.lisp.fasl\" . \"src/x.lisp\")))"
                       (lisp-implementation-version) core (nuget::%platform-features)))
    dir))

(defun nlp-mtime (path)
  (dotnet:invoke (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" path) "get_Ticks"))

;;; Same compiler: the fasl lands where ASDF looks for the source's output, newer
;;; than the source, and the sibling beside it, newer than the fasl.
(deftest nuget-library.fasls-placed-for-asdf
  (let* ((dir (nlp-shipped-tree (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))))
         (n (nuget::%place-fasls dir))
         (target (namestring (uiop:compile-file-pathname* (truename (nuget::%combine dir "src/x.lisp")))))
         (sib (concatenate 'string target ".r2r-test-rid")))
    (list n
          (dotnet:static "System.IO.File" "ReadAllText" target)
          (dotnet:static "System.IO.File" "ReadAllText" sib)
          (> (nlp-mtime target) (nlp-mtime (nuget::%combine dir "src/x.lisp")))
          (> (nlp-mtime sib) (nlp-mtime target))))
  (1 "pretend fasl" "pretend sibling" t t))


;;; Another dotcl or another platform: see nuget-fasl-version-mismatch.lisp.

;;; NuGet extracts zip times as UTC, which dates files hours into the future east
;;; of Greenwich; ASDF would then find every source newer than any fasl. The copy
;;; into the layout dates them at the copy.
(deftest nuget-library.layout-copy-is-dated-now
  (let* ((root (nlp-dir "packages"))
         (layout (nlp-dir "layout"))
         (src (nlp-write root "nlp-lib/1.2.3/dotcl/nlp-lib/a.lisp" "()")))
    (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" src
                   (dotnet:invoke (dotnet:static "System.DateTime" "get_UtcNow") "AddHours" 9d0))
    (let ((nuget::*packages-directory* root))
      (nuget::%copy-lisp-systems '(("nlp-lib" . "1.2.3")) layout))
    (<= (nlp-mtime (nuget::%combine layout "dotcl/nlp-lib/a.lisp"))
        (dotnet:invoke (dotnet:static "System.DateTime" "get_UtcNow") "get_Ticks")))
  t)

;;; --- one package, several systems --------------------------------------------
;;;
;;; A package carries the systems it was made for and the systems they depend on
;;; that dotcl does not supply, grouped by the directory of their .asd, so it
;;; loads with nothing else installed. dotcl/<id>.systems names the ones it was
;;; made for: declaring the package then loads them, with no :depends-on.

(defun nlp-two-dir-system ()
  "nlp-top in one directory, depending on nlp-dep in another."
  (let ((dep (nlp-dir "dep")))
    (nlp-write dep "nlp-dep.asd" "(defsystem \"nlp-dep\" :components ((:file \"d\")))")
    (nlp-write dep "VERSION.txt" "\"9.9\"")
    (nlp-write dep "d.lisp" "(defpackage :nlp-dep (:use :cl))")
    (asdf:load-asd (nuget::%combine dep "nlp-dep.asd"))
    (let ((top (nlp-dir "top")))
      (nlp-write top "src/nlp.lisp" "(defpackage :nlp-top (:use :cl))")
      (nlp-write top "nlp-top.asd"
                 "(defsystem \"nlp-top\" :version \"1.0.0\" :author \"a\" :description \"d\"
  :depends-on (\"nlp-dep\") :components ((:file \"src/nlp\")))")
      (asdf:load-asd (nuget::%combine top "nlp-top.asd"))
      (asdf:find-system "nlp-top"))))

(defparameter *nlp-two* (nlp-two-dir-system))

(deftest nuget-library.closure-follows-dependencies
  (mapcar #'asdf:component-name (asdf-nuget::%closure (list *nlp-two*)))
  ("nlp-dep" "nlp-top"))

;;; asdf-nuget is dotcl's own (a REQUIRE contrib), so it is not carried.
(deftest nuget-library.closure-leaves-out-dotcl-systems
  (mapcar #'asdf:component-name
          (asdf-nuget::%closure (list (asdf:find-system "nlp-lib"))))
  ("nlp-lib"))

(defparameter *nlp-two-package*
  (asdf-nuget::%pack-library *nlp-two* (nlp-dir "out") :fasl nil :version "1.0.0"))

;;; Each directory is a group of its own; a plain file beside a .asd (one a
;;; :read-file-form might read) comes along; the systems file names the root.
(deftest nuget-library.groups-per-directory
  (nlp-zip-entries *nlp-two-package*)
  ("dotcl/nlp-dep/VERSION.txt" "dotcl/nlp-dep/d.lisp" "dotcl/nlp-dep/nlp-dep.asd"
   "dotcl/nlp-top.systems" "dotcl/nlp-top/nlp-top.asd" "dotcl/nlp-top/src/nlp.lisp"
   "nlp-top.nuspec"))

(deftest nuget-library.systems-file-names-the-roots
  (values (read-from-string (nlp-zip-text *nlp-two-package* "dotcl/nlp-top.systems")))
  (:systems ("nlp-top")))

;;; The consumer reads that list from the layout it registered.
(deftest nuget-library.package-systems-from-the-layout
  (let* ((layout (nlp-dir "layout"))
         (nuget::*states* (make-hash-table :test #'equal))
         (sys (asdf:defsystem "nlp-consumer"
                :components ((:nuget "Some.Lisp.Lib" :nuget-version "1.0.0")))))
    (nlp-write layout "dotcl/some.lisp.lib.systems" "(:systems (\"nlp-top\"))")
    (setf (nuget::state-out-dir (nuget::%state (nuget::%current-rid) (nuget::%current-tfm)))
          layout)
    (asdf-nuget::%package-systems (first (asdf:component-children sys))))
  ("nlp-top"))

;;; A placed fasl is dated one second after its source, not at the moment it was
;;; placed. ASDF recompiles a file whose dependency's fasl is newer than its own,
;;; and dating by placement made whatever was placed later -- a dependency in a
;;; directory that sorts after its user -- look newer: Coalton's library compiled
;;; again in full. Sources in a layout share their copy time, so their fasls share
;;; one date whatever order they are placed in.
(deftest nuget-library.placed-fasls-dated-by-source
  (let* ((dir (nlp-shipped-tree (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))))
         (src (nuget::%combine dir "src/x.lisp")))
    (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" src
                   (dotnet:new "System.DateTime" 2020 1 1 0 0 0
                               (dotnet:static "System.DateTimeKind" "Utc")))
    (nuget::%place-fasls dir)
    (let ((target (namestring (uiop:compile-file-pathname* (truename src)))))
      (list (- (nlp-mtime target) (nlp-mtime src))
            (- (nlp-mtime (concatenate 'string target ".r2r-test-rid")) (nlp-mtime src)))))
  (10000000 20000000))

;;; A layout reached through a symbolic link (the cache under /var or /tmp on
;;; macOS): the fasl goes to the output translation of the source's truename,
;;; which is how ASDF names the component. Placed under the link's own path, it
;;; sat where ASDF never looks, and every shipped system compiled again.
;;; Creating a link on Windows needs a privilege, so there the answer is given.
(deftest nuget-library.fasls-placed-under-the-truename
  (if (dotnet:static "System.OperatingSystem" "IsWindows")
      (list 1 t)
      (let* ((dir (nlp-shipped-tree (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))))
             (link (nuget::%combine (nlp-dir "link") "via")))
        (dotnet:static "System.IO.Directory" "CreateSymbolicLink" link
                       (string-right-trim "/\\" dir))
        (let* ((linked (concatenate 'string link "/"))
               (n (nuget::%place-fasls linked))
               (target (uiop:compile-file-pathname*
                        (truename (nuget::%combine linked "src/x.lisp")))))
          (list n (and (probe-file target) t)))))
  (1 t))

;;; A packed system whose sources tie its fasls to the machine that compiles
;;; them: the pack runs the portability check, warns with a
;;; FASL-PORTABILITY-WARNING naming the place, and writes the package anyway.
(deftest-emitting-only nuget-library.pack-warns-about-unportable-sources
  (let* ((sys (nlp-system "nlp-unport" "(:file \"src/nlp\") (:file \"src/plat\")"))
         (dir (namestring (asdf:system-source-directory sys)))
         (warnings '()))
    (nlp-write dir "src/plat.lisp" "#+unix (defun nlp-plat () 1)")
    (let ((nupkg (handler-bind ((asdf-nuget:fasl-portability-warning
                                  (lambda (w) (push w warnings) (muffle-warning w))))
                   (asdf-nuget::%pack-library sys (nlp-dir "out")))))
      (list (length warnings)
            (mapcar #'asdf-nuget:finding-kind
                    (asdf-nuget:fasl-portability-warning-findings (first warnings)))
            (and (search "plat.lisp:1: read-platform" (princ-to-string (first warnings))) t)
            (and (probe-file nupkg) t))))
  (1 (:read-platform) t t))
