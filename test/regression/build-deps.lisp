;;; `dotcl build <asd> --output <fasl>` loads the system's :depends-on closure
;;; before compiling it, and leaves no intermediate files next to the output.
;;;
;;; The build loaded a dependency only when resolve-deps had already left its
;;; fasl in the project deps/ cache. The MSBuild targets run resolve-deps first;
;;; the CLI does not, so every dependency was skipped without a word and the
;;; root failed to read with "Package X not found". The concatenated unit it
;;; compiled (<out>.concat.lisp) also stayed beside the fasl.
;;;
;;; Each case runs the CLI in a child process. The fixture systems live in .asd
;;; files written to a temp dir (see build-modules.lisp for why).
;;;
;;; NOTE: stay in the default load package (CL-USER); helpers are bdep- prefixed.

(defvar *bdep-dir* "test/regression/.tmp-build-deps/")

(defvar *bdep-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *bdep-core*
  (regression-child-core))

(defun bdep-path (name)
  (namestring (merge-pathnames name (truename *bdep-dir*))))

(defun bdep-write (name text)
  (with-open-file (s (bdep-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun bdep-setup ()
  (dolist (d '("" "lib/" "app/" "app/src/" "out1/" "out2/"))
    (ensure-directories-exist (merge-pathnames d *bdep-dir*)))
  (dolist (d '("out1/" "out2/"))
    (dolist (f (directory (merge-pathnames "*.*" (bdep-path d))))
      (ignore-errors (delete-file f))))
  (bdep-write "lib/bdep-lib.asd"
              "(defsystem \"bdep-lib\" :components ((:file \"lib\")))")
  (bdep-write "lib/lib.lisp"
              "(defpackage #:bdep-lib (:use #:cl) (:export #:twice))
(in-package #:bdep-lib)
(defmacro twice (x) `(* 2 ,x))")
  (bdep-write "app/bdep-app.asd"
              "(defsystem \"bdep-app\"
  :depends-on (\"bdep-lib\")
  :components ((:file \"top\")
               (:module \"src\" :components ((:file \"main\")))))")
  (bdep-write "app/top.lisp"
              "(defpackage #:bdep-app (:use #:cl))")
  ;; A macro from the dependency, so the root cannot even be read, let alone
  ;; compiled, without the dependency loaded first.
  (bdep-write "app/src/main.lisp"
              "(in-package #:bdep-app)
(defun report () (format t \"VAL=~s~%\" (bdep-lib:twice 21)))"))

(defun bdep-run (&rest args)
  (dotcl:run-process *bdep-exe* (list* "--asm" *bdep-core* args)))

(defun bdep-leftovers (dir)
  (loop for f in (directory (merge-pathnames "*.*" (bdep-path dir)))
        for name = (file-namestring f)
        when (or (search ".concat." name) (search ".nuget-preamble." name))
          collect name))

(defun bdep-load-and-report (fasl)
  (bdep-run "--eval" "(require \"asdf\")"
            "--eval" (format nil "(push ~s asdf:*central-registry*)" (bdep-path "lib/"))
            "--eval" "(asdf:load-system \"bdep-lib\")"
            "--eval" (format nil "(load ~s)" fasl)
            "--eval" "(bdep-app::report)"))

;;; CLI build with nothing prebuilt: the dependency is loaded through ASDF, the
;;; module file is compiled, and the concat unit is gone afterwards.
(deftest-emitting-only build-deps.cli-build-loads-dependencies
  (progn
    (bdep-setup)
    (let* ((fasl (bdep-path "out1/bdep-app.fasl"))
           (b (bdep-run "build" (bdep-path "app/bdep-app.asd") "--output" fasl
                        "--asd-search-path" (bdep-path "lib/")))
           (r (bdep-load-and-report fasl)))
      (list (first b) (bdep-leftovers "out1/")
            (first r) (and (search "VAL=42" (second r)) t))))
  (0 nil 0 t))

;;; The MSBuild order (resolve-deps, then build next to the manifest) still
;;; takes the dependency from the fasl resolve-deps built, not through ASDF.
(deftest-emitting-only build-deps.resolved-fasls-are-used
  (progn
    (bdep-setup)
    (let* ((fasl (bdep-path "out2/bdep-app.fasl"))
           (rd (bdep-run "build" (bdep-path "app/bdep-app.asd") "--resolve-deps"
                         "--manifest-out" (bdep-path "out2/manifest.txt")
                         "--asd-search-path" (bdep-path "lib/")))
           (b (bdep-run "build" (bdep-path "app/bdep-app.asd") "--output" fasl
                        "--asd-search-path" (bdep-path "lib/")))
           (r (bdep-load-and-report fasl)))
      (list (first rd) (first b)
            (and (search "through ASDF" (third b)) t)
            (bdep-leftovers "out2/")
            (first r) (and (search "VAL=42" (second r)) t))))
  (0 0 nil nil 0 t))

;;; A dependency's fasl is rebuilt when the dependency's source is newer than
;;; it, and only then. resolve-deps used to check only that the fasl existed,
;;; so an edited dependency kept shipping the fasl of the first build until
;;; someone deleted it by hand.
(defun bdep-touch-later (name)
  ;; Push the file's time well past the fasl's, so the check does not hinge on
  ;; the file system's timestamp resolution.
  (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" (bdep-path name)
                 (dotnet:invoke (dotnet:static "System.DateTime" "UtcNow") "AddSeconds" 10)))

(deftest-emitting-only build-deps.stale-dependency-fasl-is-rebuilt
  (progn
    (bdep-setup)
    (dolist (f (directory (merge-pathnames "*.*" (bdep-path "out2/deps/"))))
      (ignore-errors (delete-file f)))
    (flet ((resolve ()
             (bdep-run "build" (bdep-path "app/bdep-app.asd") "--resolve-deps"
                       "--manifest-out" (bdep-path "out2/manifest.txt")
                       "--asd-search-path" (bdep-path "lib/")))
           (compiled-p (r) (and (search "compiling bdep-lib" (second r)) t)))
      (let* ((r1 (resolve))
             (r2 (resolve))
             (r3 (progn
                   (bdep-write "lib/lib.lisp"
                               "(defpackage #:bdep-lib (:use #:cl) (:export #:twice))
(in-package #:bdep-lib)
(defmacro twice (x) `(* 3 ,x))")
                   (bdep-touch-later "lib/lib.lisp")
                   (resolve)))
             (fasl (bdep-path "out2/deps/bdep-lib.fasl"))
             (v (bdep-run "--eval" (format nil "(load ~s)" fasl)
                          "--eval" "(print (bdep-lib:twice 2))")))
        (list (first r1) (compiled-p r1)
              (first r2) (compiled-p r2)
              (first r3) (compiled-p r3)
              (first v) (and (search "6" (second v)) t)))))
  (0 t 0 nil 0 t 0 t))

;;; A dependency on a contrib that has no code of its own in its .asd (a
;;; REQUIRE-SYSTEM stub such as dotcl-socket) is REQUIREd while the build
;;; compiles what depends on it, and is named in the manifest as a "require"
;;; line ahead of the fasls that need it, which LoadFromManifest REQUIREs.
;;; The stubs used to be skipped as systems without components: the dependent
;;; failed to compile with Package "DOTCL-SOCKET" not found, and once it was
;;; built some other way the application failed to start for the same reason.
(defun bdep-contrib-setup ()
  (bdep-setup)
  (dolist (d '("sock/" "sockapp/" "out3/" "out3/deps/"))
    (ensure-directories-exist (merge-pathnames d *bdep-dir*)))
  (dolist (f (directory (merge-pathnames "*.*" (bdep-path "out3/deps/"))))
    (ignore-errors (delete-file f)))
  (bdep-write "sock/bdep-sock.asd"
              "(defsystem \"bdep-sock\"
  :depends-on ((:feature :dotcl \"dotcl-socket\") \"dotcl-thread\")
  :components ((:file \"sock\")))")
  ;; Read-time references: the file cannot even be read without both packages.
  (bdep-write "sock/sock.lisp"
              "(defpackage #:bdep-sock (:use #:cl))
(in-package #:bdep-sock)
(defun probe ()
  (list (fboundp 'dotcl-socket:make-server-socket)
        (fboundp 'dotcl-thread:make-thread)))")
  (bdep-write "sockapp/bdep-sockapp.asd"
              "(defsystem \"bdep-sockapp\" :depends-on (\"bdep-sock\")
  :components ((:file \"app\")))")
  (bdep-write "sockapp/app.lisp" "(defpackage #:bdep-sockapp (:use #:cl))"))

(deftest-emitting-only build-deps.contrib-stub-dependencies-are-required
  (progn
    (bdep-contrib-setup)
    (let* ((asd (bdep-path "sockapp/bdep-sockapp.asd"))
           (manifest (bdep-path "out3/m.txt"))
           (rd (bdep-run "build" asd "--resolve-deps" "--manifest-out" manifest
                         "--asd-search-path" (bdep-path "sock/")))
           (lines (with-open-file (s manifest)
                    (loop for l = (read-line s nil) while l collect l)))
           (b (bdep-run "build" asd "--output" (bdep-path "out3/bdep-sockapp.fasl")
                        "--asd-search-path" (bdep-path "sock/")))
           (deploy (bdep-path "out3/deploy.txt")))
      (with-open-file (s deploy :direction :output :if-exists :supersede)
        (dolist (l lines) (write-line l s))
        (write-line "bdep-sockapp.fasl" s))
      ;; A fresh image, as an application starts: nothing required yet.
      (let ((r (bdep-run "--eval"
                         (format nil "(dotnet:static \"DotCL.DotclHost\" \"LoadFromManifest\" ~s)"
                                 deploy)
                         "--eval" "(print (bdep-sock::probe))")))
        (list (first rd)
              (subseq lines 0 3)
              (and (search "bdep-sock.fasl" (fourth lines)) t)
              (first b)
              (first r) (and (search "(T T)" (second r)) t)))))
  (0 ("require dotcl-bsd-sockets" "require dotcl-socket" "require dotcl-thread") t 0 0 t))

;;; A dependency is built the way ASDF builds it: each file is compiled and
;;; loaded before the next is read. resolve-deps used to concatenate the
;;; dependency's files and compile them as one, so a later file that read at
;;; read time what an earlier one registered when loaded (micros'
;;; #.(gray-package-name)) failed with "Unbound variable: *IMPLS*".
(defun bdep-serial-setup ()
  (bdep-setup)
  (dolist (d '("rt/" "rtroot/" "out4/" "out4/deps/"))
    (ensure-directories-exist (merge-pathnames d *bdep-dir*)))
  (dolist (f (directory (merge-pathnames "*.*" (bdep-path "out4/deps/"))))
    (ignore-errors (delete-file f)))
  (bdep-write "rt/rt.asd"
              "(defsystem \"rt\" :serial t :components ((:file \"a\") (:file \"b\") (:file \"c\")))")
  (bdep-write "rt/a.lisp"
              "(defpackage :rt-a (:use :cl))
(in-package :rt-a)
(defvar *impls* (make-hash-table))
(defmacro defimpl (name val) `(setf (gethash ',name *impls*) ,val))
(defun impl (name) (or (gethash name *impls*) (error \"~a not implemented\" name)))")
  (bdep-write "rt/b.lisp"
              "(in-package :rt-a)
(defimpl pkg-name \"COMMON-LISP\")")
  (bdep-write "rt/c.lisp"
              "(in-package :rt-a)
(defun uses () (find-package #.(impl 'pkg-name)))")
  (bdep-write "rtroot/rtroot.asd"
              "(defsystem \"rtroot\" :depends-on (\"rt\") :components ((:file \"root\")))")
  (bdep-write "rtroot/root.lisp"
              "(defpackage :rtroot (:use :cl))
(in-package :rtroot)
(defun run () (rt-a::uses))"))

(deftest-emitting-only build-deps.dependency-files-are-loaded-before-the-next-compiles
  (progn
    (bdep-serial-setup)
    (let* ((asd (bdep-path "rtroot/rtroot.asd"))
           (rd (bdep-run "build" asd "--resolve-deps"
                         "--manifest-out" (bdep-path "out4/m.txt")
                         "--asd-search-path" (bdep-path "rt/")))
           (root (bdep-path "out4/rtroot.fasl"))
           (b (bdep-run "build" asd "--output" root "--asd-search-path" (bdep-path "rt/")))
           (r (bdep-run "--eval" (format nil "(load ~s)" (bdep-path "out4/deps/rt.fasl"))
                        "--eval" (format nil "(load ~s)" root)
                        "--eval" "(print (rtroot::run))")))
      (list (first rd) (first b)
            (and (search "through ASDF" (third b)) t)
            (first r) (and (search "#<PACKAGE \"COMMON-LISP\">" (second r)) t))))
  (0 0 nil 0 t))
