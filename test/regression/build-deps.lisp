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
