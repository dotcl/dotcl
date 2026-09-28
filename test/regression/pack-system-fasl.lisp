;;; `dotcl pack` builds a system and its whole closure into one fasl.
;;;
;;; Two things are pinned here, each of which used to stop a pack outright.
;;;
;;; 1. Read-time eval in a dependency. pack used to gather the closure through
;;;    ASDF's MONOLITHIC-CONCATENATE-SOURCE-OP and compile the result as a
;;;    single unit. #. assumes the earlier forms have been evaluated, and in one
;;;    unit they have only been compiled, so a system whose dependency defines
;;;    the value a later file reads with #. died while reading. cl-ppcre,
;;;    flexi-streams, cl-unicode and cl-interpol all write their optimize
;;;    settings that way, so this was most of a real closure. pack now compiles
;;;    the closure one source at a time in dependency order, which is what
;;;    SAVE-APPLICATION :SYSTEM already did.
;;;
;;; 2. Generated sources. A system can add to what ASDF prepares before it
;;;    builds, and generate its own sources there; cl-unicode writes its
;;;    character tables that way. The walk never asked, so it reached a source
;;;    file that did not exist. The generator in this fixture stops right after
;;;    writing its file, the way cl-unicode's does, so the case where a
;;;    generator fails with the files that matter already written is covered
;;;    too: what decides the build is whether the sources are there.
;;;
;;; The fixture is three tiny systems rather than a real library. What is being
;;; pinned is the order the closure is collected and compiled in, which a
;;; two-file system shows as well as a two-hundred-file one, and a Quicklisp
;;; system would cost minutes.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; psf- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition and read it again.

(require "asdf")

(defvar *psf-dir* "test/regression/.tmp-pack-system/")
(defvar *psf-result* :unset)

(defun psf-write (name text)
  (with-open-file (s (merge-pathnames name (truename *psf-dir*))
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun psf-path (name)
  (namestring (merge-pathnames name (truename *psf-dir*))))

(defun psf-setup ()
  "Write the fixture systems and make ASDF able to find them.
Removes anything an earlier run generated: a table file left behind would make
the generator test pass without generating anything."
  (ensure-directories-exist *psf-dir*)

  ;; A dependency that defines, at load time, the value its dependent reads with
  ;; #. at read time.
  (psf-write "psf-lib.asd"
             "(defsystem \"psf-lib\" :serial t :components ((:file \"psf-lib\")))")
  (psf-write "psf-lib.lisp"
             "(defpackage #:psf-lib (:use #:cl) (:export #:*settings* #:value))
(in-package #:psf-lib)
(defparameter *settings* '(optimize (speed 3)))
(defun value () 4321)")
  (psf-write "psf-app.asd"
             "(defsystem \"psf-app\" :serial t :depends-on (\"psf-lib\")
  :components ((:file \"psf-app\")))")
  (psf-write "psf-app.lisp"
             "(defpackage #:psf-app (:use #:cl) (:export #:main))
(in-package #:psf-app)
(defun main ()
  (declare #.psf-lib:*settings*)
  (setf cl-user::*psf-result* (psf-lib:value)))")

  ;; A system that generates one of its own sources, the shape cl-unicode uses:
  ;; while the generated file is missing, prepare-op gains a dependency on the
  ;; system that writes it.
  (psf-write "psf-gen.asd"
             "(defsystem \"psf-gen\" :serial t :components ((:file \"psf-gen-table\")))
(defsystem \"psf-gen/build\" :serial t :components ((:file \"psf-gen-build\")))
(defmethod component-depends-on ((o prepare-op)
                                 (c (eql (find-system \"psf-gen\"))))
  `(,@(unless (probe-file (merge-pathnames \"psf-gen-table.lisp\"
                                           (system-source-directory \"psf-gen\")))
        '((load-op \"psf-gen/build\")))
    ,@(call-next-method)))")
  (psf-write "psf-gen-build.lisp"
             "(in-package #:cl-user)
(with-open-file (s (merge-pathnames \"psf-gen-table.lisp\"
                                    (asdf:system-source-directory \"psf-gen\"))
                   :direction :output
                   :if-exists :supersede :if-does-not-exist :create)
  (write-string \"(in-package #:cl-user)(defun psf-table () 1234)\" s))
(error \"psf generator stops after writing its table\")")
  (ignore-errors (delete-file (psf-path "psf-gen-table.lisp")))

  (pushnew (pathname (namestring (truename *psf-dir*)))
           asdf:*central-registry* :test #'equal)
  ;; A run that already built these saw a different set of files on disk.
  (asdf:clear-system "psf-gen")
  (asdf:clear-system "psf-gen/build"))

(defun psf-pack (system output &optional toplevel)
  "Build SYSTEM's closure into one fasl at OUTPUT, the way `dotcl pack` does."
  (dotnet:static "DotCL.DotclBuild" "PackFasl" system output toplevel nil nil)
  output)

(defun psf-build-app ()
  "Pack the system whose source reads a dependency's value with #., and run it.
The value comes back through *PSF-RESULT*, which the packed toplevel sets."
  (psf-setup)
  (setf *psf-result* :unset)
  (load (psf-pack "psf-app" (psf-path "psf-app.fasl") "psf-app:main"))
  *psf-result*)

(defun psf-build-generated ()
  "Pack the system that generates one of its own sources.
Returns whether the source was generated and what the packed fasl defines."
  (psf-setup)
  (let ((fasl (psf-pack "psf-gen" (psf-path "psf-gen.fasl"))))
    (list (notnot (probe-file (psf-path "psf-gen-table.lisp")))
          (progn (load fasl) (funcall (find-symbol "PSF-TABLE" "CL-USER"))))))

;;; Without the fix this signals while reading psf-app.lisp: *SETTINGS* is
;;; compiled into the same unit and never evaluated, so #. has nothing to read.
(deftest-emitting-only pack-system-fasl.read-time-eval-in-a-dependency
  (psf-build-app)
  4321)

;;; Without the fix the generator never runs and the build walks into
;;; psf-gen-table.lisp, which is not there.
(deftest-emitting-only pack-system-fasl.generated-source-is-built
  (psf-build-generated)
  (t 1234))
