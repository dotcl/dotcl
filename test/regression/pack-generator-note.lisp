;;; `dotcl pack` runs a system's source generator (a load-op a system asks for
;;; in its prepare-op, the way cl-unicode builds its tables) and reports it when
;;; the generator stops. When the generator had already written every file it
;;; declares, the note now says the build has them and goes on, so it does not
;;; read as the build's own failure. When files are still missing, the existing
;;; list of them is kept.
;;;
;;; Each case runs the CLI in a child process: the generator's load-op changes
;;; the image. The donor directory is empty, so the run stops after the fasl
;;; build, at restamping; only the fasl build's output is looked at.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; pgn- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition.

(defvar *pgn-dir* "test/regression/.tmp-pack-generator-note/")

(defvar *pgn-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *pgn-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun pgn-path (name)
  (ensure-directories-exist *pgn-dir*)
  (namestring (merge-pathnames name (truename *pgn-dir*))))

(defun pgn-write (name text)
  (with-open-file (s (pgn-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun pgn-asd (name writes-first)
  "A system NAME whose one source, gen.lisp, is written by NAME/build. The
generator signals an error, after writing gen.lisp when WRITES-FIRST."
  (format nil "(defsystem ~s :version \"1.0.0\" :description \"d\" :author \"a\"
  :components ((:file \"gen\")))
(defsystem ~s
  :components ()
  :output-files (load-op (o c) (values '(\"gen.lisp\") t))
  :perform (load-op (o c)
             ~:[~;(with-open-file (s (merge-pathnames \"gen.lisp\" (system-source-directory c))
                                 :direction :output :if-exists :supersede)
               (write-string \"(defvar *pgn-generated* t)\" s))~]
             (error \"generator stopped in its test data\")))
(defmethod component-depends-on ((o prepare-op) (c (eql (find-system ~s))))
  `(,@(unless (every 'probe-file (output-files 'load-op ~s))
        '((load-op ~s)))
    ,@(call-next-method)))"
          name (format nil "~a/build" name) writes-first
          name (format nil "~a/build" name) (format nil "~a/build" name)))

(defun pgn-pack (name writes-first)
  "Pack NAME with a fresh generator. Returns the child's stderr."
  (let ((dir (format nil "~a/" name)))
    (ensure-directories-exist (merge-pathnames dir *pgn-dir*))
    (ensure-directories-exist (merge-pathnames "from/" *pgn-dir*))
    (let ((gen (pgn-path (concatenate 'string dir "gen.lisp"))))
      (when (probe-file gen) (delete-file gen)))
    (pgn-write (concatenate 'string dir name ".asd") (pgn-asd name writes-first))
    (third (dotcl:run-process
            *pgn-exe*
            (list "--asm" *pgn-core* "--asd-search-path" (pgn-path dir)
                  "pack" "--system" name "--id" name "--command" name
                  "--toplevel" "cl:identity"
                  "-o" (pgn-path "out/") "--from" (pgn-path "from/")
                  "--dotcl-version" "99.0.0" "--rids" "any")))))

(defun pgn-says (text err)
  (and (search text err) t))

;;; Stopped after writing: the note says the build goes on, and the build does.
(deftest-emitting-only pack-generator-note.written-files-are-reported-present
  (let ((err (pgn-pack "pgn-wrote" t)))
    (list (pgn-says "generating pgn-wrote/build for pgn-wrote stopped" err)
          (pgn-says "are all present, so the build goes on" err)
          (pgn-says "had not written yet" err)
          (pgn-says "fasl build failed" err)))
  (t t nil nil))

;;; Stopped before writing: the missing file is still named, and the build fails.
(deftest-emitting-only pack-generator-note.missing-files-are-still-listed
  (let ((err (pgn-pack "pgn-nowrite" nil)))
    (list (pgn-says "generating pgn-nowrite/build for pgn-nowrite stopped" err)
          (pgn-says "are all present" err)
          (pgn-says "had not written yet" err)
          (pgn-says "fasl build failed" err)))
  (t nil t t))
