;;; `dotcl pack` puts the .asd's :author into the nuspec without mail addresses.
;;;
;;; By Lisp convention :author is often "Name <mail>", and nuget.org shows
;;; <authors> as a list of names, so the address ended up on the package page.
;;; pack now drops each "<...@...>" part and says so; an --authors given on the
;;; command line is used as written.
;;;
;;; Each case runs the CLI in a child process with --dry-run, which reads the
;;; metadata and prints what it will use without building anything.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; pau- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition.

(defvar *pau-dir* "test/regression/.tmp-pack-authors/")

(defvar *pau-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *pau-core*
  (regression-child-core))

(defun pau-path (name)
  (ensure-directories-exist *pau-dir*)
  (namestring (merge-pathnames name (truename *pau-dir*))))

(defun pau-write (name text)
  (with-open-file (s (pau-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun pau-pack (author &rest extra)
  "Dry-run pack a fixture whose :author is AUTHOR. Returns (exit stdout stderr)."
  (dolist (d '("app/" "from/"))
    (ensure-directories-exist (merge-pathnames d *pau-dir*)))
  (pau-write "app/pau-app.asd"
             (format nil "(defsystem \"pau-app\" :version \"1.0.0\" :description \"d\"
  :author ~s :components ())" author))
  (pau-write "from/dotcl.99.0.0.nupkg" "")
  (pau-write "from/dotcl.any.99.0.0.nupkg" "")
  (dotcl:run-process
   *pau-exe*
   (append (list "--asm" *pau-core* "--asd-search-path" (pau-path "app/")
                 "pack" "--system" "pau-app" "--id" "pau-app" "--command" "pau"
                 "--toplevel" "cl:identity"
                 "-o" (pau-path "out/") "--from" (pau-path "from/")
                 "--dotcl-version" "99.0.0" "--rids" "any" "--dry-run")
           extra)))

(defun pau-authors-line (result)
  "The authors pack reports, or NIL when it reports none."
  (let* ((out (second result))
         (tag "pack: authors \"")
         (at (search tag out)))
    (when at
      (let ((start (+ at (length tag))))
        (subseq out start (position #\" out :start start))))))

(deftest-emitting-only pack-authors.mail-is-dropped
  (let ((r (pau-pack "Ada Lovelace <ada@example.org>")))
    (list (first r) (pau-authors-line r)))
  (0 "Ada Lovelace"))

(deftest-emitting-only pack-authors.several-authors-keep-their-names
  (let ((r (pau-pack "Ada Lovelace <ada@example.org>, Alan Turing <alan@example.org>")))
    (list (first r) (pau-authors-line r)))
  (0 "Ada Lovelace, Alan Turing"))

;;; Nothing but an address: keep it (without brackets) rather than go empty.
(deftest-emitting-only pack-authors.address-only-is-kept
  (let ((r (pau-pack "<ada@example.org>")))
    (list (first r) (pau-authors-line r)))
  (0 "ada@example.org"))

;;; A plain name is used as it is, and nothing is said.
(deftest-emitting-only pack-authors.plain-name-is-unchanged
  (let ((r (pau-pack "Ada Lovelace")))
    (list (first r) (pau-authors-line r)))
  (0 nil))

;;; --authors wins and is not rewritten.
(deftest-emitting-only pack-authors.command-line-is-taken-as-written
  (let ((r (pau-pack "Ada Lovelace <ada@example.org>"
                     "--authors" "Team <team@example.org>")))
    (list (first r) (pau-authors-line r)))
  (0 nil))
