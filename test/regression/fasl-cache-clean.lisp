;;; `dotcl clean` — removing the shared ASDF compile cache.
;;;
;;; ASDF writes every fasl it compiles under
;;; {cache-home}/common-lisp/{implementation-identifier}/, outside any project
;;; and untouched by `dotnet clean`. The identifier carries the exact build, so
;;; one directory accumulates per build and nothing ever removed them (605 of
;;; them, 1.2 GB, on the machine this was written on).
;;;
;;; Two things are pinned here. The location, which the CLI recomputes in C#
;;; rather than asking a running Lisp (cleaning must not need the core that a
;;; broken cache stops from loading) -- so it is compared against uiop's own
;;; answer, which is the definition. And the selection rule, which must remove
;;; only dotcl's own per-build directories: another implementation's cache, a
;;; plain file with a dotcl- name, and anything reached through a symlink stay.
;;;
;;; The end-to-end cases run this same executable again with XDG_CACHE_HOME
;;; pointed at a directory the test builds, so nothing outside it is at risk.

(require "asdf")

(defvar *fcc-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *fcc-tmp*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fasl-cache-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fcc-populate (root)
  "Build a fake cache under ROOT and return its common-lisp directory."
  (let ((cl-dir (concatenate 'string root "common-lisp/")))
    (dolist (d '("dotcl-0.0.1-test-x64/sub/" "dotcl-0.0.2-test-x64/" "sbcl-9.9.9-test-x64/"))
      (ensure-directories-exist (concatenate 'string cl-dir d)))
    (with-open-file (s (concatenate 'string cl-dir "dotcl-0.0.1-test-x64/sub/a.fasl")
                       :direction :output :if-exists :supersede)
      (write-string "not really a fasl" s))
    (with-open-file (s (concatenate 'string cl-dir "dotcl-not-a-directory")
                       :direction :output :if-exists :supersede)
      (write-string "x" s))
    cl-dir))

(defun %fcc-fresh (name)
  "A freshly (re)created fake cache root named NAME, with its common-lisp dir."
  (let ((root (concatenate 'string *fcc-tmp* name "/")))
    (ignore-errors (uiop:delete-directory-tree (pathname root) :validate t))
    (ensure-directories-exist root)
    (values root (%fcc-populate root))))

(defun %fcc-run (root args)
  "Run the CLI with XDG_CACHE_HOME pointed at ROOT. Returns (exit stdout stderr)."
  (let ((saved (dotcl:getenv "XDG_CACHE_HOME")))
    (unwind-protect
         ;; Forward slashes on both platforms: .NET accepts them on Windows, and
         ;; converting would break the POSIX path.
         (progn (dotcl:setenv "XDG_CACHE_HOME" root)
                (dotcl:run-process *fcc-exe* args))
      (dotcl:setenv "XDG_CACHE_HOME" (or saved "")))))

(defun %fcc-survivors (cl-dir)
  "What is left under CL-DIR that clean must never touch: the other
   implementation's cache directory and the plain file with a dotcl- name."
  (list (and (probe-file (concatenate 'string cl-dir "sbcl-9.9.9-test-x64/")) t)
        (and (probe-file (concatenate 'string cl-dir "dotcl-not-a-directory")) t)))

;;; The location must be the one uiop computes -- the C# copy exists for speed,
;;; not to have an opinion.
(deftest fasl-cache-clean.root-matches-uiop
  (string= (dotcl::%fasl-cache-root)
           (string-right-trim "/" (namestring (uiop:xdg-cache-home "common-lisp/"))))
  t)

;;; Selection: dotcl's own per-build directories, nothing else.
(deftest fasl-cache-clean.selects-only-dotcl-directories
  (multiple-value-bind (root cl-dir) (%fcc-fresh "select")
    (declare (ignore root))
    (sort (dotcl::%fasl-cache-entries cl-dir) #'string<))
  ("dotcl-0.0.1-test-x64" "dotcl-0.0.2-test-x64"))

(deftest fasl-cache-clean.dry-run-removes-nothing
  (multiple-value-bind (root cl-dir) (%fcc-fresh "dry")
    (let* ((result (%fcc-run root (list "clean" "--dry-run")))
           (out (second result)))
      (list (first result)
            (and (search "would remove 2 cache directories" out) t)
            (sort (dotcl::%fasl-cache-entries cl-dir) #'string<)
            (%fcc-survivors cl-dir))))
  (0 t ("dotcl-0.0.1-test-x64" "dotcl-0.0.2-test-x64") (t t)))

(deftest fasl-cache-clean.removes-only-its-own
  (multiple-value-bind (root cl-dir) (%fcc-fresh "remove")
    (let* ((result (%fcc-run root (list "clean")))
           (out (second result)))
      (list (first result)
            (and (search "removed 2 cache directories" out) t)
            (dotcl::%fasl-cache-entries cl-dir)
            (%fcc-survivors cl-dir))))
  (0 t nil (t t)))

;;; --keep-current spares this build's own directory so the next start does not
;;; recompile everything.
(deftest fasl-cache-clean.keep-current-spares-this-build
  (multiple-value-bind (root cl-dir) (%fcc-fresh "keep")
    (let* ((mine-name (concatenate 'string "dotcl-" (lisp-implementation-version)
                                   "-test-x64"))
           (mine (concatenate 'string cl-dir mine-name "/")))
      (ensure-directories-exist mine)
      (let ((result (%fcc-run root (list "clean" "--keep-current"))))
        (list (first result)
              (and (probe-file mine) t)
              ;; the two fakes are gone, this build's directory is all that is left
              (equal (dotcl::%fasl-cache-entries cl-dir) (list mine-name))))))
  (0 t t))

;;; An empty cache is not an error.
(deftest fasl-cache-clean.nothing-to-remove
  (let ((root (concatenate 'string *fcc-tmp* "empty/")))
    (ensure-directories-exist (concatenate 'string root "common-lisp/"))
    (let ((result (%fcc-run root (list "clean"))))
      (list (first result) (and (search "nothing to remove" (second result)) t))))
  (0 t))

;;; A mistyped option must not be read as "remove everything".
(deftest fasl-cache-clean.unknown-option-is-rejected
  (multiple-value-bind (root cl-dir) (%fcc-fresh "badopt")
    (let ((result (%fcc-run root (list "clean" "--everything"))))
      (list (first result)
            (and (search "unknown option" (third result)) t)
            (length (dotcl::%fasl-cache-entries cl-dir)))))
  (2 t 2))
