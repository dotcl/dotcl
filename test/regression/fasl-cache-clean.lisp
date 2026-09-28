;;; `dotcl clean`: removing the caches dotcl writes. The shared ASDF compile
;;; cache first, and the JIT startup profiles further down.
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
                          (regression-temp-dir)
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

;;; The JIT startup profiles: the other directory `clean` empties.
;;;
;;; One file per program under {cache-home}/dotcl/jit/, about 20 KB, and
;;; nothing removed them either: an upgrade puts the new build in a new
;;; directory and orphans the previous program's profile. Reported on its own
;;; line, with its own count and its own path, because it is a second place
;;; and the path is what a user asking "what is this file" came for.
;;;
;;; The file this very run is writing is the one thing clean leaves: the
;;; runtime writes it again as the run ends, so removing it frees nothing and
;;; costs the next start its head start. It is skipped, so it is not in the
;;; count either.

(defun %fcc-current-profile-name ()
  "The profile file name this build writes, taken from the path itself so the
   test never has to reproduce the naming rule."
  (let* ((path (dotcl::%jit-profile-path))
         (slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun %fcc-populate-jit (root &key current)
  "Fake profiles under ROOT's jit directory; returns that directory. With
   CURRENT, one of them is named like the profile this build writes."
  (let ((jit (concatenate 'string root "dotcl/jit/")))
    (ensure-directories-exist jit)
    (dolist (name (list* "oldapp-1111111111111111.profile"
                         "myapp-2222222222222222.profile"
                         ;; not a profile: clean must leave it where it is
                         "README.txt"
                         (when current (list (%fcc-current-profile-name)))))
      (with-open-file (s (concatenate 'string jit name)
                         :direction :output :if-exists :supersede)
        (write-string "not really a profile" s)))
    jit))

(defun %fcc-jit-line (out)
  "The report line naming the JIT profile directory, or an empty string when
   the command printed none."
  (or (find-if (lambda (line) (search "/dotcl/jit" line))
               (uiop:split-string (substitute #\/ #\\ out) :separator '(#\Newline)))
      ""))

(defun %fcc-jit-present (jit names)
  "Which of NAMES are still in the JIT directory."
  (let ((entries (dotcl::%jit-profile-entries jit)))
    (mapcar (lambda (n) (and (member n entries :test #'string=) t)) names)))

;;; Selection: profile files, and nothing else the directory happens to hold.
(deftest fasl-cache-clean.jit-selects-only-profile-files
  (multiple-value-bind (root cl-dir) (%fcc-fresh "jit-select")
    (declare (ignore cl-dir))
    (sort (dotcl::%jit-profile-entries (%fcc-populate-jit root)) #'string<))
  ("myapp-2222222222222222.profile" "oldapp-1111111111111111.profile"))

(deftest fasl-cache-clean.jit-dry-run-removes-nothing
  (multiple-value-bind (root cl-dir) (%fcc-fresh "jit-dry")
    (declare (ignore cl-dir))
    (let* ((jit (%fcc-populate-jit root))
           (result (%fcc-run root (list "clean" "--dry-run"))))
      (list (first result)
            (and (search "would remove 2 JIT profiles" (%fcc-jit-line (second result))) t)
            (%fcc-jit-present jit '("oldapp-1111111111111111.profile"
                                    "myapp-2222222222222222.profile")))))
  (0 t (t t)))

(deftest fasl-cache-clean.jit-removes-profiles
  (multiple-value-bind (root cl-dir) (%fcc-fresh "jit-remove")
    (declare (ignore cl-dir))
    (let* ((jit (%fcc-populate-jit root))
           (result (%fcc-run root (list "clean"))))
      (list (first result)
            (and (search "removed 2 JIT profiles" (%fcc-jit-line (second result))) t)
            (%fcc-jit-present jit '("oldapp-1111111111111111.profile"
                                    "myapp-2222222222222222.profile"))
            (and (probe-file (concatenate 'string jit "README.txt")) t))))
  (0 t (nil nil) t))

;;; An empty profile directory is reported, not passed over: the line names
;;; the location, which is the answer someone running clean is looking for,
;;; and the compile cache says the same thing when it is empty.
(deftest fasl-cache-clean.jit-nothing-to-remove
  (let ((root (concatenate 'string *fcc-tmp* "jit-empty/")))
    (ignore-errors (uiop:delete-directory-tree (pathname root) :validate t))
    (ensure-directories-exist (concatenate 'string root "common-lisp/"))
    (let* ((result (%fcc-run root (list "clean")))
           (out (second result)))
      (list (first result)
            (and (search "nothing to remove" (%fcc-jit-line out)) t)
            ;; both locations, one line each
            (count-if (lambda (line) (search "nothing to remove" line))
                      (uiop:split-string out :separator '(#\Newline))))))
  (0 t 2))

;;; The profile of the run doing the cleaning stays, and is not counted as
;;; removed. A second clean then finds nothing, rather than reporting the same
;;; file gone again on every run.
(deftest fasl-cache-clean.jit-keeps-the-profile-in-use
  (multiple-value-bind (root cl-dir) (%fcc-fresh "jit-inuse")
    (declare (ignore cl-dir))
    (let* ((jit (%fcc-populate-jit root :current t))
           (mine (%fcc-current-profile-name))
           (first-run (%fcc-run root (list "clean")))
           (left (dotcl::%jit-profile-entries jit))
           (second-run (%fcc-run root (list "clean"))))
      (list (first first-run)
            (and (search "removed 2 JIT profiles" (%fcc-jit-line (second first-run))) t)
            (and (member mine left :test #'string=) t)
            (first second-run)
            (and (search "nothing to remove" (%fcc-jit-line (second second-run))) t))))
  (0 t t 0 t))

;;; A mistyped option must not be read as "remove everything".
(deftest fasl-cache-clean.unknown-option-is-rejected
  (multiple-value-bind (root cl-dir) (%fcc-fresh "badopt")
    (let ((result (%fcc-run root (list "clean" "--everything"))))
      (list (first result)
            (and (search "unknown option" (third result)) t)
            (length (dotcl::%fasl-cache-entries cl-dir)))))
  (2 t 2))
