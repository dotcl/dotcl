;;;; rank.lisp -- rank Quicklisp projects by how much of the rest of Quicklisp
;;;; is built on them.
;;;;
;;;; Stage 1 of the library-status pipeline (see README.md). Reads the dist
;;;; metadata that Quicklisp already keeps on disk, plus the project sources in
;;;; a quicklisp-projects checkout (to tell who owns each project), and writes
;;;; the list of systems worth checking, most depended upon first:
;;;;
;;;;   targets.txt   one system name per line, in rank order
;;;;   targets.tsv   rank, system, release, the count the order came from, and
;;;;                 the direct referrer count beside it
;;;;
;;;; Run it with any conforming Lisp; it uses nothing outside the standard plus
;;;; one environment lookup:
;;;;
;;;;   sh bench/library-status/fetch-ql-projects.sh out/quicklisp-projects
;;;;   DOTCL_QL_PROJECTS=out/quicklisp-projects \
;;;;     sbcl --non-interactive --load bench/library-status/rank.lisp
;;;;
;;;; The dist directory is found automatically (see %DIST-DIRECTORY); set
;;;; DOTCL_QL_DIST to point at another one. Nothing is downloaded: a machine
;;;; with no Quicklisp installed is told where to get the three files.

(defpackage :library-status/rank
  (:use :cl)
  (:export #:main))

(in-package :library-status/rank)

(defparameter *threshold* 4
  "Write out every project with at least this many dependents (see
%COUNT-DEPENDENTS for what counts as one). THRESHOLD=N in the environment
overrides it.

A cut-off in the units the ranking is made of rather than a number of rows:
\"everything at least four other people's projects are built on\" stays the
same sentence as the dist grows, and says how far down the tail the table
reaches.")

(defun %getenv (name)
  (declare (ignorable name))
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+clisp (ext:getenv name)
  #+ecl (ext:getenv name)
  #+dotcl (dotnet:static "System.Environment" "GetEnvironmentVariable" name)
  #-(or sbcl ccl clisp ecl dotcl) nil)

(defun %join (&rest parts)
  "Join PARTS with forward slashes, which every host here accepts."
  (format nil "~{~A~^/~}" parts))

(defun %dist-directory ()
  "The directory holding distinfo.txt / systems.txt / releases.txt.

DOTCL_QL_DIST wins. Otherwise look where a Quicklisp already installed on this
machine keeps them: the stock home first, then the private home dotcl uses so
that it does not disturb one belonging to another implementation."
  (let ((explicit (%getenv "DOTCL_QL_DIST")))
    (when (and explicit (plusp (length explicit)))
      (return-from %dist-directory explicit)))
  (let* ((home (or (%getenv "HOME") (%getenv "USERPROFILE")))
         (appdata (%getenv "APPDATA"))
         (xdg (%getenv "XDG_DATA_HOME"))
         (tail "quicklisp/dists/quicklisp")
         (candidates
           (remove nil
                   (list (when home (%join home "quicklisp/dists/quicklisp"))
                         (when appdata (%join appdata "dotcl" tail))
                         (when xdg (%join xdg "dotcl" tail))
                         (when home (%join home ".local/share/dotcl" tail))))))
    (or (find-if (lambda (dir) (probe-file (%join dir "systems.txt"))) candidates)
        (error "No Quicklisp dist found. Looked in:~%~{  ~A~%~}~
Set DOTCL_QL_DIST to a directory holding distinfo.txt, systems.txt and~%~
releases.txt, or fetch them:~%~
  curl -sSLO http://beta.quicklisp.org/dist/quicklisp/<version>/distinfo.txt~%~
  curl -sSLO http://beta.quicklisp.org/dist/quicklisp/<version>/systems.txt~%~
  curl -sSLO http://beta.quicklisp.org/dist/quicklisp/<version>/releases.txt"
               candidates))))

;;; --- reading the metadata --------------------------------------------------
;;;
;;; All three files are whitespace separated with a leading comment line, so one
;;; splitter does for all of them.

(defun %split-words (line)
  (let ((words '()) (start nil))
    (dotimes (i (length line))
      (let ((space (member (char line i) '(#\Space #\Tab) :test #'char=)))
        (cond ((and space start) (push (subseq line start i) words) (setf start nil))
              ((and (not space) (null start)) (setf start i)))))
    (when start (push (subseq line start) words))
    (nreverse words)))

(defun %map-data-lines (path fn)
  "Call FN with the word list of each non-comment, non-blank line of PATH."
  (with-open-file (in path :direction :input :external-format :utf-8)
    (loop for line = (read-line in nil nil)
          while line
          do (let ((words (%split-words line)))
               (when (and words (char/= (char (first words) 0) #\#))
                 (funcall fn words))))))

(defun %read-distinfo (dir)
  "The dist name and version, as two strings."
  (let ((name "quicklisp") (version "unknown"))
    (with-open-file (in (%join dir "distinfo.txt") :direction :input
                                                   :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((colon (position #\: line)))
                 (when colon
                   (let ((key (string-trim " " (subseq line 0 colon)))
                         (value (string-trim " " (subseq line (1+ colon)))))
                     (cond ((string= key "name") (setf name value))
                           ((string= key "version") (setf version value))))))))
    (values name version)))

(defstruct (project (:conc-name proj-))
  (name "" :type string)
  (release nil)                         ; archive prefix, e.g. "alexandria-20241012-git"
  (systems '())                         ; system names belonging to this project
  (referrers '())                       ; names of OTHER projects that depend on it
  (dependents '())                      ; the same, through non-test systems only
  (owner nil)                           ; see %PROJECT-OWNER
  (score 0)                             ; what the ranking sorts by, see %SCORE
  (system-refs (make-hash-table :test #'equal))) ; system name -> referrer count

(defun %read-systems (dir)
  "Parse systems.txt into (values PROJECTS SYSTEM->PROJECT EDGES).

PROJECTS maps a project name to a PROJECT struct, SYSTEM->PROJECT maps a system
name to its project name, and EDGES is a list of (SYSTEM-NAME . DEPENDENCY-NAME)
exactly as written -- resolving a dependency to its project needs the whole
index, so that is a second pass."
  (let ((projects (make-hash-table :test #'equal))
        (system->project (make-hash-table :test #'equal))
        (edges '()))
    (%map-data-lines
     (%join dir "systems.txt")
     (lambda (words)
       ;; project system-file system-name [dependency...]
       (destructuring-bind (project-name system-file system-name &rest deps) words
         (declare (ignore system-file))
         (let ((project (or (gethash project-name projects)
                            (setf (gethash project-name projects)
                                  (make-project :name project-name)))))
           (push system-name (proj-systems project))
           (setf (gethash system-name system->project) project-name)
           (dolist (dep deps)
             (push (cons system-name dep) edges))))))
    (values projects system->project edges)))

(defun %read-releases (dir projects)
  "Fill in each project's release prefix from releases.txt."
  (%map-data-lines
   (%join dir "releases.txt")
   (lambda (words)
     ;; project url size md5 sha1 prefix [system-file...]
     (when (>= (length words) 6)
       (let ((project (gethash (first words) projects)))
         (when project
           (setf (proj-release project) (sixth words))))))))

;;; --- who owns a project ------------------------------------------------------
;;;
;;; A dependent written by the same people as the library says little about how
;;; much the ecosystem leans on it: an author who splits one program into twenty
;;; projects would otherwise put every one of them at the top. The owner is read
;;; from where Quicklisp fetches the project, which is in the quicklisp-projects
;;; repository, not in the dist: projects/<name>/source.txt, whose first data
;;; line is "<kind> <location>". On a shared host (github.com and the like) the
;;; owner is the first path segment -- the user or organisation; elsewhere it is
;;; the host. Two kinds name no URL and stand for a fixed host.

(defparameter *shared-hosts*
  '("github.com" "gitlab.com" "codeberg.org" "bitbucket.org" "git.sr.ht"
    "hg.sr.ht" "sr.ht" "gitlab.common-lisp.net" "git.sdf.org" "notabug.org"
    "gitee.com" "gist.github.com" "salsa.debian.org")
  "Hosts on which many unrelated people keep repositories, so that the owner
is the first path segment rather than the host. Any host whose name contains
\"gitlab.\" is treated the same way.")

(defparameter *kind-owners*
  '(("kmr-git" . "kpe.io")              ; http://git.kpe.io/<name>.git
    ("ediware-http" . "edicl"))         ; Edi Weitz's libraries, now github.com/edicl
  "Source kinds that carry no URL, and the owner each stands for.")

(defun %projects-directory ()
  "The quicklisp-projects checkout: DOTCL_QL_PROJECTS, which must be set."
  (let ((dir (%getenv "DOTCL_QL_PROJECTS")))
    (unless (and dir (plusp (length dir)))
      (error "DOTCL_QL_PROJECTS is not set. It names a checkout of~%~
https://github.com/quicklisp/quicklisp-projects, which says who owns each~%~
project; fetch-ql-projects.sh makes one at the commit matching the dist."))
    (string-right-trim "/" dir)))

(defun %projects-commit (dir)
  "The commit the checkout is at, when .git/HEAD names one directly (a
detached checkout, as fetch-ql-projects.sh makes), else NIL."
  (let ((head (probe-file (%join dir ".git/HEAD"))))
    (when head
      (with-open-file (in head :direction :input)
        (let ((line (string-trim '(#\Space #\Tab #\Return) (or (read-line in nil) ""))))
          (when (and (= (length line) 40)
                     (every (lambda (c) (digit-char-p c 16)) line))
            line))))))

(defun %split-on (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun %url-owner (url)
  (let* ((u (let ((p (search "://" url))) (if p (subseq url (+ p 3)) url)))
         (u (if (and (> (length u) 4) (string-equal "www." u :end2 4)) (subseq u 4) u))
         ;; user@host:path (scp style) and user@host/path
         (u (let ((at (position #\@ u)))
              (if (and at (< at (or (position #\/ u) (length u)))) (subseq u (1+ at)) u)))
         (u (substitute #\/ #\: u))
         (segments (remove "" (%split-on (string-downcase u) #\/) :test #'string=))
         (host (first segments))
         (path (rest segments)))
    (cond ((null host) "?")
          ((or (member host *shared-hosts* :test #'string=) (search "gitlab." host))
           (string-left-trim "~" (or (first path) host)))
          ((string= host "common-lisp.net")
           (format nil "common-lisp.net:~A"
                   (if (equal (first path) "project") (second path) (first path))))
          ((string= host "beta.quicklisp.org")
           (format nil "quicklisp-orphans:~A"
                   (if (equal (first path) "orphans") (second path) (first path))))
          ((let ((p (search ".github.io" host)))
             (and p (= p (- (length host) 10))))
           (subseq host 0 (search ".github.io" host)))
          (t host))))

(defun %project-owner (projects-dir name)
  "The owner of project NAME. A project without a readable source.txt gets an
owner of its own, so that it shares an owner with nothing."
  (let ((path (%join projects-dir "projects" name "source.txt"))
        (fallback (format nil "?~A" name)))
    (if (not (probe-file path))
        fallback
        (with-open-file (in path :direction :input :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while line
                do (let ((words (%split-words (string-trim '(#\Return) line))))
                     (when (and words (char/= (char (first words) 0) #\#))
                       (let* ((kind (string-downcase (first words)))
                              (fixed (cdr (assoc kind *kind-owners* :test #'string=))))
                         (return (cond (fixed fixed)
                                       ((second words) (%url-owner (second words)))
                                       (t fallback))))))
                finally (return fallback))))))

;;; --- counting --------------------------------------------------------------
;;;
;;; The unit is the project (one release), never the system: counting systems
;;; would rank a project by how finely its dependents are split into
;;; sub-systems, and would let a project vote for itself -- alexandria and
;;; alexandria-tests are one release and one row here, exactly as they are one
;;; thing to install.
;;;
;;; What is counted is every project that is built on this one, directly or
;;; through others, leaving out projects by the same owner. Direct referrers
;;; alone undercount a library that most of Quicklisp reaches through one
;;; popular intermediary, and overcount one that a single author's family of
;;; projects all name.

(defun %test-system-p (name)
  "A system that exists to test or benchmark another: its name contains
\"test\" or \"bench\". A library named only by those is a library its test
suite needs, not one the ecosystem builds on, so its dependencies are not
followed."
  (let ((lower (string-downcase name)))
    (or (search "test" lower) (search "bench" lower))))

(defun %count-referrers (projects system->project edges)
  (let ((seen (make-hash-table :test #'equal)))   ; "from-project>to-project"
    (dolist (edge edges)
      (destructuring-bind (system . dep) edge
        (let* ((from (gethash system system->project))
               (to (gethash dep system->project)))
          ;; A dependency naming something the dist does not carry (an
          ;; implementation-provided module, say) has no project to credit.
          (when (and from to (not (string= from to)))
            (let ((target (gethash to projects)))
              (let ((key (concatenate 'string from ">" to)))
                (unless (gethash key seen)
                  (setf (gethash key seen) t)
                  (push from (proj-referrers target))))
              (unless (%test-system-p system)
                (pushnew from (proj-dependents target) :test #'string=))
              ;; Per-system counts only decide which system names the project
              ;; below, so they are not deduplicated by project.
              (incf (gethash dep (proj-system-refs target) 0)))))))))

(defun %score (project projects)
  "How many projects reach PROJECT through non-test dependencies, at any
depth, and are owned by someone other than its owner."
  (let ((seen (make-hash-table :test #'equal))
        (stack (list (proj-name project)))
        (count 0))
    (loop while stack
          do (dolist (name (proj-dependents (gethash (pop stack) projects)))
               (unless (or (gethash name seen) (string= name (proj-name project)))
                 (setf (gethash name seen) t)
                 (push name stack)
                 (unless (string= (proj-owner (gethash name projects))
                                  (proj-owner project))
                   (incf count)))))
    count))

(defun %representative-system (project)
  "The system name that stands for PROJECT in the table.

The system named after the project when there is one -- that is what a reader
would type -- and otherwise the one other projects actually depend on (str for
cl-str, and so on)."
  (let ((systems (proj-systems project)))
    (or (find (proj-name project) systems :test #'string=)
        (first (sort (copy-list systems)
                     (lambda (a b)
                       (let ((ra (gethash a (proj-system-refs project) 0))
                             (rb (gethash b (proj-system-refs project) 0)))
                         (if (= ra rb)
                             (string< a b)
                             (> ra rb))))))
        (proj-name project))))

(defun %ranking (projects)
  "PROJECTS with a positive score, highest first, ties by name."
  (let ((rows '()))
    (maphash (lambda (name project)
               (declare (ignore name))
               (when (plusp (proj-score project))
                 (push (list (proj-score project) project) rows)))
             projects)
    (sort rows (lambda (a b)
                 (if (= (first a) (first b))
                     (string< (proj-name (second a)) (proj-name (second b)))
                     (> (first a) (first b)))))))

;;; --- output ----------------------------------------------------------------

(defparameter *hand-picked-marker* "# --- hand-picked (not ranked) ---"
  "The line that separates the ranked systems from the hand-picked ones in
targets.txt. RENDER.LISP looks for exactly this string, because the two halves
are ordered by different things and a reader has to be told where one stops.")

(defun %read-hand-picked (path)
  "The system names in PATH (one per line, # comments), or NIL if it is absent.
Hand-picked targets are chosen for a reason the ranking cannot express, so they
are kept in their own hand-edited file rather than derived."
  (when (probe-file path)
    (let ((names '()))
      (with-open-file (in path :direction :input :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                   (when (and (plusp (length trimmed))
                              (char/= (char trimmed 0) #\#))
                     (push trimmed names)))))
      (nreverse names))))

(defun %write-targets (path rows dist-name dist-version hand-picked threshold)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "# Systems to check, most depended upon first.~%")
    (format out "# Generated by bench/library-status/rank.lisp -- do not edit.~%")
    (format out "# dist: ~A ~A~%" dist-name dist-version)
    (format out "# min-dependents: ~D~%" threshold)
    (format out "# One system per line. The order is the rank; see targets.tsv~%")
    (format out "# for the counts it came from.~%")
    (dolist (row rows)
      (format out "~A~%" (%representative-system (second row))))
    (when hand-picked
      (format out "~A~%" *hand-picked-marker*)
      (format out "# Chosen by hand, not by dependent count; see hand-picked.txt.~%")
      (dolist (name hand-picked)
        (format out "~A~%" name)))))

(defun %write-targets-tsv (path rows dist-name dist-version projects-commit threshold)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "# rank~Csystem~Crelease~Cdependents~Creferrers~%" #\Tab #\Tab #\Tab #\Tab)
    (format out "# dist: ~A ~A~%" dist-name dist-version)
    (format out "# quicklisp-projects: ~A~%" (or projects-commit "not recorded"))
    (format out "# dependents: projects built on it, at any depth, through non-test~%")
    (format out "# systems, owned by someone else; rows with at least ~D.~%" threshold)
    (format out "# referrers: projects naming it directly (any system), for comparison.~%")
    (loop for row in rows
          for rank from 1
          do (destructuring-bind (count project) row
               (format out "~D~C~A~C~A~C~D~C~D~%"
                       rank #\Tab
                       (%representative-system project) #\Tab
                       (or (proj-release project) "-") #\Tab
                       count #\Tab
                       (length (proj-referrers project)))))))

(defun main (&key (output-directory nil))
  (let* ((dir (%dist-directory))
         (projects-dir (%projects-directory))
         (out-dir (or output-directory
                      ;; Next to this file: the generated lists are checked in
                      ;; beside the script that writes them.
                      (directory-namestring (or *load-truename* *default-pathname-defaults*)))))
    (multiple-value-bind (dist-name dist-version) (%read-distinfo dir)
      (multiple-value-bind (projects system->project edges) (%read-systems dir)
        (%read-releases dir projects)
        (%count-referrers projects system->project edges)
        (maphash (lambda (name project)
                   (setf (proj-owner project) (%project-owner projects-dir name)))
                 projects)
        (maphash (lambda (name project)
                   (declare (ignore name))
                   (setf (proj-score project) (%score project projects)))
                 projects)
        (let* ((ranking (%ranking projects))
               (env-threshold (let ((s (%getenv "THRESHOLD")))
                                (when (and s (plusp (length s)))
                                  (parse-integer s :junk-allowed t))))
               (threshold (or env-threshold *threshold*))
               (rows (remove-if (lambda (row) (< (first row) threshold)) ranking))
               (projects-commit (%projects-commit projects-dir)))
          ;; Always print how far each cut-off would reach. Choosing a threshold
          ;; is a decision about how long the run takes, and the only input to it
          ;; is this table -- printing it every time means the decision can be
          ;; revisited from any run's log rather than by editing the script.
          (format t "~&projects by dependent count:~%")
          (dolist (n '(1 2 3 4 5 7 10 15 20 30 50 100))
            (format t "  >= ~3D dependents: ~4D projects~%"
                    n (count-if (lambda (row) (>= (first row) n)) ranking)))
          ;; A project with no source.txt is counted as its own owner. Say how
          ;; many there were, because a checkout that does not match the dist
          ;; shows up as a jump in this number.
          (let ((unknown 0))
            (maphash (lambda (name project)
                       (declare (ignore name))
                       (when (char= (char (proj-owner project) 0) #\?)
                         (incf unknown)))
                     projects)
            (format t "owners: ~D projects have no source.txt in ~A~%"
                    unknown projects-dir))
          ;; A hand-picked name the ranked part already covers is dropped rather
          ;; than repeated: measuring it twice would put two rows in the table
          ;; for one library, and the ranked row is the one that carries its
          ;; rank.
          (let* ((ranked-names (mapcar (lambda (row) (%representative-system (second row)))
                                       rows))
                 (all-hand (%read-hand-picked (merge-pathnames "hand-picked.txt" out-dir)))
                 (hand-picked (remove-if (lambda (n) (member n ranked-names :test #'string=))
                                         all-hand)))
            (when all-hand
              (format t "hand-picked: ~D listed, ~D added (~D already ranked)~%"
                      (length all-hand) (length hand-picked)
                      (- (length all-hand) (length hand-picked))))
          (%write-targets (merge-pathnames "targets.txt" out-dir)
                          rows dist-name dist-version hand-picked threshold)
          (%write-targets-tsv (merge-pathnames "targets.tsv" out-dir)
                              rows dist-name dist-version projects-commit threshold)
          (format t "~&dist ~A ~A: ~D projects, ~D systems, ~D dependency edges~%"
                  dist-name dist-version
                  (hash-table-count projects)
                  (hash-table-count system->project)
                  (length edges))
          (format t "quicklisp-projects: ~A~%" (or projects-commit "commit not recorded"))
          (format t "wrote ~D targets (>= ~D dependents) to ~A~%"
                  (length rows) threshold out-dir)
          (format t "top 10:~%")
          (loop for row in (subseq rows 0 (min 10 (length rows)))
                for rank from 1
                do (format t "  ~2D. ~A (~D)~%"
                           rank (%representative-system (second row)) (first row)))
          (length rows)))))))

(main)
