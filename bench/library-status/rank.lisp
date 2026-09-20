;;;; rank.lisp -- rank Quicklisp projects by how many other projects depend on them.
;;;;
;;;; Stage 1 of the library-status pipeline (see README.md). Reads the dist
;;;; metadata that Quicklisp already keeps on disk and writes the list of
;;;; systems worth checking, most depended upon first:
;;;;
;;;;   targets.txt   one system name per line, in rank order
;;;;   targets.tsv   rank, system, release, referrer count (the same rows, with
;;;;                 the numbers the order came from)
;;;;
;;;; Run it with any conforming Lisp; it uses nothing outside the standard plus
;;;; one environment lookup:
;;;;
;;;;   sbcl --non-interactive --load bench/library-status/rank.lisp
;;;;
;;;; The dist directory is found automatically (see %DIST-DIRECTORY); set
;;;; DOTCL_QL_DIST to point at another one. Nothing is downloaded: a machine
;;;; with no Quicklisp installed is told where to get the three files.

(defpackage :library-status/rank
  (:use :cl)
  (:export #:main))

(in-package :library-status/rank)

(defparameter *target-count* 150
  "How many projects to write out when no THRESHOLD is given. A budget, not a
coverage goal -- and an arbitrary one, which is why THRESHOLD exists.")

(defparameter *threshold* nil
  "When set (THRESHOLD=N in the environment), write out every project that at
least N other projects depend on, and ignore *TARGET-COUNT*.

A cut-off in the units the ranking is actually made of. \"The top 150\" says
nothing about what is in or out: it is a number of rows, and where it lands
depends on how many projects the dist happens to carry. \"Everything three or
more projects depend on\" is a statement about the ecosystem that stays the same
sentence as the dist grows -- and it is the honest way to say how far down the
tail the table reaches.")

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

;;; --- counting --------------------------------------------------------------
;;;
;;; What is counted is how many OTHER PROJECTS depend on a project, not how many
;;; systems do. Counting systems would rank a project by how finely its
;;; dependents are split into sub-systems (a library with ten test systems would
;;; vote ten times), and it would let a project vote for itself -- alexandria
;;; and alexandria-tests are one release and one row here, exactly as they are
;;; one thing to install.

(defun %count-referrers (projects system->project edges)
  (let ((seen (make-hash-table :test #'equal)))   ; "from-project>to-project"
    (dolist (edge edges)
      (destructuring-bind (system . dep) edge
        (let* ((from (gethash system system->project))
               (to (gethash dep system->project)))
          ;; A dependency naming something the dist does not carry (an
          ;; implementation-provided module, say) has no project to credit.
          (when (and from to (not (string= from to)))
            (let ((key (concatenate 'string from ">" to)))
              (unless (gethash key seen)
                (setf (gethash key seen) t)
                (push from (proj-referrers (gethash to projects)))))
            ;; Per-system counts only decide which system names the project
            ;; below, so they are not deduplicated by project.
            (let ((target (gethash to projects)))
              (incf (gethash dep (proj-system-refs target) 0)))))))))

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
  "PROJECTS sorted by referrer count, most depended upon first."
  (let ((rows '()))
    (maphash (lambda (name project)
               (declare (ignore name))
               (let ((count (length (proj-referrers project))))
                 (when (plusp count)
                   (push (list count project) rows))))
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

(defun %write-targets (path rows dist-name dist-version hand-picked)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "# Systems to check, most depended upon first.~%")
    (format out "# Generated by bench/library-status/rank.lisp -- do not edit.~%")
    (format out "# dist: ~A ~A~%" dist-name dist-version)
    (format out "# One system per line. The order is the rank; see targets.tsv~%")
    (format out "# for the counts it came from.~%")
    (dolist (row rows)
      (format out "~A~%" (%representative-system (second row))))
    (when hand-picked
      (format out "~A~%" *hand-picked-marker*)
      (format out "# Chosen by hand, not by referrer count; see hand-picked.txt.~%")
      (dolist (name hand-picked)
        (format out "~A~%" name)))))

(defun %write-targets-tsv (path rows dist-name dist-version)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "# rank~Csystem~Crelease~Creferrers~%" #\Tab #\Tab #\Tab)
    (format out "# dist: ~A ~A~%" dist-name dist-version)
    (loop for row in rows
          for rank from 1
          do (destructuring-bind (count project) row
               (format out "~D~C~A~C~A~C~D~%"
                       rank #\Tab
                       (%representative-system project) #\Tab
                       (or (proj-release project) "-") #\Tab
                       count)))))

(defun main (&key (output-directory nil))
  (let* ((dir (%dist-directory))
         (out-dir (or output-directory
                      ;; Next to this file: the generated lists are checked in
                      ;; beside the script that writes them.
                      (directory-namestring (or *load-truename* *default-pathname-defaults*)))))
    (multiple-value-bind (dist-name dist-version) (%read-distinfo dir)
      (multiple-value-bind (projects system->project edges) (%read-systems dir)
        (%read-releases dir projects)
        (%count-referrers projects system->project edges)
        (let* ((ranking (%ranking projects))
               (env-threshold (let ((s (%getenv "THRESHOLD")))
                                (when (and s (plusp (length s)))
                                  (parse-integer s :junk-allowed t))))
               (threshold (or env-threshold *threshold*))
               (rows (if threshold
                         (remove-if (lambda (row) (< (first row) threshold)) ranking)
                         (subseq ranking 0 (min *target-count* (length ranking))))))
          ;; Always print how far each cut-off would reach. Choosing a threshold
          ;; is a decision about how long the run takes, and the only input to it
          ;; is this table -- printing it every time means the decision can be
          ;; revisited from any run's log rather than by editing the script.
          (format t "~&projects by referrer count:~%")
          (dolist (n '(1 2 3 4 5 7 9 15 25 50))
            (format t "  >= ~2D referrers: ~4D projects~%"
                    n (count-if (lambda (row) (>= (first row) n)) ranking)))
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
                          rows dist-name dist-version hand-picked)
          (%write-targets-tsv (merge-pathnames "targets.tsv" out-dir)
                              rows dist-name dist-version)
          (format t "~&dist ~A ~A: ~D projects, ~D systems, ~D dependency edges~%"
                  dist-name dist-version
                  (hash-table-count projects)
                  (hash-table-count system->project)
                  (length edges))
          (format t "wrote ~D targets to ~A~%" (length rows) out-dir)
          (format t "top 10:~%")
          (loop for row in (subseq rows 0 (min 10 (length rows)))
                for rank from 1
                do (format t "  ~2D. ~A (~D)~%"
                           rank (%representative-system (second row)) (first row)))
          (length rows)))))))

(main)
