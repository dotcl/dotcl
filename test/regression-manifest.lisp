;;; Guard and loader for the manifest in test/regression/run.lisp.
;;;
;;; run.lisp lists the test files that need a place in the order, or a different load form, as LOAD
;;; forms. A file that is not listed is no longer lost (one sat unrun for eight days before this
;;; guard existed): LOAD-UNLISTED-REGRESSION-FILES loads it after the listed ones, in name order, as
;;; its header marks say (below), so a new test file needs no edit of run.lisp. What this file still
;;; refuses is a header mark that means nothing, and it keeps the opt-out
;;;
;;;     ;; skip: some-file.lisp   -- why it is not loaded
;;;
;;; for a file the list names on purpose not to load. #| ... |# comments in run.lisp hide their LOAD
;;; forms, as they do from the reader.
;;;
;;; This uses nothing but the runtime, so it holds on the compiling build, on
;;; --eval (setq *evaluator-mode* :interpret), and on an emit-free build alike.

(defvar *regression-manifest-dir* "test/regression/")

(defun %regression-manifest-basename (name)
  (let ((slash (position #\/ name :from-end t)))
    (if slash (subseq name (1+ slash)) name)))

(defun %regression-manifest-lisp-file-p (name)
  (and (> (length name) 5)
       (string= ".lisp" name :start2 (- (length name) 5))))

;;; The file names one line refers to: the argument of a ";; skip:" opt-out, or
;;; every quoted "*.lisp" string in the code part of the line. A commented-out
;;; LOAD does not count -- the file it names does not run either.
(defun %regression-manifest-names-in-line (line)
  (let* ((comment (search ";;" line))
         (skip (search "skip:" line))
         (code (if comment (subseq line 0 comment) line))
         (names (list)))
    (if (and comment skip (< comment skip))
        (let* ((rest (string-trim '(#\Space #\Tab #\Return)
                                  (subseq line (+ skip 5))))
               (stop (position #\Space rest))
               (name (if stop (subseq rest 0 stop) rest)))
          (when (%regression-manifest-lisp-file-p name)
            (push (%regression-manifest-basename name) names)))
        (let ((start 0))
          (loop
            (let ((open (position #\" code :start start)))
              (when (null open) (return))
              (let ((close (position #\" code :start (1+ open))))
                (when (null close) (return))
                (let ((text (subseq code (1+ open) close)))
                  (when (%regression-manifest-lisp-file-p text)
                    (push (%regression-manifest-basename text) names)))
                (setq start (1+ close)))))))
    names))

;;; #| ... |# comments (nested, across lines): a LOAD form inside one does not run, so it does not
;;; count. Strings are not looked into.
(defun %regression-manifest-strip-block-comments (line depth)
  "(values LINE-WITHOUT-BLOCK-COMMENT-TEXT NEW-DEPTH)."
  (let ((out (make-string-output-stream)) (i 0) (n (length line)))
    (loop
      (when (>= i n) (return))
      (cond ((and (< (1+ i) n) (char= (char line i) #\#) (char= (char line (1+ i)) #\|))
             (incf depth) (incf i 2))
            ((and (> depth 0) (< (1+ i) n) (char= (char line i) #\|) (char= (char line (1+ i)) #\#))
             (decf depth) (incf i 2))
            ((> depth 0) (incf i))
            (t (write-char (char line i) out) (incf i))))
    (values (get-output-stream-string out) depth)))

(defun %regression-manifest-referenced (manifest-path)
  (let ((names '()) (depth 0))
    (with-open-file (in manifest-path :direction :input)
      (loop
        (let ((line (read-line in nil nil)))
          (when (null line) (return))
          (multiple-value-bind (text d) (%regression-manifest-strip-block-comments line depth)
            (setq depth d)
            (dolist (name (%regression-manifest-names-in-line text))
              (push name names))))))
    names))

;;; A test file that run.lisp does not name is not an error: it is loaded after the listed ones, in name
;;; order, by LOAD-UNLISTED-REGRESSION-FILES, so a new file needs no edit of run.lisp. What it is loaded
;;; as is read from its header, the first lines of the file:
;;;
;;;     ;;; regression: skip-under-interp      -- not under :INTERPRET (as LOAD-SKIP-UNDER-INTERP)
;;;     ;;; regression: full-tier-only         -- only in the full tier (as LOAD-FULL-TIER-ONLY)
;;;     ;;; regression: full-tier-only skip-under-interp
;;;     ;;; regression: skip -- why           -- not loaded (as a ";; skip:" line)
;;;
;;; No mark: a plain LOAD in every mode. A mark that is none of these aborts the run.
(defparameter *regression-header-lines* 40
  "How many lines at the top of a test file are searched for its \";;; regression:\" marks.")

(defun %regression-file-marks (path)
  "The list of mark words of PATH's header (strings), in order of appearance; a mark line of
   the form \";;; regression: skip -- reason\" gives (\"skip\"). Unknown words are returned too."
  (let ((marks '()) (prefix ";;; regression:"))
    (with-open-file (in path :direction :input)
      (dotimes (k *regression-header-lines*)
        (let ((line (read-line in nil nil)))
          (when (null line) (return))
          (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
            (when (and (>= (length line) (length prefix))
                       (string= prefix line :end2 (length prefix)))
              (let ((rest (string-trim '(#\Space #\Tab)
                                       (subseq line (length prefix))))
                    (words '()) (start 0))
                (loop
                  (let* ((p (position-if (lambda (c) (member c '(#\Space #\Tab))) rest :start start))
                         (word (subseq rest start p)))
                    (when (string= word "--") (return))
                    (when (plusp (length word)) (push word words))
                    (if p (setq start (1+ p)) (return))))
                (dolist (w (nreverse words)) (push w marks))))))))
    (nreverse marks)))

(defun %regression-known-marks-p (marks)
  (every (lambda (m) (member m '("skip-under-interp" "full-tier-only" "skip") :test #'string=)) marks))

(defun regression-unlisted-files (manifest-path)
  "Names (no directory) of the .lisp files of the test directory that MANIFEST-PATH does not name,
   in name order, run.lisp itself left out."
  (let ((referenced (%regression-manifest-referenced manifest-path))
        (self (%regression-manifest-basename manifest-path))
        (names '()))
    (dolist (path (directory (concatenate 'string *regression-manifest-dir* "*.lisp")))
      (let ((name (file-namestring path)))
        (unless (or (string= name self) (member name referenced :test #'string=))
          (push name names))))
    (sort names #'string<)))

(defun check-regression-manifest (manifest-path)
  "Abort the run (exit 1) when an unlisted file has a mark that is not known. Returns the number of
   files the manifest names."
  (let ((referenced (%regression-manifest-referenced manifest-path))
        (bad '()))
    (dolist (name (regression-unlisted-files manifest-path))
      (let ((marks (%regression-file-marks (concatenate 'string *regression-manifest-dir* name))))
        (unless (%regression-known-marks-p marks)
          (push (cons name marks) bad))))
    (when bad
      (format *error-output*
              "~&*** regression manifest check FAILED: unknown \";;; regression:\" mark in ~a file(s) of ~a:~%"
              (length bad) *regression-manifest-dir*)
      (dolist (b (reverse bad))
        (format *error-output* "***   ~a~a: ~{~a~^ ~}~%" *regression-manifest-dir* (car b) (cdr b)))
      (format *error-output*
              "*** Known marks: skip-under-interp, full-tier-only, skip (see test/regression-manifest.lisp).~%")
      (finish-output *error-output*)
      (dotcl:quit 1))
    (length referenced)))

(defvar *unlisted-files-loaded* 0)
(defvar *unlisted-files-skipped* 0)

(defun load-unlisted-regression-files (manifest-path)
  "LOAD the files MANIFEST-PATH does not name, in name order, each as its header marks say. Under
   DOTCL_REGRESSION_SHARD only the files of this shard (REGRESSION-SHARD, SHARD-OF: a file the shard
   table does not know goes by the hash of its name, so adding one moves no other file)."
  (multiple-value-bind (i n) (if (fboundp 'regression-shard) (regression-shard) nil)
    (let ((assignment (and n (shard-assignment n))))
      (dolist (name (regression-unlisted-files manifest-path))
        (let* ((path (concatenate 'string *regression-manifest-dir* name))
               (marks (%regression-file-marks path))
               (base (subseq name 0 (- (length name) 5))))
          (cond ((member "skip" marks :test #'string=)
                 (incf *unlisted-files-skipped*))
                ((and n (/= (shard-of base n assignment) i))
                 (shard-skipped path))
                (t
                 (incf *unlisted-files-loaded*)
                 (let ((interp (member "skip-under-interp" marks :test #'string=)))
                   (cond ((member "full-tier-only" marks :test #'string=)
                          (load-full-tier-only path :skip-under-interp (and interp t)))
                         (interp (load-skip-under-interp path))
                         (t (load path)))))))))
    t))

(defun unlisted-files-summary ()
  (print (list 'UNLISTED-REGRESSION-FILES 'LOADED *unlisted-files-loaded*
               'SKIPPED-BY-MARK *unlisted-files-skipped*))
  t)
