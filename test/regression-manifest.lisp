;;; Guard for the hand-written manifest in test/regression/run.lisp.
;;;
;;; The manifest is a list of LOAD forms with no discovery behind it, so a test
;;; file that is added and not listed there never runs -- and nothing says so.
;;; The files cannot simply be loaded automatically instead: the order the
;;; manifest fixes is part of what the suite asserts, and some entries are
;;; guarded by a feature test. So this only reports. Every .lisp in
;;; test/regression/ has to be named by the manifest, either by a LOAD form or
;;; by an explicit opt-out line
;;;
;;;     ;; skip: some-file.lisp   -- why it is not loaded
;;;
;;; A file named by neither aborts the run, before any test has loaded, with a
;;; non-zero exit status.
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

(defun %regression-manifest-referenced (manifest-path)
  (let ((names '()))
    (with-open-file (in manifest-path :direction :input)
      (loop
        (let ((line (read-line in nil nil)))
          (when (null line) (return))
          (dolist (name (%regression-manifest-names-in-line line))
            (push name names)))))
    names))

(defun check-regression-manifest (manifest-path)
  (let ((referenced (%regression-manifest-referenced manifest-path))
        (self (%regression-manifest-basename manifest-path))
        (missing '()))
    (dolist (path (directory (concatenate 'string
                                          *regression-manifest-dir*
                                          "*.lisp")))
      (let ((name (file-namestring path)))
        (unless (or (string= name self)
                    (member name referenced :test #'string=))
          (push name missing))))
    (when missing
      (format *error-output*
              "~&*** regression manifest check FAILED: ~a file(s) in ~a are not referenced by ~a:~%"
              (length missing) *regression-manifest-dir* manifest-path)
      (dolist (name (sort missing #'string<))
        (format *error-output* "***   ~a~a~%" *regression-manifest-dir* name))
      (format *error-output*
              "*** Add a LOAD form for each (order matters, so they are not loaded for you),~%")
      (format *error-output*
              "*** or an opt-out line in the manifest: ;; skip: <name>.lisp -- <reason>~%")
      (finish-output *error-output*)
      (dotcl:quit 1))
    (length referenced)))
