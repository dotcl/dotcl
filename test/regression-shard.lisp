;;; Running a part of the regression suite: DOTCL_REGRESSION_SHARD=i/n runs the files of
;;; shard i of n and nothing else, so n jobs can share the suite.
;;;
;;; run.lisp loads that file when the variable is set; the shard run reads run.lisp as
;;; text, turns the LOAD forms of the files that belong to other shards into a call that
;;; only counts the skip, and loads the result. Everything else in run.lisp (the
;;; framework, the manifest and tier checks, the summaries) runs in every shard, so
;;; every shard prints its own PASSED / FAILED line and exits with its own status.
;;;
;;; Which shard a file belongs to is a pure function of test/regression/shard-times.tsv
;;; (file, seconds, group): files of one group stay together (a file that uses a helper
;;; another file defines), groups are dealt out largest first to the shard with the least
;;; work so far, and a file that is not in the table goes by the hash of its name.
;;; Every shard computes the same answer. test/regression/shard-times.py makes the table.

(defvar *regression-shard-pass* nil
  "True while the rewritten run.lisp of a shard run is being loaded.")
(defvar *shard-skipped-files* 0)
(defvar *shard-kept-files* 0)

(defun regression-shard ()
  "(values I N) from DOTCL_REGRESSION_SHARD=I/N, or NIL when it is not set."
  (let ((v (dotcl:getenv "DOTCL_REGRESSION_SHARD")))
    (when (and v (plusp (length v)))
      (let* ((slash (position #\/ v))
             (i (and slash (parse-integer v :end slash :junk-allowed t)))
             (n (and slash (parse-integer v :start (1+ slash) :junk-allowed t))))
        (unless (and i n (<= 1 i n))
          (error "DOTCL_REGRESSION_SHARD is ~s; expected I/N with 1 <= I <= N" v))
        (values i n)))))

(defun %shard-split-tabs (line)
  (let ((parts nil) (start 0))
    (loop for p = (position #\Tab line :start start)
          do (push (subseq line start p) parts)
             (if p (setq start (1+ p)) (return)))
    (nreverse parts)))

(defun %shard-name-hash (name)
  (let ((h 0))
    (loop for c across name do (setq h (mod (+ (* h 31) (char-code c)) 1000003)))
    h))

(defun %shard-table-rows (path)
  "List of (NAME SECONDS GROUP) from the table at PATH."
  (let ((rows nil))
    (with-open-file (s path)
      (loop for line = (read-line s nil nil) while line
            do (let ((line (string-right-trim '(#\Return) line)))
                 (when (and (plusp (length line)) (char/= (char line 0) #\#))
                   (let ((p (%shard-split-tabs line)))
                     (when (>= (length p) 2)
                       (push (list (first p)
                                   (let ((x (read-from-string (second p)))) (if (realp x) x 1))
                                   (or (third p) (first p)))
                             rows)))))))
    (nreverse rows)))

(defun shard-assignment (n &optional (path "test/regression/shard-times.tsv"))
  "Hash table: file name (no directory, no .lisp) -> shard number 1..N for the files of the table."
  (let ((groups (make-hash-table :test 'equal)) (order nil) (result (make-hash-table :test 'equal)))
    (dolist (r (%shard-table-rows path))
      (destructuring-bind (name sec group) r
        (let ((g (gethash group groups)))
          (unless g (setq g (list 0 nil)) (setf (gethash group groups) g) (push group order))
          (incf (first g) sec)
          (push name (second g)))))
    (let ((sorted (sort (copy-list order)
                        (lambda (a b)
                          (let ((wa (first (gethash a groups))) (wb (first (gethash b groups))))
                            (if (= wa wb) (string< a b) (> wa wb))))))
          (load (make-array n :initial-element 0)))
      (dolist (group sorted)
        (let ((best 0))
          (dotimes (k n) (when (< (aref load k) (aref load best)) (setq best k)))
          (incf (aref load best) (first (gethash group groups)))
          (dolist (name (second (gethash group groups)))
            (setf (gethash name result) (1+ best))))))
    result))

(defun shard-of (name n assignment)
  (or (gethash name assignment) (1+ (mod (%shard-name-hash name) n))))

(defun shard-skipped (path &rest keys)
  (declare (ignore path keys))
  (incf *shard-skipped-files*)
  t)

(defun %shard-rewrite (text i n assignment)
  "TEXT of run.lisp with the load forms of other shards' files replaced by SHARD-SKIPPED."
  (let ((ops '("(load-skip-under-interp \"test/regression/" "(load-full-tier-only \"test/regression/"
               "(load \"test/regression/"))
        (out (make-string-output-stream))
        (pos 0) (len (length text)))
    (loop
      (let ((best nil) (best-op nil))
        (dolist (op ops)
          (let ((p (search op text :start2 pos)))
            (when (and p (or (null best) (< p best))) (setq best p best-op op))))
        (unless best (write-string text out :start pos) (return))
        (let* ((name-start (+ best (length best-op)))
               (name-end (search ".lisp\"" text :start2 name-start))
               (name (and name-end (subseq text name-start name-end))))
          (write-string text out :start pos :end best)
          (cond ((and name (/= (shard-of name n assignment) i))
                 (write-string "(shard-skipped \"test/regression/" out)
                 (setq pos name-start))
                (t (incf *shard-kept-files*)
                   (write-string text out :start best :end name-start)
                   (setq pos name-start))))))
    (get-output-stream-string out)))

(defun %shard-prelude (text)
  "TEXT with ASDF required before the first test file. In the whole suite some file near the
start requires it and many later files use UIOP or ASDF symbols without a require of their own
(about twenty of them); a shard that does not hold that first file would fail to read them.
A few files before it read nothing of ASDF, so loading it early changes nothing for them."
  (let ((at (search "(check-regression-manifest" text)))
    (if at
        (concatenate 'string (subseq text 0 at) "(require \"asdf\")" (string #\Newline) (subseq text at))
        text)))

(defvar *regression-shard-file* nil
  "The rewritten run.lisp of this shard, once PREPARE-REGRESSION-SHARD has written it.")

(defun prepare-regression-shard (run-path)
  "Write the run.lisp of this shard (only its files) to a temporary file and remember its
name in *REGRESSION-SHARD-FILE*. run.lisp then LOADs it from its own top level: a LOAD done
from inside a function would put that function's frame under every backtrace the tests read."
  (multiple-value-bind (i n) (regression-shard)
    (let* ((assignment (shard-assignment n))
           (text (with-open-file (s run-path)
                   (let* ((buf (make-string (file-length s))) (k (read-sequence buf s)))
                     (subseq buf 0 k))))
           (new (%shard-prelude (%shard-rewrite text i n assignment)))
           (tmp (regression-temp-file (format nil "run-shard-~d-of-~d.lisp" i n))))
      (with-open-file (o tmp :direction :output :if-exists :supersede)
        (write-string new o))
      (format t "~&SHARD ~d/~d: ~d files run here (the other shards run the rest)~%" i n *shard-kept-files*)
      (setq *regression-shard-file* tmp))))
