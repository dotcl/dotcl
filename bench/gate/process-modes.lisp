;;; Show how per-process benchmark times are distributed, and say when they
;;; fall into two groups.
;;;
;;; Usage: dotcl process-modes.lisp [--no-compare] [label=]file [[label=]file ...]
;;;
;;; Each file is one side of a measurement: the stdout of one or more processes
;;; of the benchmark, concatenated, one "name<TAB>milliseconds" line per kernel
;;; per process (the format of bench/csharp-parity). The k-th line for a kernel
;;; is its value in the k-th process. With one file this describes the file;
;;; with two or more, every later side is also set against the first, which is
;;; the A/B case. --no-compare leaves the comparison lines out, for sides that
;;; are not two builds of the same thing (dotcl and C# in make bench-parity).
;;;
;;; The split rule is in bench/gate/modes.lisp. When a side is split, its median only
;;; says how many processes drew each group, so the comparison line reports the
;;; share of fast processes and the fast-group medians instead of a median
;;; difference, and does not call the change a difference or a non-difference.
;;; Nothing here decides "better" or "worse": it prints numbers, and says which
;;; of them can be compared.

(load (merge-pathnames "modes.lisp" (or *load-truename* *default-pathname-defaults*)))

(defun die (control &rest args)
  (format *error-output* "~?~%" control args)
  (dotcl:quit 1))

(defun pct (a b) (if (> a 0) (* (/ (- b a) a) 100) 0))

(defun file-label (spec)
  "(values LABEL FILE) for a [label=]file argument; without a label, the file name minus .txt."
  (let ((eq (position #\= spec)))
    (if eq
        (values (subseq spec 0 eq) (subseq spec (1+ eq)))
        (let* ((name (subseq spec (1+ (or (position #\/ spec :from-end t) -1))))
               (dot (search ".txt" name :from-end t)))
          (values (if (and dot (= dot (- (length name) 4))) (subseq name 0 dot) name) spec)))))

(defun file-nonempty-p (path)
  (and (probe-file path)
       (with-open-file (in path :element-type '(unsigned-byte 8))
         (plusp (file-length in)))))

(let ((compare t) (arguments (dotcl:script-arguments)))
  (when (equal (first arguments) "--no-compare")
    (setf compare nil arguments (rest arguments)))
  (when (null arguments)
    (format *error-output* "Usage: process-modes.lisp [--no-compare] [label=]file [[label=]file ...]~%")
    (dotcl:quit 2))
  (let ((labels '()) (files '()))
    (dolist (spec arguments)
      (multiple-value-bind (label file) (file-label spec)
        (push label labels)
        (push file files)))
    (setf labels (nreverse labels) files (nreverse files))
    (dolist (file files)
      (unless (file-nonempty-p file)
        (die "process-modes.lisp: ~A is missing or empty" file)))
    (let* ((sides (mapcar #'read-name-values files))
           (order '())
           (ns (length sides)))
      ;; The kernels in the order they first appear, over all sides in turn.
      (dolist (side sides)
        (dolist (entry side)
          (pushnew (car entry) order :test #'string=)))
      (setf order (nreverse order))
      (dolist (k order)
        (format t "~A~%" k)
        (let ((has (make-array (1+ ns) :initial-element nil))
              (info (make-array (1+ ns) :initial-element nil)))
          ;; INFO[s]: (split n min med fast-n fast-med) for the comparison lines.
          (loop for s from 1 to ns
                for side = (nth (1- s) sides)
                for label = (nth (1- s) labels)
                for values = (cdr (assoc k side :test #'string=))
                do (if (null values)
                       (format t "  ~A no result~%" (pad-right label 12))
                       (let ((m (split-modes values)))
                         (setf (aref has s) t
                               (aref info s) (list (modes-split m) (modes-n m) (modes-min m) (modes-med m)
                                                   (if (modes-split m) (modes-fast-n m) (modes-n m))
                                                   (if (modes-split m) (modes-fast-med m) (modes-med m))))
                         (format t "  ~A n=~A min ~A  median ~A  " (pad-right label 12)
                                 (pad-right (format nil "~D" (modes-n m)) 3)
                                 (pad-left (fixed (modes-min m) 3) 9) (pad-left (fixed (modes-med m) 3) 9))
                         (cond ((not (modes-checked m))
                                (format t "not checked (fewer than 3 processes)~%"))
                               ((modes-split m)
                                (format t "SPLIT: ~D fast at ~A, ~D slow at ~A (slow/fast ~A)~%"
                                        (modes-fast-n m) (fixed (modes-fast-med m) 3)
                                        (modes-slow-n m) (fixed (modes-slow-med m) 3)
                                        (fixed (/ (modes-slow-med m) (modes-fast-med m)) 2)))
                               (t (format t "one group~%"))))))
          (when compare
            (loop for s from 2 to ns
                  do (when (and (aref has 1) (aref has s))
                       (destructuring-bind (sp1 n1 mn1 md1 fn1 fm1) (aref info 1)
                         (destructuring-bind (sps ns2 mns mds fns fms) (aref info s)
                           (if (or sp1 sps)
                               (format t "  ~A -> ~A: split (~A); compare shares and groups, not medians: fast ~D/~D -> ~D/~D, fast group ~A -> ~A (~A%)~%"
                                       (first labels) (nth (1- s) labels)
                                       (if (and sp1 sps) "both" (if sp1 (first labels) (nth (1- s) labels)))
                                       fn1 n1 fns ns2 (fixed fm1 3) (fixed fms 3) (signed-fixed (pct fm1 fms) 1))
                               (format t "  ~A -> ~A: median ~A -> ~A (~A%), min ~A -> ~A (~A%)~%"
                                       (first labels) (nth (1- s) labels)
                                       (fixed md1 3) (fixed mds 3) (signed-fixed (pct md1 mds) 1)
                                       (fixed mn1 3) (fixed mns 3) (signed-fixed (pct mn1 mns) 1))))))))))))
)
