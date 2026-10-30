;;; Measure cl-bench deriv/dderiv alone, in the gabriel suite, and with a full GC before each benchmark.

(defpackage :cl-bench.gabriel
  (:use :common-lisp)
  (:export #:boyer #:browse #:dderiv-run #:deriv-run
           #:run-destructive #:run-div2-test1 #:run-div2-test2
           #:run-fft #:run-frpoly/fixnum #:run-frpoly/bignum #:run-frpoly/float
           #:run-puzzle #:run-tak #:run-ctak #:run-trtak #:run-takl
           #:run-stak #:fprint/pretty #:fprint/ugly
           #:run-traverse #:run-triangle))

(load (merge-pathnames "../../cl-bench/files/gabriel.lisp" *load-truename*))

(defparameter *gabriel*
  (list (list "tak" 100 #'cl-bench.gabriel:run-tak)
        (list "takl" 10 #'cl-bench.gabriel:run-takl)
        (list "stak" 50 #'cl-bench.gabriel:run-stak)
        (list "ctak" 50 #'cl-bench.gabriel:run-ctak)
        (list "trtak" 100 #'cl-bench.gabriel:run-trtak)
        (list "boyer" 10 #'cl-bench.gabriel:boyer)
        (list "browse" 5 #'cl-bench.gabriel:browse)
        (list "dderiv" 50 #'cl-bench.gabriel:dderiv-run)
        (list "deriv" 50 #'cl-bench.gabriel:deriv-run)
        (list "destructive" 50 #'cl-bench.gabriel:run-destructive)
        (list "div2-test-1" 50 #'cl-bench.gabriel:run-div2-test1)
        (list "div2-test-2" 50 #'cl-bench.gabriel:run-div2-test2)
        (list "fft" 10 #'cl-bench.gabriel:run-fft)
        (list "frpoly/fixnum" 30 #'cl-bench.gabriel:run-frpoly/fixnum)
        (list "frpoly/bignum" 10 #'cl-bench.gabriel:run-frpoly/bignum)
        (list "frpoly/float" 30 #'cl-bench.gabriel:run-frpoly/float)
        (list "puzzle" 5 #'cl-bench.gabriel:run-puzzle)
        (list "triangle" 1 #'cl-bench.gabriel:run-triangle)
        (list "traverse" 5 #'cl-bench.gabriel:run-traverse)
        (list "fprint/ugly" 50 #'cl-bench.gabriel:fprint/ugly)
        (list "fprint/pretty" 20 #'cl-bench.gabriel:fprint/pretty)))

(defun now-seconds ()
  (/ (float (get-internal-real-time)) internal-time-units-per-second))

(defun elapsed-seconds (start end)
  (/ (float (- end start)) internal-time-units-per-second))

(defun stats-list (stats)
  (list (nth 0 stats) (nth 1 stats) (nth 2 stats)
        (nth 3 stats) (nth 4 stats) (or (nth 5 stats) 0)))

(defun emit-result (name mode secs before after)
  (destructuring-bind (g0b g1b g2b heapb allocb pauseb) before
    (destructuring-bind (g0a g1a g2a heapa alloca pausea) after
      (format t "RESULT ~A ~A ~,6F ~D ~D ~D ~D ~D ~D ~D ~D ~D ~D ~D ~D~%"
              name mode secs
              g0b g1b g2b heapb allocb pauseb
              g0a g1a g2a heapa alloca pausea))))

(defun timed-full-gc ()
  (let ((start (get-internal-real-time)))
    (dotcl:gc)
    (elapsed-seconds start (get-internal-real-time))))

(defun run-benchmark (name times function full-gc record mode)
  (let ((gc-seconds (if full-gc (timed-full-gc) 0.0))
        (before nil)
        (after nil)
        (secs 0.0))
    (setf before (dotcl:gc-stats))
    (let ((start (get-internal-real-time)))
      (dotimes (i times) (funcall function))
      (setf secs (elapsed-seconds start (get-internal-real-time))))
    (setf after (dotcl:gc-stats))
    (when record
      (emit-result name mode secs (stats-list before) (stats-list after)))
    (values secs gc-seconds)))

(defun recorded-p (name)
  (member name '("dderiv" "deriv") :test #'string=))

(defun run-suite (full-gc)
  (let ((mode (if full-gc "suite-gc" "suite"))
        (start (get-internal-real-time))
        (gc-total 0.0))
    (dolist (entry *gabriel*)
      (destructuring-bind (name times function) entry
        (multiple-value-bind (seconds gc-seconds)
            (run-benchmark name times function full-gc (recorded-p name) mode)
          (declare (ignore seconds))
          (incf gc-total gc-seconds))))
    (format t "TOTAL ~A ~,6F ~,6F~%"
            mode (elapsed-seconds start (get-internal-real-time)) gc-total)))

(defun run-alone (bench)
  (let ((entry (find bench *gabriel* :key #'first :test #'string=)))
    (unless entry (error "unknown benchmark ~A" bench))
    (destructuring-bind (name times function) entry
      (let ((start (get-internal-real-time)))
        (multiple-value-bind (seconds gc-seconds)
            (run-benchmark name times function nil t "alone")
          (declare (ignore gc-seconds))
          (format t "TOTAL alone ~,6F ~,6F~%"
                  (elapsed-seconds start (get-internal-real-time))
                  seconds))))))

(defun words (string)
  (let ((result nil) (start nil))
    (dotimes (i (length string))
      (if (member (char string i) '(#\Space #\Tab #\Return #\Newline))
          (when start (push (subseq string start i) result) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq string start) result))
    (nreverse result)))

(defun parse-number (string)
  (multiple-value-bind (value pos) (read-from-string string)
    (declare (ignore pos))
    value))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<))
         (n (length sorted)))
    (if (oddp n)
        (nth (floor n 2) sorted)
        (/ (+ (nth (1- (floor n 2)) sorted) (nth (floor n 2) sorted)) 2.0))))

(defun starts-with-p (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun env-entry-p (name entry)
  (let ((n (length name)))
    (and (>= (length entry) (1+ n))
         (string= entry name :end1 n)
         (char= (char entry n) #\=))))

(defun self-command ()
  (let* ((path (dotnet:static "System.Environment" "ProcessPath"))
         (args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length"))
         (argv (loop for i below count collect (dotnet:invoke args "GetValue" i)))
         (name (string-downcase (file-namestring (pathname path))))
         (dotnet-host (or (string= name "dotnet") (string= name "dotnet.exe"))))
    (if dotnet-host (cons path argv) (cons path (cdr argv)))))

(defun child-environment (mode bench)
  (let ((environment
          (remove-if (lambda (entry)
                       (or (env-entry-p "DOTCL_ORDER_MODE" entry)
                           (env-entry-p "DOTCL_ORDER_BENCH" entry)
                           (env-entry-p "DOTNET_gcServer" entry)
                           (env-entry-p "COMPlus_gcServer" entry)))
                     (dotcl:posix-environ))))
    (cons (format nil "DOTCL_ORDER_MODE=~A" mode)
          (cons (format nil "DOTCL_ORDER_BENCH=~A" (or bench ""))
                (cons "DOTNET_gcServer=0" environment)))))

(defun run-child (mode bench)
  (let* ((command (self-command))
         (output (make-string-output-stream))
         (error-output (make-string-output-stream))
         (start (get-internal-real-time))
         (process (apply #'dotcl:launch-process (first command) (rest command)
                         (list :environment (child-environment mode bench)
                               :input nil :output output :error error-output)))
         (code (dotcl:process-wait process))
         (wall (elapsed-seconds start (get-internal-real-time)))
         (stdout (get-output-stream-string output))
         (stderr (get-output-stream-string error-output)))
    (unless (zerop code)
      (error "~A/~A child exited ~D: ~A" mode bench code stderr))
    (values stdout stderr wall)))

(defparameter *scenarios*
  '(("alone" "deriv") ("alone" "dderiv") ("suite" nil) ("suite-gc" nil)))

(defun parse-result (line)
  (let ((fields (words line)))
    (list (second fields) (third fields)
          (parse-number (fourth fields))
          (mapcar #'parse-number (nthcdr 4 fields)))))

(defun parse-total (line)
  (let ((fields (words line)))
    (list (second fields)
          (parse-number (third fields))
          (parse-number (fourth fields)))))

(defun run-all ()
  (let ((results (make-hash-table :test #'equal))
        (totals (make-hash-table :test #'equal)))
    (loop for round from 1 to 5 do
      (let* ((offset (mod round (length *scenarios*)))
             (order (append (nthcdr offset *scenarios*)
                            (subseq *scenarios* 0 offset))))
        (dolist (scenario order)
          (destructuring-bind (mode bench) scenario
            (format t ";; round ~D ~A ~A~%" round mode (or bench ""))
            (finish-output)
            (multiple-value-bind (stdout stderr wall) (run-child mode bench)
              (declare (ignore stderr))
              (let ((found nil))
                (with-input-from-string (stream stdout)
                  (loop for line = (read-line stream nil nil)
                        while line do
                          (cond ((starts-with-p "RESULT " line)
                                 (setf found t)
                                 (destructuring-bind (name result-mode seconds stats)
                                     (parse-result line)
                                   (push (list seconds stats)
                                         (gethash (cons result-mode name) results))))
                                ((starts-with-p "TOTAL " line)
                                 (destructuring-bind (result-mode suite gc)
                                     (parse-total line)
                                   (push (list suite gc wall)
                                         (gethash result-mode totals)))))))
                (unless found
                  (error "~A/~A child printed no result: ~S" mode bench stdout))))))))
    (values results totals)))

(defun stat-value (stats index)
  (nth index stats))

(defun report-benchmark (name mode results)
  (let* ((rows (reverse (gethash (cons mode name) results)))
         (seconds (mapcar #'first rows)))
    (format t "~20A~10A~10,3F~10,3F~10D~%"
            name mode (apply #'min seconds) (median seconds) (length rows))))

(defun report-gc-row (run name mode stats)
  (let ((before (subseq stats 0 6))
        (after (subseq stats 6 12)))
    (format t "~4D~10A~10A~7D~7D~7D~9,1F~7D~7D~7D~9,1F~9,1F~9,3F~%"
            run name mode
            (stat-value before 0) (stat-value before 1) (stat-value before 2)
            (/ (stat-value before 3) 1048576.0)
            (stat-value after 0) (stat-value after 1) (stat-value after 2)
            (/ (stat-value after 3) 1048576.0)
            (/ (- (stat-value after 4) (stat-value before 4)) 1048576.0)
            (/ (float (- (stat-value after 5) (stat-value before 5)))
               internal-time-units-per-second))))

(defun report-gc-state (name mode results)
  (let ((rows (reverse (gethash (cons mode name) results))))
    (loop for row in rows
          for run from 1 do
      (destructuring-bind (seconds stats) row
        (declare (ignore seconds))
        (report-gc-row run name mode stats)))))

(defun report (results totals)
  (format t "~&~%timing (seconds, 5 processes)~%")
  (format t "~20A~10A~10A~10A~10A~%" "benchmark" "mode" "min" "median" "runs")
  (dolist (name '("dderiv" "deriv"))
    (dolist (mode '("alone" "suite" "suite-gc"))
      (report-benchmark name mode results)))
  (format t "~&~%gc state around the timed region~%")
  (format t "~4A~10A~10A~7A~7A~7A~9A~7A~7A~7A~9A~9A~9A~%"
          "run" "benchmark" "mode"
          "g0" "g1" "g2" "heap MiB"
          "g0" "g1" "g2" "heap MiB" "alloc MiB" "pause s")
  (format t "~4A~10A~10A~7A~7A~7A~9A~7A~7A~7A~9A~9A~9A~%"
          "" "" "" "before" "" "" ""
          "after" "" "" "" "" "")
  (dolist (name '("dderiv" "deriv"))
    (dolist (mode '("alone" "suite" "suite-gc"))
      (report-gc-state name mode results)))
  (format t "~&~%suite wall time (seconds, 5 processes)~%")
  (format t "~12A~10A~10A~10A~10A~%" "mode" "min suite" "med suite" "min wall" "med wall")
  (dolist (mode '("suite" "suite-gc"))
    (let* ((rows (reverse (gethash mode totals)))
           (suite (mapcar #'first rows))
           (wall (mapcar #'third rows)))
      (format t "~12A~10,3F~10,3F~10,3F~10,3F~%"
              mode (apply #'min suite) (median suite)
              (apply #'min wall) (median wall))))
  (let* ((plain (reverse (gethash "suite" totals)))
         (full (reverse (gethash "suite-gc" totals)))
         (plain-median (median (mapcar #'first plain)))
         (full-median (median (mapcar #'first full)))
         (gc-median (median (mapcar #'second full))))
    (format t "~&full-GC overhead: suite +~,3F s median, ~,3F s spent in explicit GC~%"
            (- full-median plain-median) gc-median)))

(let ((mode (dotcl:getenv "DOTCL_ORDER_MODE")))
  (if mode
      (cond ((string-equal mode "alone")
             (run-alone (dotcl:getenv "DOTCL_ORDER_BENCH")))
            ((string-equal mode "suite-gc")
             (run-suite t))
            (t
             (run-suite nil)))
      (multiple-value-bind (results totals) (run-all)
        (format t "~&")
        (report results totals)))
  (finish-output)
  (dotcl:quit 0))
