;;; The ledger's steady detection and its cold/steady/mark table.

(load (merge-pathnames "../../bench/steady.lisp"
                       (or *load-truename* *default-pathname-defaults*)))

(deftest ledger-steady-window
  (list (ledger-steady:stable-window-p '(1.00 1.04 1.03) 3 0.05)
        (ledger-steady:stable-window-p '(1.00 1.06 1.03) 3 0.05)
        (ledger-steady:stable-window-p '(1.00 1.00) 3 0.05))
  (t nil nil))

(deftest ledger-steady-minimum-runs
  (let ((runs 0))
    (unwind-protect
         (progn
           (dotcl:setenv "LEDGER_STEADY_MIN_RUNS" "5")
           (dotcl:setenv "LEDGER_STEADY_MIN_SECONDS" "0")
           (dotcl:setenv "LEDGER_STEADY_WINDOW" "3")
           (dotcl:setenv "LEDGER_STEADY_TOLERANCE" "100")
           (dotcl:setenv "LEDGER_STEADY_MAX_RUNS" "20")
           (dotcl:setenv "LEDGER_STEADY_MAX_SECONDS" "120")
           (multiple-value-bind (best count stable)
               (ledger-steady:measure (lambda () (incf runs) (sleep 0.003)))
             (declare (ignore best))
             (list count stable)))
      (dolist (name '("LEDGER_STEADY_MIN_RUNS" "LEDGER_STEADY_MIN_SECONDS"
                      "LEDGER_STEADY_WINDOW" "LEDGER_STEADY_TOLERANCE"
                      "LEDGER_STEADY_MAX_RUNS" "LEDGER_STEADY_MAX_SECONDS"))
        (dotcl:unsetenv name))))
  (5 t))

(deftest ledger-steady-window-after
  (list (ledger-steady:window-after-p '(0.03 0.02 0.01) 3 0.01)
        (ledger-steady:window-after-p '(0.03 0.02 0.01) 3 0.02)
        (ledger-steady:window-after-p '(0.03 0.02) 3 0))
  (t nil nil))

(deftest ledger-steady-default-minimum-seconds
  (let ((runs 0))
    (unwind-protect
         (progn
           (dotcl:setenv "LEDGER_STEADY_MIN_RUNS" "2")
           (dotcl:setenv "LEDGER_STEADY_WINDOW" "3")
           (dotcl:setenv "LEDGER_STEADY_TOLERANCE" "100")
           (dotcl:setenv "LEDGER_STEADY_MAX_RUNS" "100")
           (dotcl:setenv "LEDGER_STEADY_MAX_SECONDS" "0.2")
           (dotcl:unsetenv "LEDGER_STEADY_MIN_SECONDS")
           (multiple-value-bind (best count stable)
               (ledger-steady:measure (lambda () (incf runs) (sleep 0.001)))
             (declare (ignore best count))
             stable))
      (dolist (name '("LEDGER_STEADY_MIN_RUNS" "LEDGER_STEADY_MIN_SECONDS"
                      "LEDGER_STEADY_WINDOW" "LEDGER_STEADY_TOLERANCE"
                      "LEDGER_STEADY_MAX_RUNS" "LEDGER_STEADY_MAX_SECONDS"))
        (dotcl:unsetenv name))))
  nil)

(deftest ledger-steady-minimum-seconds
  (let ((runs 0)
        (ticks 0)
        ;; Ten milliseconds per run, without depending on SLEEP granularity.
        (step (max 1 (round internal-time-units-per-second 100))))
    (unwind-protect
         (progn
           (dotcl:setenv "LEDGER_STEADY_MIN_RUNS" "2")
           (dotcl:setenv "LEDGER_STEADY_MIN_SECONDS" "0.02")
           (dotcl:setenv "LEDGER_STEADY_WINDOW" "3")
           (dotcl:setenv "LEDGER_STEADY_TOLERANCE" "100")
           (dotcl:setenv "LEDGER_STEADY_MAX_RUNS" "20")
           (dotcl:setenv "LEDGER_STEADY_MAX_SECONDS" "120")
           (multiple-value-bind (best count stable)
               (ledger-steady:measure
                (lambda () (incf runs) (incf ticks step))
                :clock (lambda () ticks))
             (declare (ignore best))
             (list stable (>= count 5) (<= count 20))))
      (dolist (name '("LEDGER_STEADY_MIN_RUNS" "LEDGER_STEADY_MIN_SECONDS"
                      "LEDGER_STEADY_WINDOW" "LEDGER_STEADY_TOLERANCE"
                      "LEDGER_STEADY_MAX_RUNS" "LEDGER_STEADY_MAX_SECONDS"))
        (dotcl:unsetenv name))))
  (t t t))

(deftest ledger-steady-since-skips-minimum-seconds
  (let ((runs 0))
    (unwind-protect
         (progn
           (dotcl:setenv "LEDGER_STEADY_MIN_RUNS" "2")
           (dotcl:setenv "LEDGER_STEADY_MIN_SECONDS" "10")
           (dotcl:setenv "LEDGER_STEADY_WINDOW" "3")
           (dotcl:setenv "LEDGER_STEADY_TOLERANCE" "100")
           (dotcl:setenv "LEDGER_STEADY_MAX_RUNS" "20")
           (dotcl:setenv "LEDGER_STEADY_MAX_SECONDS" "120")
           (multiple-value-bind (best count stable)
               (ledger-steady:measure
                (lambda () (incf runs) (sleep 0.001))
                :since (- (get-internal-real-time)
                          (* 11 internal-time-units-per-second)))
             (declare (ignore best))
             (list count stable)))
      (dolist (name '("LEDGER_STEADY_MIN_RUNS" "LEDGER_STEADY_MIN_SECONDS"
                      "LEDGER_STEADY_WINDOW" "LEDGER_STEADY_TOLERANCE"
                      "LEDGER_STEADY_MAX_RUNS" "LEDGER_STEADY_MAX_SECONDS"))
        (dotcl:unsetenv name))))
  (3 t))

#-windows
(defvar *ledger-root*
  (namestring (merge-pathnames "../../"
                               (or *load-truename* *default-pathname-defaults*))))

#-windows
(defun ledger-test-write (path text)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text s)))

#-windows
(defun ledger-test-fields (line)
  (uiop:split-string line :separator '(#\Tab)))

#-windows
(defun ledger-test-row (text name)
  (let ((line (find-if (lambda (line)
                         (string= (first (ledger-test-fields line)) name))
                       (uiop:split-string text :separator '(#\Newline)))))
    (and line (ledger-test-fields line))))

#-windows
(defun ledger-test-lines (rows)
  (with-output-to-string (s)
    (dolist (row rows)
      (loop for (field . rest) on row
            do (write-string field s)
               (when rest (write-char #\Tab s)))
      (write-char #\Newline s))))

#-windows
(defun ledger-test-run (dir &key (command "render") (tree *ledger-root*) sbcl-cmd items)
  (let* ((out (make-string-output-stream))
         (env (append (list (concatenate 'string "LEDGER_OUT=" dir "/out")
                            (concatenate 'string "LEDGER_WORK=" dir)
                            (concatenate 'string "LEDGER_PREV=" dir "/prev.tsv")
                            (concatenate 'string "LEDGER_NOTES=" dir "/notes.tsv")
                            (concatenate 'string "LEDGER_TREE=" tree)
                            "LEDGER_LOCK="
                            (concatenate 'string "SBCL_CMD=" (or sbcl-cmd "true")))
                      (when items
                        (list (concatenate 'string "LEDGER_ITEMS=" items)))
                      (dotcl:posix-environ)))
         (p (dotcl:launch-process
             "bash"
             (list (namestring (merge-pathnames "bench/ledger/run.sh" *ledger-root*))
                   command)
             :environment env :output out :input nil :error nil)))
    (dotcl:process-wait p)
    (get-output-stream-string out)))

#-windows
(defun ledger-test-executable (path text)
  (ledger-test-write path text)
  (let ((p (dotcl:launch-process "chmod" (list "+x" path)
                                 :output (make-broadcast-stream)
                                 :input nil :error nil)))
    (dotcl:process-wait p)))

#-windows
(deftest ledger-cold-steady-columns-and-marks
  (let* ((dir (regression-temp-dir))
         (outdir (concatenate 'string dir "/out")))
    (ensure-directories-exist (concatenate 'string outdir "/"))
    (ledger-test-write
     (concatenate 'string outdir "/raw.tsv")
     (ledger-test-lines
      (list (list "cl-bench/fib" "cold" "dotcl" "1" "100" "ms" "" "")
            (list "cl-bench/fib" "cold" "dotcl" "2" "90" "ms" "" "")
            (list "cl-bench/fib" "cold" "sbcl" "1" "10" "ms" "" "")
            (list "cl-bench/fib" "steady" "dotcl" "-" "25" "ms" "" "7")
            (list "cl-bench/fib" "steady" "sbcl" "-" "10" "ms" "" "4")
            (list "cl-bench/stak" "cold" "dotcl" "1" "120" "ms" "" "")
            (list "cl-bench/stak" "cold" "sbcl" "1" "10" "ms" "" "")
            (list "cl-bench/stak" "steady" "dotcl" "-" "15" "ms" "" "5")
            (list "cl-bench/stak" "steady" "sbcl" "-" "10" "ms" "" "5")
            (list "cl-bench/clos/eql-fib" "cold" "dotcl" "1" "50" "ms" "" "")
            (list "cl-bench/clos/eql-fib" "cold" "sbcl" "1" "10" "ms" "" "")
            (list "cl-bench/clos/eql-fib" "steady" "dotcl" "-" "35" "ms" "" "9")
            (list "cl-bench/clos/eql-fib" "steady" "sbcl" "-" "10" "ms" "" "6")
            (list "cl-bench/tak" "cold" "dotcl" "1" "10" "ms" "" "")
            (list "cl-bench/tak" "cold" "sbcl" "1" "10" "ms" "" "")
            (list "cl-bench/tak" "steady" "dotcl" "-" "10" "ms" "" "3")
            (list "cl-bench/tak" "steady" "sbcl" "-" "10" "ms" "" "3"))))
    (ledger-test-write
     (concatenate 'string dir "/prev.tsv")
     (ledger-test-lines
      (list (list "cl-bench/fib" "100" "10" "5.00" "ms" "how" "")
            (list "cl-bench/stak" "100" "10" "5.00" "ms" "how" ""))))
    (ledger-test-run dir)
    (let* ((results (uiop:read-file-string
                     (concatenate 'string outdir "/results.tsv")))
           (header (uiop:read-file-string
                    (concatenate 'string outdir "/results.md")))
           (regs (uiop:read-file-string
                  (concatenate 'string outdir "/regressions.tsv")))
           (fib (ledger-test-row results "cl-bench/fib"))
           (stak (ledger-test-row results "cl-bench/stak"))
           (eql (ledger-test-row results "cl-bench/clos/eql-fib"))
           (tak (ledger-test-row results "cl-bench/tak")))
      (list (subseq fib 1 9)
            (nth 8 stak)
            (nth 8 eql)
            (nth 8 tak)
            (and (search "| item | cold dotcl | cold SBCL | cold ratio | steady dotcl | steady SBCL | steady ratio | runs | mark |" header) t)
            (and (search "cl-bench/stak" regs) t))))
  (("90.0" "10.0" "9.00" "25.0" "10.0" "2.50" "7/4" "!")
   "!" "!!" "" t t))

#-windows
(defun ledger-test-fake-runner (tab)
  (format nil "#!/bin/bash~%~
if [ \"${LEDGER_STEADY:-}\" = 1 ]; then~%~
  case \" $* \" in~%~
    *bic2-run.lisp*) echo \"BIC2 12.5 ns/px chk 42 runs 5 unstable\" ;;~%~
    *) echo \"STEADY tak 7.5 6 stable\" ;;~%~
  esac~%~
else~%~
  case \" $* \" in~%~
    *bic2-run.lisp*) echo \"BIC2 10.0 ns/px chk 42\" ;;~%~
    *) echo \"tak~A5.0\" ;;~%~
  esac~%~
fi~%"
          tab))

#-windows
(deftest ledger-measure-parses-group-steady-output
  (let* ((dir (regression-temp-dir))
         (tree (concatenate 'string dir "/fake-tree/"))
         (bin (concatenate 'string tree "runtime/bin/Release/net10.0/"))
         (script (ledger-test-fake-runner (string #\Tab))))
    (ensure-directories-exist bin)
    (ensure-directories-exist (concatenate 'string tree "compiler/"))
    (ledger-test-write (concatenate 'string tree "compiler/dotcl.core") "")
    (ledger-test-executable (concatenate 'string bin "runtime") script)
    (ledger-test-executable (concatenate 'string dir "/fake-sbcl") script)
    (ledger-test-write
     (concatenate 'string dir "/items.tsv")
     (ledger-test-lines
      (list (list "parity" "parity" "-" "60")
            (list "imago/bic2" "imago" "bic2" "60"))))
    (ledger-test-run dir :command "measure" :tree tree
                     :sbcl-cmd (concatenate 'string dir "/fake-sbcl")
                     :items (concatenate 'string dir "/items.tsv"))
    (ledger-test-run dir :tree tree)
    (let* ((results (uiop:read-file-string
                     (concatenate 'string dir "/out/results.tsv")))
           (par (ledger-test-row results "parity/tak"))
           (bic (ledger-test-row results "imago/bic2")))
      (list (subseq par 0 9) (subseq bic 0 9) (nth 11 bic))))
  (("parity/tak" "5.0" "5.0" "1.00" "7.5" "7.5" "1.00" "6" "")
   ("imago/bic2" "10.0" "10.0" "1.00" "12.5" "12.5" "1.00" "5" "")
   "steady: not stable"))
