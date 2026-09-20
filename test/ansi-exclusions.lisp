;;; The adjustments dotcl makes to the ansi-test suite, in one place.
;;;
;;; Loaded by both runners -- test-ansi.lisp (the whole suite in one process)
;;; and the per-category runs the Makefile builds from test-ansi-cat.lisp -- so
;;; the two report the same numbers. They did not: the per-category path, which
;;; is the one CI runs, had none of this and counted twelve intentional
;;; deviations as failures.
;;;
;;; What each adjustment is and why it is defensible: docs/deviations.md. The
;;; measured answers from other implementations are in ansi-state.json.
;;;
;;; MUST be loaded after the category files and before DO-TESTS. Loading a
;;; category re-reads the suite's own notes.lsp, which would put the notes
;;; below back the way upstream has them; and the printer override replaces an
;;; entry the category load creates.

;;; Eight EXP.ERROR.8-11 / EXPT.ERROR.8-11: dotcl flushes float underflow to
;;; zero, as SBCL, CCL and ECL do. The suite already carries the note those
;;; implementations skip them by; registering it disabled here is how a fresh
;;; clone (which cannot hold a #+dotcl edit) is told we are one of them.
(in-package :regression-test)
(defnote :no-floating-point-underflow-by-default
  "dotcl flushes FP underflow to 0.0 by default (SBCL/CCL/ECL behavior)." t)

;;; Four LOOP.1.40-43: what an arithmetic loop's variable holds once FINALLY
;;; runs. CLHS 6.1.2.1.1 leaves it open, ansi-test tags the four as a spec
;;; problem itself, and dotcl, SBCL and ABCL all answer the same way (the
;;; stepped-past value) against the suite's expectation. Disabling this narrow
;;; note rather than :ansi-spec-problem keeps unrelated tests running.
(defnote :loop-iteration-values-in-finally
  "Arithmetic LOOP leaves the stepped value visible in FINALLY (SBCL/ABCL behavior)." t)
(in-package :cl-user)

;;; PRINT.DOUBLE-FLOAT.4 draws 10000 integers from [-10^7, 10^7-1] and requires
;;; PRIN1 of each as a double to read back as "N.0". The low endpoint is out of
;;; spec: CLHS 22.1.3.1.3 stops fixed-point notation at |x| = 10^7, so -10^7
;;; must print as -1.0d7 -- which dotcl, SBCL and ABCL all do. At 0.05% of runs
;;; it read as an unreproducible CI red.
;;;
;;; Narrow the range by that one value instead of skipping the test: the other
;;; 19999999 draws are real printer coverage. The guard makes the override
;;; self-invalidating -- if the upstream body stops matching what was patched
;;; here, it is left alone and reported rather than silently shadowed.
(in-package :cl-test)
(let* ((entry (cadr (gethash 'print.double-float.4 regression-test::*entries-table*)))
       (form (and entry (regression-test::form entry))))
  (if (and form (search "20000000" (let ((*print-readably* nil)) (princ-to-string form))))
      (deftest print.double-float.4
        (let ((chars *possible-double-float-exponent-markers*))
          (loop for type in '(short-float double-float long-float)
                nconc
                (and (not (subtypep 'double-float type))
                     (with-standard-io-syntax
                      (let ((*print-readably* nil)
                            (*read-default-float-format* type))
                        ;; Upstream: (- (random 20000000) 10000000), whose lower
                        ;; endpoint -10^7 is the one value CLHS prints in
                        ;; exponential form.
                        (loop for i = (- (random 19999999) 9999999)
                              for f = (float i 0.0d0)
                              for s1 = (with-output-to-string (s) (prin1 f s))
                              for len1 = (length s1)
                              for s2 = (format nil "~A.0" i)
                              repeat 10000
                              unless (or (/= i (rational f))
                                         (and (> len1 4)
                                              (string-equal s1 s2 :start1 0 :end1 (- len1 2))
                                              (eql (char s1 (- len1 1)) #\0)
                                              (member (char s1 (- len1 2)) chars)))
                              collect (list type i f s1 s2)))))))
        nil)
      (format t "~&;; note: PRINT.DOUBLE-FLOAT.4 no longer matches the patched form; ~
                 leaving the upstream test as is~%")))
(in-package :cl-user)
