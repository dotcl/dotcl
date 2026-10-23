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

;;; PRINT.{SHORT,SINGLE,DOUBLE,LONG}-FLOAT.{2,4} each draw 10000 integers from
;;; [-10^7, 10^7-1] and require PRIN1 of each as a float to read back as
;;; "N.0<d>". The low endpoint is out of spec: CLHS 22.1.3.1.3 stops
;;; fixed-point notation at |x| = 10^7, so -10^7 must print as -1.0xs --
;;; which dotcl, SBCL and ABCL all do. At 0.05% per test it read as an
;;; unreproducible CI red.
;;;
;;; Narrow the range by that one value instead of skipping the tests: the
;;; other 19999999 draws are real printer coverage. The rewrite edits the
;;; upstream body itself rather than restating it, and the guard makes the
;;; override self-invalidating -- if the upstream body stops carrying the
;;; draw that was narrowed, it is left alone and reported rather than
;;; silently shadowed.
(in-package :cl-test)

(defun narrow-float-print-draw (form)
  "Copy FORM with (- (random 20000000) 10000000) -- whose lower endpoint
-10^7 is the one magnitude CLHS prints in exponential form -- replaced by
the narrowed draw (- (random 19999999) 9999999). Second value is true
when the draw was found."
  (let ((found nil))
    (labels ((rew (x)
               (cond ((equal x '(- (random 20000000) 10000000))
                      (setf found t)
                      '(- (random 19999999) 9999999))
                     ((atom x) x)
                     (t (cons (rew (car x)) (rew (cdr x)))))))
      (values (rew form) found))))

(defun narrow-float-print-test (name)
  (let* ((entry (cadr (gethash name regression-test::*entries-table*)))
         (form (and entry (regression-test::form entry))))
    (multiple-value-bind (new-form found) (and form (narrow-float-print-draw form))
      (if found
          (regression-test::add-entry
           (regression-test::make-entry :pend t :name name
                                        :props (regression-test::props entry)
                                        :form new-form
                                        :vals (regression-test::vals entry)))
          (format t "~&;; note: ~A no longer matches the narrowed draw; ~
                     leaving the upstream test as is~%" name)))))

(dolist (name '(print.short-float.2 print.short-float.4
                print.single-float.2 print.single-float.4
                print.double-float.2 print.double-float.4
                print.long-float.2 print.long-float.4))
  (narrow-float-print-test name))
(in-package :cl-user)

;;; COMPILE-FILE.2 checks that a file signalling only a STYLE-WARNING returns
;;; warnings-p true, but RT muffles every STYLE-WARNING around a test unless it
;;; has :do-not-muffle-warnings, and a warning muffled outside COMPILE-FILE is
;;; not counted (SBCL 2.6.8 fails it the same way). Give it that note. RT's
;;; HAS-NOTE compares note objects, so the object goes in, not the keyword.
(in-package :cl-test)
(let ((entry (cadr (gethash 'compile-file.2 regression-test::*entries-table*))))
  (when entry
    (push (regression-test::defnote :do-not-muffle-warnings
            "Let the test see style-warnings its forms signal.")
          (getf (regression-test::props entry) :notes))))
(in-package :cl-user)
