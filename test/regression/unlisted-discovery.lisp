;;; A test file that run.lisp does not name is loaded by LOAD-UNLISTED-REGRESSION-FILES, in name
;;; order, as the ";;; regression:" marks of its header say. This file is not named in run.lisp:
;;; that it runs at all is the first check; the others are on a directory made here.

(load "test/regression-manifest.lisp")   ; run.lisp has loaded it; the file runner has not
(defvar *ud-ran* t)

(deftest unlisted-discovery.this-file-was-discovered
  (and (member "unlisted-discovery.lisp" (regression-unlisted-files "test/regression/run.lisp")
               :test #'string=)
       *ud-ran*)
  t)

(defun %ud-write (dir name &rest lines)
  (with-open-file (o (concatenate 'string dir name) :direction :output :if-exists :supersede)
    (dolist (l lines) (write-line l o))))

(deftest unlisted-discovery.marks
  (let ((dir (regression-temp-dir/)))
    (%ud-write dir "m1.lisp" ";;; a header" ";;; regression: skip-under-interp" "(print 1)")
    (%ud-write dir "m2.lisp" ";;; regression: full-tier-only skip-under-interp" "(print 1)")
    (%ud-write dir "m3.lisp" ";;; regression: skip -- why not" "(print 1)")
    (%ud-write dir "m4.lisp" "(print 1)")
    (%ud-write dir "m5.lisp" ";;; regression: bogus-mark" "(print 1)")
    (%ud-write dir "m6.lisp" "(print 1)" ";;; regression: skip-under-interp")
    (list (%regression-file-marks (concatenate 'string dir "m1.lisp"))
          (%regression-file-marks (concatenate 'string dir "m2.lisp"))
          (%regression-file-marks (concatenate 'string dir "m3.lisp"))
          (%regression-file-marks (concatenate 'string dir "m4.lisp"))
          (%regression-known-marks-p (%regression-file-marks (concatenate 'string dir "m5.lisp")))
          (%regression-file-marks (concatenate 'string dir "m6.lisp"))))
  (("skip-under-interp") ("full-tier-only" "skip-under-interp") ("skip") nil nil ("skip-under-interp")))

(deftest unlisted-discovery.block-comment-hides-a-load
  (let ((dir (regression-temp-dir/)))
    (%ud-write dir "run-x.lisp"
               "(load \"test/regression/a.lisp\")"
               "#| (load \"test/regression/b.lisp\")"
               "   (load \"test/regression/c.lisp\") |#"
               "(load \"test/regression/d.lisp\") #| (load \"test/regression/e.lisp\") |#"
               ";; (load \"test/regression/f.lisp\")")
    (sort (copy-list (%regression-manifest-referenced (concatenate 'string dir "run-x.lisp")))
          #'string<))
  ("a.lisp" "d.lisp"))
