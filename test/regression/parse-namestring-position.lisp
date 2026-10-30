;;; PARSE-NAMESTRING reports where parsing stopped within the bounded substring.

(setf (logical-pathname-translations "PN-POS-LOG")
      '(("**;*.*.*" "/tmp/pn-pos/**/*.*")))

(defun parse-namestring-position-values (thing &rest args)
  (multiple-value-bind (pathname position)
      (apply #'parse-namestring thing nil *default-pathname-defaults* args)
    (list (and pathname (namestring pathname)) position)))

(deftest parse-namestring-position.start
  (parse-namestring-position-values "xx/tmp/foo.lisp" :start 2)
  ("/tmp/foo.lisp" 13))

(deftest parse-namestring-position.start-end
  (parse-namestring-position-values "xx/tmp/foo.lisp" :start 2 :end 9)
  ("/tmp/fo" 7))

(deftest parse-namestring-position.junk-physical
  (parse-namestring-position-values "foo.lisp junk" :junk-allowed t)
  ("foo.lisp junk" 13))

(deftest parse-namestring-position.logical-junk-fallback
  (parse-namestring-position-values "PN-POS-LOG;foo bar" :junk-allowed t)
  ("PN-POS-LOG;foo bar" 18))

(deftest parse-namestring-position.logical-host
  (multiple-value-bind (pathname position)
      (parse-namestring "foo.lisp" "PN-POS-LOG" *default-pathname-defaults*)
    (list (namestring pathname) position))
  ("PN-POS-LOG:FOO.LISP" 8))

(deftest parse-namestring-position.logical-junk
  (multiple-value-bind (pathname position)
      (parse-namestring "foo bar" "PN-POS-LOG" *default-pathname-defaults*
                        :junk-allowed t)
    (list pathname position))
  (nil 3))

(deftest parse-namestring-position.logical-junk-start
  (multiple-value-bind (pathname position)
      (parse-namestring "xxfoo bar" "PN-POS-LOG" *default-pathname-defaults*
                        :start 2 :junk-allowed t)
    (list pathname position))
  (nil 3))

(deftest parse-namestring-position.pathname
  (multiple-value-bind (pathname position)
      (parse-namestring #p"/tmp/foo.lisp" nil *default-pathname-defaults* :start 2)
    (list (namestring pathname) position))
  ("/tmp/foo.lisp" 2))

(deftest parse-namestring-position.pathname-start-end
  (multiple-value-bind (pathname position)
      (parse-namestring #p"/tmp/foo.lisp" nil *default-pathname-defaults* :start 3 :end 7)
    (list (namestring pathname) position))
  ("/tmp/foo.lisp" 3))

(deftest parse-namestring-position.stream
  (let ((path (regression-temp-file "parse-namestring-position.txt")))
    (unwind-protect
        (progn
          (with-open-file (s path :direction :output :if-exists :supersede)
            (write-string "x" s))
          (with-open-file (s path :direction :input)
            (multiple-value-bind (pathname position) (parse-namestring s)
              (list (pathnamep pathname) position))))
      (ignore-errors (delete-file path))))
  (t nil))
