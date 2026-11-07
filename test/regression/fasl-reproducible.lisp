;;; A .fasl is a function of its source (and of the path it was compiled from, which it records
;;; for the definition-source lookup): compiling the same file to the same name twice gives the
;;; same bytes (the module version id, the PE time stamp and the module-name suffix come from a hash of
;;; the content, not from a random number or the clock), and combine-fasls likewise. Different
;;; sources still get different module names, so their load-time values do not collide.

(defun %frp-write (path text)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string text o))
  path)

(defun %frp-bytes (path)
  (with-open-file (i path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8))))
      (read-sequence v i)
      v)))

(defparameter *frp-source*
  "(defpackage :frp-a (:use :cl))
(in-package :frp-a)
(defstruct pt (x 0d0 :type double-float) y)
(defun f (a b) (declare (fixnum a b)) (+ a (* b 2)))
(defmacro m (x) `(list ,x ,x))
(defun g (x) (m (f x 3)))
(defun ltv () (load-time-value (list :a)))
")

;;; Each compile runs in a process of its own: counters and gensym numbers that name things in
;;; the fasl are per process, and a build is a fresh process.
(defvar *frp-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun %frp-compile (dir src &optional path-map)
  (let* ((in (%frp-write (format nil "~A/~A/src.lisp" (regression-temp-dir) dir) src))
         (out (format nil "~A/~A/out.fasl" (regression-temp-dir) dir))
         (script (%frp-write (format nil "~A/~A/build.lisp" (regression-temp-dir) dir)
                             (format nil "~@[(setq dotcl:*source-path-map* '~S)~%~](compile-file ~S :output-file ~S)~%"
                                     (and path-map
                                          (list (cons (format nil "~A/~A" (regression-temp-dir) dir) "/src")))
                                     in out)))
         (empty (%frp-write (format nil "~A/~A/empty.txt" (regression-temp-dir) dir) "")))
    (ignore-errors
     (uiop:run-program (list *frp-exe* "--core" (regression-child-core) (namestring script))
                       :input empty :output :string :error-output :string
                       :ignore-error-status t))
    out))

(deftest-emitting-only fasl-reproducible.compile-file-twice
  (let* ((a (%frp-compile "frp1" *frp-source*))
         (ba (%frp-bytes a)))
    (sleep 1.1)
    ;; The same source path and output name: the fasl records where the definitions came from
    ;; (the source path), so a different directory is a different input.
    (let* ((b (%frp-compile "frp1" *frp-source*))
           (bb (%frp-bytes b)))
      (list (> (length ba) 1000) (equalp ba bb))))
  (t t))

(deftest-emitting-only fasl-reproducible.loads-and-runs
  (let ((a (%frp-compile "frp3" *frp-source*)))
    (load a)
    (list (funcall (intern "G" :frp-a) 4)
          (equal (funcall (intern "LTV" :frp-a)) '(:a))))
  ((10 10) t))

(deftest-emitting-only fasl-reproducible.different-sources-differ
  (let* ((a (%frp-compile "frp4" "(defun %frp-x () (load-time-value (list 1)))"))
         (b (%frp-compile "frp5" "(defun %frp-y () (load-time-value (list 2)))")))
    (load a)
    (load b)
    (list (not (equalp (%frp-bytes a) (%frp-bytes b)))
          (funcall '%frp-x) (funcall '%frp-y)))
  (t (1) (2)))

(deftest-emitting-only fasl-reproducible.combine-fasls-twice
  (let* ((a (%frp-compile "frp6" *frp-source*))
         (b (%frp-compile "frp7" "(defun %frp-z () :z)"))
         (o1 (format nil "~A/frp8/all.fasl" (regression-temp-dir)))
         (o2 (format nil "~A/frp9/all.fasl" (regression-temp-dir))))
    (ensure-directories-exist o1)
    (ensure-directories-exist o2)
    (dotcl:combine-fasls (list a b) o1)
    (sleep 1.1)
    (dotcl:combine-fasls (list a b) o2)
    (list (equalp (%frp-bytes o1) (%frp-bytes o2))
          (progn (load o1) (funcall '%frp-z))))
  (t :z))

(defun %frp-contains-p (bytes string)
  "True when STRING occurs in BYTES as UTF-8 or UTF-16LE, written with / or (on Windows, where
a full path has backslashes) with \\ for the separators."
  (flet ((find-in (pattern)
           (search pattern bytes)))
    (flet ((one (string)
             (or (find-in (map '(vector (unsigned-byte 8)) #'char-code string))
                 (find-in (let ((v (make-array (* 2 (length string)) :element-type '(unsigned-byte 8) :initial-element 0)))
                            (dotimes (i (length string) v) (setf (aref v (* 2 i)) (char-code (char string i)))))))))
      (and (or (one string)
               (one (substitute #\\ #\/ string)))
           t))))

(deftest-emitting-only fasl-reproducible.source-path-map
  (let* ((dir1 (format nil "~A/frpm1" (regression-temp-dir)))
         (dir2 (format nil "~A/frpm2" (regression-temp-dir)))
         (a (%frp-compile "frpm1" *frp-source* t))
         (b (%frp-compile "frpm2" *frp-source* t))
         (ba (%frp-bytes a))
         (bb (%frp-bytes b))
         (c (%frp-compile "frpm3" *frp-source*))
         (d (%frp-compile "frpm4" *frp-source*)))
    (list (equalp ba bb)                          ; mapped: the same bytes from two directories
          (%frp-contains-p ba dir1)               ; and no trace of either directory
          (%frp-contains-p bb dir2)
          (%frp-contains-p ba "/src/src.lisp")    ; the mapped path is there
          (not (equalp (%frp-bytes c) (%frp-bytes d)))  ; unmapped (the default): the path is recorded
          (%frp-contains-p (%frp-bytes c) (format nil "~A/frpm3" (regression-temp-dir)))))
  (t nil nil t t t))
