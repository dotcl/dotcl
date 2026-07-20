;;; DOTCL:FUNCTION-SOURCE-LOCATION — LOAD/COMPILE-FILE record where a top-level
;;; definition was written, backing swank/micros sldb frame-source-location and
;;; find-definitions (M-. / jump to the erroring function).

;;; Write a fixture whose known line layout we can assert against, load it so the
;;; source locations get recorded, then query them.
(defparameter *fsl-tmp* "test/regression/fsl-fixture-tmp.lisp")

(with-open-file (s *fsl-tmp* :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
  (write-line "(defun fsl-reg-foo (x) (* x x))" s)             ; line 1
  (write-line "(defmethod fsl-reg-bar ((x integer)) x)" s)     ; line 2
  (write-line "(progn (defun fsl-reg-inner (y) y))" s)         ; line 3
  (write-line "(defclass fsl-reg-pt () ((x :initarg :x)))" s)) ; line 4

(load *fsl-tmp*)

(deftest fsl-load-records-defun-line
  (getf (dotcl:function-source-location 'fsl-reg-foo) :line)
  1)

(deftest fsl-load-records-defmethod-line
  (getf (dotcl:function-source-location 'fsl-reg-bar) :line)
  2)

;; PROGN is descended without macroexpanding, so the inner defun is recorded.
(deftest fsl-load-records-progn-nested-defun-line
  (getf (dotcl:function-source-location 'fsl-reg-inner) :line)
  3)

(deftest fsl-load-records-defclass-line
  (getf (dotcl:function-source-location 'fsl-reg-pt) :line)
  4)

(deftest fsl-file-recorded
  (notnot (search "fsl-fixture-tmp.lisp"
                  (getf (dotcl:function-source-location 'fsl-reg-foo) :file)))
  t)

(deftest fsl-unknown-name-nil
  (dotcl:function-source-location 'fsl-reg-no-such-name)
  nil)

;; Interactive / compile-string path (C-c C-c): a tool compiles a single form
;; outside LOAD/COMPILE-FILE and records it with the buffer file+line itself.
(dotcl:record-definition-sources '(defun fsl-reg-eval-fn (a) a) "buf.lisp" 42)

(deftest fsl-record-explicit-line
  (getf (dotcl:function-source-location 'fsl-reg-eval-fn) :line)
  42)

(deftest fsl-record-explicit-file
  (getf (dotcl:function-source-location 'fsl-reg-eval-fn) :file)
  "buf.lisp")

;; Records nested definitions inside a PROGN too (no macroexpansion).
(dotcl:record-definition-sources
 '(progn (defun fsl-reg-eval-a () 1) (defun fsl-reg-eval-b () 2)) "buf2.lisp" 7)

(deftest fsl-record-progn-nested
  (list (getf (dotcl:function-source-location 'fsl-reg-eval-a) :line)
        (getf (dotcl:function-source-location 'fsl-reg-eval-b) :line))
  (7 7))

(ignore-errors (delete-file *fsl-tmp*))
