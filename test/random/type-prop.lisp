;;;; Random type propagation test: ansi-test random/random-type-prop*.lsp.
;;;;
;;;; For each of about 900 standard operators, pfdietz's tester draws random
;;;; arguments of the declared argument types, evaluates the call with EVAL,
;;;; and compares the answer with a COMPILEd LAMBDA that gets some arguments as
;;;; parameters with random type declarations (and THE forms) and random
;;;; OPTIMIZE settings. A difference, or an error only one side signals, fails
;;;; the test. It exercises the compiler's type inference and the open-coded
;;;; paths of the operators, which the integer form test does not reach.
;;;;
;;;; Parameters (set with --eval before --load):
;;;;   cl-user::*tp-reps*  random tries per test (default 20; ansi-test uses 1000)
;;;;   cl-user::*tp-out*   output directory (default "out/random-type-prop/")
;;;;
;;;; Writes failures.txt (each failing test with what it returned: the
;;;; compiled form, the arguments, and both answers) and summary.txt in
;;;; *tp-out*.

(in-package :cl-user)

(defvar *tp-reps* 20)
(defvar *tp-out* "out/random-type-prop/")

(load "test/random/load.lisp")

(in-package :cl-test)

(compile-and-load "random-type-prop.lsp")
(setf *default-reps* cl-user::*tp-reps*)

(dolist (f '("random-type-prop-tests-01.lsp" "random-type-prop-tests-02.lsp"
             "random-type-prop-tests-03.lsp" "random-type-prop-tests-04.lsp"
             "random-type-prop-tests-05.lsp" "random-type-prop-tests-06.lsp"
             "random-type-prop-tests-07.lsp" "random-type-prop-tests-08.lsp"
             "random-type-prop-tests-09.lsp" "random-type-prop-tests-10.lsp"))
  (load (cl-user::%rf-ansi "random/" f)))

(let* ((out (ensure-directories-exist
             (merge-pathnames cl-user::*tp-out* (truename "."))))
       (start (get-internal-real-time))
       (names (regression-test:pending-tests))
       (failures '()))
  (with-open-file (s (merge-pathnames "failures.txt" out)
                     :direction :output :if-exists :supersede)
    (dolist (name names)
      (let* ((form (regression-test::form (regression-test::get-entry name)))
             (r (handler-case (let ((*standard-output* (make-broadcast-stream)))
                                (eval form))
                  (error (e) (list :error (princ-to-string e))))))
        (when r
          (push name failures)
          (let ((*print-pretty* nil) (*print-length* 20) (*print-level* 6))
            (format s "~S~%  test: ~S~%  value: ~S~%" name form r))
          (finish-output s)))))
  (with-open-file (s (merge-pathnames "summary.txt" out)
                     :direction :output :if-exists :supersede)
    (dolist (stream (list s *standard-output*))
      (format stream "~&random-type-prop: ~D tests x ~D reps, ~D failed (~,1F s)~%"
              (length names) cl-user::*tp-reps* (length failures)
              (/ (- (get-internal-real-time) start) internal-time-units-per-second))
      (dolist (f (reverse failures)) (format stream "  ~A~%" f)))))
