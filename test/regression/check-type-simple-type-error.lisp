;;; CHECK-TYPE signals SIMPLE-TYPE-ERROR (a TYPE-ERROR that is also a
;;; SIMPLE-CONDITION) with SBCL's report, as SBCL does. It signalled a plain
;;; TYPE-ERROR, so a handler for SIMPLE-TYPE-ERROR did not see it: translate's
;;; test of its #t reader macro expects one. Expected values are SBCL 2.6.8's
;;; answers.

(deftest check-type-simple-type-error.class
  (let ((x 'bah))
    (handler-case (progn (check-type x string) :no-error)
      (simple-type-error (e)
        (list (type-error-datum e) (type-error-expected-type e) (typep e 'simple-condition)))))
  (bah string t))

(deftest check-type-simple-type-error.report
  (let ((x 'bah) (y (list 1)) (z 3))
    (flet ((%ct-report (thunk) (handler-case (funcall thunk) (type-error (e) (princ-to-string e)))))
      (list (%ct-report (lambda () (check-type x string)))
            (%ct-report (lambda () (check-type (car y) string)))
            (%ct-report (lambda () (check-type z string "a nice string"))))))
  ("The value of X is BAH, which is not of type STRING."
   "The value of (CAR Y) is 1, which is not of type STRING."
   "The value of Z is 3, which is not a nice string."))

;; The place is read once for the report, and STORE-VALUE still sets it.
(deftest check-type-simple-type-error.store-value
  (let ((reads 0) (cell (list 1)))
    (handler-bind ((type-error (lambda (c) (declare (ignore c)) (invoke-restart 'store-value "ok"))))
      (check-type (car (progn (incf reads) cell)) string))
    (list (car cell) (<= reads 4)))
  ("ok" t))

;; A CHECK-TYPE that passes costs no allocation: the place is not captured by
;; the restart's closures, which would put it in a heap cell on entry.
(defun %ct-pass (x y) (check-type x integer) (check-type y (or null cons)) x)
(defun %ct-pass-loop (n)
  (declare (fixnum n))
  (let ((r 0))
    (dotimes (i n r) (setq r (%ct-pass 5 nil)))))

(deftest-compiled-only check-type-simple-type-error.pass-no-alloc
  (< (bytes-per-op #'%ct-pass-loop) 8)
  t)

;; The STORE-VALUE restart's report and the stored value through a variable.
(deftest check-type-simple-type-error.store-value-report
  (let ((x 'bah) (report nil))
    (handler-bind ((type-error
                     (lambda (c)
                       (let ((r (find-restart 'store-value c)))
                         (setq report (princ-to-string r))
                         (invoke-restart r 42)))))
      (check-type x integer "a count"))
    (list x report))
  (42 "Supply a new value of type a count."))
