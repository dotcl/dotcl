;;; A LAMBDA with no closed-over variables, compiled to a .fasl, is called
;;; through an entry pointer like a closure (up to 4 required arguments).

(defun %fle-state (f) (dotnet:static "DotCL.LispFunction" "EntryState" f))

(defparameter *fle-source* "
(defun %fle-l0 () (lambda () :zero))
(defun %fle-l1 () (lambda (x) (list :one x)))
(defun %fle-l2 () (lambda (a b) (< a b)))
(defun %fle-l4 () (lambda (a b c d) (list a b c d)))
(defun %fle-l5 () (lambda (a b c d e) (list a b c d e)))
(defun %fle-mv () (lambda (a) (values a (1+ a) (+ a 2))))
(defun %fle-err () (lambda (a) (car a)))
(defun %fle-rec () (lambda (n) (if (zerop n) 0 (+ 1 (funcall (%fle-rec) (1- n))))))
(defun %fle-fold (f y xs) (if (consp xs) (%fle-fold f (funcall f y (car xs)) (cdr xs)) y))
(defun %fle-sum (xs) (%fle-fold (lambda (a x) (+ a x)) 0 xs))
(defun %fle-sort (xs) (sort (copy-list xs) (lambda (a b) (< a b))))
")

(defun %fle-load ()
  (let ((src (regression-temp-file "fasl-lambda-entry-src.lisp"))
        (fasl (regression-temp-file "fasl-lambda-entry-src.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *fle-source* s))
    (load (compile-file src :output-file fasl))))

(when (compiled-mode-p) (%fle-load))

(deftest-compiled-only fasl-lambda-entry.state
  (mapcar (lambda (s) (%fle-state (funcall s))) '(%fle-l0 %fle-l1 %fle-l2 %fle-l4 %fle-l5))
  ("closure-entry" "closure-entry" "closure-entry" "closure-entry" "none"))

(deftest-compiled-only fasl-lambda-entry.calls
  (list (funcall (%fle-l0)) (funcall (%fle-l1) 1) (apply (%fle-l1) '(2))
        (mapcar (%fle-l1) '(3 4)) (funcall (%fle-l2) 1 2) (funcall (%fle-l2) 2 1)
        (funcall (%fle-l4) 1 2 3 4) (apply (%fle-l4) 1 2 '(3 4))
        (funcall (%fle-l5) 1 2 3 4 5)
        (multiple-value-list (funcall (%fle-mv) 1))
        (multiple-value-list (apply (%fle-mv) '(10)))
        (%fle-sum '(1 2 3 4 5)) (%fle-sort '(3 1 2 5 4))
        (funcall (%fle-rec) 1000))
  (:zero (:one 1) (:one 2) ((:one 3) (:one 4)) t nil (1 2 3 4) (1 2 3 4)
   (1 2 3 4 5) (1 2 3) (10 11 12) 15 (1 2 3 4 5) 1000))

(deftest-compiled-only fasl-lambda-entry.errors
  (list (handler-case (funcall (%fle-l1) 1 2) (program-error () :program-error))
        (handler-case (funcall (%fle-l2) 1) (program-error () :program-error))
        (handler-case (apply (%fle-l4) '(1)) (program-error () :program-error))
        (handler-case (funcall (%fle-err) 5) (type-error () :type-error)))
  (:program-error :program-error :program-error :type-error))

(deftest-compiled-only fasl-lambda-entry.stack-overflow-catchable
  (call-on-small-stack
   (lambda ()
     (handler-case (progn (funcall (%fle-rec) 10000000) :no-overflow)
       (error () :caught-error)
       (storage-condition () :caught-storage-condition))))
  :caught-storage-condition)
