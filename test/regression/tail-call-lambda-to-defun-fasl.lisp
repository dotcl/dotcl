;;; A tail call from an anonymous function to a DEFUN that links its own frame,
;;; in a .fasl: the shape of Coalton's list:sort, where the comparison lambda
;;; tail-calls the Ord instance's method. Such a call is prepared at the call
;;; site now instead of in TailPrepCore. It still runs in constant stack, hands
;;; on the frame the lambda borrowed, and passes the caller's values through.

(defparameter *tclf-source* "
(defvar *tclf-to-odd*)
(defvar *tclf-to-even*)
(defun %tclf-even (n) (if (= n 0) :even (funcall *tclf-to-odd* (1- n))))
(defun %tclf-odd (n) (if (= n 0) :odd (funcall *tclf-to-even* (1- n))))
(defun %tclf-setup ()
  (setf *tclf-to-odd* (lambda (n) (%tclf-odd n))
        *tclf-to-even* (lambda (n) (%tclf-even n))))
(defun %tclf-two (a b) (values b a))
(defun %tclf-swap (a b) (funcall (lambda (x y) (%tclf-two x y)) a b))
(defun %tclf-leaf (a) (declare (ignore a)) (dotcl:backtrace-with-args))
(defun %tclf-via-lambda (x) (funcall (lambda (y) (%tclf-leaf y)) (list x)))
(defstruct (tclf-dict (:constructor tclf-dict (cmp))) (cmp nil :read-only t))
(defun %tclf-cmp (a b) (cond ((< a b) :lt) ((> a b) :gt) (t :eq)))
(defun %tclf-sort (dict xs)
  (sort (copy-list xs)
        (lambda (a b) (eq :lt (funcall (lambda (p q) (funcall (tclf-dict-cmp dict) p q)) a b)))))
")

(defun %tclf-load ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "tail-call-lambda-to-defun.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *tclf-source* s))
    (load (compile-file src :output-file (merge-pathnames "tail-call-lambda-to-defun.fasl" dir)))
    (funcall '%tclf-setup)))

;; COMPILE-FILE needs Reflection.Emit, so a build without it skips this file.
(when (emitting-mode-p) (%tclf-load))

(deftest-emitting-only tail-call-lambda-to-defun-fasl.constant-stack
  (list (funcall '%tclf-even 1000000) (funcall '%tclf-even 1000001))
  (:even :odd))

(deftest-emitting-only tail-call-lambda-to-defun-fasl.values-pass-through
  (multiple-value-list (funcall '%tclf-swap 1 2))
  (2 1))

(deftest-emitting-only tail-call-lambda-to-defun-fasl.lambda-passes-the-frame-on
  (loop for f in (funcall '%tclf-via-lambda 6)
        when (member (car f) '("%TCLF-LEAF" "%TCLF-VIA-LAMBDA") :test #'string=)
          collect f)
  (("%TCLF-LEAF" (6))))

(deftest-emitting-only tail-call-lambda-to-defun-fasl.sort-through-a-dictionary
  (let ((l (loop for i below 300 collect (mod (* i 7919) 307))))
    (equal (funcall '%tclf-sort (funcall 'tclf-dict '%tclf-cmp) l)
           (sort (copy-list l) #'<)))
  t)
