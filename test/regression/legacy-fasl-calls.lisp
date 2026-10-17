;;; A fasl compiled by a released dotcl (0.1.33) calls, and is called by, code
;;; compiled now.
;;;
;;; The bodies of ordinary functions now record their own debugger frame, and
;;; compiled callers reach them without LispFunction.InvokeN. A fasl from 0.1.33
;;; still calls everything through InvokeN and TailPrepN, and its bodies expect
;;; InvokeN to have pushed their frame. Neither side may lose or double a frame,
;;; a tail call across the two must still run in constant stack, and an error
;;; caught by the old code after passing through new bodies must leave the frame
;;; list as it was.

(handler-bind ((dotcl:fasl-version-mismatch #'dotcl:load-as-is))
  (load "test/regression/legacy-fasl/calls-0.1.33.fasl"))

(defun cl-user::%lf-new-leaf (x) (list :new x))
(defun cl-user::%lf-new-bt (x) (declare (ignore x)) (values (dotcl:backtrace)))
(defun cl-user::%lf-new-throw (x) (values (error "legacy-fasl ~A" x)))
(defun cl-user::%lf-new-make-closure ()
  (lambda (x) (values (cl-user::%lf-new-throw x))))
(defun cl-user::%lf-new-od (n) (if (= n 0) :odd (cl-user::%lf-old-ev (1- n))))
(defun cl-user::%lf-new-five (a b c d n)
  (if (= n 0) (list a b c d) (cl-user::%lf-old-five b c d a (1- n))))
(defun cl-user::%lf-new-throw-tag (x) (values (throw 'cl-user::%lf-tag x)))
(defun cl-user::%lf-new-calls-old (x) (values (cl-user::%lf-old-leaf x)))
(defun cl-user::%lf-new-tail-old (x) (cl-user::%lf-old-leaf x))

(defun %lf-names (bt)
  (remove-if-not (lambda (n) (and (> (length n) 4) (string= "%LF-" n :end2 4))) bt))

(deftest legacy-fasl.calls-both-ways
  (list (cl-user::%lf-old-calls-new 1) (cl-user::%lf-old-tail-new 2)
        (cl-user::%lf-new-calls-old 3) (cl-user::%lf-new-tail-old 4))
  ((:new 1) (:new 2) (:old 3) (:old 4)))

(deftest-emitting-only legacy-fasl.one-frame-each
  (list (%lf-names (cl-user::%lf-old-bt 1))
        (%lf-names (cl-user::%lf-old-bt-tail 1)))
  (("%LF-NEW-BT" "%LF-OLD-BT") ("%LF-NEW-BT")))

(deftest legacy-fasl.old-handler-after-new-frames
  (let ((base (length (dotcl:backtrace))))
    (list (- (length (cl-user::%lf-old-catch 1)) base)
          (- (length (cl-user::%lf-old-closure-catch 1)) base)
          (- (length (dotcl:backtrace)) base)))
  (1 1 0))

(deftest-emitting-only legacy-fasl.old-cleanup-sees-its-frame
  (%lf-names (cl-user::%lf-old-unwind 1))
  ("%LF-OLD-UNWIND"))

(deftest-emitting-only legacy-fasl.tail-calls-across
  (list (cl-user::%lf-old-ev 1000000) (cl-user::%lf-old-five 1 2 3 4 1000000))
  (:even (1 2 3 4)))
