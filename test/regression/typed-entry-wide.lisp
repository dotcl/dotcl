;;; Wide typed entries: a declaimed FTYPE that gives some arguments or the
;;; result a raw kind (an integer type inside int64, DOUBLE-FLOAT) and the others
;;; any other type, with up to six arguments. The raw ones are passed raw, the
;;; others as they are ("O" in the signature).
;;;
;;; What must hold: the answers and the multiple values are those of the
;;; ordinary call, also after redefinition and across fasls; the callee's frame
;;; shows every argument; the raw arguments are checked; calls in tail position
;;; still run in constant stack.

(setf dotcl:*save-sil* t)

(defun %tw-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct %tw-pt x y)

(declaim (notinline %tw-clip %tw-scale %tw-six %tw-mv))
(declaim (ftype (function (double-float double-float double-float double-float %tw-pt)
                          (values t &optional))
                %tw-clip))
(defun %tw-clip (a b c d p)
  (if (< (+ a b) (+ c d)) (%tw-pt-x p) (list a b c d)))
(declaim (ftype (function (%tw-pt double-float) (values double-float &optional)) %tw-scale))
(defun %tw-scale (p k) (* k (%tw-pt-x p)))
(declaim (ftype (function (fixnum t fixnum t double-float fixnum) (values t &optional)) %tw-six))
(defun %tw-six (a b c d e f) (list (+ a c f) b d e))
(declaim (ftype (function (fixnum) t) %tw-mv))
(defun %tw-mv (i) (values i (* 2 i) (* 3 i)))

(defun %tw-user (n)
  (let ((p (make-%tw-pt :x 3d0 :y 4d0)) (s 0d0))
    (declare (double-float s))
    (dotimes (i n) (setq s (+ s (%tw-scale p 2d0))))
    (list (%tw-clip 1d0 2d0 5d0 6d0 p) (%tw-clip 9d0 2d0 5d0 6d0 p) s
          (%tw-six 1 :b 2 "d" 0.5d0 3)
          (multiple-value-list (%tw-mv 5))
          (+ (%tw-mv 2) 1))))

(deftest-compiled-only typed-entry-wide.signatures
  (mapcar (lambda (f) (dotcl::%typed-entry-signature (fdefinition f)))
          '(%tw-clip %tw-scale %tw-six %tw-mv))
  ("DDDDO>O" "OD>D" "LOLODL>O" "L>O"))

(deftest typed-entry-wide.values
  (%tw-user 3)
  (3d0 (9d0 2d0 5d0 6d0) 18d0 (6 :b "d" 0.5d0) (5 10 15) 3))

(deftest-emitting-only typed-entry-wide.uses-typed-entry
  (%tw-count "CALL-TYPED-SYM" #'%tw-user)
  6)

;;; A raw result used where a raw value is wanted is not boxed on the way.
(declaim (ftype (function (t) (values fixnum &optional)) %tw-len))
(defun %tw-len (l) (length l))
(defun %tw-dsum (p) (+ (%tw-scale p 1d0) (%tw-scale p 2d0)))
(defun %tw-lsum (l) (+ (%tw-len l) (%tw-len (cdr l))))

(deftest-emitting-only typed-entry-wide.raw-result-not-boxed
  (list (%tw-dsum (make-%tw-pt :x 2d0)) (%tw-lsum '(1 2 3))
        (%tw-count "UNBOX-DOUBLE" #'%tw-dsum) (%tw-count "DoubleFloat" #'%tw-dsum)
        (%tw-count "UNBOX-FIXNUM" #'%tw-lsum))
  (6d0 5 0 1 0))

(deftest typed-entry-wide.boxed-entry
  (list (funcall '%tw-clip 1d0 2d0 5d0 6d0 (make-%tw-pt :x :x))
        (multiple-value-list (funcall '%tw-mv 1))
        (apply '%tw-six '(1 2 3 4 5d0 6)))
  (:x (1 2 3) (10 2 4 5d0)))

(deftest-compiled-only typed-entry-wide.checked
  (list (handler-case (%tw-clip 1 2d0 3d0 4d0 nil)
          (type-error (e) (list :type-error (type-error-datum e))))
        (handler-case (funcall '%tw-six 1 2 3.5 4 5d0 6)
          (type-error (e) (list :type-error (type-error-datum e)))))
  ((:type-error 1) (:type-error 3.5)))

;;; The callee's frame: object arguments as they are, raw ones boxed.
(defun %tw-probe () (dotcl:backtrace-with-args))
(defvar *tw-frames* nil)
(declaim (notinline %tw-framed %tw-framed6))
(declaim (ftype (function (t double-float) (values t &optional)) %tw-framed))
(defun %tw-framed (o x) (setq *tw-frames* (%tw-probe)) (list o x))
(declaim (ftype (function (t fixnum t fixnum t double-float) (values t &optional)) %tw-framed6))
(defun %tw-framed6 (a b c d e f) (setq *tw-frames* (%tw-probe)) (list a b c d e f))

(deftest typed-entry-wide.frame
  (list (progn (list (%tw-framed :obj 2.5d0))
               (find "%TW-FRAMED" *tw-frames* :key #'car :test #'string=))
        (progn (list (%tw-framed6 "a" 1 'c 2 nil 3d0))
               (find "%TW-FRAMED6" *tw-frames* :key #'car :test #'string=)))
  (("%TW-FRAMED" :obj 2.5d0) ("%TW-FRAMED6" "a" 1 c 2 nil 3d0)))

;;; Redefinition: into another typed body, into none, into other kinds.
(declaim (ftype (function (t fixnum) (values t &optional)) %tw-redef))
(defun %tw-redef (o n) (list o n))
(defun %tw-redef-caller (n) (list (%tw-redef :a (+ n 1))))

(deftest typed-entry-wide.redefine
  (let ((r '()))
    (push (%tw-redef-caller 1) r)
    (eval '(defun %tw-redef (o n) (cons n o)))
    (push (%tw-redef-caller 1) r)
    (eval '(defun %tw-redef (o n &optional (k 10)) (list o n k)))
    (push (%tw-redef-caller 1) r)
    (setf (fdefinition '%tw-redef) (lambda (o n) (values n o)))
    (push (%tw-redef-caller 1) r)
    (nreverse r))
  (((:a 2)) ((2 . :a)) ((:a 2 10)) (2)))

;;; Tail calls: an object result through other wide typed entries runs in
;;; constant stack; one with an ordinary call in tail position keeps that tail
;;; call and goes without the typed entry.
(declaim (ftype (function (fixnum t) (values t &optional)) %tw-ev %tw-od))
(defun %tw-ev (n acc) (if (= n 0) acc (%tw-od (1- n) acc)))
(defun %tw-od (n acc) (if (= n 0) (list acc) (%tw-ev (1- n) acc)))
(defun %tw-plain (n acc) (if (= n 0) :plain (%tw-mixed (1- n) acc)))
(declaim (ftype (function (fixnum t) (values t &optional)) %tw-mixed))
(defun %tw-mixed (n acc) (if (= n 0) acc (%tw-plain (1- n) acc)))

(deftest-emitting-only typed-entry-wide.tail-calls-run-in-constant-stack
  (list (%tw-ev 3000001 :x) (%tw-mixed 1000000 :m)
        (dotcl::%typed-entry-signature #'%tw-ev)
        (dotcl::%typed-entry-signature #'%tw-mixed))
  ((:x) :m "LO>O" nil))

;;; Across fasls.
(defvar *tw-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-tw-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %tw-compile-and-load (source name)
  (let ((lisp (concatenate 'string *tw-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only typed-entry-wide.fasl
  (progn
    (%tw-compile-and-load
     "(in-package :cl-user)
      (declaim (notinline %tw-f))
      (declaim (ftype (function (t double-float t fixnum fixnum) (values t &optional)) %tw-f))
      (defun %tw-f (a x b n m) (list a (* x n) b m))"
     "tw-callee")
    (%tw-compile-and-load
     "(in-package :cl-user)
      (defun %tw-f-caller (n) (list (%tw-f :a 1.5d0 :b n 7)))"
     "tw-caller")
    (list (funcall (intern "%TW-F-CALLER") 4)
          (dotcl::%typed-entry-signature (fdefinition (intern "%TW-F")))
          (progn (eval `(defun ,(intern "%TW-F") (a x b n m) (list b a x n m)))
                 (funcall (intern "%TW-F-CALLER") 4))))
  (((:a 6d0 :b 7)) "ODOLL>O" ((:b :a 1.5d0 4 7))))
