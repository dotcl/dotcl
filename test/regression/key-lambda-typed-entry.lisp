;;; An anonymous &key lambda that captures nothing gets the typed entries a
;;; DEFUN with the same lambda list gets, so a call that passes no keyword (or
;;; one pair) does not build an argument array. Method functions are such
;;; lambdas: a generic function call that passes no keyword reached its method
;;; through an array.

(defvar *klte-plain* (lambda (a b &key (start 0) (end (length b)))
                       (list a start end)))
(defvar *klte-supplied* (lambda (a &key (x (* a 2) x-p) (y (+ a x)))
                          (list a x x-p y)))
(defvar *klte-explicit* (lambda (a &key ((:the-key k) :none)) (list a k)))
(defvar *klte-other* (lambda (a &key (x 1) &allow-other-keys) (list a x)))
(defvar *klte-call* (lambda (fn b &key (start 0) (end (length b)))
                      (funcall fn b start end)))

(deftest key-lambda-typed-entry.values
  (list (funcall *klte-plain* 1 "abc")
        (funcall *klte-plain* 1 "abc" :start 2)
        (funcall *klte-plain* 1 "abc" :end 1 :start 0)
        (apply *klte-plain* 1 "abc" '(:start 1 :end 2))
        (funcall *klte-supplied* 3)
        (funcall *klte-supplied* 3 :x 10)
        (funcall *klte-supplied* 3 :y 0)
        (funcall *klte-explicit* 1)
        (funcall *klte-explicit* 1 :the-key 2)
        (funcall *klte-other* 1 :zzz 2)
        (funcall *klte-other* 1 :x 5 :zzz 2))
  ((1 0 3) (1 2 3) (1 0 1) (1 1 2) (3 6 nil 9) (3 10 t 13) (3 6 nil 0)
   (1 :none) (1 2) (1 1) (1 5)))

(deftest key-lambda-typed-entry.errors
  (list (handler-case (funcall *klte-plain* 1 "abc" :bogus 1) (program-error () :pe))
        (handler-case (funcall *klte-plain* 1 "abc" :start) (program-error () :pe))
        (handler-case (funcall *klte-plain* 1) (program-error () :pe)))
  (:pe :pe :pe))

(defgeneric %klte-gf (a b &key start end))
(defmethod %klte-gf ((a function) b &key (start 0) (end (length b)))
  (funcall a b start end))
(defmethod %klte-gf ((a symbol) b &key (start 0) (end (length b)))
  (list a b start end))

(deftest key-lambda-typed-entry.method
  (list (%klte-gf #'list "abc")
        (%klte-gf #'list "abc" :start 1)
        (%klte-gf 'x "ab")
        (%klte-gf 'x "ab" :end 1))
  (("abc" 0 3) ("abc" 1 3) (x "ab" 0 2) (x "ab" 0 1)))

(defun %klte-bytes (fn n)
  (dotimes (i 100) (funcall fn))
  (min-allocated-bytes (lambda () (dotimes (i n) (funcall fn)))))

(defun %klte-miss (s st en) (declare (ignore s st en)) nil)

(deftest-emitting-only key-lambda-typed-entry.no-array
  (let ((f *klte-call*) (g #'%klte-miss))
    (< (%klte-bytes (lambda () (funcall f g "abc")) 10000) 4000))
  t)

(defun %klte-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))))

(deftest-emitting-only key-lambda-typed-entry.fasl
  (progn
    (load (%klte-compile
           "klte"
           '("(defvar *klte-f* (lambda (a b &key (start 0) (end (length b))) (list a start end)))"
             "(defvar *klte-f2* (lambda (fn b &key (start 0) (end (length b))) (funcall fn b start end)))"
             "(defgeneric klte-gf2 (a b &key start))"
             "(defmethod klte-gf2 ((a function) b &key (start 0)) (funcall a b start))")))
    (let ((f (symbol-value '*klte-f*)) (g #'%klte-miss))
      (list (funcall f 1 "abc")
            (funcall f 1 "abc" :end 2)
            (handler-case (funcall f 1 "abc" :bogus 1) (program-error () :pe))
            (funcall 'klte-gf2 (lambda (b s) (list b s)) "x")
            (let ((f2 (symbol-value '*klte-f2*)))
              (< (%klte-bytes (lambda () (funcall f2 g "abc")) 10000) 4000)))))
  ((1 0 3) (1 0 2) :pe ("x" 0) t))
