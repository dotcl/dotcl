;;; A local function that binds a name itself (LET, LET*, MULTIPLE-VALUE-BIND, a LAMBDA parameter,
;;; the variable of DOLIST / DOTIMES) is still substituted at a call site where the same name is
;;; bound to something else: the copy binds it again. A name it only reads, or reads before it
;;; binds it, is the call site's if the call site binds it, so the call stays an ordinary call.
;;; Every form gives the same value substituted (INLINE) as not (NOTINLINE).

(defmacro %lisb-both (name fn-def call)
  "The value of CALL with FN-DEF declared INLINE, and with it declared NOTINLINE."
  `(list (flet (,fn-def) (declare (inline ,name)) ,call)
         (flet (,fn-def) (declare (notinline ,name)) ,call)))

(deftest local-inline-self-bound-names.let
  (list (%lisb-both f (f (a) (let ((x (* a 2))) (+ x 1))) (let ((x 100)) (+ x (f 5))))
        (%lisb-both f (f (a) (let* ((x a) (y (+ x 1))) (* y 2))) (let ((x 100) (y 200)) (list x y (f 5))))
        (%lisb-both f (f (a) (let ((x a) (y 3)) (+ x y))) (let ((x 100) (y 200)) (list x y (f 5)))))
  ((111 111) ((100 200 12) (100 200 12)) ((100 200 8) (100 200 8))))

(deftest local-inline-self-bound-names.mvb-lambda-do
  (list (%lisb-both f (f (a) (multiple-value-bind (x y) (values a 2) (+ x y)))
                    (let ((x 100) (y 200)) (list x y (f 5))))
        (%lisb-both f (f (a) (funcall (lambda (x) (* x 3)) a)) (let ((x 100)) (list x (f 5))))
        (%lisb-both f (f (a) (let ((s 0)) (dolist (x (list a a)) (incf s x)) s))
                    (let ((x 100) (s 7)) (list x s (f 5))))
        (%lisb-both f (f (a) (let ((s 0)) (dotimes (i a) (incf s i)) s))
                    (let ((i 100) (s 7)) (list i s (f 5)))))
  (((100 200 7) (100 200 7)) ((100 15) (100 15)) ((100 7 10) (100 7 10)) ((100 7 10) (100 7 10))))

(defmacro %lisb-both-x (name fn-def call)
  "As %LISB-BOTH, with X bound to 1 around the definition."
  `(list (let ((x 1)) (flet (,fn-def) (declare (inline ,name)) ,call))
         (let ((x 1)) (flet (,fn-def) (declare (notinline ,name)) ,call))))

(deftest local-inline-self-bound-names.free-names-stay-the-definitions
  (list (%lisb-both-x f (f (a) (+ x a)) (let ((x 100)) (f 1)))
        (%lisb-both-x f (f (a) (+ x (let ((x 2)) (* x a)))) (let ((x 100)) (f 3)))
        (%lisb-both-x f (f (a) (let ((x (+ x 1))) (* x a))) (let ((x 100)) (f 2))))
    ((2 2) (7 7) (4 4)))

(defvar *lisb-special* 1)
(defun read-x () (symbol-value 'x))

(deftest local-inline-self-bound-names.special-and-symbol-macro
  (list (%lisb-both f (f (a) (let ((*lisb-special* (* a 10))) (+ *lisb-special* 1)))
                    (let ((*lisb-special* 100)) (list *lisb-special* (f 5))))
        (%lisb-both f (f (a) (+ *lisb-special* a)) (let ((*lisb-special* 100)) (f 5)))
        (%lisb-both f (f (a) (symbol-macrolet ((x (* a 2))) (+ x 1))) (let ((x 100)) (list x (f 5))))
        (%lisb-both f (f (a) (macrolet ((m () '(* a 3))) (+ (m) 1))) (let ((a 100)) (list a (f 5))))
        (%lisb-both f (f (a) (flet ((g (y) (* y a))) (g 2))) (let ((y 100) (a 7)) (list y a (f 5))))
        (%lisb-both f (f (a) (labels ((g (y) (if (> y 0) (+ a (g (1- y))) 0))) (g 3))) (let ((y 100) (a 7)) (list y a (f 5))))
        (%lisb-both f (f (a) (let ((x a)) (declare (special x)) (read-x))) (let ((x 100)) (list x (f 5)))))
    (((100 51) (100 51)) (105 105) ((100 11) (100 11)) ((100 16) (100 16))
   ((100 7 10) (100 7 10)) ((100 7 15) (100 7 15)) ((100 5) (100 5))))


;;; The shape of local-time's %SPLIT-TIMESTRING: a function binds PARTS and COUNT with
;;; MULTIPLE-VALUE-BIND, and is called from inside another function that binds the same names.
(defun %lisb-real (x)
  (declare (fixnum x))
  (let ((n 0))
    (declare (fixnum n))
    (labels ((g (m) (multiple-value-bind (parts count) (values m 2) (incf n (+ parts count))))
             (h (m) (multiple-value-bind (parts count) (values m 3) (g parts) (incf n count))))
      (h x)
      n)))

#+dotcl
(defun %lisb-bytes (thunk)
  (funcall thunk)
  (let ((a0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
    (dotimes (i 1000) (funcall thunk))
    (/ (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") a0) 1000)))

(deftest local-inline-self-bound-names.same-names-at-the-call-site
  (list (%lisb-real 5) (%lisb-real 0))
  (10 5))

#+dotcl
(deftest-compiled-only local-inline-self-bound-names.no-allocation
  (< (%lisb-bytes (lambda () (%lisb-real 5))) 16)
  t)
