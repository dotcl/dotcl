;;; A macro form inside a local function body is expanded once by COMPILE, as in SBCL: the check
;;; that decides whether the local function can be inlined reuses the expansion the analysis made.

(defvar *mof-count* 0)
(defmacro mof-value () 5)
(defmacro mof-count () (incf *mof-count*) nil)

(defun %mof-expansions (lambda-form)
  (setf *mof-count* 0)
  (compile nil lambda-form)
  *mof-count*)

(deftest-emitting-only macro-expanded-once-in-flet.flet
  (%mof-expansions '(lambda (a) (flet ((f (z) (mof-count) z)) (f a))))
  1)

(deftest-emitting-only macro-expanded-once-in-flet.nested
  (%mof-expansions '(lambda (a) (flet ((f () (flet ((g () (mof-count))) (g)))) (f) a)))
  1)

(deftest-emitting-only macro-expanded-once-in-flet.labels-and-plain
  (list (%mof-expansions '(lambda (a) (labels ((f (z) (mof-count) (if z (f nil) 1))) (f a))))
        (%mof-expansions '(lambda (a) (let ((y a)) (mof-count) y))))
  (1 1))

(deftest-emitting-only macro-expanded-once-in-flet.still-works
  (funcall (compile nil '(lambda (a) (flet ((f (z) (+ z (mof-value))) (g (z) (* z 2))) (g (f a)))))
           5)
  20)

