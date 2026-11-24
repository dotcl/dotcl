;;; What COMPILE-FILE and COMPILE tell the user about a mistake, as SBCL does: a malformed form,
;;; a RETURN-FROM to no block, a lambda list naming a variable twice, a call with the wrong number of
;;; arguments to a DEFUN of the file, a literal argument that cannot have the type the function wants,
;;; a literal FORMAT control with the wrong number of arguments, a second DEFUN of a name, an IGNORE of
;;; an unknown variable, an OTHERWISE clause that is not last, a macro used before it is defined.
;;; A correct input draws none of them.

(defun %cdg-conditions (name text)
  "The (class . message) of every condition COMPILE-FILE signals for the source TEXT, in order,
   and the third value of the compile."
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name))
        (conds '()) (failure nil))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (handler-bind ((warning (lambda (w)
                                (push (cons (if (typep w 'style-warning) :style :warning)
                                            (substitute #\Space #\Newline (princ-to-string w)))
                                      conds)
                                (muffle-warning w))))
        (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
          (declare (ignore fasl warnings-p))
          (setf failure failure-p))))
    (values (nreverse conds) failure)))

(defun %cdg-has (conds kind text)
  (and (find-if (lambda (c) (and (eq (car c) kind) (search text (cdr c)))) conds) t))

(deftest-emitting-only compile-diagnostics.malformed-forms
  (flet ((has (src text)
           (%cdg-has (%cdg-conditions "cdg1.lisp" src) :style text)))
    (list (has "(defun cdg-f () (let (x 1) x))" "malformed binding")
          (has "(defun cdg-f () (if 1 2 3 4))" "too many arguments")
          ;; reported as a style warning (this file's wording) or a full warning (the compiler's own)
          (or (has "(defun cdg-f () (return 1))" "no block named NIL")
              (%cdg-has (%cdg-conditions "cdg1.lisp" "(defun cdg-f () (return 1))") :warning "unknown block"))
          (or (has "(defun cdg-f () (go nowhere))" "no tagbody tag")
              (%cdg-has (%cdg-conditions "cdg1.lisp" "(defun cdg-f () (go nowhere))") :warning "unknown tag"))
          (has "(defun cdg-f (a a) a)" "bound more than once")
          (has "(defun cdg-f () (setq pi 3))" "not a variable")
          (has "(defun cdg-f () (1 2))" "Illegal function call")))
  (t t t t t t t))

(deftest-emitting-only compile-diagnostics.the-file-is-kept
  ;; one bad form no longer loses the file: the fasl exists, the good forms are in it, and the bad
  ;; one signals when it runs (SBCL replaces the offending form by a call that signals)
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/cdg2.lisp"))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(defun cdg2-ok () 7) (defun cdg2-bad () (return 1)) (defun cdg2-ok2 () 8)" s))
    (let ((fasl (handler-bind ((warning #'muffle-warning)) (compile-file path))))
      (list (and fasl t)
            (progn (handler-case (load fasl) (error () nil)) (funcall 'cdg2-ok))
            (handler-case (funcall 'cdg2-bad) (error () :error)))))
  (t 7 :error))

(deftest-emitting-only compile-diagnostics.calls-and-literals
  (flet ((has (src kind text)
           (%cdg-has (%cdg-conditions "cdg3.lisp" src) kind text)))
    (list (has "(defun cdg-g (a) a) (defun cdg-f () (cdg-g 1 2))" :warning "called with 2 arguments, but wants exactly 1")
          (has "(defun cdg-g (a b) a) (defun cdg-f () (cdg-g 1))" :warning "called with 1 argument, but wants exactly 2")
          (has "(defun cdg-f () (+ 1 \"a\"))" :warning "Constant \"a\" conflicts with its asserted type NUMBER")
          (has "(defun cdg-f () (car 5))" :warning "Constant 5 conflicts with its asserted type LIST")
          (has "(defun cdg-f () (length 5))" :warning "conflicts with its asserted type SEQUENCE")
          (has "(defun cdg-f () (format nil \"~A ~A\" 1))" :warning "Too few arguments (1) to FORMAT")
          (has "(defun cdg-f () (format nil \"~A\" 1 2))" :style "Too many arguments (2) to FORMAT")))
  (t t t t t t t))

(deftest-emitting-only compile-diagnostics.definitions-and-declarations
  (flet ((has (src kind text)
           (%cdg-has (%cdg-conditions "cdg4.lisp" src) kind text)))
    (list (has "(defun cdg-f () 1) (defun cdg-f () 2)" :warning "Duplicate definition for CDG-F found in one file")
          (has "(defun cdg-f (x) (declare (ignore y)) x)" :style "IGNORE declaration for an unknown variable: Y")
          (has "(defun cdg-f (x) (case x (otherwise 1) (2 3)))" :warning "Bad CASE clause")
          (has "(defun cdg-f () (cdg-late 1)) (defmacro cdg-late (x) x)" :style "is being redefined as a macro")))
  (t t t t))

(deftest-emitting-only compile-diagnostics.accessors-lambdas-directives
  (flet ((has (src kind text)
           (%cdg-has (%cdg-conditions "cdg6.lisp" src) kind text)))
    (list (has "(defstruct cdg-s a b) (defun cdg-f (s) (cdg-s-a s s))" :warning "CDG-S-A is called with 2 arguments, but wants exactly 1")
          (has "(defun cdg-f () (funcall (lambda (x) x)))" :warning "function called with 0 arguments, but wants exactly 1")
          (has "(defun cdg-f () (funcall #'(lambda (x &optional y) (list x y)) 1 2 3))" :warning "but wants between 1 and 2")
          (has "(defun cdg-f () (format nil \"~Q\" 1))" :style "unknown directive ~Q")))
  (t t t t))

(deftest-emitting-only compile-diagnostics.no-false-duplicate
  ;; the accessors and the constructor of a DEFSTRUCT in an EVAL-WHEN are compiled twice (at compile
  ;; time and for the fasl), and a macro that defines a function expands afresh each time: neither
  ;; is a duplicate definition; a second DEFUN in another top-level form is
  (list (%cdg-conditions "cdg7.lisp"
                         "(eval-when (:compile-toplevel :load-toplevel :execute)
                            (defstruct (cdg-o (:constructor %make-cdg-o (depth)) (:conc-name nil))
                              (depth 0 :type (unsigned-byte 16) :read-only t)))
                          (defmacro cdg-mk (n) `(defun ,n () 3))
                          (cdg-mk cdg-two)
                          (eval-when (:compile-toplevel :load-toplevel :execute) (defun cdg-one () 1))")
        (%cdg-has (%cdg-conditions "cdg8.lisp" "(defun cdg-one () 1) (defmacro cdg-mk (n) `(defun ,n () 3)) (cdg-mk cdg-one)")
                  :warning "Duplicate definition for CDG-ONE"))
  (nil t))

(deftest-emitting-only compile-diagnostics.correct-code-is-quiet
  (let ((src "(defun cdg-g (a &optional b) (list a b))
              (defstruct cdg-s a b)
              (defun cdg-f (l h)
                (declare (ignorable h))
                (list (cdg-g 1) (cdg-g 1 2) (car l) (+ 1 2.5) (length \"abc\") (nth 1 l) (gethash 'a h 0)
                      (format nil \"~A ~S~%\" 1 2) (format nil \"~{~A~}\" l) (format nil \"~D items~:P\" 3)
                      (case 1 (1 :a) (otherwise :b)) (< 1 2 3) (- 5) (/ 2)
                      (cdg-s-a (make-cdg-s)) (funcall (lambda (x &optional y) (list x y)) 1)
                      (funcall (lambda (&rest r) r)) (format nil \"~10,'0D ~@[x~] ~:@(~A~) ~/cl:print/\" 1 nil 2 3)))
              (eval-when (:compile-toplevel :load-toplevel :execute) (defun cdg-h (n) n))
              (defmacro cdg-m (x) x)
              (defun cdg-use () (cdg-m 1))"))
    (multiple-value-bind (conds failure) (%cdg-conditions "cdg5.lisp" src)
      (list conds failure)))
  (nil nil))
