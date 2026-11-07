;;; The keyword part of a call to a standard or dotcl function with &KEY
;;; parameters: an odd number of keyword arguments (CLHS 3.5.1.6) and an unknown
;;; keyword without a true :ALLOW-OTHER-KEYS (3.5.1.4) signal PROGRAM-ERROR, and
;;; :ALLOW-OTHER-KEYS T admits an unknown keyword. Covers every non-macro &KEY
;;; function in the lambda list tables. Uses CL-LL-SHAPE from
;;; cl-builtin-lambda-lists.lisp.

(defclass kwargs-c () ((a :initarg :a)))
(defclass kwargs-d () ((a :initarg :a)))

(defun kwargs-noop (&rest r) (declare (ignore r)) nil)

;; Not called at all: they build or save an application, or end the process.
(defparameter *kwargs-not-called* '("SAVE-APPLICATION" "BUILD-LIBRARY" "QUIT" "EXIT"))

;; Their NONE / UNKNOWN-AOK calls fail with a PROGRAM-ERROR of their own (no
;; lock held, no such program, no compiler on the emit-free build).
(defparameter *kwargs-valid-calls-not-checked*
  (append '("CONDITION-WAIT" "LAUNCH-PROCESS")
          (unless (emitting-mode-p) '("COMPILE-FILE"))))

;; SBCL 2.6.8 also accepts these: ALLOCATE-INSTANCE ignores its initargs.
(defparameter *kwargs-odd-accepted* '("ALLOCATE-INSTANCE"))

(defun kwargs-sample (fname pname)
  (macrolet ((is (&rest names) `(member pname ',names :test #'string=)))
    (cond
      ((is "ITEM" "NEWITEM" "OLDITEM" "NEW" "OLD" "OBJECT" "DATUM") 1)
      ((is "LIST" "LIST-1" "LIST-2" "SEQUENCE" "SEQUENCE-1" "SEQUENCE-2" "TREE" "TREE-1" "TREE-2")
       (list 1 2))
      ((is "ALIST") (list (cons 1 2)))
      ((is "STRING" "STRING1" "STRING2" "PREFIX")
       (copy-seq (if (string= fname "PARSE-INTEGER") "12" "ab")))
      ((is "PREDICATE" "TEST" "FUNCTION") #'kwargs-noop)
      ((is "RESULT-TYPE") 'list)
      ((is "SIZE" "DIMENSIONS" "NEW-DIMENSIONS") 2)
      ((is "ARRAY") (make-array 2 :adjustable t))
      ((is "PATHNAME") #p"/tmp/x.y")
      ((is "SOURCE") "a.b")
      ((is "FROM-WILDCARD") "*.b")
      ((is "TO-WILDCARD") "*.c")
      ((and (is "INPUT-FILE") (string= fname "COMPILE-FILE-PATHNAME")) "/tmp/x.lisp")
      ((and (is "STREAM") (string= fname "TO-STREAM")) (dotnet:new "System.IO.MemoryStream"))
      ((is "STREAM")
       (if (search "READ" fname) (make-string-input-stream "abc") (make-string-output-stream)))
      ((is "OUTPUT-STREAM") (make-string-output-stream))
      ((is "PACKAGE-NAME") (symbol-name (gensym "KWARGS-PKG")))
      ((is "DEFAULT-PATHNAME") *default-pathname-defaults*)
      ((is "THING") "/tmp/x.y")
      ((is "VECTOR") (make-array 2 :element-type '(unsigned-byte 8) :initial-element 65))
      ((is "SEMAPHORE") (dotcl:make-semaphore :count 1))
      ((is "TYPE-OR-OBJECT") "System.String")
      ((and (is "CLASS") (string= fname "ALLOCATE-INSTANCE")) (find-class 'kwargs-c))
      ((is "CLASS") 'kwargs-c)
      ((is "NEW-CLASS") 'kwargs-d)
      ((is "INSTANCE" "PREVIOUS" "CURRENT")
       (if (string= fname "INITIALIZE-INSTANCE")
           (allocate-instance (find-class 'kwargs-c))
           (make-instance 'kwargs-c)))
      ((is "SLOT-NAMES") t)
      ((is "FUNCTION-NAME") (intern (symbol-name (gensym "KWARGS-GF"))))
      (t nil))))

;; What (FUNCALL CALLER SYMBOL ARGS) does: :PROGRAM-ERROR, another condition's
;; type, or :RETURNED.
(defun kwargs-outcome (caller fn args)
  (block call
    (let ((*debugger-hook* (lambda (c h) (declare (ignore h))
                             (return-from call (list :debugger (type-of c)))))
          (*standard-output* (make-broadcast-stream)))
      (handler-case
          (restart-case (progn (funcall caller fn args) :returned)
            (abort (&rest r) (declare (ignore r)) :abort))
        (program-error () :program-error)
        (serious-condition (c) (type-of c))))))

;; Every (symbol positional-parameter-names keys allow-other-keys) to check.
(defun kwargs-functions ()
  (let ((r '()))
    (dolist (section (cons (cons "COMMON-LISP" (funcall dotcl-internal::*cl-builtin-lambda-lists*))
                           (funcall dotcl-internal::*ext-builtin-lambda-lists*)))
      (let ((pkg (find-package (first section))))
        (when pkg
          (dolist (entry (rest section))
            (multiple-value-bind (s status) (find-symbol (first entry) pkg)
              (when (and (eq status :external) (eq (symbol-package s) pkg) (fboundp s)
                         (not (macro-function s)) (not (special-operator-p s))
                         (not (member (first entry) *kwargs-not-called* :test #'string=)))
                (let ((ll (dotcl:function-lambda-list s)))
                  (when (member '&key ll)
                    (destructuring-bind (req opt rest keys aok) (cl-ll-shape ll)
                      (declare (ignore rest))
                      (push (list s
                                  (mapcar (lambda (p) (symbol-name (if (consp p) (car p) p)))
                                          (subseq (remove-if (lambda (p) (member p lambda-list-keywords)) ll)
                                                  0 (+ req opt)))
                                  (reverse keys) aok)
                            r))))))))))
    (nreverse r)))

;; The calls whose outcome is not the expected one, as (symbol case outcome).
(defun kwargs-wrong-calls (caller)
  (let ((bad '()))
    (dolist (e (kwargs-functions) (nreverse bad))
      (destructuring-bind (s params keys aok) e
        (let* ((name (symbol-name s))
               (k1 (if keys (intern (first keys) :keyword) :start)))
          (dolist (c `((:odd t ,k1)
                       (:odd-aok t :allow-other-keys t ,k1)
                       ,@(unless aok '((:unknown t :no-such-key 1)))
                       (:unknown-aok nil :no-such-key 1 :allow-other-keys t)
                       (:none nil)))
            (destructuring-bind (case reject &rest keyargs) c
              (unless (or (and (not reject) (member name *kwargs-valid-calls-not-checked* :test #'string=))
                          (and (member case '(:odd :odd-aok))
                               (member name *kwargs-odd-accepted* :test #'string=)))
                (let ((o (kwargs-outcome caller s
                                         (append (mapcar (lambda (p) (kwargs-sample name p)) params)
                                                 keyargs))))
                  (unless (eq (eq o :program-error) reject)
                    (push (list s case o) bad)))))))))))

(deftest builtin-keyword-args-apply
  (kwargs-wrong-calls (lambda (s args) (apply (fdefinition s) args)))
  nil)

;; A compiled call with the function's name in operator position, which some
;; functions (OPEN, DIRECTORY, MAKE-HASH-TABLE ...) compile to a direct call
;; that does not go through the function object.
(deftest-emitting-only builtin-keyword-args-compiled-call
  (kwargs-wrong-calls
   (lambda (s args)
     (funcall (handler-bind ((warning #'muffle-warning))
                (compile nil `(lambda () (,s ,@(mapcar (lambda (a) `',a) args))))))))
  nil)

;; The keyword names a C# function was registered with (WithKeyArgs) agree with
;; its table entry. LAUNCH-PROCESS also takes UIOP's spellings of two keys.
(defparameter *kwargs-registered-extra*
  '(("LAUNCH-PROCESS" "ERROR-OUTPUT" "IF-ERROR-OUTPUT-EXISTS")))

(deftest builtin-keyword-args-registered-matches-table
  (let ((n 0) (bad '()))
    (dolist (e (kwargs-functions))
      (destructuring-bind (s params keys aok) e
        (let ((registered (dotcl-internal::%function-key-args (fdefinition s))))
          (when registered
            (incf n)
            (destructuring-bind (positional rkeys raok) registered
              (unless (and (= positional (length params))
                           (eq (and raok t) (and aok t))
                           (null (set-exclusive-or
                                  rkeys
                                  (append keys (rest (assoc (symbol-name s) *kwargs-registered-extra*
                                                            :test #'string=)))
                                  :test #'string=)))
                (push (list s registered) bad)))))))
    (list (>= n 20) bad))
  (t nil))

(deftest builtin-keyword-args-message
  (list (handler-case (dotcl:make-semaphore :name "s" :count)
          (program-error (c) (princ-to-string c)))
        (handler-case (pathname-name #p"/tmp/x.y" :no-such-key 1)
          (program-error (c) (princ-to-string c))))
  ("MAKE-SEMAPHORE: odd number of keyword arguments"
   "PATHNAME-NAME: unrecognized keyword argument :NO-SUCH-KEY"))

;; DOTCL-THREAD's wrappers pass non-keyword symbols to the internal
;; %MAKE-SEMAPHORE and friends; only the public functions check.
(deftest builtin-keyword-args-public-thread-functions
  (let ((sem (dotcl:make-semaphore :name "s" :count 1))
        (cv (dotcl:make-condition-variable :name "c"))
        (lock (dotcl:make-lock)))
    (list (dotcl:wait-on-semaphore sem :timeout 0.1)
          (dotcl:wait-on-semaphore sem :timeout 0.01)
          (progn (dotcl:acquire-lock lock)
                 (unwind-protect (dotcl:condition-wait cv lock :timeout 0.01)
                   (dotcl:release-lock lock)))
          (handler-case (progn (dotcl:make-semaphore 'count 1) :returned)
            (program-error () :program-error))))
  (t nil nil :program-error))

(dolist (p (list-all-packages))
  (when (eql 0 (search "KWARGS-PKG" (package-name p)))
    (delete-package p)))
