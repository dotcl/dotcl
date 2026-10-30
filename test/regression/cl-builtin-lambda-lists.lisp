;;; Lambda lists of the COMMON-LISP operators (compiler/cl-lambda-lists.lisp).
;;;
;;; DOTCL:FUNCTION-LAMBDA-LIST answers every fbound external CL symbol, and the
;;; function objects they name, with the lambda list of the operator's dictionary
;;; entry. The coverage tests enumerate the package, so an operator that gains a
;;; definition without an entry turns them red.

(defun cl-ll-table ()
  (funcall dotcl-internal::*cl-builtin-lambda-lists*))

(defun cl-ll-names (ll)
  (mapcar (lambda (x) (if (consp x) (cl-ll-names x) (symbol-name x))) ll))

(defun cl-ll-of (x) (multiple-value-list (dotcl:function-lambda-list x)))

(deftest cl-ll-every-operator-has-one
  (let ((missing '()))
    (do-external-symbols (s "COMMON-LISP")
      (when (and (fboundp s) (not (second (cl-ll-of s))))
        (push s missing)))
    missing)
  nil)

;;; Every entry names an fbound external CL symbol, once.

(deftest cl-ll-entries-name-operators
  (let ((bad '()) (seen '()))
    (dolist (entry (cl-ll-table))
      (multiple-value-bind (s status) (find-symbol (first entry) "COMMON-LISP")
        (when (or (not (eq status :external)) (not (fboundp s))
                  (member (first entry) seen :test #'string=))
          (push (first entry) bad))
        (push (first entry) seen)))
    bad)
  nil)

;;; Each entry is a well-formed lambda list: the keywords in their order, each at
;;; most once, &BODY and nested lists only for a macro or special operator, and
;;; plain ASCII.

(defun cl-ll-well-formed-p (ll macrop)
  (let ((order '(&optional &rest &body &key &allow-other-keys))
        (pos -1)
        (rest-vars nil))
    (dolist (x ll (not (eql rest-vars 0)))
      (cond ((member x lambda-list-keywords)
             (let ((p (position x order)))
               (when (or (null p) (<= p pos) (eql rest-vars 0)
                         (and (eq x '&body) (not macrop))
                         (and (member x '(&rest &body)) rest-vars))
                 (return nil))
               (setq pos p)
               (when (member x '(&rest &body)) (setq rest-vars 0))))
            ((consp x)
             (unless (and macrop (cl-ll-well-formed-p x t)) (return nil)))
            ((not (symbolp x)) (return nil))
            ((eql pos 4) (return nil))
            ((member pos '(1 2))
             (if (eql rest-vars 0) (setq rest-vars 1) (return nil)))))))

(deftest cl-ll-entries-well-formed
  (let ((bad '()))
    (dolist (entry (cl-ll-table))
      (let ((s (find-symbol (first entry) "COMMON-LISP")))
        (unless (and (every (lambda (c) (< (char-code c) 128)) (second entry))
                     (cl-ll-well-formed-p (first (cl-ll-of s))
                                          (or (special-operator-p s) (macro-function s))))
          (push (first entry) bad))))
    bad)
  nil)

;;; Where the implementation records a lambda list of its own (the operators
;;; written in Lisp), every call the entry describes is one that lambda list
;;; accepts: no more required arguments, room for every optional one, and
;;; every keyword.

(defun cl-ll-shape (ll)
  (let ((mode :req) (req 0) (opt 0) (rest nil) (keys '()) (aok nil))
    (dolist (x ll)
      (case x
        (&optional (setq mode :opt))
        ((&rest &body) (setq mode :rest rest t))
        (&key (setq mode :key))
        (&allow-other-keys (setq aok t))
        (&aux (setq mode :aux))
        (t (case mode
             (:req (incf req))
             (:opt (incf opt))
             (:key (push (let ((v (if (consp x) (car x) x)))
                           (symbol-name (if (consp v) (first v) v)))
                         keys))))))
    (list req opt rest keys aok)))

(defun cl-ll-accepts-p (impl entry)
  (destructuring-bind (ireq iopt irest ikeys iaok) (cl-ll-shape impl)
    (destructuring-bind (ereq eopt erest ekeys eaok) (cl-ll-shape entry)
      (declare (ignore eaok))
      (and (<= ireq ereq)
           (or irest (<= (+ ereq eopt) (+ ireq iopt)))
           (or (not erest) irest)
           (or iaok (and irest (null ikeys))
               (subsetp ekeys ikeys :test #'string=))))))

(defvar *cl-ll-read-package*
  (or (find-package "CL-LL-READ") (make-package "CL-LL-READ" :use '("COMMON-LISP"))))

(defun cl-ll-implementation (s)
  (let* ((fn (and (not (special-operator-p s)) (not (macro-function s)) (fdefinition s)))
         (raw (and fn (not (typep fn 'generic-function))
                   (dotcl-internal::%function-stored-lambda-list fn))))
    (if (stringp raw)
        (let ((*package* *cl-ll-read-package*)) (read-from-string raw))
        raw)))

(deftest cl-ll-entries-agree-with-implementation
  (let ((bad '()) (checked 0))
    (dolist (entry (cl-ll-table))
      (let* ((s (find-symbol (first entry) "COMMON-LISP"))
             (impl (cl-ll-implementation s)))
        (when (consp impl)
          (incf checked)
          (unless (cl-ll-accepts-p impl (first (cl-ll-of s)))
            (push (first entry) bad)))))
    (list (> checked 100) bad))
  (t nil))

;;; Representative entries.

(deftest cl-ll-function-from-csharp
  (cl-ll-names (first (cl-ll-of 'mapcar)))
  ("FUNCTION" "LIST" "&REST" "MORE-LISTS"))

(deftest cl-ll-keywords
  (cl-ll-names (first (cl-ll-of 'remove)))
  ("ITEM" "SEQUENCE" "&KEY" "FROM-END" "TEST" "TEST-NOT" "START" "END" "COUNT" "KEY"))

(deftest cl-ll-optional-then-key
  (cl-ll-names (first (cl-ll-of 'read-from-string)))
  ("STRING" "&OPTIONAL" "EOF-ERROR-P" "EOF-VALUE" "&KEY" "START" "END" "PRESERVE-WHITESPACE"))

(deftest cl-ll-no-arguments
  (cl-ll-of 'get-universal-time)
  (nil t))

(deftest cl-ll-macro
  (cl-ll-names (first (cl-ll-of 'with-open-file)))
  (("STREAM" "FILESPEC" "&REST" "OPTIONS") "&BODY" "BODY"))

(deftest cl-ll-special-operator
  (cl-ll-names (first (cl-ll-of 'if)))
  ("TEST-FORM" "THEN-FORM" "&OPTIONAL" "ELSE-FORM"))

(deftest cl-ll-generic-function
  (cl-ll-names (first (cl-ll-of 'print-object)))
  ("OBJECT" "STREAM"))

;;; The function object answers as its name does, and so does a macro function.

(deftest cl-ll-function-object
  (list (equal (cl-ll-of #'mapcar) (cl-ll-of 'mapcar))
        (equal (cl-ll-of (macro-function 'when)) (cl-ll-of 'when)))
  (t t))

;;; A name that is a COMMON-LISP symbol is that symbol; any other is
;;; uninterned, so the table interns nothing.

(deftest cl-ll-names-are-cl-or-uninterned
  (let ((ll (first (cl-ll-of 'mapcar))))
    (list (eq (first ll) 'function)
          (eq (second ll) 'list)
          (symbol-package (fourth ll))
          (find-symbol "MORE-LISTS" "DOTCL-INTERNAL")))
  (t t nil nil))

;;; Asking twice gives the same list.

(deftest cl-ll-same-list
  (eq (dotcl:function-lambda-list 'mapcar) (dotcl:function-lambda-list 'mapcar))
  t)

;;; A user's function is not touched by the table.

(defun cl-ll-user-fn (alpha &key beta) (list alpha beta))

(deftest cl-ll-user-function-unchanged
  (cl-ll-names (first (cl-ll-of 'cl-ll-user-fn)))
  ("ALPHA" "&KEY" "BETA"))
