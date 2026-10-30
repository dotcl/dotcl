;;; Lambda lists of the operators of dotcl's own packages
;;; (compiler/ext-lambda-lists.lisp) and of the contribs the docs describe.
;;; Uses CL-LL-SHAPE, CL-LL-ACCEPTS-P and CL-LL-WELL-FORMED-P from
;;; cl-builtin-lambda-lists.lisp.

(dolist (m '("dotnet-class" "dotcl-gray" "dotcl-socket" "dotcl-thread" "dotcl-simd"))
  (require m))

(defparameter *ext-ll-packages* '("DOTCL" "DOTNET" "DOTCL-MOP" "DOTCL-CLTL2"))

;; Functions a build may leave out: emit-only, Windows-only, or not on
;; netstandard2.0.
(defparameter *ext-ll-build-dependent*
  '("FFI" "%FFI-CALL" "MAKE-FFI-CALLBACK" "SIL-TO-FASL" "LIBRARY-RUNTIME-SURFACE"
    "UI-INVOKE" "UI-POST" "REQUIRE"))

(defun ext-ll-table () (funcall dotcl-internal::*ext-builtin-lambda-lists*))

(defun ext-ll-own-operators (package)
  (let ((pkg (find-package package)) (r '()))
    (do-external-symbols (s pkg)
      (when (and (eq (symbol-package s) pkg) (fboundp s)) (push s r)))
    r))

;;; What a lambda list looks like when nothing better was recorded: a generic
;;; function's (#:R0 #:R1 ...) made from its arity, or the (FORM ENV) of the
;;; expander behind a macro.
(defun ext-ll-placeholder-p (s ll)
  (flet ((un (x name) (and (symbolp x) (null (symbol-package x))
                           (or (null name) (string= (symbol-name x) name)))))
    (or (some (lambda (x)
                (and (un x nil)
                     (let ((n (symbol-name x)))
                       (and (> (length n) 1) (char= (char n 0) #\R)
                            (every #'digit-char-p (subseq n 1))))))
              ll)
        (and (macro-function s)
             (or (and (= (length ll) 2) (un (first ll) "FORM") (un (second ll) "ENV"))
                 (and (= (length ll) 1) (un (first ll) "FORM")))))))

(defun ext-ll-missing (packages)
  (let ((bad '()))
    (dolist (p packages)
      (dolist (s (ext-ll-own-operators p))
        (multiple-value-bind (ll known) (dotcl:function-lambda-list s)
          (when (or (not known) (ext-ll-placeholder-p s ll))
            (push s bad)))))
    bad))

(deftest ext-ll-every-operator-has-one
  (ext-ll-missing *ext-ll-packages*)
  nil)

(deftest ext-ll-contrib-operators-have-one
  (ext-ll-missing '("DOTCL-THREAD" "DOTCL-SOCKET" "DOTCL-GRAY" "DOTCL-SIMD"))
  nil)

;;; Every entry names an external symbol of its own package, once, and that
;;; symbol is fbound unless a build may leave it out.

(deftest ext-ll-entries-name-operators
  (let ((bad '()))
    (dolist (section (ext-ll-table))
      (let ((pkg (find-package (first section))) (seen '()))
        (dolist (entry (rest section))
          (multiple-value-bind (s status) (find-symbol (first entry) pkg)
            (when (or (member (first entry) seen :test #'string=)
                      (if (and s (eq status :external) (eq (symbol-package s) pkg))
                          (not (or (fboundp s)
                                   (member (first entry) *ext-ll-build-dependent*
                                           :test #'string=)))
                          (not (member (first entry) *ext-ll-build-dependent*
                                       :test #'string=))))
              (push (first entry) bad)))
          (push (first entry) seen))))
    bad)
  nil)

(deftest ext-ll-entries-well-formed
  (let ((bad '()))
    (dolist (section (ext-ll-table))
      (dolist (entry (rest section))
        (let ((s (find-symbol (first entry) (first section))))
          (when (and s (fboundp s))
            (unless (and (every (lambda (c) (< (char-code c) 128)) (second entry))
                         (cl-ll-well-formed-p (first (multiple-value-list
                                                      (dotcl:function-lambda-list s)))
                                              (macro-function s)))
              (push (first entry) bad))))))
    bad)
  nil)

;;; Each entry agrees with the function: a generic function's own lambda list
;;; has the same shape, a function written in Lisp accepts every call the entry
;;; describes, and a C# function that records how many arguments it requires
;;; requires as many as the entry.

(deftest ext-ll-entries-agree-with-implementation
  (let ((bad '()) (gfs 0) (counted 0))
    (dolist (section (ext-ll-table))
      (dolist (entry (rest section))
        (let* ((s (find-symbol (first entry) (first section)))
               (fn (and s (fboundp s) (not (macro-function s)) (fdefinition s)))
               (shape (and fn (cl-ll-shape (dotcl:function-lambda-list s)))))
          (cond ((null fn))
                ((typep fn 'generic-function)
                 (incf gfs)
                 (let ((own (cl-ll-shape (dotcl-mop:generic-function-lambda-list fn))))
                   (unless (equal (subseq own 0 3) (subseq shape 0 3))
                     (push s bad))))
                ((consp (dotcl-internal::%function-stored-lambda-list fn))
                 (unless (cl-ll-accepts-p (dotcl-internal::%function-stored-lambda-list fn)
                                          (dotcl:function-lambda-list s))
                   (push s bad)))
                ((>= (dotcl-internal::%function-arity fn) 0)
                 (incf counted)
                 (unless (= (dotcl-internal::%function-arity fn) (first shape))
                   (push s bad)))))))
    (list (> gfs 20) (> counted 100) bad))
  (t t nil))

;;; Every operator named as package:name in docs/ or a contrib README, in a
;;; package loaded here, has a lambda list.

(defun ext-ll-doc-symbols (text)
  (let ((r '()) (n (length text)))
    (flet ((name-char-p (c) (or (alphanumericp c) (find c "-%*+/<>=!."))))
      (dotimes (i n r)
        (when (and (char= (char text i) #\:) (> i 0) (< (1+ i) n)
                   (name-char-p (char text (1+ i))))
          (let* ((start (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\-)))
                                             text :end i :from-end t)
                            -1))
                 (end (or (position-if-not #'name-char-p text :start (1+ i)) n))
                 (pkg (string-upcase (subseq text (1+ start) i)))
                 (name (string-right-trim "." (string-upcase (subseq text (1+ i) end)))))
            (when (find-package pkg)
              (let ((s (find-symbol name pkg)))
                (when (and s (fboundp s)) (pushnew s r))))))))))

(defun ext-ll-file-text (path)
  (with-open-file (in path :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(deftest ext-ll-documented-operators-have-one
  (let ((named '()) (bad '()))
    (dolist (f (append (directory "docs/*.md") (directory "contrib/*/README.md")))
      (dolist (s (ext-ll-doc-symbols (ext-ll-file-text f))) (pushnew s named)))
    (dolist (s named)
      (multiple-value-bind (ll known) (dotcl:function-lambda-list s)
        (when (or (not known) (ext-ll-placeholder-p s ll)) (push s bad))))
    (list (> (length named) 100) bad))
  (t nil))

;;; Representative entries.

(deftest ext-ll-dotnet-invoke
  (cl-ll-names (dotcl:function-lambda-list 'dotnet:invoke))
  ("OBJECT" "MEMBER-NAME" "&REST" "ARGS"))

(deftest ext-ll-dotnet-members
  (cl-ll-names (dotcl:function-lambda-list #'dotnet:members))
  ("TYPE-OR-OBJECT" "&KEY" "KIND" "PREFIX" "STATIC" "INHERITED" "EXTENSIONS" "DOCUMENTATION"))

(deftest ext-ll-dotnet-macro
  (list (cl-ll-names (dotcl:function-lambda-list 'dotnet:->))
        (cl-ll-names (dotcl:function-lambda-list 'dotnet:handler-bind)))
  (("OBJECT" "&REST" "STEPS") ("CLAUSES" "&BODY" "BODY")))

(deftest ext-ll-dotcl-function
  (cl-ll-names (dotcl:function-lambda-list 'dotcl:make-thread))
  ("FUNCTION" "&KEY" "NAME"))

(deftest ext-ll-dotcl-macro
  (cl-ll-names (dotcl:function-lambda-list 'dotcl:atomic-incf))
  ("PLACE" "&OPTIONAL" "DIFF"))

(deftest ext-ll-mop-generic-function
  (cl-ll-names (dotcl:function-lambda-list 'dotcl-mop:slot-value-using-class))
  ("CLASS" "OBJECT" "SLOT"))

(deftest ext-ll-no-arguments
  (multiple-value-list (dotcl:function-lambda-list 'dotnet:null))
  (nil t))

(deftest ext-ll-simd
  (list (cl-ll-names (dotcl:function-lambda-list (find-symbol "F32.4+" "DOTCL-SIMD")))
        (length (dotcl:function-lambda-list (find-symbol "MAKE-U8.16" "DOTCL-SIMD")))
        (cl-ll-names (dotcl:function-lambda-list (find-symbol "F64.2-AREF" "DOTCL-SIMD"))))
  (("A" "B") 16 ("ARRAY" "&REST" "SUBSCRIPTS")))

;;; The names are uninterned unless they are COMMON-LISP symbols.

(deftest ext-ll-names-intern-nothing
  (let ((ll (dotcl:function-lambda-list 'dotnet:invoke)))
    (list (symbol-package (first ll))
          (find-symbol "MEMBER-NAME" "DOTNET")
          (find-symbol "MEMBER-NAME" "DOTCL-INTERNAL")))
  (nil nil nil))
