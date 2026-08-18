;;; Regression tests for string literals in compiled code.
;;;
;;; A .sil file is text, so a string literal reaches the assembler as
;;; (:ldstr s) (:newobj "LispString") — taken literally that builds a fresh
;;; LispString every time the expression is EVALUATED, not once per site. The
;;; literal is a constant (CLHS 3.7.1), so it is now built once: from the
;;; constant pool in the JIT path, and from a static field filled by the type
;;; initializer in a .fasl.
;;;
;;; What must not change is the value: a literal still reads as its own
;;; characters, still compares by content, and code that copies it before
;;; mutating still works. Coalescing equal literals is what a file compiler is
;;; allowed to do; these tests pin the observable behaviour, not the identity.

(defvar *fsl-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fsl-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fsl-compile-and-load (source-forms name)
  (let ((lisp (concatenate 'string *fsl-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (dolist (f source-forms) (write f :stream s :readably t) (terpri s)))
    (load (compile-file lisp))
    t))

;;; ---- the value survives the compile-file / load round trip ----

(deftest-compiled-only fsl-literal-value
  (progn
    (%fsl-compile-and-load
     '((in-package :cl-user)
       (defun %fsl-get () "hello")
       (defun %fsl-len () (length "hello"))
       (defun %fsl-match (x) (string= x "hello"))
       ;; Same literal in two functions: one shared object is fine, a wrong
       ;; one is not.
       (defun %fsl-other () "hello"))
     "fsl-value")
    (list (funcall (intern "%FSL-GET"))
          (funcall (intern "%FSL-LEN"))
          (funcall (intern "%FSL-MATCH") "hello")
          (funcall (intern "%FSL-MATCH") "nope")
          (equal (funcall (intern "%FSL-GET")) (funcall (intern "%FSL-OTHER")))))
  ("hello" 5 t nil t))

;;; ---- a literal read in a loop reads the same characters every time ----

(deftest-compiled-only fsl-literal-in-loop
  (progn
    (%fsl-compile-and-load
     '((in-package :cl-user)
       (defun %fsl-loop (n)
         (let ((hits 0))
           (dotimes (i n hits)
             (when (string= "abc" "abc") (incf hits))))))
     "fsl-loop")
    (funcall (intern "%FSL-LOOP") 100))
  100)

;;; ---- copying before mutating still yields an independent string ----

(deftest-compiled-only fsl-copy-then-mutate
  (progn
    (%fsl-compile-and-load
     '((in-package :cl-user)
       (defun %fsl-copy-mutate ()
         (let ((s (copy-seq "abc")))
           (setf (char s 0) #\X)
           (list s "abc"))))
     "fsl-copy")
    (funcall (intern "%FSL-COPY-MUTATE")))
  ("Xbc" "abc"))

;;; ---- literals inside quoted structure and as hash keys ----

(deftest-compiled-only fsl-literal-in-structure
  (progn
    (%fsl-compile-and-load
     '((in-package :cl-user)
       (defun %fsl-alist () '(("a" . 1) ("b" . 2)))
       (defun %fsl-lookup (k) (cdr (assoc k (%fsl-alist) :test #'string=))))
     "fsl-struct")
    (list (funcall (intern "%FSL-LOOKUP") "a")
          (funcall (intern "%FSL-LOOKUP") "b")
          (funcall (intern "%FSL-LOOKUP") "c")))
  (1 2 nil))

;;; ---- the same shapes evaluated (not compile-file'd) ----

(deftest fsl-eval-literal-value
  (let ((f (lambda () "hello")))
    (list (funcall f) (length (funcall f)) (string= (funcall f) "hello")))
  ("hello" 5 t))

(deftest fsl-eval-copy-then-mutate
  (let* ((s (copy-seq "abc")))
    (setf (char s 0) #\X)
    (list s "abc"))
  ("Xbc" "abc"))
