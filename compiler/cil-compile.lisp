;;; cil-compile.lisp — Portable driver for Lisp CIL compiler (A2)
;;;
;;; Usage:
;;;   DOTCL_INPUTS="f1.lisp f2.lisp" DOTCL_OUTPUT="out.sil" sbcl --load cil-compile.lisp
;;;   DOTCL_INPUTS="f1.lisp f2.lisp" DOTCL_OUTPUT="out.sil" ros run --load cil-compile.lisp
;;;
;;; Environment variables:
;;;   DOTCL_INPUTS  — space-separated list of input .lisp files
;;;   DOTCL_OUTPUT  — output file path

;; In dotcl, the compiler is already loaded from the core (.sil file).
;; Reloading would create symbol identity conflicts between the cross-compiled
;; code (DOTCL-INTERNAL symbols) and the re-loaded code (DOTCL.CIL-COMPILER symbols).
#-dotcl
(let ((dir (directory-namestring *load-pathname*)))
  (load (merge-pathnames "cil-compiler.lisp" dir))
  (load (merge-pathnames "cil-macros.lisp" dir))
  #+sbcl (sb-ext:unlock-package :cl)
  (load (merge-pathnames "loop.lisp" dir))
  (load (merge-pathnames "cil-analysis.lisp" dir))
  (load (merge-pathnames "cil-forms.lisp" dir)))

(defpackage :dotcl.cil-compile
  (:use :cl :dotcl.cil-compiler))
(in-package :dotcl.cil-compile)

;;; SBCL quasiquote preprocessing
#+sbcl
(defun sbcl-quasiquote-p (form)
  (and (consp form)
       (symbolp (car form))
       (string= (symbol-name (car form)) "QUASIQUOTE")
       (let ((pkg (symbol-package (car form))))
         (and pkg (member (package-name pkg) '("SB-INT" "SB-IMPL") :test #'string=)))))

#+sbcl
(defun preprocess-sbcl-quasiquotes (form)
  (cond
    ((atom form) form)
    ((sbcl-quasiquote-p form)
     (preprocess-sbcl-quasiquotes (macroexpand-1 form)))
    (t
     (let ((result '())
           (current form))
       (loop while (consp current)
             do (push (preprocess-sbcl-quasiquotes (car current)) result)
                (setf current (cdr current)))
       (let ((proper-list (nreverse result)))
         (if (null current)
             proper-list
             (let ((last-cons (last proper-list)))
               (if last-cons
                   (progn (rplacd last-cons (preprocess-sbcl-quasiquotes current))
                          proper-list)
                   (preprocess-sbcl-quasiquotes current)))))))))

#-sbcl
(defun preprocess-sbcl-quasiquotes (form) form)

(defun write-instrs (instrs stream)
  (let ((*print-pretty* t)
        (*print-right-margin* 200))
    (prin1 instrs stream)
    (terpri stream)))

(defun read-all-forms (filename)
  "Read all forms from file, processing in-package and defpackage at read time
   so that symbols are interned in the correct packages."
  (let ((forms '()))
    (with-open-file (in filename)
      (handler-case
          (loop (let ((form (read in)))
                  ;; Process package-setting forms at read time so subsequent
                  ;; reads intern symbols in the correct package.
                  (when (consp form)
                    (case (car form)
                      (defpackage (eval form))
                      (in-package (eval form))
                      ;; eval-when :compile-toplevel/:execute: run immediately so that
                      ;; subsequent reads see any new packages/macros.
                      (eval-when
                       (when (or (member :compile-toplevel (cadr form))
                                 (member :execute (cadr form)))
                         (eval `(progn ,@(cddr form)))))))
                  (push form forms)))
        (end-of-file () nil)))
    (nreverse forms)))

(defun split-spaces (s)
  "Split string S on spaces, returning list of non-empty substrings."
  (let ((result '()) (start 0) (len (length s)))
    (loop for i from 0 to len
          do (when (or (= i len) (char= (char s i) #\Space))
               (when (> i start)
                 (push (subseq s start i) result))
               (setf start (1+ i))))
    (nreverse result)))

(defun portable-getenv (name)
  #+dotcl (dotcl:getenv name)
  #-dotcl (uiop:getenv name))

(defun portable-quit (code)
  #+dotcl (dotcl:quit code)
  #-dotcl (uiop:quit code))

(let* ((inputs-env (portable-getenv "DOTCL_INPUTS"))
       (output-env (portable-getenv "DOTCL_OUTPUT")))
  (unless (and inputs-env output-env
               (> (length inputs-env) 0)
               (> (length output-env) 0))
    (format *error-output* "Error: set DOTCL_INPUTS and DOTCL_OUTPUT environment variables.~%")
    (format *error-output* "Usage: DOTCL_INPUTS=\"f1.lisp f2.lisp\" DOTCL_OUTPUT=\"out.sil\" ~A --load ~A~%"
            #+sbcl "sbcl" #+dotcl "dotcl" #-(or sbcl dotcl) "lisp"
            *load-pathname*)
    (portable-quit 1))
  (let* ((input-files (split-spaces inputs-env))
         (output-file output-env)
         (forms '()))
    (dolist (input-file input-files)
      (setf forms (append forms (read-all-forms input-file))))
    (setf forms (mapcar #'preprocess-sbcl-quasiquotes forms))
    ;; If we're building a SIL that includes cil-stdlib.lisp (i.e. the canonical
    ;; cross-compile of the dotcl core), append a runtime form that locks the
    ;; CL package after stdlib load completes (#93 step 3a). The form is built
    ;; with intern/find-package so it doesn't reference the DOTCL package at
    ;; read time on the SBCL host.
    (when (some (lambda (p) (search "cil-stdlib" (namestring p))) input-files)
      (setf forms (append forms
                          '((funcall (symbol-function
                                       (intern "LOCK-PACKAGE" (find-package "DOTCL")))
                                     "COMMON-LISP")))))
    (setf *cross-compiling* t)
    (let* ((expr (if (= (length forms) 1)
                     (first forms)
                     `(progn ,@forms)))
           (instrs (compile-toplevel expr)))
      (if (string= output-file "/dev/stdout")
          (write-instrs instrs *standard-output*)
          (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede)
            (write-instrs instrs out)))
      (format t "dotcl-a2: ~{~a~^ + ~} -> ~a~%" input-files output-file)))
  (portable-quit 0))
