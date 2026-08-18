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

;; Self-host: in a running dotcl the compiler's names are split across two
;; packages. Its functions are registered under the source package name
;; (DOTCL.CIL-COMPILER), but every symbol the compiled code handles as data — a
;; special variable it reads, a key it looks up — resolves through Startup.Sym,
;; which interns into DOTCL-INTERNAL. Reading the compiler's own sources back in
;; produces the DOTCL.CIL-COMPILER half, so a form like (%getenv "X") is a
;; different symbol from the DOTCL-INTERNAL::%GETENV that keys the intrinsic
;; table, and compiles to a call to a function that does not exist.
;;
;; Bridge the halves before anything is read: for each name, keep whichever
;; symbol actually carries a binding and make the other package see that same
;; object. A name that already carries a function or value in the source package
;; (COMPILE-EXPR and the rest of the compiler) is left alone — that is where its
;; definition lives.
#+dotcl
(let ((core (find-package "DOTCL-INTERNAL"))
      (src (find-package "DOTCL.CIL-COMPILER")))
  (when (and core src (not (eq core src)))
    (let ((pending '()))
      (do-symbols (s core)
        (when (eq (symbol-package s) core)
          (push s pending)))
      (dolist (s pending)
        (multiple-value-bind (dup status) (find-symbol (symbol-name s) src)
          (cond ((null dup) (import s src))
                ((eq dup s))
                ((or (fboundp dup) (boundp dup)))
                (t
                 ;; IMPORT only makes a symbol present, so an external name has
                 ;; to be re-exported or packages that :USE this one would intern
                 ;; their own copy and collide with the later DEFPACKAGE.
                 (unintern dup src)
                 (import s src)
                 (when (eq status :external) (export s src)))))))))

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

(defparameter *default-segment-forms* 32
  "Number of top-level forms per emitted segment (one loader method each).
   Empirical plateau: with the six core sources, peak working set while loading
   dotcl.core is 68 MB at one segment per file, 59 MB at 8-64 forms per segment,
   and 68 MB again at one form per segment (per-segment overhead takes over).
   Override with DOTCL_SEG_FORMS to re-run that sweep; 0 means one per file.")

(defun chunk-list (list n)
  "Split LIST into consecutive chunks of at most N elements."
  (let ((out '()) (cur '()) (k 0))
    (dolist (x list)
      (push x cur)
      (when (>= (incf k) n)
        (push (nreverse cur) out)
        (setf cur '() k 0)))
    (when cur (push (nreverse cur) out))
    (nreverse out)))

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
         ;; One entry per input file, in order. Every file is READ before any is
         ;; compiled, exactly as when the whole core was one form: reading evals
         ;; defpackage / in-package / eval-when, and moving those evals after an
         ;; earlier file's compilation would change what they see.
         (groups (mapcar (lambda (f)
                           (mapcar #'preprocess-sbcl-quasiquotes (read-all-forms f)))
                         input-files)))
    ;; If we're building a SIL that includes cil-stdlib.lisp (i.e. the canonical
    ;; cross-compile of the dotcl core), append a runtime form that locks the
    ;; CL package after stdlib load completes. The form is built
    ;; with intern/find-package so it doesn't reference the DOTCL package at
    ;; read time on the SBCL host.
    (when (some (lambda (p) (search "cil-stdlib" (namestring p))) input-files)
      (setf groups (append groups
                           '(((funcall (symbol-function
                                         (intern "LOCK-PACKAGE" (find-package "DOTCL")))
                                       "COMMON-LISP"))))))
    (setf *cross-compiling* t)
    ;; Self-host: the compiler that does the work is the one already in the
    ;; image (package DOTCL-INTERNAL), not the sources this driver's package
    ;; names. Setting only our own flag leaves the real compiler in run-time
    ;; mode, where it plants load-time xref registrations and keeps literals as
    ;; live objects — neither of which a .sil file can carry.
    #+dotcl
    (let* ((pkg (find-package "DOTCL-INTERNAL"))
           (sym (and pkg (find-symbol "*CROSS-COMPILING*" pkg))))
      (when sym (setf (symbol-value sym) t)))
    ;; Compile the core in bounded units and join the instruction lists with
    ;; (:TOPLEVEL-BOUNDARY). Loaders split there and run each segment as its own
    ;; method, so the core's top-level code is many small methods instead of one
    ;; method for the whole core: each segment's forms are top-level forms (not
    ;; forms nested in one giant PROGN), locals/labels stay segment-scoped, and
    ;; the peak cost of assembling or JITting the toplevel is per segment.
    ;; Segments never span files, so file order and per-file scoping hold.
    ;; Loading order is unchanged — the segments run in sequence.
    (let* ((chunk (let ((s (portable-getenv "DOTCL_SEG_FORMS")))
                    (if (and s (> (length s) 0))
                        (parse-integer s)
                        *default-segment-forms*)))
           (instrs (let ((out '()) (first t))
                     (dolist (forms groups (apply #'append (nreverse out)))
                       (dolist (part (if (plusp chunk) (chunk-list forms chunk) (list forms)))
                         (when part
                           (let ((seg (compile-toplevel
                                       (if (= (length part) 1)
                                           (first part)
                                           `(progn ,@part)))))
                             (push (if first seg (cons '(:toplevel-boundary) seg)) out)
                             (setf first nil))))))))
      (if (string= output-file "/dev/stdout")
          (write-instrs instrs *standard-output*)
          (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede)
            (write-instrs instrs out)))
      (format t "dotcl-a2: ~{~a~^ + ~} -> ~a~%" input-files output-file)))
  (portable-quit 0))
