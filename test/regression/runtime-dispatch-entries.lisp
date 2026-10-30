;;; Direct runtime entries and the nested-DEFUN registration fallback.

(setf dotcl:*save-sil* t)

(defun %rde-copy-1 (s) (copy-symbol s))
(defun %rde-copy-2 (s) (copy-symbol s t))
(defun %rde-author (p) (file-author p))
(defun %rde-wdate (p) (file-write-date p))
(defun %rde-used-by (p) (package-used-by-list p))
(defun %rde-left-trim (bag s) (string-left-trim bag s))
(defun %rde-write-line (s stream) (write-line s stream :start 1 :end 3))

(defun %rde-register-fallback ()
  (defun (setf #:rde-g) (x) x))

(defun %rde-restart (args)
  (restart-case (apply #'invoke-restart 'rde args)
    (rde (&rest r &key a &allow-other-keys) (list r a))))

(progv (list 'dotcl.cil-compiler::*cross-compiling* 'dotcl-internal::*cross-compiling*)
    '(t t)
  (eval '(defun %rde-path-parts () '(#p"rde/x.txt"))))

(defun %rde-has (fn text)
  (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest runtime-dispatch-entries.values
  (list
   (let* ((s (make-symbol "RDE-SYM")))
     (setf (get s 'p) 1)
     (let ((a (%rde-copy-1 s)) (b (%rde-copy-2 s)))
       (list (symbol-name a) (symbol-package a) (eq a s) (get a 'p)
             (symbol-name b) (symbol-package b) (eq b s) (get b 'p))))
   (let ((f (regression-temp-file "rde-probe.txt")))
     (with-open-file (out f :direction :output :if-exists :supersede)
       (write-line "probe" out))
     (unwind-protect
         (list (or (null (%rde-author f)) (stringp (%rde-author f)))
               (integerp (%rde-wdate f))
               (< 0 (%rde-wdate f) (+ (get-universal-time) 86400)))
       (delete-file f)))
   (let* ((a (make-package "RDE-PK-A" :use nil))
          (b (make-package "RDE-PK-B" :use nil))
          (c (make-package "RDE-PK-C" :use nil)))
     (unwind-protect
         (progn
           (use-package a b)
           (use-package a c)
           (list (sort (mapcar #'package-name (%rde-used-by a)) #'string<)
                 (sort (mapcar #'package-name (%rde-used-by "RDE-PK-A")) #'string<)))
       (dolist (p (list b c a)) (ignore-errors (delete-package p)))))
   (list (%rde-restart '(:a 1 :b 2)) (%rde-restart '()) (%rde-restart '(:x 9 :a 3)))
   (list (%rde-left-trim "ab" "abcabx")
         (%rde-left-trim '(#\a #\b) "bbac")
         (%rde-left-trim "x" "abc"))
   (string= (with-output-to-string (s) (%rde-write-line "hello" s))
            (concatenate 'string "el" (string #\Newline)))
   (let ((p (car (%rde-path-parts))))
     (list (pathnamep p) (pathname-name p) (pathname-type p)
           (pathname-directory p))))
  (("RDE-SYM" NIL NIL NIL "RDE-SYM" NIL NIL 1)
   (t t t)
   (("RDE-PK-B" "RDE-PK-C") ("RDE-PK-B" "RDE-PK-C"))
   (((:a 1 :b 2) 1) (NIL NIL) ((:x 9 :a 3) 3))
   ("cabx" "c" "abc")
   t
   (t "x" "txt" (:relative "rde"))))

(deftest-compiled-only runtime-dispatch-entries.register-fallback
  (symbolp (%rde-register-fallback))
  t)

(deftest runtime-dispatch-entries.errors
  (list (signals-error (%rde-copy-1 42) type-error)
        (signals-error (with-output-to-string (s) (write-line "x" s :start))
                       program-error))
  (t t))

(deftest-emitting-only runtime-dispatch-entries.shape
  (list (%rde-has #'%rde-copy-1 "Runtime.CopySymbol)")
        (%rde-has #'%rde-copy-2 "Runtime.CopySymbolFull")
        (%rde-has #'%rde-author "Runtime.FileAuthor")
        (%rde-has #'%rde-wdate "Runtime.FileWriteDate")
        (%rde-has #'%rde-used-by "Runtime.PackageUsedByList")
        (%rde-has #'%rde-restart "Runtime.CollectRestArgs")
        (%rde-has #'%rde-restart "Runtime.FindKeyArg")
        (%rde-has #'%rde-left-trim "Runtime.StringLeftTrim")
        (%rde-has #'%rde-write-line "Runtime.WriteLine)")
        (%rde-has #'%rde-register-fallback "CilAssembler.RegisterFunction)"))
  (t t t t t t t t t t))

(deftest-compiled-only runtime-dispatch-entries.pathname-parts-shape
  (%rde-has #'%rde-path-parts "Runtime.MakePathnameFromParts")
  t)
