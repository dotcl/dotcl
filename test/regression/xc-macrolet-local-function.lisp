;;; While cross-compiling, a MACROLET expansion that names an enclosing FLET
;;; function or lexical variable refers to that binding.
;;;
;;; Bug: the analysis walk makes each MACROLET expander with EVAL, and EVAL under
;;; the cross-compile flag writes a quoted symbol out by name and interns it again
;;; (into the core package), as it must for code going into a .sil file. The
;;; expander is not such code: it runs at compile time and returns its expansion
;;; to the compiler. Its '(list-size ...) therefore came back as another symbol,
;;; the call missed the FLET binding, and the self-hosted compiler (loop.lisp's
;;; ESTIMATE-CODE-SIZE-1) called an undefined global LIST-SIZE.

(defun xmlf-compile-cross (form)
  "The instruction list the compiler produces for FORM in cross-compile mode,
as a string."
  (let ((flag (find-symbol "*CROSS-COMPILING*" "DOTCL-INTERNAL"))
        (compile-toplevel (find-symbol "COMPILE-TOPLEVEL" "DOTCL.CIL-COMPILER")))
    (progv (list flag) '(t)
      (prin1-to-string (funcall compile-toplevel form)))))

(defun xmlf-global-ref-p (name form)
  "T when FORM, compiled in cross-compile mode, loads the global symbol NAME."
  (and (search (format nil "(:LOAD-SYM ~s)" name) (xmlf-compile-cross form)) t))

;;; loop.lisp's shape: FLET, then a MACROLET whose backquoted expansion calls it.
(deftest xc-macrolet-local-function.backquote
  (xmlf-global-ref-p
   "XMLF-SIZE"
   '(defun xmlf-probe-1 (x)
      (flet ((xmlf-size (l) (length l)))
        (let ((n 0))
          (macrolet ((f (overhead &optional (args nil args-p))
                       `(the fixnum (+ (the fixnum ,overhead)
                                       (the fixnum (xmlf-size ,(if args-p args '(cdr x))))))))
            (if (consp x) (f n) (f 3 x)))))))
  nil)

(deftest xc-macrolet-local-function.quote
  (xmlf-global-ref-p
   "XMLF-SIZE"
   '(defun xmlf-probe-2 (x)
      (flet ((xmlf-size (l) (length l)))
        (macrolet ((f () '(xmlf-size x)))
          (f)))))
  nil)

;;; A lexical variable named by the expansion is the same binding, not a
;;; dynamic reference to a global of that name.
(deftest xc-macrolet-local-function.variable
  (xmlf-global-ref-p
   "XMLF-VAR"
   '(defun xmlf-probe-3 (xmlf-var)
      (macrolet ((f () 'xmlf-var))
        (f))))
  nil)

;;; The same forms still run correctly when compiled normally.
(deftest xc-macrolet-local-function.runs
  (flet ((xmlf-size (l) (length l)))
    (let ((x '(a b c)))
      (macrolet ((f (overhead) `(+ ,overhead (xmlf-size (cdr x)))))
        (f 10))))
  12)
