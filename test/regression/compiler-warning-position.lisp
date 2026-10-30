;;; A condition signalled while a file is compiled can say where it came from:
;;; the top-level form, and the subform when the reader recorded one.

(defun cfwp-write (name lines)
  (let ((src (regression-temp-file name)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (line lines) (write-line line s)))
    src))

(defun cfwp-collect-file (name lines)
  (let ((src (cfwp-write name lines))
        (fasl (regression-temp-file (concatenate 'string name ".fasl")))
        (warnings '()))
    (unwind-protect
         (progn
           (handler-bind ((warning (lambda (c)
                                     (push (cons (princ-to-string c)
                                                 (dotcl:compiler-condition-source c))
                                           warnings)
                                     (muffle-warning c))))
             (let ((*error-output* (make-broadcast-stream)))
               (compile-file src :output-file fasl)))
           (list (namestring (truename src)) (nreverse warnings)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl)))))

(defun cfwp-pick (warnings prefix)
  (find-if (lambda (w) (and (search prefix (car w)) w)) warnings))

(defun cfwp-field (warning key)
  (getf (cdr warning) key))

(defvar *cfwp-source*
  '("(defun cfwp-f ()"
    "  (cfwp-undefined-function)"
    "  (list cfwp-undefined-variable)"
    "  (let ((unused 1)) 2))"))

(deftest-emitting-only compiler-warning-position.compile-file
  (destructuring-bind (file warnings)
      (cfwp-collect-file "cfwp-pos.lisp" *cfwp-source*)
    (let ((fn (cfwp-pick warnings "undefined function: CFWP-UNDEFINED-FUNCTION"))
          (var (cfwp-pick warnings "undefined variable: CFWP-UNDEFINED-VARIABLE"))
          (unused (cfwp-pick warnings "The variable UNUSED is defined but never used.")))
      (list
       (and file t)
       (and fn (cfwp-field fn :line))
       (and fn (cfwp-field fn :subform-line))
       (and fn (cfwp-field fn :position))
       (and fn (cfwp-field fn :subform-position))
       (and var (cfwp-field var :line))
       (and var (cfwp-field var :subform-line))
       (and unused (cfwp-field unused :subform-line))
       (and fn (= (cfwp-field fn :position) 0)))))
  (t 1 2 0 19 1 3 4 t))

(deftest-emitting-only compiler-warning-position.compile
  (let ((warnings '()))
    (handler-bind ((warning (lambda (c)
                              (push (cons (princ-to-string c)
                                          (dotcl:compiler-condition-source c))
                                    warnings)
                              (muffle-warning c))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile nil '(lambda ()
                        (cfwp-compile-undefined)
                        (list cfwp-compile-variable)))))
    (list (and (cfwp-pick warnings "undefined function: CFWP-COMPILE-UNDEFINED") t)
          (and (cfwp-pick warnings "undefined variable: CFWP-COMPILE-VARIABLE") t)
          (and (cfwp-pick warnings "undefined function: CFWP-COMPILE-UNDEFINED")
               (not (null (cdr (cfwp-pick warnings "undefined function: CFWP-COMPILE-UNDEFINED")))))))
  (t t t))

(deftest-emitting-only compiler-warning-position.function-defined-later
  (let ((warnings (second (cfwp-collect-file
                           "cfwp-later.lisp"
                           '("(defun cfwp-later-a () (cfwp-later-b))"
                             "(defun cfwp-later-b () 1)")))))
    (and (notany (lambda (w) (search "undefined function" (car w))) warnings) t))
  t)

;;; LOAD runs in every mode, including the emit-free one, and keeps the
;;; context of the form being loaded while its code runs.
(deftest compiler-warning-position.load
  (let* ((src (cfwp-write "cfwp-load.lisp"
                          '(";; the warning is on the next line"
                            "(warn \"cfwp-load-warning\")")))
         (expected-file (namestring (truename src)))
         (seen nil)
         (reported-file nil))
    (unwind-protect
         (progn
           (handler-bind ((warning (lambda (c)
                                     (when (search "cfwp-load-warning" (princ-to-string c))
                                       (setf seen (dotcl:compiler-condition-source c)))
                                     (muffle-warning c))))
             (let ((*error-output* (make-broadcast-stream)))
               (load src)))
           (setf reported-file (and seen (getf seen :file)
                                    (namestring (truename (getf seen :file)))))
           (list (and reported-file (equal reported-file expected-file) t)
                 (and seen (getf seen :line))
                 (and seen (getf seen :subform-line))))
      (ignore-errors (delete-file src))))
  (t 2 2))

;;; As SBCL does, a compiler condition during LOAD names the file LOAD was
;;; given (*LOAD-PATHNAME*), not its true name. Loading through a directory
;;; link makes the two differ, which is the case that used to fail.
#-windows
(progn
  (defun %cfwp-link-setup ()
    (let* ((root (format nil "~a/cfwp-link" (regression-temp-dir)))
           (real (format nil "~a/real" root))
           (link (format nil "~a/link" root)))
      (ensure-directories-exist (format nil "~a/" real))
      (ignore-errors (dotnet:static "System.IO.Directory" "Delete" link))
      (dotnet:static "System.IO.Directory" "CreateSymbolicLink" link real)
      (values root real link)))

  (deftest compiler-warning-position.load-through-symlink
    (multiple-value-bind (root real link) (%cfwp-link-setup)
      (let* ((src (format nil "~a/w.lisp" real))
             (given (format nil "~a/w.lisp" link))
             (expected-file nil)
             (seen nil))
        (with-open-file (s src :direction :output :if-exists :supersede)
          (write-line "(warn \"cfwp-link-warning\")" s))
        (setf expected-file (namestring (truename src)))
        (unwind-protect
             (progn
               (handler-bind ((warning (lambda (c)
                                         (when (search "cfwp-link-warning"
                                                       (princ-to-string c))
                                           (setf seen (dotcl:compiler-condition-source c)))
                                         (muffle-warning c))))
                 (let ((*error-output* (make-broadcast-stream)))
                   (load given)))
               (list (and seen (equal (getf seen :file) given) t)
                     (and seen (getf seen :file)
                          (equal (namestring (truename (getf seen :file)))
                                 expected-file)
                          t)))
          (ignore-errors (dotnet:static "System.IO.Directory" "Delete" link))
          (ignore-errors (dotnet:static "System.IO.Directory" "Delete" root t)))))
    (t t)))
