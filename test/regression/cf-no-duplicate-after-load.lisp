;;; COMPILE-FILE of a file that was LOADed first reports no "Duplicate definition":
;;; a DEFUN that mentions a macro the file defines later is compiled again when the
;;; DEFMACRO is reached, for its own form, not as a second definition.

(defun cfnd-compile (name forms)
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (load src)
    (let ((*error-output* (make-broadcast-stream)))
      (multiple-value-list (compile-file src :output-file fasl)))))

(deftest-compiled-only compile-file-no-duplicate-after-load.macro-after-defun
  (let ((r (cfnd-compile "cfnd-1"
                         '((defun cfnd-p (b) (case (length b) (1 `(cfnd-sl ,b)) (t nil)))
                           (defmacro cfnd-sl (b) (cfnd-p b))))))
    (list (and (first r) t) (second r) (third r)))
  (t nil nil))

;; Two DEFUNs of one name in a file are still a duplicate.
(deftest-compiled-only compile-file-no-duplicate-after-load.real-duplicate
  (let ((r (cfnd-compile "cfnd-2"
                         '((defun cfnd-q (b) (list b))
                           (defun cfnd-q (b) (list b b))))))
    (third r))
  t)

;; A DEFUN that a macro expands into is forced for the form the file had, not for its own subform.
(deftest-compiled-only compile-file-no-duplicate-after-load.defun-from-macro
  (let ((r (cfnd-compile "cfnd-3"
                         '((defmacro cfnd-gen (n)
                             `(defun ,n (b) (case (length b) (1 `(cfnd-m3 ,b)) (t nil))))
                           (cfnd-gen cfnd-r)
                           (defmacro cfnd-m3 (b) (cfnd-r b))))))
    (list (and (first r) t) (second r) (third r)))
  (t nil nil))

;; A DEFUN that a macro expansion calls before the file reaches it (here the expander runs while
;; another DEFUN's body is scanned) is compiled for its own form, not as a duplicate.
(deftest-compiled-only compile-file-no-duplicate-after-load.called-by-expander
  (let ((r (cfnd-compile "cfnd-4"
                         '((defmacro cfnd-note (&body args) (cfnd-expand nil args))
                           (defun cfnd-expand (p args) (declare (ignorable p)) `(progn ,@args))
                           (defun cfnd-user (x) (let ((cfnd-note x)) (cond (cfnd-note (assert t) cfnd-note) (t nil))))))))
    (list (and (first r) t) (second r) (third r)))
  (t nil nil))
