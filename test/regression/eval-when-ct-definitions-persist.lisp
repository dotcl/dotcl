;;; What (eval-when (:compile-toplevel ...) ...) defines stays defined after
;;; COMPILE-FILE returns.
;;;
;;; COMPILE-FILE evaluates a plain top-level DEFUN early so later macros in the
;;; same file can call it, and removes those definitions afterwards: CLHS
;;; 3.2.3.1 does not let a plain DEFUN reach the global environment at compile
;;; time. The removal took everything that had become fbound during the
;;; compilation, including what an explicit :COMPILE-TOPLEVEL evaluation had
;;; defined. That evaluation is real (CLHS 3.2.3.1: "evaluate", as EVAL would),
;;; and SBCL keeps its definitions.
;;;
;;; fare-memoization depends on it: it memoizes MEMOIZED-FUNCALL at compile
;;; time, which records the wrapper in the symbol's plist and installs it as the
;;; function. The removal took the function but not the plist, so at load time
;;; UNMEMOIZE found the record, asked for the function, and got "Undefined
;;; function: MEMOIZED-FUNCALL".

(deftest-compiled-only eval-when-ct-definitions.survive-compile-file
  (let ((src "ewct-tmp.lisp")
        (fasl "ewct-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(eval-when (:compile-toplevel :load-toplevel :execute)" s)
             (write-line "  (defun ewct-kept (x) (list :kept x)))" s)
             (write-line "(defun ewct-plain () 1)" s))
           (compile-file src :output-file fasl)
           (list (and (fboundp (intern "EWCT-KEPT")) t)
                 (and (fboundp (intern "EWCT-PLAIN")) t)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (ignore-errors (fmakunbound (intern "EWCT-KEPT")))))
  (t nil))

;; The fare-memoization shape: compile time wraps the function and records the
;; wrapper; load time unwraps it through the record, redefines and rewraps.
(deftest-compiled-only eval-when-ct-definitions.wrap-at-compile-time-then-load
  (let ((src "ewct2-tmp.lisp")
        (fasl "ewct2-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(eval-when (:compile-toplevel :load-toplevel :execute)" s)
             (write-line "  (defun ewct2-unwrap (s)" s)
             (write-line "    (let ((info (get s 'ewct2-info)))" s)
             (write-line "      (when info" s)
             (write-line "        (assert (eq (cdr info) (symbol-function s)))" s)
             (write-line "        (setf (symbol-function s) (car info))" s)
             (write-line "        (remprop s 'ewct2-info))))" s)
             (write-line "  (defun ewct2-wrap (s)" s)
             (write-line "    (let* ((f (symbol-function s))" s)
             (write-line "           (w (lambda (&rest a) (cons :w (apply f a)))))" s)
             (write-line "      (setf (symbol-function s) w (get s 'ewct2-info) (cons f w))))" s)
             (write-line "  (ewct2-unwrap 'ewct2-f)" s)
             (write-line "  (defun ewct2-f (x) (list x))" s)
             (write-line "  (ewct2-wrap 'ewct2-f))" s))
           (compile-file src :output-file fasl)
           (load fasl)
           (funcall (intern "EWCT2-F") 3))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (:w 3))
