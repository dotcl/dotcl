;;; COMPILE-FILE evaluates a top level DEFUN early, so that a macro later in
;;; the same file can call the function while the file is compiled. That
;;; definition must not outlive the compilation (CLHS 3.2.3.1 gives a plain
;;; DEFUN no compile-time effect). A name that was not fbound before was
;;; already cleaned up afterwards; a name that WAS kept the new definition, so
;;; compiling a file replaced a live function before the fasl was loaded.
;;; hu.dwim.def redefines its namespace lock functions in a later file and runs
;;; code in between that needs the old ones: "ACQUIRE-LOCK: requires a lock".

(defun cfedr-f () :old)
(defun cfedr-call () (cfedr-f))

(deftest-compiled-only cf-early-defun-restore.redefinition-stays-out
  (let ((src "cfedr-tmp.lisp")
        (fasl "cfedr-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(defun cfedr-f () :new)" s)
             (write-line "(defmacro cfedr-m () (list 'quote (cfedr-f)))" s)
             (write-line "(defparameter *cfedr-seen* (cfedr-m))" s))
           (compile-file src :output-file fasl)
           (let ((after-compile (list (cfedr-f) (cfedr-call))))
             (load fasl)
             (list after-compile
                   (list (cfedr-f) (cfedr-call) (symbol-value (intern "*CFEDR-SEEN*"))))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  ((:old :old) (:new :new :new)))

;; An explicit :COMPILE-TOPLEVEL redefinition is real and stays.
(defun cfedr-g () :old)

(deftest-compiled-only cf-early-defun-restore.compile-toplevel-stays
  (let ((src "cfedr2-tmp.lisp")
        (fasl "cfedr2-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(eval-when (:compile-toplevel :load-toplevel :execute)" s)
             (write-line "  (defun cfedr-g () :new))" s))
           (compile-file src :output-file fasl)
           (cfedr-g))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  :new)
