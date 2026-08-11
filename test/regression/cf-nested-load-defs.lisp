;;; compile-file's post-compile strip (ANSI 3.2.3.1: compile-time defuns of
;;; the file must not stay fbound) must NOT wipe definitions that a nested
;;; LOAD brought in during the compile — those are real global side effects
;;; of loading OTHER files. Restoring *modules* alone did not help the
;;; (require ...) case: ASDF tracks completed systems independently, so the
;;; later fasl-load's require was a no-op and e.g. micros' backend was left
;;; calling an undefined make-lock.

(deftest-compiled-only compile-file-keeps-nested-load-defs
  (let ((lib "cfk-lib-tmp.lisp")
        (src "cfk-src-tmp.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s lib :direction :output :if-exists :supersede)
            (write-string "(defun cfk-loaded-fn () :loaded)" s))
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
            (format s "  (load ~s))~%" lib)
            (format s "(defun cfk-own-fn () :own)~%"))
          (compile-file src :output-file "cfk-src-tmp.fasl")
          (list (and (fboundp 'cfk-loaded-fn) t)
                ;; the compiled file's own defun must still be stripped
                (fboundp 'cfk-own-fn)))
      (ignore-errors (delete-file lib))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file "cfk-src-tmp.fasl"))
      (fmakunbound 'cfk-loaded-fn)))
  (t nil))
