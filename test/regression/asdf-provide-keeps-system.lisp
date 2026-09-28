;;; A file that PROVIDEs the name of the ASDF system it belongs to must not
;;; unregister that system while it is being compiled.
;;;
;;; Bug: COMPILE-FILE runs a toplevel PROVIDE at compile time, and its cleanup
;;; called ASDF:CLEAR-SYSTEM on every module name that appeared in *MODULES*
;;; during the compile. That cleanup exists for modules a compile-time REQUIRE
;;; loaded, but it also caught the file's own (provide :x). sly's contrib files
;;; end in (provide :slynk/mrepl) etc., so compiling slynk/mrepl removed the
;;; slynk/mrepl system from ASDF's registry in the middle of building it, and a
;;; system that depends on it failed with Component "slynk/mrepl" not found.
;;; Only on the first build: a cached fasl is loaded, not compiled.
;;;
;;; Fix: the cleanup clears only the names a REQUIRE added during the compile.
;;; The compile-time REQUIRE case (asdf-registry-invalidation.lisp) still clears.

(defun %cfpk-load (system-name file-name)
  "Write a one-file system SYSTEM-NAME (a secondary system of cfpk-main) whose
file ends in (provide SYSTEM-NAME), build it from source, and report whether
ASDF still has it registered."
  (require "asdf")
  (let* ((tmp (format nil "~a/dotcl-cfpk-~a/"
                      (regression-temp-dir) (get-internal-real-time)))
         (dir (substitute #\/ #\\ tmp)))
    (ensure-directories-exist dir)
    (with-open-file (s (concatenate 'string dir "cfpk-main.asd")
                       :direction :output :if-exists :supersede)
      (format s "(defsystem \"cfpk-main\")~%(defsystem ~s :components ((:file ~s)))~%"
              system-name file-name))
    (with-open-file (s (concatenate 'string dir file-name ".lisp")
                       :direction :output :if-exists :supersede)
      (format s "(defun cfpk-probe () :ok)~%(provide ~s)~%" system-name))
    (let ((registry (read-from-string "asdf:*central-registry*")))
      (eval `(pushnew ,dir ,registry :test #'equal)))
    (let ((clear (find-symbol "CLEAR-SYSTEM" "ASDF"))
          (load-system (find-symbol "LOAD-SYSTEM" "ASDF"))
          (registered (find-symbol "REGISTERED-SYSTEM" "ASDF")))
      (funcall clear "cfpk-main")
      (funcall clear system-name)
      (funcall load-system system-name :force t)
      (list (and (funcall registered system-name) t)
            (funcall 'cfpk-probe)))))

(deftest-compiled-only compile-file-provide-keeps-secondary-system
  (%cfpk-load "cfpk-main/sub" "cfpk-sub")
  (t :ok))
