;;; A COMPILE-FILE that reports failure-p stops an ASDF build, as on SBCL.
;;;
;;; UIOP's default for *COMPILE-FILE-FAILURE-BEHAVIOUR* is :error only on SBCL
;;; and MKCL and :warn elsewhere, so on dotcl a file whose compilation signalled
;;; a full WARNING was loaded anyway and the build went on. The first error seen
;;; was then a later one (a package the failed file should have made), not the
;;; cause. REQUIRE "asdf" now sets the variable to :error.
;;;
;;; Hermetic: writes temp .asd + source files into a temp dir and pushes it to
;;; asdf:*central-registry*. asdf symbols are reached via read-from-string so
;;; loading this file does not require asdf up front.

(defun %cffe-load (name body)
  "Write system NAME with one file holding BODY, load it with ASDF, and return
:LOADED, or the name of the condition type that stopped the load."
  (require "asdf")
  (let* ((dir (format nil "~a/dotcl-cffe-~a-~a/"
                      (regression-temp-dir) name (get-internal-real-time)))
         (asd (concatenate 'string dir name ".asd"))
         (src (concatenate 'string dir name ".lisp")))
    (ensure-directories-exist dir)
    (with-open-file (s asd :direction :output :if-exists :supersede)
      (format s "(defsystem ~s :components ((:file ~s)))" name name))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string body s))
    (eval (read-from-string
           (format nil "(pushnew ~s asdf:*central-registry* :test #'equal)" dir)))
    (unwind-protect
         (handler-case
             (progn
               (funcall (read-from-string "asdf:load-system") name :force t)
               :loaded)
           (error (c)
             (let ((cfe (find-symbol "COMPILE-FILE-ERROR" "UIOP")))
               (if (and cfe (typep c cfe))
                   :compile-file-error
                   (type-of c)))))
      (funcall (read-from-string "asdf:clear-system") name))))

(deftest asdf-compile-file-failure-behaviour-is-error
  (progn (require "asdf")
         (symbol-value (find-symbol "*COMPILE-FILE-FAILURE-BEHAVIOUR*" "UIOP")))
  :error)

;;; An emit-free build has no COMPILE-FILE behind ASDF: LOAD-SYSTEM loads the
;;; sources (LOAD-SOURCE-OP), so there is no failure-p to act on.
(deftest-emitting-only asdf-full-warning-stops-build
  (%cffe-load "cffe-warn"
              "(eval-when (:compile-toplevel) (warn \"cffe: full warning\"))
               (defun cffe-warn-fn () 1)")
  :compile-file-error)

(deftest asdf-style-warning-does-not-stop-build
  (%cffe-load "cffe-style"
              "(eval-when (:compile-toplevel) (warn 'style-warning))
               (defun cffe-style-fn () 1)")
  :loaded)

;;; OPERATE first upgrades ASDF by loading the system "asdf". With an asdf.asd
;;; in the registry (an ASDF checkout under a :tree source registry), that
;;; system comes from the .asd, and loading it compiles ASDF from source. An
;;; emit-free build cannot compile, so every LOAD-SYSTEM failed with
;;; PROGRAM-ERROR from COMPILE-FILE. It keeps the running ASDF instead. The
;;; fake asdf.asd here signals an error if it is ever loaded. Not run on a
;;; build with a compiler, where ASDF would really upgrade to the fake.
(unless (find :dotcl-emit *features*)
  (deftest asdf-emit-free-does-not-upgrade-asdf
      (let ((dir (format nil "~a/dotcl-cffe-fake-asdf-~a/"
                         (regression-temp-dir) (get-internal-real-time))))
        (require "asdf")
        (ensure-directories-exist dir)
        (with-open-file (s (concatenate 'string dir "asdf.asd")
                           :direction :output :if-exists :supersede)
          (write-string "(defsystem \"asdf\" :version \"999\"
                           :components ((:file \"fake-asdf\")))" s))
        ;; ASDF takes the version of a registered ASDF from this file.
        (with-open-file (s (concatenate 'string dir "version.lisp-expr")
                           :direction :output :if-exists :supersede)
          (write-string "\"999\"" s))
        (with-open-file (s (concatenate 'string dir "fake-asdf.lisp")
                           :direction :output :if-exists :supersede)
          (write-string "(error \"fake asdf loaded\")" s))
        (let ((reg (find-symbol "*CENTRAL-REGISTRY*" "ASDF")))
          (push dir (symbol-value reg))
          (unwind-protect
               (%cffe-load "cffe-noupgrade" "(defun cffe-noupgrade-fn () 1)")
            (setf (symbol-value reg)
                  (remove dir (symbol-value reg) :test #'equal)))))
    :loaded))
