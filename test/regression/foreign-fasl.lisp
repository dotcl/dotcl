;;; .fasl is a shared extension: SBCL, CCL and ECL all write files with that
;;; name, and they turn up next to sources in quicklisp dists and in bundle
;;; directories. dotcl fasls are .NET assemblies, so such a file cannot be
;;; loaded here.
;;;
;;; Bug: LOAD dispatched on the PE header ("MZ"), so a foreign fasl missed the
;;; fasl branch and was read as source text, failing with a reader error about
;;; whatever byte came first. A PE file that is not a dotcl assembly reached
;;; Assembly.Load and threw a raw BadImageFormatException ("Bad IL format").
;;; Neither said "this is not a dotcl fasl".
;;;
;;; Fix: recognize a non-PE .fasl as a foreign fasl. An explicit LOAD signals a
;;; FILE-ERROR naming the file, and the module search (module-provide-contrib's
;;; .fasl -> .sil -> .lisp walk) warns and falls through to the source candidate
;;; instead of failing the whole REQUIRE.

(defun %ff-tmpdir (tag)
  (let ((dir (format nil "~a/dotcl-foreign-fasl-~a-~a/"
                     (or (dotcl:getenv "TEMP") "/tmp") tag
                     (get-internal-real-time))))
    (ensure-directories-exist dir)
    dir))

(defun %ff-write-bytes (path string)
  "Write STRING as raw bytes, so PATH is a binary file rather than Lisp source."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (loop for ch across string do (write-byte (char-code ch) s))))

(defun %ff-load-outcome (path)
  "LOAD PATH, reporting the condition class and whether it names PATH."
  (handler-case (progn (load path) :no-error)
    (file-error (e)
      (list :file-error
            (and (search "not a dotcl fasl" (princ-to-string e)) t)
            (equal (namestring (pathname (file-error-pathname e)))
                   (namestring (pathname path)))))
    (error (e) (list :other (type-of e)))))

;;; A foreign fasl handed to LOAD directly: FILE-ERROR that says so and carries
;;; the pathname, not a reader error on the first byte of the binary.
(defun %ff-foreign-case ()
  (let ((path (concatenate 'string (%ff-tmpdir "load") "sbcl.fasl")))
    ;; The header SBCL writes; any non-PE content exercises the same check.
    (%ff-write-bytes path "# FASL")
    (%ff-load-outcome path)))

(deftest foreign-fasl.explicit-load-signals-file-error
  (%ff-foreign-case)
  (:file-error t t))

;;; A PE image that is not a managed assembly (a native DLL, or a truncated
;;; file) used to surface as a bare BadImageFormatException.
(defun %ff-not-managed-case ()
  (let ((path (concatenate 'string (%ff-tmpdir "native") "native.fasl")))
    (%ff-write-bytes path "MZ not really a portable executable")
    (%ff-load-outcome path)))

(deftest foreign-fasl.non-managed-pe-signals-file-error
  (%ff-not-managed-case)
  (:file-error t t))

;;; Module search: a foreign fasl parked where dotcl's would be must not fail
;;; the REQUIRE. It is skipped with a warning and the .lisp candidate loads.
(defun %ff-module-fallback-case ()
  (let* ((root (%ff-tmpdir "contrib"))
         (moddir (concatenate 'string root "ffmod/"))
         (paths (dotnet:static "DotCL.Runtime" "ContribExtraSearchPaths")))
    (ensure-directories-exist moddir)
    (%ff-write-bytes (concatenate 'string moddir "ffmod.fasl") "# FASL")
    (with-open-file (s (concatenate 'string moddir "ffmod.lisp")
                       :direction :output :if-exists :supersede)
      (write-string "(defparameter cl-user::*ff-module-loaded* :from-source)" s)
      (terpri s))
    (dotnet:invoke paths "Add" root)
    (unwind-protect
         (progn (require "ffmod")
                (list (and (member "ffmod" *modules* :test #'string=) t)
                      (and (boundp 'cl-user::*ff-module-loaded*)
                           (symbol-value 'cl-user::*ff-module-loaded*))))
      (dotnet:invoke paths "Remove" root))))

(deftest foreign-fasl.module-search-falls-through-to-source
  (%ff-module-fallback-case)
  (t :from-source))
