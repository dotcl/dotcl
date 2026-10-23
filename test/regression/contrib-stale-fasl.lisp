;;; REQUIRE's module search (.fasl, .sil, .lisp) loaded a contrib's fasl even
;;; when the .lisp beside it had been edited since, so the old behavior ran.
;;; A .fasl / .sil older than its .lisp is now skipped and the source loads.

(defun %csf-setup (tag)
  (let* ((root (format nil "~a/dotcl-contrib-stale-fasl-~a-~a/"
                       (regression-temp-dir) tag (get-internal-real-time)))
         (moddir (concatenate 'string root tag "/")))
    (ensure-directories-exist moddir)
    (values root moddir)))

(defun %csf-write-source (path value)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s "(defparameter cl-user::*csf-module-value* ~s)~%" value)))

(defun %csf-touch-later (path)
  (dotnet:static "System.IO.File" "SetLastWriteTimeUtc" path
                 (dotnet:invoke (dotnet:static "System.DateTime" "UtcNow") "AddSeconds" 10)))

(defun %csf-require (root name)
  (let ((paths (dotnet:static "DotCL.Runtime" "ContribExtraSearchPaths")))
    (makunbound 'cl-user::*csf-module-value*)
    (dotnet:invoke paths "Add" root)
    (unwind-protect
         (let ((*error-output* (make-broadcast-stream)))
           (require name)
           (and (boundp 'cl-user::*csf-module-value*)
                (symbol-value 'cl-user::*csf-module-value*)))
      (dotnet:invoke paths "Remove" root))))

(defun %csf-case (tag newer-source-p)
  (multiple-value-bind (root moddir) (%csf-setup tag)
    (let ((src (concatenate 'string moddir tag ".lisp")))
      (%csf-write-source src :old)
      (let ((*error-output* (make-broadcast-stream))
            (*standard-output* (make-broadcast-stream)))
        (compile-file src))
      (%csf-write-source src :new)
      (if newer-source-p
          (%csf-touch-later src)
          (%csf-touch-later (concatenate 'string moddir tag ".fasl")))
      (%csf-require root tag))))

;;; Source edited after the fasl was built: the source is what loads.
(deftest-emitting-only contrib-stale-fasl.newer-source-wins
  (%csf-case "csfnew" t)
  :new)

;;; A fasl newer than its source is still the one loaded.
(deftest-emitting-only contrib-stale-fasl.fresh-fasl-is-used
  (%csf-case "csffresh" nil)
  :old)

;;; A fasl compiled against other runtime source (the second half of its
;;; generation stamp) may call members this runtime does not have. REQUIRE passes
;;; it over for the source or a later candidate; with neither, loading it is a
;;; FASL-VERSION-MISMATCH error. A fasl from another compiler only still loads.

(defun %csf-replace-once (path from to)
  (let* ((b (with-open-file (s path :element-type '(unsigned-byte 8))
              (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                (read-sequence v s)
                v)))
         (from (map '(vector (unsigned-byte 8)) #'char-code from))
         (i (search from b)))
    (assert (and i (null (search from b :start2 (1+ i)))))
    (replace b (map '(vector (unsigned-byte 8)) #'char-code to) :start1 i)
    (with-open-file (s path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
      (write-sequence b s))))

(defun %csf-flip (string)
  (map 'string (lambda (ch) (case ch (#\0 #\1) (#\1 #\0) (t (if (char= ch #\a) #\b #\a))))
       string))

(defun %csf-restamp (fasl part)
  "Make FASL look compiled by another runtime (PART :RUNTIME) or compiler (:COMPILER)."
  (let* ((gen (dotcl::%core-generation))
         (dash (position #\- gen))
         (other (ecase part
                  (:runtime (concatenate 'string (subseq gen 0 (1+ dash)) (%csf-flip (subseq gen (1+ dash)))))
                  (:compiler (concatenate 'string (%csf-flip (subseq gen 0 dash)) (subseq gen dash))))))
    (%csf-replace-once fasl gen other)))

(defun %csf-build (tag value)
  "A module TAG compiled from a source defining VALUE. Returns root, source and fasl."
  (multiple-value-bind (root moddir) (%csf-setup tag)
    (let ((src (concatenate 'string moddir tag ".lisp")))
      (%csf-write-source src value)
      (let ((*error-output* (make-broadcast-stream))
            (*standard-output* (make-broadcast-stream)))
        (compile-file src))
      ;; Only the fasl: a .sil beside it would be the next candidate.
      (let ((sil (concatenate 'string moddir tag ".sil")))
        (when (probe-file sil) (delete-file sil)))
      (values root src (concatenate 'string moddir tag ".fasl")))))

;;; The source beside it is loaded, though the fasl is newer.
(deftest-emitting-only contrib-stale-fasl.other-runtime-loads-source
  (multiple-value-bind (root src fasl) (%csf-build "csfrt" :old)
    (%csf-write-source src :new)
    (%csf-restamp fasl :runtime)
    (%csf-touch-later fasl)
    (%csf-require root "csfrt"))
  :new)

;;; A matching fasl further down the search path is loaded instead.
(deftest-emitting-only contrib-stale-fasl.other-runtime-later-fasl
  (multiple-value-bind (root1 src1 fasl1) (%csf-build "csfrt2" :stale)
    (delete-file src1)
    (%csf-restamp fasl1 :runtime)
    (multiple-value-bind (root2 src2) (%csf-build "csfrt2" :fresh)
      (delete-file src2)
      (let ((paths (dotnet:static "DotCL.Runtime" "ContribExtraSearchPaths")))
        (makunbound 'cl-user::*csf-module-value*)
        (dotnet:invoke paths "Add" root1)
        (dotnet:invoke paths "Add" root2)
        (unwind-protect
             (let ((*error-output* (make-broadcast-stream)))
               (require "csfrt2")
               (symbol-value 'cl-user::*csf-module-value*))
          (dotnet:invoke paths "Remove" root1)
          (dotnet:invoke paths "Remove" root2)))))
  :fresh)

;;; No source and no other candidate: an error, which LOAD-AS-IS gets past.
(deftest-emitting-only contrib-stale-fasl.other-runtime-alone-is-an-error
  (multiple-value-bind (root src fasl) (%csf-build "csfrt3" :old)
    (delete-file src)
    (%csf-restamp fasl :runtime)
    (list (handler-case (%csf-require root "csfrt3")
            (dotcl:fasl-version-mismatch () :mismatch))
          (handler-bind ((dotcl:fasl-version-mismatch #'dotcl:load-as-is))
            (%csf-require root "csfrt3"))))
  (:mismatch :old))

;;; Another compiler only: loaded as it is, as a shipped contrib would be.
(deftest-emitting-only contrib-stale-fasl.other-compiler-still-loads
  (multiple-value-bind (root src fasl) (%csf-build "csfcomp" :old)
    (delete-file src)
    (%csf-restamp fasl :compiler)
    (%csf-require root "csfcomp"))
  :old)

;;; The generation names the runtime source as well as the compiler.
(deftest runtime-source-in-generation
  (let ((gen (dotcl::%core-generation)))
    (and gen (position #\- gen) (> (length gen) (1+ (position #\- gen))) t))
  t)
