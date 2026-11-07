;;; A library package's shipped fasls, read by another dotcl or on another
;;; platform than the one that made them.
;;;
;;;   - Same OS and CPU, another dotcl: the fasls are used as they are, after a
;;;     NUGET:FASL-VERSION-MISMATCH warning. A handler can invoke
;;;     NUGET:COMPILE-FROM-SOURCE instead (or NUGET:USE-SHIPPED-FASLS).
;;;   - Another OS or CPU, or none recorded: the sources are compiled.
;;;   - A placed fasl that fails to load: its source is compiled and loaded.
;;;
;;; The first half works on a made-up layout and runs everywhere; the second packs
;;; and loads a real system, which needs the compiler.

(require "asdf-nuget")

(defun nvm-dir (tag)
  (let ((d (nuget::%combine (regression-temp-dir)
                            (format nil "dotcl-nvm-~a-~a" tag
                                    (dotnet:invoke (dotnet:static "System.Guid" "NewGuid")
                                                   "ToString" "N")))))
    (dotnet:static "System.IO.Directory" "CreateDirectory" d)
    d))

(defun nvm-write (dir name text)
  (let ((p (nuget::%combine dir name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory"
                   (dotnet:static "System.IO.Path" "GetDirectoryName" p))
    (dotnet:static "System.IO.File" "WriteAllText" p text)
    p))

(defun nvm-this-core () (funcall (find-symbol "%CORE-GENERATION" "DOTCL")))

(defun nvm-record-text (core platform)
  (with-standard-io-syntax
    (prin1-to-string
     `(:dotcl-version "99.0.0-test" :core ,core
       ,@(unless (eq platform :none) (list :platform platform))
       :fasls (("src/x.lisp.fasl" . "src/x.lisp"))))))

(defun nvm-tree (core &optional (platform (nuget::%platform-features)))
  "A system directory as a layout holds it, with a build record naming CORE and
PLATFORM (:NONE leaves the platform out, as dotcl before it was recorded did)."
  (let ((dir (nvm-dir "shipped")))
    (nvm-write dir "src/x.lisp" "(defun nvm-x () 1)")
    (nvm-write dir "src/x.lisp.fasl" "pretend fasl")
    (nvm-write dir "src/x.lisp.fasl.r2r-test-rid" "pretend sibling")
    (nvm-write dir "dotcl-build.sexp" (nvm-record-text core platform))
    dir))

(defun nvm-target (dir)
  (namestring (uiop:compile-file-pathname* (truename (nuget::%combine dir "src/x.lisp")))))

(defmacro nvm-capturing-errors (&body body)
  "BODY's values, then what it wrote to *ERROR-OUTPUT*."
  `(let ((*error-output* (make-string-output-stream)))
     (let ((v (multiple-value-list (progn ,@body))))
       (values-list (append v (list (get-output-stream-string *error-output*)))))))

;;; Another dotcl, same platform: placed, with one warning line.
(deftest nuget-fasl.other-dotcl-uses-fasls
  (let* ((dir (nvm-tree "not-this-core"))
         (seen '()))
    (multiple-value-bind (n out)
        (nvm-capturing-errors
          (handler-bind ((warning (lambda (c) (push (type-of c) seen))))
            (nuget::%place-fasls dir)))
      (list n seen
            (dotnet:static "System.IO.File" "ReadAllText" (nvm-target dir))
            (and (search "was compiled by dotcl 99.0.0-test" out) t)
            (count #\Newline (string-right-trim '(#\Newline) out)))))
  (1 (nuget:fasl-version-mismatch) "pretend fasl" t 0))

;;; The restart: nothing placed, so ASDF compiles the sources.
(deftest nuget-fasl.restart-compiles-sources
  (let* ((dir (nvm-tree "not-this-core"))
         (n (handler-bind ((nuget:fasl-version-mismatch #'nuget:compile-from-source))
              (nuget::%place-fasls dir))))
    (list n (probe-file (nvm-target dir))))
  (nil nil))

(deftest nuget-fasl.restart-uses-fasls
  (let* ((dir (nvm-tree "not-this-core"))
         (n (handler-bind ((nuget:fasl-version-mismatch #'nuget:use-shipped-fasls))
              (nuget::%place-fasls dir))))
    (list n (and (probe-file (nvm-target dir)) t)))
  (1 t))

;;; Compiling from source after an earlier image took the fasls: the placed fasl
;;; (and its sibling) would be found newer than the source and loaded, so it goes.
(deftest nuget-fasl.restart-removes-earlier-placement
  (let* ((dir (nvm-tree "not-this-core"))
         (target (nvm-target dir)))
    (nvm-capturing-errors (nuget::%place-fasls dir))
    (let ((before (list (and (probe-file target) t)
                        (and (probe-file (concatenate 'string target ".r2r-test-rid")) t))))
      (let ((nuget::*fasl-decisions* (make-hash-table :test #'equal)))
        (handler-bind ((nuget:fasl-version-mismatch #'nuget:compile-from-source))
          (nuget::%place-fasls dir)))
      (list before
            (probe-file target)
            (probe-file (concatenate 'string target ".r2r-test-rid")))))
  ((t t) nil nil))

;;; The decision holds for the image: a second registration neither warns again
;;; nor changes its mind.
(deftest nuget-fasl.decided-once
  (let* ((dir (nvm-tree "not-this-core"))
         (warnings 0))
    (handler-bind ((nuget:fasl-version-mismatch
                     (lambda (c) (incf warnings) (nuget:compile-from-source c))))
      (nuget::%place-fasls dir)
      (list (nuget::%place-fasls dir) warnings (probe-file (nvm-target dir)))))
  (nil 1 nil))

;;; Another OS or CPU: the sources, whatever the dotcl, and no warning to handle.
(deftest nuget-fasl.other-platform-compiles-sources
  (let ((other (if (member :windows *features*) '(:linux :unix :x86-64) '(:windows :win32 :x86))))
    (loop for core in (list (nvm-this-core) "not-this-core")
          collect (let ((dir (nvm-tree core other))
                        (warned nil))
                    (multiple-value-bind (n out)
                        (nvm-capturing-errors
                          (handler-bind ((warning (lambda (c) c (setf warned t))))
                            (nuget::%place-fasls dir)))
                      (list n warned (probe-file (nvm-target dir))
                            (and (search "was compiled on" out) t))))))
  ((nil nil nil t) (nil nil nil t)))

;;; A record without the platform (made before it was recorded): the sources.
(deftest nuget-fasl.unrecorded-platform-compiles-sources
  (let ((dir (nvm-tree "not-this-core" :none)))
    (multiple-value-bind (n out) (nvm-capturing-errors (nuget::%place-fasls dir))
      (list n (probe-file (nvm-target dir))
            (and (search "does not record the platform" out) t))))
  (nil nil t))

;;; The same dotcl on the same platform: placed, silently.
(deftest nuget-fasl.same-dotcl-uses-fasls
  (let ((dir (nvm-tree (nvm-this-core))))
    (multiple-value-bind (n out) (nvm-capturing-errors (nuget::%place-fasls dir))
      (list n out)))
  (1 ""))

;;; Only the OS and CPU features count, in any order.
(deftest nuget-fasl.platform-features
  (list (nuget::%platform-features '(:dotcl :unix :arm64 :darwin :64-bit :sb-thread))
        (equal (nuget::%platform-features (reverse (nuget::%platform-features)))
               (nuget::%platform-features)))
  ((:darwin :unix :arm64 :64-bit) t))

;;; --- a real package --------------------------------------------------------
;;;
;;; The library's WHICH answers :FASL in the shipped fasl and :SOURCE in the
;;; shipped source (edited after packing), so calling it says which was loaded.

(defun nvm-layout (name)
  "Pack system NAME, lay its package out as a consumer would, and return the
system's directory in the layout."
  (let ((src (nvm-dir "lib")))
    (nvm-write src "src.lisp"
               (format nil "(defpackage :~a (:use :cl) (:export #:which))
(in-package :~a)
(defun which () :fasl)" name name))
    (nvm-write src (format nil "~a.asd" name)
               (format nil "(defsystem ~s :version \"1.0.0\" :author \"a\" :description \"d\"
  :components ((:file \"src\")))" name))
    (asdf:load-asd (nuget::%combine src (format nil "~a.asd" name)))
    (let* ((nupkg (asdf-nuget::%pack-library (asdf:find-system name) (nvm-dir "out")))
           (root (nvm-dir "packages"))
           (layout (nvm-dir "layout")))
      (dotnet:static "System.IO.Compression.ZipFile" "ExtractToDirectory" nupkg
                     (nuget::%combine root name "1.0.0"))
      (let ((nuget::*packages-directory* root))
        (nuget::%copy-lisp-systems (list (cons name "1.0.0")) layout))
      (let ((dir (nuget::%combine layout "dotcl" name)))
        (nvm-write dir "src.lisp"
                   (format nil "(defpackage :~a (:use :cl) (:export #:which))
(in-package :~a)
(defun which () :source)" name name))
        ;; what the image has from packing, so a load that did nothing shows
        (setf (fdefinition (find-symbol "WHICH" (string-upcase name))) (lambda () :stale))
        (asdf:clear-system name)
        dir))))

(defun nvm-edit-record (dir &rest changes)
  (let* ((path (nuget::%combine dir "dotcl-build.sexp"))
         (r (read-from-string (dotnet:static "System.IO.File" "ReadAllText" path))))
    (loop for (k v) on changes by #'cddr do (setf (getf r k) v))
    (dotnet:static "System.IO.File" "WriteAllText" path
                   (with-standard-io-syntax (prin1-to-string r)))))

(defun nvm-load (name dir)
  "Register DIR as a layout would, load NAME, and return what WHICH answers and
what was written to *ERROR-OUTPUT*."
  (nvm-capturing-errors
    (let ((asdf:*central-registry* (list (pathname (concatenate 'string dir "/")))))
      (nuget::%place-fasls dir)
      (asdf:load-system name)
      (funcall (find-symbol "WHICH" (string-upcase name))))))

;;; The record names the platform the package was made on.
(deftest-emitting-only nuget-fasl.record-names-the-platform
  (let ((dir (nvm-layout "nvm-rec")))
    (getf (read-from-string (dotnet:static "System.IO.File" "ReadAllText"
                                           (nuget::%combine dir "dotcl-build.sexp")))
          :platform))
  #.(nuget::%platform-features))

;;; (a) Another dotcl: the shipped fasl is loaded, after one warning line.
(deftest-emitting-only nuget-fasl.load-other-dotcl
  (let ((dir (nvm-layout "nvm-a")))
    (nvm-edit-record dir :core "0123456789abcdef" :dotcl-version "99.0.0-test")
    (multiple-value-bind (which out) (nvm-load "nvm-a" dir)
      (list which (and (search "was compiled by dotcl 99.0.0-test" out) t))))
  (:fasl t))

;;; (b) The handler picks COMPILE-FROM-SOURCE: the source is compiled and loaded.
(deftest-emitting-only nuget-fasl.load-restart-compiles
  (let ((dir (nvm-layout "nvm-b")))
    (nvm-edit-record dir :core "0123456789abcdef")
    (handler-bind ((nuget:fasl-version-mismatch #'nuget:compile-from-source))
      (values (nvm-load "nvm-b" dir))))
  :source)

;;; (c) A fasl that does not load: here it references a DotCL.Runtime newer than
;;; this one, which .NET refuses. Its source is compiled and loaded instead.
(defun nvm-runtime-version-bytes ()
  (let* ((v (dotnet:invoke (dotnet:invoke (dotnet:invoke (dotnet:static "System.Type" "GetType"
                                                                       "DotCL.Runtime, DotCL.Runtime")
                                                         "get_Assembly")
                                          "GetName")
                           "get_Version")))
    (loop for part in '("get_Major" "get_Minor" "get_Build" "get_Revision")
          for n = (max 0 (dotnet:invoke v part))
          append (list (ldb (byte 8 0) n) (ldb (byte 8 8) n)))))

(defun nvm-bump-runtime-reference (fasl)
  "Make FASL's reference to DotCL.Runtime ask for major version 99. Returns how
many references were changed (one, or the test is not testing anything)."
  (let* ((bytes (with-open-file (in fasl :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                    (read-sequence v in)
                    v)))
         (pattern (coerce (nvm-runtime-version-bytes) '(vector (unsigned-byte 8))))
         (n 0))
    (loop for start = 0 then (1+ at)
          for at = (search pattern bytes :start2 start)
          while at
          do (setf (aref bytes at) 99 (aref bytes (1+ at)) 0)
             (incf n))
    (with-open-file (out fasl :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :supersede)
      (write-sequence bytes out))
    n))

(deftest-emitting-only nuget-fasl.load-failure-compiles
  (let ((dir (nvm-layout "nvm-c")))
    (let ((bumped (nvm-bump-runtime-reference (nuget::%combine dir "src.lisp.fasl"))))
      (multiple-value-bind (which out) (nvm-load "nvm-c" dir)
        (list bumped which (and (search "did not load" out) t)))))
  (1 :source t))

;;; (d) Another platform: the source is compiled, even under the same dotcl.
(deftest-emitting-only nuget-fasl.load-other-platform
  (let ((dir (nvm-layout "nvm-d")))
    (nvm-edit-record dir :platform (if (member :windows *features*)
                                       '(:linux :unix :x86-64)
                                       '(:windows :win32 :x86)))
    (values (nvm-load "nvm-d" dir)))
  :source)

;;; --- The version rule: a dotcl reads what an older one (0.1.34 and later) wrote ----------------

(defun nvm-versioned-tree (version &key (platform (nuget::%platform-features)) host-independent)
  (let ((dir (nvm-dir "versioned")))
    (nvm-write dir "src/x.lisp" "(defun nvm-x () 1)")
    (nvm-write dir "src/x.lisp.fasl" "pretend fasl")
    (nvm-write dir "dotcl-build.sexp"
               (with-standard-io-syntax
                 (prin1-to-string
                  `(:dotcl-version ,version :core "not-this-core" :platform ,platform
                    ,@(and host-independent '(:host-independent t))
                    :fasls (("src/x.lisp.fasl" . "src/x.lisp"))))))
    dir))

(defun nvm-place-versioned (version &rest keys &key (reading "0.1.40") &allow-other-keys)
  "(values placed-count warned text-written-to-error-output) for a tree built by VERSION, read
by a dotcl of version READING (default 0.1.40; the running version is whatever the checkout was
built as, and a build without git tags says 0.0.0)."
  (let* ((dir (apply #'nvm-versioned-tree version (loop for (k v) on keys by #'cddr unless (eq k :reading) append (list k v))))
         (nuget::*reading-version* reading)
         (warned nil))
    (multiple-value-bind (n out)
        (nvm-capturing-errors
          (handler-bind ((warning (lambda (c) c (setf warned t))))
            (nuget::%place-fasls dir)))
      (values n warned out))))

(deftest nuget-fasl.version-numbers
  (list (nuget::%version-numbers "0.1.36+327.g429aec7.dirty")
        (nuget::%version-numbers "0.1.34")
        (nuget::%version-numbers "99.0.0-test")
        (nuget::%version-numbers "garbage")
        (nuget::%version-numbers nil)
        (nuget::%version< '(0 1 33) '(0 1 34))
        (nuget::%version< '(0 1 34) '(0 1 34))
        (nuget::%version< '(0 2) '(0 1 99))
        (nuget::%version< '(0 1) '(0 1 0 1)))
  ((0 1 36) (0 1 34) (99 0 0) nil nil t nil nil t))

;;; Older than this dotcl but 0.1.34 or later: the fasls are used as they are, silently.
(deftest nuget-fasl.older-dotcl-fasls-load-silently
  (multiple-value-bind (n warned out) (nvm-place-versioned "0.1.34")
    (list n warned out))
  (1 nil ""))

;;; Newer than this dotcl: the same warning as for any other dotcl, then the fasls (the loader
;;; refuses one that needs a newer runtime, as before).
(deftest nuget-fasl.newer-dotcl-still-warns
  (multiple-value-bind (n warned out) (nvm-place-versioned "99.0.0")
    (list n warned (and (search "was compiled by dotcl 99.0.0" out) t)))
  (1 t t))

;;; Below 0.1.34, or a version that cannot be read: the sources, without an error or a warning.
(deftest nuget-fasl.too-old-dotcl-compiles-sources
  (let ((r '()))
    (dolist (v '("0.1.33" "0.0.1-test" "garbage"))
      (multiple-value-bind (n warned out) (nvm-place-versioned v)
        (push (list n warned (and (search "so its sources are compiled instead" out) t)) r)))
    (nreverse r))
  ((nil nil t) (nil nil t) (nil nil t)))

;;; A package that declares itself host independent is used on another platform.
(deftest nuget-fasl.host-independent-ignores-platform
  (let ((other (if (member :windows *features*) '(:linux :unix :x86-64) '(:windows :win32 :x86))))
    (list (nvm-place-versioned "0.1.34" :platform other :host-independent t)
          (nvm-place-versioned "0.1.34" :platform other)))
  (1 nil))

;;; The packer writes the declaration into the build record, and the record reads back.
(deftest nuget-fasl.build-record-host-independent
  (let ((with (read-from-string (asdf-nuget::%build-record '(("a.fasl" . "a.lisp")) t)))
        (without (read-from-string (asdf-nuget::%build-record '(("a.fasl" . "a.lisp"))))))
    (list (getf with :host-independent) (getf without :host-independent :absent)
          (getf with :fasls) (and (getf with :platform) t)))
  (t :absent (("a.fasl" . "a.lisp")) t))

;;; A dotcl built from a source without git tags (a bundle clone, git archive) says 0.0.0, and
;;; one whose version is not a number at all is "unknown": either way every recorded version
;;; looks the same or newer, so the shipped fasls are used after the usual warning -- neither
;;; silently nor from source.
(deftest nuget-fasl.reader-without-a-version-warns
  (let ((r '()))
    (dolist (reading '("0.0.0" "unknown"))
      (multiple-value-bind (n warned) (nvm-place-versioned "0.1.34" :reading reading)
        (push (list n warned) r)))
    (nreverse r))
  ((1 t) (1 t)))
