;;; A fasl compiled by another dotcl, given to LOAD.
;;;
;;;   - Another core: DOTCL:FASL-VERSION-MISMATCH, an ERROR, with the restarts
;;;     DOTCL:LOAD-AS-IS and CONTINUE. Unhandled, nothing of the fasl runs.
;;;   - A newer DotCL.Runtime than this one: the same condition, with neither
;;;     restart, since .NET cannot bind the fasl.
;;;   - Under ASDF's LOAD-OP: the file is compiled again and loaded.
;;;
;;; Another dotcl's fasl is made by rewriting the stamps in one compiled here.

(require "asdf")

(defun %fvm-dir ()
  (let ((dir (format nil "~a/dotcl-fvm-~a/" (regression-temp-dir) (get-internal-real-time))))
    (ensure-directories-exist dir)
    dir))

(defun %fvm-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun %fvm-replace-once (path from to)
  "Replace the one occurrence of the octets FROM in the file at PATH with TO."
  (let* ((b (%fvm-bytes path))
         (i (search from b)))
    (assert (and i (null (search from b :start2 (1+ i)))))
    (replace b to :start1 i)
    (with-open-file (s path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
      (write-sequence b s))
    path))

(defun %fvm-octets (string) (map '(vector (unsigned-byte 8)) #'char-code string))

(defun %fvm-other-core (fasl)
  (%fvm-replace-once fasl (%fvm-octets (dotcl::%core-generation))
                     (%fvm-octets (map 'string (lambda (ch) (if (char= ch #\0) #\1 #\0))
                                       (dotcl::%core-generation)))))

(defun %fvm-u16s (&rest ns)
  (coerce (loop for n in ns append (list (ldb (byte 8 0) n) (ldb (byte 8 8) n)))
          '(vector (unsigned-byte 8))))

(defun %fvm-newer-runtime (fasl)
  "Make FASL reference DotCL.Runtime 99.x: what a later dotcl's fasl looks like here."
  (let ((v (dotnet:invoke
            (dotnet:invoke
             (dotnet:invoke (dotnet:static "System.Type" "GetType" "DotCL.Runtime, DotCL.Runtime")
                            "Assembly")
             "GetName")
            "Version")))
    (%fvm-replace-once fasl
                       (%fvm-u16s (dotnet:invoke v "Major") (dotnet:invoke v "Minor")
                                  (dotnet:invoke v "Build") (dotnet:invoke v "Revision"))
                       (%fvm-u16s 99))))

(defun %fvm-compile (dir name value)
  "Compile NAME.lisp, defining the function NAME to return VALUE. Returns the fasl."
  (let ((src (format nil "~a~a.lisp" dir name)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defun cl-user::~a () ~s)" name value))
    (namestring (compile-file src))))

(defun %fvm-restarts (c)
  (loop for name in '(dotcl:load-as-is continue)
        collect (and (find-restart name c) t)))

(deftest fasl-version-mismatch.same-core-is-silent
  (let ((fasl (%fvm-compile (%fvm-dir) "fvm-same" 1)))
    (handler-case (progn (load fasl) (funcall 'cl-user::fvm-same))
      (dotcl:fasl-version-mismatch () :signalled)))
  1)

(deftest fasl-version-mismatch.other-core-is-an-error
  (let ((fasl (%fvm-other-core (%fvm-compile (%fvm-dir) "fvm-other" 2))))
    (multiple-value-bind (c restarts)
        (block nil
          (handler-bind ((dotcl:fasl-version-mismatch
                           (lambda (c) (return (values c (%fvm-restarts c))))))
            (load fasl)))
      (list (typep c 'error)
            (equal (namestring (dotcl:fasl-version-mismatch-pathname c)) fasl)
            (equal (dotcl:fasl-version-mismatch-built-by c) (dotcl::%core-generation))
            (and (search "compiled by a different dotcl core" (princ-to-string c)) t)
            restarts
            (fboundp 'cl-user::fvm-other))))
  (t t nil t (t t) nil))

(deftest fasl-version-mismatch.load-as-is
  (let ((fasl (%fvm-other-core (%fvm-compile (%fvm-dir) "fvm-as-is" 3)))
        (n 0))
    (handler-bind ((dotcl:fasl-version-mismatch
                     (lambda (c) (incf n) (dotcl:load-as-is c))))
      (load fasl)
      (load fasl))
    (list (funcall 'cl-user::fvm-as-is) n))
  (3 1))

(deftest fasl-version-mismatch.continue
  (let ((fasl (%fvm-other-core (%fvm-compile (%fvm-dir) "fvm-continue" 4))))
    (handler-bind ((dotcl:fasl-version-mismatch #'continue))
      (load fasl))
    (funcall 'cl-user::fvm-continue))
  4)

(deftest fasl-version-mismatch.newer-runtime-cannot-load
  (let ((fasl (%fvm-newer-runtime (%fvm-compile (%fvm-dir) "fvm-newer" 5))))
    (handler-case (load fasl)
      (dotcl:fasl-version-mismatch (c)
        (list (and (search "needs DotCL.Runtime 99." (princ-to-string c)) t)
              (%fvm-restarts c)
              (fboundp 'cl-user::fvm-newer)))))
  (t (nil nil) nil))

;;; ASDF compiles the file again instead of signalling, and the new fasl is the
;;; one loaded.
(deftest fasl-version-mismatch.asdf-recompiles
  (let* ((dir (%fvm-dir))
         (asd (format nil "~afvm-sys.asd" dir)))
    (with-open-file (s asd :direction :output :if-exists :supersede)
      (write-string "(defsystem \"fvm-sys\" :components ((:file \"fvm-sys-a\")))" s))
    (with-open-file (s (format nil "~afvm-sys-a.lisp" dir) :direction :output :if-exists :supersede)
      (write-string "(defun cl-user::fvm-sys-a () 6)" s))
    (asdf:load-asd asd)
    (asdf:load-system "fvm-sys" :force t)
    (let ((fasl (namestring (first (asdf:output-files 'asdf:compile-op
                                                      (asdf:find-component "fvm-sys" "fvm-sys-a"))))))
      (%fvm-other-core fasl)
      (fmakunbound 'cl-user::fvm-sys-a)
      (asdf:clear-system "fvm-sys")
      (asdf:load-asd asd)
      (prog1 (list (handler-case (progn (asdf:load-system "fvm-sys") :loaded)
                     (error (e) (princ-to-string e)))
                   (funcall 'cl-user::fvm-sys-a)
                   (and (search (%fvm-octets (dotcl::%core-generation)) (%fvm-bytes fasl)) t))
        (asdf:clear-system "fvm-sys"))))
  (:loaded 6 t))
