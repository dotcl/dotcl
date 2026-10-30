;;; A dist installed after (require "quicklisp") on an empty home must be
;;; visible to ASDF in that same image, not only in a later process.

(require "quicklisp")

(defvar *qids-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *qids-core* (regression-child-core))
(defvar *qids-dir* (regression-temp-dir))
(defvar *qids-fixture* nil)

(defun qids-path (name)
  (concatenate 'string *qids-dir* "/" name))

(defun qids-write (path text &key header)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (when header
      (write-line (if (search "releases.txt" (namestring path))
                      "# project url size file-md5 content-sha1 prefix system-file"
                      "# release system-file system-name dependency")
                  out))
    (write-string text out))
  path)

(defun qids-file-url (path)
  (format nil "file:///~a" (string-left-trim "/" (namestring path))))

(defun qids-file-size (path)
  (dotnet:invoke (dotnet:new "System.IO.FileInfo" path) "get_Length"))

(defun qids-make-tgz (source tar tgz)
  (ignore-errors (dotnet:static "System.IO.File" "Delete" tar t))
  (ignore-errors (dotnet:static "System.IO.File" "Delete" tgz t))
  (dotnet:static "System.Formats.Tar.TarFile"
                 "CreateFromDirectory" source tar nil)
  (let* ((in (dotnet:static "System.IO.File" "OpenRead" tar))
         (out (dotnet:static "System.IO.File" "Create" tgz))
         (gz (dotnet:new "System.IO.Compression.GZipStream"
                         out
                         (dotnet:static "System.IO.Compression.CompressionMode"
                                        "Compress"))))
    (dotnet:invoke in "CopyTo" gz)
    (dotnet:invoke gz "Dispose")
    (dotnet:invoke out "Dispose")
    (dotnet:invoke in "Dispose"))
  tgz)

(defun qids-make-fixture ()
  (let* ((base (concatenate 'string (qids-path "fixture") "/"))
         (source (concatenate 'string base "source/"))
         (prefix "qids-regression-1")
         (release (format nil "~a~a/" source prefix))
         (tgz (concatenate 'string base "qids.tgz"))
         (url (qids-file-url tgz)))
    (qids-write (format nil "~aqids-regression.asd" release)
                "(defsystem \"qids-regression\"
  :version \"1.0\"
  :components ((:file \"qids-regression\")))")
    (qids-write (format nil "~aqids-regression.lisp" release)
                "(defpackage :qids-regression
  (:use :cl)
  (:export #:loaded-p))
(in-package :qids-regression)
(defun loaded-p () t)")
    (qids-make-tgz source
                   (concatenate 'string base "qids.tar")
                   tgz)
    (qids-write (concatenate 'string base "systems.txt")
                "qids-release qids-regression.asd qids-regression" :header t)
    (qids-write (concatenate 'string base "releases.txt")
                (format nil "qids-release ~a ~a 0 0 ~a qids-regression.asd"
                        url (qids-file-size tgz) prefix)
                :header t)
    (qids-write (concatenate 'string base "distinfo.txt")
                (format nil "name: qids-dist~%version: 1~%~
system-index-url: ~a~%release-index-url: ~a~%archive-base-url: ~a~%~
canonical-distinfo-url: ~a~%distinfo-subscription-url: ~a~%"
                        (qids-file-url (concatenate 'string base "systems.txt"))
                        (qids-file-url (concatenate 'string base "releases.txt"))
                        (qids-file-url base)
                        (qids-file-url (concatenate 'string base "distinfo.txt"))
                        (qids-file-url (concatenate 'string base "distinfo.txt"))))
    (concatenate 'string base "distinfo.txt")))

(defun qids-fixture ()
  (or *qids-fixture*
      (setf *qids-fixture* (qids-make-fixture))))

(defun qids-home (name)
  (let ((home (concatenate 'string (qids-path name) "/")))
    (ignore-errors (dotnet:static "System.IO.Directory" "Delete" home t))
    (ensure-directories-exist home)
    home))

(defun qids-script (name text)
  (qids-write (concatenate 'string (qids-path "scripts") "/" name) text))

(defun qids-run (script)
  (destructuring-bind (code out err)
      (dotcl:run-process *qids-exe* (list "--core" *qids-core* script))
    (list code out err)))

(defun qids-child-prelude (home)
  (format nil
          "(defpackage #:ql-setup (:use #:cl) (:export #:*quicklisp-home*))
(in-package #:cl-user)
(defparameter ql-setup:*quicklisp-home* #p\"~a\")
(require \"quicklisp\")
" home))

(deftest quicklisp-install-dist-searcher.installed-dist-is-visible
  (let* ((home (qids-home "home"))
         (script
           (qids-script
            "install-dist.lisp"
            (concatenate
             'string
             (qids-child-prelude home)
             "(defun qids-file-fetch (url file &rest keys)
  (declare (ignore keys))
  (let ((path (ql-http:path (ql-http:url url))))
    (when (and (> (length path) 2)
               (char= (char path 0) #\\/)
               (char= (char path 2) #\\:))
      (setf path (subseq path 1)))
    (ensure-directories-exist file)
    (ql-util:copy-file path file)))
(pushnew (cons \"file\" 'qids-file-fetch)
         ql-http:*fetch-scheme-functions* :key #'car :test #'equal)
(ql-dist:install-dist \""
             (qids-file-url (qids-fixture))
             "\" :prompt nil)
(ql:quickload :qids-regression)
(let ((fn (find-symbol \"LOADED-P\" \"QIDS-REGRESSION\")))
  (format t \"~&QIDS-LOADED=~a~%\" (and fn (fboundp fn) (funcall fn))))
"))))
    (destructuring-bind (code out err) (qids-run script)
      (declare (ignore err))
      (list code (and (search "QIDS-LOADED=T" out) t))))
  (0 t))

(deftest quicklisp-install-dist-searcher.empty-home-require-creates-no-files
  (let* ((home (qids-home "empty-home"))
         (script
           (qids-script
            "empty-home.lisp"
            (concatenate
             'string
             (qids-child-prelude home)
             "(let ((entries (dotnet:static \"System.IO.Directory\"
                                           \"GetFileSystemEntries\" \""
             home
             "\")))
  (format t \"~&QIDS-EMPTY-COUNT=~a~%\" (dotnet:invoke entries \"Length\")))
"))))
    (destructuring-bind (code out err) (qids-run script)
      (declare (ignore err))
      (list code (and (search "QIDS-EMPTY-COUNT=0" out) t))))
  (0 t))
