;;; Two dists that both provide one system, installed in the same second: the
;;; dotcl-named dist must win, but an explicit later preference must stay.

(require "quicklisp")

(defvar *qdpr-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *qdpr-core* (regression-child-core))
(defvar *qdpr-dir* (regression-temp-dir))
(defvar *qdpr-fixture* nil)

(defun qdpr-path (name)
  (concatenate 'string *qdpr-dir* "/" name))

(defun qdpr-write (path text &key header)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (when header
      (write-line (if (search "releases.txt" (namestring path))
                      "# project url size file-md5 content-sha1 prefix system-file"
                      "# release system-file system-name dependency")
                  out))
    (write-string text out))
  path)

(defun qdpr-file-url (path)
  (format nil "file:///~a" (string-left-trim "/" (namestring path))))

(defun qdpr-file-size (path)
  (dotnet:invoke (dotnet:new "System.IO.FileInfo" path) "get_Length"))

(defun qdpr-make-tgz (source tar tgz)
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

(defun qdpr-make-dist (base name)
  (let* ((source (concatenate 'string base name "-source/"))
         (release (concatenate 'string source "qdpr-1/"))
         (tar (concatenate 'string base name ".tar"))
         (tgz (concatenate 'string base name ".tgz"))
         (dist (concatenate 'string base "dist-" name "/")))
    (qdpr-write (concatenate 'string release "qdpr-dupe.asd")
                "(defsystem \"qdpr-dupe\"
  :version \"1.0\"
  :components ((:file \"qdpr-dupe\")))")
    (qdpr-write (concatenate 'string release "qdpr-dupe.lisp")
                (format nil "(defpackage :qdpr-dupe
  (:use :cl)
  (:export #:which))
(in-package :qdpr-dupe)
(defun which () ~s)" name))
    (qdpr-make-tgz source tar tgz)
    (qdpr-write (concatenate 'string dist "systems.txt")
                "qdpr-release qdpr-dupe.asd qdpr-dupe" :header t)
    (qdpr-write (concatenate 'string dist "releases.txt")
                (format nil "qdpr-release ~a ~a 0 0 qdpr-1 qdpr-dupe.asd"
                        (qdpr-file-url tgz) (qdpr-file-size tgz))
                :header t)
    (qdpr-write (concatenate 'string dist "distinfo.txt")
                (format nil "name: ~a~%version: 1~%~
system-index-url: ~a~%release-index-url: ~a~%archive-base-url: ~a~%~
canonical-distinfo-url: ~a~%distinfo-subscription-url: ~a~%"
                        name
                        (qdpr-file-url (concatenate 'string dist "systems.txt"))
                        (qdpr-file-url (concatenate 'string dist "releases.txt"))
                        (qdpr-file-url base)
                        (qdpr-file-url (concatenate 'string dist "distinfo.txt"))
                        (qdpr-file-url (concatenate 'string dist "distinfo.txt"))))
    (qdpr-file-url (concatenate 'string dist "distinfo.txt"))))

(defun qdpr-fixture ()
  (or *qdpr-fixture*
      (let ((base (concatenate 'string (qdpr-path "fixture") "/")))
        (setf *qdpr-fixture*
              (list (qdpr-make-dist base "quicklisp")
                    (qdpr-make-dist base "dotcl"))))))

(defun qdpr-home (name)
  (let ((home (concatenate 'string (qdpr-path name) "/")))
    (ignore-errors (dotnet:static "System.IO.Directory" "Delete" home t))
    (ensure-directories-exist home)
    home))

(defun qdpr-script (name text)
  (qdpr-write (concatenate 'string (qdpr-path "scripts") "/" name) text))

(defun qdpr-run (script)
  (destructuring-bind (code out err)
      (dotcl:run-process *qdpr-exe* (list "--core" *qdpr-core* script))
    (list code out err)))

(defun qdpr-field (out marker)
  (let* ((start (search marker out))
         (from (and start (+ start (length marker))))
         (end (and from (position #\Newline out :start from))))
    (and from (subseq out from end))))

(defun qdpr-prelude (home)
  (format nil
          "(defpackage #:ql-setup (:use #:cl) (:export #:*quicklisp-home*))
(in-package #:cl-user)
(defparameter ql-setup:*quicklisp-home* #p\"~a\")
(require \"quicklisp\")
(defun qdpr-file-fetch (url file &rest keys)
  (declare (ignore keys))
  (let ((path (ql-http:path (ql-http:url url))))
    (when (and (> (length path) 2)
               (char= (char path 0) #\\/)
               (char= (char path 2) #\\:))
      (setf path (subseq path 1)))
    (ensure-directories-exist file)
    (ql-util:copy-file path file)))
(pushnew (cons \"file\" 'qdpr-file-fetch)
         ql-http:*fetch-scheme-functions* :key #'car :test #'equal)
" home))

(defun qdpr-install-both (home)
  (destructuring-bind (quicklisp dotcl) (qdpr-fixture)
    (qdpr-script
     "install.lisp"
     (concatenate 'string
                  (qdpr-prelude home)
                  "(let ((t0 (get-universal-time)))
  (loop while (= t0 (get-universal-time))))
(let ((before (get-universal-time)))
  (ql-dist:install-dist \""
                  quicklisp
                  "\" :prompt nil)
  (ql-dist:install-dist \""
                  dotcl
                  "\" :prompt nil)
  (format t \"~&QDPR-SAME-SECOND=~a~%\" (= before (get-universal-time))))
(ql:quickload :qdpr-dupe :silent t)
(let ((system (ql-dist:find-system \"qdpr-dupe\")))
  (format t \"~&QDPR-DIST=~a~%\" (ql-dist:name (ql-dist:dist system)))
  (format t \"~&QDPR-WHICH=~a~%\"
          (funcall (find-symbol \"WHICH\" \"QDPR-DUPE\"))))
"))))

(deftest quicklisp-dist-preference.same-second-dotcl-wins
  (let ((home (qdpr-home "same-second")))
    (destructuring-bind (code out err) (qdpr-run (qdpr-install-both home))
      (declare (ignore err))
      (list code
            (and (search "QDPR-SAME-SECOND=T" out) t)
            (qdpr-field out "QDPR-DIST=")
            (qdpr-field out "QDPR-WHICH="))))
  (0 t "dotcl" "dotcl"))

(deftest quicklisp-dist-preference.user-choice-stays
  (let* ((home (qdpr-home "user-choice"))
         (first (qdpr-script
                 "user-choice.lisp"
                 (concatenate
                  'string
                  (qdpr-prelude home)
                  "(let ((t0 (get-universal-time)))
  (loop while (= t0 (get-universal-time))))
(ql-dist:install-dist \""
                  (first (qdpr-fixture))
                  "\" :prompt nil)
(ql-dist:install-dist \""
                  (second (qdpr-fixture))
                  "\" :prompt nil)
(let ((ours (ql-dist:find-dist \"dotcl\"))
      (stock (ql-dist:find-dist \"quicklisp\")))
  (setf (ql-dist:preference stock)
        (+ 10 (ql-dist:preference ours)))
  (format t \"~&QDPR-USER-PREF=~a~%\" (ql-dist:preference stock)))
(ql:quickload :qdpr-dupe :silent t)
(format t \"~&QDPR-USER-DIST=~a~%\"
        (ql-dist:name (ql-dist:dist (ql-dist:find-system \"qdpr-dupe\"))))
")))
         (second (qdpr-script
                  "user-choice-after.lisp"
                  (concatenate
                   'string
                   (qdpr-prelude home)
                   "(let ((stock (ql-dist:find-dist \"quicklisp\")))
  (format t \"~&QDPR-AFTER-PREF=~a~%\" (ql-dist:preference stock)))
(ql:quickload :qdpr-dupe :silent t)
(format t \"~&QDPR-AFTER-DIST=~a~%\"
        (ql-dist:name (ql-dist:dist (ql-dist:find-system \"qdpr-dupe\"))))
"))))
    (destructuring-bind (code1 out1 err1) (qdpr-run first)
      (declare (ignore err1))
      (destructuring-bind (code2 out2 err2) (qdpr-run second)
        (declare (ignore err2))
        (list code1
              code2
              (qdpr-field out1 "QDPR-USER-DIST=")
              (qdpr-field out2 "QDPR-AFTER-DIST=")
              (equal (qdpr-field out1 "QDPR-USER-PREF=")
                     (qdpr-field out2 "QDPR-AFTER-PREF="))))))
  (0 0 "quicklisp" "quicklisp" t))
