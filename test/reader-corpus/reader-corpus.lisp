;;;; Read the sources of the Quicklisp libraries with the reader and record, per
;;;; file, how many forms it read and the first error. Run by check.sh in this
;;;; directory (make test-reader-corpus); see there for the environment.

(in-package :cl-user)

(defun reader-env (name)
  #+dotcl (dotcl:getenv name)
  #+sbcl (sb-ext:posix-getenv name)
  #-(or dotcl sbcl) nil)

(defun reader-impl-name ()
  (or (reader-env "READER_IMPL")
      #+dotcl "dotcl"
      #+sbcl "sbcl"
      #-(or dotcl sbcl) "lisp"))

(defun split-on (line separators)
  (let ((fields '()) (start nil))
    (loop for i from 0 below (length line)
          for c = (char line i)
          do (if (member c separators)
                 (when start
                   (push (subseq line start i) fields)
                   (setf start nil))
                 (unless start (setf start i))))
    (when start (push (subseq line start) fields))
    (nreverse fields)))

(defun split-ws (line) (split-on line '(#\Space #\Tab #\Return)))

(defun split-tsv (line)
  (let ((fields '()) (start 0))
    (loop for i from 0 below (length line)
          when (char= (char line i) #\Tab)
            do (push (subseq line start i) fields)
               (setf start (1+ i)))
    (push (subseq line start) fields)
    (nreverse fields)))

(defun trim (s)
  (string-trim '(#\Space #\Tab #\Return #\Newline) s))

(defun blank-line-p (line) (zerop (length (trim line))))

(defun comment-line-p (line)
  (and (plusp (length line)) (char= (char line 0) #\#)))

(defun read-lines (path)
  (with-open-file (s path :direction :input)
    (loop for line = (read-line s nil :eof)
          until (eq line :eof)
          collect line)))

(defun read-table (path key-column value-column)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (read-lines path) table)
      (unless (or (blank-line-p line) (comment-line-p line))
        (let ((fields (split-ws line)))
          (when (> (length fields) (max key-column value-column))
            (setf (gethash (nth key-column fields) table)
                  (nth value-column fields))))))))

(defun parse-features ()
  (let ((text (reader-env "READER_FEATURES")))
    (when (and text (plusp (length text)))
      (mapcar (lambda (name) (intern (string-upcase name) :keyword))
              (remove-if #'blank-line-p (split-on text '(#\, #\Space)))))))

(defun path-exists-p (path)
  (not (null (probe-file path))))

(defun default-ql-home ()
  (or (reader-env "LIBRARY_STATUS_QL_HOME")
      (let ((home (or (reader-env "HOME") "")))
        (if (path-exists-p (concatenate 'string home "/quicklisp"))
            (concatenate 'string home "/quicklisp")
            (concatenate 'string
                         (or (reader-env "XDG_DATA_HOME")
                             (concatenate 'string home "/.local/share"))
                         "/dotcl/quicklisp")))))

(defun release-directory (corpus prefix)
  (let ((path (merge-pathnames (concatenate 'string prefix "/")
                               (pathname (concatenate 'string corpus "/")))))
    (and (path-exists-p path) path)))

(defun source-files (release-dir)
  (sort (remove-duplicates
         (append (directory (merge-pathnames "**/*.lisp" release-dir))
                 (directory (merge-pathnames "**/*.asd" release-dir)))
         :test #'equal)
        #'string< :key #'namestring))

(defun file-key (file release-dir prefix)
  (handler-case
      (concatenate 'string prefix "/"
                   (enough-namestring (truename file) (truename release-dir)))
    (error () (concatenate 'string prefix "/" (file-namestring file)))))

(defun token-after (text marker &key (quote #\"))
  (let ((start (search marker text :test #'char-equal)))
    (when start
      (let ((i (+ start (length marker))))
        (when (and (< i (length text)) (char= (char text i) quote))
          (incf i))
        (let ((end (position-if (lambda (c)
                                  (or (char= c quote)
                                      (char= c #\Space)
                                      (char= c #\Tab)
                                      (char= c #\Newline)))
                                text :start i)))
          (let ((name (and end (subseq text i end))))
            (and name (plusp (length name)) name)))))))

(defun missing-package-name (condition)
  (let ((name (or (handler-case
                      (when (typep condition 'package-error)
                        (let ((package (package-error-package condition)))
                          (and package (package-name package))))
                    (error () nil))
                  (let ((msg (princ-to-string condition)))
                    (when (or (search "not found" msg :test #'char-equal)
                              (search "does not exist" msg :test #'char-equal))
                      (token-after msg "Package "))))))
    (and name (not (find-package name)) name)))

(defun missing-symbol-info (condition)
  "Return (values symbol-name package-name) for a package-prefix error."
  (let ((msg (princ-to-string condition)))
    (when (or (search "is not external in package" msg :test #'char-equal)
              (search "is not external in the" msg :test #'char-equal)
              (and (search "not found in the" msg :test #'char-equal)
                   (search "package" msg :test #'char-equal)))
      (let ((symbol (token-after msg "Symbol "))
            (package (or (handler-case
                             (when (typep condition 'package-error)
                               (let ((p (package-error-package condition)))
                                 (and p (package-name p))))
                           (error () nil))
                         (token-after msg "in package ")
                         (token-after msg "in the "))))
        (values symbol package)))))

(defun locked-symbol-info (condition)
  "Return (values symbol-name package-name) for a package-lock error."
  (let ((msg (princ-to-string condition)))
    (when (and (search "Lock on package" msg :test #'char-equal)
               (search "when interning" msg :test #'char-equal))
      (values (token-after msg "when interning ")
              (token-after msg "Lock on package ")))))

(defun make-missing-package (name)
  (handler-case
      (progn (make-package name :use '("CL")) t)
    (error () nil)))

(defun export-symbol (symbol-name package-name)
  (let ((package (find-package package-name)))
    (and package
         (handler-case
             (progn (export (intern symbol-name package) package) t)
           (error (e)
             #+sbcl
             (when (typep e 'sb-ext:package-locked-error)
               (handler-case
                   (progn (sb-ext:unlock-package package)
                          (export (intern symbol-name package) package)
                          t)
                 (error () nil)))
             #-sbcl (progn e nil))))))

(defun recover-condition (condition)
  "Make the package prefix in CONDITION resolvable; return a key or NIL."
  (let ((package (missing-package-name condition)))
    (if package
        (and (make-missing-package package)
             (concatenate 'string "pkg:" package))
        (multiple-value-bind (symbol package-name) (locked-symbol-info condition)
          (unless (and symbol package-name)
            (multiple-value-setq (symbol package-name)
              (missing-symbol-info condition)))
          (and symbol package-name
               (export-symbol symbol package-name)
               (concatenate 'string "sym:" package-name ":" symbol))))))

(defun read-file-forms (path)
  "Return (values forms first-error), recovering package prefix errors."
  (let ((tries 0) (recovered '()))
    (loop
      (incf tries)
      (let ((forms 0) (first-error nil))
        (handler-case
            (with-open-file (s path :direction :input :external-format :utf-8)
              (loop
                (let ((form (read s nil :eof)))
                  (when (eq form :eof) (return))
                  (incf forms))))
          (error (e) (setf first-error e)))
        (let ((key (and first-error (recover-condition first-error))))
          (if (and key
                   (not (member key recovered :test #'string=))
                   (< tries 500))
              (push key recovered)
              (return (values forms first-error))))))))

(defun clean-message (condition)
  (let ((msg (princ-to-string condition)))
    (setf msg (map 'string (lambda (c)
                             (if (member c '(#\Tab #\Newline #\Return)) #\Space c))
                   msg))
    (if (> (length msg) 240) (subseq msg 0 240) msg)))

(defun write-row (out file forms error)
  (format out "~a~c~d~c~a~c~a~%"
          file #\Tab forms #\Tab
          (if error (format nil "~a" (type-of error)) "")
          #\Tab
          (if error (clean-message error) "")))

(defun collect ()
  (let* ((repo (or (reader-env "READER_REPO") "."))
         (default-dist (concatenate 'string (default-ql-home) "/dists/quicklisp"))
         (dist (or (reader-env "READER_DIST") default-dist))
         (corpus (or (reader-env "READER_CORPUS")
                     (concatenate 'string default-dist "/software")))
         (out-path (or (reader-env "READER_OUT")
                       "reader-corpus.tsv"))
         (limit 100000)
         (file-limit (let ((text (reader-env "READER_FILES")))
                       (cond ((null text) 500)
                             ((string-equal text "all") nil)
                             (t (or (ignore-errors (parse-integer text)) 500)))))
         (targets-path (merge-pathnames "bench/library-status/targets.txt"
                                        (concatenate 'string repo "/")))
         (systems (read-table (merge-pathnames "systems.txt" (concatenate 'string dist "/")) 2 0))
         (releases (read-table (merge-pathnames "releases.txt" (concatenate 'string dist "/")) 0 5))
         (targets '())
         (feature-list (parse-features))
         (started (get-universal-time))
         (file-count 0) (form-count 0) (error-count 0) (missing 0))
    (dolist (line (read-lines targets-path))
      (unless (or (blank-line-p line) (comment-line-p line))
        (when (< (length targets) limit)
          (push (first (split-ws line)) targets))))
    (setf targets (nreverse targets))
    (with-open-file (out out-path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
      (format out "# reader-corpus implementation=~a~%" (reader-impl-name))
      (format out "# features=~s~%" (or feature-list *features*))
      (format out "# corpus=~a dist=~a~%" corpus dist)
      (let ((*package* (make-package "READER-CORPUS-SCRATCH" :use '("CL")))
            (*read-eval* nil)
            (*readtable* (copy-readtable nil))
            (*features* (or feature-list *features*)))
        (dolist (system targets)
          (when (and file-limit (>= file-count file-limit)) (return))
          (let* ((project (gethash system systems))
                 (prefix (and project (gethash project releases)))
                 (release-dir (and prefix (release-directory corpus prefix))))
            (cond
              ((null prefix)
               (format *error-output* "no dist release for system ~a~%" system))
              ((null release-dir)
               (incf missing)
               (format *error-output* "no source for ~a (~a)~%" project prefix))
              (t
               (dolist (file (source-files release-dir))
                 (when (and file-limit (>= file-count file-limit)) (return))
                 (multiple-value-bind (forms error) (read-file-forms file)
                   (incf file-count)
                   (incf form-count forms)
                   (when error (incf error-count))
                   (write-row out (file-key file release-dir prefix) forms error)))))))
        (finish-output out)))
    (format *error-output*
            "~&reader-corpus: ~d files, ~d forms, ~d files with errors, ~d missing releases, ~ds~%"
            file-count form-count error-count missing
            (- (get-universal-time) started))
    (finish-output)))

(collect)
