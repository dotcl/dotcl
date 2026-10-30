;;; library-status tells a missing native library or header apart from fail.

(require "asdf")

(defvar *lsn-loaded* nil)

(defun lsn-path (name)
  (let ((dir (format nil "~Alibrary-status-native/" (regression-temp-dir))))
    (ensure-directories-exist dir)
    (concatenate 'string dir name)))

(defun lsn-write (name text)
  (let ((path (lsn-path name)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (write-string text out))
    path))

(defun lsn-json-value (value out)
  (cond ((stringp value) (format out "~S" value))
        ((integerp value) (format out "~D" value))
        ((eq value :null) (write-string "null" out))
        ((eq value t) (write-string "true" out))
        (t (write-string "null" out))))

(defun lsn-json (path entries)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-char #\[ out)
    (loop for entry in entries
          for first = t then nil
          do (unless first (write-char #\, out))
             (write-char #\{ out)
             (loop for (key . value) in entry
                   for first-pair = t then nil
                   do (unless first-pair (write-char #\, out))
                      (format out "~S: " key)
                      (lsn-json-value value out))
             (write-char #\} out))
    (write-char #\] out))
  path)

(defun lsn-load-render ()
  (unless *lsn-loaded*
    (let ((json (lsn-json (lsn-path "baseline-results.json")
                          (list (list (cons "system" "baseline")
                                      (cons "status" "load-only")))))
          (targets (lsn-write "baseline-targets.txt" "baseline\n"))
          (out (lsn-path "baseline.md")))
      (dolist (pair `(("LIBRARY_STATUS_JSON" . ,json)
                      ("LIBRARY_STATUS_TARGETS" . ,targets)
                      ("LIBRARY_STATUS_ANNOTATIONS" . ,(lsn-path "none.json"))
                      ("LIBRARY_STATUS_TESTS_JSON" . ,(lsn-path "none.json"))
                      ("LIBRARY_STATUS_FORKS_JSON" . ,(lsn-path "none.json"))
                      ("LIBRARY_STATUS_SBCL_JSON" . ,(lsn-path "none.json"))
                      ("LIBRARY_STATUS_SBCL_TESTS_JSON" . ,(lsn-path "none.json"))
                      ("LIBRARY_STATUS_OUT" . ,out)
                      ("DOTCL_VERSION" . "test")))
        (dotcl:setenv (car pair) (cdr pair)))
      (unwind-protect
           (let ((*standard-output* (make-broadcast-stream)))
             (load (merge-pathnames "../../bench/library-status/render.lisp"
                                    (or *load-truename* *default-pathname-defaults*))))
        (dolist (name '("LIBRARY_STATUS_JSON" "LIBRARY_STATUS_TARGETS"
                        "LIBRARY_STATUS_ANNOTATIONS" "LIBRARY_STATUS_TESTS_JSON"
                        "LIBRARY_STATUS_FORKS_JSON" "LIBRARY_STATUS_SBCL_JSON"
                        "LIBRARY_STATUS_SBCL_TESTS_JSON" "LIBRARY_STATUS_OUT"
                        "DOTCL_VERSION"))
          (dotcl:unsetenv name)))
      (setf *lsn-loaded* t))))

(defun lsn-render (tag results tests sbcl-results sbcl-tests)
  (lsn-load-render)
  (let* ((json (lsn-json (lsn-path (format nil "~A-results.json" tag)) results))
         (tests-json (lsn-json (lsn-path (format nil "~A-tests.json" tag)) tests))
         (sbcl-json (lsn-json (lsn-path (format nil "~A-sbcl.json" tag)) sbcl-results))
         (sbcl-tests-json (lsn-json (lsn-path (format nil "~A-sbcl-tests.json" tag))
                                    sbcl-tests))
         (targets (lsn-write (format nil "~A-targets.txt" tag)
                             (with-output-to-string (out)
                               (dolist (entry results)
                                 (format out "~A~%"
                                         (cdr (assoc "system" entry :test #'string=)))))))
         (out (lsn-path (format nil "~A.md" tag)))
         (log (make-string-output-stream)))
    (let ((*standard-output* log))
      (funcall (find-symbol "MAIN" "LIBRARY-STATUS/RENDER")
       :json json :targets targets :out out
       :annotations (lsn-path "none.json") :tests tests-json
       :forks (lsn-path "none.json") :sbcl-json sbcl-json
       :sbcl-tests sbcl-tests-json :dotcl-version "test"))
    (values (uiop:read-file-string out) (get-output-stream-string log))))

(defun lsn-row (markdown system)
  (find-if (lambda (line) (search (format nil "| ~A |" system) line))
           (uiop:split-string markdown :separator '(#\Newline))))

(deftest library-status-native-load-failure
  (let* ((results (list (list (cons "system" "native-test-yaml")
                              (cons "status" "fail")
                              (cons "note" "Unable to load any of the alternatives: (libyaml.dylib libyaml-0.2.dylib)"))))
         (sbcl (list (list (cons "system" "native-test-yaml")
                           (cons "status" "fail")
                           (cons "note" "Unable to load any of the alternatives: (libyaml.dylib libyaml-0.2.dylib)")))))
    (multiple-value-bind (markdown log)
        (lsn-render "yaml" results '() sbcl '())
      (let ((row (lsn-row markdown "native-test-yaml")))
        (list (and (search "`native`" row) t)
              (and (search "libyaml.dylib" row) t)
              (and (search "SBCL too" row) t)
              (and (search "native 1" log) t)))))
  (t t t t))

(deftest library-status-native-test-failure
  (let* ((results (list (list (cons "system" "native-test-cuda")
                              (cons "status" "load-only"))))
         (tests (list (list (cons "system" "native-test-cuda")
                            (cons "checked" "2026-10-23")
                            (cons "verdict" "load-fail")
                            (cons "note" "CUDA SDK not found, or at least failed to load libcuda."))))
         (sbcl-tests (list (list (cons "system" "native-test-cuda")
                                 (cons "verdict" "load-fail")
                                 (cons "note" "CUDA SDK not found, or at least failed to load libcuda.")))))
    (multiple-value-bind (markdown log)
        (lsn-render "cuda" results tests '() sbcl-tests)
      (let ((row (lsn-row markdown "native-test-cuda")))
        (list (and (search "`native`" row) t)
              (and (search "tests: libcuda is missing" row) t)
              (and (search "native: libcuda is missing" row) t)
              (and (search "SBCL too" row) t)
              (and (search "native 1" log) t)))))
  (t t t t t))

(deftest library-status-native-explicit-field
  (let* ((results
           (list (list (cons "system" "native-test-freetype")
                       (cons "status" "fail")
                       (cons "note" "Subprocess #<PROCESS-INFO> with command (cc -o x -c y) exited with error code 1")
                       (cons "native" "ft2build.h")))))
    (let ((row (lsn-row (lsn-render "freetype" results '() '() '())
                        "native-test-freetype")))
      (list (and (search "`native` (ft2build.h)" row) t)
            (not (search "`fail`" row)))))
  (t t))

(deftest library-status-native-other-failure-stays-fail
  (let* ((results (list (list (cons "system" "native-test-package")
                              (cons "status" "fail")
                              (cons "note" "Package SB-SYS not found")))))
    (multiple-value-bind (markdown log)
        (lsn-render "package" results '() '() '())
      (let ((row (lsn-row markdown "native-test-package")))
        (list (and (search "`fail`" row) t)
              (and (search "`native`" row) nil)
              (and (search "native 0" log) t)))))
  (t nil t))

#-windows
(deftest library-status-native-log-names-library
  (let* ((sh (namestring (merge-pathnames "../../bench/library-status/native.sh"
                                          (or *load-truename* *default-pathname-defaults*))))
         (header (lsn-write "header.log"
                            "grovel.h:2:10: fatal error: ft2build.h: No such file or directory\n"))
         (alternatives (lsn-write "alternatives.log"
                                  "Unable to load any of the alternatives: (libyaml.dylib libyaml-0.2.dylib)\n"))
         (compiler (lsn-write "compiler.log"
                              "Subprocess #<PROCESS-INFO> with command (cc -o x -c y) exited with error code 1\n"))
         (run (lambda (log)
                (string-trim '(#\Newline #\Return #\Space)
                             (uiop:run-program
                              (list "sh" "-c" ". \"$1\"; native_missing \"$2\"" "sh" sh log)
                              :output :string :error-output :output
                              :ignore-error-status t)))))
    (list (funcall run header)
          (funcall run alternatives)
          (funcall run compiler)))
  ("ft2build.h" "libyaml.dylib" "C compiler"))
