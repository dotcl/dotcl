;;; delta.sh reports what got worse or better between two library-status files.

(require "asdf")

(defun lsd-path (name)
  (let ((dir (format nil "~Alibrary-status-delta/" (regression-temp-dir))))
    (ensure-directories-exist dir)
    (concatenate 'string dir name)))

(defun lsd-write (name text)
  (let ((path (lsd-path name)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (write-string text out))
    path))

(defun lsd-entry (system &key status verdict passed failed checked)
  (with-output-to-string (out)
    (format out "{\"system\": \"~A\"" system)
    (when status (format out ", \"status\": \"~A\"" status))
    (when verdict (format out ", \"verdict\": \"~A\"" verdict))
    (when passed (format out ", \"passed\": ~D" passed))
    (when failed (format out ", \"failed\": ~D" failed))
    (when checked (format out ", \"checked\": \"~A\"" checked))
    (write-string "}" out)))

(defun lsd-json (&rest entries)
  (format nil "[~{~A~^, ~}]" entries))

(defun lsd-script ()
  (namestring (merge-pathnames "../../bench/library-status/delta.sh"
                               (or *load-truename* *default-pathname-defaults*))))

(defun lsd-run (&rest args)
  (multiple-value-bind (out err code)
      (uiop:run-program (append (list "sh" (lsd-script)) args)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (declare (ignore err))
    (values out code)))

#-windows
(deftest library-status-delta-classifies-changes
  (let* ((old (lsd-write "delta-old-results.json"
                         (lsd-json (lsd-entry "alpha" :status "load-only")
                                   (lsd-entry "beta" :status "fail")
                                   (lsd-entry "gone" :status "fail"))))
         (new (lsd-write "delta-new-results.json"
                         (lsd-json (lsd-entry "alpha" :status "fail")
                                   (lsd-entry "beta" :status "load-only")
                                   (lsd-entry "new" :status "load-only")))))
    (multiple-value-bind (out code) (lsd-run old new)
      (list (and (search "WORSE" out) t)
            (and (search "alpha  load load-only -> fail" out) t)
            (and (search "BETTER" out) t)
            (and (search "beta  load fail -> load-only" out) t)
            (and (search "ADDED" out) t)
            (and (search "new  load load-only" out) t)
            (and (search "REMOVED" out) t)
            (and (search "gone  load fail" out) t)
            (and (search "worse 1, better 1, added 1, removed 1" out) t)
            (= code 1))))
  (t t t t t t t t t t))

#-windows
(deftest library-status-delta-counts-and-verdicts
  (let* ((old (lsd-write "delta-old-tests.json"
                         (lsd-json (lsd-entry "alpha" :verdict "fail" :passed 10 :failed 2)
                                   (lsd-entry "beta" :verdict "error" :passed 0 :failed 0)
                                   (lsd-entry "gamma" :verdict "pass" :passed 5 :failed 0))))
         (new (lsd-write "delta-new-tests.json"
                         (lsd-json (lsd-entry "alpha" :verdict "fail" :passed 8 :failed 4)
                                   (lsd-entry "beta" :verdict "fail" :passed 0 :failed 0)
                                   (lsd-entry "gamma" :verdict "pass" :passed 5 :failed 0
                                              :checked "2026-01-01")))))
    (multiple-value-bind (out code) (lsd-run old new)
      (list (and (search "alpha  passed 10 -> 8; failed 2 -> 4" out) t)
            (and (search "beta  tests error -> fail" out) t)
            (not (search "gamma" out))
            (and (search "worse 1, better 1, added 0, removed 0" out) t)
            (= code 1))))
  (t t t t t))

#-windows
(deftest library-status-delta-identical-file
  (let ((file (lsd-write "delta-same.json"
                         (lsd-json (lsd-entry "alpha" :status "load-only")))))
    (multiple-value-bind (out code) (lsd-run file file)
      (list (and (search "worse 0, better 0, added 0, removed 0" out) t)
            (= code 0))))
  (t t))
