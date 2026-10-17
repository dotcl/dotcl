;;; LOAD reads a fasl by path, each load into a context of its own. What that
;;; must not break: the edit / recompile / reload cycle on one output file.
;;;
;;; - Loading the same path again after a recompile runs the new module, not the
;;;   assembly already loaded from that path.
;;; - Code from the first load that had not run yet still runs after the file was
;;;   replaced (COMPILE-FILE writes a new file and renames it over the old one).
;;; - The replace leaves no temporary file behind (on Windows, other than the
;;;   loaded fasl moved aside), and DELETE-FILE works on a fasl that is loaded.

(defun flp-dir ()
  (pathname (concatenate 'string (regression-temp-dir) "/flp/")))

(defun flp-write (name &rest lines)
  (let ((path (merge-pathnames name (flp-dir))))
    (ensure-directories-exist path)
    (with-open-file (f path :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l f)))
    path))

(defun flp-compile-and-load (name &rest lines)
  (let* ((src (apply #'flp-write name lines))
         (out (make-pathname :type "fasl" :defaults src)))
    (compile-file src :output-file out)
    (load out)
    out))

(deftest-emitting-only fasl-load-by-path.reload-after-recompile
  (progn
    (flp-compile-and-load "flp-a.lisp"
                          "(defun flp-a-f () :v1)"
                          "(defun flp-a-g () :old-g)")
    (let ((v1 (funcall (find-symbol "FLP-A-F")))
          ;; Not called before the recompile: its body is compiled from the
          ;; first file only when it is first called, after the file is replaced.
          (old-g (symbol-function (find-symbol "FLP-A-G"))))
      (let ((out (flp-compile-and-load "flp-a.lisp"
                                       "(defun flp-a-f () :v2)"
                                       "(defun flp-a-g () :new-g)")))
        (let ((v2 (funcall (find-symbol "FLP-A-F"))))
          ;; The same path a third time, unchanged.
          (load out)
          (list v1 v2
                (funcall old-g)
                (funcall (find-symbol "FLP-A-G")))))))
  (:v1 :v2 :old-g :new-g))

(deftest-emitting-only fasl-load-by-path.no-temporary-left
  (progn
    (flp-compile-and-load "flp-b.lisp" "(defun flp-b-f () 1)")
    (flp-compile-and-load "flp-b.lisp" "(defun flp-b-f () 2)")
    (list (funcall (find-symbol "FLP-B-F"))
          (and (probe-file (merge-pathnames "flp-b.fasl" (flp-dir))) t)
          ;; On Windows the first fasl stays mapped, so it can only be renamed
          ;; aside (.in-use-*); a later move-aside of the name deletes it.
          (remove-if-not (lambda (p) (and (search "flp-b.fasl." p)
                                          #+windows (not (search ".in-use-" p))))
                         (mapcar #'file-namestring
                                 (directory (merge-pathnames "*.*" (flp-dir)))))))
  (2 t nil))

(deftest-emitting-only fasl-load-by-path.delete-loaded-fasl
  (let ((out (flp-compile-and-load "flp-c.lisp" "(defun flp-c-f () :c)")))
    (list (funcall (find-symbol "FLP-C-F"))
          (delete-file out)
          (probe-file out)
          (funcall (find-symbol "FLP-C-F"))))
  (:c t nil :c))

;; Only a loaded fasl is renamed aside: superseding a read-only file is still
;; an error, and the file is left as it was.
(deftest fasl-load-by-path.read-only-supersede-errors
  (let ((path (flp-write "flp-ro.txt" "old")))
    (dotnet:static "System.IO.File" "SetAttributes" (namestring path)
                   (dotnet:static "System.IO.FileAttributes" "ReadOnly"))
    (unwind-protect
         (list (handler-case
                   (with-open-file (f path :direction :output :if-exists :supersede)
                     (write-line "new" f)
                     :written)
                 (error () :error))
               (with-open-file (f path) (read-line f))
               (remove-if-not (lambda (p) (search "flp-ro.txt." p))
                              (mapcar #'file-namestring
                                      (directory (merge-pathnames "*.*" (flp-dir))))))
      (dotnet:static "System.IO.File" "SetAttributes" (namestring path)
                     (dotnet:static "System.IO.FileAttributes" "Normal"))
      (delete-file path)))
  (:error "old" nil))

(deftest-emitting-only fasl-load-by-path.io-supersede-loaded-fasl
  (let ((out (flp-compile-and-load "flp-d.lisp" "(defun flp-d-f () :d)")))
    (list (funcall (find-symbol "FLP-D-F"))
          (with-open-file (f out :direction :io :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
            (write-byte 7 f)
            (file-position f 0)
            (read-byte f))
          (with-open-file (f out :element-type '(unsigned-byte 8)) (file-length f))
          (progn (delete-file out) (probe-file out))))
  (:d 7 1 nil))

;; Superseding a loaded fasl writes a new file instead of truncating the mapped
;; one: functions that have not run yet still run afterwards.
(deftest-emitting-only fasl-load-by-path.supersede-keeps-loaded-code
  (let ((out (flp-compile-and-load "flp-e.lisp"
                                   "(defun flp-e-f () :f)"
                                   "(defun flp-e-g () (list :g (flp-e-f)))"
                                   "(defun flp-e-h () :h)")))
    (list (funcall (find-symbol "FLP-E-F"))
          (with-open-file (f out :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
            (write-byte 1 f))
          (funcall (find-symbol "FLP-E-G"))
          (with-open-file (f out :element-type '(unsigned-byte 8)) (file-length f))
          (progn
            (flp-compile-and-load "flp-e.lisp" "(defun flp-e-f () :f2)")
            (with-open-file (f out :direction :io :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
              (write-byte 2 f))
            (funcall (find-symbol "FLP-E-H")))
          (prog1 (with-open-file (f out :element-type '(unsigned-byte 8)) (read-byte f))
            (delete-file out))))
  (:f 1 (:g :f) 1 :h 2))
