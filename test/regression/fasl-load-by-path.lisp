;;; LOAD reads a fasl by path, each load into a context of its own. What that
;;; must not break: the edit / recompile / reload cycle on one output file.
;;;
;;; - Loading the same path again after a recompile runs the new module, not the
;;;   assembly already loaded from that path.
;;; - Code from the first load that had not run yet still runs after the file was
;;;   replaced (COMPILE-FILE writes a new file and renames it over the old one).
;;; - The replace leaves no temporary file behind, and DELETE-FILE works on a
;;;   fasl that is loaded.

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
          (remove-if-not (lambda (p) (search "flp-b.fasl." p))
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
