;;; (load "foo") with no type and no file of that name tries foo.fasl and then foo.lisp (as
;;; SBCL does); a name that has a type, or that exists as written, is not completed.

(defvar *ldt-result* nil)

(defun %ldt-write (path text)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string text s)))

(deftest load-default-type.source-only
  (let* ((dir (regression-temp-dir)) (base (format nil "~A/ldt-src" dir)))
    (%ldt-write (format nil "~A.lisp" base) "(setq cl-user::*ldt-result* :from-lisp)")
    (setq *ldt-result* nil)
    (unwind-protect (list (load base) *ldt-result*)
      (ignore-errors (delete-file (format nil "~A.lisp" base)))))
  (t :from-lisp))

(deftest-emitting-only load-default-type.fasl-preferred-over-source
  (let* ((dir (regression-temp-dir)) (base (format nil "~A/ldt-both" dir))
         (src (format nil "~A.lisp" base)) (fasl (format nil "~A.fasl" base)))
    (%ldt-write src "(setq cl-user::*ldt-result* :from-fasl)")
    (compile-file src :output-file fasl)
    (%ldt-write src "(setq cl-user::*ldt-result* :from-lisp)")
    (setq *ldt-result* nil)
    (unwind-protect (progn (load base) *ldt-result*)
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  :from-fasl)

(deftest load-default-type.not-completed
  (let* ((dir (regression-temp-dir)) (base (format nil "~A/ldt-other" dir)))
    (%ldt-write (format nil "~A.lisp" base) "(setq cl-user::*ldt-result* :x)")
    (unwind-protect
         (list (handler-case (load (format nil "~A.txt" base)) (file-error () :file-error))
               (handler-case (load (format nil "~A-missing" base)) (file-error () :file-error))
               (load (format nil "~A-missing" base) :if-does-not-exist nil))
      (ignore-errors (delete-file (format nil "~A.lisp" base)))))
  (:file-error :file-error nil))

(deftest load-default-type.exact-name-wins
  (let* ((dir (regression-temp-dir)) (base (format nil "~A/ldt-exact" dir)))
    (%ldt-write base "(setq cl-user::*ldt-result* :exact)")
    (%ldt-write (format nil "~A.lisp" base) "(setq cl-user::*ldt-result* :lisp)")
    (setq *ldt-result* nil)
    (unwind-protect (progn (load base) *ldt-result*)
      (ignore-errors (delete-file base))
      (ignore-errors (delete-file (format nil "~A.lisp" base)))))
  :exact)
