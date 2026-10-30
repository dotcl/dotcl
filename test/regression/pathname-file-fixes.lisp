;;; Pathname and file operations that disagreed with SBCL.
;;;
;;; OPEN :DIRECTION :INPUT :IF-DOES-NOT-EXIST :CREATE must make an empty file
;;; and ignore :IF-EXISTS. ENSURE-DIRECTORIES-EXIST must return the given
;;; pathspec and say whether it created anything. FILE-WRITE-DATE must signal a
;;; FILE-ERROR for a missing file, as FILE-AUTHOR does. RENAME-FILE's FILE-ERROR
;;; must name the missing target when that is what failed.

(defun pff-dir (name)
  (let* ((base (pathname (concatenate 'string (regression-temp-dir) "/")))
         (dir (merge-pathnames (concatenate 'string name "/") base)))
    (ensure-directories-exist dir)
    dir))

(defmacro pff-with-dir ((var name) &body body)
  `(let ((,var (pff-dir ,name)))
     (unwind-protect (progn ,@body)
       (ignore-errors (dotcl:delete-directory ,var :recursive t)))))

(defun pff-namestring-error (thunk)
  "The namestring of THUNK's FILE-ERROR, or the error's type."
  (handler-case (progn (funcall thunk) :no-error)
    (file-error (e) (namestring (file-error-pathname e)))
    (error (e) (type-of e))))

(deftest open-input-create-ignores-if-exists
  (pff-with-dir (dir "open-input-create")
    (loop for if-exists in '(nil :error :new-version :rename
                             :rename-and-delete :overwrite :append :supersede)
          for i from 0
          always
          (let ((path (merge-pathnames (format nil "missing-~d.txt" i) dir)))
            (handler-case
                (with-open-file (s path :direction :input
                                        :if-exists if-exists
                                        :if-does-not-exist :create)
                  (and (probe-file path) (= 0 (file-length s))))
              (error () nil)))))
  t)

(deftest ensure-directories-exist-returns-given-string
  (pff-with-dir (dir "ensure-string")
    (let* ((path (namestring (merge-pathnames "a/b" dir)))
           (first-call (multiple-value-list (ensure-directories-exist path)))
           (second-call (multiple-value-list (ensure-directories-exist path))))
      (list (eq (first first-call) path) (second first-call)
            (eq (first second-call) path) (second second-call))))
  (t t t nil))

(deftest ensure-directories-exist-returns-given-pathname
  (pff-with-dir (dir "ensure-pathname")
    (let* ((path (merge-pathnames "c/d" dir))
           (result (multiple-value-list (ensure-directories-exist path))))
      (list (eq (first result) path) (second result))))
  (t t))

(deftest file-write-date-missing-file-signals
  (pff-with-dir (dir "fwd-file")
    (let ((path (merge-pathnames "no-such-file" dir)))
      (equal (pff-namestring-error (lambda () (file-write-date path)))
             (namestring path))))
  t)

(deftest file-write-date-missing-directory-signals
  (pff-with-dir (dir "fwd-dir")
    (let ((path (merge-pathnames "no-such-dir/file" dir)))
      (equal (pff-namestring-error (lambda () (file-write-date path)))
             (namestring path))))
  t)

(deftest file-write-date-below-regular-file-signals
  (pff-with-dir (dir "fwd-below")
    (let ((file (merge-pathnames "regular" dir)))
      (with-open-file (s file :direction :output :if-exists :supersede)
        (write-string "x" s))
      (equal (pff-namestring-error
              (lambda () (file-write-date (merge-pathnames "regular/child" dir))))
             (namestring (merge-pathnames "regular/child" dir)))))
  t)

(deftest rename-file-error-names-missing-target
  (pff-with-dir (dir "rename-target")
    (let ((source (merge-pathnames "a.txt" dir))
          (target (merge-pathnames "sub/b.txt" dir)))
      (with-open-file (s source :direction :output :if-exists :supersede)
        (write-string "x" s))
      (equal (pff-namestring-error (lambda () (rename-file source target)))
             (namestring target))))
  t)
