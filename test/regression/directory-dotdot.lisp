;;; ".." inside a pathname is an ordinary directory step: DIRECTORY, PROBE-FILE and
;;; friends follow it. Only :UP / :BACK directly after :ABSOLUTE or :WILD-INFERIORS
;;; are errors (the root has no parent). DIRECTORY used to refuse any ".." after the
;;; root, which broke an XDG_DATA_HOME such as "/home/u/src/../data".

(defun %dd-setup ()
  (let ((base (format nil "~a/dotdot" (regression-temp-dir))))
    (ensure-directories-exist (format nil "~a/a/" base))
    (ensure-directories-exist (format nil "~a/x/" base))
    (with-open-file (s (format nil "~a/x/f.lisp" base) :direction :output :if-exists :supersede)
      (write-line "(print 1)" s))
    base))

(defun %dd-names (list base)
  (let ((n (length base)))
    (sort (mapcar (lambda (p) (let ((s (namestring (truename p))))
                                (subseq s (min n (length s)))))
                  list)
          #'string<)))

(deftest directory-dotdot.directory-follows-dotdot
  (let ((base (%dd-setup)))
    (list (%dd-names (directory (format nil "~a/a/../x/*.lisp" base)) (namestring (truename base)))
          (and (member (truename (format nil "~a/x/" base))
                       (directory (format nil "~a/a/../*/" base)) :test #'equal)
               t)))
  (("x/f.lisp") t))

(deftest directory-dotdot.directory-of-a-dotdot-directory
  (let ((base (%dd-setup)))
    (equal (truename (format nil "~a/" base))
           (first (directory (format nil "~a/a/../" base)))))
  t)

(deftest directory-dotdot.up-after-absolute-is-still-an-error
  (list (handler-case (progn (directory (make-pathname :directory '(:absolute :up))) :no-error)
          (file-error () :file-error))
        (handler-case (progn (directory (make-pathname :directory '(:absolute :wild-inferiors :back)))
                             :no-error)
          (file-error () :file-error))
        (handler-case (progn (length (directory (make-pathname :directory '(:relative :wild-inferiors :up))))
                             :no-error)
          (file-error () :file-error)))
  (:file-error :file-error :file-error))
