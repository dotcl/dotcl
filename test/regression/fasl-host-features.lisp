;;; Host-dependent features (#+x86-64, #+windows, ...) read while a file is
;;; compiled are recorded in its fasl; LOAD warns DOTCL:FASL-HOST-MISMATCH when
;;; one has another truth on the host it is loaded on.

(defun %fhf-dir ()
  (let ((dir (format nil "~a/dotcl-fhf-~a/" (regression-temp-dir) (get-internal-real-time))))
    (ensure-directories-exist dir)
    dir))

(defun %fhf-compile (name text)
  (let ((src (format nil "~a~a.lisp" (%fhf-dir) name)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (namestring (compile-file src))))

(defun %fhf-flipped (feature)
  "dotcl:*host-features* with FEATURE's truth reversed."
  (if (member feature dotcl:*host-features*)
      (remove feature dotcl:*host-features*)
      (cons feature dotcl:*host-features*)))

(defun %fhf-load-warnings (fasl &optional (features dotcl:*host-features*))
  (let ((seen '()))
    (handler-bind ((warning (lambda (c)
                              (push c seen)
                              (muffle-warning c))))
      (let ((dotcl:*host-features* features))
        (load fasl)))
    (nreverse seen)))

(deftest-emitting-only fasl-host-features.recorded
  ;; Atoms the evaluation reaches are recorded, with AND / OR / NOT walked; WINDOWS
  ;; sits behind a false SBCL, so the result cannot depend on it.
  (let ((fasl (%fhf-compile "fhf-rec"
                            "(defun cl-user::fhf-rec () #+x86-64 1 #-x86-64 2)
(defun cl-user::fhf-rec2 () #+(and sbcl windows) :a #+(and (not sbcl) arm64) :b)")))
    (let ((rec (dotcl:fasl-host-features fasl)))
      (list (mapcar #'car rec)
            (every (lambda (e) (eq (cdr e) (and (member (car e) dotcl:*host-features*) t)))
                   rec))))
  ((:arm64 :x86-64) t))

(deftest-emitting-only fasl-host-features.ignore-variable
  (let ((fasl (let ((dotcl:*host-feature-ignore* '(:arm64)))
                (%fhf-compile "fhf-ign" "(defun cl-user::fhf-ign () #+x86-64 1 #+arm64 2)"))))
    (mapcar #'car (dotcl:fasl-host-features fasl)))
  (:x86-64))

(deftest-emitting-only fasl-host-features.warn-on-other-host
  (let* ((fasl (%fhf-compile "fhf-warn" "(defun cl-user::fhf-warn () #+x86-64 1 #-x86-64 2)"))
         (ws (%fhf-load-warnings fasl (%fhf-flipped :x86-64))))
    (list (length ws)
          (typep (first ws) 'dotcl:fasl-host-mismatch)
          (equal (dotcl:fasl-host-mismatch-features (first ws)) '(:x86-64))
          (equal (namestring (dotcl:fasl-host-mismatch-pathname (first ws))) fasl)
          (and (search "X86-64" (princ-to-string (first ws))) t)))
  (1 t t t t))

(deftest-emitting-only fasl-host-features.same-host-is-silent
  (let ((fasl (%fhf-compile "fhf-same" "(defun cl-user::fhf-same () #+x86-64 1 #-x86-64 2)")))
    (length (%fhf-load-warnings fasl)))
  0)

(deftest-emitting-only fasl-host-features.variable-silences
  (let ((fasl (%fhf-compile "fhf-quiet" "(defun cl-user::fhf-quiet () #+x86-64 1 #-x86-64 2)")))
    (let ((dotcl:*warn-host-dependent-fasl* nil))
      (length (%fhf-load-warnings fasl (%fhf-flipped :x86-64)))))
  0)

(deftest-emitting-only fasl-host-features.no-plus-no-record
  (let ((fasl (%fhf-compile "fhf-none" "(defun cl-user::fhf-none () 1)")))
    (list (dotcl:fasl-host-features fasl)
          (length (%fhf-load-warnings fasl (%fhf-flipped :x86-64)))))
  (nil 0))

(deftest-emitting-only fasl-host-features.not-host-dependent-not-recorded
  (let ((fasl (%fhf-compile "fhf-other" "(defun cl-user::fhf-other () #+sbcl 1 #-dotcl 2 #+(or sbcl dotcl) 3)")))
    (dotcl:fasl-host-features fasl))
  nil)

(deftest-emitting-only fasl-host-features.old-fasl-has-no-record
  ;; A fasl with no host record at all (as an older dotcl wrote) loads silently.
  (let* ((fasl (%fhf-compile "fhf-old" "(defun cl-user::fhf-old () #+x86-64 1 #-x86-64 2)"))
         (b (with-open-file (s fasl :element-type '(unsigned-byte 8))
              (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                (read-sequence v s)
                v)))
         (key (map '(vector (unsigned-byte 8)) #'char-code "dotcl-host-features"))
         (i (search key b)))
    ;; Rename the key so the loader does not recognise it.
    (replace b (map '(vector (unsigned-byte 8)) #'char-code "dotcl-host-featurez") :start1 i)
    (with-open-file (s fasl :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
      (write-sequence b s))
    (list (dotcl:fasl-host-features fasl)
          (length (%fhf-load-warnings fasl (%fhf-flipped :x86-64)))))
  (nil 0))
