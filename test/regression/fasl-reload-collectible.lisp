;;; Loading the same fasl again. The first load of a path goes in by path (and may use
;;; native code); every load of that path after it goes into a collectible load context
;;; as IL, so reloading a system while developing does not pile up assemblies. The
;;; definitions of the newest load are the ones that stay.

(defun %frc-assemblies ()
  (let* ((arr (dotnet:invoke (dotnet:static "System.AppDomain" "get_CurrentDomain") "GetAssemblies"))
         (n (dotnet:invoke arr "get_Length")))
    (loop for i below n collect (dotnet:invoke arr "GetValue" i))))

(defun %frc-live (prefix)
  "The assemblies whose name starts with PREFIX still loaded after a full collection."
  (dotcl:gc) (dotcl:gc)
  (remove-if-not (lambda (a)
                   (let ((name (dotnet:invoke (dotnet:invoke a "GetName") "get_Name")))
                     (and (stringp name) (eql 0 (search prefix name)))))
                 (%frc-assemblies)))

(defun %frc-locations (prefix)
  (mapcar (lambda (a) (dotnet:invoke a "get_Location")) (%frc-live prefix)))

(defun %frc-quiet (thunk)
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (funcall thunk)))

(defun %frc-compile (dir name n)
  "Write NAME.lisp defining things that answer N, compile it to NAME.fasl, return the fasl."
  (let ((src (format nil "~a~a.lisp" dir name))
        (fasl (format nil "~a~a.fasl" dir name)))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (format o "(defun ~a-f () ~d)~%" name n)
      (format o "(defstruct ~a-s (a ~d))~%" name n)
      (format o "(defclass ~a-c () ((x :initform ~d :accessor ~a-cx)))~%" name n name)
      (format o "(defgeneric ~a-g (o))~%(defmethod ~a-g ((o ~a-c)) (~a-cx o))~%" name name name name)
      (format o "(defparameter ~a-closure (let ((k ~d)) (lambda () k)))~%" name n))
    (%frc-quiet (lambda () (compile-file src :output-file fasl)))
    fasl))

(defun %frc-fn (name suffix) (symbol-function (intern (format nil "~:@(~a-~a~)" name suffix) :cl-user)))
(defun %frc-answers (name)
  (list (funcall (%frc-fn name "f"))
        (funcall (%frc-fn name "g") (make-instance (intern (format nil "~:@(~a-c~)" name) :cl-user)))
        (funcall (symbol-value (intern (format nil "~:@(~a-closure~)" name) :cl-user)))
        (funcall (%frc-fn name "s-a")
                 (funcall (symbol-function (intern (format nil "~:@(make-~a-s~)" name) :cl-user))))))

(defun %frc-load (fasl) (%frc-quiet (lambda () (load fasl))))

(defun %frc-dir ()
  (let ((dir (format nil "~a/frc-~a/" (regression-temp-dir) (get-internal-real-time))))
    (ensure-directories-exist dir)
    dir))

(deftest-emitting-only fasl-reload-collectible.count-is-bounded
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcaa" 7)))
    (%frc-load fasl)
    (dotimes (i 30) (%frc-load fasl))
    (list (%frc-answers "frcaa")
          (<= (length (%frc-live "frcaa")) 3)))
  ((7 7 7 7) t))

(deftest-emitting-only fasl-reload-collectible.newest-definitions-win
  (let ((dir (%frc-dir)) (all-right t))
    (loop for n from 1 to 12
          do (%frc-load (%frc-compile dir "frcab" n))
             (unless (equal (%frc-answers "frcab") (list n n n n)) (setq all-right nil)))
    (list all-right (<= (length (%frc-live "frcab")) 3)))
  (t t))

(deftest-emitting-only fasl-reload-collectible.first-load-by-path-then-bytes
  ;; The first load is the file's own module (it has a Location); a reload is an image
  ;; in memory (none).
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcac" 1)))
    (%frc-load fasl)
    (let ((after-first (%frc-locations "frcac")))
      (%frc-load fasl)
      (let ((after-second (%frc-locations "frcac")))
        (list (length after-first)
              (plusp (length (first after-first)))
              (length after-second)
              (count "" after-second :test #'string=)))))
  (1 t 2 1))

(deftest-emitting-only fasl-reload-collectible.kept-old-function-still-runs
  (let* ((dir (%frc-dir)))
    (%frc-load (%frc-compile dir "frcad" 1))
    (let ((old (%frc-fn "frcad" "f")))
      (loop for n from 2 to 6 do (%frc-load (%frc-compile dir "frcad" n)))
      (list (funcall old) (funcall (%frc-fn "frcad" "f"))
            (<= (length (%frc-live "frcad")) 4))))
  (1 6 t))

(deftest-emitting-only fasl-reload-collectible.sibling-only-the-first-time
  ;; A <fasl>.r2r-<rid> beside the fasl is taken by the first load; a reload does
  ;; not go back to it (it is in memory, with no Location).
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcae" 1))
         (sibling (format nil "~a.r2r-~a" fasl (dotnet:static "DotCL.Runtime" "PortableRid"))))
    (with-open-file (in fasl :element-type '(unsigned-byte 8))
      (with-open-file (out sibling :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence buf in)
          (write-sequence buf out))))
    (let ((r2r0 (car (dotcl:r2r-stats))))
      (%frc-load fasl)
      (let ((r2r1 (car (dotcl:r2r-stats))))
        (%frc-load fasl)
        (%frc-load fasl)
        (list (- r2r1 r2r0)
              (- (car (dotcl:r2r-stats)) r2r1)
              (count "" (%frc-locations "frcae") :test #'string=)
              (equal (%frc-answers "frcae") '(1 1 1 1))))))
  (1 0 1 t))

(deftest-emitting-only fasl-reload-collectible.broken-fasl-then-fixed
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcaf" 1)) (errors 0))
    (%frc-load fasl)
    ;; Broken in place: a load fails and says so, and the next good one still works.
    (with-open-file (o fasl :direction :output :if-exists :supersede)
      (write-string "this is not a fasl" o))
    (handler-case (%frc-load fasl) (error () (incf errors)))
    (with-open-file (o fasl :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence (make-array 64 :element-type '(unsigned-byte 8) :initial-element 77) o))
    (handler-case (%frc-load fasl) (error () (incf errors)))
    (%frc-load (%frc-compile dir "frcaf" 2))
    ;; A path whose first load failed.
    (let ((fasl2 (format nil "~afrcag.fasl" dir)))
      (with-open-file (o fasl2 :direction :output :if-exists :supersede)
        (write-string "garbage" o))
      (handler-case (%frc-load fasl2) (error () (incf errors)))
      (%frc-load (%frc-compile dir "frcag" 3)))
    (list errors (%frc-answers "frcaf") (%frc-answers "frcag")))
  (3 (2 2 2 2) (3 3 3 3)))

(deftest-emitting-only fasl-reload-collectible.threads-loading-together
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcah" 5))
         (cells (make-array 4 :initial-element nil))
         (threads
           (loop for k below 4
                 collect (let ((k k))
                           (dotnet:new "System.Threading.Thread"
                                       (dotnet:make-delegate
                                        "System.Threading.ThreadStart"
                                        (lambda ()
                                          (setf (aref cells k)
                                                (handler-case (progn (dotimes (i 5) (load fasl)) :ok)
                                                  (error (e) (princ-to-string e)))))))))))
    (dolist (th threads) (dotnet:invoke th "Start"))
    (dolist (th threads) (dotnet:invoke th "Join"))
    (list (coerce cells 'list) (%frc-answers "frcah") (<= (length (%frc-live "frcah")) 6)))
  ((:ok :ok :ok :ok) (5 5 5 5) t))

;;; A DEFMETHOD with a docstring is the case that used to keep every reload: the
;;; documentation table held the method (and with it the code of the old load).
(deftest-emitting-only fasl-reload-collectible.documented-method
  (let* ((dir (%frc-dir)) (src (format nil "~afrcai.lisp" dir)) (fasl (format nil "~afrcai.fasl" dir)))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (format o "(defgeneric frcai-g (o))~%(defmethod frcai-g ((o integer)) \"the doc\" (+ o 1))~%"))
    (%frc-quiet (lambda () (compile-file src :output-file fasl)))
    (%frc-load fasl)
    (dotimes (i 30) (%frc-load fasl))
    (let ((m (first (generic-function-methods #'frcai-g))))
      (list (funcall #'frcai-g 1)
            (documentation m t)
            (<= (length (%frc-live "frcai")) 3))))
  (2 "the doc" t))

(defgeneric %frd-g (o))
(deftest fasl-reload-collectible.documentation-of-objects-still-works
  ;; Objects documented by identity: a method, a package, an anonymous class.
  (let* ((m (defmethod %frd-g ((o integer)) o))
         (pkg (make-package "FRD-DOC-PKG" :use nil))
         (cls (make-instance 'standard-class :name nil)))
    (setf (documentation m t) "method doc"
          (documentation pkg t) "package doc"
          (documentation cls 'type) "class doc")
    (prog1 (list (documentation m t) (documentation pkg t) (documentation cls 'type)
                 (progn (setf (documentation m t) nil) (documentation m t)))
      (delete-package pkg)))
  ("method doc" "package doc" "class doc" nil))

;;; A form the program keeps (as the quoted forms inside compiled code are) remembers its
;;; macro expansion, made while the function its operator names was the one of one particular
;;; load. That memory must not keep that function, or every reload stays.
(deftest-emitting-only fasl-reload-collectible.evaluated-form-does-not-keep-old-function
  (let* ((dir (%frc-dir)) (fasl (%frc-compile dir "frcaj" 4)) (kept '()) (results '()))
    (%frc-load fasl)
    (dotimes (i 20)
      (%frc-load fasl)
      (let ((form (list 'frcaj-f)))
        (push form kept)
        (%mini-macroexpand-1-cached form)
        (push (eval form) results)))
    (list (remove-duplicates results) (<= (length (%frc-live "frcaj")) 3)
          (length kept)))
  ((4) t 20))
