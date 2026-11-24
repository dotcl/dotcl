;;; Literals that go through MAKE-LOAD-FORM survive loading the fasl in a different
;;; process: a circular structure made with MAKE-LOAD-FORM-SAVING-SLOTS stays circular,
;;; and a condition object is rebuilt. Each case compiles here and loads the fasl in a
;;; child process started from REGRESSION-CHILD-CORE.

(defvar *cfx-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cfx-core* (regression-child-core))

(defun %cfx-run (text)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "cfx.lisp" dir))
         (fasl (merge-pathnames "cfx.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (let ((r (dotcl:run-process *cfx-exe*
                                (list "--core" *cfx-core* "--eval"
                                      (format nil "(load ~S)" (namestring fasl))))))
      (string-trim '(#\Newline #\Space #\Return) (second r)))))

(deftest-compiled-only cf-mlf-cross-process.cyclic-struct
  (%cfx-run "(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct cfx-node val next)
  (defmethod make-load-form ((o cfx-node) &optional env)
    (make-load-form-saving-slots o :environment env)))
(defparameter *cfx-n*
  '#.(let ((n (make-cfx-node :val 1)))
       (setf (cfx-node-next n) (make-cfx-node :val 2 :next n))
       n))
(princ (list (eq *cfx-n* (cfx-node-next (cfx-node-next *cfx-n*)))
             (cfx-node-val (cfx-node-next *cfx-n*))))")
  "(T 2)")

(deftest-compiled-only cf-mlf-cross-process.condition
  (%cfx-run "(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition cfx-cond (error) ((v :initarg :v :reader cfx-v)))
  (defmethod make-load-form ((o cfx-cond) &optional env)
    (make-load-form-saving-slots o :environment env)))
(defparameter *cfx-c* '#.(make-condition 'cfx-cond :v 42))
(princ (list (typep *cfx-c* 'cfx-cond) (cfx-v *cfx-c*)))")
  "(T 42)")
