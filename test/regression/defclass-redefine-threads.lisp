;;; Threads that redefine the same class (a class with accessors) at the same time: the
;;; lists of accessor methods the redefinition keeps are shared, and every thread finishes
;;; without an error and leaves a working class.

(deftest-emitting-only defclass-redefine-threads.together
  (let* ((cells (make-array 8 :initial-element nil))
         (threads
           (loop for k below 8
                 collect (let ((k k))
                           (dotnet:new "System.Threading.Thread"
                                       (dotnet:make-delegate
                                        "System.Threading.ThreadStart"
                                        (lambda ()
                                          (setf (aref cells k)
                                                (handler-case
                                                    (progn
                                                      (dotimes (i 40)
                                                        (eval '(defclass drt-c () ((x :initform 1 :accessor drt-x)
                                                                                   (y :initarg :y :reader drt-y)))))
                                                      :ok)
                                                  (error (e) (princ-to-string e)))))))))))
    (dolist (th threads) (dotnet:invoke th "Start"))
    (dolist (th threads) (dotnet:invoke th "Join"))
    (let ((o (make-instance 'drt-c :y 5)))
      (list (coerce cells 'list) (drt-x o) (drt-y o)
            (length (remove-if-not (lambda (m) (eq (first (method-specializers m)) (find-class 'drt-c)))
                                   (generic-function-methods #'drt-x))))))
  ((:ok :ok :ok :ok :ok :ok :ok :ok) 1 5 1))
