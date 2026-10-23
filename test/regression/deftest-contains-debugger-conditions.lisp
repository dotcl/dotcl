;;; DEFTEST records a condition that reaches the debugger as a FAIL of that
;;; test, also when it is not an ERROR, and the run goes on.

(deftest deftest-contains-debugger-conditions
  (let ((*test-count* 0) (*pass-count* 0) (*fail-count* 0) (*fail-names* nil)
        (*standard-output* (make-broadcast-stream)))
    (deftest inner-interrupt (dotcl::%interactive-interrupt) nil)
    (deftest inner-non-error (error 'simple-condition :format-control "x") nil)
    (deftest inner-error (error "x") nil)
    (deftest inner-pass (+ 1 2) 3)
    (list *test-count* *pass-count* *fail-count* (reverse *fail-names*)))
  (4 1 3 (inner-interrupt inner-non-error inner-error)))

(deftest deftest-keeps-signal-of-serious-condition
  (let ((*test-count* 0) (*pass-count* 0) (*fail-count* 0) (*fail-names* nil)
        (*standard-output* (make-broadcast-stream)))
    (deftest inner-signal (signal 'storage-condition) nil)
    (list *pass-count* *fail-count*))
  (1 0))
