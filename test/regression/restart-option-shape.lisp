;;; RESTART-CASE clause options and RESTART-BIND binding keywords: a malformed one
;;; signals a PROGRAM-ERROR; the valid shapes still work.

(defmacro %ros-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest restart-option-shape.invalid
  (list (%ros-pe (restart-case 1 (foo () :report)))
        (%ros-pe (restart-case 1 (foo () :interactive)))
        (%ros-pe (restart-case 1 (foo () :test)))
        (%ros-pe (restart-case 1 (foo () :report 1 :report 2)))
        (%ros-pe (restart-case 1 (foo () :test #'identity :test #'identity)))
        (%ros-pe (restart-case 1 (foo . 1)))
        (%ros-pe (restart-bind ((foo #'print :bogus 1)) 1))
        (%ros-pe (restart-bind ((foo #'print :report-function)) 1))
        (%ros-pe (restart-bind ((foo)) 1))
        (%ros-pe (restart-bind (foo) 1)))
  (:program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error))

(deftest restart-option-shape.valid
  (list (restart-case (invoke-restart 'foo 3) (foo (x) :report "r" :interactive (lambda () nil) :test (lambda (c) (declare (ignore c)) t) x))
        (restart-case 5)
        (restart-case (invoke-restart 'foo) (foo () :bogus-keyword-form))
        (restart-bind ((foo (lambda () 7) :test-function (lambda (c) (declare (ignore c)) t) :report-function (lambda (s) s)
                            :interactive-function (lambda () nil)))
          (invoke-restart 'foo))
        (restart-bind () 8))
  (3 5 :bogus-keyword-form 7 8))
