;;;; Builds one system, and everything it depends on, with
;;;; contrib/asdf-parallel and prints how long it took. Works on dotcl and SBCL.
;;;; Start the Lisp with an empty XDG_CACHE_HOME for a cold build:
;;;;
;;;;   PB_SYSTEM=ironclad PB_JOBS=4 XDG_CACHE_HOME=$(mktemp -d) dotcl pbuild.lisp
;;;;   PB_SYSTEM=ironclad PB_JOBS=4 XDG_CACHE_HOME=$(mktemp -d) sbcl --non-interactive --load pbuild.lisp
;;;;
;;;; Environment:
;;;;   PB_SYSTEM          the system to build
;;;;   PB_JOBS            compiles at once (1: ASDF's own serial perform)
;;;;   PB_EXECUTOR        optional: thread, fork or serial
;;;;   PB_PARALLEL_EVAL   dotcl only: if set, evaluation at compile time is not
;;;;                      serialized on the process-wide eval lock
;;;;
;;;; Output: BUILD <system> jobs <n> <ms> ms, then the driver's statistics.
(require "asdf")
(load (merge-pathnames "../../contrib/asdf-parallel/asdf-parallel.lisp" *load-truename*))

(let* ((sys (uiop:getenv "PB_SYSTEM"))
       (jobs (parse-integer (or (uiop:getenv "PB_JOBS") "1")))
       (ex (uiop:getenv "PB_EXECUTOR"))
       (asdf-parallel:*executor*
         (if (and ex (plusp (length ex))) (intern (string-upcase ex) :keyword) asdf-parallel:*executor*))
       (t0 (get-internal-real-time)))
  #+dotcl (when (uiop:getenv "PB_PARALLEL_EVAL") (dotcl:set-parallel-eval t))
  (handler-bind ((warning #'muffle-warning))
    (let ((*compile-verbose* nil) (*compile-print* nil))
      (asdf-parallel:load-system sys :jobs jobs)))
  (format t "~&BUILD ~A jobs ~D ~D ms~%~S~%" sys jobs
          (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)
          asdf-parallel:*statistics*)
  (finish-output))
(uiop:quit)
