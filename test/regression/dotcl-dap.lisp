;;; The dotcl-dap contrib, driven by a DAP client over loopback: attach, a
;;; function breakpoint hit on a worker thread, stackTrace / scopes /
;;; variables / evaluate on the stopped thread, continue. Only the worker
;;; stops: the main thread and a second worker keep running meanwhile.
;;;
;;; A second session covers a self-recursive function loaded from a file
;;; (only its outer call stops until it is defined again, then every call
;;; does), a function redefined while its breakpoint is set, and conditions: the
;;; "uncaught" exception filter stops the thread, and a restart resumes it.
;;;
;;; Every read has a socket receive timeout and every wait an upper bound, so
;;; a regression fails the test instead of hanging the suite.

(require "dotcl-dap")
(require "dotcl-bsd-sockets")

(defun dap-test-target (x y) (+ x y))
(defun dap-test-caller-free (n) (* n 100))
(let ((dotcl:*emit-frame-locals* t))
  (eval '(defun dap-test-caller (n) (let ((m (* n 10))) (1+ (dap-test-target n m))))))

(defvar *dap-test-seq* 0)
(defvar *dap-test-events* '())

(defun %dap-send (stream command &rest arguments)
  (let ((seq (incf *dap-test-seq*)))
    (dotcl-dap::write-message
     stream (list :obj "seq" seq "type" "request" "command" command
                  "arguments" (cons :obj arguments)))
    seq))

(defun %dap-read (stream)
  (let ((m (dotcl-dap::read-message stream)))
    (unless m (error "DAP test: connection closed"))
    m))

(defun %dap-request (stream command &rest arguments)
  "Send a request and return its response, keeping the events that arrive first."
  (let ((seq (apply #'%dap-send stream command arguments)))
    (loop for m = (%dap-read stream)
          do (if (and (equal (dotcl-dap::jget m "type") "response")
                      (eql (dotcl-dap::jget m "request_seq") seq))
                 (return m)
                 (setf *dap-test-events* (append *dap-test-events* (list m)))))))

(defun %dap-event (stream name)
  (let ((seen (find name *dap-test-events*
                    :key (lambda (m) (dotcl-dap::jget m "event")) :test #'equal)))
    (if seen
        (progn (setf *dap-test-events* (remove seen *dap-test-events*)) seen)
        (loop for m = (%dap-read stream)
              when (equal (dotcl-dap::jget m "event") name) do (return m)))))

(defun %dap-body (m key) (dotcl-dap::jget (dotcl-dap::jget m "body") key))

(defun %dap-scenario ()
  (let* ((server (dotcl-dap:start-server :port 0))
         (socket (dotcl-bsd-sockets:tcp-connect "127.0.0.1" (dotcl-dap:server-port server)
                                                :timeout 5))
         (stream (progn
                   (setf (dotcl-bsd-sockets:sockopt-receive-timeout socket) 10)
                   (dotcl-bsd-sockets:socket-make-stream
                    socket :element-type '(unsigned-byte 8))))
         (out '())
         (spinning t)
         (spins 0)
         (worker nil)
         (spinner nil))
    (flet ((note (key value) (push (cons key value) out)))
      (unwind-protect
           (let ((init (%dap-request stream "initialize" "adapterID" "dotcl")))
             (note :initialize (list (dotcl-dap::jget init "success")
                                     (%dap-body init "supportsFunctionBreakpoints")))
             (note :initialized (and (%dap-event stream "initialized") t))
             (note :attach (dotcl-dap::jget (%dap-request stream "attach") "success"))
             (let ((r (%dap-request stream "setFunctionBreakpoints"
                                    "breakpoints" (vector (list :obj "name" "dap-test-target")
                                                          (list :obj "name" "no-such-fn-here")))))
               (note :breakpoints (map 'list (lambda (b) (dotcl-dap::jget b "verified"))
                                       (%dap-body r "breakpoints"))))
             (%dap-request stream "configurationDone")
             (setf spinner (dotcl-thread:make-thread
                            (lambda () (loop while spinning do (incf spins) (sleep 0.01)))
                            :name "dap-test-spinner"))
             (setf worker (dotcl-thread:make-thread (lambda () (dap-test-caller 4))
                                                    :name "dap-test-worker"))
             (let* ((stopped (%dap-event stream "stopped"))
                    (tid (%dap-body stopped "threadId")))
               (note :stopped (list (%dap-body stopped "reason")
                                    (%dap-body stopped "allThreadsStopped")))
               ;; The others still run while the worker is stopped.
               (let ((before spins))
                 (sleep 0.3)
                 (note :others-run (list (> spins before)
                                         (dap-test-caller-free 2)
                                         (dotcl-thread:thread-alive-p worker))))
               (note :threads
                     (and (find tid (%dap-body (%dap-request stream "threads") "threads")
                                :key (lambda (th) (dotcl-dap::jget th "id")))
                          t))
               (let* ((frames (%dap-body (%dap-request stream "stackTrace" "threadId" tid)
                                         "stackFrames"))
                      (names (map 'list (lambda (f) (dotcl-dap::jget f "name")) frames))
                      (top (dotcl-dap::jget (aref frames 0) "id"))
                      (caller (find-if (lambda (f)
                                         (search "DAP-TEST-CALLER" (dotcl-dap::jget f "name")))
                                       frames)))
                 (note :top-frame (first names))
                 (note :caller-frame (and caller (dotcl-dap::jget caller "name")))
                 (let* ((scope (aref (%dap-body (%dap-request stream "scopes" "frameId" top)
                                                "scopes")
                                     0))
                        (vars (%dap-body (%dap-request stream "variables" "variablesReference"
                                                       (dotcl-dap::jget scope "variablesReference"))
                                         "variables")))
                   (note :variables (map 'list (lambda (v)
                                                 (list (dotcl-dap::jget v "name")
                                                       (dotcl-dap::jget v "value")))
                                         vars)))
                 (flet ((ev (expr &optional frame)
                          (let ((r (if frame
                                       (%dap-request stream "evaluate" "expression" expr
                                                     "frameId" frame "context" "repl")
                                       (%dap-request stream "evaluate" "expression" expr
                                                     "context" "repl"))))
                            (if (eq (dotcl-dap::jget r "success") t)
                                (%dap-body r "result")
                                (list :failed (dotcl-dap::jget r "message"))))))
                   (note :eval-top (ev "(+ x y)" top))
                   (note :eval-thread (ev "(dotcl:thread-name (dotcl:current-thread))" top))
                   (note :eval-no-frame (ev "(+ 1 2)"))
                   (note :eval-error (car (ev "(car 5)" top)))
                   (when caller
                     (note :eval-caller (ev "(list n m)" (dotcl-dap::jget caller "id"))))))
               (note :continue (dotcl-dap::jget (%dap-request stream "continue" "threadId" tid)
                                                "success"))
               (note :not-stopped-after-continue
                     (not (member worker (dotcl-dap:stopped-threads))))
               (note :worker-result (dotcl-thread:thread-join worker))
               (note :disconnect (dotcl-dap::jget (%dap-request stream "disconnect") "success"))
               (note :after-disconnect (dap-test-target 1 2))))
        (setf spinning nil)
        (when spinner (dotcl-thread:thread-join spinner))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)))
    (nreverse out)))


(defvar *dap-client-errors* '())

;;; An error on this thread would reach the "uncaught" filter of the session it
;;; is the client of, and wait there for itself.
(defun %dap-guard (scenario)
  (handler-case (funcall scenario)
    (error (e) (push (princ-to-string e) *dap-client-errors*) nil)))

(defvar *dap-result* (%dap-guard #'%dap-scenario))

(defun %dap-get (key) (cdr (assoc key *dap-result*)))

(deftest dotcl-dap.initialize
  (list (%dap-get :initialize) (%dap-get :initialized) (%dap-get :attach))
  ((t t) t t))

(deftest dotcl-dap.function-breakpoints
  (%dap-get :breakpoints)
  (t :false))

(deftest dotcl-dap.stopped-only-this-thread
  (%dap-get :stopped)
  ("function breakpoint" :false))

(deftest dotcl-dap.others-keep-running
  (%dap-get :others-run)
  (t 200 t))

(deftest dotcl-dap.threads
  (%dap-get :threads)
  t)

(deftest dotcl-dap.top-frame
  (%dap-get :top-frame)
  "(DAP-TEST-TARGET 4 40)")

(deftest dotcl-dap.top-frame-variables
  (%dap-get :variables)
  (("X" "4") ("Y" "40")))

(deftest dotcl-dap.evaluate-in-frame
  (list (%dap-get :eval-top) (%dap-get :eval-thread)
        (%dap-get :eval-no-frame) (%dap-get :eval-error))
  ("44" "\"dap-test-worker\"" "3" :failed))

(deftest dotcl-dap.continue
  (list (%dap-get :continue) (%dap-get :worker-result))
  (t 45))

(deftest dotcl-dap.not-stopped-after-continue
  (%dap-get :not-stopped-after-continue)
  t)

(deftest dotcl-dap.disconnect-restores
  (list (%dap-get :disconnect) (%dap-get :after-disconnect))
  (t 3))

;;; The caller's locals are recorded only by compiled code.
(deftest-compiled-only dotcl-dap.caller-frame
  (list (%dap-get :caller-frame) (%dap-get :eval-caller))
  ("(DAP-TEST-CALLER 4)" "(4 40)"))

;;; Second session.

(defparameter *dap-fixture* "test/regression/dap-fixture-tmp.lisp")

(with-open-file (s *dap-fixture* :direction :output :if-exists :supersede)
  (write-line "(in-package :cl-user)" s)
  (write-line "" s)
  (write-line "(defun dap-rec-fact (n) (if (<= n 1) 1 (* n (dap-rec-fact (1- n)))))" s))
(load *dap-fixture*)

(defun dap-redef-target (x) (* x 2))
(eval '(defun dap-rec-no-source (n) (if (<= n 1) 1 (* n (dap-rec-no-source (1- n))))))
(defun dap-err-fn (x)
  (restart-case (error "dap test failure ~A" x)
    (use-value (v) (list :used v))))
(defun dap-cerror-fn ()
  (cerror "Go on." "dap test soft error")
  :after)

(defun %dap-open (server)
  (let ((socket (dotcl-bsd-sockets:tcp-connect "127.0.0.1" (dotcl-dap:server-port server)
                                               :timeout 5)))
    (setf (dotcl-bsd-sockets:sockopt-receive-timeout socket) 10)
    (dotcl-bsd-sockets:socket-make-stream socket :element-type '(unsigned-byte 8))))

(defun %dap-join (thread &optional (seconds 20))
  "THREAD's result, or :TIMEOUT when it is still running after SECONDS."
  (loop repeat (* seconds 100)
        while (dotcl-thread:thread-alive-p thread)
        do (sleep 0.01))
  (if (dotcl-thread:thread-alive-p thread)
      :timeout
      (dotcl-thread:thread-join thread)))

(defun %dap-run-out (stream thread &optional (seconds 20))
  "Continue THREAD each time it stops until it finishes. Returns the number
of stops after the first and THREAD's result."
  (let ((stops 0))
    (loop repeat (* seconds 100)
          while (dotcl-thread:thread-alive-p thread)
          do (if (member thread (dotcl-dap:stopped-threads))
                 (let ((tid (%dap-body (%dap-event stream "stopped") "threadId")))
                   (incf stops)
                   (%dap-request stream "continue" "threadId" tid))
                 (sleep 0.01)))
    (list stops (%dap-join thread 1))))

(defun %dap-top-frame (stream tid)
  (aref (%dap-body (%dap-request stream "stackTrace" "threadId" tid) "stackFrames") 0))

(defun %dap-scenario-2 ()
  (setf *dap-test-events* '())
  (let* ((server (dotcl-dap:start-server :port 0))
         (stream (%dap-open server))
         (out '()))
    (flet ((note (key value) (push (cons key value) out))
           (ok (r) (dotcl-dap::jget r "success"))
           (stopped ()
             (let ((m (%dap-event stream "stopped")))
               (values (%dap-body m "threadId") m)))
           (worker (fn) (dotcl-thread:make-thread fn :name "dap-test-worker-2")))
      (unwind-protect
           (let ((init (%dap-request stream "initialize" "adapterID" "dotcl")))
             (note :filters (map 'list (lambda (f) (dotcl-dap::jget f "filter"))
                                 (%dap-body init "exceptionBreakpointFilters")))
             (%dap-request stream "attach")
             (let ((r (%dap-request stream "setFunctionBreakpoints"
                                    "breakpoints" (vector (list :obj "name" "dap-rec-fact")
                                                          (list :obj "name" "dap-redef-target")
                                                          (list :obj "name" "dap-rec-no-source")))))
               (note :bp2 (map 'list (lambda (b) (list (dotcl-dap::jget b "verified")
                                                       (and (dotcl-dap::jget b "message") t)))
                               (%dap-body r "breakpoints"))))
             (note :notinline (dotcl:global-notinline-p 'dap-rec-fact))
             (note :exception-filters
                   (ok (%dap-request stream "setExceptionBreakpoints"
                                     "filters" (vector "uncaught"))))
             (%dap-request stream "configurationDone")
             ;; Compiled before the breakpoint: only the outer call stops.
             (let ((w (worker (lambda () (dap-rec-fact 3)))))
               (let* ((tid (stopped))
                      (top (%dap-top-frame stream tid)))
                 (note :source (list (file-namestring
                                      (dotcl-dap::jget (dotcl-dap::jget top "source") "path"))
                                     (dotcl-dap::jget top "line")))
                 (%dap-request stream "continue" "threadId" tid))
               ;; Whether the inner calls stop too depends on how the body was
               ;; compiled (they do when the evaluator calls through the name).
               (note :before-redefinition (second (%dap-run-out stream w))))
             ;; Defined again while the breakpoint is set: each recursive call stops.
             (eval '(defun dap-rec-fact (n) (if (<= n 1) 1 (* n (dap-rec-fact (1- n))))))
             (let ((w (worker (lambda () (dap-rec-fact 3))))
                   (tops '()))
               (dotimes (i 3)
                 (let* ((tid (stopped))
                        (top (%dap-top-frame stream tid)))
                   (push (dotcl-dap::jget top "name") tops)
                   (%dap-request stream "continue" "threadId" tid)))
               (note :recursion (nreverse tops))
               (note :recursion-result (%dap-join w)))
             ;; A definition made while the breakpoint is set is wrapped too.
             (eval '(defun dap-redef-target (x) (* x 3)))
             (let ((w (worker (lambda () (dap-redef-target 5)))))
               (let ((tid (stopped)))
                 (note :redefined-top (dotcl-dap::jget (%dap-top-frame stream tid) "name"))
                 (%dap-request stream "continue" "threadId" tid))
               (note :redefined-result (%dap-join w)))
             ;; An unhandled error: exceptionInfo, the restarts, then USE-VALUE.
             (let ((w (worker (lambda () (dap-err-fn 1)))))
               (multiple-value-bind (tid m) (stopped)
                 (note :exception-stop (list (%dap-body m "reason") (%dap-body m "description")))
                 (note :exception-frames
                       (map 'list (lambda (f) (dotcl-dap::jget f "name"))
                            (%dap-body (%dap-request stream "stackTrace" "threadId" tid)
                                       "stackFrames")))
                 (let ((info (%dap-request stream "exceptionInfo" "threadId" tid)))
                   (note :exception-info (list (%dap-body info "exceptionId")
                                               (%dap-body info "description"))))
                 (let* ((restarts (%dap-body (%dap-request stream "dotcl/restarts" "threadId" tid)
                                             "restarts"))
                        (names (map 'list (lambda (r) (dotcl-dap::jget r "name")) restarts)))
                   (note :restart-names (and (member "USE-VALUE" names :test #'string=) t))
                   (let ((listing (%dap-request stream "evaluate" "expression" ":restarts"
                                                "context" "repl")))
                     (note :restarts-command (and (search "USE-VALUE" (%dap-body listing "result"))
                                                  t)))
                   (let* ((top (dotcl-dap::jget (%dap-top-frame stream tid) "id"))
                          (scopes (%dap-body (%dap-request stream "scopes" "frameId" top)
                                             "scopes")))
                     (note :scopes (map 'list (lambda (sc) (dotcl-dap::jget sc "name")) scopes)))
                   (note :bad-restart
                         (ok (%dap-request stream "dotcl/invokeRestart" "threadId" tid
                                           "index" 99)))
                   (note :invoke-restart
                         (ok (%dap-request stream "dotcl/invokeRestart" "threadId" tid
                                           "index" (position "USE-VALUE" names :test #'string=)
                                           "argumentText" " (* 6 7) ")))))
               (note :restart-result (%dap-join w)))
             ;; The same through evaluate.
             (let ((w (worker (lambda () (dap-err-fn 2)))))
               (stopped)
               (let ((r (%dap-request stream "evaluate" "expression" ":restart 0 :via-evaluate"
                                      "context" "repl")))
                 (note :evaluate-restart (ok r)))
               (note :evaluate-restart-result (%dap-join w)))
             ;; Continue takes CONTINUE when there is one.
             (let ((w (worker #'dap-cerror-fn)))
               (let ((tid (stopped)))
                 (%dap-request stream "continue" "threadId" tid))
               (note :continue-restart (%dap-join w)))
             ;; Clearing the breakpoints puts the definitions and proclamations back.
             (%dap-request stream "setFunctionBreakpoints" "breakpoints" #())
             (note :cleared (list (dap-redef-target 5) (dap-rec-fact 4)
                                  (dotcl:global-notinline-p 'dap-rec-fact)))
             (%dap-request stream "disconnect")
             (note :hook-restored (not (eq (dotcl:symbol-global-value '*debugger-hook*)
                                           #'dotcl-dap::%debugger-hook))))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)))
    (nreverse out)))

(defvar *dap-result-2* (%dap-guard #'%dap-scenario-2))
(ignore-errors (delete-file *dap-fixture*))

(defun %dap-get-2 (key) (cdr (assoc key *dap-result-2*)))

(deftest dotcl-dap.exception-filter-offered
  (list (%dap-get-2 :filters) (%dap-get-2 :exception-filters))
  (("uncaught") t))

(deftest dotcl-dap.breakpoint-on-recursive-function
  (list (mapcar #'first (%dap-get-2 :bp2)) (%dap-get-2 :notinline))
  ((t t t) t))

;;; Cross references, which tell a function that calls itself, are recorded by
;;; the compiler: a DEFUN is compiled whenever there is an emitter, a DEFUN
;;; given to EVAL only in compiled mode.
(deftest-emitting-only dotcl-dap.breakpoint-on-recursive-function-message
  (second (first (%dap-get-2 :bp2)))
  t)

(deftest-compiled-only dotcl-dap.breakpoint-on-recursive-function-without-source
  (third (%dap-get-2 :bp2))
  (t t))

(deftest dotcl-dap.self-calls-stop
  (list (%dap-get-2 :before-redefinition) (%dap-get-2 :recursion) (%dap-get-2 :recursion-result))
  (6 ("(DAP-REC-FACT 3)" "(DAP-REC-FACT 2)" "(DAP-REC-FACT 1)") 6))

(deftest dotcl-dap.frame-source
  (%dap-get-2 :source)
  ("dap-fixture-tmp.lisp" 3))

(deftest dotcl-dap.redefinition-stops
  (list (%dap-get-2 :redefined-top) (%dap-get-2 :redefined-result))
  ("(DAP-REDEF-TARGET 5)" 15))

(deftest dotcl-dap.exception-stop
  (list (%dap-get-2 :exception-stop) (%dap-get-2 :exception-info))
  (("exception" "dap test failure 1") ("SIMPLE-ERROR" "dap test failure 1")))

(deftest dotcl-dap.exception-frames
  (first (%dap-get-2 :exception-frames))
  "(DAP-ERR-FN 1)")

(deftest dotcl-dap.restarts
  (list (%dap-get-2 :restart-names) (%dap-get-2 :restarts-command) (%dap-get-2 :scopes))
  (t t ("Locals" "Condition" "Restarts")))

(deftest dotcl-dap.invoke-restart
  (list (%dap-get-2 :bad-restart) (%dap-get-2 :invoke-restart) (%dap-get-2 :restart-result))
  (:false t (:used 42)))

(deftest dotcl-dap.restart-through-evaluate
  (list (%dap-get-2 :evaluate-restart) (%dap-get-2 :evaluate-restart-result))
  (t (:used :via-evaluate)))

(deftest dotcl-dap.continue-takes-continue-restart
  (%dap-get-2 :continue-restart)
  :after)

(deftest dotcl-dap.cleared
  (list (%dap-get-2 :cleared) (%dap-get-2 :hook-restored))
  ((15 24 nil) t))

;;; Third session: line breakpoints in a file that was loaded.

(defparameter *dap-line-fixture* "test/regression/dap-line-fixture-tmp.lisp")

(with-open-file (s *dap-line-fixture* :direction :output :if-exists :supersede)
  (format s "(in-package :cl-user)~%~%(defun dap-line-fn (x)~%  (let ((y (* x 2)))~%    (dap-line-note y)~%    (+ y~%       (dap-line-helper x))))~%~%(defun dap-line-helper (z) (* z 100))~%(defun dap-line-note (v) v)~%"))
(load *dap-line-fixture*)

(defun %dap-scenario-3 ()
  (setf *dap-test-events* '())
  (let* ((server (dotcl-dap:start-server :port 0))
         (stream (%dap-open server))
         (path (namestring (truename *dap-line-fixture*)))
         (out '()))
    (flet ((note (key value) (push (cons key value) out))
           (stopped ()
             (let ((m (%dap-event stream "stopped")))
               (values (%dap-body m "threadId") m))))
      (unwind-protect
           (progn
             (%dap-request stream "initialize" "adapterID" "dotcl")
             (%dap-request stream "attach")
             (let ((r (%dap-request stream "setBreakpoints"
                                    "source" (list :obj "path" path)
                                    "breakpoints" (map 'vector (lambda (n) (list :obj "line" n))
                                                       '(1 3 5 7 9)))))
               (note :line-bps (map 'list (lambda (b) (list (dotcl-dap::jget b "line")
                                                            (dotcl-dap::jget b "verified")))
                                    (%dap-body r "breakpoints"))))
             (%dap-request stream "configurationDone")
             (when (find :dotcl-emit *features*)
               (let ((w (dotcl-thread:make-thread (lambda () (dap-line-fn 3))
                                                  :name "dap-test-worker-3"))
                     (stops '()))
                 (dotimes (i 3)
                   (multiple-value-bind (tid m) (stopped)
                     (let* ((top (%dap-top-frame stream tid))
                            (fid (dotcl-dap::jget top "id"))
                            (scope (aref (%dap-body (%dap-request stream "scopes" "frameId" fid)
                                                    "scopes")
                                         0))
                            (vars (%dap-body (%dap-request stream "variables" "variablesReference"
                                                           (dotcl-dap::jget scope "variablesReference"))
                                             "variables"))
                            (ev (%dap-request stream "evaluate"
                                              "expression" (format nil "(* 10 ~A)"
                                                                   (dotcl-dap::jget (aref vars 0) "name"))
                                              "frameId" fid "context" "repl")))
                       (push (list (%dap-body m "reason")
                                   (and (%dap-body m "hitBreakpointIds") t)
                                   (dotcl-dap::jget top "line")
                                   (file-namestring
                                    (dotcl-dap::jget (dotcl-dap::jget top "source") "path"))
                                   (map 'list (lambda (v) (list (dotcl-dap::jget v "name")
                                                                (dotcl-dap::jget v "value")))
                                        vars)
                                   (%dap-body ev "result"))
                             stops))
                     (%dap-request stream "continue" "threadId" tid)))
                 (note :line-stops (nreverse stops))
                 (note :line-result (second (%dap-run-out stream w))))
               ;; Stepping from a line breakpoint: next goes to the following
               ;; lines of the same function, then the function returns.
               (%dap-request stream "setBreakpoints" "source" (list :obj "path" path)
                             "breakpoints" (vector (list :obj "line" 5)))
               (let ((w (dotcl-thread:make-thread (lambda () (dap-line-fn 3))
                                                  :name "dap-test-worker-3s"))
                     (steps '()))
                 (dotimes (i 3)
                   (multiple-value-bind (tid m) (stopped)
                     (push (list (%dap-body m "reason")
                                 (dotcl-dap::jget (%dap-top-frame stream tid) "line"))
                           steps)
                     (%dap-request stream "next" "threadId" tid)))
                 (note :steps (nreverse steps))
                 (note :step-result (%dap-run-out stream w))))
             (%dap-request stream "setBreakpoints" "source" (list :obj "path" path)
                           "breakpoints" #())
             (let ((w (dotcl-thread:make-thread (lambda () (dap-line-fn 1))
                                                :name "dap-test-worker-3b")))
               (note :line-cleared (%dap-run-out stream w)))
             (%dap-request stream "disconnect"))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)))
    (nreverse out)))

(defvar *dap-result-3* (%dap-guard #'%dap-scenario-3))
(ignore-errors (delete-file *dap-line-fixture*))

(defun %dap-get-3 (key) (cdr (assoc key *dap-result-3*)))

(deftest-emitting-only dotcl-dap.line-breakpoints-verified
  (%dap-get-3 :line-bps)
  ((1 :false) (3 :false) (5 t) (7 t) (9 t)))

(deftest dotcl-dap.line-breakpoints-not-placed-without-emitter
  (if (emitting-mode-p)
      t
      (every (lambda (b) (eq (second b) :false)) (%dap-get-3 :line-bps)))
  t)

;;; Line 5 is the call in the LET body, line 7 the form that starts the next
;;; line, and line 9 the body of the helper it calls.
(deftest-emitting-only dotcl-dap.line-breakpoint-stops
  (%dap-get-3 :line-stops)
  (("breakpoint" t 5 "dap-line-fixture-tmp.lisp" (("X" "3") ("Y" "6")) "30")
   ("breakpoint" t 7 "dap-line-fixture-tmp.lisp" (("X" "3") ("Y" "6")) "30")
   ("breakpoint" t 9 "dap-line-fixture-tmp.lisp" (("Z" "3")) "30")))

(deftest-emitting-only dotcl-dap.line-breakpoint-continue
  (%dap-get-3 :line-result)
  306)

(deftest-emitting-only dotcl-dap.next
  (list (%dap-get-3 :steps) (%dap-get-3 :step-result))
  ((("breakpoint" 5) ("step" 6) ("step" 7)) (0 306)))

(deftest dotcl-dap.line-breakpoints-cleared
  (%dap-get-3 :line-cleared)
  (0 102))

(deftest dotcl-dap.client-errors
  *dap-client-errors*
  nil)
