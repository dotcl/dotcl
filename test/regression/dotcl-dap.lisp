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

(defvar *dap-log* (make-string-output-stream))
(defvar *dap-log-text* "")

(defun %dap-log-lines ()
  (dotcl-thread:with-lock-held (dotcl-dap::*log-lock*)
    (setf *dap-log-text*
          (concatenate 'string *dap-log-text* (get-output-stream-string *dap-log*))))
  (let ((lines '()) (start 0))
    (loop for nl = (position #\Newline *dap-log-text* :start start)
          while nl do (push (subseq *dap-log-text* start nl) lines) (setf start (1+ nl)))
    (nreverse lines)))

(defun %dap-wait-log (prefix)
  (loop repeat 500
        do (let ((hit (find-if (lambda (l) (eql 0 (search prefix l))) (%dap-log-lines))))
             (when hit (return hit)))
           (sleep 0.01)))

(defun %dap-body (m key) (dotcl-dap::jget (dotcl-dap::jget m "body") key))

(defun %dap-stopped-and-announced (stream)
  "The next stopped event, and whether a thread started event for its thread
came before it. The other events read on the way are kept for %DAP-EVENT."
  (let ((started '()) (kept '()))
    (flet ((take (m)
             (cond ((equal (dotcl-dap::jget m "event") "stopped") m)
                   (t (when (and (equal (dotcl-dap::jget m "event") "thread")
                                 (equal (%dap-body m "reason") "started"))
                        (push (%dap-body m "threadId") started))
                      (push m kept)
                      nil))))
      (let ((stopped (or (loop for m = (pop *dap-test-events*) while m
                               thereis (take m))
                         (loop thereis (take (%dap-read stream))))))
        (setf *dap-test-events* (append (nreverse kept) *dap-test-events*))
        (values stopped (and (member (%dap-body stopped "threadId") started) t))))))

(defun %dap-scenario ()
  (let* ((server (progn (setf dotcl-dap:*log-stream* *dap-log*)
                        (dotcl-dap:start-server :port 0)))
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
           (let ((init (%dap-request stream "initialize" "adapterID" "dotcl"
                                       "clientName" "Test Client")))
             (note :connected (and (%dap-wait-log "dotcl-dap: client connected from 127.0.0.1:") t))
             (note :initialize (list (dotcl-dap::jget init "success")
                                     (%dap-body init "supportsFunctionBreakpoints")))
             (note :initialized (and (%dap-event stream "initialized") t))
             (note :attach (dotcl-dap::jget (%dap-request stream "attach") "success"))
             (let ((out (%dap-event stream "output")))
               (note :output-event (list (%dap-body out "category")
                                         (let ((text (%dap-body out "output")))
                                           (and (eql 0 (search "Attached to dotcl " text))
                                                (search "(pid " text)
                                                t)))))
             (note :attached-log (%dap-wait-log "dotcl-dap: attached"))
             (let ((r (%dap-request stream "setFunctionBreakpoints"
                                    "breakpoints" (vector (list :obj "name" "dap-test-target")
                                                          (list :obj "name" "no-such-fn-here")))))
               (note :breakpoints (map 'list (lambda (b) (dotcl-dap::jget b "verified"))
                                       (%dap-body r "breakpoints"))))
             (%dap-request stream "setExceptionBreakpoints" "filters" #())
             (%dap-request stream "configurationDone")
             (let ((threads (%dap-body (%dap-request stream "threads") "threads")))
               (note :attach-threads
                     (list (and (plusp (length threads)) t)
                           (and (find (dotcl-dap::thread-id (dotcl:current-thread)) threads
                                      :key (lambda (th) (dotcl-dap::jget th "id")))
                                t)
                           (equal (dotcl-dap::jget (elt threads 0) "id")
                                  (dotcl-dap::thread-id (dotcl:current-thread)))
                           (and (find "dap-server" threads
                                      :key (lambda (th) (dotcl-dap::jget th "name"))
                                      :test #'equal)
                                t))))
             (setf spinner (dotcl-thread:make-thread
                            (lambda () (loop while spinning do (incf spins) (sleep 0.01)))
                            :name "dap-test-spinner"))
             (setf worker (dotcl-thread:make-thread (lambda () (dap-test-caller 4))
                                                    :name "dap-test-worker"))
             (multiple-value-bind (stopped announced) (%dap-stopped-and-announced stream)
              (note :announced-before-stop announced)
              (let ((tid (%dap-body stopped "threadId")))
               (note :stopped (list (%dap-body stopped "reason")
                                    (%dap-body stopped "allThreadsStopped")))
               (let ((event (%dap-event stream "thread")))
                 (note :thread-event (list (%dap-body event "reason")
                                           (integerp (%dap-body event "threadId")))))
               (note :running-stack
                     (let ((id (dotcl-dap::thread-id spinner)))
                       (let ((r (%dap-request stream "stackTrace" "threadId" id)))
                         (list (dotcl-dap::jget r "success")
                               (length (%dap-body r "stackFrames"))))))
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
               (note :stop-log (%dap-wait-log "dotcl-dap: thread"))
               (note :disconnect (dotcl-dap::jget (%dap-request stream "disconnect") "success"))
               (note :disconnected-log (%dap-wait-log "dotcl-dap: client disconnected"))
               (note :after-disconnect (dap-test-target 1 2)))))
        (setf spinning nil)
        (when spinner (dotcl-thread:thread-join spinner))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)
        (setf dotcl-dap:*log-stream* nil)))
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

(deftest dotcl-dap.attach-threads
  (%dap-get :attach-threads)
  (t t t t))

(deftest dotcl-dap.thread-event
  (%dap-get :thread-event)
  ("started" t))

(deftest dotcl-dap.thread-announced-before-stop
  (%dap-get :announced-before-stop)
  t)

(deftest dotcl-dap.running-thread-stack
  (%dap-get :running-stack)
  (t 0))

(deftest dotcl-dap.log-connect
  (%dap-get :connected)
  t)

(deftest dotcl-dap.log-attach
  (%dap-get :attached-log)
  "dotcl-dap: attached (Test Client)")

(deftest dotcl-dap.log-stop
  (%dap-get :stop-log)
  "dotcl-dap: thread \"dap-test-worker\" stopped at DAP-TEST-TARGET (function breakpoint)")

(deftest dotcl-dap.log-disconnect
  (%dap-get :disconnected-log)
  "dotcl-dap: client disconnected")

(deftest dotcl-dap.output-event
  (%dap-get :output-event)
  ("console" t))

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
(defvar *dap-unbound-var*)
(defun dap-unbound-fn () *dap-unbound-var*)
(defun dap-missing-call () (dap-no-such-runtime-fn 7))

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
             ;; The runtime's own restarts reach DAP: unbound variable.
             (makunbound '*dap-unbound-var*)
             (let ((w (worker #'dap-unbound-fn)))
               (let* ((tid (stopped))
                      (restarts (%dap-body (%dap-request stream "dotcl/restarts"
                                                         "threadId" tid)
                                           "restarts"))
                      (names (map 'list (lambda (r) (dotcl-dap::jget r "name"))
                                  restarts)))
                 (note :unbound-restart-names
                       (list (and (member "USE-VALUE" names :test #'string=) t)
                             (and (member "STORE-VALUE" names :test #'string=) t)))
                 (note :unbound-restart
                       (ok (%dap-request stream "dotcl/invokeRestart"
                                         "threadId" tid
                                         "index" (position "USE-VALUE" names
                                                           :test #'string=)
                                         "argumentText" " 41 "))))
               (note :unbound-restart-result (%dap-join w)))
             ;; The runtime's own restarts reach DAP: undefined function.
             (let ((w (worker #'dap-missing-call)))
               (let* ((tid (stopped))
                      (restarts (%dap-body (%dap-request stream "dotcl/restarts"
                                                         "threadId" tid)
                                           "restarts"))
                      (names (map 'list (lambda (r) (dotcl-dap::jget r "name"))
                                  restarts)))
                 (note :undefined-restart-names
                       (list (and (member "USE-VALUE" names :test #'string=) t)
                             (and (find-if (lambda (n) (search "RETURN-VALUE" n))
                                           names)
                                  t)
                             (and (member "CONTINUE" names :test #'string=) t)))
                 (note :undefined-restart
                       (ok (%dap-request stream "dotcl/invokeRestart"
                                         "threadId" tid
                                         "index" (position "USE-VALUE" names
                                                           :test #'string=)
                                         "argumentText" " (lambda (x) (* x 6)) "))))
               (note :undefined-restart-result (%dap-join w)))
             ;; Continue takes CONTINUE when there is one.
             (let ((w (worker #'dap-cerror-fn)))
               (let ((tid (stopped)))
                 (%dap-request stream "continue" "threadId" tid))
               (note :continue-restart (%dap-join w)))
             ;; The "error" filter stops where the error is signaled, handled or not.
             (%dap-request stream "setExceptionBreakpoints" "filters" (vector "uncaught" "error"))
             (note :bos-set (eq (dotcl:symbol-global-value '*break-on-signals*) 'error))
             (let ((w (worker (lambda () (handler-case (dap-err-fn 4) (error () :handled))))))
               (multiple-value-bind (tid m) (stopped)
                 (note :signal-stop (list (%dap-body m "reason") (%dap-body m "description")))
                 (note :signal-break-mode
                       (%dap-body (%dap-request stream "exceptionInfo" "threadId" tid) "breakMode"))
                 (note :signal-restarts
                       (and (find "USE-VALUE"
                                  (map 'list (lambda (r) (dotcl-dap::jget r "name"))
                                       (%dap-body (%dap-request stream "dotcl/restarts"
                                                                "threadId" tid)
                                                  "restarts"))
                                  :test #'string=)
                            t))
                 (%dap-request stream "continue" "threadId" tid))
               (note :signal-continue (%dap-join w))
               (note :signal-stops-left (length (dotcl-dap:stopped-threads))))
             (let ((w (worker (lambda () (handler-case (dap-err-fn 5) (error () :handled))))))
               (let ((tid (stopped)))
                 (%dap-request stream "evaluate" "expression" ":restart 0 :picked" "context" "repl")
                 (note :signal-restart tid))
               (note :signal-restart-result (%dap-join w)))
             (note :signal-then-unhandled
                   (let ((w (worker (lambda () (dap-err-fn 6)))))
                     (let ((tid (stopped)))
                       (%dap-request stream "continue" "threadId" tid))
                     ;; First chance at the signal, second chance when nothing handled it.
                     (let ((tid (stopped)))
                       (%dap-request stream "evaluate" "expression" ":restart 0 :end" "context" "repl")
                       (list (integerp tid) (%dap-join w)))))
             (%dap-request stream "setExceptionBreakpoints" "filters" (vector "uncaught"))
             (note :bos-restored (null (dotcl:symbol-global-value '*break-on-signals*)))
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
  (("uncaught" "error") t))

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

(deftest dotcl-dap.unbound-variable-restarts
  (list (%dap-get-2 :unbound-restart-names)
        (%dap-get-2 :unbound-restart)
        (%dap-get-2 :unbound-restart-result))
  ((t t) t 41))

(deftest dotcl-dap.undefined-function-restarts
  (list (%dap-get-2 :undefined-restart-names)
        (%dap-get-2 :undefined-restart)
        (%dap-get-2 :undefined-restart-result))
  ((t t t) t 42))

(deftest dotcl-dap.continue-takes-continue-restart
  (%dap-get-2 :continue-restart)
  :after)

(deftest dotcl-dap.signal-stop
  (list (%dap-get-2 :bos-set) (%dap-get-2 :signal-stop) (%dap-get-2 :signal-break-mode)
        (%dap-get-2 :signal-restarts) (%dap-get-2 :signal-continue)
        (%dap-get-2 :signal-stops-left))
  (t ("exception" "dap test failure 4") "always" t :handled 0))

(deftest dotcl-dap.signal-restart
  (%dap-get-2 :signal-restart-result)
  (:used :picked))

(deftest dotcl-dap.signal-then-unhandled-stops-again
  (list (%dap-get-2 :signal-then-unhandled) (%dap-get-2 :bos-restored))
  ((t (:used :end)) t))

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

;;; Fourth session: DEFMETHOD lines, and breakpoints that follow a file being
;;; loaded. The breakpoints are set before the file is first loaded; loading
;;; it places them (and tells the client), loading it again places them again.

(defparameter *dap-method-fixture* "test/regression/dap-method-fixture-tmp.lisp")

(with-open-file (s *dap-method-fixture* :direction :output :if-exists :supersede)
  (format s "(in-package :cl-user)~%~%(defgeneric dap-m-gf (x))~%(defmethod dap-m-gf ((x integer))~%  (let ((m (* x 7)))~%    (cons m 1)))~%(defmethod dap-m-gf ((x string))~%  (length x))~%(defmethod dap-m-gf :around ((x integer))~%  (list (call-next-method)))~%(defmacro dap-m-mac (a)~%  (list 'quote a))~%(defun dap-m-fn (x)~%  (let ((r (* x 2)))~%    (cons r 1)))~%"))

(defun %dap-stops-of (stream thread)
  "Run THREAD to its end, continuing at each stop. Returns (line locals) of
each stop, then the result."
  (let ((stops '()))
    (loop repeat 2000
          while (dotcl-thread:thread-alive-p thread)
          do (if (member thread (dotcl-dap:stopped-threads))
                 (let* ((tid (%dap-body (%dap-event stream "stopped") "threadId"))
                        (top (%dap-top-frame stream tid))
                        (scope (aref (%dap-body (%dap-request stream "scopes" "frameId"
                                                              (dotcl-dap::jget top "id"))
                                                "scopes")
                                     0))
                        (vars (%dap-body (%dap-request stream "variables" "variablesReference"
                                                       (dotcl-dap::jget scope "variablesReference"))
                                         "variables")))
                   (push (list (dotcl-dap::jget top "line")
                               (map 'list (lambda (v) (list (dotcl-dap::jget v "name")
                                                            (dotcl-dap::jget v "value")))
                                    vars))
                         stops)
                   (%dap-request stream "continue" "threadId" tid))
                 (sleep 0.01)))
    (list (nreverse stops) (%dap-join thread 1))))

(defun %dap-scenario-4 ()
  (setf *dap-test-events* '())
  (let* ((server (dotcl-dap:start-server :port 0))
         (stream (%dap-open server))
         (path (namestring (truename *dap-method-fixture*)))
         (original-load (fdefinition 'load))
         (out '()))
    (flet ((note (key value) (push (cons key value) out))
           (call (fn) (%dap-stops-of stream (dotcl-thread:make-thread fn :name "dap-test-worker-4")))
           (set-lines (lines)
             (let ((r (%dap-request stream "setBreakpoints"
                                    "source" (list :obj "path" path)
                                    "breakpoints" (map 'vector (lambda (n) (list :obj "line" n))
                                                       lines))))
               (map 'list (lambda (b) (list (dotcl-dap::jget b "line")
                                            (dotcl-dap::jget b "verified")))
                    (%dap-body r "breakpoints")))))
      (unwind-protect
           (progn
             (%dap-request stream "initialize" "adapterID" "dotcl")
             (%dap-request stream "attach")
             (note :before-load (set-lines '(6 8 10 12 15)))
             (%dap-request stream "configurationDone")
             (when (find :dotcl-emit *features*)
               (note :load-wrapped (not (eq (fdefinition 'load) original-load)))
               (load *dap-method-fixture*)
               (note :events
                     (sort (loop repeat 4
                                 collect (let ((b (%dap-body (%dap-event stream "breakpoint")
                                                             "breakpoint")))
                                           (list (dotcl-dap::jget b "line")
                                                 (dotcl-dap::jget b "verified"))))
                           #'< :key #'first))
               (note :primary (call (lambda () (dap-m-gf 3))))
               (note :string (call (lambda () (dap-m-gf "abc"))))
               (note :function (call (lambda () (dap-m-fn 4))))
               (load *dap-method-fixture*)
               (note :after-reload (call (lambda () (dap-m-gf 3))))
               (note :function-after-reload (call (lambda () (dap-m-fn 4))))
               (set-lines '())
               (note :cleared (list (call (lambda () (dap-m-gf 3)))
                                    (call (lambda () (dap-m-gf "ab")))
                                    (length (generic-function-methods #'dap-m-gf)))))
             (%dap-request stream "disconnect")
             (note :load-restored (eq (fdefinition 'load) original-load)))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)
        (dotcl:without-package-locks (setf (fdefinition 'load) original-load))))
    (nreverse out)))

(defvar *dap-result-4* (%dap-guard #'%dap-scenario-4))
(ignore-errors (delete-file *dap-method-fixture*))

(defun %dap-get-4 (key) (cdr (assoc key *dap-result-4*)))

(deftest dotcl-dap.waiting-breakpoints-not-verified
  (mapcar #'second (%dap-get-4 :before-load))
  (:false :false :false :false :false))

(deftest-emitting-only dotcl-dap.load-places-breakpoints
  (list (%dap-get-4 :load-wrapped) (%dap-get-4 :events))
  (t ((6 t) (8 t) (10 t) (15 t))))

;;; The AROUND method (line 10) runs before the primary one (line 6).
(deftest-emitting-only dotcl-dap.method-breakpoints
  (list (%dap-get-4 :primary) (%dap-get-4 :string) (%dap-get-4 :function))
  ((((10 (("X" "3"))) (6 (("X" "3") ("M" "21")))) ((21 . 1)))
   (((8 (("X" "\"abc\"")))) 3)
   (((15 (("X" "4") ("R" "8")))) (8 . 1))))

(deftest-emitting-only dotcl-dap.reload-replaces-breakpoints
  (list (%dap-get-4 :after-reload) (%dap-get-4 :function-after-reload))
  ((((10 (("X" "3"))) (6 (("X" "3") ("M" "21")))) ((21 . 1)))
   (((15 (("X" "4") ("R" "8")))) (8 . 1))))

(deftest-emitting-only dotcl-dap.method-breakpoints-cleared
  (%dap-get-4 :cleared)
  ((nil ((21 . 1))) (nil 2) 3))

(deftest dotcl-dap.file-functions-restored
  (%dap-get-4 :load-restored)
  t)

;;; Fifth session: a function breakpoint on a function loaded from a FASL whose
;;; parameter name the loading process has never interned (uninterned between
;;; COMPILE-FILE and LOAD, as in a fresh process reading a cached FASL).
;;; VARIABLES names the parameter as written, not as #:DAP-FASL-N.

(defun %dap-fasl-fixture ()
  (let ((src (regression-temp-file "dap-fasl-fixture.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(in-package :cl-user)
(defun dap-fasl-fact (dap-fasl-n) (if (<= dap-fasl-n 1) 1 (* dap-fasl-n (dap-fasl-fact (1- dap-fasl-n)))))
" s))
    (let ((fasl (compile-file src :output-file (regression-temp-file "dap-fasl-fixture.fasl"))))
      (multiple-value-bind (sym status) (find-symbol "DAP-FASL-N" "CL-USER")
        (when status (unintern sym "CL-USER")))
      (load fasl))))

(defun %dap-scenario-5 ()
  (setf *dap-test-events* '())
  (%dap-fasl-fixture)
  (let* ((server (dotcl-dap:start-server :port 0))
         (stream (%dap-open server))
         (out '()))
    (unwind-protect
         (progn
           (%dap-request stream "initialize" "adapterID" "dotcl")
           (%dap-request stream "attach")
           (%dap-request stream "setFunctionBreakpoints"
                         "breakpoints" (vector (list :obj "name" "dap-fasl-fact")))
           (%dap-request stream "configurationDone")
           (let* ((fn (find-symbol "DAP-FASL-FACT" "CL-USER"))
                  (w (dotcl-thread:make-thread (lambda () (funcall fn 3))
                                               :name "dap-test-worker-5"))
                  (tid (%dap-body (%dap-event stream "stopped") "threadId"))
                  (top (dotcl-dap::jget (%dap-top-frame stream tid) "id"))
                  (scope (aref (%dap-body (%dap-request stream "scopes" "frameId" top) "scopes") 0))
                  (vars (%dap-body (%dap-request stream "variables" "variablesReference"
                                                 (dotcl-dap::jget scope "variablesReference"))
                                   "variables")))
             (push (cons :variables (map 'list (lambda (v) (list (dotcl-dap::jget v "name")
                                                                 (dotcl-dap::jget v "value")))
                                         vars))
                   out)
             (%dap-request stream "setFunctionBreakpoints" "breakpoints" #())
             (%dap-request stream "continue" "threadId" tid)
             (push (cons :result (%dap-join w)) out))
           (%dap-request stream "disconnect"))
      (ignore-errors (close stream))
      (dotcl-dap:stop-server server))
    (nreverse out)))

(defvar *dap-result-5* (when (emitting-mode-p) (%dap-guard #'%dap-scenario-5)))

(deftest-emitting-only dotcl-dap.fasl-function-variables
  (list (cdr (assoc :variables *dap-result-5*)) (cdr (assoc :result *dap-result-5*)))
  ((("DAP-FASL-N" "3")) 6))

;;; Sixth session: conditions, hit counts and log messages of function and
;;; line breakpoints. A hit that does not stop leaves the thread running.

(defparameter *dap-cond-fixture* "test/regression/dap-cond-fixture-tmp.lisp")

(with-open-file (s *dap-cond-fixture* :direction :output :if-exists :supersede)
  (format s "(in-package :cl-user)~%~%(defun dap-cond-sum (n)~%  (let ((total 0))~%    (dotimes (i n)~%      (setf total (+ total i)))~%    total))~%"))
(load *dap-cond-fixture*)

(defun dap-cond-fn (n) (* n 10))

(defun %dap-console-lines (stream)
  "The console output events read so far (a request flushes the ones sent
before it), as their lines, and the line of the first one's place."
  (%dap-request stream "threads")
  (let ((outs (remove-if-not (lambda (m)
                               (and (equal (dotcl-dap::jget m "event") "output")
                                    (equal (%dap-body m "category") "console")))
                             *dap-test-events*)))
    (setf *dap-test-events* '())
    (list (mapcar (lambda (m) (string-right-trim '(#\Newline) (%dap-body m "output"))) outs)
          (and outs (%dap-body (first outs) "line")))))

(defun %dap-collect (stream thunk expression)
  "Run THUNK on a new thread, continuing at each stop. Returns the value of
EXPRESSION in the top frame at each stop, THUNK's result, and the console
output."
  (setf *dap-test-events* '())
  (let ((w (dotcl-thread:make-thread thunk :name "dap-test-worker-6"))
        (stops '()))
    (loop repeat 2000
          while (dotcl-thread:thread-alive-p w)
          do (if (member w (dotcl-dap:stopped-threads))
                 (let* ((tid (%dap-body (%dap-stopped-and-announced stream) "threadId"))
                        (fid (dotcl-dap::jget (%dap-top-frame stream tid) "id")))
                   (push (%dap-body (%dap-request stream "evaluate" "expression" expression
                                                  "frameId" fid "context" "watch")
                                    "result")
                         stops)
                   (%dap-request stream "continue" "threadId" tid))
                 (sleep 0.01)))
    (list (nreverse stops) (%dap-join w 1) (%dap-console-lines stream))))

(defun %dap-scenario-6 ()
  (setf *dap-test-events* '())
  (let* ((server (dotcl-dap:start-server :port 0))
         (stream (%dap-open server))
         (path (namestring (truename *dap-cond-fixture*)))
         (out '()))
    (flet ((note (key value) (push (cons key value) out))
           (fbp (&rest keys)
             (%dap-request stream "setFunctionBreakpoints"
                           "breakpoints" (vector (list* :obj "name" "dap-cond-fn" keys))))
           (lbp (&rest keys)
             (%dap-request stream "setBreakpoints" "source" (list :obj "path" path)
                           "breakpoints" (vector (list* :obj "line" 6 keys))))
           (calls (n) (lambda () (loop for i from 1 to n collect (dap-cond-fn i)))))
      (unwind-protect
           (let ((init (%dap-request stream "initialize" "adapterID" "dotcl")))
             (note :capabilities
                   (mapcar (lambda (k) (%dap-body init k))
                           '("supportsConditionalBreakpoints" "supportsHitConditionalBreakpoints"
                             "supportsLogPoints")))
             (%dap-request stream "attach")
             (%dap-request stream "configurationDone")
             (fbp "condition" "(= n 3)")
             (note :condition (%dap-collect stream (calls 5) "n"))
             (note :hit-counts
                   (loop for hit in '("%2" "3" ">= 4" "<2")
                         collect (progn (fbp "hitCondition" hit)
                                        (first (%dap-collect stream (calls 5) "n")))))
             (fbp "condition" "(evenp n)" "hitCondition" "2")
             (note :condition-and-hit-count (first (%dap-collect stream (calls 6) "n")))
             (fbp "logMessage" "n={n} sq={(* n n)} {{x}}" "condition" "(oddp n)")
             (note :logpoint (%dap-collect stream (calls 4) "n"))
             (let ((b (aref (%dap-body (fbp "hitCondition" "often") "breakpoints") 0)))
               (note :bad-hit-count (list (dotcl-dap::jget b "verified")
                                          (and (search "hit count" (dotcl-dap::jget b "message")) t))))
             (fbp "condition" "(car n)")
             (let ((r (%dap-collect stream (calls 1) "n")))
               (note :failing-condition
                     (list (first r)
                           (and (some (lambda (l) (search "condition (car n) failed" l))
                                      (first (third r)))
                                t))))
             (%dap-request stream "setFunctionBreakpoints" "breakpoints" #())
             (when (find :dotcl-emit *features*)
               (let ((sum (lambda () (dap-cond-sum 5))))
                 (lbp "condition" "(= i 2)")
                 (note :line-condition (%dap-collect stream sum "(list i total)"))
                 (lbp "hitCondition" ">3")
                 (note :line-hit-count (first (%dap-collect stream sum "i")))
                 (lbp "logMessage" "i={i} total={total}")
                 (note :line-logpoint (%dap-collect stream sum "i"))))
             (%dap-request stream "setBreakpoints" "source" (list :obj "path" path)
                           "breakpoints" #())
             (%dap-request stream "disconnect"))
        (ignore-errors (close stream))
        (dotcl-dap:stop-server server)))
    (nreverse out)))

(defvar *dap-result-6* (%dap-guard #'%dap-scenario-6))
(ignore-errors (delete-file *dap-cond-fixture*))

(defun %dap-get-6 (key) (cdr (assoc key *dap-result-6*)))

(deftest dotcl-dap.breakpoint-capabilities
  (%dap-get-6 :capabilities)
  (t t t))

(deftest dotcl-dap.breakpoint-condition
  (%dap-get-6 :condition)
  (("3") (10 20 30 40 50) (() nil)))

(deftest dotcl-dap.breakpoint-hit-counts
  (list (%dap-get-6 :hit-counts) (%dap-get-6 :condition-and-hit-count))
  ((("2" "4") ("3") ("4" "5") ("1")) ("4")))

(deftest dotcl-dap.logpoint
  (first (%dap-get-6 :logpoint))
  ())

(deftest dotcl-dap.logpoint-output
  (list (second (%dap-get-6 :logpoint)) (first (third (%dap-get-6 :logpoint))))
  ((10 20 30 40) ("n=1 sq=1 {x}" "n=3 sq=9 {x}")))

(deftest dotcl-dap.breakpoint-bad-hit-count
  (%dap-get-6 :bad-hit-count)
  (:false t))

(deftest dotcl-dap.breakpoint-failing-condition-stops
  (%dap-get-6 :failing-condition)
  (("1") t))

(deftest-emitting-only dotcl-dap.line-breakpoint-condition
  (%dap-get-6 :line-condition)
  (("(2 1)") 10 (() nil)))

;;; Hits 4 and 5 are the passes with I = 3 and 4.
(deftest-emitting-only dotcl-dap.line-breakpoint-hit-count
  (%dap-get-6 :line-hit-count)
  ("3" "4"))

(deftest-emitting-only dotcl-dap.line-logpoint
  (%dap-get-6 :line-logpoint)
  (() 10 (("i=0 total=0" "i=1 total=0" "i=2 total=1" "i=3 total=3" "i=4 total=6") 6)))

(deftest dotcl-dap.client-errors
  *dap-client-errors*
  nil)
