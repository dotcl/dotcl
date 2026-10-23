;;; dotcl-dap.lisp: a Debug Adapter Protocol server inside a running dotcl.
;;;
;;;   (require "dotcl-dap")
;;;   (dotcl-dap:start-server :port 4711)
;;;
;;; A DAP client attaches over TCP. A function breakpoint, or a condition
;;; that would enter the debugger, stops only the thread it happens on; the
;;; REPL and every other thread keep running. While a thread is stopped,
;;; evaluate runs on that thread with the frame's variables bound, and a
;;; thread stopped on a condition can be resumed through one of its restarts.

(require "dotcl-thread")
(require "dotcl-socket")
(require "dotcl-jsonrpc")

(defpackage :dotcl-dap
  (:use :cl :dotcl-jsonrpc)
  (:export #:start-server #:stop-server #:server-port #:*server*
           #:stopped-threads))

(in-package :dotcl-dap)

;;; The server and its one session.

(defstruct server socket thread port (session nil))

(defstruct session
  stream
  (lock (dotcl-thread:make-lock "dap-session"))
  (seq 0)
  (open t)
  (package (find-package "CL-USER")))

(defvar *server* nil)

;; Everything below is guarded by *state-lock*.
(defvar *state-lock* (dotcl-thread:make-lock "dap-state"))
(defvar *thread-ids* (make-hash-table :test 'eq))
(defvar *next-thread-id* 0)
(defvar *breakpoints* '() "List of BP structures currently installed.")
(defvar *next-id* 0)
(defvar *stops* '() "Threads stopped at a breakpoint, as STOP structures.")
(defvar *frames* (make-hash-table) "frame id -> (stop . frame)")
(defvar *exception-filters* '() "The exception breakpoint filters the client enabled.")
(defvar *previous-debugger-hook* nil)
(defvar *hook-installed* nil)
(defvar *var-refs* (make-hash-table) "variablesReference -> list of (name . value)")

(defun %new-id ()
  (dotcl-thread:with-lock-held (*state-lock*) (incf *next-id*)))

(defun thread-id (thread)
  (dotcl-thread:with-lock-held (*state-lock*)
    (or (gethash thread *thread-ids*)
        (setf (gethash thread *thread-ids*) (incf *next-thread-id*)))))

(defun %send (session message)
  (dotcl-thread:with-lock-held ((session-lock session))
    (when (session-open session)
      (setf message (list* :obj "seq" (incf (session-seq session)) (cdr message)))
      (handler-case (write-message (session-stream session) message)
        (error () (setf (session-open session) nil))))))

(defun %event (session event &optional body)
  (%send session (list :obj "type" "event" "event" event "body" (or body '(:obj)))))

(defun %respond (session request &key (success t) message body)
  (%send session (list :obj "type" "response"
                       "request_seq" (jget request "seq")
                       "success" (if success t :false)
                       "command" (jget request "command")
                       "message" message
                       "body" (or body '(:obj)))))

(defun %live-session ()
  (let ((s (and *server* (server-session *server*))))
    (and s (session-open s) s)))

;;; Function breakpoints: the function's definition is replaced by a wrapper
;;; that stops the calling thread before running the original. The name is
;;; proclaimed NOTINLINE meanwhile, so a definition compiled while the
;;; breakpoint is set calls itself through the wrapper too, and a watch on
;;; the name wraps each new definition.

(defstruct bp id name symbol original wrapper saved-inlinep)

(defstruct frame id name args locals file line)

(defstruct stop
  thread thread-id reason bp condition restarts frames
  (lock (dotcl-thread:make-lock "dap-stop"))
  (cv (dotcl-thread:make-condition-variable))
  (queue '()))

(defun %parameter-bindings (lambda-list args)
  "Pair the parameter names of LAMBDA-LIST with ARGS, as far as they go."
  (let ((out '()) (rest args) (mode :required))
    (dolist (p lambda-list)
      (cond ((member p '(&optional &rest &key &aux &body)) (setf mode p))
            ((member p lambda-list-keywords))
            (t (let ((sym (if (consp p) (if (consp (car p)) (cadar p) (car p)) p)))
                 (case mode
                   ((:required &optional)
                    (when rest (push (cons sym (pop rest)) out)))
                   ((&rest &body) (push (cons sym rest) out))
                   (&key (let ((tail (member (intern (symbol-name sym) "KEYWORD") rest)))
                           (when tail (push (cons sym (second tail)) out)))))))))
    (when (and rest (eq mode :required))
      (loop for a in rest for i from (length out)
            do (push (cons (format nil "ARG~D" i) a) out)))
    (nreverse out)))

(defvar *internal-frames* '("%BP-HIT" "%LINE-HIT" "%STEP-POINT" "%STOP-HERE" "%CAPTURE-FRAMES" "%DEBUGGER-HOOK"))

(defun %frame-source (name)
  "The file and line where the function a frame named NAME runs was defined,
when exactly one recorded definition has that name."
  (let ((found '()))
    (dolist (package (list-all-packages))
      (let ((sym (find-symbol name package)))
        (when (and sym (fboundp sym) (not (member sym found)))
          (let ((loc (ignore-errors (dotcl:function-source-location sym))))
            (when (getf loc :file) (push sym found))))))
    (when (= (length found) 1)
      (let ((loc (dotcl:function-source-location (first found))))
        (values (getf loc :file) (getf loc :line))))))

(defun %make-frame (&key name args locals)
  (multiple-value-bind (file line) (and (stringp name) (%frame-source name))
    (make-frame :id (%new-id) :name name :args args :locals locals :file file :line line)))

(defun %capture-frames (&optional skip-until)
  "The calling thread's frames below this package's own, innermost first.
With SKIP-UNTIL, also drop the frames up to and including the innermost one
whose name is in that list."
  (let* ((bt (loop for entry in (dotcl:backtrace-with-args)
                   for i from 0
                   collect (list* i entry)))
         (cut (and skip-until
                   (position-if (lambda (e) (member (second e) skip-until :test #'string=))
                                bt :from-end t))))
    (when cut (setf bt (nthcdr (1+ cut) bt)))
    (loop for (i name . args) in bt
          unless (member name *internal-frames* :test #'string=)
            collect (%make-frame :name name :args args :locals (dotcl:frame-locals i)))))

(defvar *in-stop* nil)

(defun %bp-hit (bp args)
  (let ((session (%live-session)))
    (when (and session (not *in-stop*)
               (not (eq (dotcl:current-thread) (server-thread *server*))))
      (let* ((thread (dotcl:current-thread))
             (top (%make-frame :name (prin1-to-string (bp-symbol bp))
                               :args args
                               :locals (%parameter-bindings
                                        (ignore-errors
                                         (dotcl:function-lambda-list (bp-original bp)))
                                        args)))
             (stop (make-stop :thread thread :thread-id (thread-id thread)
                              :reason "function breakpoint" :bp bp
                              :frames (cons top (%capture-frames)))))
        (let ((loc (ignore-errors (dotcl:function-source-location (bp-symbol bp)))))
          (setf (frame-file top) (getf loc :file) (frame-line top) (getf loc :line)))
        (%stop-here session stop)))))

;;; Conditions: with the "uncaught" filter on, the global *DEBUGGER-HOOK*
;;; stops the thread a condition would enter the debugger on. The thread is
;;; resumed by invoking one of the restarts it had then; continue invokes
;;; CONTINUE when there is one and otherwise declines, so the debugger (or
;;; the hook that was there before) takes the condition as it would have.

(defun %condition-text (condition)
  (or (ignore-errors (princ-to-string condition))
      (prin1-to-string (type-of condition))))

(defun %debugger-hook (condition hook)
  (declare (ignore hook))
  (let ((session (%live-session)))
    (when (and session (not *in-stop*)
               (member "uncaught" *exception-filters* :test #'string=)
               (not (eq (dotcl:current-thread) (server-thread *server*))))
      (let* ((thread (dotcl:current-thread))
             (stop (make-stop :thread thread :thread-id (thread-id thread)
                              :reason "exception" :condition condition
                              :restarts (compute-restarts condition)
                              :frames (%capture-frames '("INVOKE-DEBUGGER")))))
        (%stop-here session stop)
        (let ((r (find-restart 'continue condition)))
          (when r (invoke-restart r))))))
  (let ((previous *previous-debugger-hook*))
    (when previous (funcall previous condition previous))))

(defun %set-exception-filters (filters)
  (dotcl-thread:with-lock-held (*state-lock*)
    (setf *exception-filters* filters)
    (cond ((and filters (not *hook-installed*))
           (setf *previous-debugger-hook* (dotcl:symbol-global-value '*debugger-hook*)
                 (dotcl:symbol-global-value '*debugger-hook*) #'%debugger-hook
                 *hook-installed* t))
          ((and (null filters) *hook-installed*)
           (when (eq (dotcl:symbol-global-value '*debugger-hook*) #'%debugger-hook)
             (setf (dotcl:symbol-global-value '*debugger-hook*) *previous-debugger-hook*))
           (setf *previous-debugger-hook* nil *hook-installed* nil)))))

(defun %stop-here (session stop)
  (dotcl-thread:with-lock-held (*state-lock*)
    (push stop *stops*)
    (dolist (f (stop-frames stop))
      (setf (gethash (frame-id f) *frames*) (cons stop f))))
  (%event session "stopped"
          (list* :obj "reason" (stop-reason stop)
                 "threadId" (stop-thread-id stop)
                 "allThreadsStopped" :false
                 (append
                  (when (stop-bp stop)
                    (list "hitBreakpointIds"
                          (vector (if (bp-p (stop-bp stop))
                                      (bp-id (stop-bp stop))
                                      (line-bp-id (stop-bp stop))))))
                  (when (stop-condition stop)
                    (list "description" (%condition-text (stop-condition stop))
                          "text" (prin1-to-string (type-of (stop-condition stop))))))))
  (unwind-protect
       (loop
         (let ((command nil) (*in-stop* t))
           (dotcl-thread:with-lock-held ((stop-lock stop))
             (loop until (or (stop-queue stop) (not (session-open session)))
                   do (dotcl-thread:condition-wait (stop-cv stop) (stop-lock stop)
                                                   :timeout 1))
             (setf command (if (session-open session) (pop (stop-queue stop)) :continue)))
           (if (eq command :continue)
               (return)
               (funcall command))))
    (dotcl-thread:with-lock-held (*state-lock*)
      (setf *stops* (remove stop *stops*))
      (dolist (f (stop-frames stop))
        (remhash (frame-id f) *frames*)))))

(defun %resume (stop command)
  "Post COMMAND, which resumes STOP's thread, and forget the stop now: after the
response the client takes the thread to be running."
  (dotcl-thread:with-lock-held (*state-lock*)
    (setf *stops* (remove stop *stops*))
    (dolist (f (stop-frames stop))
      (remhash (frame-id f) *frames*)))
  (%post stop command))

(defun %post (stop command)
  (dotcl-thread:with-lock-held ((stop-lock stop))
    (setf (stop-queue stop) (append (stop-queue stop) (list command)))
    (dotcl-thread:condition-notify (stop-cv stop))))

(defun %call-in-stop (stop fn &key (timeout 30))
  "Run FN on the stopped thread and return its values as a list."
  (let ((sem (dotcl-thread:make-semaphore))
        (result nil))
    (%post stop (lambda ()
                  (setf result (multiple-value-list
                                (handler-case (cons :ok (multiple-value-list (funcall fn)))
                                  (error (e) (list :error (princ-to-string e))))))
                  (dotcl-thread:signal-semaphore sem)))
    (if (dotcl-thread:wait-on-semaphore sem :timeout timeout)
        (car result)
        (list :error "timed out waiting for the stopped thread"))))

(defun %on-redefine (symbol definition)
  (let ((bp (dotcl-thread:with-lock-held (*state-lock*)
              (find symbol *breakpoints* :key #'bp-symbol))))
    (when (and bp (functionp definition) (not (eq definition (bp-wrapper bp))))
      (setf (bp-original bp) definition)
      (setf (fdefinition symbol) (bp-wrapper bp)))))

(defun %install (bp)
  (let ((sym (bp-symbol bp)))
    (setf (bp-saved-inlinep bp) (list (dotcl:global-notinline-p sym)
                                      (dotcl:global-inline-p sym))
          (bp-wrapper bp) (lambda (&rest args)
                            (%bp-hit bp args)
                            (apply (bp-original bp) args)))
    (proclaim `(notinline ,sym))
    (setf (fdefinition sym) (bp-wrapper bp))
    (dotcl:watch-fdefinition sym #'%on-redefine)))

(defun %uninstall (bp)
  (let ((sym (bp-symbol bp)))
    (dotcl:watch-fdefinition sym nil)
    (when (and (fboundp sym) (eq (fdefinition sym) (bp-wrapper bp)))
      (setf (fdefinition sym) (bp-original bp)))
    (apply #'dotcl::%set-proclaimed-inlinep sym (bp-saved-inlinep bp))))

(defun %recompile-self-calls (bp)
  "Compile BP's function again from its lambda expression, when one is known,
so that the calls it makes to itself go through the breakpoint. NIL when
done, else a message for the client."
  (multiple-value-bind (expression closure-p)
      (ignore-errors (function-lambda-expression (bp-original bp)))
    (if (and (consp expression) (eq (first expression) 'lambda) (not closure-p))
        (handler-case (progn (compile (bp-symbol bp) expression) nil)
          (error (e) (format nil "recompiling failed (~A); calls the function makes to itself do not stop until it is defined again" e)))
        "calls the function makes to itself do not stop until it is defined again")))

(defun %read-name (name package)
  (let ((*package* package) (*read-eval* nil))
    (let ((sym (ignore-errors (read-from-string name))))
      (and (symbolp sym) sym))))

(defun set-function-breakpoints (names package)
  "Replace the installed function breakpoints with ones on NAMES. Returns a
list of (verified-p id message), one per name."
  (mapc #'%uninstall (dotcl-thread:with-lock-held (*state-lock*)
                       (shiftf *breakpoints* '())))
  (loop for name in names
        collect (let ((sym (%read-name name package)))
                  (if (and sym (fboundp sym) (not (macro-function sym))
                           (not (special-operator-p sym))
                           (not (typep (fdefinition sym) 'generic-function))
                           (not (find sym *breakpoints* :key #'bp-symbol)))
                      (let ((bp (make-bp :id (%new-id) :name name :symbol sym
                                         :original (fdefinition sym))))
                        (dotcl-thread:with-lock-held (*state-lock*)
                          (push bp *breakpoints*))
                        (%install bp)
                        (list t (bp-id bp)
                              (and (member sym (ignore-errors (dotcl:who-calls sym)))
                                   (%recompile-self-calls bp))))
                      (list nil (%new-id)
                            (if (and sym (fboundp sym)
                                     (typep (fdefinition sym) 'generic-function))
                                (format nil "~A is a generic function" name)
                                (format nil "no function named ~A" name)))))))

(defun %clear-breakpoints ()
  (mapc #'%uninstall (dotcl-thread:with-lock-held (*state-lock*)
                       (shiftf *breakpoints* '())))
  (%clear-line-breakpoints)
  (%set-exception-filters '()))

(defun stopped-threads ()
  "The threads stopped at a breakpoint now."
  (dotcl-thread:with-lock-held (*state-lock*)
    (mapcar #'stop-thread *stops*)))

;;; Line breakpoints: the DEFUN around the line is read again from the file,
;;; with the position of every list it contains, and compiled again with
;;; DOTCL:*EMIT-FORM-PREFIX* putting a call in front of the outermost form the
;;; compiler meets on that line. Clearing them puts the definition that was
;;; there back.

(defstruct line-bp id path line verified message)

(defstruct instrumented symbol original definition)

(defvar *line-bps* (make-hash-table :test 'equal)
  "path -> (line-bps . instrumented definitions)")

(defun %line-starts (text)
  (let ((starts (list 0)))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline) do (push (1+ i) starts))
    (coerce (nreverse starts) 'vector)))

(defun %line-of (starts pos)
  "The 1-based line of character position POS."
  (let ((lo 0) (hi (1- (length starts))))
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (aref starts mid) pos) (setf lo mid) (setf hi (1- mid)))))
    (1+ lo)))

(defstruct toplevel form start end package lines parents)

(defun %read-toplevel-forms (path)
  "The top-level forms of the file at PATH, each with the line of every list
in it and each list's enclosing list. IN-PACKAGE forms are followed."
  (let* ((text (with-open-file (in path :external-format :utf-8)
                 (let ((out (make-string-output-stream)))
                   (loop for line = (read-line in nil) while line
                         do (write-line line out))
                   (get-output-stream-string out))))
         (starts (%line-starts text))
         (lines (make-hash-table :test 'eq))
         (open-paren (get-macro-character #\( (copy-readtable nil)))
         (readtable (copy-readtable))
         (*package* (find-package "CL-USER"))
         (*read-eval* nil)
         (forms '()))
    (set-macro-character #\( (lambda (stream char)
                               (let* ((pos (1- (file-position stream)))
                                      (form (funcall open-paren stream char)))
                                 (when (consp form)
                                   (setf (gethash form lines) (%line-of starts pos)))
                                 form))
                         nil readtable)
    (with-input-from-string (in text)
      (loop
        (let* ((*readtable* readtable)
               (form (read in nil in)))
          (when (eq form in) (return))
          (when (consp form)
            (let ((parents (make-hash-table :test 'eq)))
              (labels ((walk (x)
                         (loop for tail on x
                               for e = (car tail)
                               when (and (consp e) (gethash e lines) (not (gethash e parents)))
                                 do (setf (gethash e parents) x) (walk e)
                               while (consp (cdr tail)))))
                (walk form))
              (push (make-toplevel :form form :start (gethash form lines)
                                   :end (%line-of starts (max 0 (1- (file-position in))))
                                   :package *package* :lines lines :parents parents)
                    forms))
            (when (and (eq (first form) 'in-package) (find-package (second form)))
              (setf *package* (find-package (second form))))))))
    (nreverse forms)))

(defun %current-definition (symbol)
  "SYMBOL's definition under a function breakpoint's wrapper, if it has one."
  (let ((bp (dotcl-thread:with-lock-held (*state-lock*)
              (find symbol *breakpoints* :key #'bp-symbol))))
    (if bp (bp-original bp) (fdefinition symbol))))

(defun %set-current-definition (symbol definition)
  (let ((bp (dotcl-thread:with-lock-held (*state-lock*)
              (find symbol *breakpoints* :key #'bp-symbol))))
    (if bp
        (setf (bp-original bp) definition)
        (setf (fdefinition symbol) definition))))

(defun %restore-instrumented (instrumented)
  (dolist (i instrumented)
    (let ((sym (instrumented-symbol i)))
      (when (and (fboundp sym)
                 (eq (%current-definition sym) (instrumented-definition i)))
        (%set-current-definition sym (instrumented-original i))))))

(defun %clear-line-breakpoints ()
  (let ((entries (dotcl-thread:with-lock-held (*state-lock*)
                   (prog1 (loop for v being the hash-values of *line-bps* collect v)
                     (clrhash *line-bps*)))))
    (dolist (e entries) (%restore-instrumented (cdr e)))))

(defun %find-line-bp (id)
  (dotcl-thread:with-lock-held (*state-lock*)
    (loop for (bps) being the hash-values of *line-bps*
          do (let ((b (find id bps :key #'line-bp-id)))
               (when b (return b))))))

(defun %line-hit (id)
  (let ((session (%live-session))
        (lbp (%find-line-bp id)))
    (when (and session lbp (not *in-stop*)
               (not (eq (dotcl:current-thread) (server-thread *server*))))
      (%set-step (dotcl:current-thread) nil)
      (let* ((thread (dotcl:current-thread))
             (frames (%capture-frames))
             (stop (make-stop :thread thread :thread-id (thread-id thread)
                              :reason "breakpoint" :bp lbp :frames frames)))
        (when frames
          (setf (frame-file (first frames)) (line-bp-path lbp)
                (frame-line (first frames)) (line-bp-line lbp)))
        (%stop-here session stop)))))

(defun %instrument (toplevel bps path)
  "Compile TOPLEVEL's DEFUN again with a stop in front of the outermost form
starting on each of BPS' lines. Marks the BPS verified when their line got
one, and returns an INSTRUMENTED, or NIL when nothing was compiled."
  (let* ((form (toplevel-form toplevel))
         (sym (second form))
         (lines (toplevel-lines toplevel))
         (parents (toplevel-parents toplevel))
         (asked (make-hash-table :test 'eq))
         (hit-lines '()))
    (flet ((give-up (message)
             (dolist (b bps) (setf (line-bp-message b) message))
             (return-from %instrument nil)))
      (unless (and (member (first form) '(defun cl:defun)) (symbolp sym))
        (give-up "only lines inside a DEFUN can have a breakpoint"))
      (unless (find :dotcl-emit *features*)
        (give-up "this build does not compile code, so it cannot place line breakpoints"))
      (unless (fboundp sym)
        (give-up (format nil "~S is not defined; load the file first" sym)))
      (let ((original (%current-definition sym)))
        (flet ((prefix (f)
                 (let ((line (gethash f lines)))
                   (when (and line (not (eq f form))
                              (loop for p = (gethash f parents) then (gethash p parents)
                                    while p
                                    never (and (eql (gethash p lines) line) (gethash p asked))))
                     (let ((b (find line bps :key #'line-bp-line)))
                       (setf (gethash f asked) t)
                       (if b
                           (progn (pushnew line hit-lines)
                                  `(%line-hit ,(line-bp-id b)))
                           `(%step-point ,path ,line)))))))
          (handler-case
              (let ((*package* (toplevel-package toplevel))
                    (dotcl:*emit-form-prefix* #'prefix)
                    (dotcl:*emit-frame-locals* t))
                (dotcl:record-definition-sources form path (toplevel-start toplevel))
                (funcall (compile nil `(lambda () ,form))))
            (error (e)
              (give-up (format nil "compiling the definition again failed: ~A" e)))))
        (dolist (b bps)
          (if (member (line-bp-line b) hit-lines)
              (setf (line-bp-verified b) t)
              (setf (line-bp-message b) "no form starts on this line")))
        (make-instrumented :symbol sym :original original
                           :definition (%current-definition sym))))))

(defun set-line-breakpoints (path lines)
  "Replace the line breakpoints in the file at PATH with ones on LINES.
Returns the LINE-BP structures, one per line."
  (let ((old (dotcl-thread:with-lock-held (*state-lock*)
               (prog1 (gethash path *line-bps*) (remhash path *line-bps*)))))
    (%restore-instrumented (cdr old)))
  (let ((bps (loop for line in lines
                   collect (make-line-bp :id (%new-id) :path path :line line)))
        (instrumented '()))
    (when bps
      (let ((toplevels (handler-case (%read-toplevel-forms path)
                         (error (e)
                           (dolist (b bps)
                             (setf (line-bp-message b) (format nil "reading the file failed: ~A" e)))
                           nil))))
        (dolist (tl toplevels)
          (let ((mine (remove-if-not (lambda (b) (<= (toplevel-start tl) (line-bp-line b)
                                                     (toplevel-end tl)))
                                     bps)))
            (when mine
              (let ((i (%instrument tl mine path)))
                (when i (push i instrumented))))))
        (dolist (b bps)
          (unless (or (line-bp-verified b) (line-bp-message b))
            (setf (line-bp-message b) "the line is outside every top-level form")))))
    (dotcl-thread:with-lock-held (*state-lock*)
      (setf (gethash path *line-bps*) (cons bps instrumented)))
    bps))

;;; Stepping: every line of a DEFUN compiled for a line breakpoint also gets a
;;; step point, which stops only a thread that is stepping. next stops at the
;;; next point no deeper than where it started, stepIn at the next point
;;; anywhere, stepOut at the next point less deep.

(defvar *steps* (make-hash-table :test 'eq) "thread -> (mode . depth)")
(defvar *stepping* nil "True while some thread is stepping.")

(defun %set-step (thread step)
  (dotcl-thread:with-lock-held (*state-lock*)
    (if step
        (setf (gethash thread *steps*) step)
        (remhash thread *steps*))
    (setf *stepping* (plusp (hash-table-count *steps*)))))

(defun %step-point (path line)
  (when *stepping*
    (let* ((thread (dotcl:current-thread))
           (step (dotcl-thread:with-lock-held (*state-lock*) (gethash thread *steps*)))
           (session (%live-session)))
      (when (and step session (not *in-stop*))
        (let ((frames (%capture-frames)))
          (when (ecase (car step)
                  (:next (<= (length frames) (cdr step)))
                  (:in t)
                  (:out (< (length frames) (cdr step))))
            (%set-step thread nil)
            (when frames
              (setf (frame-file (first frames)) path
                    (frame-line (first frames)) line))
            (%stop-here session (make-stop :thread thread :thread-id (thread-id thread)
                                           :reason "step" :frames frames))))))))

;;; Requests.

(defun %find-stop (thread-id)
  (dotcl-thread:with-lock-held (*state-lock*)
    (find thread-id *stops* :key #'stop-thread-id)))

(defun %frame (frame-id)
  (dotcl-thread:with-lock-held (*state-lock*)
    (gethash frame-id *frames*)))

(defun %var-ref (bindings)
  (if bindings
      (let ((id (%new-id)))
        (dotcl-thread:with-lock-held (*state-lock*)
          (setf (gethash id *var-refs*) bindings))
        id)
      0))

(defun %binding-name (b)
  (let ((n (car b)))
    (if (stringp n) n (prin1-to-string n))))

(defun %frame-bindings (frame package)
  "The frame's variables as (symbol . value), for binding around an evaluate."
  (loop for (name . value) in (frame-locals frame)
        for sym = (if (symbolp name) name
                      (let ((s (%read-name name package))) s))
        when (and sym (symbolp sym) (not (constantp sym)) (not (keywordp sym)))
          collect (cons sym value)))

(defun %eval-in-frame (expression frame package)
  (let* ((*package* package)
         (form (let ((*read-eval* nil)) (read-from-string expression)))
         (bindings (and frame (%frame-bindings frame package))))
    (eval (if bindings
              `(let ,(loop for (s . v) in bindings collect `(,s ',v))
                 (declare (ignorable ,@(mapcar #'car bindings)))
                 ,form)
              form))))

(defun %print (x)
  (let ((*print-length* 50) (*print-level* 5))
    (prin1-to-string x)))

(defun %children (value)
  "Child variables shown under VALUE, as (name . value)."
  (cond ((consp value)
         (loop for tail on value for i from 0 below 100
               collect (cons (format nil "~D" i) (car tail))
               unless (listp (cdr tail))
                 collect (cons "." (cdr tail)) and do (loop-finish)))
        ((and (vectorp value) (not (stringp value)))
         (loop for e across value for i from 0 below 100
               collect (cons (format nil "~D" i) e)))))

(defun %restart-list (stop)
  (loop for r in (stop-restarts stop)
        for i from 0
        collect (list :obj "index" i
                      "name" (prin1-to-string (restart-name r))
                      "description" (or (ignore-errors (princ-to-string r)) ""))))

(defun %invoke-restart (session stop index expressions)
  "Resume STOP's thread through its restart number INDEX, with the values of
EXPRESSIONS (strings, evaluated on that thread) as the restart's arguments.
Returns NIL, or a message saying why not."
  (let ((restart (and (integerp index) (>= index 0) (nth index (stop-restarts stop))))
        (top (first (stop-frames stop)))
        (package (session-package session)))
    (if (null restart)
        (format nil "no restart ~A" index)
        (let ((args (if expressions
                        (%call-in-stop stop (lambda ()
                                              (mapcar (lambda (e) (%eval-in-frame e top package))
                                                      expressions)))
                        (list :ok nil))))
          (if (eq (car args) :ok)
              (progn
                (%resume stop (lambda () (apply #'invoke-restart restart (cadr args))))
                (%event session "continued" (list :obj "threadId" (stop-thread-id stop)
                                                  "allThreadsContinued" :false))
                nil)
              (cadr args))))))

(defun %split-expressions (text &optional (start 0))
  "The expressions in TEXT from START on, each as its own string."
  (let ((*read-suppress* t) (pos start))
    (loop while (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline))) text
                                 :start pos)
          collect (let ((next (nth-value 1 (read-from-string text t nil :start pos))))
                    (prog1 (string-trim '(#\Space #\Tab #\Newline) (subseq text pos next))
                      (setf pos next))))))

(defun %restart-command (expression)
  "For an evaluate of \":restarts\" or \":restart N expr...\", the list
(:restarts) or (:restart N expr-string...); else NIL."
  (let ((text (string-trim " " expression)))
    (when (and (> (length text) 8) (string-equal ":restart" text :end2 8))
      (let ((*read-eval* nil) (*package* (find-package "KEYWORD")))
        (multiple-value-bind (head pos) (read-from-string text)
          (case head
            (:restarts (list :restarts))
            (:restart
             (multiple-value-bind (n pos) (read-from-string text nil nil :start pos)
               (list* :restart n (%split-expressions text pos))))))))))

(defun %stop-for-evaluate (args)
  "The stop an evaluate without a frame acts on: the one stopped on a condition
when there is exactly one."
  (let ((entry (and (jget args "frameId") (%frame (jget args "frameId")))))
    (if entry
        (car entry)
        (let ((stops (dotcl-thread:with-lock-held (*state-lock*)
                       (remove-if-not #'stop-condition *stops*))))
          (and (= (length stops) 1) (first stops))))))

(defun handle-request (session request)
  (let ((command (jget request "command"))
        (args (jget request "arguments" '(:obj))))
    (flet ((ok (&rest body) (%respond session request :body (cons :obj body)))
           (fail (message) (%respond session request :success nil :message message)))
      (cond
        ((string= command "initialize")
         (ok "supportsFunctionBreakpoints" t
             "supportsConfigurationDoneRequest" t
             "supportsSingleThreadExecutionRequests" t
             "supportsExceptionInfoRequest" t
             "exceptionBreakpointFilters"
             (vector (list :obj "filter" "uncaught"
                           "label" "Unhandled conditions"
                           "description" "Stop where a condition would enter the debugger; resume through one of its restarts."
                           "default" t)))
         (%event session "initialized"))
        ((string= command "setExceptionBreakpoints")
         (%set-exception-filters
          (remove-if-not (lambda (f) (equal f "uncaught"))
                         (coerce (jget args "filters" #()) 'list)))
         (ok))
        ((string= command "exceptionInfo")
         (let* ((stop (%find-stop (jget args "threadId")))
                (c (and stop (stop-condition stop))))
           (if c
               (ok "exceptionId" (prin1-to-string (type-of c))
                   "description" (%condition-text c)
                   "breakMode" "unhandled"
                   "details" (list :obj "typeName" (prin1-to-string (type-of c))
                                   "message" (%condition-text c)))
               (fail "the thread is not stopped on a condition"))))
        ((string= command "dotcl/restarts")
         (let ((stop (%find-stop (jget args "threadId"))))
           (if stop
               (ok "restarts" (coerce (%restart-list stop) 'vector))
               (fail "the thread is not stopped"))))
        ((string= command "dotcl/invokeRestart")
         (let* ((stop (%find-stop (jget args "threadId")))
                (why (if stop
                         (%invoke-restart session stop (jget args "index")
                                          (if (jget args "argumentText")
                                              (%split-expressions (jget args "argumentText"))
                                              (coerce (jget args "arguments" #()) 'list)))
                         "the thread is not stopped")))
           (if why (fail why) (ok))))
        ((member command '("attach" "launch" "configurationDone") :test #'string=)
         (ok))
        ((string= command "setBreakpoints")
         (let ((path (jget (jget args "source" '(:obj)) "path")))
           (if (null path)
               (fail "setBreakpoints needs source.path")
               (ok "breakpoints"
                   (map 'vector (lambda (b)
                                  (list :obj "id" (line-bp-id b)
                                        "verified" (if (line-bp-verified b) t :false)
                                        "line" (line-bp-line b)
                                        "message" (line-bp-message b)))
                        (set-line-breakpoints
                         path (map 'list (lambda (b) (jget b "line"))
                                   (jget args "breakpoints" #()))))))))
        ((string= command "setFunctionBreakpoints")
         (let ((names (map 'list (lambda (b) (jget b "name")) (jget args "breakpoints" #()))))
           (ok "breakpoints"
               (map 'vector (lambda (r)
                              (destructuring-bind (verified id message) r
                                (list :obj "id" id "verified" (if verified t :false)
                                      "message" message)))
                    (set-function-breakpoints names (session-package session))))))
        ((string= command "threads")
         (ok "threads"
             (map 'vector (lambda (th)
                            (list :obj "id" (thread-id th)
                                  "name" (or (ignore-errors (dotcl:thread-name th))
                                             "thread")))
                  (remove-duplicates
                   (append (dotcl:all-threads) (stopped-threads))))))
        ((string= command "stackTrace")
         (let ((stop (%find-stop (jget args "threadId"))))
           (if stop
               (let ((frames (stop-frames stop)))
                 (ok "stackFrames"
                     (map 'vector (lambda (f)
                                    (list* :obj "id" (frame-id f)
                                           "name" (format nil "(~A~{ ~A~})" (frame-name f)
                                                          (mapcar #'%print (frame-args f)))
                                           "line" (or (frame-line f) 0) "column" 0
                                           (when (frame-file f)
                                             (list "source"
                                                   (list :obj "name" (file-namestring (frame-file f))
                                                         "path" (frame-file f))))))
                          frames)
                     "totalFrames" (length frames)))
               (fail "the thread is not stopped"))))
        ((string= command "scopes")
         (let ((entry (%frame (jget args "frameId"))))
           (if entry
               (let ((stop (car entry)))
                 (ok "scopes"
                     (coerce
                      (list*
                       (list :obj "name" "Locals"
                             "variablesReference" (%var-ref (frame-locals (cdr entry)))
                             "expensive" :false)
                       (when (stop-condition stop)
                         (list (list :obj "name" "Condition"
                                     "variablesReference"
                                     (%var-ref (list (cons (prin1-to-string
                                                            (type-of (stop-condition stop)))
                                                           (stop-condition stop))))
                                     "expensive" :false)
                               (list :obj "name" "Restarts"
                                     "variablesReference"
                                     (%var-ref (loop for r in (stop-restarts stop)
                                                     for i from 0
                                                     collect (cons (format nil "~D: ~S" i
                                                                           (restart-name r))
                                                                   r)))
                                     "expensive" :false))))
                      'vector)))
               (fail "no such frame"))))
        ((string= command "variables")
         (let ((bindings (dotcl-thread:with-lock-held (*state-lock*)
                           (gethash (jget args "variablesReference") *var-refs*))))
           (ok "variables"
               (map 'vector (lambda (b)
                              (list :obj "name" (%binding-name b)
                                    "value" (%print (cdr b))
                                    "type" (%print (type-of (cdr b)))
                                    "variablesReference" (%var-ref (%children (cdr b)))))
                    bindings))))
        ((and (string= command "evaluate")
              (%restart-command (jget args "expression" "")))
         (let ((cmd (%restart-command (jget args "expression")))
               (stop (%stop-for-evaluate args)))
           (cond ((null stop) (fail "no thread stopped on a condition"))
                 ((eq (first cmd) :restarts)
                  (ok "result" (format nil "~{~A~^~%~}"
                                       (loop for r in (stop-restarts stop)
                                             for i from 0
                                             collect (format nil "~D: [~S] ~A" i (restart-name r)
                                                             (or (ignore-errors (princ-to-string r))
                                                                 ""))))
                      "variablesReference" 0))
                 (t (let ((why (%invoke-restart session stop (second cmd) (cddr cmd))))
                      (if why
                          (fail why)
                          (ok "result" (format nil "invoked restart ~A" (second cmd))
                              "variablesReference" 0)))))))
        ((string= command "evaluate")
         (let* ((expression (jget args "expression"))
                (entry (and (jget args "frameId") (%frame (jget args "frameId"))))
                (package (session-package session))
                (result (if entry
                            (%call-in-stop (car entry)
                                           (lambda ()
                                             (%eval-in-frame expression (cdr entry) package)))
                            (handler-case
                                (cons :ok (multiple-value-list
                                           (%eval-in-frame expression nil package)))
                              (error (e) (list :error (princ-to-string e)))))))
           (if (eq (car result) :ok)
               (let ((value (cadr result)))
                 (ok "result" (format nil "~{~A~^, ~}" (mapcar #'%print (cdr result)))
                     "variablesReference" (%var-ref (%children value))))
               (fail (cadr result)))))
        ((string= command "continue")
         (let ((stop (%find-stop (jget args "threadId"))))
           (when stop
             (%set-step (stop-thread stop) nil)
             (%resume stop :continue))
           (ok "allThreadsContinued" :false)))
        ((member command '("next" "stepIn" "stepOut") :test #'string=)
         (let ((stop (%find-stop (jget args "threadId"))))
           (if stop
               (progn
                 (%set-step (stop-thread stop)
                            (cons (cond ((string= command "next") :next)
                                        ((string= command "stepIn") :in)
                                        (t :out))
                                  (length (stop-frames stop))))
                 (%resume stop :continue)
                 (ok))
               (fail "the thread is not stopped"))))
        ((string= command "disconnect")
         (%clear-breakpoints)
         (dotcl-thread:with-lock-held (*state-lock*)
           (clrhash *steps*)
           (setf *stepping* nil))
         (ok)
         (dolist (stop (dotcl-thread:with-lock-held (*state-lock*) (copy-list *stops*)))
           (%post stop :continue))
         (setf (session-open session) nil))
        (t (fail (format nil "unsupported request ~A" command)))))))

(defun %serve (session)
  (unwind-protect
       (loop while (session-open session)
             do (let ((message (handler-case (read-message (session-stream session))
                                 (error () nil))))
                  (unless message (return))
                  (when (equal (jget message "type") "request")
                    (handler-case (handle-request session message)
                      (error (e)
                        (%respond session message :success nil
                                                  :message (princ-to-string e)))))))
    (setf (session-open session) nil)
    (%clear-breakpoints)
    (ignore-errors (close (session-stream session)))))

(defun start-server (&key (host "127.0.0.1") (port 0))
  "Listen for a DAP client on HOST:PORT (0 picks a free port) and serve one
session at a time in a background thread. Returns the server; SERVER-PORT
tells the port."
  (when *server* (stop-server))
  (let* ((socket (dotcl-socket:make-server-socket host port))
         (server (make-server :socket socket :port (dotcl-socket:local-port socket))))
    (setf (server-thread server)
          (dotcl-thread:make-thread
           (lambda ()
             (loop
               (let ((stream (handler-case (dotcl-socket:socket-accept socket :binary t)
                               (error () (return)))))
                 (let ((session (make-session :stream stream)))
                   (setf (server-session server) session)
                   (%serve session)))))
           :name "dap-server"))
    (setf *server* server)))

(defun stop-server (&optional (server *server*))
  (when server
    (let ((session (server-session server)))
      (when session
        (setf (session-open session) nil)
        (ignore-errors (close (session-stream session)))))
    (%clear-breakpoints)
    (dotcl-socket:socket-close (server-socket server))
    (when (eq server *server*) (setf *server* nil))
    t))

(provide "dotcl-dap")
