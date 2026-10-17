;;; A terminal the host supplies (dotcl-repl:make-terminal) in place of
;;; System.Console. Standard input here is not a console, so any key read or
;;; size asked of System.Console would fail: these pass only if every read,
;;; write and question goes through the terminal given.

(require "dotcl-repl")

(defun rht-key (ch name &key shift alt ctrl)
  (dotnet:new "System.ConsoleKeyInfo" ch (dotnet:enum-or "System.ConsoleKey" name)
              shift alt ctrl))

(defun rht-type (string)
  (loop for ch across string
        collect (rht-key ch (if (alpha-char-p ch) (string (char-upcase ch)) "Oem1"))))

(defun rht-enter () (rht-key #\Return "Enter"))
(defun rht-down () (rht-key #\Nul "DownArrow"))

(defun rht-call (keys fn &rest options)
  "Call FN with *TERMINAL* reading KEYS. Returns FN's values as a list and what
was written."
  (let* ((out (make-string-output-stream))
         (dotcl-repl:*terminal*
           (apply #'dotcl-repl:make-terminal
                  :read-key (lambda () (pop keys))
                  :write (lambda (s) (write-string s out))
                  (append options
                          (list :width (constantly 80) :height (constantly 24))))))
    (let ((values (multiple-value-list (funcall fn))))
      (list values (get-output-stream-string out)))))

(deftest rht-line-editing
  (destructuring-bind (values written)
      (rht-call (append (rht-type "ab") (list (rht-key #\Nul "LeftArrow"))
                        (rht-type "x") (list (rht-enter)))
                (lambda () (dotcl-repl::read-line-edited "> ")))
    (list values (and (search "axb" written) t) (eql (search "> " written) 8)))
  (("axb" nil) t t))

(deftest rht-end-of-input
  (car (rht-call '() (lambda () (dotcl-repl::read-line-edited "> "))))
  (nil))

(deftest rht-menu-usable
  (list (car (rht-call '() #'dotcl-repl::menu-usable-p))
        (car (rht-call '() #'dotcl-repl::menu-usable-p :term "dumb"))
        (car (rht-call '() #'dotcl-repl::ensure-console)))
  ((t) (nil) (t)))

(deftest rht-size
  (car (rht-call '() (lambda () (list (dotcl-repl::terminal-width)
                                      (dotcl-repl::terminal-height)))))
  ((80 24)))

(deftest rht-completion-menu
  (let ((dotcl-repl:*completer*
          (lambda (text offset)
            (declare (ignore text))
            (list :start 0 :end offset
                  :items '((:label "foo-bar") (:label "foo-baz"))))))
    (destructuring-bind (values written)
        (rht-call (append (rht-type "fo")
                          (list (rht-key #\Tab "Tab") (rht-down) (rht-enter) (rht-enter)))
                  (lambda () (dotcl-repl::read-line-edited "> ")))
      (list values (and (search "foo-bar" written) t))))
  (("foo-baz" nil) t))

(defvar *rht-restarts* '("0: [ABORT] Return to top level."
                         "1: [RETRY] Retry."))

(deftest rht-debugger-restart-menu
  (destructuring-bind (values written)
      (rht-call (list (rht-down) (rht-enter))
                (lambda () (dotcl-repl::debugger-read "0] " *rht-restarts*)))
    (list values (and (search "[RETRY]" written) t)))
  ((1) t))

(deftest rht-debugger-frame-menu
  (car (rht-call (list (rht-down) (rht-down) (rht-enter))
                 (lambda ()
                   (dotcl-repl::debugger-read "0] " '("0: (A)" "1: (B)" "2: (C)")
                                              0 :frames))))
  (2))

(deftest rht-ctrl-c-as-input
  (car (rht-call '() (lambda () (dotcl-repl::ctrl-c-as-input t))))
  (nil))

;;; -- The whole REPL on a host's terminal and streams ----------------------------

(defun rht-repl (keys &rest options)
  "Run RUN-REPL on KEYS and the end of input after them, on a thread of its own
as a host would, so that no handler of the test's takes the REPL's errors.
Returns what the terminal was given and what went to the output stream."
  (let* ((written (make-string-output-stream))
         (output (make-string-output-stream))
         (terminal (dotcl-repl:make-terminal
                    :read-key (lambda () (pop keys))
                    :write (lambda (s) (write-string s written))
                    :width (constantly 80) :height (constantly 24)))
         (result :unset)
         (thread (dotcl:make-thread
                  (lambda ()
                    (let ((dotcl-repl:*history-file* nil)
                          (dotcl-repl:*history* '()))
                      (setf result
                            (apply #'dotcl-repl:run-repl
                                   (append options
                                           (list :terminal terminal :output output
                                                 :color nil)))))))))
    (dotcl:thread-join thread)
    (list result
          (get-output-stream-string written)
          (get-output-stream-string output))))

(deftest rht-repl-values-and-output
  (destructuring-bind (result written output)
      (rht-repl (append (rht-type "(+ 1 2)") (list (rht-enter))
                        (rht-type "(princ 'hi)") (list (rht-enter))))
    (list result
          (and (search "CL-USER>" written) t)
          (and (search "dotcl REPL." output) t)
          (and (search (format nil "~%3~%") output) t)
          (and (search "HIHI" output) t)))
  (nil t t t t))

(deftest rht-repl-debugger-menu
  (destructuring-bind (result written output)
      (rht-repl (append (rht-type "(car 1)") (list (rht-enter) (rht-enter))
                        (rht-type "(+ 2 2)") (list (rht-enter))))
    (list result
          (and (search "Debugger entered" output) t)
          (and (search "[ABORT]" written) t)
          (and (search (format nil "~%4~%") output) t)))
  (nil t t t))

(deftest rht-repl-not-on-a-dumb-terminal
  (handler-case
      (dotcl-repl:run-repl :terminal (dotcl-repl:make-terminal :read-key (constantly nil)
                                                               :write (constantly nil)
                                                               :term "dumb"))
    (error () :refused))
  :refused)

(deftest rht-interrupt-repl-when-idle
  (dotcl-repl:interrupt-repl)
  nil)

;;; The host asks for the interrupt from another thread while the REPL evaluates
;;; a form that would not end by itself.
(deftest rht-interrupt-repl
  (let* ((submitted nil)
         (keys (append (rht-type "(loop (sleep 0.01))") (list (rht-enter))))
         (output (make-string-output-stream))
         (terminal (dotcl-repl:make-terminal
                    :read-key (lambda ()
                                (let ((k (pop keys)))
                                  (unless keys (setf submitted t))
                                  k))
                    :write (constantly nil)))
         (thread (dotcl:make-thread
                  (lambda ()
                    (let ((dotcl-repl:*history-file* nil))
                      (dotcl-repl:run-repl :terminal terminal :output output
                                           :color nil)))))
         (sent nil))
    (loop repeat 500 until submitted do (sleep 0.01))
    (sleep 0.2)
    (loop repeat 500
          until (setf sent (dotcl-repl:interrupt-repl))
          do (sleep 0.01))
    (dotcl:thread-join thread)
    (let ((text (get-output-stream-string output)))
      (list sent
            (and (or (search "Interrupted" text)
                     (search "INTERACTIVE-INTERRUPT" text))
                 t))))
  (t t))

;;; What the console REPL sees stays as it was, while a run-repl on another
;;; thread runs with colour and after it ends.
(defun rht-console-state ()
  (list (dotnet:static "DotCL.Startup" "ReadlineHook")
        (dotnet:static "DotCL.Startup" "DebuggerReadHook")
        dotcl-repl::*history-loaded*
        (dotnet:static "DotCL.ReplColor" "OutOn")
        (dotnet:static "DotCL.ReplColor" "ErrOn")
        (dotnet:static "DotCL.Debugger" "InteractiveRepl")
        (dotnet:static "DotCL.ReplLoop" "Hosted")))

(deftest rht-repl-leaves-console-state
  (let* ((before (rht-console-state))
         (midway nil)
         (checked nil)
         (during nil)
         (keys (append (rht-type "(+ 1 2)") (list (rht-enter))))
         (output (make-string-output-stream))
         (terminal (dotcl-repl:make-terminal
                    :read-key (lambda ()
                                (or (pop keys)
                                    (progn (setf midway t)
                                           (loop repeat 500 until checked do (sleep 0.01))
                                           nil)))
                    :write (constantly nil)))
         (thread (dotcl:make-thread
                  (lambda ()
                    (let ((dotcl-repl:*history-file* nil))
                      (dotcl-repl:run-repl :terminal terminal :output output :color t))))))
    (loop repeat 500 until midway do (sleep 0.01))
    (setf during (rht-console-state))
    (setf checked t)
    (dotcl:thread-join thread)
    (list (equal before during)
          (equal before (rht-console-state))
          (car (last before))
          (and (find (code-char 27) (get-output-stream-string output)) t)))
  (t t nil t))

;;; A terminal that fails ends that REPL only: the console's hooks stay, and the
;;; next RUN-REPL runs.
(deftest rht-repl-terminal-fails
  (let ((console-read (lambda (prompt) (declare (ignore prompt)) nil))
        (console-debug (lambda (prompt labels) (declare (ignore prompt labels)) nil))
        (saved-read (dotnet:static "DotCL.Startup" "ReadlineHook"))
        (saved-debug (dotnet:static "DotCL.Startup" "DebuggerReadHook")))
    (dotcl::%set-repl-readline-hook console-read)
    (dotcl::%set-debugger-read-hook console-debug)
    (unwind-protect
         (let* ((output (make-string-output-stream))
                (terminal (dotcl-repl:make-terminal
                           :read-key (constantly nil)
                           :write (lambda (s) (declare (ignore s)) (error "terminal gone"))))
                (thread (dotcl:make-thread
                         (lambda ()
                           (dotcl-repl:run-repl :terminal terminal :output output
                                                :color nil)))))
           (dotcl:thread-join thread)
           (list (and (search "terminal gone" (get-output-stream-string output)) t)
                 (eq (dotnet:static "DotCL.Startup" "ReadlineHook") console-read)
                 (eq (dotnet:static "DotCL.Startup" "DebuggerReadHook") console-debug)
                 (and (search (format nil "~%3~%")
                              (third (rht-repl (append (rht-type "(+ 1 2)")
                                                       (list (rht-enter))))))
                      t)))
      (dotcl::%set-repl-readline-hook (or saved-read nil))
      (dotcl::%set-debugger-read-hook (or saved-debug nil))))
  (t t t t))

;;; When the debugger's hook fails in a hosted REPL, the debugger reads its
;;; lines from the host's line editor, never from System.Console.
(defvar *rht-debugger-line* nil)

(deftest rht-repl-debugger-hook-fails
  (let* ((reset (setf *rht-debugger-line* nil))
         (lines (list "(car 1)" "(setq cl-user::*rht-debugger-line* 7)" ":abort"
                      "(+ 2 2)"))
         (output (make-string-output-stream))
         (thread (dotcl:make-thread
                  (lambda ()
                    (let ((*debugger-hook* nil))
                    (dotcl::%repl-run output output nil
                                      (lambda (prompt) (declare (ignore prompt)) (pop lines))
                                      (lambda (&rest args)
                                        (declare (ignore args))
                                        (error "menu gone"))))))))
    (declare (ignore reset))
    (dotcl:thread-join thread)
    (let ((text (get-output-stream-string output)))
      (list *rht-debugger-line*
            (and (search "menu gone" text) t)
            (and (search (format nil "~%4~%") text) t))))
  (7 t t))

;;; An interrupt that reaches the REPL's thread while the editor waits for a key
;;; is dropped, so the line being typed survives; one that arrives after the REPL
;;; has ended does nothing.
(deftest rht-interrupt-repl-while-waiting
  (let* ((keys (append (rht-type "(+ 1 2)") (list (rht-enter))))
         (wait nil)
         (output (make-string-output-stream))
         (terminal (dotcl-repl:make-terminal
                    :read-key (lambda ()
                                (unless wait
                                  (setf wait dotcl-repl::*key-wait*)
                                  (dotcl:interrupt-thread (dotcl:current-thread)
                                                          (dotcl-repl::repl-interrupter wait))
                                  (sleep 0.05))
                                (pop keys))
                    :write (constantly nil)))
         (after :unset)
         (thread (dotcl:make-thread
                  (lambda ()
                    (dotcl-repl:run-repl :terminal terminal :output output :color nil)
                    (setf after
                          (handler-case (progn (funcall (dotcl-repl::repl-interrupter wait))
                                               :nothing)
                            (condition (c) (type-of c))))))))
    (dotcl:thread-join thread)
    (let ((text (get-output-stream-string output)))
      (list (and (search (format nil "~%3~%") text) t)
            (and (search "Interrupted" text) t)
            after)))
  (t nil :nothing))

;;; Ctrl+C at the console is not delivered on the thread of a hosted REPL; the
;;; console's thread takes it. The request is for the whole process, so the
;;; handler here is in place before the REPL can make it: the REPL reads its
;;; line only once READY is set. Made any earlier, a slow main thread (the
;;; emit-free build on a busy machine) took it before the HANDLER-CASE, and
;;; the script's debugger hook ended the run.
(deftest rht-console-interrupt-not-for-host
  (let* ((ready nil)
         (lines (list "(progn (dotnet:static \"DotCL.ConditionSystem\" \"RequestInterrupt\") (dotimes (i 100000) (identity i)) :survived)"))
         (output (make-string-output-stream))
         (thread nil)
         (here (unwind-protect
                    (handler-case
                        (progn
                          (setf thread
                                (dotcl:make-thread
                                 (lambda ()
                                   (let ((*debugger-hook* nil))
                                     (dotcl::%repl-run output output nil
                                                       (lambda (prompt)
                                                         (declare (ignore prompt))
                                                         (loop repeat 3000 until ready
                                                               do (sleep 0.01))
                                                         (pop lines))
                                                       nil)))))
                          (setf ready t)
                          (dotcl:thread-join thread)
                          (dotimes (i 100000) (identity i))
                          :missed)
                      (interactive-interrupt () :here))
                 ;; A request nobody took must not reach the tests after this one.
                 (dotnet:static "DotCL.ConditionSystem" "DiscardInterrupt"))))
    (dotcl:thread-join thread)
    (list (and (search ":SURVIVED" (get-output-stream-string output)) t)
          here))
  (t :here))
