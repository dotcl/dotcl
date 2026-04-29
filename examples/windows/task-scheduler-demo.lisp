;;;; Task Scheduler demo for dotcl on Windows
;;;;
;;;; Drives the Schedule.Service COM API from Lisp via dotnet:invoke /
;;;; dotnet:new (D741). Registers two tasks — one logon-trigger, one event-
;;;; trigger reacting to USB plug events — runs the logon one on-demand for
;;;; instant audible feedback (SAPI.SpVoice via task-scheduler-payload.lisp),
;;;; waits for Enter, then unregisters both.
;;;;
;;;; Works only on Windows. Requires the dev tree (uses dotnet run --project
;;;; runtime/runtime.csproj) — for production use the packed dotcl.exe path.
;;;;
;;;; Usage (from repo root):
;;;;   dotnet run --project runtime/runtime.csproj -- \
;;;;     --asm compiler/cil-out.sil examples/windows/task-scheduler-demo.lisp

(format t "=== dotcl Task Scheduler demo ===~%")

;; -----------------------------------------------------------------------
;; Path discovery — locate the dev tree from this file's load path so the
;; registered task knows how to invoke dotcl with our payload script.
;; -----------------------------------------------------------------------

(defun demo-paths ()
  ;; directory-namestring on Windows strips the drive letter; truename restores it.
  (let* ((demo-dir  (truename (directory-namestring *load-pathname*)))
         (proj-root (truename (merge-pathnames "../../" demo-dir)))
         (cil-out   (namestring (merge-pathnames "compiler/cil-out.sil" proj-root)))
         (runtime   (namestring (merge-pathnames "runtime/runtime.csproj" proj-root)))
         (payload   (namestring (merge-pathnames "task-scheduler-payload.lisp" demo-dir))))
    (values "dotnet"
            (format nil "run --project \"~a\" --no-build -- --asm \"~a\" \"~a\""
                    runtime cil-out payload)
            payload)))

;; -----------------------------------------------------------------------
;; Schedule.Service helpers — thin wrappers over the COM IDispatch chain.
;; -----------------------------------------------------------------------

(defun %connect-service ()
  (let ((svc (dotnet:new "Schedule.Service")))
    (dotnet:invoke svc "Connect")
    svc))

(defun %folder (svc &optional (path "\\"))
  (dotnet:invoke svc "GetFolder" path))

(defun %new-task-def (svc)
  (dotnet:invoke svc "NewTask" 0))

;; TASK_TRIGGER_LOGON = 9. UserId restricts the trigger to a specific
;; account; without it the trigger fires for any logon and requires admin
;; to register.
(defun %add-logon-trigger (def id user-id)
  (let* ((triggers (dotnet:invoke def "Triggers"))
         (trigger  (dotnet:invoke triggers "Create" 9)))
    (setf (dotnet:invoke trigger "Id") id)
    (setf (dotnet:invoke trigger "UserId") user-id)
    trigger))

;; TASK_TRIGGER_EVENT = 0. Subscription is a Windows Event Log XPath query.
(defun %add-event-trigger (def id event-log provider-name &key event-id)
  (let* ((triggers (dotnet:invoke def "Triggers"))
         (trigger  (dotnet:invoke triggers "Create" 0))
         (xpath
          (if event-id
              (format nil
                      "<QueryList><Query Id='0' Path='~a'><Select Path='~a'>~
                       *[System[Provider[@Name='~a'] and EventID=~a]]</Select>~
                       </Query></QueryList>"
                      event-log event-log provider-name event-id)
              (format nil
                      "<QueryList><Query Id='0' Path='~a'><Select Path='~a'>~
                       *[System[Provider[@Name='~a']]]</Select>~
                       </Query></QueryList>"
                      event-log event-log provider-name))))
    (setf (dotnet:invoke trigger "Subscription") xpath)
    (setf (dotnet:invoke trigger "Id") id)
    trigger))

;; TASK_ACTION_EXEC = 0
(defun %add-exec-action (def path arguments)
  (let* ((actions (dotnet:invoke def "Actions"))
         (action  (dotnet:invoke actions "Create" 0)))
    (setf (dotnet:invoke action "Path") path)
    (setf (dotnet:invoke action "Arguments") arguments)
    action))

(defun %tune-settings (def)
  (let ((s (dotnet:invoke def "Settings")))
    (setf (dotnet:invoke s "Hidden") t)
    (setf (dotnet:invoke s "StartWhenAvailable") t)
    (setf (dotnet:invoke s "AllowDemandStart") t)
    (setf (dotnet:invoke s "DisallowStartIfOnBatteries") nil)
    (setf (dotnet:invoke s "StopIfGoingOnBatteries") nil)
    s))

(defun current-user-id ()
  (format nil "~a\\~a"
          (dotnet:static "System.Environment" "UserDomainName")
          (dotnet:static "System.Environment" "UserName")))

;; TASK_CREATE_OR_UPDATE = 6, TASK_LOGON_INTERACTIVE_TOKEN = 3.
;; The userId VARIANT must be a real user string (not Type.Missing — late-
;; bound COM with Missing here yields E_ACCESSDENIED on some builds).
(defun register-task (folder name def)
  (dotnet:invoke folder "RegisterTaskDefinition"
                 name def 6 (current-user-id) nil 3 nil))

(defun delete-task (folder name)
  (handler-case (dotnet:invoke folder "DeleteTask" name 0)
    (error () nil)))

(defun list-task-names (folder)
  ;; flags=1 (TASK_ENUM_HIDDEN) so hidden tasks (incl. ours) appear.
  (let* ((tasks (dotnet:invoke folder "GetTasks" 1))
         (count (dotnet:invoke tasks "Count"))
         (out   nil))
    ;; IRegisteredTaskCollection is 1-based.
    (dotimes (k count)
      (let ((task (dotnet:invoke tasks "Item" (1+ k))))
        (push (dotnet:invoke task "Name") out)))
    (nreverse out)))

(defun run-task-now (folder name)
  (let ((task (dotnet:invoke folder "GetTask" name)))
    (dotnet:invoke task "Run" nil)))

;; -----------------------------------------------------------------------
;; Demo flow
;; -----------------------------------------------------------------------

(multiple-value-bind (dotnet-exe args payload-path) (demo-paths)
  (format t "  exec:    ~a ~a~%" dotnet-exe args)
  (format t "  payload: ~a~%" payload-path)
  (cond
    ((not (probe-file payload-path))
     (format t "~%ERROR: payload not found at ~a~%" payload-path))
    (t
     (let* ((svc  (%connect-service))
            (root (%folder svc)))

       ;; Task 1: logon trigger (restricted to current user — see %add-logon-trigger)
       (let ((def (%new-task-def svc))
             (uid (current-user-id)))
         (%add-logon-trigger def "LogonTrigger" uid)
         (%add-exec-action def dotnet-exe args)
         (%tune-settings def)
         (register-task root "DotclLogonHello" def)
         (format t "~%registered: DotclLogonHello (TASK_TRIGGER_LOGON for ~a)~%" uid))

       ;; Task 2: USB plug event trigger.
       ;; Microsoft-Windows-Kernel-PnP / EventID 410 fires on device install.
       (let ((def (%new-task-def svc)))
         (%add-event-trigger def "EventTrigger"
                             "Microsoft-Windows-Kernel-PnP/Configuration"
                             "Microsoft-Windows-Kernel-PnP"
                             :event-id 410)
         (%add-exec-action def dotnet-exe args)
         (%tune-settings def)
         (register-task root "DotclUsbHello" def)
         (format t "registered: DotclUsbHello (TASK_TRIGGER_EVENT, USB plug)~%"))

       ;; Show our footprint
       (format t "~%Dotcl* tasks currently in root folder:~%")
       (dolist (n (list-task-names root))
         (when (and (>= (length n) 5)
                    (string= "Dotcl" (subseq n 0 5)))
           (format t "  - ~a~%" n)))

       ;; Run on-demand for instant audible feedback (don't wait for logon)
       (format t "~%Running DotclLogonHello on-demand — listen for SAPI voice...~%")
       (run-task-now root "DotclLogonHello")

       (format t "~%Press Enter to unregister demo tasks and exit... ")
       (force-output)
       (read-line *standard-input* nil)

       (delete-task root "DotclLogonHello")
       (delete-task root "DotclUsbHello")
       (format t "Cleaned up.~%")))))
