;;; A THROW raised in an async continuation must not decide "no catcher" there.
;;;
;;; The continuation of an (ASYNC ...) block runs on a thread the async
;;; machinery handed the work to, with its own (thread-local) catch-tag stack.
;;; The tags that can catch a THROW raised there belong to the thread that
;;; AWAITed the block, and the route back to them is the Task fault the
;;; continuation's caller turns the exception into.
;;;
;;; Answering "is there a catcher?" on the continuation thread therefore gets it
;;; wrong, and the way it got it wrong was expensive: HANDLER-CASE expands to a
;;; TAGBODY whose handler clause is reached by (GO tag), so a condition signalled
;;; after an await raised a THROW to that tag, the throw was declared catcherless
;;; on the spot, and the CONTROL-ERROR about a tag the user never wrote REPLACED
;;; the condition the outer HANDLER-CASE had been written for. In the emit-free
;;; build it took the whole test suite down with it.
;;;
;;; The decision is now deferred, not skipped: inside a continuation the throw
;;; travels as a Task fault, DOTNET:AWAIT rethrows it on the awaiting thread, and
;;; if no catcher is outstanding there it becomes CONTROL-ERROR at that boundary.
;;; So a throw that really has no catcher anywhere still reports one, and a
;;; condition that does have a handler reaches it intact.
;;;
;;; No build guard: this is a statement about the runtime, and the emit-free
;;; build is the one that showed it.

(defun %atc-delay () (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))

;;; ---- the condition survives the boundary ----

;; The issue reproducer. Before the fix this signalled CONTROL-ERROR about
;; (#:FLET-LIFT)-style internal tags instead of running the handler.
(deftest async-throw-continuation.error-after-await-reaches-handler
  (handler-case
      (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) (error "boom")))
    (error () :caught))
  :caught)

;; The handler sees the real condition, not a substitute.
(deftest async-throw-continuation.handler-sees-the-condition
  (handler-case
      (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) (error "boom")))
    (control-error () :wrong-condition)
    (simple-error (c) (format nil "~a" c)))
  "boom")

;; A handler established INSIDE the async block, across the await.
(defvar *atc-log* nil)

(deftest async-throw-continuation.handler-bind-inside-across-await
  (progn
    (setq *atc-log* nil)
    (list (handler-case
              (dotnet:await
               (dotcl:async
                 (handler-bind ((error (lambda (c) (declare (ignore c))
                                         (push :inner *atc-log*))))
                   (dotcl:await (%atc-delay))
                   (error "boom"))))
            (error () :caught-outer))
          *atc-log*))
  (:caught-outer (:inner)))

;;; ---- an explicit CATCH on the awaiting side ----

;; The tag is outstanding on the thread that awaits, and the throw comes out of
;; the continuation. It has to arrive.
(deftest async-throw-continuation.throw-to-catch-outside-the-async
  (catch 'atc-tag
    (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) (throw 'atc-tag :from-continuation))))
  :from-continuation)

;;; ---- a throw with no catcher anywhere still reports one ----

;; Deferred, not skipped: the decision moves to the awaiting thread, where the
;; answer is still no.
(deftest async-throw-continuation.no-catcher-anywhere-is-control-error
  (handler-case
      (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) (throw 'atc-absent 1)))
    (control-error () :control-error))
  :control-error)

;; And the ordinary case -- a throw outside any async -- is unchanged.
(deftest async-throw-continuation.plain-throw-without-catcher-unchanged
  (handler-case (throw 'atc-absent 1)
    (control-error () :control-error))
  :control-error)

;;; ---- the happy paths still work ----

(deftest async-throw-continuation.value-through-await
  (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) 42))
  42)

;; Two awaits in one block: the second continuation may run inline on the thread
;; the first one finished on, so the marker has to nest rather than toggle.
(deftest async-throw-continuation.two-awaits-then-error
  (handler-case
      (dotnet:await (dotcl:async (dotcl:await (%atc-delay))
                                 (dotcl:await (%atc-delay))
                                 (error "boom")))
    (error () :caught))
  :caught)

(deftest async-throw-continuation.two-awaits-value
  (dotnet:await (dotcl:async (dotcl:await (%atc-delay)) (dotcl:await (%atc-delay)) :done))
  :done)
