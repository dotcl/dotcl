;;; DOTCL:WITHOUT-INTERRUPTS and friends (SBCL's semantics): an
;;; INTERRUPT-THREAD function, DESTROY-THREAD or Ctrl-C reaching a thread
;;; inside WITHOUT-INTERRUPTS waits until the thread leaves it, however it
;;; leaves. WITH-LOCAL-INTERRUPTS lets them in again inside it; WITH-INTERRUPTS
;;; does only under ALLOW-WITH-INTERRUPTS.

(defvar *wi-log* nil)
(defvar *wi-phase* 0)

(defun %wi-await (p)
  (loop repeat 3000 until (>= *wi-phase* p) do (sleep 0.01)))

(defun %wi-spin-until (p)
  (loop until (>= *wi-phase* p)))

(defun %wi-note (x) (push x *wi-log*))

(defun %wi-identity (x) x)

;;; Start BODY-FN on a thread, wait for it to reach phase 1, interrupt it with
;;; INTERRUPT-FN, give the interrupt time to land, then release phase 2.
(defun %wi-run (body-fn interrupt-fn)
  (setq *wi-log* nil *wi-phase* 0)
  (let ((th (dotcl:make-thread body-fn :name "without-interrupts")))
    (%wi-await 1)
    (dotcl:interrupt-thread th interrupt-fn)
    (sleep 0.2)
    (setq *wi-phase* 2)
    (values (dotcl:thread-join th) (reverse *wi-log*))))

(deftest without-interrupts.deferred-until-exit
  (multiple-value-list
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (setq *wi-phase* 1)
                (%wi-spin-until 2)
                (%wi-note :body-end))
              (%wi-note :after)
              :done)
            (lambda () (%wi-note :interrupt))))
  (:done (:body-end :interrupt :after)))

(deftest without-interrupts.nested
  (nth-value 1
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (dotcl:without-interrupts
                  (setq *wi-phase* 1)
                  (%wi-spin-until 2)
                  (%wi-note :inner-end))
                (%wi-note :outer-end))
              (%wi-note :after))
            (lambda () (%wi-note :interrupt))))
  (:inner-end :outer-end :interrupt :after))

(deftest without-interrupts.with-local-interrupts
  (nth-value 1
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (setq *wi-phase* 1)
                (%wi-spin-until 2)
                (%wi-note :before-local)
                (dotcl:with-local-interrupts (%wi-note :local))
                (%wi-note :body-end)))
            (lambda () (%wi-note :interrupt))))
  (:before-local :interrupt :local :body-end))

(deftest without-interrupts.with-interrupts-needs-allow
  (nth-value 1
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (setq *wi-phase* 1)
                (%wi-spin-until 2)
                (dotcl:with-interrupts (%wi-note :with-interrupts))
                (%wi-note :body-end)))
            (lambda () (%wi-note :interrupt))))
  (:with-interrupts :body-end :interrupt))

(deftest without-interrupts.allow-with-interrupts
  (nth-value 1
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (setq *wi-phase* 1)
                (%wi-spin-until 2)
                (dotcl:allow-with-interrupts
                  (%wi-note :allowed)
                  (dotcl:with-interrupts (%wi-note :with-interrupts)))
                (%wi-note :body-end)))
            (lambda () (%wi-note :interrupt))))
  (:allowed :interrupt :with-interrupts :body-end))

;;; A throw out of the body: cleanups inside run first, the interrupt lands
;;; on the way out, before the catch returns.
(deftest without-interrupts.throw-out
  (nth-value 1
   (%wi-run (lambda ()
              (catch 'out
                (dotcl:without-interrupts
                  (unwind-protect
                       (progn (setq *wi-phase* 1)
                              (%wi-spin-until 2)
                              (throw 'out (%wi-note :thrown)))
                    (%wi-note :cleanup))))
              (%wi-note :after))
            (lambda () (%wi-note :interrupt))))
  (:thrown :cleanup :interrupt :after))

(deftest without-interrupts.error-out-to-handler-case
  (multiple-value-list
   (%wi-run (lambda ()
              (handler-case
                  (dotcl:without-interrupts
                    (setq *wi-phase* 1)
                    (%wi-spin-until 2)
                    (%wi-note :body-end)
                    (error "out"))
                (error () (%wi-note :handled) :handled)))
            (lambda () (%wi-note :interrupt))))
  (:handled (:body-end :interrupt :handled)))

;;; The deferred function exits non-locally, as a timeout does: it unwinds the
;;; thread from where WITHOUT-INTERRUPTS ends, not from inside the body.
(deftest without-interrupts.deferred-interrupt-throws
  (multiple-value-list
   (%wi-run (lambda ()
              (catch 'stop
                (dotcl:without-interrupts
                  (setq *wi-phase* 1)
                  (%wi-spin-until 2)
                  (%wi-note :body-end))
                (%wi-note :not-reached)
                :normal))
            (lambda () (throw 'stop :stopped))))
  (:stopped (:body-end)))

;;; A Lisp wait inside WITHOUT-INTERRUPTS is not cut short; inside
;;; WITH-LOCAL-INTERRUPTS it is.
(deftest without-interrupts.sleep-not-cut-short
  (multiple-value-list
   (%wi-run (lambda ()
              (dotcl:without-interrupts
                (setq *wi-phase* 1)
                (sleep 0.5)
                (%wi-note :slept)))
            (lambda () (%wi-note :interrupt))))
  ((:slept) (:slept :interrupt)))

(deftest without-interrupts.sleep-in-with-local-interrupts
  (let ((start (get-internal-real-time)))
    (list
     (%wi-run (lambda ()
                (catch 'stop
                  (dotcl:without-interrupts
                    (setq *wi-phase* 1)
                    (dotcl:with-local-interrupts (sleep 30))
                    :slept-through)))
              (lambda () (throw 'stop :stopped)))
     (< (- (get-internal-real-time) start)
        (* 10 internal-time-units-per-second))))
  (:stopped t))

(deftest without-interrupts.condition-wait-not-cut-short
  (let ((lock (dotcl:make-lock)) (cv (dotcl:make-condition-variable)))
    (setq *wi-log* nil *wi-phase* 0)
    (let ((th (dotcl:make-thread
               (lambda ()
                 (dotcl:without-interrupts
                   (dotcl:acquire-lock lock)
                   (setq *wi-phase* 1)
                   (loop until (>= *wi-phase* 2)
                         do (dotcl:condition-wait cv lock :timeout 5))
                   (dotcl:release-lock lock)
                   (%wi-note :woken))))))
      (%wi-await 1)
      (dotcl:interrupt-thread th (lambda () (%wi-note :interrupt)))
      (sleep 0.2)
      (dotcl:acquire-lock lock)
      (setq *wi-phase* 2)
      (dotcl:condition-notify cv)
      (dotcl:release-lock lock)
      (dotcl:thread-join th)
      (reverse *wi-log*)))
  (:woken :interrupt))

(defvar *wi-destroy-note* nil)

(deftest without-interrupts.destroy-deferred
  (progn
    (setq *wi-destroy-note* nil *wi-phase* 0)
    (let ((th (dotcl:make-thread
               (lambda ()
                 (dotcl:without-interrupts
                   (setq *wi-phase* 1)
                   (%wi-spin-until 2)
                   (setq *wi-destroy-note* :body-finished))
                 (setq *wi-destroy-note* :ran-after)))))
      (%wi-await 1)
      (dotcl:destroy-thread th)
      (sleep 0.2)
      (setq *wi-phase* 2)
      (dotcl:thread-join th)
      (list *wi-destroy-note* (dotcl:thread-alive-p th))))
  (:body-finished nil))

(deftest without-interrupts.ctrl-c-deferred
  (let ((n 0) (note nil))
    (handler-case
        (progn
          (dotcl:without-interrupts
            (dotnet:static "DotCL.ConditionSystem" "RequestInterrupt")
            (dotimes (i 100000) (setq n (+ n (%wi-identity i))))
            (setq note :body-done))
          :not-interrupted)
      (interactive-interrupt () (list note :interrupted))))
  (:body-done :interrupted))

(deftest without-interrupts.variables
  (list dotcl:*interrupts-enabled*
        (dotcl:without-interrupts
          (list dotcl:*interrupts-enabled* dotcl:*allow-with-interrupts*
                (dotcl:with-local-interrupts dotcl:*interrupts-enabled*)
                (dotcl:allow-with-interrupts dotcl:*allow-with-interrupts*)
                (dotcl:without-interrupts
                  (dotcl:with-local-interrupts dotcl:*interrupts-enabled*))))
        dotcl:*interrupts-enabled*)
  (t (nil nil t t nil) t))

(deftest without-interrupts.values
  (multiple-value-list (dotcl:without-interrupts (values 1 2 3)))
  (1 2 3))

(deftest without-interrupts.local-macros-outside-are-errors
  (list (handler-case (macroexpand '(dotcl:allow-with-interrupts 1))
          (error () :error))
        (handler-case (macroexpand '(dotcl:with-local-interrupts 1))
          (error () :error)))
  (:error :error))
