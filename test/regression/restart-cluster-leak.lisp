;;; RESTART-CASE leaked its restart cluster when the BODY exited via a non-local
;;; control transfer (return-from / throw / go) that jumped out of the restart-case.
;;; The compiled restart-case popped the cluster only on the normal-exit and
;;; no-match-rethrow paths, so a non-local :leave through the body bypassed the pop.
;;; SBCL's compile-file body does exactly this, so every stem of the make-host-2
;;; cross-build leaked one RECOMPILE cluster; after ~120 stems a stale restart/handler
;;; fired for a frame that was already gone and crashed the run.
;;; Fix: pop the cluster in a FINALLY block so every exit path pops exactly once.

(defun %rc-rdepth () (nth-value 1 (dotcl:%handler-cluster-depth)))

;; return-from out of the body must not leak the cluster
(deftest restart-leak.return-from
  (let ((b (%rc-rdepth)))
    (block out (restart-case (return-from out 'x) (r () nil)))
    (- (%rc-rdepth) b))
  0)

;; throw out of the body must not leak
(deftest restart-leak.throw
  (let ((b (%rc-rdepth)))
    (catch 'tg (restart-case (throw 'tg 'x) (r () nil)))
    (- (%rc-rdepth) b))
  0)

;; go out of the body must not leak
(deftest restart-leak.go
  (let ((b (%rc-rdepth)))
    (tagbody (restart-case (go d) (r () nil)) d)
    (- (%rc-rdepth) b))
  0)

;; repeated non-local exits must not accumulate (the make-host-2 symptom)
(deftest restart-leak.repeated-no-accumulation
  (let ((b (%rc-rdepth)))
    (dotimes (i 50)
      (block out (restart-case (return-from out i) (r () nil))))
    (- (%rc-rdepth) b))
  0)

;; normal completion still balanced
(deftest restart-leak.normal
  (let ((b (%rc-rdepth)))
    (restart-case (values 1 2 3) (r () nil))
    (- (%rc-rdepth) b))
  0)

;; invoking a restart still runs its clause and returns its value (behaviour intact)
(deftest restart-leak.invoke-still-works
  (restart-case (invoke-restart 'myr 42) (myr (x) (list :ran x)))
  (:ran 42))

;; invoking a restart is also balanced
(deftest restart-leak.invoke-balanced
  (let ((b (%rc-rdepth)))
    (restart-case (invoke-restart 'r2) (r2 () nil))
    (- (%rc-rdepth) b))
  0)

;; with-simple-restart (expands to restart-case) still returns (values nil t) when invoked
(deftest restart-leak.with-simple-restart-invoked
  (multiple-value-list (with-simple-restart (foo "f") (invoke-restart 'foo)))
  (nil t))

;;; HANDLER-CASE had the identical leak: a non-local exit (return-from / go) through
;;; the body :leave's out of the try, bypassing the handler-cluster pop. A later
;;; signal then fired the stale handler and threw HandlerCaseInvocationException past
;;; the (gone) frame — the actual SBCL make-host-2 irrat crash. Fixed the same way.

(defun %hc-hdepth () (nth-value 0 (dotcl:%handler-cluster-depth)))

(deftest handler-leak.return-from
  (let ((b (%hc-hdepth)))
    (block out (handler-case (return-from out 'x) (error () nil)))
    (- (%hc-hdepth) b))
  0)

(deftest handler-leak.go
  (let ((b (%hc-hdepth)))
    (tagbody (handler-case (go d) (error () nil)) d)
    (- (%hc-hdepth) b))
  0)

(deftest handler-leak.throw
  (let ((b (%hc-hdepth)))
    (catch 'tg (handler-case (throw 'tg 'x) (error () nil)))
    (- (%hc-hdepth) b))
  0)

(deftest handler-leak.repeated-no-accumulation
  (let ((b (%hc-hdepth)))
    (dotimes (i 50)
      (block out (handler-case (return-from out i) (error () nil))))
    (- (%hc-hdepth) b))
  0)

(deftest handler-leak.normal
  (let ((b (%hc-hdepth)))
    (handler-case (values 1 2) (error () nil))
    (- (%hc-hdepth) b))
  0)

;; handler-case still catches and still returns the clause value
(deftest handler-leak.catch-still-works
  (handler-case (error "boom") (error (c) (list :caught (princ-to-string c))))
  (:caught "boom"))

;; normal completion returns the body value
(deftest handler-leak.normal-value
  (handler-case (+ 1 2) (error () :err))
  3)
