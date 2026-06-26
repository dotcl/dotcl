;;; Regression tests for dotcl:async / dotcl:await — non-blocking async (step B)
;;; The (dotcl:async ...) block returns a .NET Task; we drive it to completion with
;;; the blocking dotnet:await to assert the computed value.

(defun %a-fr (n)
  "A completed Task<int> carrying N."
  (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult" '("System.Int32") n))

;;; single await bound in let*, then a synchronous tail
(deftest d1302-async-single
  (dotnet:await (dotcl:async (let* ((x (dotcl:await (%a-fr 20)))) (+ x 1))))
  21)

;;; two sequential awaits
(deftest d1302-async-seq
  (dotnet:await (dotcl:async (let* ((a (dotcl:await (%a-fr 3)))
                                    (b (dotcl:await (%a-fr 4))))
                               (* a b))))
  12)

;;; synchronous binding interleaved between awaits
(deftest d1302-async-interleave
  (dotnet:await (dotcl:async (let* ((a (dotcl:await (%a-fr 10)))
                                    (b (* a 2))
                                    (c (dotcl:await (%a-fr b))))
                               (+ a c))))
  30)

;;; await as a statement (for effect), value is the tail form
(deftest d1302-async-stmt
  (dotnet:await (dotcl:async
                  (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                  "done"))
  "done")

;;; a faulted awaited task propagates its INNER exception out of the async chain
(deftest d1302-async-fault
  (handler-case
      (dotnet:await
       (dotcl:async
         (let* ((x (dotcl:await
                    (dotnet:static-generic "System.Threading.Tasks.Task" "FromException"
                                           '("System.Int32")
                                           (dotnet:new "System.InvalidOperationException" "boom2")))))
           x)))
    (error (e) (and (search "boom2" (format nil "~A" e)) t)))
  t)

;;; (dotcl:async ...) yields a real Task (non-blocking), not the bare value
(deftest d1302-async-returns-task
  (let ((ty (dotnet:object-type
             (dotcl:async (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5)) 1))))
    (and (search "System.Threading.Tasks.Task" (format nil "~A" ty)) t))
  t)

;;; dotcl:await used outside dotcl:async is a macroexpansion error
(deftest d1302-await-outside-async
  (handler-case (progn (macroexpand-1 '(dotcl:await 1)) :no-error)
    (error () :error))
  :error)

;;; Special bound around the async block stays visible across an await,
;;; even though the continuation runs on a thread-pool thread.
(defvar *async-ctx* :global)

(deftest d1303-special-across-await
  (dotnet:await (let ((*async-ctx* :outer))
                  (dotcl:async
                    (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                    *async-ctx*)))
  :outer)

;;; special visible after two awaits, alongside the awaited values
(deftest d1303-special-two-awaits
  (dotnet:await (let ((*async-ctx* :two))
                  (dotcl:async (let* ((a (dotcl:await (%a-fr 1)))
                                      (b (dotcl:await (%a-fr 2))))
                                 (list *async-ctx* a b)))))
  (:two 1 2))

;;; no surrounding binding → the global value
(deftest d1303-special-global
  (dotnet:await (dotcl:async
                  (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 1))
                  *async-ctx*))
  :global)

;;; handler-bind established inside an async block stays active across an
;;; await — a condition signaled in a continuation (post-await) finds the handler,
;;; even though the continuation runs on a thread-pool thread.
(defvar *hb-log* nil)

(deftest d1304-handler-bind-across-await
  (progn
    (setq *hb-log* nil)
    (let ((outcome
           (handler-case
               (dotnet:await
                (dotcl:async
                  (handler-bind ((error (lambda (c) (declare (ignore c))
                                          (push :handled *hb-log*))))
                    (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                    (error "boom"))))
             (error () :caught-outer))))
      (list outcome *hb-log*)))
  (:caught-outer (:handled)))

;;; handler-bind with no condition signaled → handler not called, body value returned
(deftest d1304-handler-bind-no-signal
  (progn
    (setq *hb-log* nil)
    (let ((v (dotnet:await
              (dotcl:async
                (handler-bind ((error (lambda (c) (declare (ignore c))
                                        (push :handled *hb-log*))))
                  (dotcl:await (%a-fr 7))
                  :ok)))))
      (list v *hb-log*)))
  (:ok nil))

;;; unwind-protect across await — cleanup runs after the protected form
;;; settles, and the protected form's value is returned.
(defvar *up-log* nil)

(deftest d1305-unwind-protect-normal
  (progn
    (setq *up-log* nil)
    (let ((r (dotnet:await
              (dotcl:async
                (let* ((v (unwind-protect
                              (progn (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5)) :body)
                            (push :cleanup *up-log*))))
                  v)))))
      (list r (reverse *up-log*))))
  (:body (:cleanup)))

;;; cleanup runs even when the protected form faults across an await; error propagates
(deftest d1305-unwind-protect-fault
  (progn
    (setq *up-log* nil)
    (let ((outcome
           (handler-case
               (dotnet:await
                (dotcl:async
                  (unwind-protect
                      (progn (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                             (error "boom"))
                    (push :cleanup *up-log*))))
             (error () :caught))))
      (list outcome (reverse *up-log*))))
  (:caught (:cleanup)))

;;; cleanup may itself contain an await
(deftest d1305-unwind-protect-async-cleanup
  (progn
    (setq *up-log* nil)
    (let ((r (dotnet:await
              (dotcl:async
                (let* ((v (unwind-protect
                              (progn (dotcl:await (%a-fr 1)) :body3)
                            (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 3))
                            (push :cleaned *up-log*))))
                  v)))))
      (list r (reverse *up-log*))))
  (:body3 (:cleaned)))

;;; handler-case across await. A condition signaled after an await inside the
;;; protected form is caught by a matching clause; the clause value is returned.
(deftest d1306-handler-case-catch
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                             (error "boom"))
                    (error (e) (list :caught (and (search "boom" (format nil "~A" e)) t))))))
  (:caught t))

;;; no condition → protected value returned, no clause run
(deftest d1306-handler-case-no-error
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (%a-fr 41)) 42)
                    (error (e) (declare (ignore e)) :should-not))))
  42)

;;; non-matching condition type propagates past the handler-case to the outer handler
(deftest d1306-handler-case-nonmatch-propagates
  (handler-case
      (dotnet:await (dotcl:async
                      (handler-case
                          (progn (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
                                 (error "boom"))
                        (warning (w) (declare (ignore w)) :inner-warn))))
    (error () :outer-error))
  :outer-error)

;;; a faulted awaited .NET task (not a Lisp signal) is also caught by handler-case
(deftest d1306-handler-case-awaited-fault
  (dotnet:await (dotcl:async
                  (handler-case
                      (let* ((x (dotcl:await
                                 (dotnet:static-generic "System.Threading.Tasks.Task" "FromException"
                                                        '("System.Int32")
                                                        (dotnet:new "System.InvalidOperationException" "boom3")))))
                        x)
                    (error (e) (list :caught (and (search "boom3" (format nil "~A" e)) t))))))
  (:caught t))

;;; the handler clause body may itself contain an await
(deftest d1306-handler-case-async-handler
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 1))
                             (error "x"))
                    (error (e) (declare (ignore e))
                      (let* ((r (dotcl:await (%a-fr 99)))) (list :recovered r))))))
  (:recovered 99))

;;; async handler-case :no-error clause. Previously the CPS macro rejected
;;; any :no-error clause across await ("unsupported"). Now it runs on the success
;;; path, bound to the body's value, and is skipped when a handler clause fires.

;;; success path: :no-error receives the body value (no condition signaled)
(deftest i339-no-error-success-value
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (%a-fr 7)) 7)
                    (:no-error (v) (* v 10))
                    (error (e) (declare (ignore e)) :err))))
  70)

;;; error path: the matching handler runs; :no-error must NOT run
(deftest i339-no-error-skipped-on-fault
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (%a-fr 0)) (error "boom"))
                    (:no-error (v) (declare (ignore v)) :should-not)
                    (error (e) (declare (ignore e)) :handled))))
  :handled)

;;; the :no-error clause body may itself await
(deftest i339-no-error-body-awaits
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (%a-fr 3)) 3)
                    (:no-error (v) (let* ((r (dotcl:await (%a-fr 100)))) (+ v r))))))
  103)

;;; :no-error with &rest lambda-list binds the single async value
(deftest i339-no-error-rest-lambda-list
  (dotnet:await (dotcl:async
                  (handler-case
                      (progn (dotcl:await (%a-fr 42)) 42)
                    (:no-error (&rest vs) (car vs)))))
  42)

;;; restart-case across await. The restart cluster is pushed by
;;; %async-restart and snapshotted into continuations by %async-bind, so
;;; find-restart / invoke-restart see it even after an await. invoke-restart's
;;; non-local transfer lands on the matching clause (tag-matched in the runtime
;;; connector); a non-matching (outer) restart propagates.

;;; normal completion: body value returned, no clause run
(deftest i339-restart-normal-completion
  (dotnet:await (dotcl:async
                  (restart-case (progn (dotcl:await (%a-fr 7)) 7)
                    (r () 99))))
  7)

;;; invoke-restart across await, args bound to the clause params
(deftest i339-restart-invoke-with-args
  (dotnet:await (dotcl:async
                  (restart-case (progn (dotcl:await (%a-fr 0)) (invoke-restart 'add 10 20))
                    (add (a b) (+ a b)))))
  30)

;;; multiple restarts; the named one is selected
(deftest i339-restart-multiple-select
  (dotnet:await (dotcl:async
                  (restart-case (progn (dotcl:await (%a-fr 0)) (invoke-restart 'two))
                    (one () :first)
                    (two () :second))))
  :second)

;;; the selected restart clause body may itself await
(deftest i339-restart-clause-awaits
  (dotnet:await (dotcl:async
                  (restart-case (progn (dotcl:await (%a-fr 0)) (invoke-restart 'k 5))
                    (k (n) (let* ((r (dotcl:await (%a-fr 100)))) (+ n r))))))
  105)

;;; find-restart sees the cluster across an await
(deftest i339-restart-find-restart-visible
  (dotnet:await (dotcl:async
                  (restart-case
                      (progn (dotcl:await (%a-fr 0))
                             (if (find-restart 'present) :visible :invisible))
                    (present () :r))))
  :visible)

;;; invoke-restart fired from a continuation AFTER the await (cluster survives)
(deftest i339-restart-invoke-after-await
  (dotnet:await (dotcl:async
                  (restart-case
                      (let* ((x (dotcl:await (%a-fr 1)))) (declare (ignore x))
                        (invoke-restart 'fin))
                    (fin () :done))))
  :done)

;;; an OUTER restart invoked from an inner restart-case body propagates past inner
(deftest i339-restart-outer-propagates
  (dotnet:await (dotcl:async
                  (restart-case
                      (restart-case
                          (progn (dotcl:await (%a-fr 0)) (invoke-restart 'outer))
                        (inner () :inner))
                    (outer () :outer))))
  :outer)

;;; with-simple-restart (macro → restart-case) works across await via the
;;; macroexpand fallback in %async-cps; invoking the restart yields (values nil t)
;;; but in single-value position the primary value nil is seen.
(deftest i339-restart-with-simple-restart-invoke
  (dotnet:await (dotcl:async
                  (with-simple-restart (skip "skip it")
                    (dotcl:await (%a-fr 0))
                    (invoke-restart 'skip))))
  nil)

(deftest i339-restart-with-simple-restart-normal
  (dotnet:await (dotcl:async
                  (with-simple-restart (skip "skip it")
                    (dotcl:await (%a-fr 0))
                    5)))
  5)

;;; the macroexpand fallback also lets a plain macro (when) span an await
(deftest i339-async-macroexpand-fallback-when
  (dotnet:await (dotcl:async
                  (let* ((x (dotcl:await (%a-fr 11))))
                    (when (> x 5) x))))
  11)
