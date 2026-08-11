;;; Regression for the concurrent-compile corruption of the compiler's global
;;; hash tables under (set-parallel-eval t).
;;;
;;; With eval no longer serialized, many worker threads compile method/function
;;; bodies at once. Compilation of a lambda calls VAR-NAME on each uninterned
;;; variable, which does a check-then-insert on the global *uninterned-var-names*
;;; table; two workers inserting at once tore the backing Dictionary and left it
;;; permanently corrupt ("non-concurrent collection ... corrupted"), so a later
;;; unrelated compile — e.g. expanding DEFTEST — crashed. Fix: the mutable global
;;; compiler tables (*uninterned-var-names*, *function-return-types*, setf /
;;; struct / accessor registries, ...) are created :synchronized t.
;;;
;;; Deterministic pass condition: every worker completes with no error. The race
;;; is timing-sensitive, so this hammers it over several rounds; it reproduced the
;;; corruption reliably within a few rounds before the fix.

(require "dotcl-thread")

(defgeneric %pct-gf (x))

(defun %parallel-compile-tables-case ()
  (let ((errs '())
        (errlock (dotcl:make-lock))
        (nthreads 8)
        (rounds 10))
    (dotcl:set-parallel-eval t)
    (unwind-protect
         (dotimes (r 6)
           (let ((threads '()))
             (dotimes (i nthreads)
               (push
                (dotcl-thread:make-thread
                 (let ((ii i) (rr r))
                   (lambda ()
                     (handler-case
                         (dotimes (j rounds)
                           (let ((tag (intern (format nil "PCT-~D-~D-~D" rr ii j) :keyword)))
                             ;; concurrent lambda compile: the macro bodies expand
                             ;; to gensym'd bindings, so VAR-NAME hits the global
                             ;; *uninterned-var-names* table concurrently.
                             (eval `(defmethod %pct-gf ((x (eql ,tag)))
                                      (let ((acc 0)) (dotimes (k ,(1+ j)) (incf acc k)) acc)))
                             (funcall (compile nil `(lambda (a)
                                                      (loop for k below ,(1+ j) sum (+ a k))))
                                      ii)))
                       (error (e)
                         (dotcl:acquire-lock errlock)
                         (push (format nil "~A" e) errs)
                         (dotcl:release-lock errlock)))))
                 :name (format nil "pct-~D-~D" r i))
                threads))
             (dolist (th threads) (dotcl-thread:thread-join th))))
      (dotcl:set-parallel-eval nil))
    (if errs (cons :errors errs) :ok)))

(deftest-compiled-only parallel-compile-tables-no-corruption
  (%parallel-compile-tables-case)
  :ok)

;;; Regression for the *uninterned-var-counter* lost-update under parallel-eval.
;;; VAR-NAME mints "NAME#:N" for each distinct uninterned symbol; N came from a
;;; plain (incf) on a special var — a non-atomic read-modify-write. Under
;;; (set-parallel-eval t) two workers could read the same N and mint identical
;;; name strings for DISTINCT gensyms — not a crash, a silent capture hazard
;;; (an uninterned var wrongly aliasing another in closure capture). Fix: the
;;; counter is an ATOMIC-LONG bumped with ATOMIC-LONG-INCF.
;;;
;;; Each worker calls VAR-NAME on a batch of fresh (make-symbol) gensyms (every
;;; call takes the cache-miss path and bumps the counter); all returned names
;;; must be distinct. A lost update yields a duplicate "G#:N".
(defun %uninterned-name-collision-case ()
  (let ((names '())
        (lock (dotcl:make-lock))
        (nthreads 8)
        (per 250))
    (dotcl:set-parallel-eval t)
    (unwind-protect
         (let ((threads '()))
           (dotimes (i nthreads)
             (push
              (dotcl-thread:make-thread
               (lambda ()
                 (let ((local '()))
                   (dotimes (j per)
                     (declare (ignorable j))
                     (push (dotcl.cil-compiler::var-name (make-symbol "G")) local))
                   (dotcl:acquire-lock lock)
                   (setf names (nconc local names))
                   (dotcl:release-lock lock)))
               :name (format nil "unc-~D" i))
              threads))
           (dolist (th threads) (dotcl-thread:thread-join th)))
      (dotcl:set-parallel-eval nil))
    (let ((total (length names))
          (distinct (length (remove-duplicates names :test #'string=))))
      (if (= total distinct) :ok (list :collision total distinct)))))

(deftest uninterned-var-counter-no-collision
  (%uninterned-name-collision-case)
  :ok)
