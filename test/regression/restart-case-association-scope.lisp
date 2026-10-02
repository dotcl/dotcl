;;; A RESTART-CASE whose body signals associates its restarts with the condition
;;; at signal time (CLHS 9.1.4.2.4, as WITH-CONDITION-RESTARTS does). The
;;; association was added to a per-thread list and never removed: every condition
;;; signalled that way stayed reachable for the life of the thread, and every
;;; restart lookup scanned all of them. SBCL's type parser signals in such a
;;; RESTART-CASE for each unknown type, so SBCL's build on dotcl kept millions.
;;; The associations now go when the RESTART-CASE's restarts do, on every way out.

(define-condition rcas-signal (condition) ())
(defun %rcas-parse-like ()
  (block nil (restart-case (signal 'rcas-signal) (muffle-warning () (return :m)))))
(defun %rcas-heap () (dotnet:static "System.GC" "GetTotalMemory" t))

;; Every way out of the RESTART-CASE takes its associations with it: after many
;; signals (handled, and through a restart invoked from the handler) the thread
;; holds as many as before.
(defun %rcas-marks () (dotnet:static "DotCL.RestartClusterStack" "AssociationMark"))
(deftest restart-case-association-scope.associations-dropped
  (let ((before (%rcas-marks)))
    (handler-bind ((rcas-signal (lambda (c) (declare (ignore c)) nil)))
      (dotimes (i 1000) (%rcas-parse-like)))
    (handler-bind ((rcas-signal (lambda (c) (invoke-restart (find-restart 'muffle-warning c)))))
      (dotimes (i 1000) (%rcas-parse-like)))
    (= before (%rcas-marks)))
  t)

;; 100,000 signals do not leave 100,000 conditions behind (about 80 MB before).
;; Not on the emit-free build: there the interpreter expands RESTART-CASE afresh
;; on every call, and a separate leak (a fresh lambda list per expansion) keeps
;; memory of its own.
(deftest-emitting-only restart-case-association-scope.no-leak
  (handler-bind ((rcas-signal (lambda (c) (declare (ignore c)) nil)))
    (dotimes (i 2000) (%rcas-parse-like))
    (let ((before (%rcas-heap)))
      (dotimes (i 100000) (%rcas-parse-like))
      (< (- (%rcas-heap) before) (* 10 1024 1024))))
  t)

;; FIND-RESTART / COMPUTE-RESTARTS with and without a condition argument, nested
;; RESTART-CASEs, a THROW out of an inner one, a signal of the same condition from
;; another RESTART-CASE afterwards, and a restart invoked from a handler: the same
;; as SBCL 2.6.8.
(define-condition rcas-a (condition) ())
(define-condition rcas-b (condition) ())
(defun %rcas-names (rs) (remove-if-not (lambda (n) (member n '(r-outer r-inner r-plain))) (mapcar #'restart-name rs)))
(defvar *rcas-log* nil)
(defun %rcas-note (tag x) (push (list tag x) *rcas-log*))
(defun %rcas-scenario ()
  (setf *rcas-log* nil)
  (let ((ca (make-condition 'rcas-a)) (cb (make-condition 'rcas-b)) (cother (make-condition 'rcas-a)))
    (handler-bind
        ((rcas-a (lambda (c)
                (%rcas-note :a-find-outer-c (and (find-restart 'r-outer c) t))
                (%rcas-note :a-find-outer-other (and (find-restart 'r-outer cother) t))
                (%rcas-note :a-find-outer-nil (and (find-restart 'r-outer) t))
                (%rcas-note :a-compute-c (%rcas-names (compute-restarts c)))
                (%rcas-note :a-compute-other (%rcas-names (compute-restarts cother)))
                (%rcas-note :a-compute-nil (%rcas-names (compute-restarts)))))
         (rcas-b (lambda (c)
                (%rcas-note :b-find-inner-c (and (find-restart 'r-inner c) t))
                (%rcas-note :b-find-outer-c (and (find-restart 'r-outer c) t))
                (%rcas-note :b-find-outer-ca (and (find-restart 'r-outer ca) t))
                (%rcas-note :b-compute-c (%rcas-names (compute-restarts c)))
                (%rcas-note :b-compute-ca (%rcas-names (compute-restarts ca)))
                (%rcas-note :b-compute-nil (%rcas-names (compute-restarts)))
                (throw 'rcas-out-inner :thrown))))
      (restart-case
          (progn
            ;; signal from the outer body: r-outer associated with ca
            (restart-case (signal ca) (r-outer () :x))
            (restart-bind ((r-plain (lambda () :p)))
              (restart-case
                  (progn
                    (%rcas-note :mid (catch 'rcas-out-inner
                                 (restart-case (signal cb) (r-inner () :i))))
                    ;; after the non-local exit from the inner RESTART-CASE
                    (%rcas-note :after-throw-compute-cb (%rcas-names (compute-restarts cb)))
                    (%rcas-note :after-throw-find-inner-cb (and (find-restart 'r-inner cb) t))
                    ;; signal again from an outer body, same condition object as before
                    (restart-case (signal ca) (r-outer () :y))
                    ;; invoke a restart through a handler
                    (%rcas-note :invoked (handler-bind ((rcas-a (lambda (c) (invoke-restart (find-restart 'r-outer c)))))
                                     (restart-case (signal ca) (r-outer () :invoked-ok))))
                    (%rcas-note :after-invoke-compute-ca (%rcas-names (compute-restarts ca))))
                (r-outer () :z))))
        (r-outer () :w))))
  (reverse *rcas-log*))

(deftest restart-case-association-scope.visibility
  (%rcas-scenario)
  ((:A-FIND-OUTER-C T) (:A-FIND-OUTER-OTHER T) (:A-FIND-OUTER-NIL T) (:A-COMPUTE-C (R-OUTER R-OUTER)) (:A-COMPUTE-OTHER (R-OUTER)) (:A-COMPUTE-NIL (R-OUTER R-OUTER)) (:B-FIND-INNER-C T) (:B-FIND-OUTER-C T) (:B-FIND-OUTER-CA T) (:B-COMPUTE-C (R-INNER R-OUTER R-PLAIN R-OUTER)) (:B-COMPUTE-CA (R-OUTER R-PLAIN R-OUTER)) (:B-COMPUTE-NIL (R-INNER R-OUTER R-PLAIN R-OUTER)) (:MID :THROWN) (:AFTER-THROW-COMPUTE-CB (R-OUTER R-PLAIN R-OUTER)) (:AFTER-THROW-FIND-INNER-CB NIL) (:A-FIND-OUTER-C T) (:A-FIND-OUTER-OTHER T) (:A-FIND-OUTER-NIL T) (:A-COMPUTE-C (R-OUTER R-OUTER R-PLAIN R-OUTER)) (:A-COMPUTE-OTHER (R-OUTER R-PLAIN R-OUTER)) (:A-COMPUTE-NIL (R-OUTER R-OUTER R-PLAIN R-OUTER)) (:INVOKED :INVOKED-OK) (:AFTER-INVOKE-COMPUTE-CA (R-OUTER R-PLAIN R-OUTER))))
