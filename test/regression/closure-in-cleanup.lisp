;;; Closure created inside try/finally regions (unwind-protect cleanup,
;;; special-variable let): *in-try-block* / *in-finally-block* are reset at
;;; the closure boundary (the closure body is a separate CLR method, outside
;;; the outer try/finally region), so closure-internal block/return-from and
;;; tagbody/go take the local leave path — results must be unchanged.

(defvar *cic-special* nil)

;; Closure made in unwind-protect CLEANUP; its internal block/return-from
;; must work (previously compiled via the throw path due to stale
;; *in-finally-block*; now via local leave).
(deftest closure-in-cleanup.return-from
  (let ((probe nil))
    (unwind-protect
        :body-value
      (setq probe (funcall (lambda (x)
                             (block b
                               (when x (return-from b (* x 2)))
                               :not-reached))
                           21)))
    probe)
  42)

;; Closure made in cleanup with tagbody/go inside.
(deftest closure-in-cleanup.tagbody-go
  (let ((probe nil))
    (unwind-protect
        :body-value
      (setq probe
            (funcall (lambda (n)
                       (let ((acc 0))
                         (tagbody
                          top
                            (when (< acc n)
                              (setq acc (+ acc 1))
                              (go top)))
                         acc))
                     5)))
    probe)
  5)

;; Closure made inside a special-binding let (try/finally): internal
;; block + tagbody, result unchanged.
(deftest closure-in-cleanup.special-let
  (let ((*cic-special* t))
    (funcall (lambda (n)
               (block b
                 (let ((acc 0))
                   (tagbody
                    top
                      (when (>= acc n) (return-from b acc))
                      (setq acc (+ acc 1))
                      (go top)))))
             7))
  7)

;; Nested: closure made in cleanup contains its own unwind-protect whose
;; protected form does return-from through it — the closure's own
;; *in-finally-block* discipline must still apply inside.
(deftest closure-in-cleanup.nested-uwp
  (let ((log nil)
        (result nil))
    (unwind-protect
        :body
      (setq result
            (funcall (lambda ()
                       (block b
                         (unwind-protect
                             (return-from b :inner-return)
                           (setq log (cons :cleanup log))))))))
    (list result log))
  (:inner-return (:cleanup)))
