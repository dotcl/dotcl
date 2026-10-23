;;; A LET that binds specials unbinds them all at once on exit, back to the
;;; depth saved before its bindings, on every way out.

(setf dotcl:*save-sil* t)

(defvar *slud-a* :global-a)
(defvar *slud-b* :global-b)

(defun %slud-inner (n)
  (let ((*slud-a* n) (*slud-b* (list n)))
    (cond ((= n 0) (throw 'slud (list *slud-a* *slud-b*)))
          ((= n 1) (error "slud ~A" *slud-a*))
          (t (list *slud-a* *slud-b* (%slud-peek))))))

(defun %slud-peek () (list *slud-a* *slud-b*))

(deftest special-let-unbind-depth.exits
  (list (catch 'slud (%slud-inner 0))
        (list *slud-a* *slud-b*)
        (handler-case (%slud-inner 1) (error () :err))
        (list *slud-a* *slud-b*)
        (%slud-inner 2)
        (list *slud-a* *slud-b*)
        (let ((*slud-a* 10))
          (list (block b (let ((*slud-b* 11) (*slud-a* 12)) (return-from b (%slud-peek))))
                (%slud-peek)))
        (list *slud-a* *slud-b*))
  ((0 (0)) (:global-a :global-b) :err (:global-a :global-b)
   (2 (2) (2 (2))) (:global-a :global-b)
   ((12 11) (10 :global-b)) (:global-a :global-b)))

(deftest special-let-unbind-depth.progv-inside
  (list (let ((*slud-a* 1))
          (progv '(*slud-b*) '(2) (let ((*slud-b* 3)) (%slud-peek))))
        (%slud-peek))
  ((1 3) (:global-a :global-b)))

(deftest-emitting-only special-let-unbind-depth.one-unbind
  (let ((d (%flat-text (dotcl:function-sil #'%slud-inner))))
    (list (and (search "DynamicBindings.UnbindTo" d) t)
          (search "DynamicBindings.Pop" d)))
  (t nil))

;; A fasl compiled when frames popped in push order pops below the top; the
;; bindings left keep answering.
(deftest special-let-unbind-depth.pop-out-of-order
  (let ((db "DotCL.DynamicBindings"))
    (dotnet:static db "Push" '*slud-a* 1)
    (dotnet:static db "Push" '*slud-b* 2)
    (dotnet:static db "Push" '*slud-a* 3)
    (let ((r1 (%slud-peek)))
      (dotnet:static db "Pop" '*slud-a*)
      (let ((r2 (%slud-peek)))
        (dotnet:static db "Pop" '*slud-a*)
        (let ((r3 (%slud-peek)))
          (dotnet:static db "Pop" '*slud-b*)
          (list r1 r2 r3 (%slud-peek))))))
  ((3 2) (1 2) (:global-a 2) (:global-a :global-b)))
