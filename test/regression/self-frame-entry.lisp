;;; The entry of a self-framed DEFUN links its frame, keeps its compilation unit
;;; and checks the stack depth and pending interrupts. Frames, tail calls onto a
;;; borrowed frame, redefinition while running, non-local exits, argument count
;;; errors, values and threads behave as before.

(defun %sf-names ()
  (loop for f in (dotcl:backtrace)
        when (and (> (length f) 5) (string= "%SFE-" f :end2 5)) collect f))
(defun %sf-depth () (length (dotcl:backtrace)))

(defun %sfe-h (x) (list x (%sf-names)))
(defun %sfe-g (x) (let ((r (%sfe-h x))) (if r r nil)))
(defun %sfe-f (x) (%sfe-g x))
(defun %sfe-top (x) (let ((r (%sfe-f x))) (if r r nil)))

(defun %sfe-thrower (x) (throw '%sfe-tag x))
(defun %sfe-mid (x) (let ((r (%sfe-thrower x))) (if r r nil)))
(defun %sfe-catcher (x) (catch '%sfe-tag (%sfe-mid x)))
(defun %sfe-err (x) (let ((r (car x))) (if r r nil)))
(defun %sfe-err-mid (x) (let ((r (%sfe-err x))) (if r r nil)))
(defun %sfe-call-fn (f x) (let ((r (funcall f x))) (if r r nil)))
(defun %sfe-block (x)
  (block out (%sfe-call-fn (lambda (y) (return-from out (list :out y))) x)))
(defun %sfe-mv (x) (values x (1+ x) (+ x 2)))
(defun %sfe-mv-caller (x) (multiple-value-list (%sfe-mv x)))
(defun %sfe-deep (n) (if (= n 0) (%sf-depth) (let ((r (%sfe-deep (1- n)))) (if r r nil))))
(defun %sfe-runaway (n) (let ((r (%sfe-runaway (1+ n)))) (if r r nil)))
(defun %sfe-two (a b) (list a b))
(defun %sfe-call-two (f) (let ((r (funcall f 1))) (if r r nil)))

(deftest self-frame-entry.borrowed-caller
  (%sfe-top 1)
  (1 ("%SFE-H" "%SFE-G" "%SFE-TOP")))

(deftest self-frame-entry.non-local-exits
  (let ((base (%sf-depth)))
    (list (%sfe-catcher 3)
          (handler-case (%sfe-err-mid 1) (type-error () :caught))
          (%sfe-block 4)
          (- (%sf-depth) base)
          (- (%sfe-deep 300) (%sfe-deep 0))
          (%sfe-top 2)))
  (3 :caught (:out 4) 0 300 (2 ("%SFE-H" "%SFE-G" "%SFE-TOP"))))

(deftest self-frame-entry.stack-exhaustion
  (call-on-small-stack
   (lambda ()
     (let ((base (%sf-depth)))
       (list (handler-case (%sfe-runaway 0) (storage-condition () :exhausted))
             (- (%sf-depth) base)
             (%sfe-top 3)))))
  (:exhausted 0 (3 ("%SFE-H" "%SFE-G" "%SFE-TOP"))))

(deftest self-frame-entry.argument-count
  (list (handler-case (%sfe-call-two #'%sfe-two) (program-error () :arity))
        (%sfe-top 4))
  (:arity (4 ("%SFE-H" "%SFE-G" "%SFE-TOP"))))

(deftest self-frame-entry.values
  (list (%sfe-mv-caller 1) (multiple-value-list (%sfe-mv 5)))
  ((1 2 3) (5 6 7)))

;; The running bodies drop their last references when they redefine themselves;
;; their frames keep the code alive until they return.
(defun %sfe-redef-inner (x)
  (eval '(defun %sfe-redef-inner (x) (list :new x)))
  (eval '(defun %sfe-redef-outer (x) (list :new-outer x)))
  (dotimes (i 3) (dotcl:gc))
  (let ((r (list :old x))) (if r r nil)))
(defun %sfe-redef-outer (x) (%sfe-redef-inner x))

(deftest-emitting-only self-frame-entry.redefined-while-running
  (list (%sfe-redef-outer 1) (%sfe-redef-outer 2))
  ((:old 1) (:new-outer 2)))

(deftest self-frame-entry.threads
  (let ((threads (loop for k below 4
                       collect (let ((k k))
                                 (dotcl:make-thread
                                  (lambda ()
                                    (let (r) (dotimes (i 5000) (setq r (list (%sfe-top k) (%sfe-catcher k))))
                                      r)))))))
    (mapcar #'dotcl:thread-join threads))
  (((0 ("%SFE-H" "%SFE-G" "%SFE-TOP")) 0) ((1 ("%SFE-H" "%SFE-G" "%SFE-TOP")) 1)
   ((2 ("%SFE-H" "%SFE-G" "%SFE-TOP")) 2) ((3 ("%SFE-H" "%SFE-G" "%SFE-TOP")) 3)))
