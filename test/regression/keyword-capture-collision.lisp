;;; Regression: a &key parameter captured through nested closures must not be
;;; shadowed in free-variable analysis by a same-named KEYWORD appearing in the
;;; body (e.g. `:input` next to the variable `input` in `(apply f :input input)`).
;;;
;;; The free-var candidate table is keyed by variable NAME string, so the keyword
;;; :INPUT and the variable INPUT share the "INPUT" slot. During candidate
;;; collection LOCAL-BOUND-P is forced true, so the keyword could grab the slot
;;; and suppress the variable's closure capture; the reference then compiled as a
;;; dynamic (special) load and failed at run time with "Unbound variable: INPUT".
;;;
;;; This is the exact uiop %use-launch-program shape
;;; (with-program-input/output/error-output nesting + place-setter + a labels /
;;; unwind-protect / if-let body), which made `(uiop:run-program ... :output
;;; :string)` break whenever asdf was compiled by the affected compiler. The
;;; reproduction reuses that verbatim structure — the bug is sensitive to the
;;; free-var walk order, so a simplified body does not trigger it.

(defmacro kcc-if-let ((var val) then &optional else)
  `(let ((,var ,val)) (if ,var ,then ,else)))
(defmacro kcc-place-setter (place)
  (when place (let ((value (gensym))) `#'(lambda (,value) (setf ,place ,value)))))
(defmacro kcc-with-input (((rv &optional (av (gensym) avp)) form &key setf active keys) &body body)
  `(apply 'kcc-cwpio #'(lambda (,rv ,av) ,@(unless avp `((declare (ignore ,av)))) ,@body)
          :input ,form ,active (kcc-place-setter ,setf) ,keys))
(defmacro kcc-with-output (((rv &optional (av (gensym) avp)) form &key setf active keys) &body body)
  `(apply 'kcc-cwpio #'(lambda (,rv ,av) ,@(unless avp `((declare (ignore ,av)))) ,@body)
          :output ,form ,active (kcc-place-setter ,setf) ,keys))
(defmacro kcc-with-error (((rv &optional (av (gensym) avp)) form &key setf active keys) &body body)
  `(apply 'kcc-cwpio #'(lambda (,rv ,av) ,@(unless avp `((declare (ignore ,av)))) ,@body)
          :error-output ,form ,active (kcc-place-setter ,setf) ,keys))
(defun kcc-cwpio (fun &rest args) (declare (ignore args)) (funcall fun 'reduced nil))
(defun kcc-launch (command &rest keys &key input output error-output &allow-other-keys)
  (declare (ignore keys)) (list command input output error-output))
(defun kcc-wait (p) (declare (ignore p)) 0)
(defun kcc-close (p) (declare (ignore p)) nil)

(defun kcc-ulp (command &rest keys &key input output error-output &allow-other-keys)
  (let* ((active-input-p (and input t))
         (active-output-p (and output t))
         (active-error-output-p (and error-output t))
         (activity (cond (active-output-p :output) (active-input-p :input)
                         (active-error-output-p :error-output) (t nil)))
         output-result error-output-result exit-code process-info)
    (kcc-with-output ((reduced-output output-activity) output :setf output-result
                      :active (eq activity :output) :keys keys)
      (kcc-with-error ((reduced-error-output error-output-activity) error-output
                       :setf error-output-result :active (eq activity :error-output) :keys keys)
        (kcc-with-input ((reduced-input input-activity) input
                         :active (eq activity :input) :keys keys)
          (setf process-info (apply 'kcc-launch command :input reduced-input :output reduced-output
                                    :error-output (if (eq error-output :output) :output reduced-error-output) keys))
          (labels ((get-stream (sn &optional fb) (declare (ignore sn fb)) nil)
                   (run-activity (act sn &optional fb)
                     (kcc-if-let (stream (get-stream sn fb)) (funcall act stream) (list act sn))))
            (unwind-protect
                 (ecase activity ((nil))
                   (:input (run-activity input-activity 'input-stream t))
                   (:output (run-activity output-activity 'output-stream t))
                   (:error-output (run-activity error-output-activity 'error-output-stream)))
              (kcc-close process-info)
              (setf exit-code (kcc-wait process-info)))))))
    (list output-result error-output-result exit-code)))

;; Without the fix, compiling KCC-ULP mis-classifies the captured INPUT variable
;; as special and this call signals "Unbound variable: INPUT". Fixed: completes.
(deftest keyword-capture-collision-run
  (progn (kcc-ulp '("cmd") :input "in" :output :string :error-output nil) :completed)
  :completed)
