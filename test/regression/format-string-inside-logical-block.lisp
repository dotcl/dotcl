;;; FORMAT to a fresh string (NIL, or a string output stream) inside a pretty
;;; logical block on another stream builds its own text. A nested ~? / ~@?
;;; used to flush the text gathered so far to the block's stream, so it was
;;; printed there and went missing from the result. collectors' with-formatter
;;; failed that way under lisp-unit2's summary context, which wraps each test
;;; in a PPRINT-LOGICAL-BLOCK.

(defun fsilb-run (thunk)
  "Call THUNK inside a pretty logical block on a separate string stream.
Return THUNK's value and what reached the block's stream."
  (let* ((outer (make-string-output-stream))
         (value nil))
    (let ((*print-pretty* t))
      (pprint-logical-block (outer nil)
        (pprint-indent :current 0 outer)
        (setf value (funcall thunk))))
    (values value (get-output-stream-string outer))))

(deftest format-nil-at-question-inside-logical-block
  (fsilb-run (lambda ()
               (let ((*print-pretty* nil))
                 (format nil "~@[~@?~]~?" "-" "~A" '(1)))))
  "-1" "")

(deftest format-nil-question-inside-logical-block
  (fsilb-run (lambda () (format nil "a~?b" "~A" '(1))))
  "a1b" "")

(deftest format-string-stream-inside-logical-block
  (fsilb-run (lambda ()
               (with-output-to-string (s)
                 (format s "a~@[~@?~]~?" "-" "~A" '(1)))))
  "a-1" "")

;; Writing to the block's own stream still goes through the block.
(deftest format-to-block-stream-at-question
  (with-output-to-string (outer)
    (let ((*print-pretty* t))
      (pprint-logical-block (outer nil :prefix "[" :suffix "]")
        (format outer "x~@?y" "~A" 1))))
  "[x1y]")
