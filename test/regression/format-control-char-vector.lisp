;;; A format control that is a string but not a simple one -- a character vector
;;; with a fill pointer, or a COPY-SEQ of one -- is still a string. ERROR, WARN,
;;; SIGNAL, CERROR and MAKE-CONDITION took only the simple kind as a format
;;; control; anything else became the message by printing it, so the report of
;;; (error s) showed #(#\LATIN_SMALL_LETTER_H ...). eclector reads string
;;; literals this way (VECTOR-PUSH-EXTEND into an adjustable character vector,
;;; then COPY-SEQ), so an (error "...") evaluated by #. under eclector reported
;;; as a vector of characters.

(defun fccv-string (text)
  (let ((r (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c across text do (vector-push-extend c r))
    r))

(deftest format-control-char-vector.error
  (let ((s (fccv-string "got ~A")))
    (handler-case (error s 42)
      (simple-error (e)
        (list (princ-to-string e)
              (eq (simple-condition-format-control e) s)
              (simple-condition-format-arguments e)))))
  ("got 42" t (42)))

(deftest format-control-char-vector.error-copy-seq
  (handler-case (error (copy-seq (fccv-string "copied ~A")) :x)
    (error (e) (princ-to-string e)))
  "copied X")

(deftest format-control-char-vector.warn
  (let ((seen nil))
    (handler-bind ((simple-warning
                     (lambda (w) (setq seen (princ-to-string w)) (muffle-warning w))))
      (warn (fccv-string "careful ~A") 1))
    seen)
  "careful 1")

(deftest format-control-char-vector.signal
  (handler-case (signal (fccv-string "signalled"))
    (simple-condition (c) (princ-to-string c)))
  "signalled")

(deftest format-control-char-vector.cerror
  (handler-case (cerror (fccv-string "go on") (fccv-string "stopped ~A") 7)
    (simple-error (e) (princ-to-string e)))
  "stopped 7")

(deftest format-control-char-vector.make-condition
  (princ-to-string (make-condition 'simple-error
                                   :format-control (fccv-string "made ~A")
                                   :format-arguments '(3)))
  "made 3")
