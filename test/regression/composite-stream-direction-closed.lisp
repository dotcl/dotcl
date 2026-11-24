;;; An operation on a closed half of a composite stream, or in the wrong direction through
;;; a synonym stream, signals an error (a STREAM-ERROR / TYPE-ERROR) instead of answering.

(defvar *csdc-syn* nil)

(defun %csdc-error-type (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)
    (stream-error () :stream-error)
    (error () :other-error)))

(deftest composite-stream-direction-closed.closed-output-half
  (list (let* ((out (make-string-output-stream))
               (e (make-echo-stream (make-string-input-stream "ab") out)))
          (close out)
          (%csdc-error-type (lambda () (fresh-line e))))
        (let* ((out (make-string-output-stream))
               (tw (make-two-way-stream (make-string-input-stream "x") out)))
          (close out)
          (list (%csdc-error-type (lambda () (finish-output tw)))
                (%csdc-error-type (lambda () (fresh-line tw))))))
  (:stream-error (:stream-error :stream-error)))

(deftest composite-stream-direction-closed.closed-input-half
  (let* ((in (make-string-input-stream "x"))
         (tw (make-two-way-stream in (make-string-output-stream))))
    (close in)
    (%csdc-error-type (lambda () (clear-input tw))))
  :stream-error)

(deftest composite-stream-direction-closed.synonym-wrong-direction
  (let ((*csdc-syn* (make-string-output-stream))
        (in-syn (make-synonym-stream '*csdc-syn*)))
    (list (%csdc-error-type (lambda () (read in-syn nil :eof)))
          (let ((*csdc-syn* (make-string-input-stream "ab")))
            (%csdc-error-type (lambda () (fresh-line in-syn))))))
  (:type-error :type-error))

(deftest composite-stream-direction-closed.valid-uses-unchanged
  (let* ((out (make-string-output-stream))
         (tw (make-two-way-stream (make-string-input-stream "abc") out))
         (e (make-echo-stream (make-string-input-stream "q") (make-string-output-stream))))
    (write-string "x" tw)
    (list (progn (finish-output tw) (fresh-line tw)) (read-char tw) (progn (clear-input tw) t)
          (peek-char nil e)))
  (t #\a t #\q))
