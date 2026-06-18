;;; Regression tests for Gray Streams predicate consistency

;;; D1211 — open-stream-p must accept Gray Streams instances.
;;; Previously (streamp gs) => T but (open-stream-p gs) signaled
;;; "OPEN-STREAM-P: not a stream". open-stream-p now mirrors streamp:
;;; a gray input/output stream is reported open.
(require "dotcl-gray")

(defclass %gray-out (dotcl-gray:fundamental-character-output-stream) ())
(defclass %gray-in  (dotcl-gray:fundamental-character-input-stream) ())

(deftest d1211-gray-out-streamp
  (streamp (make-instance '%gray-out))
  t)

(deftest d1211-gray-out-open-stream-p
  (open-stream-p (make-instance '%gray-out))
  t)

(deftest d1211-gray-out-output-stream-p
  (output-stream-p (make-instance '%gray-out))
  t)

(deftest d1211-gray-in-streamp
  (streamp (make-instance '%gray-in))
  t)

(deftest d1211-gray-in-open-stream-p
  (open-stream-p (make-instance '%gray-in))
  t)

(deftest d1211-gray-in-input-stream-p
  (input-stream-p (make-instance '%gray-in))
  t)

;;; Non-stream objects still signal a type error.
(deftest d1211-open-stream-p-non-stream
  (handler-case (progn (open-stream-p 42) :no-error)
    (error () :errored))
  :errored)

;;; D1217 — force-output / finish-output / clear-output trampoline to the gray
;;; generics (stream-force-output etc.) instead of "not a stream designator".
(deftest d1217-gray-force-output
  (handler-case (progn (force-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)

(deftest d1217-gray-finish-output
  (handler-case (progn (finish-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)

(deftest d1217-gray-clear-output
  (handler-case (progn (clear-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)
