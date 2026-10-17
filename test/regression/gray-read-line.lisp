;;; READ-LINE on a Gray stream calls STREAM-READ-LINE (Gray proposal, SBCL);
;;; the default method on FUNDAMENTAL-CHARACTER-INPUT-STREAM reads with
;;; STREAM-READ-CHAR.
(require "dotcl-gray")

(defclass %grl-in (dotcl-gray:fundamental-character-input-stream)
  ((data :initarg :data :accessor %grl-data)
   (calls :initform nil :accessor %grl-calls)))
(defmethod dotcl-gray:stream-read-char ((s %grl-in))
  (if (%grl-data s) (pop (%grl-data s)) :eof))
(defmethod dotcl-gray:stream-read-line :before ((s %grl-in))
  (push :read-line (%grl-calls s)))

(defclass %grl-own (dotcl-gray:fundamental-character-input-stream) ())
(defmethod dotcl-gray:stream-read-line ((s %grl-own)) (values "own" t))

(deftest gray-read-line.default-method
  (let ((s (make-instance '%grl-in :data (coerce (format nil "ab~%c") 'list))))
    (list (multiple-value-list (read-line s))
          (multiple-value-list (read-line s))
          (multiple-value-list (read-line s nil :end))
          (handler-case (read-line s) (end-of-file () :eof))
          (%grl-calls s)))
  (("ab" nil) ("c" t) (:end t) :eof (:read-line :read-line :read-line :read-line)))

(deftest gray-read-line.own-method
  (multiple-value-list (read-line (make-instance '%grl-own)))
  ("own" t))
