;;; A Gray output stream can be a component of a broadcast stream (SBCL writes to it);
;;; MAKE-BROADCAST-STREAM refused it with a TYPE-ERROR. Gray streams inside two-way, echo
;;; and concatenated streams already worked and keep working.
(require "dotcl-gray")

(defvar *gsc-log* nil)
(defclass gsc-out (dotcl-gray:fundamental-character-output-stream) ())
(defmethod dotcl-gray:stream-write-char ((s gsc-out) c) (push c *gsc-log*) c)
(defclass gsc-in (dotcl-gray:fundamental-character-input-stream) ((chars :initarg :chars :accessor gsc-chars)))
(defmethod dotcl-gray:stream-read-char ((s gsc-in)) (if (gsc-chars s) (pop (gsc-chars s)) :eof))
(defmethod dotcl-gray:stream-unread-char ((s gsc-in) c) (push c (gsc-chars s)) nil)

(deftest gray-stream-in-composites.broadcast-write
  (progn (setf *gsc-log* nil)
         (let ((b (make-broadcast-stream (make-instance 'gsc-out))))
           (write-string "hi" b)
           (write-char #\! b)
           (format b "~A" 7))
         (coerce (reverse *gsc-log*) 'string))
  "hi!7")

(deftest gray-stream-in-composites.broadcast-with-a-string-stream
  (progn (setf *gsc-log* nil)
         (let* ((s (make-string-output-stream))
                (b (make-broadcast-stream (make-instance 'gsc-out) s)))
           (write-string "ab" b)
           (list (coerce (reverse *gsc-log*) 'string) (get-output-stream-string s))))
  ("ab" "ab"))

(deftest gray-stream-in-composites.not-an-output-stream-still-errors
  (list (handler-case (make-broadcast-stream (make-instance 'gsc-in :chars nil)) (type-error () :type-error))
        (handler-case (make-broadcast-stream 42) (type-error () :type-error))
        (handler-case (make-broadcast-stream (make-string-input-stream "x")) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest gray-stream-in-composites.two-way-echo-concatenated
  (progn (setf *gsc-log* nil)
         (let ((tw (make-two-way-stream (make-instance 'gsc-in :chars (list #\q)) (make-instance 'gsc-out)))
               (ec (make-echo-stream (make-instance 'gsc-in :chars (list #\e)) (make-instance 'gsc-out)))
               (cc (make-concatenated-stream (make-string-input-stream "") (make-instance 'gsc-in :chars (list #\z)))))
           (write-string "w" tw)
           (list (read-char tw) (read-char ec) (read-char cc) (coerce (reverse *gsc-log*) 'string))))
  (#\q #\e #\z "we"))
