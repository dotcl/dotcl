;;; LISTEN, CLEAR-INPUT, WRITE-BYTE, CLEAR-OUTPUT and STREAM-ELEMENT-TYPE reach
;;; a Gray stream also when it is a component of a two-way, echo or synonym
;;; stream, and LISTEN on a Gray stream asks STREAM-LISTEN. LISTEN looked at
;;; standard input instead, WRITE-BYTE on such a two-way stream said it was not
;;; a binary output stream, CLEAR-INPUT / CLEAR-OUTPUT did nothing, and the
;;; two-way stream reported CHARACTER. cl-octet-streams connects two pipes
;;; (Gray streams) with MAKE-TWO-WAY-STREAM and failed 13 checks. Expected
;;; values are SBCL 2.6.8's answers.
(require "dotcl-gray")

(defclass %gcc-bin (dotcl-gray:fundamental-binary-input-stream
                    dotcl-gray:fundamental-binary-output-stream)
  ((data :initarg :data :initform (list 1 2 3) :accessor %gcc-data)
   (out :initform nil :accessor %gcc-out)))
(defmethod stream-element-type ((s %gcc-bin)) '(unsigned-byte 8))
(defmethod dotcl-gray:stream-read-byte ((s %gcc-bin))
  (if (%gcc-data s) (pop (%gcc-data s)) :eof))
(defmethod dotcl-gray:stream-write-byte ((s %gcc-bin) b) (push b (%gcc-out s)) b)
(defmethod dotcl-gray:stream-listen ((s %gcc-bin)) (and (%gcc-data s) t))
(defmethod dotcl-gray:stream-clear-input ((s %gcc-bin)) (setf (%gcc-data s) nil))
(defmethod dotcl-gray:stream-clear-output ((s %gcc-bin)) (setf (%gcc-out s) (list :cleared)))

(defclass %gcc-chars (dotcl-gray:fundamental-character-input-stream)
  ((chars :initarg :chars :accessor %gcc-chars)))
(defmethod dotcl-gray:stream-read-char ((s %gcc-chars))
  (if (%gcc-chars s) (pop (%gcc-chars s)) :eof))
(defmethod dotcl-gray:stream-unread-char ((s %gcc-chars) c) (push c (%gcc-chars s)) nil)

(deftest gray-composite-components.listen
  (let ((s (make-instance '%gcc-bin :data (list 1)))
        (empty (make-instance '%gcc-bin :data nil)))
    (list (listen s) (listen empty)
          (listen (make-two-way-stream s empty))
          (listen (make-two-way-stream empty s))
          (listen (make-echo-stream s empty))))
  (t nil t nil t))

;; A character stream with no STREAM-LISTEN method of its own: the default reads
;; a character without waiting and puts it back.
(deftest gray-composite-components.listen-character-default
  (let ((s (make-instance '%gcc-chars :chars (list #\a)))
        (empty (make-instance '%gcc-chars :chars nil)))
    (list (listen s) (read-char s nil :eof) (listen s) (listen empty)))
  (t #\a nil nil))

(deftest gray-composite-components.write-byte
  (let* ((in (make-instance '%gcc-bin)) (out (make-instance '%gcc-bin))
         (tw (make-two-way-stream in out)))
    (list (write-byte 9 tw) (write-byte 10 tw) (%gcc-out out) (%gcc-out in)))
  (9 10 (10 9) nil))

(deftest gray-composite-components.read-byte
  (let* ((in (make-instance '%gcc-bin)) (tw (make-two-way-stream in (make-instance '%gcc-bin))))
    (list (read-byte tw) (read-byte tw) (read-byte tw) (read-byte tw nil :eof)))
  (1 2 3 :eof))

(deftest gray-composite-components.clear-input
  (let* ((in (make-instance '%gcc-bin)) (tw (make-two-way-stream in (make-instance '%gcc-bin))))
    (clear-input tw)
    (read-byte tw nil :eof))
  :eof)

(deftest gray-composite-components.clear-output
  (let* ((out (make-instance '%gcc-bin)) (tw (make-two-way-stream (make-instance '%gcc-bin) out)))
    (write-byte 1 tw)
    (clear-output tw)
    (%gcc-out out))
  (:cleared))

(deftest gray-composite-components.element-type
  (stream-element-type (make-two-way-stream (make-instance '%gcc-bin) (make-instance '%gcc-bin)))
  (unsigned-byte 8))
