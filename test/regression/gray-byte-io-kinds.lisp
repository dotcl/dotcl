;;; WRITE-BYTE / READ-BYTE / OPEN-STREAM-P on Gray streams, including a class
;;; that becomes a Gray stream only when it is redefined with a new superclass.
(require "dotcl-gray")

(defclass %gbk-out (dotcl-gray:fundamental-binary-output-stream)
  ((bytes :initform nil :accessor %gbk-bytes)))
(defmethod dotcl-gray:stream-write-byte ((s %gbk-out) b) (push b (%gbk-bytes s)) b)

(defclass %gbk-in (dotcl-gray:fundamental-binary-input-stream)
  ((data :initarg :data :accessor %gbk-data)))
(defmethod dotcl-gray:stream-read-byte ((s %gbk-in))
  (if (%gbk-data s) (pop (%gbk-data s)) :eof))

(deftest gray-byte-io-kinds.write-byte
  (let ((s (make-instance '%gbk-out)))
    (list (write-byte 1 s) (write-byte 255 s) (reverse (%gbk-bytes s))
          (open-stream-p s) (output-stream-p s)))
  (1 255 (1 255) t t))

(deftest gray-byte-io-kinds.read-byte
  (let ((s (make-instance '%gbk-in :data (list 7 8))))
    (list (read-byte s) (read-byte s nil :end) (read-byte s nil :end)
          (handler-case (read-byte s) (end-of-file () :eof-error))
          (open-stream-p s)))
  (7 8 :end :eof-error t))

(deftest gray-byte-io-kinds.two-way
  (let* ((o (make-instance '%gbk-out))
         (i (make-instance '%gbk-in :data (list 3)))
         (tw (make-two-way-stream i o)))
    (write-byte 9 tw)
    (list (read-byte tw) (%gbk-bytes o)))
  (3 (9)))

(defclass %gbk-late () ((bytes :initform nil :accessor %gbk-late-bytes)))

(deftest gray-byte-io-kinds.redefined-into-stream
  (let ((s (make-instance '%gbk-late)))
    (list (handler-case (write-byte 1 s) (type-error () :type-error))
          (handler-case (open-stream-p s) (type-error () :type-error))
          (progn
            (defclass %gbk-late (dotcl-gray:fundamental-binary-output-stream)
              ((bytes :initform nil :accessor %gbk-late-bytes)))
            (defmethod dotcl-gray:stream-write-byte ((s %gbk-late) b)
              (push b (%gbk-late-bytes s)) b)
            (write-byte 5 s))
          (open-stream-p s)
          (%gbk-late-bytes s)))
  (:type-error :type-error 5 t (5)))

;;; OPEN-STREAM-P with methods on some classes only: other arguments go to the
;;; built-in, also on repeated calls, until a method for their class is added.
(defclass %gbk-osp-a (dotcl-gray:fundamental-binary-output-stream) ())
(defclass %gbk-osp-b (dotcl-gray:fundamental-binary-output-stream) ())
(defmethod open-stream-p ((s %gbk-osp-a)) :a)

(deftest gray-byte-io-kinds.open-stream-p-fallback
  (let ((a (make-instance '%gbk-osp-a))
        (b (make-instance '%gbk-osp-b))
        (str (make-string-output-stream)))
    (let ((before (loop repeat 3 collect (list (open-stream-p a) (open-stream-p b)
                                               (open-stream-p str)))))
      (close str)
      (defmethod open-stream-p ((s %gbk-osp-b)) :b)
      (list before (open-stream-p a) (open-stream-p b) (open-stream-p str)
            (handler-case (open-stream-p 3) (type-error () :type-error)))))
  (((:a t t) (:a t t) (:a t t)) :a :b nil :type-error))

;;; A class that is only a FUNDAMENTAL-OUTPUT-STREAM / FUNDAMENTAL-INPUT-STREAM
;;; (fast-io's streams) still sends byte, character and sequence I/O to the
;;; Gray generics, as on SBCL.
(defclass %gbk-plain-out (dotcl-gray:fundamental-output-stream)
  ((items :initform nil :accessor %gbk-items)))
(defmethod dotcl-gray:stream-write-byte ((s %gbk-plain-out) b) (push (list :b b) (%gbk-items s)) b)
(defmethod dotcl-gray:stream-write-char ((s %gbk-plain-out) c) (push (list :c c) (%gbk-items s)) c)
(defmethod dotcl-gray:stream-write-sequence ((s %gbk-plain-out) seq &optional start end)
  (push (list :seq (coerce (subseq seq start (or end (length seq))) 'list)) (%gbk-items s)) seq)

(defclass %gbk-plain-in (dotcl-gray:fundamental-input-stream)
  ((data :initarg :data :accessor %gbk-plain-data)))
(defmethod dotcl-gray:stream-read-byte ((s %gbk-plain-in))
  (if (%gbk-plain-data s) (pop (%gbk-plain-data s)) :eof))
(defmethod dotcl-gray:stream-read-char ((s %gbk-plain-in))
  (if (%gbk-plain-data s) (code-char (pop (%gbk-plain-data s))) :eof))
(defmethod dotcl-gray:stream-read-sequence ((s %gbk-plain-in) seq &optional start end)
  (let ((end (or end (length seq))))
    (loop for i from start below end do (setf (elt seq i) (pop (%gbk-plain-data s))))
    end))

(deftest gray-byte-io-kinds.plain-output-stream
  (let ((s (make-instance '%gbk-plain-out))
        (v (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (list (write-byte 7 s) (write-char #\a s) (progn (write-sequence v s :start 1) nil)
          (reverse (%gbk-items s)) (output-stream-p s) (input-stream-p s)))
  (7 #\a nil ((:b 7) (:c #\a) (:seq (2 3))) t nil))

(deftest gray-byte-io-kinds.plain-input-stream
  (let ((s (make-instance '%gbk-plain-in :data (list 65 66 67 68 69)))
        (v (make-array 3 :initial-element 0)))
    (list (read-byte s) (read-char s) (read-sequence v s :start 1) (coerce v 'list)
          (read-byte s nil :end) (read-byte s nil :end) (input-stream-p s)))
  (65 #\B 3 (0 67 68) 69 :end t))
