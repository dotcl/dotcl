;;; A Gray STREAM-WRITE-STRING method is called with START and END (CLHS 21.1.2 in
;;; the Gray streams proposal, as on SBCL): the whole string is 0 to its length. A
;;; method that does (subseq string start end) used to get NIL NIL.
(require "dotcl-gray")

(defvar *gws-seen* nil)
(defclass %gws-probe (dotcl-gray:fundamental-character-output-stream) ())
(defmethod dotcl-gray:stream-write-char ((s %gws-probe) c) c)
(defmethod dotcl-gray:stream-write-string ((s %gws-probe) string &optional start end)
  (push (list string start end) *gws-seen*)
  (subseq string start end))

(deftest gray-write-string-start-end.write-string
  (progn (setf *gws-seen* nil)
         (write-string "abc" (make-instance '%gws-probe))
         (reverse *gws-seen*))
  (("abc" 0 3)))

(deftest gray-write-string-start-end.substring-is-the-same-text
  (progn (setf *gws-seen* nil)
         (write-string "abcdef" (make-instance '%gws-probe) :start 1 :end 3)
         (destructuring-bind ((string start end)) (reverse *gws-seen*)
           (subseq string start end)))
  "bc")

(deftest gray-write-string-start-end.format
  (progn (setf *gws-seen* nil)
         (format (make-instance '%gws-probe) "hi")
         (destructuring-bind ((string start end)) (reverse *gws-seen*)
           (subseq string start end)))
  "hi")

(deftest gray-write-string-start-end.write-char-stream-still-works
  (let ((s (make-instance '%gws-probe)))
    (write-char #\a s)
    (princ 12 s)
    t)
  t)

;;; With :START / :END, WRITE-STRING and WRITE-LINE hand the method the whole string and the
;;; bounds, as SBCL does, not the substring and 0 / its length.
(deftest gray-write-string-start-end.bounds-are-passed-through
  (progn (setf *gws-seen* nil)
         (write-string "abcdef" (make-instance '%gws-probe) :start 1 :end 4)
         (write-line "abcdef" (make-instance '%gws-probe) :start 1 :end 4)
         (reverse *gws-seen*))
  (("abcdef" 1 4) ("abcdef" 1 4)))
