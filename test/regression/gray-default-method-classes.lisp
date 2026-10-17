;;; dotcl-gray's default methods sit on the classes the Gray proposal and SBCL
;;; give them: the column / line-start / string output defaults on
;;; FUNDAMENTAL-CHARACTER-OUTPUT-STREAM, STREAM-LISTEN and
;;; STREAM-READ-CHAR-NO-HANG on FUNDAMENTAL-CHARACTER-INPUT-STREAM. A binary
;;; stream without its own methods gets no applicable method.
(require "dotcl-gray")

(defclass %gdm-bin-in (dotcl-gray:fundamental-binary-input-stream) ())
(defclass %gdm-bin-out (dotcl-gray:fundamental-binary-output-stream) ())
(defclass %gdm-chr-out (dotcl-gray:fundamental-character-output-stream)
  ((out :initform nil :accessor %gdm-out)))
(defmethod dotcl-gray:stream-write-char ((s %gdm-chr-out) c) (push c (%gdm-out s)) c)

(defun %gdm-err (thunk) (handler-case (progn (funcall thunk) :no-error) (error () :error)))

(deftest gray-default-method-classes.binary-has-no-character-defaults
  (let ((i (make-instance '%gdm-bin-in))
        (o (make-instance '%gdm-bin-out)))
    (list (%gdm-err (lambda () (dotcl-gray:stream-listen i)))
          (%gdm-err (lambda () (dotcl-gray:stream-read-char-no-hang i)))
          (%gdm-err (lambda () (dotcl-gray:stream-start-line-p o)))
          (%gdm-err (lambda () (dotcl-gray:stream-advance-to-column o 3)))
          (%gdm-err (lambda () (dotcl-gray:stream-line-column o)))
          (%gdm-err (lambda () (dotcl-gray:stream-force-output o)))))
  (:error :error :error :error :error :no-error))

(deftest gray-default-method-classes.character-output-defaults
  (let ((o (make-instance '%gdm-chr-out)))
    (list (dotcl-gray:stream-line-column o) (dotcl-gray:stream-start-line-p o)
          (write-string "ab" o) (terpri o) (coerce (reverse (%gdm-out o)) 'string)))
  (nil nil "ab" nil #.(format nil "ab~%")))

;; A binary stream that defines STREAM-READ-CHAR / STREAM-WRITE-CHAR itself is
;; read and written a character at a time through them; the operations whose
;; defaults are for character streams only signal, as on SBCL.
(defclass %gdm-bival-in (dotcl-gray:fundamental-binary-input-stream)
  ((s :initarg :s :accessor %gdm-s) (p :initform 0 :accessor %gdm-p)))
(defmethod dotcl-gray:stream-read-char ((s %gdm-bival-in))
  (if (< (%gdm-p s) (length (%gdm-s s)))
      (prog1 (char (%gdm-s s) (%gdm-p s)) (incf (%gdm-p s)))
      :eof))
(defclass %gdm-bival-out (dotcl-gray:fundamental-binary-output-stream)
  ((out :initform nil :accessor %gdm-out)))
(defmethod dotcl-gray:stream-write-char ((s %gdm-bival-out) c) (push c (%gdm-out s)) c)

(deftest gray-default-method-classes.binary-with-character-methods
  (flet ((in () (make-instance '%gdm-bival-in :s (format nil "ab~%")))
         (out () (make-instance '%gdm-bival-out)))
    (list (read-char (in))
          (let ((o (out))) (write-char #\x o) (%gdm-out o))
          (%gdm-err (lambda () (read-line (in))))
          (%gdm-err (lambda () (peek-char nil (in))))
          (%gdm-err (lambda () (listen (in))))
          (%gdm-err (lambda () (terpri (out))))
          (%gdm-err (lambda () (write-string "ab" (out))))))
  (#\a (#\x) :error :error :error :error :error))
