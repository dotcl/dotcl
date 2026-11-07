;;; A COND whose arms are integers is read by the typed arithmetic paths as the
;;; nested IF it is, so (+ i (cond ...)) is computed raw. Values, fall-through to
;;; NIL, bodies with several forms and side effects are those of COND.

(defvar *cia-log* nil)

(defun %cia-next (i o)
  (declare (fixnum i) (type (unsigned-byte 8) o))
  (let* ((x (+ i (cond ((< o 5) 1) ((< o 9) 2) ((< o 200) 3) (t 4)))))
    (declare (fixnum x))
    (* x 2)))

(defun %cia-multi (i o)
  (declare (fixnum i) (type (unsigned-byte 8) o))
  (+ i (cond ((< o 5) (push :a *cia-log*) 10) ((< o 9) 20) (t 30))))

(defun %cia-nil (i o)
  (declare (fixnum i) (type (unsigned-byte 8) o))
  (cond ((< o 5) (+ i 1)) ((< o 9) (+ i 2))))

(defun %cia-test-only (o) (declare (type (unsigned-byte 8) o)) (cond ((< o 5)) (t 7)))

(deftest cond-integer-arms-typed.values
  (progn (setq *cia-log* nil)
  (list (%cia-next 10 0) (%cia-next 10 6) (%cia-next 10 100) (%cia-next 10 250)
        (%cia-multi 1 0) (%cia-multi 1 6) (%cia-multi 1 50) (reverse *cia-log*)
        (%cia-nil 1 0) (%cia-nil 1 6) (%cia-nil 1 50)
        (%cia-test-only 1) (%cia-test-only 50)))
  (22 24 26 28 11 21 31 (:a) 2 3 nil t 7))
