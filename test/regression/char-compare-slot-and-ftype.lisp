;;; CHAR= / CHAR< ... whose operand is a CHARACTER structure slot or a call of a function declared to
;;; return a CHARACTER compare the codes, without the generic comparison.

(setf dotcl:*save-sil* t)

(defstruct (ccs-tok) (ch #\space :type character) (k #\a :type character))
(declaim (ftype (function (t) character) ccs-first-char))
(defun ccs-first-char (s) (char s 0))

(defun ccs-slot= (tk) (if (char= (ccs-tok-ch tk) #\space) 1 2))
(defun ccs-slot-slot (a b) (if (char< (ccs-tok-ch a) (ccs-tok-k b)) :lt :ge))
(defun ccs-call (s) (if (char= (ccs-first-char s) #\a) 1 2))
(defun ccs-call-slot (s tk) (if (char/= (ccs-first-char s) (ccs-tok-k tk)) :ne :eq))

(defun %ccs-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil (symbol-function fn)))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n) (setf p (1+ q))))))

(deftest char-compare-slot-and-ftype.results
  (list (ccs-slot= (make-ccs-tok)) (ccs-slot= (make-ccs-tok :ch #\x))
        (ccs-slot-slot (make-ccs-tok :ch #\a) (make-ccs-tok :k #\b))
        (ccs-slot-slot (make-ccs-tok :ch #\c) (make-ccs-tok :k #\b))
        (ccs-call "abc") (ccs-call "xbc")
        (ccs-call-slot "a" (make-ccs-tok)) (ccs-call-slot "b" (make-ccs-tok)))
  (1 2 :lt :ge 1 2 :eq :ne))

(deftest-compiled-only char-compare-slot-and-ftype.no-generic-comparison
  (mapcar (lambda (f) (%ccs-count f "IsTrueCharEq"))
          '(ccs-slot= ccs-slot-slot ccs-call ccs-call-slot))
  (0 0 0 0))

(deftest char-compare-slot-and-ftype.not-a-character-is-an-error
  (handler-case (progn (setf (ccs-tok-ch (make-ccs-tok)) 5) :no-error)
    (error () :error))
  :error)
