;;; The string character fast path against the general per-element path.

(defparameter *pctb-alphabet*
  (coerce (list #\a #\A #\b #\B #\c #\C #\x #\X #\y #\Y #\z #\Z
                #\0 #\1 #\9 #\Space #\- #\_
                (code-char 233) (code-char 201) (code-char 223))
          'vector))

(defvar *pctb-seed* 0)

(defun %pctb-random ()
  (setf *pctb-seed* (mod (+ (* *pctb-seed* 1103) 12345) 65536)))

(defun %pctb-fill (n)
  (let ((s (make-string n)))
    (dotimes (i n s)
      (setf (char s i)
            (aref *pctb-alphabet* (mod (%pctb-random) (length *pctb-alphabet*)))))))

(defun %pctb-general-eql (a b) (eql a b))
(defun %pctb-general-equal (a b) (equal a b))
(defun %pctb-general-equalp (a b) (equalp a b))
(defun %pctb-general-char= (a b) (char= a b))
(defun %pctb-general-char-equal (a b) (char-equal a b))

(defparameter *pctb-tests*
  '((eql . %pctb-general-eql)
    (equal . %pctb-general-equal)
    (equalp . %pctb-general-equalp)
    (char= . %pctb-general-char=)
    (char-equal . %pctb-general-char-equal)))

(defun %pctb-sequences (n)
  (let* ((str (%pctb-fill n))
         (vec (make-array n :element-type 'character))
         (base (make-array n :element-type 'base-char))
         (fp (make-array n :element-type 'character :fill-pointer n))
         (backing (%pctb-fill (+ n 4)))
         (disp (make-array n :element-type 'character
                           :displaced-to backing
                           :displaced-index-offset (if (zerop n) 0 2))))
    (dotimes (i n)
      (let ((c (char str i)))
        (setf (aref vec i) c
              (aref fp i) c
              (aref base i) (if (< (char-code c) 128) c #\?))))
    (list str vec base fp disp (coerce str 'list))))

(defun %pctb-mismatch (name item seq start end from-end fast slow got want)
  (list name item start end from-end fast slow got want))

(defun %pctb-check-sequence (seq)
  (let ((n (length seq)))
    (loop for (fast . slow) in *pctb-tests* do
      (loop for start from 0 to n do
        (loop for end from start to n do
          (dolist (from-end '(nil t))
            (loop for item across *pctb-alphabet* do
              (let ((got (position item seq :test fast :start start :end end
                                   :from-end from-end))
                    (want (position item seq :test slow :start start :end end
                                    :from-end from-end)))
                (unless (eql got want)
                  (return-from %pctb-check-sequence
                    (%pctb-mismatch :position item seq start end from-end fast slow got want))))
              (let ((got (find item seq :test fast :start start :end end
                               :from-end from-end))
                    (want (find item seq :test slow :start start :end end
                                :from-end from-end)))
                (unless (eql got want)
                  (return-from %pctb-check-sequence
                    (%pctb-mismatch :find item seq start end from-end fast slow got want)))))))))
    (loop for start from 0 to n do
      (loop for end from start to n do
        (dolist (from-end '(nil t))
          (loop for item across *pctb-alphabet* do
            (let ((got (position item seq :start start :end end :from-end from-end))
                  (want (position item seq :test #'%pctb-general-eql
                                  :start start :end end :from-end from-end)))
              (unless (eql got want)
                (return-from %pctb-check-sequence
                  (%pctb-mismatch :position/no-test item seq start end from-end
                                  :eql :eql got want))))
            (let ((got (find item seq :start start :end end :from-end from-end))
                  (want (find item seq :test #'%pctb-general-eql
                              :start start :end end :from-end from-end)))
              (unless (eql got want)
                (return-from %pctb-check-sequence
                  (%pctb-mismatch :find/no-test item seq start end from-end
                                  :eql :eql got want))))))))
    t))

(defun %pctb-check-all (lengths)
  (dolist (n lengths t)
    (dolist (seq (%pctb-sequences n))
      (let ((r (%pctb-check-sequence seq)))
        (unless (eq r t) (return-from %pctb-check-all r))))))

(deftest position-char-test-bounds.values
  (progn
    (setf *pctb-seed* 20261025)
    (%pctb-check-all '(0 1 2 5 11)))
  t)

(defun %pctb-outcome (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (type-error () :type-error)
    (error () :error)))

(defun %pctb-same-outcome (fast slow)
  (let ((a (%pctb-outcome fast)) (b (%pctb-outcome slow)))
    (list a (eql a b))))

(deftest position-char-test-bounds.errors
  (let ((str "abcabc") (vec (make-array 6 :element-type 'character
                                        :initial-contents "abcabc")))
    (list
     (%pctb-same-outcome (lambda () (position #\a str :start -1 :test #'char-equal))
                         (lambda () (position #\a str :start -1 :test #'%pctb-general-char-equal)))
     (%pctb-same-outcome (lambda () (position #\a str :start 7 :test #'char-equal))
                         (lambda () (position #\a str :start 7 :test #'%pctb-general-char-equal)))
     (%pctb-same-outcome (lambda () (position #\a str :end 7 :test #'equal))
                         (lambda () (position #\a str :end 7 :test #'%pctb-general-equal)))
     (%pctb-same-outcome (lambda () (position #\a str :start 4 :end 2 :test #'equalp))
                         (lambda () (position #\a str :start 4 :end 2 :test #'%pctb-general-equalp)))
     (%pctb-same-outcome (lambda () (find #\a str :start 7 :test #'char=))
                         (lambda () (find #\a str :start 7 :test #'%pctb-general-char=)))
     (%pctb-same-outcome (lambda () (position 1 str :test #'char-equal))
                         (lambda () (position 1 str :test #'%pctb-general-char-equal)))
     (%pctb-same-outcome (lambda () (position #\a vec :test #'char=))
                         (lambda () (position #\a vec :test #'%pctb-general-char=)))
     (%pctb-same-outcome (lambda () (position #\a 5 :test #'equal))
                         (lambda () (position #\a 5 :test #'%pctb-general-equal)))))
    ((:type-error t) (:type-error t) (:type-error t) (:type-error t)
     (:type-error t) (:type-error t) (:ok t) (:type-error t)))

(deftest position-char-test-bounds.non-character-item
  (list (position 1 "abc" :test #'equal)
        (position 1 "abc" :test #'equalp)
        (find 1 "abc" :test #'equal)
        (find 1 "abc" :test #'equalp)
        (position #\a "AbC" :test #'equal)
        (position #\a "AbC" :test #'equalp)
        (find #\a "AbC" :test #'equal)
        (find #\a "AbC" :test #'equalp))
  (nil nil nil nil nil 0 nil #\A))
