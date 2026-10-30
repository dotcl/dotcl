;;; (POSITION ITEM SEQ :TEST #'CHAR-EQUAL) and (... :TEST #'CHAR=) call the runtime
;;; entry directly; the results and the errors are those of the general call.

(setf dotcl:*save-sil* t)

(defun %pctd-hex (c) (position c "0123456789abcdef" :test #'char-equal))
(defun %pctd-eq (c s) (position c s :test #'char=))
(defun %pctd-local-test (c)
  (flet ((test (a b) (eql a (char-upcase b))))
    (position c "abc" :test #'test)))
(defun %pctd-local-position (c)
  (flet ((position (&rest r) (list :local (length r))))
    (position c "abc" :test #'char-equal)))
(defun %pctd-eql (c s) (position c s :test #'eql))
(defun %pctd-equal (c s) (position c s :test #'equal))
(defun %pctd-equalp (c s) (position c s :test #'equalp))
(defun %pctd-find-equal (c s) (find c s :test #'equal))

(deftest position-char-test-direct.values
  (list (%pctd-hex #\A) (%pctd-hex #\a) (%pctd-hex #\z)
        (%pctd-eq #\b "abc") (%pctd-eq #\B "abc") (%pctd-eq #\b (vector #\a #\b))
        (%pctd-local-test #\B) (%pctd-local-position #\a)
        (multiple-value-list (%pctd-hex #\f)))
  (10 10 nil 1 nil 1 1 (:local 4) (15)))

(deftest position-char-test-direct.errors
  (list (handler-case (%pctd-hex 5) (type-error () :type-error))
        (handler-case (%pctd-eq #\a 5) (type-error () :type-error))
        (handler-case (%pctd-eq #\b (vector 1 #\b)) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest-compiled-only position-char-test-direct.direct
  (list (and (search "Runtime.PositionLiteralString" (%flat-text (dotcl:function-sil #'%pctd-hex))) t)
        (and (search "Runtime.PositionCharEq" (%flat-text (dotcl:function-sil #'%pctd-eq))) t)
        (and (search "Runtime.PositionEql" (%flat-text (dotcl:function-sil #'%pctd-eql))) t)
        (and (search "Runtime.PositionEqual" (%flat-text (dotcl:function-sil #'%pctd-equal))) t)
        (and (search "Runtime.PositionEqualp" (%flat-text (dotcl:function-sil #'%pctd-equalp))) t)
        (and (search "Runtime.FindEqual" (%flat-text (dotcl:function-sil #'%pctd-find-equal))) t))
  (t t t t t t))
