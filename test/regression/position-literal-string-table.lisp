;;; POSITION / FIND on a literal string answer from a table the compiler builds.

(setf dotcl:*save-sil* t)

(defparameter *plst-literal* "0123456789abcdefABC-éÉĀā")
(defparameter *plst-variable* (copy-seq *plst-literal*))
(defconstant +plst-const+ "0123456789abcdefABC-éÉĀā")

(defparameter *plst-codes*
  (append (loop for i from 0 to 255 collect i)
          '(256 257 258 300 1000 4660 #xFFFF)))

(defmacro %plst-compare (op test &rest keys)
  (let ((test-keys (if test `(:test (function ,test)) nil)))
    `(loop for code in *plst-codes*
           do (let* ((item (code-char code))
                     (got (,op item "0123456789abcdefABC-éÉĀā"
                               ,@test-keys ,@keys))
                     (want (,op item *plst-variable*
                                ,@test-keys ,@keys)))
                (unless (eql got want)
                  (return (list ',op ',test ',keys code got want))))
           finally (return t))))

(defun %plst-all ()
  (macrolet ((chk (op test &rest keys)
               `(let ((r (%plst-compare ,op ,test ,@keys)))
                  (unless (eq r t) (return-from %plst-all r)))))
    (chk position eql)
    (chk position eql :start 3)
    (chk position eql :end 7)
    (chk position eql :start 3 :end 12)
    (chk position eql :from-end t)
    (chk position eql :start 2 :end 20 :from-end t)
    (chk position eql :start 12 :end 12)
    (chk position eql :start 24)
    (chk position char=)
    (chk position char= :start 3)
    (chk position char= :end 7)
    (chk position char= :start 3 :end 12)
    (chk position char= :from-end t)
    (chk position char= :start 2 :end 20 :from-end t)
    (chk position char= :start 12 :end 12)
    (chk position char= :start 24)
    (chk position char-equal)
    (chk position char-equal :start 3)
    (chk position char-equal :end 7)
    (chk position char-equal :start 3 :end 12)
    (chk position char-equal :from-end t)
    (chk position char-equal :start 2 :end 20 :from-end t)
    (chk position char-equal :start 12 :end 12)
    (chk position char-equal :start 24)
    (chk find eql)
    (chk find eql :start 3)
    (chk find eql :end 7)
    (chk find eql :start 3 :end 12)
    (chk find eql :from-end t)
    (chk find eql :start 2 :end 20 :from-end t)
    (chk find eql :start 12 :end 12)
    (chk find eql :start 24)
    (chk find char=)
    (chk find char= :start 3)
    (chk find char= :end 7)
    (chk find char= :start 3 :end 12)
    (chk find char= :from-end t)
    (chk find char= :start 2 :end 20 :from-end t)
    (chk find char= :start 12 :end 12)
    (chk find char= :start 24)
    (chk find char-equal)
    (chk find char-equal :start 3)
    (chk find char-equal :end 7)
    (chk find char-equal :start 3 :end 12)
    (chk find char-equal :from-end t)
    (chk find char-equal :start 2 :end 20 :from-end t)
    (chk find char-equal :start 12 :end 12)
    (chk find char-equal :start 24)
    (chk position nil)
    (chk position nil :start 3 :end 12)
    (chk position nil :from-end t)
    (chk find nil)
    (chk find nil :start 2 :end 20 :from-end t)
    t))

(deftest position-literal-string-table.values
  (%plst-all)
  t)

(defun %plst-outcome (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (type-error () :type-error)
    (error () :error)))

(defun %plst-same-outcome (fast slow)
  (let ((a (%plst-outcome fast)) (b (%plst-outcome slow)))
    (list a (eql a b))))

(deftest position-literal-string-table.non-character
  (list
   (position 1 *plst-literal* :test #'eql)
   (find 1 *plst-literal* :test #'eql)
   (%plst-same-outcome
    (lambda () (position 1 *plst-literal* :test #'char=))
    (lambda () (position 1 *plst-variable* :test #'char=)))
   (%plst-same-outcome
    (lambda () (find 1 *plst-literal* :test #'char-equal))
    (lambda () (find 1 *plst-variable* :test #'char-equal)))
   (%plst-same-outcome
    (lambda () (position nil *plst-literal* :test #'char-equal))
    (lambda () (position nil *plst-variable* :test #'char-equal))))
  (nil nil (:type-error t) (:type-error t) (:type-error t)))

(defun %plst-hex-position (c)
  (position c "0123456789abcdef" :test #'char-equal))

(defun %plst-hex-find (c)
  (find c "0123456789abcdef" :test #'char-equal))

(defun %plst-hex-plain (c)
  (position c "0123456789abcdef"))

(defun %plst-hex-window (c)
  (position c "0123456789abcdef" :start 2 :end 12 :from-end t))

(defun %plst-const (c)
  (position c +plst-const+ :test #'char-equal))

(defun %plst-variable (c s)
  (position c s :test #'char-equal))

(defun %plst-key (c)
  (position c "0123456789abcdef" :test #'char-equal :key #'identity))

(defun %plst-other-test (c)
  (position c "0123456789abcdef" :test #'equal))

(defun %plst-typed-locals (c s)
  (let* ((n (length s))
         (last (and (plusp n) (char s (1- n)))))
    (declare (type fixnum n))
    (list n last (position c "0123456789abcdef" :test #'char-equal))))

(defun %plst-sil-has-cast (fn op)
  (let ((text (%flat-text (dotcl:function-sil fn))))
    (and (search op text)
         (search "(CASTCLASS LispString)" text)
         t)))

(deftest position-literal-string-table.typed-locals
  (%plst-typed-locals #\A "0123456789abcdef")
  (16 #\f 10))

(deftest-compiled-only position-literal-string-table.stack-types
  (list (%plst-sil-has-cast #'%plst-hex-position "Runtime.PositionLiteralString")
        (%plst-sil-has-cast #'%plst-hex-find "Runtime.FindLiteralString")
        (%plst-sil-has-cast #'%plst-typed-locals "Runtime.PositionLiteralString"))
  (t t t))

(deftest-compiled-only position-literal-string-table.compiled
  (list
   (and (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-hex-position))) t)
   (and (search "Runtime.FindLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-hex-find))) t)
   (and (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-hex-plain))) t)
   (and (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-hex-window))) t)
   (and (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-const))) t)
   (not (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-variable))))
   (not (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-key))))
   (not (search "Runtime.PositionLiteralString"
                (%flat-text (dotcl:function-sil #'%plst-other-test)))))
  (t t t t t t t t))
