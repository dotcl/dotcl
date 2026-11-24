;;; (TYPEP x 'compound-type) with a constant type is rewritten into tests of its parts. 
;;; The table below is SBCL 2.6.8's answer for each type against 35 objects (1 = true), the same list on every line.
;;; The objects: see %TCT-OBJECTS. The compiled constant form must give SBCL's answers.

(defstruct tct-s a)
(defclass tct-c () ())
(deftype tct-small () '(integer 0 9))
(deftype tct-seqnl () '(and sequence (not list)))
(deftype tct-sym-or-str () '(or symbol string))
(deftype tct-pair (&optional a) (declare (ignore a)) 'cons)

(defun %tct-objects ()
  (list 'nil 't ':kw 'sym 0 7 9 10 -1 most-positive-fixnum (1+ most-positive-fixnum) 3/4 1.5f0 2.5d0 #c(1 2)
        #\a "str" "" (make-string 2) #(1 2) (vector) #*101 (make-array 3 :element-type '(unsigned-byte 8))
        (make-array '(2 2)) '(1 2) '(1 . 2) (list nil) #'car (lambda (x) x) (make-hash-table)
        (make-tct-s :a 1) (make-instance 'tct-c) (make-condition 'simple-error) #p"a/b"
        (make-array 2 :adjustable t :fill-pointer 1)))

(defun %tct-run (fn) (map 'string (lambda (o) (if (funcall fn o) #\1 #\0)) (%tct-objects)))

(deftest typep-compound-types.1
  (%tct-run (lambda (x) (typep x '(AND SEQUENCE (NOT LIST)))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.1r
  (%tct-run (lambda (x) (typep x (identity '(AND SEQUENCE (NOT LIST))))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.2
  (%tct-run (lambda (x) (typep x '(AND LIST (NOT NULL)))))
  "00000000000000000000000011100000000")

(deftest typep-compound-types.2r
  (%tct-run (lambda (x) (typep x (identity '(AND LIST (NOT NULL))))))
  "00000000000000000000000011100000000")

(deftest typep-compound-types.3
  (%tct-run (lambda (x) (typep x '(OR NULL SYMBOL))))
  "11110000000000000000000000000000000")

(deftest typep-compound-types.3r
  (%tct-run (lambda (x) (typep x (identity '(OR NULL SYMBOL)))))
  "11110000000000000000000000000000000")

(deftest typep-compound-types.4
  (%tct-run (lambda (x) (typep x '(OR NULL (MEMBER :A :B 3)))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.4r
  (%tct-run (lambda (x) (typep x (identity '(OR NULL (MEMBER :A :B 3))))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.5
  (%tct-run (lambda (x) (typep x '(OR FIXNUM STRING))))
  "00001111110000001110000000000000000")

(deftest typep-compound-types.5r
  (%tct-run (lambda (x) (typep x (identity '(OR FIXNUM STRING)))))
  "00001111110000001110000000000000000")

(deftest typep-compound-types.6
  (%tct-run (lambda (x) (typep x '(NOT LIST))))
  "01111111111111111111111100011111111")

(deftest typep-compound-types.6r
  (%tct-run (lambda (x) (typep x (identity '(NOT LIST)))))
  "01111111111111111111111100011111111")

(deftest typep-compound-types.7
  (%tct-run (lambda (x) (typep x '(NOT NULL))))
  "01111111111111111111111111111111111")

(deftest typep-compound-types.7r
  (%tct-run (lambda (x) (typep x (identity '(NOT NULL)))))
  "01111111111111111111111111111111111")

(deftest typep-compound-types.8
  (%tct-run (lambda (x) (typep x '(NOT (OR CONS NUMBER)))))
  "11110000000000011111111100011111111")

(deftest typep-compound-types.8r
  (%tct-run (lambda (x) (typep x (identity '(NOT (OR CONS NUMBER))))))
  "11110000000000011111111100011111111")

(deftest typep-compound-types.9
  (%tct-run (lambda (x) (typep x '(AND (NOT NULL) (NOT CONS)))))
  "01111111111111111111111100011111111")

(deftest typep-compound-types.9r
  (%tct-run (lambda (x) (typep x (identity '(AND (NOT NULL) (NOT CONS))))))
  "01111111111111111111111100011111111")

(deftest typep-compound-types.10
  (%tct-run (lambda (x) (typep x '(OR))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.10r
  (%tct-run (lambda (x) (typep x (identity '(OR)))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.11
  (%tct-run (lambda (x) (typep x '(AND))))
  "11111111111111111111111111111111111")

(deftest typep-compound-types.11r
  (%tct-run (lambda (x) (typep x (identity '(AND)))))
  "11111111111111111111111111111111111")

(deftest typep-compound-types.12
  (%tct-run (lambda (x) (typep x '(MEMBER 1 2 3))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.12r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER 1 2 3)))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.13
  (%tct-run (lambda (x) (typep x '(MEMBER :KW SYM))))
  "00110000000000000000000000000000000")

(deftest typep-compound-types.13r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER :KW SYM)))))
  "00110000000000000000000000000000000")

(deftest typep-compound-types.14
  (%tct-run (lambda (x) (typep x '(MEMBER NIL))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.14r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER NIL)))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.15
  (%tct-run (lambda (x) (typep x '(MEMBER))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.15r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER)))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.16
  (%tct-run (lambda (x) (typep x '(EQL 7))))
  "00000100000000000000000000000000000")

(deftest typep-compound-types.16r
  (%tct-run (lambda (x) (typep x (identity '(EQL 7)))))
  "00000100000000000000000000000000000")

(deftest typep-compound-types.17
  (%tct-run (lambda (x) (typep x '(EQL :KW))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.17r
  (%tct-run (lambda (x) (typep x (identity '(EQL :KW)))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.18
  (%tct-run (lambda (x) (typep x '(EQL NIL))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.18r
  (%tct-run (lambda (x) (typep x (identity '(EQL NIL)))))
  "10000000000000000000000000000000000")

(deftest typep-compound-types.19
  (%tct-run (lambda (x) (typep x '(NOT (EQL 7)))))
  "11111011111111111111111111111111111")

(deftest typep-compound-types.19r
  (%tct-run (lambda (x) (typep x (identity '(NOT (EQL 7))))))
  "11111011111111111111111111111111111")

(deftest typep-compound-types.20
  (%tct-run (lambda (x) (typep x '(OR (EQL 1) (EQL 2.0d0)))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.20r
  (%tct-run (lambda (x) (typep x (identity '(OR (EQL 1) (EQL 2.0d0))))))
  "00000000000000000000000000000000000")

(deftest typep-compound-types.21
  (%tct-run (lambda (x) (typep x '(MEMBER 1.5))))
  "00000000000010000000000000000000000")

(deftest typep-compound-types.21r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER 1.5)))))
  "00000000000010000000000000000000000")

(deftest typep-compound-types.22
  (%tct-run (lambda (x) (typep x '(MEMBER #\a #\b))))
  "00000000000000010000000000000000000")

(deftest typep-compound-types.22r
  (%tct-run (lambda (x) (typep x (identity '(MEMBER #\a #\b)))))
  "00000000000000010000000000000000000")

(deftest typep-compound-types.23
  (%tct-run (lambda (x) (typep x '(INTEGER 0 9))))
  "00001110000000000000000000000000000")

(deftest typep-compound-types.23r
  (%tct-run (lambda (x) (typep x (identity '(INTEGER 0 9)))))
  "00001110000000000000000000000000000")

(deftest typep-compound-types.24
  (%tct-run (lambda (x) (typep x '(INTEGER 0))))
  "00001111011000000000000000000000000")

(deftest typep-compound-types.24r
  (%tct-run (lambda (x) (typep x (identity '(INTEGER 0)))))
  "00001111011000000000000000000000000")

(deftest typep-compound-types.25
  (%tct-run (lambda (x) (typep x '(INTEGER * 9))))
  "00001110100000000000000000000000000")

(deftest typep-compound-types.25r
  (%tct-run (lambda (x) (typep x (identity '(INTEGER * 9)))))
  "00001110100000000000000000000000000")

(deftest typep-compound-types.26
  (%tct-run (lambda (x) (typep x '(OR (INTEGER 0 9) NULL))))
  "10001110000000000000000000000000000")

(deftest typep-compound-types.26r
  (%tct-run (lambda (x) (typep x (identity '(OR (INTEGER 0 9) NULL)))))
  "10001110000000000000000000000000000")

(deftest typep-compound-types.27
  (%tct-run (lambda (x) (typep x '(AND INTEGER (NOT (INTEGER 0 9))))))
  "00000001111000000000000000000000000")

(deftest typep-compound-types.27r
  (%tct-run (lambda (x) (typep x (identity '(AND INTEGER (NOT (INTEGER 0 9)))))))
  "00000001111000000000000000000000000")

(deftest typep-compound-types.28
  (%tct-run (lambda (x) (typep x '(AND NUMBER (NOT INTEGER)))))
  "00000000000111100000000000000000000")

(deftest typep-compound-types.28r
  (%tct-run (lambda (x) (typep x (identity '(AND NUMBER (NOT INTEGER))))))
  "00000000000111100000000000000000000")

(deftest typep-compound-types.29
  (%tct-run (lambda (x) (typep x '(OR INTEGER RATIO))))
  "00001111111100000000000000000000000")

(deftest typep-compound-types.29r
  (%tct-run (lambda (x) (typep x (identity '(OR INTEGER RATIO)))))
  "00001111111100000000000000000000000")

(deftest typep-compound-types.30
  (%tct-run (lambda (x) (typep x '(AND FIXNUM (NOT (INTEGER 0 9))))))
  "00000001110000000000000000000000000")

(deftest typep-compound-types.30r
  (%tct-run (lambda (x) (typep x (identity '(AND FIXNUM (NOT (INTEGER 0 9)))))))
  "00000001110000000000000000000000000")

(deftest typep-compound-types.31
  (%tct-run (lambda (x) (typep x '(AND SEQUENCE (NOT LIST) (NOT STRING)))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.31r
  (%tct-run (lambda (x) (typep x (identity '(AND SEQUENCE (NOT LIST)
                                                 (NOT STRING))))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.32
  (%tct-run (lambda (x) (typep x '(OR VECTOR LIST))))
  "10000000000000001111111011100000001")

(deftest typep-compound-types.32r
  (%tct-run (lambda (x) (typep x (identity '(OR VECTOR LIST)))))
  "10000000000000001111111011100000001")

(deftest typep-compound-types.33
  (%tct-run (lambda (x) (typep x '(AND VECTOR (NOT STRING)))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.33r
  (%tct-run (lambda (x) (typep x (identity '(AND VECTOR (NOT STRING))))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.34
  (%tct-run (lambda (x) (typep x '(OR STRING SYMBOL))))
  "11110000000000001110000000000000000")

(deftest typep-compound-types.34r
  (%tct-run (lambda (x) (typep x (identity '(OR STRING SYMBOL)))))
  "11110000000000001110000000000000000")

(deftest typep-compound-types.35
  (%tct-run (lambda (x) (typep x '(AND (OR LIST VECTOR) (NOT NULL)))))
  "00000000000000001111111011100000001")

(deftest typep-compound-types.35r
  (%tct-run (lambda (x) (typep x (identity '(AND (OR LIST VECTOR) (NOT NULL))))))
  "00000000000000001111111011100000001")

(deftest typep-compound-types.36
  (%tct-run (lambda (x) (typep x '(OR (AND LIST (NOT NULL)) STRING))))
  "00000000000000001110000011100000000")

(deftest typep-compound-types.36r
  (%tct-run (lambda (x) (typep x (identity '(OR (AND LIST (NOT NULL)) STRING)))))
  "00000000000000001110000011100000000")

(deftest typep-compound-types.37
  (%tct-run (lambda (x) (typep x 'TCT-SMALL)))
  "00001110000000000000000000000000000")

(deftest typep-compound-types.37r
  (%tct-run (lambda (x) (typep x (identity 'TCT-SMALL))))
  "00001110000000000000000000000000000")

(deftest typep-compound-types.38
  (%tct-run (lambda (x) (typep x 'TCT-SEQNL)))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.38r
  (%tct-run (lambda (x) (typep x (identity 'TCT-SEQNL))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.39
  (%tct-run (lambda (x) (typep x 'TCT-SYM-OR-STR)))
  "11110000000000001110000000000000000")

(deftest typep-compound-types.39r
  (%tct-run (lambda (x) (typep x (identity 'TCT-SYM-OR-STR))))
  "11110000000000001110000000000000000")

(deftest typep-compound-types.40
  (%tct-run (lambda (x) (typep x '(OR TCT-SMALL NULL))))
  "10001110000000000000000000000000000")

(deftest typep-compound-types.40r
  (%tct-run (lambda (x) (typep x (identity '(OR TCT-SMALL NULL)))))
  "10001110000000000000000000000000000")

(deftest typep-compound-types.41
  (%tct-run (lambda (x) (typep x '(NOT TCT-SMALL))))
  "11110001111111111111111111111111111")

(deftest typep-compound-types.41r
  (%tct-run (lambda (x) (typep x (identity '(NOT TCT-SMALL)))))
  "11110001111111111111111111111111111")

(deftest typep-compound-types.42
  (%tct-run (lambda (x) (typep x '(AND TCT-SEQNL (NOT STRING)))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.42r
  (%tct-run (lambda (x) (typep x (identity '(AND TCT-SEQNL (NOT STRING))))))
  "00000000000000000001111000000000001")

(deftest typep-compound-types.43
  (%tct-run (lambda (x) (typep x '(OR TCT-PAIR NULL))))
  "10000000000000000000000011100000000")

(deftest typep-compound-types.43r
  (%tct-run (lambda (x) (typep x (identity '(OR TCT-PAIR NULL)))))
  "10000000000000000000000011100000000")

(deftest typep-compound-types.44
  (%tct-run (lambda (x) (typep x '(AND SEQUENCE TCT-SEQNL))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.44r
  (%tct-run (lambda (x) (typep x (identity '(AND SEQUENCE TCT-SEQNL)))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.45
  (%tct-run (lambda (x) (typep x '(OR TCT-S NULL))))
  "10000000000000000000000000000010000")

(deftest typep-compound-types.45r
  (%tct-run (lambda (x) (typep x (identity '(OR TCT-S NULL)))))
  "10000000000000000000000000000010000")

(deftest typep-compound-types.46
  (%tct-run (lambda (x) (typep x '(AND (NOT TCT-S) ATOM))))
  "11111111111111111111111100011101111")

(deftest typep-compound-types.46r
  (%tct-run (lambda (x) (typep x (identity '(AND (NOT TCT-S) ATOM)))))
  "11111111111111111111111100011101111")

(deftest typep-compound-types.47
  (%tct-run (lambda (x) (typep x '(OR TCT-C FUNCTION))))
  "00000000000000000000000000011001000")

(deftest typep-compound-types.47r
  (%tct-run (lambda (x) (typep x (identity '(OR TCT-C FUNCTION)))))
  "00000000000000000000000000011001000")

(deftest typep-compound-types.48
  (%tct-run (lambda (x) (typep x '(AND ATOM (NOT SYMBOL)))))
  "00001111111111111111111100011111111")

(deftest typep-compound-types.48r
  (%tct-run (lambda (x) (typep x (identity '(AND ATOM (NOT SYMBOL))))))
  "00001111111111111111111100011111111")

(deftest typep-compound-types.49
  (%tct-run (lambda (x) (typep x '(AND KEYWORD (NOT NULL)))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.49r
  (%tct-run (lambda (x) (typep x (identity '(AND KEYWORD (NOT NULL))))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.50
  (%tct-run (lambda (x) (typep x '(OR KEYWORD NUMBER))))
  "00101111111111100000000000000000000")

(deftest typep-compound-types.50r
  (%tct-run (lambda (x) (typep x (identity '(OR KEYWORD NUMBER)))))
  "00101111111111100000000000000000000")

(deftest typep-compound-types.51
  (%tct-run (lambda (x) (typep x '(AND SYMBOL (NOT KEYWORD)))))
  "11010000000000000000000000000000000")

(deftest typep-compound-types.51r
  (%tct-run (lambda (x) (typep x (identity '(AND SYMBOL (NOT KEYWORD))))))
  "11010000000000000000000000000000000")

(deftest typep-compound-types.52
  (%tct-run (lambda (x) (typep x '(OR CHARACTER STRING))))
  "00000000000000011110000000000000000")

(deftest typep-compound-types.52r
  (%tct-run (lambda (x) (typep x (identity '(OR CHARACTER STRING)))))
  "00000000000000011110000000000000000")

(deftest typep-compound-types.53
  (%tct-run (lambda (x) (typep x '(AND ARRAY (NOT VECTOR)))))
  "00000000000000000000000100000000000")

(deftest typep-compound-types.53r
  (%tct-run (lambda (x) (typep x (identity '(AND ARRAY (NOT VECTOR))))))
  "00000000000000000000000100000000000")

(deftest typep-compound-types.54
  (%tct-run (lambda (x) (typep x '(OR BIT-VECTOR HASH-TABLE))))
  "00000000000000000000010000000100000")

(deftest typep-compound-types.54r
  (%tct-run (lambda (x) (typep x (identity '(OR BIT-VECTOR HASH-TABLE)))))
  "00000000000000000000010000000100000")

(deftest typep-compound-types.55
  (%tct-run (lambda (x) (typep x '(AND VECTOR (NOT SIMPLE-VECTOR)))))
  "00000000000000001110011000000000001")

(deftest typep-compound-types.55r
  (%tct-run (lambda (x) (typep x (identity '(AND VECTOR (NOT SIMPLE-VECTOR))))))
  "00000000000000001110011000000000001")

(deftest typep-compound-types.56
  (%tct-run (lambda (x) (typep x '(OR SIMPLE-STRING PATHNAME))))
  "00000000000000001110000000000000010")

(deftest typep-compound-types.56r
  (%tct-run (lambda (x) (typep x (identity '(OR SIMPLE-STRING PATHNAME)))))
  "00000000000000001110000000000000010")

(deftest typep-compound-types.57
  (%tct-run (lambda (x) (typep x '(SATISFIES ATOM))))
  "11111111111111111111111100011111111")

(deftest typep-compound-types.57r
  (%tct-run (lambda (x) (typep x (identity '(SATISFIES ATOM)))))
  "11111111111111111111111100011111111")

(deftest typep-compound-types.58
  (%tct-run (lambda (x) (typep x '(OR (SATISFIES NULL) STRING))))
  "10000000000000001110000000000000000")

(deftest typep-compound-types.58r
  (%tct-run (lambda (x) (typep x (identity '(OR (SATISFIES NULL) STRING)))))
  "10000000000000001110000000000000000")

(deftest typep-compound-types.59
  (%tct-run (lambda (x) (typep x '(AND (SATISFIES CONSP) LIST))))
  "00000000000000000000000011100000000")

(deftest typep-compound-types.59r
  (%tct-run (lambda (x) (typep x (identity '(AND (SATISFIES CONSP) LIST)))))
  "00000000000000000000000011100000000")

(deftest typep-compound-types.60
  (%tct-run (lambda (x) (typep x '(NOT (SATISFIES SYMBOLP)))))
  "00001111111111111111111111111111111")

(deftest typep-compound-types.60r
  (%tct-run (lambda (x) (typep x (identity '(NOT (SATISFIES SYMBOLP))))))
  "00001111111111111111111111111111111")

(deftest typep-compound-types.61
  (%tct-run (lambda (x) (typep x '(OR SINGLE-FLOAT DOUBLE-FLOAT))))
  "00000000000011000000000000000000000")

(deftest typep-compound-types.61r
  (%tct-run (lambda (x) (typep x (identity '(OR SINGLE-FLOAT DOUBLE-FLOAT)))))
  "00000000000011000000000000000000000")

(deftest typep-compound-types.62
  (%tct-run (lambda (x) (typep x '(AND FLOAT (NOT SINGLE-FLOAT)))))
  "00000000000001000000000000000000000")

(deftest typep-compound-types.62r
  (%tct-run (lambda (x) (typep x (identity '(AND FLOAT (NOT SINGLE-FLOAT))))))
  "00000000000001000000000000000000000")

(deftest typep-compound-types.63
  (%tct-run (lambda (x) (typep x '(OR RATIONAL FLOAT))))
  "00001111111111000000000000000000000")

(deftest typep-compound-types.63r
  (%tct-run (lambda (x) (typep x (identity '(OR RATIONAL FLOAT)))))
  "00001111111111000000000000000000000")

(deftest typep-compound-types.64
  (%tct-run (lambda (x) (typep x '(AND REAL (NOT RATIONAL)))))
  "00000000000011000000000000000000000")

(deftest typep-compound-types.64r
  (%tct-run (lambda (x) (typep x (identity '(AND REAL (NOT RATIONAL))))))
  "00000000000011000000000000000000000")

(deftest typep-compound-types.65
  (%tct-run (lambda (x) (typep x '(OR COMPLEX CHARACTER))))
  "00000000000000110000000000000000000")

(deftest typep-compound-types.65r
  (%tct-run (lambda (x) (typep x (identity '(OR COMPLEX CHARACTER)))))
  "00000000000000110000000000000000000")

(deftest typep-compound-types.66
  (%tct-run (lambda (x) (typep x '(AND ATOM (NOT NUMBER)))))
  "11110000000000011111111100011111111")

(deftest typep-compound-types.66r
  (%tct-run (lambda (x) (typep x (identity '(AND ATOM (NOT NUMBER))))))
  "11110000000000011111111100011111111")

(deftest typep-compound-types.67
  (%tct-run (lambda (x) (typep x '(OR FUNCTION HASH-TABLE))))
  "00000000000000000000000000011100000")

(deftest typep-compound-types.67r
  (%tct-run (lambda (x) (typep x (identity '(OR FUNCTION HASH-TABLE)))))
  "00000000000000000000000000011100000")

(deftest typep-compound-types.68
  (%tct-run (lambda (x) (typep x 'KEYWORD)))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.68r
  (%tct-run (lambda (x) (typep x (identity 'KEYWORD))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.69
  (%tct-run (lambda (x) (typep x '(NOT KEYWORD))))
  "11011111111111111111111111111111111")

(deftest typep-compound-types.69r
  (%tct-run (lambda (x) (typep x (identity '(NOT KEYWORD)))))
  "11011111111111111111111111111111111")

(deftest typep-compound-types.70
  (%tct-run (lambda (x) (typep x '(OR KEYWORD NULL))))
  "10100000000000000000000000000000000")

(deftest typep-compound-types.70r
  (%tct-run (lambda (x) (typep x (identity '(OR KEYWORD NULL)))))
  "10100000000000000000000000000000000")

(deftest typep-compound-types.71
  (%tct-run (lambda (x) (typep x '(AND SYMBOL KEYWORD))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.71r
  (%tct-run (lambda (x) (typep x (identity '(AND SYMBOL KEYWORD)))))
  "00100000000000000000000000000000000")

(deftest typep-compound-types.72
  (%tct-run (lambda (x) (typep x '(VECTOR * 2))))
  "00000000000000000011000000000000001")

(deftest typep-compound-types.72r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR * 2)))))
  "00000000000000000011000000000000001")

(deftest typep-compound-types.73
  (%tct-run (lambda (x) (typep x '(VECTOR * 0))))
  "00000000000000000100100000000000000")

(deftest typep-compound-types.73r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR * 0)))))
  "00000000000000000100100000000000000")

(deftest typep-compound-types.74
  (%tct-run (lambda (x) (typep x '(VECTOR T 2))))
  "00000000000000000001000000000000001")

(deftest typep-compound-types.74r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR T 2)))))
  "00000000000000000001000000000000001")

(deftest typep-compound-types.75
  (%tct-run (lambda (x) (typep x '(VECTOR * *))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.75r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR * *)))))
  "00000000000000001111111000000000001")

(deftest typep-compound-types.76
  (%tct-run (lambda (x) (typep x '(VECTOR * 3))))
  "00000000000000001000011000000000000")

(deftest typep-compound-types.76r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR * 3)))))
  "00000000000000001000011000000000000")

(deftest typep-compound-types.77
  (%tct-run (lambda (x) (typep x '(AND VECTOR (NOT (VECTOR * 0))))))
  "00000000000000001011011000000000001")

(deftest typep-compound-types.77r
  (%tct-run (lambda (x) (typep x (identity '(AND VECTOR (NOT (VECTOR * 0)))))))
  "00000000000000001011011000000000001")

(deftest typep-compound-types.78
  (%tct-run (lambda (x) (typep x '(OR (VECTOR * 2) (VECTOR * 3)))))
  "00000000000000001011011000000000001")

(deftest typep-compound-types.78r
  (%tct-run (lambda (x) (typep x (identity '(OR (VECTOR * 2) (VECTOR * 3))))))
  "00000000000000001011011000000000001")

(deftest typep-compound-types.79
  (%tct-run (lambda (x) (typep x '(SIMPLE-ARRAY * (2)))))
  "00000000000000000011000000000000000")

(deftest typep-compound-types.79r
  (%tct-run (lambda (x) (typep x (identity '(SIMPLE-ARRAY * (2))))))
  "00000000000000000011000000000000000")

(deftest typep-compound-types.80
  (%tct-run (lambda (x) (typep x '(ARRAY * (2 2)))))
  "00000000000000000000000100000000000")

(deftest typep-compound-types.80r
  (%tct-run (lambda (x) (typep x (identity '(ARRAY * (2 2))))))
  "00000000000000000000000100000000000")

(deftest typep-compound-types.81
  (%tct-run (lambda (x) (typep x '(VECTOR CHARACTER))))
  "00000000000000001110000000000000000")

(deftest typep-compound-types.81r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR CHARACTER)))))
  "00000000000000001110000000000000000")

(deftest typep-compound-types.82
  (%tct-run (lambda (x) (typep x '(VECTOR BIT))))
  "00000000000000000000010000000000000")

(deftest typep-compound-types.82r
  (%tct-run (lambda (x) (typep x (identity '(VECTOR BIT)))))
  "00000000000000000000010000000000000")

