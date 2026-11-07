;;; ALPHA-CHAR-P, ALPHANUMERICP, UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P, GRAPHIC-CHAR-P,
;;; CHAR-UPCASE and CHAR-DOWNCASE with one argument are direct calls. Results for ASCII, Latin-1,
;;; Greek and others, the errors for a non-character and a wrong argument count, and a local
;;; function of the same name are as before; the expected values are those of the build before.

(deftest char-fns-direct.1
  (handler-case (multiple-value-list (MAPCAR (FUNCTION ALPHA-CHAR-P) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T NIL NIL NIL NIL NIL T T T T T NIL NIL NIL T)))

(deftest char-fns-direct.2
  (handler-case (multiple-value-list (LIST (ALPHA-CHAR-P #\a) (ALPHA-CHAR-P #\Z) (ALPHA-CHAR-P #\5) (ALPHA-CHAR-P #\ ) (ALPHA-CHAR-P #\Newline) (ALPHA-CHAR-P #\Tab) (ALPHA-CHAR-P #\~) (ALPHA-CHAR-P (CODE-CHAR 233)) (ALPHA-CHAR-P (CODE-CHAR 201)) (ALPHA-CHAR-P (CODE-CHAR 955)) (ALPHA-CHAR-P (CODE-CHAR 923)) (ALPHA-CHAR-P (CODE-CHAR 223)) (ALPHA-CHAR-P (CODE-CHAR 8364)) (ALPHA-CHAR-P (CODE-CHAR 0)) (ALPHA-CHAR-P (CODE-CHAR 127)) (ALPHA-CHAR-P (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T NIL NIL NIL NIL NIL T T T T T NIL NIL NIL T)))

(deftest char-fns-direct.3
  (handler-case (multiple-value-list (HANDLER-CASE (ALPHA-CHAR-P 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.4
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE ALPHA-CHAR-P) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.5
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE ALPHA-CHAR-P)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.6
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (ALPHA-CHAR-P #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.7
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE ALPHA-CHAR-P)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (T))

(deftest char-fns-direct.8
  (handler-case (multiple-value-list (MAPCAR (FUNCTION ALPHANUMERICP) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T T NIL NIL NIL NIL T T T T T NIL NIL NIL T)))

(deftest char-fns-direct.9
  (handler-case (multiple-value-list (LIST (ALPHANUMERICP #\a) (ALPHANUMERICP #\Z) (ALPHANUMERICP #\5) (ALPHANUMERICP #\ ) (ALPHANUMERICP #\Newline) (ALPHANUMERICP #\Tab) (ALPHANUMERICP #\~) (ALPHANUMERICP (CODE-CHAR 233)) (ALPHANUMERICP (CODE-CHAR 201)) (ALPHANUMERICP (CODE-CHAR 955)) (ALPHANUMERICP (CODE-CHAR 923)) (ALPHANUMERICP (CODE-CHAR 223)) (ALPHANUMERICP (CODE-CHAR 8364)) (ALPHANUMERICP (CODE-CHAR 0)) (ALPHANUMERICP (CODE-CHAR 127)) (ALPHANUMERICP (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T T NIL NIL NIL NIL T T T T T NIL NIL NIL T)))

(deftest char-fns-direct.10
  (handler-case (multiple-value-list (HANDLER-CASE (ALPHANUMERICP 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.11
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE ALPHANUMERICP) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.12
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE ALPHANUMERICP)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.13
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (ALPHANUMERICP #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.14
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE ALPHANUMERICP)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (T))

(deftest char-fns-direct.15
  (handler-case (multiple-value-list (MAPCAR (FUNCTION UPPER-CASE-P) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((NIL T NIL NIL NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.16
  (handler-case (multiple-value-list (LIST (UPPER-CASE-P #\a) (UPPER-CASE-P #\Z) (UPPER-CASE-P #\5) (UPPER-CASE-P #\ ) (UPPER-CASE-P #\Newline) (UPPER-CASE-P #\Tab) (UPPER-CASE-P #\~) (UPPER-CASE-P (CODE-CHAR 233)) (UPPER-CASE-P (CODE-CHAR 201)) (UPPER-CASE-P (CODE-CHAR 955)) (UPPER-CASE-P (CODE-CHAR 923)) (UPPER-CASE-P (CODE-CHAR 223)) (UPPER-CASE-P (CODE-CHAR 8364)) (UPPER-CASE-P (CODE-CHAR 0)) (UPPER-CASE-P (CODE-CHAR 127)) (UPPER-CASE-P (CODE-CHAR 12354)))) (error () (list :error)))
  ((NIL T NIL NIL NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.17
  (handler-case (multiple-value-list (HANDLER-CASE (UPPER-CASE-P 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.18
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE UPPER-CASE-P) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.19
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE UPPER-CASE-P)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.20
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (UPPER-CASE-P #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.21
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE UPPER-CASE-P)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (NIL))

(deftest char-fns-direct.22
  (handler-case (multiple-value-list (MAPCAR (FUNCTION LOWER-CASE-P) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((T NIL NIL NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.23
  (handler-case (multiple-value-list (LIST (LOWER-CASE-P #\a) (LOWER-CASE-P #\Z) (LOWER-CASE-P #\5) (LOWER-CASE-P #\ ) (LOWER-CASE-P #\Newline) (LOWER-CASE-P #\Tab) (LOWER-CASE-P #\~) (LOWER-CASE-P (CODE-CHAR 233)) (LOWER-CASE-P (CODE-CHAR 201)) (LOWER-CASE-P (CODE-CHAR 955)) (LOWER-CASE-P (CODE-CHAR 923)) (LOWER-CASE-P (CODE-CHAR 223)) (LOWER-CASE-P (CODE-CHAR 8364)) (LOWER-CASE-P (CODE-CHAR 0)) (LOWER-CASE-P (CODE-CHAR 127)) (LOWER-CASE-P (CODE-CHAR 12354)))) (error () (list :error)))
  ((T NIL NIL NIL NIL NIL NIL T NIL T NIL NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.24
  (handler-case (multiple-value-list (HANDLER-CASE (LOWER-CASE-P 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.25
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE LOWER-CASE-P) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.26
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE LOWER-CASE-P)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.27
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (LOWER-CASE-P #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.28
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE LOWER-CASE-P)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (T))

(deftest char-fns-direct.29
  (handler-case (multiple-value-list (MAPCAR (FUNCTION BOTH-CASE-P) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T NIL NIL NIL NIL NIL T T T T NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.30
  (handler-case (multiple-value-list (LIST (BOTH-CASE-P #\a) (BOTH-CASE-P #\Z) (BOTH-CASE-P #\5) (BOTH-CASE-P #\ ) (BOTH-CASE-P #\Newline) (BOTH-CASE-P #\Tab) (BOTH-CASE-P #\~) (BOTH-CASE-P (CODE-CHAR 233)) (BOTH-CASE-P (CODE-CHAR 201)) (BOTH-CASE-P (CODE-CHAR 955)) (BOTH-CASE-P (CODE-CHAR 923)) (BOTH-CASE-P (CODE-CHAR 223)) (BOTH-CASE-P (CODE-CHAR 8364)) (BOTH-CASE-P (CODE-CHAR 0)) (BOTH-CASE-P (CODE-CHAR 127)) (BOTH-CASE-P (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T NIL NIL NIL NIL NIL T T T T NIL NIL NIL NIL NIL)))

(deftest char-fns-direct.31
  (handler-case (multiple-value-list (HANDLER-CASE (BOTH-CASE-P 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.32
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE BOTH-CASE-P) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.33
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE BOTH-CASE-P)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.34
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (BOTH-CASE-P #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.35
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE BOTH-CASE-P)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (T))

(deftest char-fns-direct.36
  (handler-case (multiple-value-list (MAPCAR (FUNCTION GRAPHIC-CHAR-P) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T T T NIL NIL T T T T T T T NIL NIL T)))

(deftest char-fns-direct.37
  (handler-case (multiple-value-list (LIST (GRAPHIC-CHAR-P #\a) (GRAPHIC-CHAR-P #\Z) (GRAPHIC-CHAR-P #\5) (GRAPHIC-CHAR-P #\ ) (GRAPHIC-CHAR-P #\Newline) (GRAPHIC-CHAR-P #\Tab) (GRAPHIC-CHAR-P #\~) (GRAPHIC-CHAR-P (CODE-CHAR 233)) (GRAPHIC-CHAR-P (CODE-CHAR 201)) (GRAPHIC-CHAR-P (CODE-CHAR 955)) (GRAPHIC-CHAR-P (CODE-CHAR 923)) (GRAPHIC-CHAR-P (CODE-CHAR 223)) (GRAPHIC-CHAR-P (CODE-CHAR 8364)) (GRAPHIC-CHAR-P (CODE-CHAR 0)) (GRAPHIC-CHAR-P (CODE-CHAR 127)) (GRAPHIC-CHAR-P (CODE-CHAR 12354)))) (error () (list :error)))
  ((T T T T NIL NIL T T T T T T T NIL NIL T)))

(deftest char-fns-direct.38
  (handler-case (multiple-value-list (HANDLER-CASE (GRAPHIC-CHAR-P 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.39
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE GRAPHIC-CHAR-P) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.40
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE GRAPHIC-CHAR-P)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.41
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (GRAPHIC-CHAR-P #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.42
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE GRAPHIC-CHAR-P)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (T))

(deftest char-fns-direct.43
  (handler-case (multiple-value-list (MAPCAR (FUNCTION CHAR-UPCASE) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((#\A #\Z #\5 #\  #\Newline #\Tab #\~ #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE #\GREEK_CAPITAL_LETTER_LAMDA #\GREEK_CAPITAL_LETTER_LAMDA #\LATIN_SMALL_LETTER_SHARP_S #\EURO_SIGN #\Null #\Rubout #\HIRAGANA_LETTER_A)))

(deftest char-fns-direct.44
  (handler-case (multiple-value-list (LIST (CHAR-UPCASE #\a) (CHAR-UPCASE #\Z) (CHAR-UPCASE #\5) (CHAR-UPCASE #\ ) (CHAR-UPCASE #\Newline) (CHAR-UPCASE #\Tab) (CHAR-UPCASE #\~) (CHAR-UPCASE (CODE-CHAR 233)) (CHAR-UPCASE (CODE-CHAR 201)) (CHAR-UPCASE (CODE-CHAR 955)) (CHAR-UPCASE (CODE-CHAR 923)) (CHAR-UPCASE (CODE-CHAR 223)) (CHAR-UPCASE (CODE-CHAR 8364)) (CHAR-UPCASE (CODE-CHAR 0)) (CHAR-UPCASE (CODE-CHAR 127)) (CHAR-UPCASE (CODE-CHAR 12354)))) (error () (list :error)))
  ((#\A #\Z #\5 #\  #\Newline #\Tab #\~ #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE #\GREEK_CAPITAL_LETTER_LAMDA #\GREEK_CAPITAL_LETTER_LAMDA #\LATIN_SMALL_LETTER_SHARP_S #\EURO_SIGN #\Null #\Rubout #\HIRAGANA_LETTER_A)))

(deftest char-fns-direct.45
  (handler-case (multiple-value-list (HANDLER-CASE (CHAR-UPCASE 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.46
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE CHAR-UPCASE) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.47
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE CHAR-UPCASE)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.48
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (CHAR-UPCASE #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.49
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE CHAR-UPCASE)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (#\A))

(deftest char-fns-direct.50
  (handler-case (multiple-value-list (MAPCAR (FUNCTION CHAR-DOWNCASE) (LIST #\a #\Z #\5 #\  #\Newline #\Tab #\~ (CODE-CHAR 233) (CODE-CHAR 201) (CODE-CHAR 955) (CODE-CHAR 923) (CODE-CHAR 223) (CODE-CHAR 8364) (CODE-CHAR 0) (CODE-CHAR 127) (CODE-CHAR 12354)))) (error () (list :error)))
  ((#\a #\z #\5 #\  #\Newline #\Tab #\~ #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\Greek_small_letter_lamda #\Greek_small_letter_lamda #\LATIN_SMALL_LETTER_SHARP_S #\EURO_SIGN #\Null #\Rubout #\HIRAGANA_LETTER_A)))

(deftest char-fns-direct.51
  (handler-case (multiple-value-list (LIST (CHAR-DOWNCASE #\a) (CHAR-DOWNCASE #\Z) (CHAR-DOWNCASE #\5) (CHAR-DOWNCASE #\ ) (CHAR-DOWNCASE #\Newline) (CHAR-DOWNCASE #\Tab) (CHAR-DOWNCASE #\~) (CHAR-DOWNCASE (CODE-CHAR 233)) (CHAR-DOWNCASE (CODE-CHAR 201)) (CHAR-DOWNCASE (CODE-CHAR 955)) (CHAR-DOWNCASE (CODE-CHAR 923)) (CHAR-DOWNCASE (CODE-CHAR 223)) (CHAR-DOWNCASE (CODE-CHAR 8364)) (CHAR-DOWNCASE (CODE-CHAR 0)) (CHAR-DOWNCASE (CODE-CHAR 127)) (CHAR-DOWNCASE (CODE-CHAR 12354)))) (error () (list :error)))
  ((#\a #\z #\5 #\  #\Newline #\Tab #\~ #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\Greek_small_letter_lamda #\Greek_small_letter_lamda #\LATIN_SMALL_LETTER_SHARP_S #\EURO_SIGN #\Null #\Rubout #\HIRAGANA_LETTER_A)))

(deftest char-fns-direct.52
  (handler-case (multiple-value-list (HANDLER-CASE (CHAR-DOWNCASE 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.53
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE CHAR-DOWNCASE) 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.54
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (QUOTE CHAR-DOWNCASE)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.55
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (CHAR-DOWNCASE #\a #\b))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest char-fns-direct.56
  (handler-case (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE CHAR-DOWNCASE)) #\a) (ERROR NIL :ERROR))) (error () (list :error)))
  (#\a))

(deftest char-fns-direct.57
  (handler-case (multiple-value-list (FLET ((ALPHA-CHAR-P (C) (LIST :LOCAL C))) (ALPHA-CHAR-P #\a))) (error () (list :error)))
  ((:LOCAL #\a)))

(deftest char-fns-direct.58
  ;; The direct call and the call through the function cell agree for every code
  ;; (the answers themselves depend on the Unicode data of the platform).
  (flet ((same (c)
           (and (eq (alpha-char-p c) (funcall 'alpha-char-p c))
                (eq (alphanumericp c) (funcall 'alphanumericp c))
                (eq (upper-case-p c) (funcall 'upper-case-p c))
                (eq (lower-case-p c) (funcall 'lower-case-p c))
                (eq (both-case-p c) (funcall 'both-case-p c))
                (eq (graphic-char-p c) (funcall 'graphic-char-p c))
                (char= (char-upcase c) (funcall 'char-upcase c))
                (char= (char-downcase c) (funcall 'char-downcase c)))))
    (list (loop for i below 300 always (same (code-char i)))
          (loop for i below 65536 always (same (code-char i)))))
  (t t))
