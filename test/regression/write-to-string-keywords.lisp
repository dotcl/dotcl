;;; WRITE-TO-STRING with printer keywords binds the printer variables without building a List and a
;;; HashSet per call. The first occurrence of a repeated keyword, :allow-other-keys, the variables not
;;; named staying as they were, and the errors are as before; the expected values are those of the build
;;; before the change, which agree with SBCL apart from error class names.

(deftest write-to-string-keywords.1
  (handler-case (multiple-value-list (WRITE-TO-STRING 255 :BASE 16)) (error () (list :error)))
  ("FF"))

(deftest write-to-string-keywords.2
  (handler-case (multiple-value-list (WRITE-TO-STRING 255 :BASE 2 :RADIX T)) (error () (list :error)))
  ("#b11111111"))

(deftest write-to-string-keywords.3
  (handler-case (multiple-value-list (WRITE-TO-STRING 255 :BASE 8 :RADIX T)) (error () (list :error)))
  ("#o377"))

(deftest write-to-string-keywords.4
  (handler-case (multiple-value-list (WRITE-TO-STRING 255 :RADIX T)) (error () (list :error)))
  ("255."))

(deftest write-to-string-keywords.5
  (handler-case (multiple-value-list (WRITE-TO-STRING -255 :BASE 16)) (error () (list :error)))
  ("-FF"))

(deftest write-to-string-keywords.6
  (handler-case (multiple-value-list (WRITE-TO-STRING 12345678901234567890123 :BASE 36)) (error () (list :error)))
  ("20DGOHX2W7BEK7F"))

(deftest write-to-string-keywords.7
  (handler-case (multiple-value-list (WRITE-TO-STRING 1/3 :BASE 3 :RADIX T)) (error () (list :error)))
  ("#3r1/10"))

(deftest write-to-string-keywords.8
  (handler-case (multiple-value-list (WRITE-TO-STRING "ab" :ESCAPE NIL)) (error () (list :error)))
  ("ab"))

(deftest write-to-string-keywords.9
  (handler-case (multiple-value-list (WRITE-TO-STRING "ab" :ESCAPE T)) (error () (list :error)))
  ("\"ab\""))

(deftest write-to-string-keywords.10
  (handler-case (multiple-value-list (WRITE-TO-STRING "ab")) (error () (list :error)))
  ("\"ab\""))

(deftest write-to-string-keywords.11
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE ABC) :CASE :DOWNCASE)) (error () (list :error)))
  ("abc"))

(deftest write-to-string-keywords.12
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE ABC) :CASE :CAPITALIZE)) (error () (list :error)))
  ("Abc"))

(deftest write-to-string-keywords.13
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE ABC) :ESCAPE T :CASE :DOWNCASE)) (error () (list :error)))
  ("abc"))

(deftest write-to-string-keywords.14
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE (1 2 3)) :PRETTY NIL)) (error () (list :error)))
  ("(1 2 3)"))

(deftest write-to-string-keywords.15
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE (1 2 3 4 5)) :LENGTH 3)) (error () (list :error)))
  ("(1 2 3 ...)"))

(deftest write-to-string-keywords.16
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE (1 (2 (3 (4))))) :LEVEL 2)) (error () (list :error)))
  ("(1 (2 #))"))

(deftest write-to-string-keywords.17
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE (1 2 3)) :BASE 2)) (error () (list :error)))
  ("(1 10 11)"))

(deftest write-to-string-keywords.19
  (handler-case (multiple-value-list (WRITE-TO-STRING #(1 2 3) :ARRAY T)) (error () (list :error)))
  ("#(1 2 3)"))

(deftest write-to-string-keywords.20
  (handler-case (multiple-value-list (WRITE-TO-STRING 3.5d0 :BASE 2)) (error () (list :error)))
  ("3.5d0"))

(deftest write-to-string-keywords.21
  (handler-case (multiple-value-list (WRITE-TO-STRING :A :GENSYM NIL)) (error () (list :error)))
  (":A"))

(deftest write-to-string-keywords.22
  (handler-case (multiple-value-list (WRITE-TO-STRING (MAKE-SYMBOL "G") :GENSYM NIL)) (error () (list :error)))
  ("G"))

(deftest write-to-string-keywords.23
  (handler-case (multiple-value-list (WRITE-TO-STRING (MAKE-SYMBOL "G") :GENSYM T)) (error () (list :error)))
  ("#:G"))

(deftest write-to-string-keywords.24
  (handler-case (multiple-value-list (WRITE-TO-STRING 10 :BASE 16 :BASE 2)) (error () (list :error)))
  ("A"))

(deftest write-to-string-keywords.25
  (handler-case (multiple-value-list (WRITE-TO-STRING 10 :RADIX T :RADIX NIL)) (error () (list :error)))
  ("10."))

(deftest write-to-string-keywords.26
  (handler-case (multiple-value-list (WRITE-TO-STRING 10 :ALLOW-OTHER-KEYS T :BOGUS 1 :BASE 2)) (error () (list :error)))
  ("1010"))

(deftest write-to-string-keywords.27
  (handler-case (multiple-value-list (WRITE-TO-STRING 10 :BOGUS 1 :ALLOW-OTHER-KEYS T)) (error () (list :error)))
  ("10"))

(deftest write-to-string-keywords.28
  (handler-case (multiple-value-list (HANDLER-CASE (WRITE-TO-STRING 10 :BOGUS 1) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest write-to-string-keywords.29
  (handler-case (multiple-value-list (HANDLER-CASE (WRITE-TO-STRING 10 :BASE) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest write-to-string-keywords.30
  (handler-case (multiple-value-list (HANDLER-CASE (WRITE-TO-STRING 10 5 6) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest write-to-string-keywords.31
  (handler-case (multiple-value-list (HANDLER-CASE (WRITE-TO-STRING) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest write-to-string-keywords.32
  (handler-case (multiple-value-list (HANDLER-CASE (WRITE-TO-STRING 10 :BASE 40) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest write-to-string-keywords.33
  (handler-case (multiple-value-list (LET ((*PRINT-BASE* 8)) (LIST (WRITE-TO-STRING 64) (WRITE-TO-STRING 64 :BASE 10) (WRITE-TO-STRING 64)))) (error () (list :error)))
  (("100" "64" "100")))

(deftest write-to-string-keywords.34
  (handler-case (multiple-value-list (LET ((*PRINT-BASE* 16)) (WRITE-TO-STRING 255 :BASE 10 :RADIX NIL))) (error () (list :error)))
  ("255"))

(deftest write-to-string-keywords.35
  (handler-case (multiple-value-list (LIST (WRITE-TO-STRING 255 :BASE 16) *PRINT-BASE*)) (error () (list :error)))
  (("FF" 10)))

(deftest write-to-string-keywords.36
  (handler-case (multiple-value-list (WRITE-TO-STRING (QUOTE (A . B)) :PRETTY T)) (error () (list :error)))
  ("(A . B)"))

(deftest write-to-string-keywords.37
  (handler-case (multiple-value-list (WRITE-TO-STRING 1.5 :READABLY NIL)) (error () (list :error)))
  ("1.5"))

(deftest write-to-string-keywords.38
  (handler-case (multiple-value-list (WRITE-TO-STRING "a\"b" :ESCAPE T)) (error () (list :error)))
  ("\"a\\\"b\""))

(deftest write-to-string-keywords.39
  (handler-case (multiple-value-list (WRITE-TO-STRING 255 :BASE 16 :PRETTY NIL :ESCAPE NIL :CIRCLE NIL)) (error () (list :error)))
  ("FF"))

(deftest write-to-string-keywords.40
  (handler-case (multiple-value-list (LET ((L (LIST 1))) (SETF (CDR L) L) (WRITE-TO-STRING L :CIRCLE T))) (error () (list :error)))
  ("#1=(1 . #1#)"))

