;;; The reader turns a token into its string through a per-thread scratch buffer
;;; rather than a StringBuilder, hands READ-ATOM's string to PARSE-SYMBOL instead
;;; of building it twice, and READ-FROM-STRING does not copy its input when
;;; :start/:end were not given. All three are shape-preserving; these pin the
;;; shapes that go through them.

(deftest reader-token.plain-symbols-and-numbers
  (multiple-value-list (read-from-string "(a b (c . 1))"))
  ((a b (c . 1)) 13))

(deftest reader-token.position-after-token
  (multiple-value-list (read-from-string "abc def"))
  (abc 4))

(deftest reader-token.start-end
  (multiple-value-list (read-from-string "xxx(1 2)yyy" t nil :start 3 :end 8))
  ((1 2) 8))

(deftest reader-token.preserve-whitespace
  (multiple-value-list (read-from-string "ab cd" t nil :preserve-whitespace t))
  (ab 2))

(deftest reader-token.eof-value
  (multiple-value-list (read-from-string "" nil :eof))
  (:eof 0))

(deftest reader-token.mixed-objects
  (values (read-from-string "(\"s\" #\\a 1.5 :kw)"))
  ("s" #\a 1.5 :kw))

(deftest reader-token.escaped-tokens
  (list (symbol-name (read-from-string "|has space|"))
        (symbol-name (read-from-string "\\(weird"))
        (symbol-name (read-from-string "|123|")))
  ("has space" "(WEIRD" "123"))

(deftest reader-token.package-qualified
  (list (eq (read-from-string "cl:car") 'car)
        (eq (read-from-string "cl::car") 'car)
        (eq (read-from-string ":kw") :kw))
  (t t t))

(deftest reader-token.number-syntax
  (values (read-from-string "(1 -2 3/4 1.5d0 #xff #b101)"))
  (1 -2 3/4 1.5d0 255 5))

(deftest reader-token.long-token-past-scratch-size
  ;; The scratch buffer starts at 64 chars; a longer name must still read whole.
  (let* ((name (make-string 300 :initial-element #\Z))
         (sym (read-from-string name)))
    (list (length (symbol-name sym)) (char (symbol-name sym) 299)))
  (300 #\Z))

(deftest reader-token.long-package-qualified
  (let* ((name (concatenate 'string "cl::" (make-string 200 :initial-element #\Y)))
         (sym (read-from-string name)))
    (list (length (symbol-name sym)) (eq (symbol-package sym) (find-package "CL"))))
  (200 t))

(deftest reader-token.nil-and-t-are-singletons
  (list (eq (read-from-string "nil") nil) (eq (read-from-string "t") t))
  (t t))

(deftest reader-token.read-preserves-input-string
  ;; READ-FROM-STRING now reads the caller's string directly when no :start/:end;
  ;; it must not disturb it.
  (let ((s (copy-seq "(1 2 3)")))
    (read-from-string s)
    s)
  "(1 2 3)")
