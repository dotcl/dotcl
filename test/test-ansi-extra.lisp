;;; test-ansi-extra.lisp — CLHS audit で発見したカバレッジの穴を補う追加テスト
;;; ansi-test スイートに含まれていない振る舞いを検証する。
;;;
;;; 各テストは (assert ...) または (unless ... (error ...)) 形式で記述し、
;;; 失敗したらエラーを signaling する。

(in-package :cl-user)

(defvar *extra-pass* 0)
(defvar *extra-fail* 0)

(defmacro check (name form)
  `(handler-case
     (progn ,form (incf *extra-pass*) (format t "PASS ~a~%" ,name))
     (error (e) (incf *extra-fail*) (format t "FAIL ~a: ~a~%" ,name e))))

;;; ============================================================
;;; Ch.23 Reader: with-standard-io-syntax
;;; CLHS Figure 23-1 の全変数が仕様値にバインドされることを確認
;;; ============================================================

(check "with-standard-io-syntax/*print-readably*=t"
  (let ((*print-readably* nil))
    (with-standard-io-syntax
      (assert (eq *print-readably* t)
              () "*print-readably* should be T, got ~s" *print-readably*))))

(check "with-standard-io-syntax/*print-escape*=t"
  (let ((*print-escape* nil))
    (with-standard-io-syntax
      (assert (eq *print-escape* t)))))

(check "with-standard-io-syntax/*print-base*=10"
  (let ((*print-base* 16))
    (with-standard-io-syntax
      (assert (= *print-base* 10)))))

(check "with-standard-io-syntax/*read-base*=10"
  (let ((*read-base* 16))
    (with-standard-io-syntax
      (assert (= *read-base* 10)))))

(check "with-standard-io-syntax/*read-default-float-format*=single-float"
  (let ((*read-default-float-format* 'double-float))
    (with-standard-io-syntax
      (assert (eq *read-default-float-format* 'single-float)))))

(check "with-standard-io-syntax/*read-eval*=t"
  (let ((*read-eval* nil))
    (with-standard-io-syntax
      (assert (eq *read-eval* t)))))

(check "with-standard-io-syntax/*read-suppress*=nil"
  (let ((*read-suppress* t))
    (with-standard-io-syntax
      (assert (eq *read-suppress* nil)))))

(check "with-standard-io-syntax/*package*=cl-user"
  (let ((*package* (find-package :keyword)))
    (with-standard-io-syntax
      (assert (eq *package* (find-package :cl-user))))))

(check "with-standard-io-syntax/*print-case*=:upcase"
  (let ((*print-case* :downcase))
    (with-standard-io-syntax
      (assert (eq *print-case* :upcase)))))

(check "with-standard-io-syntax/*print-circle*=nil"
  (let ((*print-circle* t))
    (with-standard-io-syntax
      (assert (eq *print-circle* nil)))))

(check "with-standard-io-syntax/*print-pretty*=nil"
  (let ((*print-pretty* t))
    (with-standard-io-syntax
      (assert (eq *print-pretty* nil)))))

(check "with-standard-io-syntax/*print-gensym*=t"
  (let ((*print-gensym* nil))
    (with-standard-io-syntax
      (assert (eq *print-gensym* t)))))

(check "with-standard-io-syntax/*print-length*=nil"
  (let ((*print-length* 5))
    (with-standard-io-syntax
      (assert (eq *print-length* nil)))))

(check "with-standard-io-syntax/*print-level*=nil"
  (let ((*print-level* 3))
    (with-standard-io-syntax
      (assert (eq *print-level* nil)))))

(check "with-standard-io-syntax/*print-radix*=nil"
  (let ((*print-radix* t))
    (with-standard-io-syntax
      (assert (eq *print-radix* nil)))))

;;; ============================================================
;;; Ch.23 Reader: get-dispatch-macro-character / set-dispatch-macro-character
;;; ============================================================

(check "get-dispatch-macro-character/digit-returns-nil"
  ;; CLHS: if sub-char is a decimal digit, returns nil
  (assert (null (get-dispatch-macro-character #\# #\3))
          () "get-dispatch-macro-character with digit should return nil"))

(check "get-dispatch-macro-character/unset-returns-nil"
  ;; No function registered for #\{ by default
  (assert (null (get-dispatch-macro-character #\# #\{))
          () "get-dispatch-macro-character with unregistered sub-char should return nil"))

(check "get/set-dispatch-macro-character/roundtrip"
  ;; After setting a user function, get should return that exact function
  (let* ((*readtable* (copy-readtable))
         (fn (lambda (s c n) (declare (ignore s c n)) :test-result)))
    (set-dispatch-macro-character #\# #\{ fn)
    (let ((retrieved (get-dispatch-macro-character #\# #\{)))
      (assert (eq retrieved fn)
              () "get-dispatch-macro-character should return the function set by set-dispatch-macro-character, got ~s" retrieved))))

;;; ============================================================
;;; Ch.22 Printer: print-unreadable-object
;;; ============================================================

(check "print-unreadable-object/basic"
  (let ((s (with-output-to-string (out)
              (print-unreadable-object ('foo out)))))
    (assert (string= s "#<>") () "basic: expected ~s, got ~s" "#<>" s)))

(check "print-unreadable-object/:type-t"
  ;; ANSI test expects "#<TYPE >" (trailing space) when :type t and no body
  (let ((s (with-output-to-string (out)
              (print-unreadable-object ('foo out :type t)))))
    (assert (string= s "#<SYMBOL >") () ":type t: expected ~s, got ~s" "#<SYMBOL >" s)))

(check "print-unreadable-object/body"
  (let ((s (with-output-to-string (out)
              (print-unreadable-object ('foo out) (write-string "hello" out)))))
    (assert (string= s "#<hello>") () "body: expected ~s, got ~s" "#<hello>" s)))

(check "print-unreadable-object/:type-t-body"
  (let ((s (with-output-to-string (out)
              (print-unreadable-object ('foo out :type t) (write-string "bar" out)))))
    (assert (string= s "#<SYMBOL bar>") () ":type+body: expected ~s, got ~s" "#<SYMBOL bar>" s)))

(check "print-unreadable-object/:type-nil-runtime"
  ;; :type is evaluated at runtime — nil flag should suppress type printing
  (let* ((flag nil)
         (s (with-output-to-string (out)
               (print-unreadable-object ('foo out :type flag) (write-string "x" out)))))
    (assert (string= s "#<x>") () ":type nil runtime: expected ~s, got ~s" "#<x>" s)))

(check "print-unreadable-object/print-not-readable"
  ;; When *print-readably* is true, must signal print-not-readable
  (let ((err nil))
    (handler-case
      (let ((*print-readably* t))
        (with-output-to-string (out)
          (print-unreadable-object ('foo out))))
      (error (e) (setf err e)))
    (assert (typep err 'print-not-readable)
            () "expected print-not-readable, got ~s" err)))

;;; ============================================================
;;; Ch.22 Printer: princ binds *print-escape* and *print-readably*
;;; ============================================================

(check "princ/binds-print-escape-nil"
  ;; CLHS: princ is equivalent to write with :escape nil :readably nil
  ;; *print-escape* must be nil during princ output
  (let ((escape-during nil))
    (defmethod print-object :around ((x symbol) stream)
      (declare (ignore stream))
      (setf escape-during *print-escape*)
      (call-next-method))
    (with-output-to-string (s)
      (princ 'foo s))
    (assert (null escape-during)
            () "princ: *print-escape* should be nil during printing, got ~s" escape-during)))

(check "princ/binds-print-readably-nil"
  ;; *print-readably* must be nil during princ output
  (let ((*print-readably* t))
    ;; princ should not signal print-not-readable even if *print-readably* is t in outer scope
    (let ((result (with-output-to-string (s)
                    (princ "hello" s))))
      (assert (string= result "hello")
              () "princ: should output normally when outer *print-readably*=t, got ~s" result))))

;;; ============================================================
;;; Ch.22 FORMAT: ~G (General Floating-Point)
;;; ============================================================

(check "format/~G/F-range"
  ;; 1.5: n=1, dd=6 <= d=7 → use ~F format (no exponent)
  (let ((s (format nil "~G" 1.5)))
    (assert (not (find #\E s)) () "~G 1.5 should use F format, got ~s" s)))

(check "format/~G/E-range-small"
  ;; 0.001: n=-2, dd=9 > d=7 → use ~E format
  (let ((s (format nil "~G" 0.001)))
    (assert (find #\E s) () "~G 0.001 should use E format, got ~s" s)))

(check "format/~G/E-range-large"
  ;; 15000000.0: n=8, dd=-1 < 0 → use ~E format
  (let ((s (format nil "~G" 15000000.0)))
    (assert (find #\E s) () "~G 15000000.0 should use E format, got ~s" s)))

(check "format/~G/zero"
  ;; 0.0: n=0, dd=d >= 0 → use ~F format
  (let ((s (format nil "~G" 0.0)))
    (assert (not (find #\E s)) () "~G 0.0 should use F format, got ~s" s)))

;;; ============================================================
;;; Ch.22 FORMAT: ~$ (Monetary Floating-Point)
;;; ============================================================

(check "format/~$/basic"
  ;; Default: 2 decimal digits, min 1 integer digit, no width
  (let ((s (format nil "~$" 1.5)))
    (assert (string= s "1.50") () "~$ 1.5: expected \"1.50\", got ~s" s)))

(check "format/~$/negative"
  ;; Negative: sign prepended
  (let ((s (format nil "~$" -1.5)))
    (assert (string= s "-1.50") () "~$ -1.5: expected \"-1.50\", got ~s" s)))

(check "format/~$/@-sign"
  ;; @ modifier: always print sign
  (let ((s (format nil "~@$" 1.5)))
    (assert (string= s "+1.50") () "~@$ 1.5: expected \"+1.50\", got ~s" s)))

(check "format/~$/min-int-digits"
  ;; n=8: integer part padded with zeros to 8 digits
  (let ((s (format nil "~2,8$" 1.5)))
    (assert (string= s "00000001.50") () "~2,8$ 1.5: expected \"00000001.50\", got ~s" s)))

(check "format/~$/width"
  ;; w=10: total width padded with spaces (default padchar)
  (let ((s (format nil "~2,1,10$" 1.5)))
    (assert (string= s "      1.50") () "~2,1,10$ 1.5: expected \"      1.50\", got ~s" s)))

(check "format/~$/colon-at"
  ;; :@ modifier: sign before padding
  (let ((s (format nil "~2,1,10:@$" 1.5)))
    (assert (string= s "+     1.50") () "~2,1,10:@$ 1.5: expected \"+     1.50\", got ~s" s)))

(check "format/~$/padchar"
  ;; Custom padchar '.'
  (let ((s (format nil "~2,1,10,'.$" 1.5)))
    (assert (string= s "......1.50") () "~2,1,10,'.$ 1.5: expected \"......1.50\", got ~s" s)))

;;; ============================================================
;;; Ch.22 Printer: named character printing (CLHS 22.1.3.2)
;;; ============================================================

(check "print-char/space-named"
  ;; CLHS 22.1.3.2: space with *print-readably*=nil uses literal #\ form
  ;; (ANSI test PRINT.CHAR.4 expects "#\ " not "#\Space")
  (let ((s (with-output-to-string (out) (prin1 #\Space out))))
    (assert (string= s "#\\ ") () "#\\Space with readably=nil should print as \"#\\\\ \", got ~s" s)))

(check "print-char/newline-named"
  (let ((s (with-output-to-string (out) (prin1 #\Newline out))))
    (assert (string= s "#\\Newline") () "#\\Newline should print as \"#\\\\Newline\", got ~s" s)))

(check "print-char/graphic-literal"
  ;; Graphic non-named chars print as #\<char>
  (let ((s (with-output-to-string (out) (prin1 #\a out))))
    (assert (string= s "#\\a") () "#\\a should print as \"#\\\\a\", got ~s" s)))

;;; ============================================================
;;; Ch.21 Streams / Ch.22 Printer: fresh-line after write-char/write-string
;;; ============================================================

(check "fresh-line/after-write-char"
  ;; CLHS 21.2.22: fresh-line outputs newline if stream not at beginning of line
  (let ((s (with-output-to-string (out)
              (write-char #\x out)
              (fresh-line out))))   ; should output newline
    (assert (string= s (format nil "x~%"))
            () "fresh-line after write-char: expected \"x\\n\", got ~s" s)))

(check "fresh-line/after-write-string"
  (let ((s (with-output-to-string (out)
              (write-string "abc" out)
              (fresh-line out))))
    (assert (string= s (format nil "abc~%"))
            () "fresh-line after write-string: expected \"abc\\n\", got ~s" s)))

(check "fresh-line/after-terpri"
  ;; After terpri, already at line start — fresh-line should not output newline
  (let ((s (with-output-to-string (out)
              (write-string "abc" out)
              (terpri out)
              (fresh-line out))))   ; already at start of line
    (assert (string= s (format nil "abc~%"))
            () "fresh-line after terpri: should not add extra newline, got ~s" s)))

;;; ============================================================
;;; Ch.22 Printer: pprint-dispatch-table type
;;; ============================================================

(check "pprint-dispatch-table/typep"
  ;; CLHS 22.4.5: copy-pprint-dispatch returns a pprint-dispatch-table
  (let ((d (copy-pprint-dispatch)))
    (assert (typep d 'pprint-dispatch-table)
            () "copy-pprint-dispatch should return a pprint-dispatch-table, got type ~s"
            (type-of d))))

(check "pprint-dispatch-table/type-of"
  ;; PPRINT-DISPATCH-TABLE is not exported from CL per ANSI test *CL-SYMBOL-NAMES*;
  ;; check the type name string rather than symbol identity
  (let ((d (copy-pprint-dispatch)))
    (assert (string= (symbol-name (type-of d)) "PPRINT-DISPATCH-TABLE")
            () "type-of pprint-dispatch-table: expected PPRINT-DISPATCH-TABLE, got ~s"
            (type-of d))))

;;; ============================================================
;;; Ch.22 Printer: pprint-dispatch function
;;; ============================================================

(check "pprint-dispatch/custom-match"
  ;; After set-pprint-dispatch, pprint-dispatch should return the function and T
  (let ((d (copy-pprint-dispatch)))
    (set-pprint-dispatch 'integer
      (lambda (s o) (format s "#INT[~a]" o))
      0 d)
    (multiple-value-bind (fn match)
      (pprint-dispatch 42 d)
      (assert (eq match t) () "pprint-dispatch match should be T, got ~s" match)
      (assert (functionp fn) () "pprint-dispatch fn should be a function, got ~s" fn))))

(check "pprint-dispatch/no-match"
  ;; Without matching entry, returns (NIL NIL)
  (let ((d (copy-pprint-dispatch)))
    (multiple-value-bind (fn match)
      (pprint-dispatch 42 d)
      (assert (null match) () "pprint-dispatch no-match: match should be NIL, got ~s" match)
      (assert (null fn) () "pprint-dispatch no-match: fn should be NIL, got ~s" fn))))

;;; ============================================================
;;; Ch.21 Streams: make-broadcast-stream writes to all streams
;;; ============================================================

(check "broadcast-stream/write-string-all"
  ;; CLHS 21.2.2: make-broadcast-stream output goes to ALL component streams
  (let* ((s1 (make-string-output-stream))
         (s2 (make-string-output-stream))
         (bc (make-broadcast-stream s1 s2)))
    (write-string "hello" bc)
    (assert (string= (get-output-stream-string s1) "hello")
            () "broadcast-stream s1: expected \"hello\", got ~s" (get-output-stream-string s1))
    (assert (string= (get-output-stream-string s2) "hello")
            () "broadcast-stream s2: expected \"hello\", got ~s" (get-output-stream-string s2))))

;;; ============================================================
;;; Ch.21 Streams: stream-element-type and read-sequence
;;; ============================================================

(check "stream-element-type/character-stream"
  ;; String streams have CHARACTER element type
  (let ((s (make-string-input-stream "x")))
    (assert (eq (stream-element-type s) 'character)
            () "string-stream element-type should be CHARACTER, got ~s"
            (stream-element-type s))))

(check "read-sequence/string-with-bounds"
  ;; read-sequence with :start/:end fills only specified range
  (let ((buf (make-string 5 :initial-element #\-))
        (s (make-string-input-stream "hello")))
    (let ((n (read-sequence buf s :start 1 :end 4)))
      (assert (= n 4) () "read-sequence should return end index 4, got ~a" n)
      (assert (char= (char buf 0) #\-) () "pos 0 should be unchanged")
      (assert (char= (char buf 1) #\h) () "pos 1 should be h")
      (assert (char= (char buf 4) #\-) () "pos 4 should be unchanged"))))

;;; ============================================================
;;; 結果サマリ
;;; ============================================================

(format t "~%=== test-ansi-extra results: ~a passed, ~a failed ===~%"
        *extra-pass* *extra-fail*)
(when (> *extra-fail* 0)
  (error "test-ansi-extra: ~a test(s) failed" *extra-fail*))
