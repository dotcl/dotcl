;;; package.lisp — ANSI tests for defpackage, in-package, find-package

;;; --- defpackage ---

(deftest defpackage.1
  (let ((p (defpackage "TEST-PKG-1" (:use))))
    (packagep p))
  t)

(deftest defpackage.2
  (let ((p (defpackage "TEST-PKG-2" (:use))))
    (package-name p))
  "TEST-PKG-2")

(deftest defpackage.3
  (progn
    (defpackage "TEST-PKG-3" (:use "CL"))
    (let ((p (find-package "TEST-PKG-3")))
      (packagep p)))
  t)

(deftest defpackage.export.1
  (progn
    (defpackage "TEST-PKG-EX1" (:use) (:export "FOO"))
    (let ((p (find-package "TEST-PKG-EX1")))
      (packagep p)))
  t)

;;; --- find-package ---

(deftest find-package.1
  (packagep (find-package "CL"))
  t)

(deftest find-package.2
  (packagep (find-package "COMMON-LISP"))
  t)

(deftest find-package.3
  (packagep (find-package "CL-USER"))
  t)

(deftest find-package.4
  (find-package "NONEXISTENT-PKG-12345")
  nil)

;;; --- package-name ---

(deftest package-name.1
  (package-name (find-package "CL"))
  "COMMON-LISP")

(deftest package-name.2
  (package-name (find-package "CL-USER"))
  "COMMON-LISP-USER")

;;; --- intern ---

(deftest intern.1
  (progn
    (defpackage "TEST-INTERN-1" (:use))
    (let ((s (intern "HELLO" (find-package "TEST-INTERN-1"))))
      (values (symbol-name s))))
  "HELLO")

;;; --- case ---

(deftest case.1
  (case 1 (1 :one) (2 :two) (3 :three))
  :one)

(deftest case.2
  (case 2 (1 :one) (2 :two) (3 :three))
  :two)

(deftest case.3
  (case 4 (1 :one) (2 :two) (otherwise :other))
  :other)

(deftest case.4
  (case :b ((:a :b) :ab) ((:c :d) :cd))
  :ab)

(deftest case.5
  (case 99 (1 :one) (2 :two))
  nil)

;;; --- ecase ---

(deftest ecase.1
  (ecase 1 (1 :one) (2 :two))
  :one)

(deftest ecase.2
  (signals-error (ecase 3 (1 :one) (2 :two)) error)
  t)

;;; --- with-output-to-string ---

(deftest with-output-to-string.1
  (with-output-to-string (s)
    (write-string "hello" s))
  "hello")

(deftest with-output-to-string.2
  (with-output-to-string (s)
    (write-string "abc" s)
    (write-string "def" s))
  "abcdef")

;;; --- with-input-from-string ---

(deftest with-input-from-string.1
  (with-input-from-string (s "hello")
    (values (read-line s)))
  "hello")

;;; --- multiple-value-setq ---

(deftest multiple-value-setq.1
  (let ((a nil) (b nil))
    (multiple-value-setq (a b) (values 1 2))
    (list a b))
  (1 2))

;;; --- format ---

(deftest format.nil.1
  (format nil "hello")
  "hello")

(deftest format.nil.2
  (format nil "~A" 42)
  "42")

(deftest format.nil.3
  (format nil "~A ~A" "foo" "bar")
  "foo bar")

(deftest format.nil.4
  (format nil "~S" "hello")
  "\"hello\"")

(deftest format.nil.5
  (format nil "~D" 42)
  "42")

(deftest format.nil.6
  (format nil "~%hello")
  "
hello")

(deftest format.nil.7
  (format nil "~~")
  "~")

(deftest format.nil.8
  (format nil "~B" 10)
  "1010")

(deftest format.nil.9
  (format nil "~O" 8)
  "10")

(deftest format.nil.10
  (format nil "~X" 255)
  "FF")

(deftest format.cond.1
  (format nil "~[zero~;one~;two~]" 1)
  "one")

(deftest format.cond.2
  (format nil "~:[false~;true~]" nil)
  "false")

(deftest format.cond.3
  (format nil "~:[false~;true~]" t)
  "true")

(do-tests-summary)
