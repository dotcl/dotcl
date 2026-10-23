;;; contrib/asdf-version-range: a :version dependency may name a range, and
;;; ASDF checks it when it resolves the dependency.

(require "asdf")
(require "asdf-version-range")

(deftest asdf-version-range.satisfies
  (mapcar (lambda (c) (asdf-version-range:range-satisfies-p (first c) (second c)))
          '(("1.4.0" (:^ "1.2")) ("2.0" (:^ "1.2")) ("1.1.9" (:^ "1.2"))
            ("0.3.9" (:^ "0.3.1")) ("0.4" (:^ "0.3.1")) ("0.0.4" (:^ "0.0.4")) ("0.0.5" (:^ "0.0.4"))
            ("2" (:= "2.0")) ("2.5" (:>= "2" :< "3")) ("3.0" (:>= "2" :< "3"))
            ("3.0" "1.2") ("1.1" "1.2") ("1.0" (:> "1.0")) ("1.0" (:<= "1.0"))))
  (t nil nil t nil t nil t t nil t nil nil t))

(deftest asdf-version-range.compatible-key
  (mapcar #'asdf-version-range:compatible-key-string '("1.2.3" "2" "0.3.1" "0.0.4" "0" "0.0"))
  ("1" "2" "0.3" "0.0.4" "0" "0.0"))

(deftest asdf-version-range.invalid
  (list (handler-case (asdf-version-range:check-range '(:^)) (error () :error))
        (handler-case (asdf-version-range:check-range '(:~ "1.2")) (error () :error))
        (handler-case (asdf-version-range:check-range '(:< "x")) (error () :error)))
  (:error :error :error))

(defvar *avr-dir* (concatenate 'string (regression-temp-dir) "/avr/"))

(defun avr-write (name text)
  (let ((path (merge-pathnames name *avr-dir*)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))))

(avr-write "avr-x.asd" "(defsystem \"avr-x\" :version \"2.1.0\")")
(avr-write "avr-ok.asd" "(defsystem \"avr-ok\" :depends-on ((:version \"avr-x\" (:>= \"2\" :< \"3\"))))")
(avr-write "avr-old.asd" "(defsystem \"avr-old\" :depends-on ((:version \"avr-x\" (:^ \"1.2\"))))")

(deftest asdf-version-range.asdf-checks-range
  (let ((asdf:*central-registry* (list (pathname *avr-dir*))))
    (list (and (asdf:load-system "avr-ok") t)
          (handler-case (progn (asdf:load-system "avr-old") :loaded)
            (asdf/find-component:missing-dependency-of-version () :refused))))
  (t :refused))

(dolist (s '("avr-ok" "avr-old" "avr-x")) (asdf:clear-system s))
