;;; OPEN accepts only the CLHS values for :IF-EXISTS, :IF-DOES-NOT-EXIST,
;;; and a :ELEMENT-TYPE that names a type.

(deftest open-bad-if-exists
  (handler-case (open "open-option-values-nofile" :if-exists :foo)
    (type-error (e) (type-error-datum e)))
  :foo)

(deftest open-bad-if-exists.expected-type
  (handler-case (open "open-option-values-nofile" :if-exists :foo)
    (type-error (e) (car (type-error-expected-type e))))
  member)

(deftest open-bad-if-does-not-exist
  (handler-case (open "open-option-values-nofile" :if-does-not-exist :foo)
    (type-error (e) (type-error-datum e)))
  :foo)

(deftest open-unknown-element-type
  (handler-case (open "open-option-values-nofile" :element-type 'no-such-type-zz)
    (error () :error))
  :error)

(deftest open-if-does-not-exist-nil
  (open "open-option-values-nofile" :if-does-not-exist nil)
  nil)

(deftest open-if-exists-valid-values
  (let ((file (regression-temp-file "open-option-values.txt")))
    (unwind-protect
         (progn
           (with-open-file (s file :direction :output :if-exists :supersede) (write-string "a" s))
           (with-open-file (s file :direction :output :if-exists :append) (write-string "b" s))
           (with-open-file (s file :direction :output :if-exists :overwrite) (write-string "c" s))
           (with-open-file (s file) (values (read-line s))))
      (ignore-errors (delete-file file))))
  "cb")
