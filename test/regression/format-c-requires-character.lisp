;;; ~C with an argument that is not a character is an error (it printed nothing).

(deftest format-c-requires-character.error
  (values (handler-case (format nil "~C" 12) (error () :error))
          (handler-case (format nil "~:C" "x") (error () :error))
          (handler-case (format nil "~@C" nil) (error () :error))
          (handler-case (format nil "a~Cb" '(1 2)) (error () :error)))
  :error :error :error :error)

(deftest format-c-requires-character.valid
  (values (format nil "~C" #\a) (format nil "~:C" #\Space) (format nil "~@C" #\a) (format nil "[~C]" #\z))
  "a" "Space" "#\\a" "[z]")
