;;; FORMAT directives with prefix parameters (widths, pad characters, V and #) print what they
;;; printed before the parameters were read without a substring and a box per directive.

(deftest format-width-params.aesthetic
  (list (format nil "[~10A]" "name12") (format nil "[~10@A]" "name12") (format nil "[~3A]" "longer")
        (format nil "[~10,3A]" "ab") (format nil "[~10,3@A]" "ab") (format nil "[~,,2A]" "ab")
        (format nil "[~8,,,'*A]" "ab") (format nil "[~8,,,'*@A]" "ab") (format nil "[~5A]" nil)
        (format nil "[~5@S]" "ab") (format nil "[~8S]" "ab") (format nil "[~8@S]" :k)
        (format nil "[~0A]" "ab") (format nil "[~+4A]" "ab"))
  ("[name12    ]" "[    name12]" "[longer]" "[ab         ]" "[         ab]" "[ab  ]"
   "[ab******]" "[******ab]" "[NIL  ]" "[ \"ab\"]" "[\"ab\"    ]" "[      :K]" "[ab]" "[ab  ]"))

(deftest format-width-params.integer
  (list (format nil "[~5D]" 12345) (format nil "[~5D]" 42) (format nil "[~5,'0D]" 42) (format nil "[~5,'*D]" -42)
        (format nil "[~3D]" 123456) (format nil "[~5@D]" 42) (format nil "[~:D]" 1234567)
        (format nil "[~10:D]" 1234567) (format nil "[~,,'.,2:D]" 123456) (format nil "[~8X]" 255)
        (format nil "[~8,'0B]" 5) (format nil "[~4O]" 8) (format nil "[~5D]" (expt 10 20))
        (format nil "[~200D]" 1))
  ("[12345]" "[   42]" "[00042]" "[**-42]" "[123456]" "[  +42]" "[1,234,567]" 
   "[ 1,234,567]" "[12.34.56]" "[      FF]" "[00000101]" "[  10]" "[100000000000000000000]"
   #.(format nil "[~A1]" (make-string 199 :initial-element #\Space))))

(deftest format-width-params.v-and-hash
  (list (format nil "[~VD]" 6 42) (format nil "[~V,'0D]" 6 42) (format nil "[~VA]" 5 "ab") (format nil "[~V@A]" 5 "ab")
        (format nil "[~VD]" nil 42) (format nil "[~#D]" 1 2) (format nil "[~V,VD]" 6 #\# 42)
        (format nil "[~V,V,V,VD]" 6 #\0 #\, 2 1234))
  ("[    42]" "[000042]" "[ab   ]" "[   ab]" "[42]" "[ 1]" "[####42]" "[001234]"))

(defun %fwp-fn (stream arg colon at &rest params)
  (format stream "<~A ~A ~A ~S>" arg colon at params))

(deftest format-width-params.user-function-gets-every-parameter
  (list (format nil "~12,'x,V/%fwp-fn/" 7 'arg)
        (format nil "~3,V/%fwp-fn/" nil 'a)
        (format nil "~:@/%fwp-fn/" 'z))
  ("<ARG NIL NIL (12 #\\x 7)>" "<A NIL NIL (3 NIL)>" "<Z T T NIL>"))

(deftest format-width-params.errors-and-big-numbers
  (list (handler-case (format nil "~99999999999999999999D" 1) (serious-condition () :error))
        (handler-case (format nil "~5,'D" 1) (error () :error))
        (handler-case (format nil "~V,'D" 1) (error () :error))
        (format nil "[~D]" "x")
        (length (format nil "~100D" 5)) (length (format nil "~,,,D" 5)))
  (:error :error :error "[x]" 100 1))
