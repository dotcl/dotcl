;;; ~v/function/ hands the argument V took to the function as it is.

(defun %fsvp (stream arg colon at &rest params)
  (format stream "<~S ~S ~S ~S>" arg colon at params))

(deftest format-slash-v-parameter.any-object
  (list (format nil "~v/%fsvp/" 5 'x)
        (format nil "~v/%fsvp/" 'sym 'x)
        (format nil "~v:/%fsvp/" '(1 2) "s")
        (format nil "~v@/%fsvp/" "str" 1)
        (format nil "~3,v/%fsvp/" #\c 'x)
        (format nil "~v/%fsvp/" nil 'x))
  ("<X NIL NIL (5)>" "<X NIL NIL (SYM)>" "<\"s\" T NIL ((1 2))>"
   "<1 NIL T (\"str\")>" "<X NIL NIL (3 #\\c)>" "<X NIL NIL (NIL)>"))
