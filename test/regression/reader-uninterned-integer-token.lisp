;;; Values come from SBCL 2.6.8. #: followed by a sign and digits of *READ-BASE* is a reader
;;; error; any other token (a float, a ratio, a trailing dot, an escaped digit) is a symbol name.

(defun %rui (s) (handler-case (let ((*print-pretty* nil)) (prin1-to-string (read-from-string s))) (reader-error () :reader-error)))

(deftest reader-uninterned-integer-token
  (list (mapcar #'%rui '("#:123" "#:1" "#:+1" "#:-1" "(#:123 x)"))
        (mapcar #'%rui '("#:1.5" "#:1/2" "#:1e5" "#:1." "#:1a" "#:a1" "#:|123|" "#:+" "#:a"))
        (let ((*read-base* 16)) (mapcar #'%rui '("#:a" "#:ff" "#:+a" "#:g" "#:fg"))))
  ((:reader-error :reader-error :reader-error :reader-error :reader-error)
   ("#:|1.5|" "#:|1/2|" "#:|1E5|" "#:|1.|" "#:|1A|" "#:A1" "#:|123|" "#:+" "#:A")
   (:reader-error :reader-error :reader-error "#:G" "#:FG")))
