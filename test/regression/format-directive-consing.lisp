;;; A FORMAT directive does not allocate a parameter list it has no parameters for.
;;;
;;; Every directive built a List<object?> for its prefix parameters and then an
;;; object?[] of the resolved ones -- before looking at whether there were any.
;;; Most directives have none (~A, ~S, ~D, ~%), so that was 56 bytes each, on top
;;; of what the directive actually does. FORMAT to a stream, which does not even
;;; build a result string, cost 152 B for one ~A and 304 B for three.
;;;
;;; The list is now built only when a parameter turns up, and the resolved array
;;; is the shared empty one when there are none.
;;;
;;; Values here are dotcl's own, and were byte-identical before and after the
;;; change -- the parameterised directives especially, since that is the path
;;; that still builds both.

;;; Directives with no prefix parameters: the case that stops allocating.

(deftest format-directive-consing.no-parameters
  (list (format nil "~a" 1)
        (format nil "~a" "hi")
        (format nil "~s" "hi")
        (format nil "~a" nil)
        (format nil "~:a" nil)
        (format nil "~d" 12345)
        (format nil "~x ~o ~b" 255 8 5)
        (format nil "~~")
        (format nil "~c" #\a))
  ("1" "hi" "\"hi\"" "NIL" "()" "12345" "FF 10 101" "~" "a"))

;;; One numeric or character parameter -- the list is built here, so these check
;;; that building it lazily did not change when it is built.

(deftest format-directive-consing.one-parameter
  (list (format nil "~5a|" "ab")
        (format nil "~5@a|" "ab")
        (format nil "~5d|" 42)
        (format nil "~5,'0d|" 42)
        (format nil "~,2f" 3.14159)
        (format nil "~10,2f|" 3.14159)
        (format nil "~3r" 10)
        (format nil "~10,,,'*a|" "ab"))
  ("ab   |" "   ab|" "   42|" "00042|" "3.14" "      3.14|" "101" "ab********|"))

;;; Empty parameters -- a bare comma adds a null placeholder, which is the branch
;;; that used to reach for the list before deciding whether any parameter existed.

(deftest format-directive-consing.empty-parameters
  (list (format nil "~,,2a|" "ab")
        (format nil "~,,,'-a|" "ab")
        (format nil "~,,1,'xd|" 42))
  ("ab  |" "ab|" "42|"))

;;; V and # parameters consume or count arguments while being resolved.

(deftest format-directive-consing.v-and-hash-parameters
  (list (format nil "~va|" 5 "ab")
        (format nil "~v,'0d|" 5 42)
        (format nil "~#[none~;one~:;many~]" 1 2)
        (format nil "~#[zero~;one~;two~]"))
  ("ab   |" "00042|" "many" "zero"))

;;; The directives that read arguments out of order, since the parameter
;;; resolution pass is what advances the argument index for ~V.

;;; The point of the change, measured against a stream so no result string is
;;; built and what is left is the per-directive cost.

(defparameter *fdc-sink* (make-broadcast-stream))

(defun %fdc-one (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (format *fdc-sink* "~a" 1)))))

(defun %fdc-three (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (format *fdc-sink* "~a~a~a" 1 2 3)))))

;; The bounds sit between what these cost now (96 and 136 B) and what they cost
;; before (152 and 304). Neither can reach zero: FORMAT still turns each argument
;; into a string and still copies the argument array once.
;; Compiled-only, like the other consing assertions.
(deftest-compiled-only format-directive-consing.one-directive
  (< (bytes-per-op #'%fdc-one) 125)
  t)

(deftest-compiled-only format-directive-consing.three-directives
  (< (bytes-per-op #'%fdc-three) 200)
  t)
