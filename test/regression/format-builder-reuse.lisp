(defun fmt-reuse-printer (stream arg colon at &rest params)
  ;; ~/name/ calls out to Lisp from inside FORMAT; this one formats again.
  (declare (ignore colon at params))
  (write-string (format nil "[~D]" arg) stream))

;;; FORMAT builds its output in a per-thread StringBuilder that is reused across
;;; calls. It is handed out at most once per thread at a time; anything that
;;; re-enters FORMAT while it is held must get its own. These pin the shapes
;;; that re-enter:
;;;   - a directive that recurses (~{...~}, ~?, ~<...~>)
;;;   - an argument whose printing calls FORMAT again (PRINT-OBJECT method)
;;;   - a nested FORMAT in the argument expression

(deftest format-reuse.iteration
  (format nil "~{~A~^,~}" '(1 2 3))
  "1,2,3")

(deftest format-reuse.nested-iteration
  (format nil "[~{(~{~A~^ ~})~^ ~}]" '((1 2) (3 4)))
  "[(1 2) (3 4)]")

(deftest format-reuse.recursive-directive
  (format nil "~?" "~A-~A" '(1 2))
  "1-2")

(deftest format-reuse.argument-is-a-format
  (format nil "<~A>" (format nil "~A+~A" 1 2))
  "<1+2>")

(deftest format-reuse.conditional-and-plural
  (format nil "~D item~:P and ~[zero~;one~:;many~]" 2 2)
  "2 items and many")

(defclass fmt-reuse-obj () ((n :initarg :n :reader fmt-reuse-n)))
(defmethod print-object ((o fmt-reuse-obj) stream)
  ;; Printing this object re-enters FORMAT from inside an outer FORMAT.
  (format stream "#<obj ~A>" (format nil "n=~D" (fmt-reuse-n o))))

(deftest format-reuse.print-object-reenters
  (format nil "a=~A b=~A" (make-instance 'fmt-reuse-obj :n 1)
                          (make-instance 'fmt-reuse-obj :n 2))
  "a=#<obj n=1> b=#<obj n=2>")

(deftest format-reuse.tilde-slash-user-function-reenters
  (format nil "~/cl-user::fmt-reuse-printer/" 7)
  "[7]")

(deftest format-reuse.to-stream-and-to-string-agree
  (list (format nil "~A/~A" :x :y)
        (with-output-to-string (s) (format s "~A/~A" :x :y)))
  ("X/Y" "X/Y"))

;;; WRITE / WRITE-TO-STRING take a no-keyword fast path; the keyword forms must
;;; still bind the printer variables.
(deftest format-reuse.write-to-string-no-keywords
  (write-to-string 255)
  "255")

(deftest format-reuse.write-to-string-with-keywords
  (list (write-to-string 255 :base 16)
        (write-to-string 255 :base 2)
        (write-to-string "s" :escape nil)
        (write-to-string "s" :escape t))
  ("FF" "11111111" "s" "\"s\""))

(deftest format-reuse.write-no-keywords
  (with-output-to-string (s)
    (let ((*standard-output* s)) (write "x")))
  "\"x\"")

(deftest format-reuse.write-with-stream-keyword
  (with-output-to-string (s) (write 255 :stream s :base 8))
  "377")
