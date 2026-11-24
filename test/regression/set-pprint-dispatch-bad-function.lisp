;;; SET-PPRINT-DISPATCH refuses a function argument that is not a function
;;; designator (or NIL); accepted, it made every later print of a matching
;;; object fail.

(deftest set-pprint-dispatch-bad-function.refused
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (list (handler-case (progn (set-pprint-dispatch 'integer 5) :accepted)
            (type-error () :type-error))
          (let ((*print-pretty* t)) (prin1-to-string 7))))
  (:type-error "7"))

(deftest set-pprint-dispatch-bad-function.designators
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (list (set-pprint-dispatch 'integer 'car)
          (set-pprint-dispatch 'integer nil)
          (set-pprint-dispatch 'integer (lambda (s o) (declare (ignore o)) (write-string "I" s)))
          (let ((*print-pretty* t)) (prin1-to-string 7))))
  (nil nil nil "I"))

;;; A pprint-dispatch function that signals an error must not leave the printer
;;; inside a logical block: the FORMAT output after it keeps its literal text.
(deftest pprint-dispatch-error-unwinds-block
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'integer (lambda (s o) (declare (ignore s o)) (error "boom")))
    (let ((*print-pretty* t))
      (list (handler-case (prin1-to-string 1) (error () :caught))
            (handler-case (format nil "x ~S y" 1) (error () :caught))
            (progn (set-pprint-dispatch 'integer nil)
                   (format nil "F ~S~%G" 'x)))))
  (:caught :caught #.(format nil "F X~%G")))

;;; A PRINT-OBJECT method that opens a logical block with a prefix and then
;;; signals an error, inside a FORMAT logical block with a mandatory newline:
;;; the error reaches the handler and the printer is left in a clean state.
(defstruct ppbad-obj a)
(defmethod print-object ((o ppbad-obj) s)
  (pprint-logical-block (s nil :prefix "#<")
    (write-string "xx" s)
    (error "boom")))

(deftest pprint-print-object-error-in-format-block
  (let ((*print-pretty* t))
    (list (handler-case (format nil "~<~A~:@_~A~:>" (list 1 (make-ppbad-obj)))
            (error (c) (princ-to-string c)))
          (format nil "F ~S~%G" 'x)))
  ("boom" #.(format nil "F X~%G")))

;;; Same, with a newline inside the failing block, and with the enclosing block made
;;; by PPRINT-LOGICAL-BLOCK instead of FORMAT: each leaves the printer clean.
(defstruct ppbad-obj2 a)
(defmethod print-object ((o ppbad-obj2) s)
  (pprint-logical-block (s nil :prefix "[" :suffix "]")
    (write-string "xx" s)
    (pprint-newline :fill s)
    (error "boom2")))

(deftest pprint-print-object-error-with-newline
  (let ((*print-pretty* t))
    (list (handler-case (format nil "~<~A~:@_~A~:>" (list 1 (make-ppbad-obj2)))
            (error (c) (princ-to-string c)))
          (format nil "F ~S~%G" 'x)))
  ("boom2" #.(format nil "F X~%G")))

(deftest pprint-print-object-error-in-plb
  (let ((*print-pretty* t))
    (list (handler-case
              (with-output-to-string (s)
                (pprint-logical-block (s nil :prefix "(")
                  (write 1 :stream s)
                  (pprint-newline :mandatory s)
                  (write (make-ppbad-obj) :stream s)))
            (error (c) (princ-to-string c)))
          (format nil "F ~S~%G" 'x)))
  ("boom" #.(format nil "F X~%G")))

;;; Without an error, a block of a PRINT-OBJECT method inside a FORMAT block comes out
;;; as in SBCL.
(defstruct ppok-obj a)
(defmethod print-object ((o ppok-obj) s)
  (pprint-logical-block (s nil :prefix "<" :suffix ">")
    (write-string "xxxx" s)
    (pprint-newline :fill s)
    (write-string "yyyy" s)))

(deftest pprint-print-object-nested-layout
  (let ((*print-pretty* t) (*print-right-margin* 100))
    (list (format nil "~<~A~:@_~A~:>" (list 1 (make-ppok-obj)))
          (let ((*print-right-margin* 20)) (format nil "~<ab~_~A~:>" (list (make-ppok-obj))))))
  (#.(format nil "1~%<xxxxyyyy>") "ab<xxxxyyyy>"))
