;;; WITH-SLOTS, WITH-ACCESSORS, WITH-HASH-TABLE-ITERATOR, WITH-PACKAGE-ITERATOR,
;;; WITH-OUTPUT-TO-STRING and WITH-INPUT-FROM-STRING with a malformed argument list
;;; signal a PROGRAM-ERROR; the valid shapes still work.

(defmacro %wms-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest with-macro-arg-shape.invalid
  (list (%wms-pe (with-slots))
        (%wms-pe (with-slots (a)))
        (%wms-pe (with-slots a x))
        (%wms-pe (with-slots (1) x 1))
        (%wms-pe (with-slots ((a)) x 1))
        (%wms-pe (with-slots ((a b c)) x 1))
        (%wms-pe (with-accessors))
        (%wms-pe (with-accessors ((a)) x 1))
        (%wms-pe (with-accessors (a) x 1))
        (%wms-pe (with-accessors ((a b c)) x 1))
        (%wms-pe (with-hash-table-iterator))
        (%wms-pe (with-hash-table-iterator (x) 1))
        (%wms-pe (with-hash-table-iterator (x h y) 1))
        (%wms-pe (with-hash-table-iterator (1 (make-hash-table)) 1))
        (%wms-pe (with-package-iterator))
        (%wms-pe (with-package-iterator (x) 1))
        (%wms-pe (with-package-iterator (x :cl) 1))
        (%wms-pe (with-package-iterator (x :cl :bogus) 1))
        (%wms-pe (with-output-to-string))
        (%wms-pe (with-output-to-string (s nil :bogus 1) 1))
        (%wms-pe (with-output-to-string (s nil :element-type) 1))
        (%wms-pe (with-input-from-string (s)))
        (%wms-pe (with-input-from-string (s "a" :bogus 1) 1))
        (%wms-pe (with-input-from-string (s "a" :start) 1)))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error))

(defstruct %wms-p a b)

(deftest with-macro-arg-shape.valid
  (list (with-slots (a (bb b)) (make-%wms-p :a 1 :b 2) (list a bb))
        (with-slots () (make-%wms-p) 3)
        (with-accessors ((x %wms-p-a)) (make-%wms-p :a 4) x)
        (with-hash-table-iterator (next (make-hash-table)) (next))
        (with-package-iterator (next :cl :external) (and (next) t))
        (with-output-to-string (s) (write-string "x" s))
        (with-output-to-string (s nil :element-type 'character) (write-string "y" s))
        (with-output-to-string (s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)) (write-string "z" s) 5)
        (with-input-from-string (s "abc" :start 1 :end 3) (read-char s))
        (let (i) (with-input-from-string (s "abc" :index i) (read-char s) (read-char s)) i))
  ((1 2) 3 4 nil t "x" "y" 5 #\b 2))
