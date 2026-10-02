;;; BASE-STRING and SIMPLE-BASE-STRING are classes, and a string whose element
;;; type is BASE-CHAR is an instance of them, so a method can specialize on
;;; them. GrammaTech's cl-utils (system gt) defines REPLACE-ALL methods on
;;; BASE-STRING and could not be loaded.

(deftest base-string-class.find-class
  (list (class-name (find-class 'base-string))
        (class-name (find-class 'simple-base-string)))
  (base-string simple-base-string))

(deftest base-string-class.precedence
  (subseq (mapcar #'class-name
                  (dotcl-mop:class-precedence-list (find-class 'simple-base-string)))
          0 4)
  (simple-base-string base-string simple-string string))

(deftest base-string-class.class-of
  (list (class-name (class-of (make-array 3 :element-type 'base-char
                                            :initial-element #\a)))
        (class-name (class-of (make-array 3 :element-type 'base-char
                                            :initial-element #\a
                                            :fill-pointer 1))))
  (simple-base-string base-string))

(deftest base-string-class.character-string-unchanged
  (subtypep (class-of (make-string 2 :initial-element #\a)) (find-class 'base-string))
  nil t)

(defgeneric %bsc-kind (x))
(defmethod %bsc-kind ((x string)) :string)
(defmethod %bsc-kind ((x base-string)) :base-string)

(deftest base-string-class.dispatch
  (list (%bsc-kind (make-array 2 :element-type 'base-char :initial-element #\b))
        (%bsc-kind (with-output-to-string (s nil :element-type 'base-char)
                     (write-string "xy" s)))
        (%bsc-kind (make-string 2 :initial-element #\b)))
  (:base-string :base-string :string))
