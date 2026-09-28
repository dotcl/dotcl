;;; compile-file of a DEFCLASS whose :ACCESSOR names an existing generic
;;; function must not cost that function its methods when the fasl is loaded.
;;;
;;; compile-file strips the definitions the file made at compile time. For a new
;;; (SETF ACC) it also dropped the generic function registry entry, but it looked
;;; the entry up by the accessor symbol ACC instead of by the setf key, so it
;;; removed the READER generic function ACC. Loading the fasl then found no
;;; generic function for ACC, made a fresh one, and every method defined before
;;; it was gone. ironclad's (defclass blake2-mac () ((digest-length :accessor
;;; digest-length ...))) wiped all 60+ DIGEST-LENGTH methods this way, but only
;;; on a first, compiling quickload; loading the same fasls later was fine.

(defgeneric cfak-len (x))
(defmethod cfak-len ((x symbol)) (list :sym x))
(defmethod cfak-len (x) (list :t x))

(deftest-compiled-only compile-file-accessor-keeps-existing-gf-methods
  (let ((src "cfak-src-tmp.lisp")
        (fasl "cfak-src-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(in-package ~s)~%" (package-name *package*))
            (format s "(defclass cfak-mac () ((len :accessor cfak-len :initarg :len)))~%"))
          (let ((gf0 #'cfak-len))
            (compile-file src :output-file fasl)
            (load fasl)
            (list (eq gf0 #'cfak-len)
                  (cfak-len :e)
                  (cfak-len 3)
                  (cfak-len (make-instance 'cfak-mac :len 7)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (t (:sym :e) (:t 3) 7))
