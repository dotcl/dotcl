;;; dotcl:pretty-stream-p is true for the stream of a logical block being
;;; printed, including the stream a PRINT-OBJECT method, a condition report or
;;; a ~/fn/ function is handed inside one, and false elsewhere. The expected
;;; values are what sb-pretty:pretty-stream-p returns on SBCL 2.6.8.

(defvar *psp-seen* nil)

(defclass psp-obj () ())
(defmethod print-object ((o psp-obj) s)
  (push (list :object (dotcl:pretty-stream-p s)) *psp-seen*)
  (write-string "OBJ" s))

(define-condition psp-condition (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (push (list :report (dotcl:pretty-stream-p s)) *psp-seen*)
             (write-string "COND" s))))

(defun psp-slash (s a &optional colon at)
  (declare (ignore a colon at))
  (push (list :slash (dotcl:pretty-stream-p s)) *psp-seen*))

(deftest pretty-stream-p.contexts
  (let ((*psp-seen* nil)
        (*print-pretty* t))
    (list
     (format nil "~@<| ~@;~A~:>" (make-instance 'psp-obj))
     (format nil "~@<| ~@;~A~:>" (make-condition 'psp-condition))
     (format nil "~@<~/psp-slash/~:>" 1)
     (format nil "~/psp-slash/" 1)
     (princ-to-string (make-instance 'psp-obj))
     (princ-to-string (make-condition 'psp-condition))
     (with-output-to-string (s)
       (pprint-logical-block (s nil)
         (princ (dotcl:pretty-stream-p s) s)
         (princ (dotcl:pretty-stream-p *standard-output*) s)
         (pprint-logical-block (s nil)
           (princ (dotcl:pretty-stream-p s) s))
         (princ (with-output-to-string (s2)
                  (princ (dotcl:pretty-stream-p s2) s2))
                s)))
     (dotcl:pretty-stream-p (make-string-output-stream))
     (reverse *psp-seen*)))
  ("| OBJ" "| COND" "" "" "OBJ" "COND" "TNILTNIL" nil
   ((:object t) (:report t) (:slash t) (:slash nil) (:object nil) (:report nil))))
