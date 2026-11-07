;;; FLOOR / TRUNCATE / CEILING / ROUND / GETHASH taken in a single-value position leave
;;; the values channel as "one value". What a multiple-value consumer reads next must
;;; be its own call's values.

(defvar *v2p-h* (let ((h (make-hash-table))) (setf (gethash 1 h) :one) h))
(defun %v2p-two () (values 10 20))
(defun %v2p-none () (values))

(deftest values2-primary-single.consumers
  (list
   (let ((q (floor 7 2))) (list q (multiple-value-list (%v2p-two))))
   (let ((q (truncate 7 2))) (list q (multiple-value-list (values q))))
   (let ((g (gethash 1 *v2p-h*))) (list g (multiple-value-list (gethash 2 *v2p-h*))))
   (let ((g (gethash 2 *v2p-h* :dflt))) (list g (multiple-value-list (%v2p-none))))
   (progn (ceiling 7 2) (multiple-value-list (round 7 2)))
   (progn (floor 7 2) (length (multiple-value-list (%v2p-none))))
   (multiple-value-list (progn (gethash 1 *v2p-h*) (%v2p-two)))
   (multiple-value-bind (a b) (progn (floor 9 4) (floor 9 4)) (list a b))
   (list (+ (floor 9 4) (truncate 9 4)) (multiple-value-list (gethash 1 *v2p-h*)))
   (funcall (lambda () (floor 9 4))))
  ((3 (10 20)) (3 (3)) (:one (nil nil)) (:dflt nil) (4 -1) 0 (10 20) (2 1) (4 (:one t)) 2))

(deftest values2-primary-single.aref-string
  (let ((s (copy-seq "abc"))
        (f (make-array 4 :element-type 'character :adjustable t :fill-pointer 2 :initial-element #\z)))
    (list (aref s 0) (aref s 2) (aref "xyz" 1) (aref f 1)
          (handler-case (aref s 3) (error () :error))
          (handler-case (aref s -1) (error () :error))
          (handler-case (aref s 1.5) (error () :error))
          (let ((i 2)) (aref s i))))
  (#\a #\c #\y #\z :error :error :error #\c))
