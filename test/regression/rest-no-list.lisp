;;; A &rest variable used only through LENGTH, NTH with a constant index, CAR /
;;; CADR / CADDR (and FIRST / SECOND / THIRD), NULL / NOT / ENDP or as the last
;;; argument of APPLY is not consed when the function is called with up to four
;;; extra arguments. Any other use still gets a real list.

(defun %rnl-len (&rest xs) (length xs))
(defun %rnl-nth (&rest xs) (list (nth 0 xs) (nth 2 xs) (nth 7 xs)))
(defun %rnl-add4 (a b c d) (+ a b c d))
(defun %rnl-app (a &rest xs) (apply #'%rnl-add4 a xs))
(defun %rnl-esc (&rest xs) xs)
(defun %rnl-shadow (&rest xs) (let ((xs 5)) (length (list xs))))
(defun %rnl-setq (&rest xs) (setq xs (cdr xs)) (length xs))
(defun %rnl-lam (&rest xs) (funcall (lambda () (length xs))))
(defun %rnl-case (&rest xs) (case (length xs) (0 :none) (1 :one) (t :many)))
(defun %rnl-decl (&rest xs) (declare (ignore xs)) 42)
(defun %rnl-first (&rest xs) (list (first xs) (cadr xs) (null xs)))
(defun %rnl-mutate (&rest xs) (let ((l xs)) (list (length l) (eq l xs))))

(defun %rnl-loop3 () (dotimes (i 100000) (%rnl-len 1 2 3)) nil)
(defun %rnl-loop4 () (dotimes (i 100000) (%rnl-app 1 2 3 4)) nil)

(deftest-emitting-only rest-no-list.no-allocation
  (progn
    (%rnl-loop3) (%rnl-loop4)
    (list (< (min-allocated-bytes #'%rnl-loop3) 200000)
          (< (min-allocated-bytes #'%rnl-loop4) 200000)))
  (t t))

(deftest rest-no-list.values
  (list (%rnl-len) (%rnl-len 1) (%rnl-len 1 2 3 4) (%rnl-len 1 2 3 4 5)
        (%rnl-nth) (%rnl-nth 1 2 3) (%rnl-app 1 2 3 4)
        (%rnl-esc 1 2) (%rnl-esc) (%rnl-shadow 1 2) (%rnl-setq 1 2 3)
        (%rnl-lam 1 2) (%rnl-case) (%rnl-case 1) (%rnl-case 1 2)
        (%rnl-decl 1 2) (%rnl-first) (%rnl-first 9 8) (%rnl-mutate 1 2 3)
        (apply #'%rnl-len '(1 2 3)) (funcall #'%rnl-nth 5 6 7 8))
  (0 1 4 5 (nil nil nil) (1 3 nil) 10 (1 2) nil 1 2 2 :none :one :many 42
   (nil nil t) (9 8 nil) (3 t) 3 (5 7 nil)))
