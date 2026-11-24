;;; LABELS of several functions that do not call themselves or each other recursively is
;;; compiled as nested FLETs; MEMBER of a short literal list of characters / symbols / fixnums
;;; in a test position is compiled as EQL tests.

(defun %lnm-a (x y)
  (labels ((double (n) (* 2 n))
           (add-x (n) (+ n x))
           (both (n) (add-x (double n))))
    (list (double y) (add-x y) (both y))))

;; a later definition referenced by an earlier one (the order in the LABELS is not the call order)
(defun %lnm-order (x)
  (labels ((outer (n) (inner (1+ n)))
           (inner (n) (* n x)))
    (outer 4)))

;; functions used as values
(defun %lnm-values (x)
  (labels ((inc (n) (+ n x))
           (dec (n) (- n x)))
    (list (mapcar #'inc '(1 2 3)) (mapcar #'dec '(1 2 3)) (funcall #'inc 10))))

;; closures escaping the LABELS keep their captured variables
(defun %lnm-escape (x)
  (labels ((getter () x)
           (setter (v) (setq x v)))
    (list #'getter #'setter)))

;; recursion and mutual recursion stay what they were
(defun %lnm-rec (n)
  (labels ((ev (k) (if (zerop k) t (od (1- k))))
           (od (k) (if (zerop k) nil (ev (1- k))))
           (fact (k) (if (< k 2) 1 (* k (fact (1- k))))))
    (list (ev n) (od n) (fact n))))

;; the function name shadows a global one
(defun %lnm-shadow (x)
  (labels ((car (l) (list :local l x))
           (cdr (l) (car l)))
    (cdr 7)))

;; BLOCK / RETURN-FROM out of a local function
(defun %lnm-return (xs)
  (block done
    (labels ((check (n) (when (minusp n) (return-from done :negative)) n)
             (twice (n) (* 2 (check n))))
      (mapcar #'twice xs))))

(deftest labels-nest.values
  (list (%lnm-a 10 3) (%lnm-order 3) (%lnm-values 5) (%lnm-rec 5) (%lnm-shadow 1)
        (%lnm-return '(1 2 3)) (%lnm-return '(1 -2 3)))
  ((6 13 16) 15 ((6 7 8) (-4 -3 -2) 15) (nil t 120) (:local 7 1) (2 4 6) :negative))

(deftest labels-nest.escaping-closures-share-the-variable
  (let* ((fs (%lnm-escape 1)) (get (first fs)) (set (second fs)))
    (list (funcall get) (funcall set 9) (funcall get)))
  (1 9 9))

(defun %lnm-ws-p (c) (and (member c '(#\Space #\Tab #\Linefeed #\Return)) t))
(defun %lnm-kind (x)
  (cond ((member x '(:a :b :c)) :abc)
        ((member x '(1 2 3)) :num)
        ((member x '(nil)) :nil)
        ((member x '(#\x)) :x)
        (t :other)))
(defun %lnm-tail (x) (member x '(1 2 3)))
(defvar *%lnm-count* 0)
(defun %lnm-effect () (incf *%lnm-count*) 2)

(deftest member-literal.test-position
  (list (mapcar #'%lnm-ws-p (list #\Space #\a #\Tab #\Return #\Linefeed #\x))
        (mapcar #'%lnm-kind (list :a :c :d 1 3 4 nil #\x #\y 2.0 "x" 'foo))
        (%lnm-tail 2) (%lnm-tail 9))
  ((t nil t t t nil) (:abc :abc :other :num :num :other :nil :x :other :other :other :other)
   (2 3) nil))

(deftest member-literal.evaluates-its-argument-once
  (progn (setf *%lnm-count* 0)
         (list (if (member (%lnm-effect) '(1 2 3)) :yes :no) *%lnm-count*))
  (:yes 1))
