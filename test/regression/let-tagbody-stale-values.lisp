;;; A LET whose init calls a function returning several values (FIND-SYMBOL),
;;; and a TAGBODY or loop that calls one and then leaves through RETURN-FROM,
;;; must not report those values as their own when they return a constant.

(defun %ltsv-let () (let ((s (find-symbol "%LTSV-NONE" :cl-user))) (declare (ignore s)) nil))
(defun %ltsv-let* () (let* ((s (find-symbol "%LTSV-NONE" :cl-user)) (b s)) (declare (ignore b)) nil))
(defun %ltsv-let-var () (let ((s (find-symbol "%LTSV-NONE" :cl-user))) s))
(defun %ltsv-tagbody ()
  (block nil (tagbody (find-symbol "%LTSV-NONE" :cl-user) (return-from nil nil))))
(defun %ltsv-dolist (l) (dolist (x l) (find-symbol x :cl-user)))
(defun %ltsv-dotimes () (dotimes (i 1) (find-symbol "%LTSV-NONE" :cl-user)))
(defun %ltsv-do () (do ((i 0 (1+ i))) ((= i 1)) (find-symbol "%LTSV-NONE" :cl-user)))
(defun %ltsv-loop ()
  (loop for s = (find-symbol "%LTSV-NONE" :cl-user) repeat 1 collect s into r finally (return nil)))

(deftest let-tagbody-stale-values.1
  (list (multiple-value-list (%ltsv-let))
        (multiple-value-list (%ltsv-let*))
        (multiple-value-list (%ltsv-let-var))
        (multiple-value-list (%ltsv-tagbody))
        (multiple-value-list (%ltsv-dolist (list "%LTSV-NONE")))
        (multiple-value-list (%ltsv-dotimes))
        (multiple-value-list (%ltsv-do))
        (multiple-value-list (%ltsv-loop)))
  ((nil) (nil) (nil) (nil) (nil) (nil) (nil) (nil)))

(deftest let-tagbody-stale-values.values-kept
  (list (multiple-value-list (let ((a 1)) (declare (ignore a)) (find-symbol "CAR" :cl)))
        (multiple-value-list (block b (return-from b (values 1 2))))
        (multiple-value-list (funcall (lambda () (let ((a (find-symbol "X" :cl-user)))
                                                   (declare (ignore a))
                                                   (values nil 7))))))
  ((car :external) (1 2) (nil 7)))

;;; FIND-SYMBOL, INTERN, READ-LINE and SUBTYPEP in a single-value position
;;; under an inline operation, an &OPTIONAL / &KEY default, a SETQ, or a
;;; function that only passes their values on.

(defun %ltsv-car () (car (find-symbol "%LTSV-NONE" :cl-user)))
(defun %ltsv-not () (not (not (find-symbol "%LTSV-NONE" :cl-user))))
(defun %ltsv-opt () ((lambda (&optional (a (find-symbol "%LTSV-NONE" :cl-user))) a)))
(defun %ltsv-key () ((lambda (&key (a (find-symbol "%LTSV-NONE" :cl-user))) (declare (ignore a)) nil)))
(defun %ltsv-setq () (let (a) (setq a (find-symbol "%LTSV-NONE" :cl-user))))
(defun %ltsv-find (n) (find-symbol n :cl-user))
(defun %ltsv-wrap () (car (%ltsv-find "%LTSV-NONE")))
(defun %ltsv-funcall () (car (funcall #'find-symbol "%LTSV-NONE" :cl-user)))
(defun %ltsv-catch () (catch 'k (car (find-symbol "%LTSV-NONE" :cl-user))))
(defun %ltsv-intern () (let (a) (setq a (intern "CAR" :cl))))
(defun %ltsv-read-line () (with-input-from-string (s "abc") (let (x) (setq x (read-line s)))))
(defun %ltsv-read-line-eof () (with-input-from-string (s "") (car (read-line s nil nil))))
(defun %ltsv-subtypep () (not (not (subtypep 'fixnum 'integer))))
(defun %ltsv-subtypep-car () (car (subtypep 'integer 'fixnum)))

(deftest let-tagbody-stale-values.2
  (mapcar (lambda (f) (multiple-value-list (funcall f)))
          '(%ltsv-car %ltsv-not %ltsv-opt %ltsv-key %ltsv-setq %ltsv-wrap %ltsv-funcall
            %ltsv-catch %ltsv-intern %ltsv-read-line %ltsv-read-line-eof
            %ltsv-subtypep %ltsv-subtypep-car))
  ((nil) (nil) (nil) (nil) (nil) (nil) (nil) (nil) (car) ("abc") (nil) (t) (nil)))

(deftest let-tagbody-stale-values.2-values-kept
  (list (multiple-value-list (funcall (lambda () (subtypep 'fixnum 'integer))))
        (multiple-value-list (funcall (lambda () (find-symbol "CAR" :cl))))
        (multiple-value-list (%ltsv-find "%LTSV-NONE"))
        (multiple-value-bind (a b) (subtypep 'fixnum 'integer) (list a b))
        (nth-value 1 (subtypep 'integer 'fixnum))
        (multiple-value-bind (a b) (find-symbol "CAR" :cl) (list a b))
        (nth-value 1 (intern "CAR" :cl))
        (with-input-from-string (s "ab") (multiple-value-list (read-line s)))
        (with-input-from-string (s "ab") (multiple-value-bind (a b) (read-line s) (list a b)))
        (multiple-value-list (funcall #'subtypep 'integer 'fixnum)))
  ((t t) (car :external) (nil nil) (t t) t (car :external) :external ("ab" t) ("ab" t) (nil t)))

;;; The same through the function objects (FUNCALL of #'FIND-SYMBOL ...),
;;; whose values must not depend on which compiler built the core.

(deftest let-tagbody-stale-values.3
  (list (multiple-value-list (car (funcall #'find-symbol "%LTSV-NONE" :cl-user)))
        (multiple-value-list (let (a) (setq a (funcall #'intern "CAR" :cl))))
        (multiple-value-list (car (funcall #'subtypep 'integer 'fixnum)))
        (multiple-value-list (with-input-from-string (s "") (car (funcall #'read-line s nil nil))))
        (multiple-value-list (let (a) (setq a (funcall #'read-line (make-string-input-stream "ab")))))
        (multiple-value-list (funcall #'read-line (make-string-input-stream "ab")))
        (multiple-value-list (funcall #'find-symbol "CAR" :cl)))
  ((nil) (car) (nil) (nil) ("ab") ("ab" t) (car :external)))
