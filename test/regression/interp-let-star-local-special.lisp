;;; In an interpreted LET*, a variable the body declares special is dynamic from its
;;; own binding on: a later init form that calls a function reading it sees the new
;;; value. cl-ppcre's CONVERT binds FLAGS this way and calls CONVERT-AUX in a later
;;; init form; an emit-free build failed with "Unbound variable: FLAGS".

(defun %ilsls-read () (declare (special ilsls-a ilsls-b)) (list ilsls-a ilsls-b))

(defun %ilsls-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(deftest interp-let-star-local-special.later-init-sees-it
  (list (%ilsls-eval :interpret
                     '(let* ((ilsls-a 1) (ilsls-b 2) (r (%ilsls-read)))
                       (declare (special ilsls-a ilsls-b))
                       r))
        (%ilsls-eval :compile
                     '(let* ((ilsls-a 1) (ilsls-b 2) (r (%ilsls-read)))
                       (declare (special ilsls-a ilsls-b))
                       r)))
  ((1 2) (1 2)))

(deftest interp-let-star-local-special.outer-lexical-not-rebound
  (list (%ilsls-eval :interpret
                     '(let ((ilsls-a :outer))
                       (let* ((ilsls-a 5) (ilsls-b (+ ilsls-a 1)) (r (%ilsls-read)))
                         (declare (special ilsls-a ilsls-b))
                         (list r ilsls-a ilsls-b))))
        (%ilsls-eval :interpret
                     '(let* ((ilsls-a 7) (ilsls-b ilsls-a))
                       (declare (special ilsls-a))
                       (list ilsls-a ilsls-b))))
  (((5 6) 5 6) (7 7)))
