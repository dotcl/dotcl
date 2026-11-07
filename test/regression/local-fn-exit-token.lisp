;;; A LABELS / FLET function that is only called by name and returns from a block
;;; outside itself hands the caller an exit token instead of throwing. The behaviour
;;; of RETURN-FROM is what it always was.

(defvar *lfe-log* nil)
(defvar *lfe-special* :outer)

(defun %lfe-basic (x)
  (let ((a 0))
    (block b
      (labels ((parse () (if (> x 3) (done) (setq a 1)) (if (> x 2) (done)) (fail))
               (done () (return-from b (list :done a x)))
               (fail () (return-from b :fail)))
        (parse) nil))))

(defun %lfe-defun-block (x)
  (labels ((g (n) (if (> n 2) (return-from %lfe-defun-block (values n x 7)) (g (1+ n)))))
    (g 0)
    :not-reached))

(defun %lfe-cleanup (x)
  (setq *lfe-log* nil)
  (list (block b
          (labels ((g () (unwind-protect (progn (push :body *lfe-log*) (return-from b x))
                           (push :cleanup *lfe-log*)))
                   (h () (g) (push :after-g *lfe-log*)))
            (h)
            (push :after-h *lfe-log*)
            :normal))
        (reverse *lfe-log*)))

(defun %lfe-special ()
  (list (block b
          (labels ((g () (let ((*lfe-special* :inner)) (return-from b *lfe-special*))))
            (g)))
        *lfe-special*))

(defun %lfe-stack (x)
  (block b
    (labels ((g () (return-from b :left)))
      (list 1 (if x (g) 2) 3))))

(defun %lfe-two-blocks (x)
  (block outer
    (list :r
          (block inner
            (labels ((g (n) (cond ((= n 1) (return-from inner :inner))
                                  ((= n 2) (return-from outer :outer))
                                  (t :neither))))
              (g x))))))

(defun %lfe-walk (tree)
  (block found
    (labels ((walk (x)
               (cond ((atom x) (when (eql x 5) (return-from found :five)))
                     (t (walk (car x)) (walk (cdr x))))))
      (walk tree)
      :none)))

(defun %lfe-escaping (x)
  ;; #'g is a value: the exception path
  (block b
    (labels ((g () (return-from b x)))
      (funcall #'g)
      :no)))

(defun %lfe-dead ()
  (let (saved)
    (block b
      (labels ((g () (return-from b 1)))
        (setq saved (lambda () (g)))
        nil))
    (handler-case (funcall saved) (control-error () :control-error))))

(defun %lfe-in-cleanup (x)
  (block b
    (labels ((g () (return-from b x)))
      (unwind-protect (push :protected *lfe-log*) (g))
      :no)))

(defun %lfe-handler-case (x)
  (block b
    (labels ((g () (handler-case (error "boom") (error () (return-from b (list :handled x))))))
      (g)
      :no)))

(defun %lfe-nested-lambda (x)
  (block b
    (labels ((g () (return-from b x)))
      (mapc (lambda (y) (when (> y 1) (g))) (list 1 2 3))
      :no)))

(defun %lfe-flet (x)
  (block b
    (flet ((g () (return-from b x)))
      (flet ((h () (g) :no))
        (h)
        :no2))))

(deftest local-fn-exit-token.basic
  (list (%lfe-basic 5) (%lfe-basic 3) (%lfe-basic 1))
  ((:done 0 5) (:done 1 3) :fail))

(deftest local-fn-exit-token.values
  (multiple-value-list (%lfe-defun-block 9))
  (3 9 7))

(deftest local-fn-exit-token.cleanup-order
  (%lfe-cleanup 42)
  (42 (:body :cleanup)))

(deftest local-fn-exit-token.special
  (%lfe-special)
  (:inner :outer))

(deftest local-fn-exit-token.stack
  (list (%lfe-stack t) (%lfe-stack nil))
  (:left (1 2 3)))

(deftest local-fn-exit-token.two-blocks
  (list (%lfe-two-blocks 1) (%lfe-two-blocks 2) (%lfe-two-blocks 3))
  ((:r :inner) :outer (:r :neither)))

(deftest local-fn-exit-token.recursive
  (list (%lfe-walk '(1 (2 3) (4 (5)))) (%lfe-walk '(1 2)))
  (:five :none))

(deftest local-fn-exit-token.escaping-and-dead
  (list (%lfe-escaping 8) (%lfe-dead))
  (8 :control-error))

(deftest local-fn-exit-token.misc
  (progn (setq *lfe-log* nil)
  (list (%lfe-in-cleanup 4) (reverse *lfe-log*) (%lfe-handler-case 6) (%lfe-nested-lambda 3) (%lfe-flet 2)))
  (4 (:protected) (:handled 6) 3 2))
