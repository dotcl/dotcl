;;; An &optional or &rest function with a large body keeps the body once: the
;;; array entry and the typed entries (one per arity) all call one shared body
;;; that takes each optional as a positional argument, with a marker for one the
;;; call did not supply, or the rest list as one argument.
;;;
;;; Each arity used to carry its own copy of the body, JIT-compiled on its first
;;; call. These check that the behaviour of the copies survived, that tail
;;; self-calls loop, and that the copies are gone for a large body and kept for
;;; a small one.

(defun %psb-compile-string (form)
  (let ((compile-toplevel (find-symbol "COMPILE-TOPLEVEL" "DOTCL.CIL-COMPILER")))
    (prin1-to-string (funcall compile-toplevel form))))

(defun %psb-count (needle haystack)
  (loop with start = 0
        for pos = (search needle haystack :start2 start)
        while pos
        count t
        do (setf start (1+ pos))))

;; A body well over the copy threshold, so the entries call the shared body.
(defmacro %psb-big (&rest result)
  `(let ((acc 0))
     ,@(loop for i from 0 below 40
             collect `(when (eql acc ,(- i 100)) (setq acc (+ acc ,i))))
     (progn acc ,@result)))

(defvar *psb-count* 0)

(defun %psb-opt (a &optional (b 1) (c (+ a b)) (d (incf *psb-count*)))
  (%psb-big (list a b c d)))

(deftest positional-shared-body.optional-arities
  (let ((*psb-count* 0))
    (list (%psb-opt 10) (%psb-opt 10 5) (%psb-opt 10 5 0) (%psb-opt 10 5 0 7)
          *psb-count*))
  ((10 1 11 1) (10 5 15 2) (10 5 0 3) (10 5 0 7) 3))

(deftest positional-shared-body.optional-apply-and-nil
  (list (apply #'%psb-opt '(1 nil nil nil)) (funcall #'%psb-opt 1 2 3 4)
        (apply #'%psb-opt 1 '(2)))
  ((1 nil nil nil) (1 2 3 4) (1 2 3 1)))

(deftest positional-shared-body.optional-wrong-count
  (list (handler-case (progn (%psb-opt) :no-error) (program-error () :error))
        (handler-case (progn (apply #'%psb-opt '(1 2 3 4 5)) :no-error)
          (program-error () :error)))
  (:error :error))

(defun %psb-rest (a &rest xs)
  (%psb-big (list a xs)))

(deftest positional-shared-body.rest-arities
  (list (%psb-rest 1) (%psb-rest 1 2) (%psb-rest 1 2 3) (%psb-rest 1 2 3 4)
        (apply #'%psb-rest 1 '(5 6 7 8 9)))
  ((1 nil) (1 (2)) (1 (2 3)) (1 (2 3 4)) (1 (5 6 7 8 9))))

(defun %psb-rest-mutate (&rest xs)
  (%psb-big (setf (car xs) :changed) xs))

(deftest positional-shared-body.rest-list-is-fresh
  (let ((l (list 1 2)))
    (list (%psb-rest-mutate 1 2) (%psb-rest-mutate 3) l))
  ((:changed 2) (:changed) (1 2)))

;; Tail self-calls loop through the shared body: a million iterations would
;; overflow the stack otherwise.
(defun %psb-opt-loop (n &optional (acc 0))
  (%psb-big)
  (if (= n 0) acc (%psb-opt-loop (1- n) (+ acc 1))))

(defun %psb-opt-loop-default (n &optional (acc 0) (step 1))
  (%psb-big)
  (if (= n 0) (list acc step) (%psb-opt-loop-default (1- n) (+ acc step))))

(defun %psb-rest-loop (n &rest xs)
  (%psb-big)
  (if (= n 0) (length xs) (%psb-rest-loop (1- n) n)))

(deftest-compiled-only positional-shared-body.tail-self-call-loops
  (list (%psb-opt-loop 1000000)
        (%psb-opt-loop-default 1000000)
        (%psb-rest-loop 1000000 1 2 3))
  (1000000 (1000000 1) 1))

(defun %psb-return (a &optional (b 2))
  (%psb-big)
  (when (> a 5) (return-from %psb-return (list :early a b)))
  (list :late a b))

(deftest positional-shared-body.return-from
  (list (%psb-return 1) (%psb-return 9 3))
  ((:late 1 2) (:early 9 3)))

(defun %psb-multiple (a &rest xs)
  (%psb-big)
  (values a (length xs)))

(deftest positional-shared-body.multiple-values
  (multiple-value-list (%psb-multiple 1 2 3))
  (1 2))

;; A non-tail self-call keeps the old per-arity copies.
(defun %psb-nontail (n &optional (acc nil))
  (%psb-big)
  (if (= n 0) acc (cons n (%psb-nontail (1- n) acc))))

(deftest positional-shared-body.non-tail-recursion
  (%psb-nontail 3 '(:end))
  (3 2 1 :end))

(deftest-emitting-only positional-shared-body.large-body-is-shared
  (let ((opt (%psb-compile-string
              '(defun %psb-sil-a (a &optional (b 1) (c 2)) (%psb-big (list a b c)))))
        (rest (%psb-compile-string
               '(defun %psb-sil-b (a &rest xs) (%psb-big (list a xs))))))
    (list (%psb-count ":SHARED" opt)
          (%psb-count "(:CALL-KEY-SHARED)" opt)
          (%psb-count ":SHARED" rest)
          (%psb-count "(:CALL-KEY-SHARED)" rest)))
  ;; optional: array entry + 3 arities; rest: array entry + 3 arities
  (1 4 1 4))

(deftest-emitting-only positional-shared-body.small-body-keeps-copies
  (let ((opt (%psb-compile-string '(defun %psb-sil-c (a &optional (b 1)) (+ a b))))
        (rest (%psb-compile-string '(defun %psb-sil-d (a &rest xs) (cons a xs)))))
    (list (%psb-count ":SHARED" opt) (%psb-count ":SHARED" rest)))
  (0 0))
