;;; :COUNT is a key of REMOVE, DELETE and SUBSTITUTE (and their -IF / -IF-NOT forms) only. FIND,
;;; POSITION, COUNT and REMOVE-DUPLICATES do not take it: it is an unknown keyword, not ignored.

(defun %sck-class (thunk) (handler-case (funcall thunk) (error () :error)))

(deftest seq-functions-count-key.rejected
  (list (%sck-class (lambda () (find 'a '(a b c b a) :count 1)))
        (%sck-class (lambda () (find-if #'symbolp '(a b) :count 1)))
        (%sck-class (lambda () (position 'a '(a b a) :count 1)))
        (%sck-class (lambda () (count 'a '(a b a) :count 1)))
        (%sck-class (lambda () (remove-duplicates '(1 2 1 3 2 1) :count 1)))
        (%sck-class (lambda () (delete-duplicates (list 1 2 1) :count 1))))
  (:error :error :error :error :error :error))

(deftest seq-functions-count-key.allowed
  (list (remove 'a '(a b a) :count 1)
        (remove-if #'symbolp '(a b c) :count 2)
        (substitute 'x 'a '(a b a) :count 1)
        (find 'a '(a b) :allow-other-keys t :count 1)
        (position 'b '(a b) :key #'identity :start 0 :end 2 :from-end t))
  ((b a) (c) (x b a) a 1))
