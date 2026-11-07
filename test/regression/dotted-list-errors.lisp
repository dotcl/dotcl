;;; A list that ends in an atom other than NIL, given where a sequence is expected, signals a
;;; TYPE-ERROR whose datum is that tail and whose expected type is LIST (the tail is not a list), so
;;; that (typep datum expected-type) is false, as the ANSI tests check.

(defun %dle-dotted () (list* 1 2 3))

(defun %dle-check (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (if (typep (type-error-datum e) (type-error-expected-type e))
                        :datum-satisfies-type
                        :ok))
    (error (e) (list :other (type-of e)))))

(defmacro %dle-forms (&rest forms)
  `(list ,@(mapcar (lambda (f) `(%dle-check (lambda () ,f))) forms)))

(deftest dotted-list-errors.sequence-functions
  (%dle-forms (map 'list #'identity (%dle-dotted))
              (map 'vector #'identity (%dle-dotted))
              (map nil #'identity (%dle-dotted))
              (find 9 (%dle-dotted))
              (find 9 (%dle-dotted) :test #'=)
              (position 9 (%dle-dotted))
              (count 9 (%dle-dotted))
              (remove 9 (%dle-dotted))
              (delete 9 (%dle-dotted))
              (substitute 0 9 (%dle-dotted))
              (substitute 0 9 (%dle-dotted) :count 1)
              (mismatch (%dle-dotted) '(1 2 3))
              (mismatch '(1 2 3) (%dle-dotted))
              (search '(1) (%dle-dotted))
              (search (%dle-dotted) '(1 2 3))
              (reduce #'+ (%dle-dotted))
              (fill (%dle-dotted) 0)
              (member 9 (%dle-dotted))
              (assoc 9 (%dle-dotted))
              (reverse (%dle-dotted))
              (length (%dle-dotted))
              (every #'identity (%dle-dotted))
              (some #'null (%dle-dotted))
              (find-if #'null (%dle-dotted))
              (position-if #'null (%dle-dotted))
              (count-if #'null (%dle-dotted))
              (remove-if #'null (%dle-dotted))
              (nreverse (%dle-dotted))
              (mapcar #'identity (%dle-dotted)))
  (:ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok :ok
   :ok :ok :ok :ok :ok :ok))
