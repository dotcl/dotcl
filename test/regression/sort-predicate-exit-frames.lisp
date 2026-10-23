;;; SORT calls the predicate without putting the debugger frame list back after
;;; each call; it puts it back once when the sort ends, normally or not. A
;;; predicate that leaves the sort by an error or a non-local exit must still
;;; leave the frame list as it was: the backtrace afterwards starts at the
;;; caller, and the next calls record their frames.

(defun %spef-named-lt (a b) (< a b))

(defun %spef-frames ()
  (loop for f in (dotcl:backtrace)
        when (member f '("%SPEF-OUTER" "%SPEF-NAMED-LT" "%SPEF-PRED-ERROR") :test #'string=)
          collect f))

(defun %spef-outer (thunk) (let ((r (funcall thunk))) (list r (%spef-frames))))

(defun %spef-pred-error (a b)
  (when (= a 1) (error "stop"))
  (%spef-named-lt a b))

(deftest sort-predicate-exit-frames.error
  (%spef-outer
   (lambda ()
     (handler-case (sort (list 5 3 1 4 2) #'%spef-pred-error)
       (error () :caught))))
  (:caught ("%SPEF-OUTER")))

(deftest sort-predicate-exit-frames.return-from
  (%spef-outer
   (lambda ()
     (block out
       (sort (list 5 3 1 4 2)
             (lambda (a b) (when (= a 1) (return-from out :left)) (%spef-named-lt a b))))))
  (:left ("%SPEF-OUTER")))

(deftest sort-predicate-exit-frames.throw-with-key
  (%spef-outer
   (lambda ()
     (catch 'out
       (stable-sort (vector 5 3 1 4 2)
                    (lambda (a b) (when (= a -1) (throw 'out :thrown)) (%spef-named-lt a b))
                    :key #'-))))
  (:thrown ("%SPEF-OUTER")))

(deftest sort-predicate-exit-frames.results
  (list (sort (list 5 3 1 4 2) #'%spef-named-lt)
        (coerce (sort (vector 5 3 1 4 2) #'> :key #'-) 'list)
        (stable-sort (list '(1 . a) '(0 . b) '(1 . c) '(0 . d)) #'< :key #'car)
        (sort (copy-seq "dcba") #'char<))
  ((1 2 3 4 5) (1 2 3 4 5) ((0 . b) (0 . d) (1 . a) (1 . c)) "abcd"))
