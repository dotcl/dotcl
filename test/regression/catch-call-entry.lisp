;;; CATCH whose body is a call to a compiled function: the call is entered
;;; directly, and THROW, cleanups, values and the tag stack keep their meaning.

(defun %cce-thrower (tag v) (throw tag v))

(deftest catch-call-entry.nested-inner-first
  (catch '%cce-a
    (list (catch '%cce-b (%cce-thrower '%cce-b 1))
          (catch '%cce-a (%cce-thrower '%cce-a 2))
          (%cce-thrower '%cce-a 3)))
  3)

(deftest catch-call-entry.same-tag-innermost
  (catch '%cce-t
    (list (catch '%cce-t (catch '%cce-t (%cce-thrower '%cce-t :inner)))
          :after))
  (:inner :after))

(defun %cce-uwp (log)
  (unwind-protect (%cce-thrower '%cce-u :v)
    (push :cleanup (car log))))

(deftest catch-call-entry.unwind-protect
  (let ((log (list nil)))
    (list (catch '%cce-u (%cce-uwp log)) (car log)))
  (:v (:cleanup)))

(defun %cce-mv () (throw '%cce-m (values 1 2 3)))

(deftest catch-call-entry.multiple-values
  (multiple-value-list (catch '%cce-m (%cce-mv)))
  (1 2 3))

(defun %cce-out (n)
  (if (= n 0)
      (throw '%cce-o :bottom)
      (list (catch '%cce-in (%cce-out (1- n))))))

(deftest catch-call-entry.outer-through-inner-catch
  (catch '%cce-o (%cce-out 40))
  :bottom)

(deftest catch-call-entry.no-catcher
  (handler-case (%cce-thrower '%cce-none 1)
    (control-error () :control-error))
  :control-error)

(defun %cce-deep (n)
  (if (= n 0)
      (throw '%cce-d 0)
      (1+ (catch '%cce-d (%cce-deep (1- n))))))

(deftest catch-call-entry.deep-stack
  (catch '%cce-d (%cce-deep 100))
  100)

(deftest catch-call-entry.tag-stack-after-error
  (progn
    (ignore-errors (catch '%cce-e (%cce-thrower '%cce-nonexistent 1)))
    (handler-case (%cce-thrower '%cce-e 1)
      (control-error () :control-error)))
  :control-error)

(defun %cce-fail () (error "boom"))

(deftest catch-call-entry.error-in-callee
  (list (handler-case (catch '%cce-f (%cce-fail)) (error () :err))
        (catch '%cce-f (%cce-thrower '%cce-f :ok)))
  (:err :ok))
