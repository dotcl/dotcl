;;; CATCH keeps the thread's catch state in a local from entry to exit, and a
;;; function whose THROW may return to its caller's CATCH keeps the state it
;;; claimed. Arguments of the catch body's call still run inside the CATCH.

(setf dotcl:*save-sil* t)

(defun %cse-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %cse-thrower (tag v) (throw tag v))

(defun %cse-two (a b) (throw '%cse-x (list a b)))

(defun %cse-catcher (n)
  (catch '%cse-x (%cse-two n (1+ n))))

(defun %cse-plain (n)
  (catch '%cse-p (if (> n 0) (throw '%cse-p :thrown) :normal)))

(deftest-compiled-only catch-state-entry.sil-shape
  (list (%cse-has "CatchTagStack.Enter" #'%cse-catcher)
        (%cse-has "CatchTagStack.Leave" #'%cse-catcher)
        (%cse-has "ThrowReturn.Call2S" #'%cse-catcher)
        (%cse-has "ThrowReturn.ClaimS" #'%cse-two)
        (%cse-has "ThrowReturn.CanReturnS" #'%cse-two)
        (%cse-has "ThrowReturn.ReturnS" #'%cse-two)
        (%cse-has "CatchTagStack.Enter" #'%cse-plain)
        (%cse-has "CatchTagStack.Leave" #'%cse-plain))
  (t t t t t t t t))

(deftest catch-state-entry.values
  (list (%cse-catcher 1) (%cse-plain 0) (%cse-plain 1))
  ((1 2) :normal :thrown))

(deftest catch-state-entry.argument-throws-to-same-catch
  (catch '%cse-x (%cse-two (%cse-thrower '%cse-x :from-arg) 2))
  :from-arg)

(deftest catch-state-entry.argument-with-own-catch
  (catch '%cse-x (%cse-two (catch '%cse-y (%cse-thrower '%cse-y 5)) 6))
  (5 6))

(deftest catch-state-entry.argument-error-leaves-stack
  (list (handler-case (catch '%cse-x (%cse-two (error "arg") 1)) (error () :err))
        (handler-case (%cse-thrower '%cse-x 1) (control-error () :control-error)))
  (:err :control-error))

(defun %cse-nested (n)
  (if (= n 0)
      (throw '%cse-n :bottom)
      (catch '%cse-other (%cse-nested (1- n)))))

(deftest catch-state-entry.through-other-tags
  (catch '%cse-n (%cse-nested 30))
  :bottom)
