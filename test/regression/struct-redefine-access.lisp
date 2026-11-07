;;; Reading and writing structure slots through compiled code when the structure is redefined
;;; (its slot layout changes) before, between and after the accesses. An instance keeps the layout
;;; it was made with; code compiled against one layout reads instances of that layout, and
;;; reports "redefined" for an instance of another. (The two tests that need that early binding
;;; are emitting-only: the tree-walk interpreter of an emit-free build looks the version up when
;;; it runs, so it reports the old instance instead.) Accessors on a variable bound once and used
;;; several times must behave the same as one access at a time.

(defstruct (rd1 (:constructor make-rd1 (a b))) (a 0d0 :type double-float) (b 0d0 :type double-float))

(defun %rd1-sum (o) (let ((p o)) (+ (rd1-a p) (rd1-b p) (rd1-a p))))
(defun %rd1-around (o thunk)
  (let ((p o))
    (let ((x (rd1-a p)))
      (funcall thunk)
      (+ x (rd1-b p) (rd1-a p)))))
(defun %rd1-bump (o) (let ((p o)) (setf (rd1-a p) (+ (rd1-a p) 1d0)) (setf (rd1-b p) (* 2d0 (rd1-b p))) (+ (rd1-a p) (rd1-b p))))

(defvar *rd1-old* (make-rd1 1d0 2d0))

(defun %rd1-redefine ()
  (eval '(defstruct (rd1 (:constructor make-rd1 (a b c)))
          (a 0d0 :type double-float) (b 0d0 :type double-float) (c 5d0 :type double-float))))

(defun %rd1-message (thunk)
  (handler-case (funcall thunk)
    (error (e) (if (search "redefined" (princ-to-string e)) :redefined :other-error))))

(deftest struct-redefine-access.before
  (list (%rd1-sum *rd1-old*) (%rd1-around *rd1-old* (lambda () nil)) (%rd1-bump (make-rd1 1d0 2d0)))
  (4d0 4d0 6d0))

(deftest-emitting-only struct-redefine-access.during-and-after
  ;; The thunk redefines the structure between the accesses of one variable.
  (let* ((during (%rd1-around *rd1-old* #'%rd1-redefine))
         (after-old (%rd1-sum *rd1-old*))
         (new (make-rd1 10d0 20d0 30d0)))
    (list during
          after-old
          (%rd1-bump (let ((x *rd1-old*)) x))
          (%rd1-message (lambda () (%rd1-sum new)))
          (%rd1-message (lambda () (%rd1-around new (lambda () nil))))
          (%rd1-message (lambda () (%rd1-bump new)))))
  (4d0 4d0 6d0 :redefined :redefined :redefined))

(defun %rd2-message-or-value (thunk)
  (handler-case (funcall thunk)
    (error (e) (if (search "redefined" (princ-to-string e)) :redefined :other-error))))

(defstruct (rd2 (:constructor make-rd2 (a b))) (a 0d0 :type double-float) (b 0 :type fixnum))
(defun %rd2-sum (o) (let ((p o)) (+ (rd2-a p) (rd2-b p) (rd2-a p))))
(defvar *rd2-old* (make-rd2 1d0 2))

(deftest-emitting-only struct-redefine-access.slot-type-changed
  ;; Same names and count, a different type: also a different layout.
  (progn
    (eval '(defstruct (rd2 (:constructor make-rd2 (a b))) (a 0d0 :type double-float) (b 0d0 :type double-float)))
    (list (%rd2-sum *rd2-old*)
          (%rd2-message-or-value (lambda () (%rd2-sum (make-rd2 1d0 2d0))))))
  (4d0 :redefined))

(defstruct (rd3 (:constructor make-rd3 (a))) (a 0d0 :type double-float))
(defun %rd3-twice (o) (let ((p o)) (+ (rd3-a p) (rd3-a p))))

(deftest struct-redefine-access.same-definition-again
  ;; Evaluating the same DEFSTRUCT again does not change the layout: nothing goes stale.
  (let ((o (make-rd3 1.5d0)))
    (eval '(defstruct (rd3 (:constructor make-rd3 (a))) (a 0d0 :type double-float)))
    (list (%rd3-twice o) (%rd3-twice (make-rd3 2d0))))
  (3d0 4d0))

(defstruct (rd3-other (:constructor make-rd1-like ())) (z 0 :type fixnum))
(defun %rd3-message (thunk)
  (handler-case (progn (funcall thunk) :no-error) (error () :error)))

(deftest struct-redefine-access.not-a-structure
  (list (%rd3-message (lambda () (%rd3-twice 5)))
        (%rd3-message (lambda () (%rd3-twice nil)))
        (%rd3-message (lambda () (%rd3-twice (make-rd1-like)))))
  (:error :error :error))

