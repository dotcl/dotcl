;;; Closure &key unknown-keyword check (ANSI 3.5.1.4).
;;; The closure-body compiler's key-check gate historically diverged from the
;;; plain-function compiler: bare (lambda (&key)) closures emitted no check
;;; and silently accepted unknown keywords. Plain and closure paths must
;;; behave identically. Closure variants capture a free variable so they
;;; provably take the compile-closure-body path.

(defun ckc-plain-bare (&key) :ok)

(deftest closure-key-check.plain-bare-unknown
  (handler-case (progn (ckc-plain-bare :foo 1) :no-error)
    (program-error () :signaled))
  :signaled)

(deftest closure-key-check.closure-bare-unknown
  (let ((z :leak))
    (handler-case (progn (funcall (lambda (&key) z) :foo 1) :no-error)
      (program-error () :signaled)))
  :signaled)

(deftest closure-key-check.closure-bare-ok
  (let ((z :ok))
    (funcall (lambda (&key) z)))
  :ok)

;; :allow-other-keys t at the CALL site permits unknown keywords.
(deftest closure-key-check.allow-other-keys-arg
  (let ((z :aok))
    (funcall (lambda (&key) z) :foo 1 :allow-other-keys t))
  :aok)

;; &allow-other-keys in the lambda list permits unknown keywords.
(deftest closure-key-check.allow-other-keys-lambda-list
  (let ((z :aokl))
    (funcall (lambda (&key &allow-other-keys) z) :foo 1))
  :aokl)

;; Named &key params: known keyword accepted, unknown rejected (closure path).
(deftest closure-key-check.closure-named-key
  (let ((z 10))
    (list (funcall (lambda (&key x) (list z x)) :x 1)
          (handler-case (progn (funcall (lambda (&key x) (list z x)) :y 2)
                               :no-error)
            (program-error () :signaled))))
  ((10 1) :signaled))

;; Explicit keyword-name keys ((:kw var)): known accepted / unknown rejected,
;; both plain and closure paths.
(defun ckc-plain-exp (&key ((:kw v) :d)) v)

(deftest closure-key-check.plain-explicit-kw
  (list (ckc-plain-exp :kw 5)
        (handler-case (progn (ckc-plain-exp :other 1) :no-error)
          (program-error () :signaled)))
  (5 :signaled))

(deftest closure-key-check.closure-explicit-kw
  (let ((z 0))
    (list (funcall (lambda (&key ((:kw v) :d)) (list z v)) :kw 5)
          (handler-case (progn (funcall (lambda (&key ((:kw v) :d)) (list z v))
                                        :other 1)
                               :no-error)
            (program-error () :signaled))))
  ((0 5) :signaled))
