;;; LOOP WITH with a destructuring pattern (CLHS 6.1.1.7, 6.1.2.2).
;;;
;;; A nested pattern such as (a (b c)) used to fail at macroexpansion time
;;; ("CAR: not a list"), because the pattern was spliced into a
;;; DESTRUCTURING-BIND lambda list after a single &OPTIONAL, so the inner
;;; list was read as an optional (var init) spec. Every level is now
;;; &OPTIONAL with an ignored &REST tail: missing values are NIL and extra
;;; values are ignored, as in LOOP FOR destructuring. Expected values are
;;; what SBCL produces.

(deftest loop-with-destructuring-flat
  (loop with (a b) = '(1 2) return (list a b))
  (1 2))

(deftest loop-with-destructuring-nested
  (loop with (a (b c)) = '(1 (2 3)) return (list a b c))
  (1 2 3))

(deftest loop-with-destructuring-nested-car
  (loop with ((a b) c) = '((1 2) 3) return (list a b c))
  (1 2 3))

(deftest loop-with-destructuring-nil-skips
  (list (loop with (a nil b) = '(1 2 3) return (list a b))
        (loop with (a (b nil c)) = '(1 (2 9 3)) return (list a b c)))
  ((1 3) (1 2 3)))

(deftest loop-with-destructuring-dotted
  (loop with (a . b) = '(1 2 3) return (list a b))
  (1 (2 3)))

(deftest loop-with-destructuring-missing-and-extra
  (list (loop with (a (b c)) = '(1 (2)) return (list a b c))
        (loop with (a b) = '(1) return (list a b))
        (loop with (a (b)) = '(1 (2 3) 4) return (list a b))
        (loop with (a b) return (list a b)))
  ((1 2 nil) (1 nil) (1 2) (nil nil)))

(deftest loop-with-destructuring-of-type
  (list (loop with (a (b c)) of-type (fixnum (fixnum fixnum)) = '(1 (2 3))
              return (list a b c))
        (loop with (a b) fixnum = '(1 2) return (list a b)))
  ((1 2 3) (1 2)))

(deftest loop-with-destructuring-and
  (loop with (a b) = '(1 2) and (c (d)) = '(3 (4)) return (list a b c d))
  (1 2 3 4))

(deftest-compiled-only loop-with-destructuring-compiled
  (funcall (compile nil '(lambda ()
                          (loop with (a (b c)) = '(1 (2 3)) return (list a b c)))))
  (1 2 3))

(deftest loop-for-destructuring-nested-and-nil
  (list (loop for (a (b c)) in '((1 (2 3)) (4 (5 6))) collect (list a b c))
        (loop for (a nil b) in '((1 2 3)) collect (list a b))
        (loop for (a (nil c)) in '((1 (2 3))) collect (list a c))
        (loop for (a (b c)) = '(1 (2 3)) then nil repeat 1 collect (list a b c)))
  (((1 2 3) (4 5 6)) ((1 3)) ((1 3)) ((1 2 3))))
