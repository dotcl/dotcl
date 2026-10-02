;;; An FLET function that captures variables, used as a value only through a
;;; macro.
;;;
;;; A local function that is only ever called is compiled with its captured
;;; variables as extra parameters instead of as a closure. Whether it is used as
;;; a value is decided on the source, so #'F produced by a macro from data the
;;; walk does not see (here a vector; in SBCL's sources, the structure its
;;; backquote reader makes for ,X) handed out the lifted function, and calling
;;; it with the original arguments failed with "wrong number of arguments".
;;; SBCL's CASE expander (PARSE-CASE-CLAUSES, #'TESTIFY under ,@) is written
;;; this way.

(defmacro flv-fn (v) `(function ,(aref v 0)))

(deftest flet-lifted-function-as-value-via-macro
  (let ((y :y))
    (flet ((flv-f (k) (list k y)))
      (list (flv-f 0) (mapcar (flv-fn #(flv-f)) '(1 2)))))
  ((0 :y) ((1 :y) (2 :y))))

(deftest flet-lifted-function-as-value-in-backquote
  (let ((test 'eql) (v 'x))
    (flet ((flv-testify (k) `(,test ,v ',k)))
      (list (flv-testify 1)
            `(or ,@(mapcar (flv-fn #(flv-testify)) '(a b))))))
  ((eql x '1) (or (eql x 'a) (eql x 'b))))
