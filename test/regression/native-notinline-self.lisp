;;; A function whose parameters are all declared FIXNUM and whose return type is
;;; inferred as FIXNUM, but which declares itself NOTINLINE in its own body: the
;;; inferred type does not answer for a NOTINLINE name (CLHS 3.2.2.3), so the
;;; body is compiled as an ordinary one, and the method around it has to be the
;;; ordinary one too. It used to be the native method, which reads its raw
;;; arguments as objects: every call was a NullReferenceException.

(defun %nns-fib (n)
  (declare (fixnum n) (notinline %nns-fib))
  (if (< n 2) n (the fixnum (+ (%nns-fib (- n 1)) (%nns-fib (- n 2))))))

(defun %nns-count (n acc)
  (declare (fixnum n acc) (notinline %nns-count))
  (if (= n 0) acc (%nns-count (- n 1) (the fixnum (+ acc 2)))))

(deftest native-notinline-self.values
  (list (%nns-fib 20) (%nns-count 1000 0))
  (6765 2000))
