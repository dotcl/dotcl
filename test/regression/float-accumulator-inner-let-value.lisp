;;; An undeclared double accumulator stays a native double when the value added to it is a LET
;;; (a clamp written out, as an inlined function expands) reading a variable of the loop. It used to
;;; box a double on every pass (46 bytes each).

(defun %fail-clamp-loop ()
  (let ((s 0d0))
    (dotimes (i 100000)
      (incf s (let ((n (* i 0.5d0))) (if (< n 0d0) 0d0 (if (> n 1000d0) 1000d0 n)))))
    s))

(defun %fail-clamp-loop-let* ()
  (let ((s 0d0))
    (dotimes (i 100000)
      (incf s (let* ((n (* i 0.5d0)) (m (+ n 1d0))) (if (> m 500d0) 500d0 m))))
    s))

;; the loop variable's name is bound again inside the value: the inner one is a different variable
(defun %fail-clamp-loop-shadow ()
  (let ((s 0d0))
    (dotimes (i 1000)
      (incf s (let ((i 2d0)) (* i 0.5d0))))
    s))

;; an inner variable named like the accumulator
(defun %fail-clamp-loop-shadow-acc ()
  (let ((s 0d0))
    (dotimes (i 1000)
      (incf s (let ((s 3)) (* s 0.5d0))))
    s))

(defun %fail-bytes (f)
  (funcall f)
  (let ((b0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
    (funcall f)
    (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") b0)))

(deftest-compiled-only float-accumulator-inner-let-value.results
  (list (%fail-clamp-loop) (%fail-clamp-loop-let*) (%fail-clamp-loop-shadow) (%fail-clamp-loop-shadow-acc))
  (9.89995d7 4.97507495d7 1000d0 1500d0))

(deftest-compiled-only float-accumulator-inner-let-value.no-boxing
  (list (< (%fail-bytes #'%fail-clamp-loop) 20000)
        (< (%fail-bytes #'%fail-clamp-loop-let*) 20000))
  (t t))
