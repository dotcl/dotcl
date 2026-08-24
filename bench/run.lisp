;;; bench/run.lisp -- cl-bench runner with suite/name selection
;;;
;;; Usage (from project root):
;;;   make bench                    # all suites (dotcl)
;;;   make bench SUITE=gabriel      # one suite
;;;   make bench BENCH=tak          # one benchmark
;;;   make bench-state              # run both dotcl & SBCL, generate bench-state.json
;;;
;;; Or directly:
;;;   dotnet run --project runtime -- --asm compiler/cil-out.sil bench/run.lisp
;;;   ros run --load bench/run.lisp --eval '(quit)'

(defvar *bench-suite* nil "Suite to run: :gabriel :math :bignum :hash :arrays :clos :richards, or NIL for all")
(defvar *bench-name* nil  "Specific benchmark name to run (string), or NIL for all in suite")
(defvar *bench-runs* nil  "If non-nil integer N, survey mode: emit N samples as JSON array. NIL = scalar output (default).")
(defvar *bench-warmup* 1  "Number of warmup iterations to discard before recording N samples. Ignored in scalar mode.")
(defvar *bench-consed* t  "When true, also measure bytes allocated per benchmark (emitted as \"<name>/consed\").")

;;; --- Allocation measurement ---
;;;
;;; Time alone does not say why a benchmark is slow. The allocation counter does:
;;; a benchmark that conses an order of magnitude more than another implementation
;;; is paying for GC and for the allocation itself, and that is a different repair
;;; from a slow inner loop.
;;;
;;; Both hosts expose the same thing -- a monotonic process-wide count of bytes
;;; allocated since start -- so the delta across a benchmark is its consing.
;;; dotcl: element 4 of DOTCL:GC-STATS (GC.GetTotalAllocatedBytes).
;;; SBCL:  SB-EXT:GET-BYTES-CONSED.
;;; Elsewhere this returns NIL and no "/consed" key is emitted.
;;;
;;; The counter is per-thread-buffer accurate, not exact: a delta of a few
;;; hundred bytes is noise. The numbers that matter here are megabytes.

(defun bench-bytes-consed ()
  #+dotcl (nth 4 (dotcl:gc-stats))
  #+(and sbcl (not dotcl)) (sb-ext:get-bytes-consed)
  #-(or dotcl sbcl) nil)

;;; --- Results tracking ---

(defvar *bench-count* 0 "Number of benchmarks emitted so far")

;;; --- Emission helpers ---

(defun bench-emit-comma ()
  (when (> *bench-count* 0)
    (format *error-output* ",~%"))
  (incf *bench-count*))

(defun bench-emit-scalar (name elapsed times consed)
  (bench-emit-comma)
  (format *error-output* "  ~S: ~,3F" name elapsed)
  (finish-output *error-output*)
  (format t "~&~25A ~8,3F sec  (~D runs)" name elapsed times)
  (when consed
    (bench-emit-comma)
    (format *error-output* "  ~S: ~D" (bench-consed-key name) consed)
    (finish-output *error-output*)
    (format t "  ~,1F MB consed" (/ consed 1048576.0)))
  (format t "~%"))

(defun bench-consed-key (name)
  ;; A separate JSON key rather than a nested object: both aggregators
  ;; (make-state.sh, make-survey-state.lisp) key on "<name>": <value> and carry
  ;; any name through untouched, so consing gets its own dotcl/sbcl/ratio row
  ;; with no change on their side.
  (concatenate 'string name "/consed"))

(defun bench-emit-consed-samples (name samples)
  (bench-emit-comma)
  (format *error-output* "  ~S: [" (bench-consed-key name))
  (let ((first t))
    (dolist (s samples)
      (if first (setq first nil) (format *error-output* ", "))
      (format *error-output* "~D" s)))
  (format *error-output* "]")
  (finish-output *error-output*))

(defun bench-emit-samples (name samples times)
  (bench-emit-comma)
  (format *error-output* "  ~S: [" name)
  (let ((first t))
    (dolist (s samples)
      (if first (setq first nil) (format *error-output* ", "))
      (format *error-output* "~,3F" s)))
  (format *error-output* "]")
  (finish-output *error-output*)
  (format t "~&~25A ~D samples (inner ~D) " name (length samples) times)
  (let ((first t))
    (dolist (s samples)
      (if first (setq first nil) (format t " "))
      (format t "~,3F" s)))
  (format t "~%"))

(defun bench-emit-null (name)
  (bench-emit-comma)
  (format *error-output* "  ~S: null" name)
  (finish-output *error-output*))

;;; --- Timing macro ---
;;; Scalar mode (*bench-runs* = NIL): one measurement emitted as "name": 0.123
;;; Survey mode (*bench-runs* = N):   N samples emitted as   "name": [s1, s2, ...]
;;;                                   after *bench-warmup* warmup iterations are discarded
;;; Either way, when the host exposes an allocation counter, the bytes consed
;;; are emitted in the same shape under "name/consed" (*bench-consed* = NIL
;;; turns that off).

(defmacro bench (name times &body body)
  ;; The allocation counter is read outside the timed region on both ends, so
  ;; reading it (which itself conses a little) is not charged to the benchmark.
  `(when (or (null *bench-name*) (string-equal *bench-name* ,name))
     (handler-case
       (if (and *bench-runs* (> *bench-runs* 0))
           (let (samples consed)
             (dotimes (run (+ *bench-warmup* *bench-runs*))
               (let* ((c0 (and *bench-consed* (bench-bytes-consed)))
                      (start (get-internal-real-time)))
                 (dotimes (i ,times) ,@body)
                 (let* ((elapsed (float (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second)))
                        (c1 (and *bench-consed* (bench-bytes-consed))))
                   (when (>= run *bench-warmup*)
                     (push elapsed samples)
                     (when (and c0 c1) (push (- c1 c0) consed))))))
             (bench-emit-samples ,name (nreverse samples) ,times)
             (when consed (bench-emit-consed-samples ,name (nreverse consed))))
           (let* ((c0 (and *bench-consed* (bench-bytes-consed)))
                  (start (get-internal-real-time)))
             (dotimes (i ,times) ,@body)
             (let* ((elapsed (float (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)))
                    (c1 (and *bench-consed* (bench-bytes-consed))))
               (bench-emit-scalar ,name elapsed ,times
                                  (and c0 c1 (- c1 c0))))))
       (error (e)
         (bench-emit-null ,name)
         (format t "~&~25A ERROR: ~A~%" ,name e)))))

;;; --- Suite runner ---

(defmacro with-suite ((key load-files) &body benchmarks)
  `(when (or (null *bench-suite*) (eq *bench-suite* ,key))
     ,@(mapcar (lambda (f) `(load ,f))
               (if (listp load-files) load-files (list load-files)))
     (format t "~&;; --- ~A ---~%" ,key)
     ,@benchmarks))

;;; --- Header ---

(format *error-output* "~&{~%")
(finish-output *error-output*)
(format t "~&;; cl-bench~%")
(format t ";; ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
(when *bench-suite*
  (format t ";; suite: ~A~%" *bench-suite*))
(when *bench-name*
  (format t ";; name:  ~A~%" *bench-name*))
(format t ";; ----------------------------------------~%")

;;; --- Gabriel benchmarks ---

(defpackage :cl-bench.gabriel
  (:use :common-lisp)
  (:export #:boyer #:browse #:dderiv-run #:deriv-run
           #:run-destructive #:run-div2-test1 #:run-div2-test2
           #:run-fft #:run-frpoly/fixnum #:run-frpoly/bignum #:run-frpoly/float
           #:run-puzzle #:run-tak #:run-ctak #:run-trtak #:run-takl
           #:run-stak #:fprint/pretty #:fprint/ugly
           #:run-traverse #:run-triangle))

(with-suite (:gabriel "cl-bench/files/gabriel.lisp")
  (bench "tak"              100  (cl-bench.gabriel:run-tak))
  (bench "takl"              10  (cl-bench.gabriel:run-takl))
  (bench "stak"              50  (cl-bench.gabriel:run-stak))
  (bench "ctak"              50  (cl-bench.gabriel:run-ctak))
  (bench "trtak"            100  (cl-bench.gabriel:run-trtak))
  (bench "boyer"             10  (cl-bench.gabriel:boyer))
  (bench "browse"             5  (cl-bench.gabriel:browse))
  (bench "dderiv"            50  (cl-bench.gabriel:dderiv-run))
  (bench "deriv"             50  (cl-bench.gabriel:deriv-run))
  (bench "destructive"       50  (cl-bench.gabriel:run-destructive))
  (bench "div2-test-1"       50  (cl-bench.gabriel:run-div2-test1))
  (bench "div2-test-2"       50  (cl-bench.gabriel:run-div2-test2))
  (bench "fft"               10  (cl-bench.gabriel:run-fft))
  (bench "frpoly/fixnum"     30  (cl-bench.gabriel:run-frpoly/fixnum))
  (bench "frpoly/bignum"     10  (cl-bench.gabriel:run-frpoly/bignum))
  (bench "frpoly/float"      30  (cl-bench.gabriel:run-frpoly/float))
  (bench "puzzle"             5  (cl-bench.gabriel:run-puzzle))
  (bench "triangle"           1  (cl-bench.gabriel:run-triangle))
  (bench "traverse"           5  (cl-bench.gabriel:run-traverse))
  (bench "fprint/ugly"       50  (cl-bench.gabriel:fprint/ugly))
  (bench "fprint/pretty"     20  (cl-bench.gabriel:fprint/pretty)))

;;; --- Math benchmarks ---

(defpackage :cl-bench.math
  (:use :common-lisp)
  (:export #:run-factorial #:run-fib #:run-fib-ratio
           #:run-ackermann #:run-mandelbrot/complex
           #:run-mandelbrot/dfloat #:run-mrg32k3a))

(defpackage :cl-bench.crc
  (:use :common-lisp)
  (:export #:run-crc40))

(with-suite (:math ("cl-bench/files/math.lisp" "cl-bench/files/crc40.lisp"))
  (bench "factorial"         1000  (cl-bench.math:run-factorial))
  (bench "fib"                 50  (cl-bench.math:run-fib))
  (bench "fib-ratio"          500  (cl-bench.math:run-fib-ratio))
  (bench "ackermann"            1  (cl-bench.math:run-ackermann))
  (bench "mandelbrot/complex"  100 (cl-bench.math:run-mandelbrot/complex))
  (bench "mandelbrot/dfloat"   100 (cl-bench.math:run-mandelbrot/dfloat))
  (bench "mrg32k3a"            20  (cl-bench.math:run-mrg32k3a))
  (bench "crc40"                2  (cl-bench.crc:run-crc40)))

;;; --- Bignum benchmarks ---

(defpackage :cl-bench.bignum
  (:use :common-lisp)
  (:export #:run-elem-100-1000 #:run-elem-1000-100 #:run-elem-10000-1
           #:run-pari-100-10 #:run-pari-200-5 #:run-pari-1000-1
           #:run-pi-decimal/small #:run-pi-decimal/big #:run-pi-atan))

(defpackage :cl-bench.ratios
  (:use :common-lisp)
  (:export #:run-pi-ratios))

(with-suite (:bignum ("cl-bench/files/bignum.lisp" "cl-bench/files/ratios.lisp"))
  (bench "bignum/elem-100-1000"  1  (cl-bench.bignum:run-elem-100-1000))
  (bench "bignum/elem-1000-100"  1  (cl-bench.bignum:run-elem-1000-100))
  (bench "bignum/elem-10000-1"   1  (cl-bench.bignum:run-elem-10000-1))
  (bench "bignum/pari-100-10"    1  (cl-bench.bignum:run-pari-100-10))
  (bench "bignum/pari-200-5"     1  (cl-bench.bignum:run-pari-200-5))
  (bench "pi-decimal/small"    100  (cl-bench.bignum:run-pi-decimal/small))
  (bench "pi-decimal/big"        2  (cl-bench.bignum:run-pi-decimal/big))
  (bench "pi-atan"             200  (cl-bench.bignum:run-pi-atan))
  (bench "pi-ratios"             2  (cl-bench.ratios:run-pi-ratios)))

;;; --- Hash benchmarks ---

(defpackage :cl-bench.hash
  (:use :common-lisp)
  (:export #:run-slurp-lines #:hash-strings #:hash-integers))

(with-suite (:hash "cl-bench/files/hash.lisp")
  (bench "hash-strings"    2  (cl-bench.hash:hash-strings))
  (bench "hash-integers"  10  (cl-bench.hash:hash-integers)))

;;; --- Array/sequence benchmarks ---

(defpackage :cl-bench.arrays
  (:use :common-lisp)
  (:export #:bench-1d-arrays #:bench-2d-arrays #:bench-3d-arrays
           #:bench-bitvectors #:bench-strings #:bench-strings/adjustable
           #:bench-string-concat #:bench-search-sequence))

(with-suite (:arrays "cl-bench/files/arrays.lisp")
  (bench "1d-arrays"           1  (cl-bench.arrays:bench-1d-arrays))
  (bench "2d-arrays"           1  (cl-bench.arrays:bench-2d-arrays))
  (bench "3d-arrays"           1  (cl-bench.arrays:bench-3d-arrays))
  (bench "bitvectors"          3  (cl-bench.arrays:bench-bitvectors))
  (bench "strings"             1  (cl-bench.arrays:bench-strings))
  (bench "strings/adjustable"  1  (cl-bench.arrays:bench-strings/adjustable))
  (bench "string-concat"       1  (cl-bench.arrays:bench-string-concat))
  (bench "search-sequence"     1  (cl-bench.arrays:bench-search-sequence)))

;;; --- CLOS benchmarks ---

(defpackage :cl-bench.clos
  (:use :common-lisp)
  (:export #:run-defclass #:run-defmethod
           #:make-instances #:make-instances/simple
           #:methodcalls/simple #:methodcalls/simple+after
           #:methodcalls/complex #:run-eql-fib))

(with-suite (:clos "cl-bench/files/clos.lisp")
  (bench "clos/defclass"            1  (cl-bench.clos:run-defclass))
  (bench "clos/defmethod"           1  (cl-bench.clos:run-defmethod))
  (bench "clos/instantiate"         2  (cl-bench.clos:make-instances))
  (bench "clos/simple-instantiate" 200 (cl-bench.clos:make-instances/simple))
  (bench "clos/methodcalls"         5  (cl-bench.clos:methodcalls/simple))
  (bench "clos/method+after"        2  (cl-bench.clos:methodcalls/simple+after))
  (bench "clos/complex-methods"     5  (cl-bench.clos:methodcalls/complex))
  (bench "clos/eql-fib"             2  (cl-bench.clos:run-eql-fib)))

;;; --- Richards benchmark ---

(defpackage :cl-bench.richards
  (:use :common-lisp)
  (:export #:richards))

(with-suite (:richards "cl-bench/files/richards.lisp")
  (bench "richards"  5  (cl-bench.richards:richards)))

;;; --- Footer ---

(format t ";; ----------------------------------------~%")
(format *error-output* "~%}~%")
(finish-output *error-output*)
(format t ";; Done.~%")
