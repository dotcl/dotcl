;;; A literal nested deeper than the emitter's inline-depth cap must still be
;;; rebuilt by the .fasl itself. Past the cap the constant used to be parked in
;;; the process-local constant pool and reloaded with CilAssembler.GetConstant,
;;; which the compiling process resolves fine — but a *fresh* process loading the
;;; fasl reads whatever unrelated object happens to sit at that index (or indexes
;;; past the pool and dies with an index-out-of-bounds).
;;;
;;; Symptom: a 1500-deep quoted list came back 500 deep after a fresh load, the
;;; rest of the chain silently replaced by another compilation unit's constants.
;;;
;;; Fix: in FASL mode the over-deep sub-object is emitted into a static helper
;;; method and called, which resets both the nesting depth and the IL stack
;;; while keeping the fasl self-contained.
;;;
;;; The same-process load below cannot see the bug (the pool is right there), so
;;; the test also asserts the invariant directly: a fasl must not reference the
;;; constant pool at all.

(defun %fasl-deep-write-source (src depth)
  (with-open-file (s src :direction :output :if-exists :supersede)
    (write-string "(defparameter *deep* '" s)
    (dotimes (i depth) (write-char #\( s))
    (write-string " a " s)
    (dotimes (i depth) (write-char #\) s))
    (write-char #\) s)
    (terpri s)))

(defun %fasl-deep-list-depth (x)
  (let ((n 0))
    (loop while (consp x) do (incf n) (setf x (car x)))
    n))

(defun %fasl-deep-file-contains-p (path pattern)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (buf (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (let ((plen (length pattern)))
        (loop for i from 0 to (- len plen)
              thereis (loop for j from 0 below plen
                            always (= (aref buf (+ i j)) (char-code (char pattern j)))))))))

(defun %fasl-deep-constant-case ()
  (let* ((depth 1500)
         (tmp (format nil "~a/dotcl-deepconst-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (fasl (format nil "~a/src.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (%fasl-deep-write-source src depth)
    (compile-file src)
    (load fasl)
    (list (%fasl-deep-list-depth (symbol-value (read-from-string "*deep*")))
          ;; The fasl must rebuild the literal on its own: no process-local pool.
          (%fasl-deep-file-contains-p fasl "GetConstant"))))

(deftest-compiled-only fasl-deep-constant.self-contained
  (%fasl-deep-constant-case)
  (1500 nil))

;;; A list literal longer than the per-method chunk size is emitted as a chain of
;;; helper methods, each prepending its slice onto the list built so far (one
;;; method per literal made the JIT's load-time working set explode). The chunk
;;; boundaries must be invisible: length, element order, the dotted tail and
;;; nested elements all have to survive a compile-file / load round trip.

(defun %fasl-chunked-list-case ()
  (let* ((n 3000)
         (tmp (format nil "~a/dotcl-chunklist-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (fasl (format nil "~a/src.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      ;; Elements straddle chunk boundaries: plain fixnums, a nested list every
      ;; 7th, and a dotted tail at the very end.
      (write-string "(defparameter *chunked* '(" s)
      (dotimes (i n)
        (if (zerop (mod i 7))
            (format s "(sub ~a ~a) " i (- i))
            (format s "~a " i)))
      (write-string ". end))" s)
      (terpri s))
    (compile-file src)
    (load fasl)
    (let ((v (symbol-value (read-from-string "*chunked*"))))
      (list (length (loop for x on v while (consp x) collect (car x)))
            (nth 0 v) (nth 1 v) (nth 499 v) (nth 500 v) (nth 2999 v)
            (nth 2996 v) (nth 2499 v)
            (cdr (last v))))))

(deftest-compiled-only fasl-chunked-list.roundtrip
  (%fasl-chunked-list-case)
  (3000 (sub 0 0) 1 499 500 2999 (sub 2996 -2996) (sub 2499 -2499) end))

;;; A vector literal longer than the chunk size fills its backing array through
;;; helper methods (same JIT-working-set fix). Length and element values across
;;; chunk boundaries must round trip.

(defun %fasl-chunked-vector-case ()
  (let* ((n 3000)
         (tmp (format nil "~a/dotcl-chunkvec-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (fasl (format nil "~a/src.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defparameter *cvec* #(" s)
      (dotimes (i n)
        (if (zerop (mod i 7))
            (format s "(sub ~a) " i)
            (format s "~a " i)))
      (write-string "))" s)
      (terpri s))
    (compile-file src)
    (load fasl)
    (let ((v (symbol-value (read-from-string "*cvec*"))))
      (list (vectorp v) (length v)
            (aref v 0) (aref v 499) (aref v 500) (aref v 2996) (aref v 2999)))))

(deftest-compiled-only fasl-chunked-vector.roundtrip
  (%fasl-chunked-vector-case)
  (t 3000 (sub 0) 499 500 (sub 2996) 2999))
