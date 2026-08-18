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

;;; MANY medium literals inside ONE top level form. Individually every one of
;;; them clears the existing caps — not deep, not a long list, not a long vector
;;; — but they all land in the same method and simply accumulate. That is how a
;;; single top level form reached megabytes of IL, which the JIT has to compile
;;; in full when the fasl is loaded. Past a per-method budget each further
;;; literal goes into its own helper method, so the round trip has to survive
;;; boundaries that now fall in the middle of a form rather than a literal.

(defun %fasl-many-literals-case ()
  (let* ((n 60)
         (per 40)
         (tmp (format nil "~a/dotcl-manylit-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (fasl (format nil "~a/src.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defparameter *many* (make-hash-table :test #'eql))" s)
      (terpri s)
      ;; One form, so the literals cannot be separated by top level splitting.
      (write-string "(let ((tbl *many*))" s)
      (terpri s)
      (dotimes (i n)
        (format s "  (setf (gethash ~a tbl) '(" i)
        (dotimes (j per)
          (format s "(k~a-~a ~a ~a sym~a) " i j i j j))
        (write-string "))" s)
        (terpri s))
      (write-string "  tbl)" s)
      (terpri s))
    (compile-file src)
    (load fasl)
    (let ((tbl (symbol-value (read-from-string "*many*"))))
      (list (hash-table-count tbl)
            ;; Entries from both sides of the spill boundary, and the tail of a
            ;; literal that may itself have been continued in another method.
            (length (gethash 0 tbl))
            (first (gethash 0 tbl))
            (first (gethash 59 tbl))
            (car (last (gethash 59 tbl)))
            (nth 17 (gethash 30 tbl))
            ;; A literal must not be shared between two entries.
            (eq (gethash 0 tbl) (gethash 1 tbl))
            ;; The helper methods have to be in the fasl itself, not the
            ;; process-local constant pool.
            (%fasl-deep-file-contains-p fasl "GetConstant")))))

(deftest-compiled-only fasl-many-literals.roundtrip
  (%fasl-many-literals-case)
  (60 40 (k0-0 0 0 sym0) (k59-0 59 0 sym0) (k59-39 59 39 sym39)
      (k30-17 30 17 sym17) nil nil))

;;; Uninterned symbols in literals get one static field each, so that every
;;; occurrence of the same gensym loads the same object. All of them used to go
;;; on CompiledModule, and .NET caps a type at 64K fields — past that the CLR
;;; refuses the type with "Internal limitation: too many fields", naming neither
;;; the file nor the cause. Fields now roll over onto holder types, so the EQ
;;; guarantee has to survive a boundary that falls between two occurrences of
;;; the same symbol.

(defun %fasl-gensym-holder-case ()
  (let* ((n 5000)                       ; > MaxFieldsPerHolder, so it rolls over
         (tmp (format nil "~a/dotcl-gsymhold-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp))
         (fasl (format nil "~a/src.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      ;; Each gensym appears twice, in two different top level forms, so the
      ;; two loads can land on different holder types.
      (write-string "(defparameter *a* '())" s) (terpri s)
      (write-string "(defparameter *b* '())" s) (terpri s)
      ;; One pool of symbol objects, made at compile time, spliced into both
      ;; forms — so the two occurrences really are the same object and the fasl
      ;; has to reproduce that.
      (format s "(eval-when (:compile-toplevel :load-toplevel :execute)~%  ~
                   (defparameter *pool*~%    ~
                     (let ((acc '())) (dotimes (i ~a) (push (make-symbol (format nil \"G~~a\" i)) acc))~
                       (nreverse acc))))" n)
      (terpri s)
      (write-string "(defmacro %gen (place)
  `(progn ,@(mapcar (lambda (g) `(push ',g ,place)) *pool*)))" s)
      (terpri s)
      (write-string "(%gen *a*)" s) (terpri s)
      (write-string "(%gen *b*)" s) (terpri s))
    (compile-file src)
    (load fasl)
    (let ((a (symbol-value (read-from-string "*a*")))
          (b (symbol-value (read-from-string "*b*"))))
      (list (length a) (length b)
            ;; Same gensym, two occurrences, two (possibly different) holders.
            (every #'eq a b)
            ;; Distinct gensyms stay distinct.
            (length (remove-duplicates a :test #'eq))
            (and (symbolp (first a)) (null (symbol-package (first a))))
            ;; The rollover actually happened (holder types are named in the
            ;; assembly's metadata).
            (%fasl-deep-file-contains-p fasl "CompiledModuleLiterals")))))

(deftest-compiled-only fasl-gensym-holders.eq-across-holders
  (%fasl-gensym-holder-case)
  (5000 5000 t 5000 t t))
