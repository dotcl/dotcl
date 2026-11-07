;;; modes.lisp -- do per-process timings fall into two groups?
;;;
;;; Not a program on its own: the bench scripts (process-modes.lisp, pairs.lisp,
;;; test-modes.lisp) load it with (load "bench/gate/modes.lisp") from the directory of
;;; the script, as
;;;
;;;     (load (merge-pathnames "modes.lisp" (directory-of-this-script)))
;;;
;;; Why this exists. The same binary can run a kernel at two distinct speeds
;;; depending on the process it lands in: where the JIT places the method decides
;;; whether a hot loop straddles an alignment boundary, and that changes from one
;;; process to the next. One such kernel measured 39-40 ms in some processes and
;;; 57-58 ms in the others, with the instructions of the loop identical. A median
;;; of such values tells you the share of processes in each group, not the speed
;;; of the code, and a spread figure (CV) computed across them is inflated to the
;;; point where a real difference hides inside it. So the first thing to know
;;; about a set of per-process values is whether it is one group or two.
;;;
;;; The rule, kept simple enough to state in one breath:
;;;
;;;   Sort the values. Among the gaps between neighbours that leave at least two
;;;   values above them, take the widest. Call everything below it the fast
;;;   group and everything above it the slow group. The values are split when
;;;   that gap is at least +MODES-REL+ (10%) of the fast group's median AND at
;;;   least +MODES-SPREAD+ (3) times the wider of the two groups' own ranges.
;;;
;;; The slow group needs two members, the fast group only one. Interference from
;;; the rest of the machine only ever adds time, so a single slow process is the
;;; ordinary outlier every benchmark has; a single fast process is not something
;;; noise can produce. Fewer than 3 values are not checked at all.
;;;
;;; One large outlier on top of a real split widens the slow group's range and
;;; can hide the split. The rule errs toward "not split" in that case; it never
;;; invents one out of spread alone.
;;;
;;; (split-modes values) takes a list of numbers and returns a MODES structure:
;;;   modes-n, modes-min (smallest value), modes-med (median of all values),
;;;   modes-checked (true when n >= 3), modes-split (true when split as above),
;;;   modes-fast-n, modes-fast-med, modes-slow-n, modes-slow-med (the two groups,
;;;   when split; 0 otherwise).
;;;
;;; The arithmetic is in double-floats, as the awk it replaces did it.

(defconstant +modes-rel+ 0.10d0)
(defconstant +modes-spread+ 3)

(defstruct modes n min med checked split fast-n fast-med slow-n slow-med)

;;; Median of the sorted vector V from index LO to HI (zero based, inclusive).
(defun modes-median (v lo hi)
  (let* ((k (1+ (- hi lo)))
         (mid (+ lo (floor (1- k) 2))))
    (if (oddp k)
        (aref v mid)
        (/ (+ (aref v mid) (aref v (1+ mid))) 2))))

(defun split-modes (values)
  (let* ((v (sort (map 'vector (lambda (x) (coerce x 'double-float)) values) #'<))
         (n (length v))
         (result (make-modes :n n :min (aref v 0) :med (modes-median v 0 (1- n))
                             :checked (>= n 3) :split nil
                             :fast-n 0 :fast-med 0d0 :slow-n 0 :slow-med 0d0)))
    (when (modes-checked result)
      ;; Zero based: the gap after index I (0 .. n-3) leaves at least two values above it.
      (let ((best -1d0) (at 0))
        (loop for i from 0 to (- n 3)
              for gap = (- (aref v (1+ i)) (aref v i))
              when (> gap best) do (setf best gap at i))
        (let* ((lo-range (- (aref v at) (aref v 0)))
               (hi-range (- (aref v (1- n)) (aref v (1+ at))))
               (wide (if (> lo-range hi-range) lo-range hi-range))
               (fast-med (modes-median v 0 at)))
          (when (and (>= best (* +modes-rel+ fast-med))
                     (>= best (* +modes-spread+ wide)))
            (setf (modes-split result) t
                  (modes-fast-n result) (1+ at)
                  (modes-fast-med result) fast-med
                  (modes-slow-n result) (- n (1+ at))
                  (modes-slow-med result) (modes-median v (1+ at) (1- n)))))))
    result))

;;; printf-style number formatting, so the scripts print what the awk ones printed.
;;; C rounds the exact binary value half to even, which is what ROUND of the
;;; rational does.
(defun fixed (x digits)
  "X as printf's %.<DIGITS>f writes it."
  (let* ((neg (minusp (float-sign (coerce x 'double-float))))
         (scaled (round (* (abs (rational x)) (expt 10 digits))))
         (text (format nil "~D" scaled)))
    (when (<= (length text) digits)
      (setf text (concatenate 'string (make-string (- (1+ digits) (length text)) :initial-element #\0) text)))
    (concatenate 'string (if neg "-" "")
                 (subseq text 0 (- (length text) digits))
                 (if (plusp digits) "." "")
                 (subseq text (- (length text) digits)))))

(defun pad-left (string width)
  (if (< (length string) width)
      (concatenate 'string (make-string (- width (length string)) :initial-element #\Space) string)
      string))

(defun pad-right (string width)
  (if (< (length string) width)
      (concatenate 'string string (make-string (- width (length string)) :initial-element #\Space))
      string))

(defun signed-fixed (x digits)
  "X as printf's %+.<DIGITS>f writes it."
  (let ((text (fixed x digits)))
    (if (char= (char text 0) #\-) text (concatenate 'string "+" text))))

;;; Reading "name<TAB>value" lines.
(defun split-tabs (line)
  (let ((fields '()) (start 0))
    (loop for i from 0 to (length line)
          when (or (= i (length line)) (char= (char line i) #\Tab))
            do (push (subseq line start i) fields)
               (setf start (1+ i)))
    (nreverse fields)))

(defun numeric-field (field)
  "The double-float FIELD spells (awk's idea of a number: optional sign, digits, fraction,
exponent, blanks around), or NIL."
  (let* ((s (string-trim '(#\Space #\Tab #\Return) field))
         (i 0) (n (length s)) (digits 0))
    (when (and (< i n) (find (char s i) "+-")) (incf i))
    (loop while (and (< i n) (digit-char-p (char s i))) do (incf i) (incf digits))
    (when (and (< i n) (char= (char s i) #\.))
      (incf i)
      (loop while (and (< i n) (digit-char-p (char s i))) do (incf i) (incf digits)))
    (when (zerop digits) (return-from numeric-field nil))
    (when (and (< i n) (find (char s i) "eE"))
      (let ((j (1+ i)) (exp-digits 0))
        (when (and (< j n) (find (char s j) "+-")) (incf j))
        (loop while (and (< j n) (digit-char-p (char s j))) do (incf j) (incf exp-digits))
        (when (plusp exp-digits) (setf i j))))
    (when (= i n)
      (let ((*read-default-float-format* 'double-float))
        (coerce (let ((x (read-from-string s))) (if (realp x) x 0)) 'double-float)))))

(defun read-name-values (path)
  "The (name . values) pairs of the file at PATH, in order of first appearance. A line is
name<TAB>value; blank lines and lines without a numeric value are ignored. The k-th line
for a name is its value in the k-th process."
  (let ((names '()) (table (make-hash-table :test 'equal)))
    (with-open-file (in path)
      (loop for line = (read-line in nil) while line
            do (let ((fields (split-tabs line)))
                 (when (>= (length fields) 2)
                   (let ((value (numeric-field (second fields))))
                     (when value
                       (let ((name (first fields)))
                         (unless (nth-value 1 (gethash name table)) (push name names))
                         (setf (gethash name table) (append (gethash name table) (list value))))))))))
    (mapcar (lambda (name) (cons name (gethash name table))) (nreverse names))))
