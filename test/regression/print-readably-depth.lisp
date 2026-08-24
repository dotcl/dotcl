;;; The printer bounds its own recursion so that a deeply nested object cannot
;;; overflow its stack, and past that bound it elides: a list prints as "(...)",
;;; an array as "#". That is a fine answer for ordinary printing.
;;;
;;; It is the wrong answer under *PRINT-READABLY* T. CLHS 22.1.3 gives
;;; *print-readably* priority over everything that would lose information, and
;;; requires an error when readable output is impossible. Eliding produced a
;;; SHORTER object that read back as something else — for a caller that stores
;;; the representation and reads it later (a .fasl storing a literal), a silently
;;; wrong constant, or a read error far from the cause: a 1500-deep literal came
;;; back as the text "(...)", and loading it failed with "a token consisting
;;; solely of dots is illegal".

(defun %prd-nest (depth)
  "A list nested DEPTH deep: (((( ... a ... ))))."
  (let ((x 'a))
    (dotimes (i depth x)
      (setf x (list x)))))

;;; Under *print-readably*, past the bound, an error — not a shortened string.

(deftest print-readably-deep-list-signals
  (handler-case
      (let ((*print-readably* t))
        (write-to-string (%prd-nest 1500))
        :no-error)
    (error () :signalled))
  :signalled)

(deftest print-readably-deep-vector-signals
  (handler-case
      (let ((*print-readably* t)
            (v (make-array 1 :initial-element (%prd-nest 1500))))
        (write-to-string v)
        :no-error)
    (error () :signalled))
  :signalled)

;;; The condition is PRINT-NOT-READABLE, the type CLHS names for this.

(deftest print-readably-deep-condition-type
  (handler-case
      (let ((*print-readably* t))
        (write-to-string (%prd-nest 1500))
        :no-error)
    (print-not-readable () :print-not-readable)
    (error () :some-other-error))
  :print-not-readable)

;;; Ordinary printing is unchanged: it still elides rather than signalling, so
;;; printing a deep object for a human keeps working.

(deftest print-not-readably-deep-list-elides
  (let ((*print-readably* nil))
    (let ((s (write-to-string (%prd-nest 1500))))
      (and (stringp s) (search "..." s) t)))
  t)

;;; Depths within the bound print in full either way — the guard must not fire
;;; early and turn an ordinary literal into an error.

(deftest print-readably-shallow-round-trips
  (let* ((*print-readably* t)
         (x (%prd-nest 100))
         (s (write-to-string x)))
    (equal (read-from-string s) x))
  t)
