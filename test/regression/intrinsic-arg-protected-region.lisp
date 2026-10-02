;;; Calls the compiler open-codes (SUBSEQ, GETHASH, GET, PRINC / PRIN1 / PRINT
;;; with a stream, READ-LINE / READ-CHAR / PEEK-CHAR / READ with arguments)
;;; pushed each argument's value and then compiled the next argument. When a
;;; later argument opened a protected region (CATCH, HANDLER-CASE,
;;; UNWIND-PROTECT) or left one, the earlier values were on the evaluation
;;; stack where the CLR requires it empty, and the method failed with "Common
;;; Language Runtime detected an invalid program". Found by the random integer
;;; form test with its extra shapes (make test-random-forms RANDOM_EXTRA=1).

(defun %iapr-subseq (s) (list (subseq s (catch 'a 1)) (subseq s 0 (handler-case 2 (error () 0)))))
(defun %iapr-gethash (h) (gethash (catch 'a 1) h (unwind-protect 7)))
(defun %iapr-get () (get '%iapr-get (catch 'a 'p) (catch 'b 5)))
(defun %iapr-princ () (with-output-to-string (o) (princ (catch 'a 12) (catch 'b o))))
(defun %iapr-read-line (s)
  (with-input-from-string (in s)
    (list (read-char in (catch 'a t)) (read-line in (handler-case nil (error () t)) :eof))))

(deftest intrinsic-arg-protected-region
  (list (%iapr-subseq "abc") (%iapr-gethash (make-hash-table)) (%iapr-get) (%iapr-princ)
        (%iapr-read-line "xy"))
  (("bc" "ab") 7 5 "12" (#\x "y")))
