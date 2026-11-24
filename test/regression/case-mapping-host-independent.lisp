;;; The reader, the printer and ~( use one Unicode simple case mapping that does
;;; not depend on the host: ICU, the invariant-globalization tables and NLS
;;; disagree on a handful of characters (long s, the Latin Extended-D letters
;;; added in Unicode 15), and a symbol must read and print the same on all of
;;; them.

(defun %cmhi-char (code) (code-char code))
(defun %cmhi-str (&rest codes) (map 'string #'code-char codes))

(deftest case-mapping-host-independent.reader-upcases-long-s
  (list (symbol-name (read-from-string (%cmhi-str #x17F)))
        (symbol-name (read-from-string (%cmhi-str #xB5)))
        (symbol-name (read-from-string (%cmhi-str #xE9))))
  ("S" #.(string (code-char #x39C)) #.(string (code-char #xC9))))

(deftest case-mapping-host-independent.reader-upcases-unicode-15-letters
  (list (char-code (char (symbol-name (read-from-string (%cmhi-str #xA7CF))) 0))
        (char-code (char (symbol-name (read-from-string (%cmhi-str #xA7D3))) 0))
        (char-code (char (symbol-name (read-from-string (%cmhi-str #xA7D5))) 0)))
  (#xA7CE #xA7D2 #xA7D4))

(deftest case-mapping-host-independent.printer-bars
  (list (prin1-to-string (intern (%cmhi-str #x17F)))
        (prin1-to-string (intern (%cmhi-str #xA7D3)))
        (prin1-to-string (intern (%cmhi-str #xA7D2))))
  (#.(format nil "|~A|" (code-char #x17F))
   #.(format nil "|~A|" (code-char #xA7D3))
   #.(string (code-char #xA7D2))))

(deftest case-mapping-host-independent.format-case-conversion
  (list (format nil "~:@(~A~)" (%cmhi-str #x17F #x61))
        (format nil "~(~A~)" (%cmhi-str #xA7CE)))
  ("SA" #.(string (code-char #xA7CF))))

(deftest case-mapping-host-independent.format-directive-is-ascii
  (handler-case (format nil (concatenate 'string "~" (string (code-char #x17F))) 1)
    (error () :error))
  :error)

(deftest case-mapping-host-independent.paren-agrees-with-char-case
  (loop for i from 0 below #x10000
        for c = (code-char i)
        count (and c
                   (or (and (upper-case-p c)
                            (not (string= (format nil "~(~c~)" c) (string (char-downcase c)))))
                       (and (lower-case-p c)
                            (not (string= (format nil "~@:(~c~)" c) (string (char-upcase c)))))))
        into bad
        finally (return bad))
  0)
