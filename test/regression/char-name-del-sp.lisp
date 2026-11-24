;;; Values come from SBCL: #\Del and #\Sp are read as Rubout and Space; CHAR-NAME still answers
;;; the canonical names.

(deftest char-name-del-sp
  (list (read-from-string "#\\Del") (read-from-string "#\\del") (read-from-string "#\\Sp")
        (read-from-string "#\\SP") (char-name #\Rubout) (char-name #\Space)
        (eql (name-char "Del") (code-char 127)) (eql (name-char "sp") #\Space))
  (#.(code-char 127) #.(code-char 127) #\Space #\Space "Rubout" "Space" t t))
