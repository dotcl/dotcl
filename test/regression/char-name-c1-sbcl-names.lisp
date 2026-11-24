;;; NAME-CHAR reads the names SBCL gives the C1 control characters (values from SBCL 2.6.8:
;;; every one of the 32 reads back as its own code). CHAR-NAME is unchanged.
(deftest char-name-c1-sbcl-names.name-char
  (mapcar (lambda (n) (char-code (name-char n)))
          '("U80" "U81" "Break-Permitted" "No-Break-Permitted" "U84" "Next-Line" "Start-Selected-Area"
            "End-Selected-Area" "Character-Tabulation-Set" "Character-Tabulation-With-Justification"
            "Reverse-Linefeed" "Single-Shift-Two" "Device-Control-String" "Private-Use-One"
            "Set-Transmit-State" "Cancel-Character" "Message-Waiting" "Start-Guarded-Area"
            "Start-String" "U99" "Single-Character-Introducer" "Control-Sequence-Introducer"
            "String-Terminator" "Operating-System-Command" "Privacy-Message"
            "Application-Program-Command" "start-selected-area"))
  (128 129 130 131 132 133 134 135 136 137 141 142 144 145 147 148 149 150 152 153 154 155 156 157 158 159 134))

(deftest char-name-c1-sbcl-names.reader
  (list (char-code #\Start-Selected-Area) (char-code #\U80) (char-code #\Break-Permitted))
  (134 128 130))
