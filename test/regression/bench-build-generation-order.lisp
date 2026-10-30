;;; The Release bench build must generate the char-name table first, not after it.
(defun %bbgo-recipes ()
  (let ((r (dotcl:run-process "make" '("-n" "-B" "bench-build"))))
    (when (eql (first r) 0)
      (with-input-from-string (in (second r))
        (loop for line = (read-line in nil nil) while line collect line)))))

(deftest bench-build-generates-char-names-first
  (let* ((lines (%bbgo-recipes))
         (gen (position-if (lambda (line) (and (search "UnicodeCharNames.g.cs" line) t)) lines))
         (build (position-if (lambda (line) (and (search "dotnet build" line) t)) lines)))
    (and gen build (< gen build) t))
  t)
