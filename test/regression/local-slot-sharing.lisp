;;; Locals whose lifetimes do not overlap share one IL local slot.
;;;
;;; The compiler declares a fresh IL local for every binding and temporary, so
;;; an unrolled body declared thousands of them: RyuJIT stops optimizing a
;;; method with 2,000 or more, and ironclad's ARGON2-G-ROUNDS had 2,010. The
;;; assembler now computes where each local is live and gives locals of one type
;;; that are never live at the same time one slot. These tests check that values
;;; that ARE live at the same time (across a loop, across both arms of a branch)
;;; survive, and that the slots actually shrink.

(defmacro lss-body (n)
  `(let ((acc 0) (odd 0))
     (declare (type (unsigned-byte 24) acc odd))
     (dotimes (i n)
       ,@(loop for j below n
               collect `(let ((t1 (logand (+ i ,j) #xffff)))
                          (declare (type (unsigned-byte 16) t1))
                          (if (oddp t1)
                              (let ((u (logand (* t1 ,(1+ j)) #xffff)))
                                (setf odd (logand (+ odd u) #xffffff)))
                              (let ((w (logand (+ t1 ,(* 3 j)) #xffff)))
                                (setf acc (logand (+ acc w) #xffffff)))))))
     (list acc odd)))

(defun lss-expected (n)
  (let ((acc 0) (odd 0))
    (dotimes (i n)
      (dotimes (j n)
        (let ((t1 (logand (+ i j) #xffff)))
          (if (oddp t1)
              (setf odd (logand (+ odd (logand (* t1 (1+ j)) #xffff)) #xffffff))
              (setf acc (logand (+ acc (logand (+ t1 (* 3 j)) #xffff)) #xffffff))))))
    (list acc odd)))

(defun lss-120 (n) (declare (ignorable n)) (lss-body 120))

(deftest local-slot-sharing.values-survive
  (list (equal (lss-120 120) (lss-expected 120))
        (equal (lss-120 0) (list 0 0)))
  (t t))

(defun lss-net-list (arr)
  (loop for i below (dotnet:invoke arr "Length") collect (dotnet:invoke arr "GetValue" i)))

(deftest-compiled-only local-slot-sharing.fasl-slots-shrink
  (let ((src "lss-src-tmp.lisp")
        (fasl "lss-src-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(in-package ~s)~%" (package-name *package*))
            (format s "(defun lss-file-120 (n) (declare (ignorable n)) (lss-body 120))~%"))
          (compile-file src :output-file fasl)
          (load fasl)
          (let ((flags (dotnet:static "System.Enum" "ToObject"
                                      (dotnet:resolve-type "System.Reflection.BindingFlags") 62))
                (most 0))
            (dolist (asm (lss-net-list (dotnet:invoke (dotnet:static "System.AppDomain" "CurrentDomain")
                                                      "GetAssemblies")))
              (unless (dotnet:invoke asm "IsDynamic")
                (when (search "lss-src-tmp" (dotnet:invoke asm "Location"))
                  (dolist (ty (lss-net-list (dotnet:invoke asm "GetTypes")))
                    (dolist (m (lss-net-list (dotnet:invoke ty "GetMethods" flags)))
                      (let ((body (dotnet:invoke m "GetMethodBody")))
                        (when body
                          (setf most (max most (dotnet:invoke (dotnet:invoke body "LocalVariables")
                                                              "Count"))))))))))
            ;; 120 x 3 bindings and their temporaries, almost none live at once
            (list (equal (funcall 'lss-file-120 120) (lss-expected 120))
                  (< 0 most 60))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (t t))
