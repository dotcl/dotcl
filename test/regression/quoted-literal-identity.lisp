;;; A quoted list is one object, not a recipe for building one.
;;;
;;; The cross-compiled path emitted a MAKECONS chain for every quoted cons, so
;;; a literal inside a function body was consed again on every call. cil-out.sil
;;; held 17,145 of those. (%map-rt-category 'list), which is four MEMBERs over
;;; literal lists of strings, cost 552 bytes; the same source typed at the REPL
;;; cost nothing, because that path puts the literal in the constant pool.
;;;
;;; Data that means the same on both sides of the output file -- strings,
;;; numbers, characters -- now takes the constant-pool road there too.
;;;
;;; Keywords are deliberately NOT included. An instruction list is a list of
;;; keyword-headed forms and the compiler NCONCs instruction lists together;
;;; sharing those templates leaves the appended tail attached to the literal,
;;; and the next compilation inherits it. That showed up as a native fixnum
;;; function compiling to invalid IL, and as "Undeclared local: Z_3" -- a local
;;; belonging to the *previous* function compiled.

;;; SBCL: T for all of these.
(deftest qlit.literal-is-one-object
  (flet ((strings () '("STRING" "SIMPLE-STRING"))
         (numbers () '(1 2 3))
         (chars () '(#\a #\b))
         (nested () '(("a" 1) ("b" 2))))
    (list (eq (strings) (strings))
          (eq (numbers) (numbers))
          (eq (chars) (chars))
          (eq (nested) (nested))
          (eq (car (nested)) (car (nested)))))
  (t t t t t))

(deftest qlit.literal-contents
  (flet ((f () '("STRING" "SIMPLE-STRING" "BASE-STRING")))
    (list (f) (length (f)) (member "SIMPLE-STRING" (f) :test #'string=)))
  (("STRING" "SIMPLE-STRING" "BASE-STRING") 3
   ("SIMPLE-STRING" "BASE-STRING")))

;;; Compiling several native fixnum functions in one session must not let one
;;; leak locals into the next. This is what breaks first when a keyword-headed
;;; instruction template gets shared.
(defun %qlit-tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (%qlit-tak (1- x)
                 (%qlit-tak (1- y) z x)
                 (%qlit-tak (1- z) x y))))

(defun %qlit-two (x y)
  (declare (fixnum x y))
  (if (< x y) x (%qlit-two (1- x) y)))

(deftest qlit.native-fixnum-functions-are-independent
  (list (%qlit-tak 18 12 6) (%qlit-two 5 2))
  (7 1))
