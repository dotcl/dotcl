;;; DEFSTRUCT with a malformed name, option or slot description signals a PROGRAM-ERROR;
;;; the valid shapes still work.

(defmacro %dst-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest defstruct-shape.invalid
  (list (%dst-pe (defstruct))
        (%dst-pe (defstruct 1))
        (%dst-pe (defstruct %dst-a1 1))
        (%dst-pe (defstruct %dst-a2 (1)))
        (%dst-pe (defstruct %dst-a3 (s 1 :bogus 2)))
        (%dst-pe (defstruct %dst-a4 (s 1 :type)))
        (%dst-pe (defstruct %dst-a5 (s 1 :read-only)))
        (%dst-pe (defstruct (%dst-a6 :bogus)))
        (%dst-pe (defstruct (%dst-a7 (:bogus))))
        (%dst-pe (defstruct (%dst-a8 (:conc-name a b))))
        (%dst-pe (defstruct (%dst-a9 (:conc-name 1))))
        (%dst-pe (defstruct (%dst-a10 (:copier 1))))
        (%dst-pe (defstruct (%dst-a11 (:copier a b))))
        (%dst-pe (defstruct (%dst-a12 (:predicate 1))))
        (%dst-pe (defstruct (%dst-a13 (:predicate a b))))
        (%dst-pe (defstruct (%dst-a14 (:include))))
        (%dst-pe (defstruct (%dst-a15 (:constructor 1))))
        (%dst-pe (defstruct (%dst-a16 (:constructor mk a b))))
        (%dst-pe (defstruct (%dst-a17 (:type))))
        (%dst-pe (defstruct (%dst-a18 (:named x))))
        (%dst-pe (defstruct (%dst-a19 (:type list) (:initial-offset))))
        (%dst-pe (defstruct (%dst-a20 (:type list) (:initial-offset -1))))
        (%dst-pe (defstruct (%dst-a21 (:type list) (:initial-offset a)))))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error))

(deftest defstruct-shape.valid
  (list (progn (eval '(defstruct %dst-b1 "doc" a (b 1 :type integer :read-only t))) (%dst-b1-b (make-%dst-b1)))
        (progn (eval '(defstruct (%dst-b2 (:conc-name nil) (:copier nil) (:predicate nil)
                                          (:constructor mk-b2 (x)) (:print-function nil))
                       x))
               (x (mk-b2 3)))
        (progn (eval '(defstruct (%dst-b3 (:include %dst-b1 (a 5)) :named (:conc-name %dst-b3-)))) (%dst-b3-a (make-%dst-b3)))
        (progn (eval '(defstruct (%dst-b4 (:type list) :named (:initial-offset 1)) x)) (length (make-%dst-b4)))
        (progn (eval '(defstruct (%dst-b5 (:constructor)))) (typep (make-%dst-b5) '%dst-b5)))
  (1 3 5 3 t))
