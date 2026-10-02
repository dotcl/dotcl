;;; DOTNET:REGISTER-CLASS-HANDLERS: install the Lisp bodies of a type without
;;; defining the type.
;;;
;;; A facade saved by DOTNET:LIBRARY dispatches every method to a Lisp body
;;; looked up by (type full name, dispatch key). Saving does not register the
;;; bodies, so the process that loads the facade has to. Before this, the only
;;; way was to run DOTNET:DEFINE-CLASS again, which emits a throwaway type and
;;; cannot run at all on the emit-free runtime. Registering emits nothing, and
;;; the dispatch table and its entry points build without the emitter, so a
;;; facade loaded by the emit-free runtime reaches its bodies too.

(require :dotnet-class)

;;; The dispatch half is present on every build, emit-free included: register
;;; bodies, then call the entry point the facade IL calls, by hand.
(deftest register-class-handlers-dispatch-instance
  (progn
    (dotnet:register-class-handlers "DotclTest.RchDirect" ()
      (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b 100))))
    (dotnet:static "DotCL.Emitter.DynamicClassBuilder" "DispatchLispMethod"
                   "DotclTest.RchDirect" "Add#System.Int32|System.Int32"
                   (dotnet:static "System.Type" "GetType" "System.Int32")
                   nil
                   (dotnet:new-array "System.Object" 2 3)))
  105)

(deftest register-class-handlers-dispatch-static
  (progn
    (dotnet:register-class-handlers "DotclTest.RchDirectOps" ()
      (:functions ("Sq" ((x Int32)) :returns Int32 (* x x))))
    (dotnet:static "DotCL.Emitter.DynamicClassBuilder" "DispatchLispStatic"
                   "DotclTest.RchDirectOps" "Sq#System.Int32"
                   (dotnet:static "System.Type" "GetType" "System.Int32")
                   (dotnet:new-array "System.Object" 9)))
  81)

;;; A no-parameter method dispatches under its bare name.
(deftest register-class-handlers-dispatch-no-params
  (progn
    (dotnet:register-class-handlers "DotclTest.RchDirect2" ()
      (:methods ("Hello" () :returns String "hi")))
    (dotnet:static "DotCL.Emitter.DynamicClassBuilder" "DispatchLispMethod"
                   "DotclTest.RchDirect2" "Hello"
                   (dotnet:static "System.Type" "GetType" "System.String")
                   nil
                   (dotnet:new-array "System.Object")))
  "hi")

(deftest register-class-handlers-returns-full-name
  (dotnet:register-class-handlers "DotclTest.RchName" ()
    (:methods ("M" () :returns Int32 1)))
  "DotclTest.RchName")

(deftest register-class-handlers-rejects-non-function-body
  (handler-case
      (progn (dotnet:%register-class-handlers
              "DotclTest.RchBad" nil nil nil
              (list (list "M" "System.Int32" nil 42)))
             :no-error)
    (error () :error))
  :error)

;;; End to end, on a build that can save a facade: save it (which registers
;;; nothing, so the bodies given here are never seen), load it as an ordinary
;;; assembly, register the real bodies, and call through the facade's own IL --
;;; constructor, instance method and static function.
(defvar *rch-log* nil)

(deftest-emitting-only register-class-handlers-saved-facade
  (let ((path (concatenate 'string (regression-temp-dir) "/DotclRchPack.dll")))
    (dotnet:library ("DotclRchPack" :path path)
      (:class "DotclRchPack.Calc" ()
        (:ctor ((n Int32)) (declare (ignorable n)) nil)
        (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b 1000))))
      (:module "DotclRchPack.Ops"
        (:functions ("Sq" ((x Int32)) :returns Int32 (* x x 1000)))))
    (dotnet:load-assembly path)
    (dotnet:register-class-handlers "DotclRchPack.Calc" ()
      (:ctor ((n Int32)) (push n *rch-log*))
      (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
    (dotnet:register-class-handlers "DotclRchPack.Ops" ()
      (:functions ("Sq" ((x Int32)) :returns Int32 (* x x))))
    (let ((c (dotnet:new "DotclRchPack.Calc" 7)))
      (list (dotnet:invoke c "Add" 2 3)
            (dotnet:static "DotclRchPack.Ops" "Sq" 9)
            *rch-log*)))
  (5 81 (7)))

;;; The dispatch table is read by the generated methods on whatever thread
;;; .NET code calls them from, while another thread may register bodies for
;;; another type. It was a plain Dictionary, and a read during the resize a
;;; registration caused reported a registered body missing ("no Lisp handler
;;; registered"). Here one thread dispatches to a fixed body while this one
;;; registers many other types.
(require "dotcl-thread")

(defvar *rch-race-done* nil)

(deftest register-class-handlers-dispatch-while-registering
  (progn
    (dotnet:register-class-handlers "DotclTest.RchRaceFixed" ()
      (:functions ("Sq" ((x Int32)) :returns Int32 (* x x))))
    (setf *rch-race-done* nil)
    (let* ((int32 (dotnet:static "System.Type" "GetType" "System.Int32"))
           (errors 0)
           (calls 0)
           (reader (dotcl-thread:make-thread
                    (lambda ()
                      (loop until *rch-race-done*
                            do (handler-case
                                   (progn
                                     (dotnet:static "DotCL.Emitter.DynamicClassBuilder"
                                                    "DispatchLispStatic"
                                                    "DotclTest.RchRaceFixed" "Sq#System.Int32"
                                                    int32 (dotnet:new-array "System.Object" 3))
                                     (incf calls))
                                 (error () (incf errors))))))))
      (unwind-protect
           (let ((fn (lambda (x) (* x x))))
             (dotimes (i (emit-free-scale 100000 20000))
               (dotnet:%register-class-handlers
                (format nil "DotclTest.RchRace~a" i) nil nil nil
                (list (list "Sq" "System.Int32" (list "System.Int32") fn nil nil t (list "x")))
                nil nil nil nil nil nil nil)))
        (setf *rch-race-done* t)
        (dotcl-thread:thread-join reader))
      (list (plusp calls) errors)))
  (t 0))
