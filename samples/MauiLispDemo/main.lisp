;;; main.lisp — Entry point for the MauiLispDemo sample.
;;;
;;; Step 8 / D797: REPL-style demo.
;;;
;;; - Snippet is a Lisp-defined class with Title + Source properties.
;;; - MainVM exposes:
;;;     Snippets              — ObservableCollection<object> of Snippet
;;;     SelectedSnippet        — two-way bound to CollectionView.SelectedItem
;;;     Result                 — string displayed below the Evaluate button
;;;     EvaluateCommand        — ICommand; D797 just echoes the source,
;;;                              actual (eval) comes in D798
;;; - MainPage is a ContentPage whose XAML (embedded resource) has a
;;;   CollectionView + Editor + Button + Result Label. Editor is bound to
;;;   SelectedSnippet.Source so selecting a row shows its source.

(in-package :cl-user)

(format *error-output* "[main.lisp] loading in package ~S~%" *package*)


(require :dotnet-class)

;; Type aliases must take effect at compile time too: dotnet:define-class
;; resolves short names while macroexpanding, so a plain SETF/SETF GETHASH
;; would only run at load time and the macros below would fail to look up
;; CONTENTPAGE / IENUMERABLE during compile-file. eval-when makes the
;; registration happen in both compile and load phases.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash "CONTENTPAGE" dotnet::*type-aliases*)
        "Microsoft.Maui.Controls.ContentPage")
  (setf (gethash "IENUMERABLE" dotnet::*type-aliases*)
        "System.Collections.IEnumerable"))

;; -------------------------------------------------------------------------
;; LispCommand — ICommand whose Execute body funcalls a Lisp lambda stored
;; in the Action field (D792). Unchanged from earlier steps.

(dotnet:define-class "MauiLispDemo.LispCommand" (Object)
  (:implements ICommand)
  (:events ("CanExecuteChanged" EventHandler))
  (:fields ("Action" Object))
  (:methods
    ("CanExecute" ((param Object)) :returns Boolean
      (declare (ignore param)) t)
    ("Execute" ((param Object)) :returns Void
      (let ((action (dotnet:invoke self "Action")))
        (when action (funcall action param))))))

;; -------------------------------------------------------------------------
;; Snippet — a row in the CollectionView. Title is the list-view label,
;; Source is the body shown (and editable) in the Editor.

(dotnet:define-class "MauiLispDemo.Snippet" (Object)
  (:properties
    ("Title" String)
    ("Source" String)))

(defun %make-snippet (title source)
  (let ((s (dotnet:new "MauiLispDemo.Snippet")))
    (dotnet:%set-invoke s "Title" title)
    (dotnet:%set-invoke s "Source" source)
    s))

;; Button ラベルのトグル候補 (D799/D800). 押すたび 1 歩進む。
(defparameter *evaluate-button-labels*
  '("Evaluate" "評価" "평가" "Evaluar" "Evaluasi"))

;; D800: snippet が評価時に走るときに VM を触れるように特殊変数で公開。
;; MainVM の ctor で (setq *main-vm* self) される。snippet からは
;; (dotnet:%set-invoke *main-vm* "EvaluateButtonText" ...) 等で UI を
;; 直接書き換えられる。
(defvar *main-vm* nil)

;; -------------------------------------------------------------------------
;; Evaluator helper — read every top-level form from SOURCE (a string) and
;; eval it. Returns the value of the last form (nil if SOURCE is empty).
;; Any read-time or eval-time error propagates; callers wrap with handler-case.

(defun %eval-all-forms (source)
  ;; snippet の read/eval は常に cl-user で行う。デフォルトの *package*
  ;; が何かに切り替わっていると `*main-vm*` のような unqualified symbol
  ;; が別 package に intern されて defvar と別物になってしまう (D800
  ;; で発覚、snippet が UI を書き換えない原因)。
  (let* ((*package* (find-package :cl-user))
         (input (make-string-input-stream source))
         (last-value nil)
         (eof (gensym "EOF-")))
    (unwind-protect
         (loop for form = (read input nil eof)
               until (eq form eof)
               do (setf last-value (eval form)))
      (close input))
    last-value))


;; -------------------------------------------------------------------------
;; MainVM — Snippets / SelectedSnippet / Result / EvaluateCommand.

(dotnet:define-class "MauiLispDemo.MainVM" (Object)
  (:implements INotifyPropertyChanged)
  (:events ("PropertyChanged" PropertyChangedEventHandler))
  (:fields
    ("LabelIndex" Int32))
  (:properties
    ("Snippets" IEnumerable)
    ("SelectedSnippet" Object :notify t)
    ("Result" String :notify t)
    ("EvaluateButtonText" String :notify t)
    ("EvaluateCommand" ICommand)
    ("ToggleLanguageCommand" ICommand)
    ("RunCommand" ICommand))
  (:ctor ()
    ;; snippet からアクセスできるように global に自身を晒す。
    (setq *main-vm* self)
    (let ((snippets (dotnet:static "MauiLispDemo.XamlHelper"
                                   "NewObservableCollection"))
          (vm self))
      (dotnet:invoke snippets "Add"
                     (%make-snippet "Hello"
                                    "(format nil \"hello from lisp\")"))
      (dotnet:invoke snippets "Add"
                     (%make-snippet "Arithmetic"
                                    "(+ 1 2 3 4 5 6 7 8 9 10)"))
      (dotnet:invoke snippets "Add"
                     (%make-snippet "Factorial"
                                    (format nil "(defun fact (n)~%  (if (<= n 1) 1~%      (* n (fact (- n 1)))))~%(fact 10)")))
      (dotnet:invoke snippets "Add"
                     (%make-snippet
                      "Rename Evaluate button"
                      (format nil ";; snippet が UI を書き換えるデモ。~%;; 評価すると Evaluate ボタンのラベルが書き換わる。~%;; 🌐 を押せば次の言語で cycle 再開。~%(dotnet:%set-invoke *main-vm* \"EvaluateButtonText\"~%                    (format nil \"🎉 ~~D\" (random 1000)))")))
      ;; Live-coding (#162) 用の defun サンプル。Evaluate で関数を定義、
      ;; 次に Run ボタンでその定義が走る。書き換えて Evaluate → Run で
      ;; 新挙動が見える。
      (dotnet:invoke snippets "Add"
                     (%make-snippet
                      "Define my-click (greet)"
                      (format nil "(defun my-click ()~%  (dotnet:%set-invoke *main-vm* \"Result\"~%                      \"hello from a live-defined my-click\"))")))
      (dotnet:invoke snippets "Add"
                     (%make-snippet
                      "Define my-click (counter)"
                      (format nil "(unless (boundp '*click-count*)~%  (defvar *click-count* 0))~%(defun my-click ()~%  (incf *click-count*)~%  (dotnet:%set-invoke *main-vm* \"Result\"~%                      (format nil \"my-click has fired ~~D time(s)\" *click-count*)))")))
      (dotnet:%set-invoke self "Snippets" snippets)
      (dotnet:%set-invoke self "Result" "")
      (dotnet:%set-invoke self "LabelIndex" 0)
      (dotnet:%set-invoke self "EvaluateButtonText"
                          (first *evaluate-button-labels*))
      (let ((eval-cmd (dotnet:new "MauiLispDemo.LispCommand"))
            (toggle-cmd (dotnet:new "MauiLispDemo.LispCommand"))
            (run-cmd (dotnet:new "MauiLispDemo.LispCommand")))
        (dotnet:%set-invoke eval-cmd "Action"
                            (lambda (param)
                              (declare (ignore param))
                              (dotnet:invoke vm "RunEvaluate")))
        (dotnet:%set-invoke toggle-cmd "Action"
                            (lambda (param)
                              (declare (ignore param))
                              (dotnet:invoke vm "CycleLanguage")))
        (dotnet:%set-invoke run-cmd "Action"
                            (lambda (param)
                              (declare (ignore param))
                              (dotnet:invoke vm "RunHook")))
        (dotnet:%set-invoke self "EvaluateCommand" eval-cmd)
        (dotnet:%set-invoke self "ToggleLanguageCommand" toggle-cmd)
        (dotnet:%set-invoke self "RunCommand" run-cmd))))
  (:methods
    ("RunEvaluate" () :returns Void
      ;; D798: read → eval → prin1. Every read/eval error is caught so the
      ;; Button press can never crash the window; the message surfaces in
      ;; the Result label instead.
      (let* ((snip (dotnet:invoke self "SelectedSnippet"))
             (src (if snip (dotnet:invoke snip "Source") "")))
        (dotnet:%set-invoke
         self "Result"
         (handler-case
             (prin1-to-string (%eval-all-forms src))
           (error (c)
             (format nil "ERROR (~A):~%~A" (type-of c) c))))))
    ("CycleLanguage" () :returns Void
      ;; D799: Evaluate ボタンのラベルを *evaluate-button-labels* で
      ;; cycle する。`:notify t` 付き EvaluateButtonText の setter が
      ;; PropertyChanged を発火し、XAML Button.Text が追従する。
      (let* ((idx (dotnet:invoke self "LabelIndex"))
             (next (mod (1+ idx) (length *evaluate-button-labels*))))
        (dotnet:%set-invoke self "LabelIndex" next)
        (dotnet:%set-invoke self "EvaluateButtonText"
                            (nth next *evaluate-button-labels*))))
    ("RunHook" () :returns Void
      ;; Live-coding hook (#162): call CL-USER::MY-CLICK if defined.
      ;; Snippet で `(defun my-click ...)` 評価 → この Run ボタンで実行。
      ;; Editor で書き換えて再評価すれば次の Run から新定義で動く。
      (let ((sym (find-symbol "MY-CLICK" :cl-user)))
        (if (and sym (fboundp sym))
            (handler-case
                (progn (funcall sym) nil)
              (error (c)
                (dotnet:%set-invoke self "Result"
                                    (format nil "MY-CLICK ERROR (~A):~%~A"
                                            (type-of c) c))))
            (dotnet:%set-invoke self "Result"
                                "my-click is not defined yet. Evaluate a snippet that (defun my-click ...)."))))))

;; -------------------------------------------------------------------------
;; MainPage — ContentPage subclass. Ctor pulls the XAML from the assembly's
;; manifest resources and binds a MainVM.

(dotnet:define-class "MauiLispDemo.MainPage" (ContentPage)
  (:ctor ()
    (let ((xaml (dotnet:static "MauiLispDemo.XamlHelper" "ReadEmbeddedXaml"
                               "MauiLispDemo.MainPage.xaml")))
      (dotnet:static "MauiLispDemo.XamlHelper" "LoadFromXaml" self xaml))
    (let ((vm (dotnet:new "MauiLispDemo.MainVM")))
      (dotnet:%set-invoke self "BindingContext" vm))))

(defun build-main-page ()
  "Instantiate MauiLispDemo.MainPage. The ctor body (D783) already applied
   the XAML and set up the BindingContext, so the caller (App.CreateWindow)
   can wrap this directly in a Window."
  (dotnet:new "MauiLispDemo.MainPage"))

(format *error-output* "[main.lisp] build-main-page defined: ~S~%"
        (fboundp 'build-main-page))
