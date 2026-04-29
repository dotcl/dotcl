;;; main.lisp — MonoGame Game subclass written in Lisp.
;;;
;;; Demonstrates `dotnet:define-class` against the MonoGame `Game` class.
;;; Update + Draw are overridden to animate the background color over
;;; time. main.lisp is compiled into MonoGameLispDemo.fasl by the
;;; project-core build target (#166); main.lisp itself doesn't ship at
;;; runtime.

(in-package :cl-user)

(format *error-output* "[main.lisp] loading in package ~S~%" *package*)

(require :dotnet-class)

;; Type aliases visible at compile-time too: dotnet:define-class resolves
;; short names while macroexpanding, so eval-when keeps the registration
;; effective in both compile and load phases.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash "GAME" dotnet::*type-aliases*)
        "Microsoft.Xna.Framework.Game")
  (setf (gethash "GAMETIME" dotnet::*type-aliases*)
        "Microsoft.Xna.Framework.GameTime")
  (setf (gethash "GRAPHICSDEVICEMANAGER" dotnet::*type-aliases*)
        "Microsoft.Xna.Framework.GraphicsDeviceManager"))

;; Animated background color: hue cycles with elapsed time.
(defun pulse-color (seconds)
  "Return Color RGB cycling through hues over a 6-second period."
  (let* ((t-norm (mod seconds 6.0))
         (phase (floor t-norm 2.0))
         (frac  (mod t-norm 2.0))
         (a (round (* 255 (- 1.0 (abs (- 1.0 frac))))))
         (b (round (* 255 (abs (- 1.0 frac)))))
         (r 0) (g 0) (bl 0))
    (case phase
      (0 (setf r b g a bl 0))      ; red→green
      (1 (setf r 0 g b bl a))      ; green→blue
      (2 (setf r a g 0 bl b)))     ; blue→red
    (dotnet:new "Microsoft.Xna.Framework.Color" r g bl)))

;; Demo.LispGame: subclass of Game. The constructor must instantiate a
;; GraphicsDeviceManager(this) — its mere construction registers it on
;; the Game so GraphicsDevice gets initialized later.
(dotnet:define-class "Demo.LispGame" (Game)
  (:ctor ()
    (dotnet:new "Microsoft.Xna.Framework.GraphicsDeviceManager" self))
  (:methods
    ("Draw" ((gt GameTime)) :returns Void :override t
      (let* ((gd (dotnet:invoke self "GraphicsDevice"))
             (total (dotnet:invoke gt "TotalGameTime"))
             (secs (dotnet:invoke total "TotalSeconds"))
             (c (pulse-color secs)))
        (dotnet:invoke gd "Clear" c)))))

(defun make-game ()
  "Instantiate Demo.LispGame for Program.cs to Run()."
  (dotnet:new "Demo.LispGame"))

(format *error-output* "[main.lisp] make-game defined: ~S~%"
        (fboundp 'make-game))
