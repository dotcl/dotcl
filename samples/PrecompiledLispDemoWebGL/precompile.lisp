;;;; Build-time only. Produces the two STABLE-NAMED .NET IL assemblies that Unity
;;;; IL2CPP links into the WebGL build, written straight into Assets/Plugins:
;;;;   appfasl.dll   — this app's Lisp (compile-file :module-name "appfasl")
;;;;   dotclcore.dll — the runtime core (compiler+stdlib), from the dev SIL core
;;;;                   (sil-to-fasl ... "dotclcore")
;;;;
;;;; Stable names + file name == internal assembly name are required so the host's
;;;; Assembly.Load(name) (DotclHost.RunLinkedModuleByName) resolves them under
;;;; IL2CPP, exactly as for NativeAOT (see samples/PrecompiledLispDemoAot).
;;;; The dotcl compiler used here itself uses Reflection.Emit — the dev/build side.
;;;; The shipped WebGL binary never emits. Paths are relative to this sample dir.
;;;; :corlib-name "netstandard" rewrites each fasl's corlib reference from
;;;; System.Private.CoreLib to the portable netstandard facade, which Unity's
;;;; IL2CPP BCL provides (System.Private.CoreLib it does not). Without this the
;;;; fasls fail to load under IL2CPP.
(compile-file "app.lisp" :output-file "Assets/Plugins/appfasl.dll" :module-name "appfasl" :corlib-name "netstandard")
(dotcl:sil-to-fasl "../../compiler/cil-out.sil" "Assets/Plugins/dotclcore.dll" "dotclcore" "netstandard")
(dotcl:quit 0)
