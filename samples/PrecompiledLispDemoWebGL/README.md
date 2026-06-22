# PrecompiledLispDemoWebGL

*Precompiled dotcl + an emit-free evaluator running in the browser — Unity **IL2CPP WebGL**.*

> **Status: working.** `build.sh` produces a Unity IL2CPP WebGL bundle that runs
> in the browser — verified end-to-end. **Precompiled Common Lisp draws an
> animated curve** (every point computed by Lisp each frame), and a **browser
> input box evaluates Lisp live** to reshape it — emit-free, no server, no
> recompile. Getting here needed three dotcl↔Unity fixes, all upstream now: a
> JSON-free runtime build (no `System.Text.Json`), retargeting each fasl's corlib
> reference from `System.Private.CoreLib` to the portable `netstandard` facade
> (`:corlib-name "netstandard"`), and making the compiler emit IL2CPP-clean
> (verifiable, non-covariant) calls.

The headline of the **shippable runtime** goal (see
[`DESIGN.md`](../../DESIGN.md)): dotcl in a web page, with no server and no
codegen. Unity's WebGL scripting backend is **IL2CPP only**, so the build is fully
ahead-of-time compiled — `IsDynamicCodeSupported` is `False` and `Reflection.Emit`
does not exist — yet it runs precompiled Lisp **and** evaluates new code at run
time through the tree-walk interpreter. Same proof as
[`PrecompiledLispDemoAot`](../PrecompiledLispDemoAot) (NativeAOT), shipped to the
browser instead of a native binary.

## What you see / can do

A Lissajous curve, every point computed by the precompiled Lisp `px`/`py` (in
`app.lisp`) each frame. The curve's shape lives in global special variables, so
typing Lisp in the input box changes it on the next frame — interpreted, no
recompile:

- `(setf *fx* 7)` / `(setf *fy* 5)` — change the curve's lobe ratio
- `(setf *amp* 0.5)` — shrink it; `(setf *spin* 4)` — speed up the drift
- `(defun px (i n tick) (* *amp* (cos (* *fx* (/ (* 2 pi i) n)))))` — redefine the
  drawing function whole; the animation immediately uses the interpreted version

The browser → Lisp path is `unityInstance.SendMessage('Bootstrap', 'EvalFromJs',
code)` → `DotclHost.EvalString` (the emit-free evaluator); results and errors go to
the on-page panel.

## How it works

Identical build-time-link strategy to the NativeAOT sample:

- Three managed assemblies sit in `Assets/Plugins/` and IL2CPP bakes them into the
  WebGL build:
  - `DotCL.Runtime.dll` — the emit-free **netstandard2.0** runtime build.
  - `dotclcore.dll` — the runtime core (compiler + stdlib), from the dev SIL core.
  - `appfasl.dll` — this app's Lisp, from `app.lisp`.
- **Stable assembly names** (`compile-file :module-name`, the 3rd `sil-to-fasl`
  arg) with **file name == internal assembly name**, so the host can resolve them
  by `Assembly.Load(name)` under IL2CPP — never `Assembly.LoadFrom`, which is
  unavailable here just like under NativeAOT.
- `DemoBootstrap` (a `MonoBehaviour`) boots them with
  `DotclHost.RunLinkedModuleByName("dotclcore" / "appfasl")` and runs the demo,
  mirroring output to the browser console and an on-page `<pre>` via the tiny
  `Assets/Plugins/DotclWebGL.jslib` bridge.
- `Assets/link.xml` keeps the three assemblies whole so IL2CPP's managed-code
  stripper doesn't remove the reflected `CompiledModule.ModuleInit` (the IL2CPP
  analogue of `<TrimmerRootAssembly>`).

## Build

Needs Unity `6000.4.4f1` with the **WebGL Build Support** module (IL2CPP +
bundled emscripten). The build driver is a POSIX shell script — on **Windows** run
it from **git-bash** (bundled with [Git for Windows](https://gitforwindows.org/));
there is no `.bat`/`.cmd` entry point. One script stages the three plugins, then
runs a headless Unity WebGL build:

```sh
./build.sh          # Linux / macOS / Windows (git-bash)
# -> Build/index.html + Build/Build/*.unityweb,*.js   (see build.log)
```

`build.sh` reads the editor version from `ProjectSettings/ProjectVersion.txt` and
locates Unity at the standard Unity Hub path for the OS; override a non-standard
install with `UNITY=/path/to/Unity ./build.sh`. The only per-OS difference is the
Unity executable path — the `dotnet` plugin build and the Unity `-batchmode`
invocation are identical everywhere.

First, `prepare-plugins.sh` builds the netstandard2.0 runtime and
precompiles `app.lisp` + the dev SIL core into the stable-named plugins. (Requires
`compiler/cil-out.sil` — build it once from the repo root with `make
cross-compile`.)

## Run / publish

`.wasm` won't load over `file://`; serve `Build/` over HTTP, then open the URL in
a browser. The build enables **Decompression Fallback**
(`PlayerSettings.WebGL.decompressionFallback`), so the JS loader gunzips the
compressed assets client-side and **any** static server works — no
`Content-Encoding: gzip` header required. A plain `python -m http.server`, a
`gh-pages` branch, or a Lisp `lack.app.directory` (e.g. `roswell/http.server`) all
serve it as-is:

```sh
cd Build && python -m http.server 8080   # or: ros install roswell/http.server && http.server
# open http://localhost:8080
```

(Without Decompression Fallback, a Gzip build instead needs the server to send
`Content-Encoding: gzip` for the assets.)

For the headline URL, publish the contents of `Build/` to any static host
(e.g. a `gh-pages` branch). Publishing to a public site is a manual, explicitly
authorized step — this sample only produces the build locally.

## Expected output (on the page / in the console)

```
IsDynamicCodeSupported = False
Core booted (build-time linked, no Assembly.LoadFrom).
App fasl linked.
fib(20)          = 6765
sum-squares(100) = 328350
    [C# host-log] hello dotcl, from precompiled Lisp
greet len        = 5
Emit-free eval (IsDynamicCodeSupported=False):
  (+ 1 2)                       = 3
  (loop for i to 4 collect i)   = (0 1 2 3 4)
  (fib 10)  [calls compiled fn] = 55
  defun cube; (cube 5)          = 125
  defmacro twice; (twice ..)    = 54
  defclass/defmethod; (sq ..)   = 49

Done — precompiled Lisp + emit-free eval (defun/defmacro/CLOS) in a Unity WebGL/IL2CPP build, no Reflection.Emit.
```
