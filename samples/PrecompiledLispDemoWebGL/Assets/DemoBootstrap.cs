using System;
using System.Runtime.InteropServices;
using UnityEngine;
using DotCL;

// DemoBootstrap — the WebGL headline: an animated curve whose every point is
// computed by precompiled Common Lisp each frame, running inside a Unity IL2CPP
// WebGL build (IsDynamicCodeSupported = False, no Reflection.Emit). A browser
// input box EVALs Lisp at run time — e.g. (setf *fx* 7) or a whole (defun px ...)
// — and the curve changes live, through the tree-walk interpreter, no recompile.
//
// Lisp drives the visuals: Update() calls the precompiled PX/PY/HUE (in appfasl,
// baked into the build) per point; EvalFromJs() routes browser input through the
// emit-free evaluator, mutating the special variables PX/PY read. The two Lisp
// images (dotclcore/appfasl) are referenced as fixed-name assemblies and booted
// by stable name via DotclHost.RunLinkedModuleByName (Assembly.Load on the
// already-linked assembly — never Assembly.LoadFrom, unavailable under IL2CPP).
public class DemoBootstrap : MonoBehaviour
{
#if UNITY_WEBGL && !UNITY_EDITOR
    [DllImport("__Internal")] private static extern void DotclLog(string message);
#else
    private static void DotclLog(string message) { }
#endif

    private static void Log(string message)
    {
        Debug.Log(message);   // browser console
        DotclLog(message);    // on-page output panel via DotclWebGL.jslib
    }

    private const int N = 48;            // points on the curve
    private LineRenderer _line;
    private readonly Vector3[] _pts = new Vector3[N];
    private bool _booted;
    private double _tick;

    private void Start()
    {
#if UNITY_WEBGL && !UNITY_EDITOR
        // By default the Unity WebGL canvas captures ALL keyboard input on the
        // page, so the HTML <textarea> next to it never receives keystrokes —
        // you can't type into the REPL. Turning this off makes Unity capture
        // keys only while the canvas itself is focused, letting the input box work.
        WebGLInput.captureAllKeyboardInput = false;
#endif

        DotclHost.Initialize();
        DotclHost.SetThrowingDebuggerHook();   // Lisp errors throw back to C#, not the debugger
        Log($"IsDynamicCodeSupported = {System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported}");

        // Boot the FASL core and the app image — build-time-linked, by stable name.
        DotclHost.RunLinkedModuleByName("dotclcore");
        DotclHost.Register("host-log", a => { Log($"    [Lisp] {a[0]}"); return null; });
        DotclHost.RunLinkedModuleByName("appfasl");
        Log("dotcl booted in the browser (build-time linked, emit-free).");

        // One-time proof the precompiled image runs and calls back into C#.
        Log($"fib(20) = {DotclHost.ToClr<long>(DotclHost.Call("FIB", 20))}");
        DotclHost.Call("GREET", "dotcl");

        SetupScene();
        _booted = true;
        Log("Curve is live. Try evaluating:  (setf *fx* 7)   (setf *amp* 0.5)   (setf *spin* 4)");
        Log("…or redefine it whole:  (defun px (i n tick) (* *amp* (cos (* *fx* (/ (* 2 pi i) n)))))");
    }

    private void SetupScene()
    {
        var camGo = new GameObject("Cam");
        var cam = camGo.AddComponent<Camera>();
        cam.orthographic = true;
        cam.orthographicSize = 1.15f;
        cam.transform.position = new Vector3(0, 0, -10);
        cam.clearFlags = CameraClearFlags.SolidColor;
        cam.backgroundColor = new Color(0.04f, 0.05f, 0.08f);

        _line = new GameObject("Curve").AddComponent<LineRenderer>();
        _line.material = new Material(Shader.Find("Sprites/Default"));
        _line.useWorldSpace = true;
        _line.loop = false;
        _line.widthMultiplier = 0.02f;
        _line.numCornerVertices = 2;
        _line.positionCount = N;
    }

    private void Update()
    {
        if (!_booted) return;
        _tick += 1.0;

        // Lisp computes every point this frame (precompiled → fast).
        for (int i = 0; i < N; i++)
        {
            double x = DotclHost.ToClr<double>(DotclHost.Call("PX", i, N, _tick));
            double y = DotclHost.ToClr<double>(DotclHost.Call("PY", i, N, _tick));
            _pts[i] = new Vector3((float)x, (float)y, 0f);
        }
        _line.SetPositions(_pts);

        double h = DotclHost.ToClr<double>(DotclHost.Call("HUE", _tick));
        Color c0 = Color.HSVToRGB((float)h, 0.8f, 1f);
        Color c1 = Color.HSVToRGB((float)((h + 0.4) % 1.0), 0.8f, 1f);
        _line.startColor = c0;
        _line.endColor = c1;
    }

    // Called from the browser (WebGL template) via unityInstance.SendMessage.
    // Evaluates a Lisp string with the emit-free interpreter and reports the
    // result (or the condition) to the on-page output panel.
    public void EvalFromJs(string code)
    {
        if (string.IsNullOrWhiteSpace(code)) return;
        try
        {
            var result = DotclHost.EvalString(code);
            Log($"> {code}");
            Log($"  => {result}");
        }
        catch (Exception e)
        {
            Log($"> {code}");
            Log($"  ! {e.Message}");
        }
    }
}
