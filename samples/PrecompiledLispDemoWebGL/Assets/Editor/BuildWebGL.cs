using System.Collections.Generic;
using System.Linq;
using UnityEditor;
using UnityEditor.Build;
using UnityEditor.Build.Reporting;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.SceneManagement;

// Headless WebGL build entry point. Invoked by build.sh:
//   Unity.exe -batchmode -nographics -quit -projectPath . \
//             -buildTarget WebGL -executeMethod BuildWebGL.Build
//
// Creates a one-object scene whose GameObject carries DemoBootstrap, forces the
// IL2CPP backend (WebGL is IL2CPP-only anyway, but we set it explicitly), and
// builds into ./Build. Exits non-zero on failure so CI / the bat can detect it.
public static class BuildWebGL
{
    public static void Build()
    {
        PlayerSettings.SetScriptingBackend(NamedBuildTarget.WebGL, ScriptingImplementation.IL2CPP);
        // Smallest output: no exceptions table, brotli-free, gzip is fine for a demo.
        PlayerSettings.WebGL.compressionFormat = WebGLCompressionFormat.Gzip;
        // Decompression fallback: the JS loader gunzips the .gz assets client-side,
        // so the build runs on ANY static server — including ones that don't set
        // Content-Encoding: gzip (e.g. a plain Lisp lack.app.directory / Python
        // http.server). Without this, the server must send that header itself.
        PlayerSettings.WebGL.decompressionFallback = true;
        // Custom page with the Lisp input box (SendMessage → DemoBootstrap.EvalFromJs).
        PlayerSettings.WebGL.template = "PROJECT:dotcl";

        // The LineRenderer material uses Shader.Find("Sprites/Default") at run time;
        // in a code-only build that shader isn't referenced by any asset, so force
        // it into the build via Always-Included Shaders. Without this the curve
        // renders with a missing-shader (magenta) material or not at all.
        EnsureAlwaysIncludedShader("Sprites/Default");

        // Build a scene programmatically so we don't hand-author a .unity YAML file.
        var scene = EditorSceneManager.NewScene(NewSceneSetup.EmptyScene, NewSceneMode.Single);
        var go = new GameObject("Bootstrap");
        go.AddComponent<DemoBootstrap>();
        const string scenePath = "Assets/DemoScene.unity";
        EditorSceneManager.SaveScene(scene, scenePath);

        var options = new BuildPlayerOptions
        {
            scenes = new[] { scenePath },
            locationPathName = "Build",
            target = BuildTarget.WebGL,
            options = BuildOptions.None,
        };

        BuildReport report = BuildPipeline.BuildPlayer(options);
        BuildSummary summary = report.summary;
        Debug.Log($"[BuildWebGL] result={summary.result} size={summary.totalSize} errors={summary.totalErrors}");
        if (summary.result != BuildResult.Succeeded)
            EditorApplication.Exit(1);
        EditorApplication.Exit(0);
    }

    // Add a built-in shader to GraphicsSettings' Always-Included Shaders list so a
    // runtime Shader.Find for it resolves in the player (it would otherwise be
    // stripped, since no asset references it).
    private static void EnsureAlwaysIncludedShader(string shaderName)
    {
        var shader = Shader.Find(shaderName);
        if (shader == null) { Debug.LogWarning($"[BuildWebGL] shader not found: {shaderName}"); return; }

        var graphicsSettings = AssetDatabase
            .LoadAllAssetsAtPath("ProjectSettings/GraphicsSettings.asset")
            .FirstOrDefault();
        if (graphicsSettings == null) { Debug.LogWarning("[BuildWebGL] GraphicsSettings not found"); return; }

        var so = new SerializedObject(graphicsSettings);
        var arr = so.FindProperty("m_AlwaysIncludedShaders");

        for (int i = 0; i < arr.arraySize; i++)
            if (arr.GetArrayElementAtIndex(i).objectReferenceValue == shader)
                return; // already present

        int idx = arr.arraySize;
        arr.InsertArrayElementAtIndex(idx);
        arr.GetArrayElementAtIndex(idx).objectReferenceValue = shader;
        so.ApplyModifiedProperties();
        AssetDatabase.SaveAssets();
        Debug.Log($"[BuildWebGL] added always-included shader: {shaderName}");
    }
}
