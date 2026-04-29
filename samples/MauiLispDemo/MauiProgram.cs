using Microsoft.Extensions.Logging;
using DotCL;

namespace MauiLispDemo;

public static class MauiProgram
{
	public static MauiApp CreateMauiApp()
	{
		// Boot dotcl BEFORE constructing MAUI pages. The project-core flow
		// (#166) ships a single bundle dir alongside the app: dotcl.core,
		// the contrib dependency FASLs (just dotnet-class for this demo),
		// and MauiLispDemo.fasl (compiled from main.lisp at build time).
		// LoadFromManifest reads the manifest and loads them in order — so
		// after this call, MainVM, MainPage, BUILD-MAIN-PAGE etc. are all
		// defined and ready for App.CreateWindow to invoke.
		InitializeDotcl();

		var builder = MauiApp.CreateBuilder();
		builder.UseMauiApp<App>();

#if DEBUG
		builder.Logging.AddDebug();
#endif

		return builder.Build();
	}

	// Log file lives where we have write access:
	//   - Windows desktop: next to the exe (AppContext.BaseDirectory)
	//   - Android: FileSystem.AppDataDirectory (writable per-app dir)
	private static readonly string _logPath = System.IO.Path.Combine(
#if ANDROID
		Microsoft.Maui.Storage.FileSystem.AppDataDirectory,
#else
		AppContext.BaseDirectory,
#endif
		"dotcl-maui.log");

	private static void Log(string message)
	{
		var line = $"{DateTime.Now:HH:mm:ss.fff} {message}";
		try { System.IO.File.AppendAllText(_logPath, line + Environment.NewLine); } catch { }
		System.Diagnostics.Debug.WriteLine(line);
		Console.Error.WriteLine(line);
	}

	private static void InitializeDotcl()
	{
		try { System.IO.File.WriteAllText(_logPath, ""); } catch { }
		Log($"[dotcl] log file: {_logPath}");

		try
		{
			DotclHost.Initialize();
			Log("[dotcl] Initialize OK");

			// Force MAUI assemblies to load BEFORE the Lisp side runs, so
			// dotcl's ResolveDotNetType (which scans
			// AppDomain.CurrentDomain.GetAssemblies) can see MAUI types
			// referenced by the bundled Lisp code.
			_ = typeof(Microsoft.Maui.Controls.ContentPage).FullName;
			_ = typeof(Microsoft.Maui.Controls.Label).FullName;
			_ = typeof(Microsoft.Maui.Controls.LayoutOptions).FullName;
			Log("[dotcl] MAUI core assemblies forced-loaded");

			var manifestPath = ResolveManifestPath();
			Log($"[dotcl] manifest: {manifestPath}");

			var loaded = DotclHost.LoadFromManifest(manifestPath);
			Log($"[dotcl] LoadFromManifest loaded {loaded} fasls");

			// Probe: BUILD-MAIN-PAGE (defined in MauiLispDemo.fasl, compiled
			// from main.lisp) should be bound by now.
			try
			{
				var sym = Startup.Sym("BUILD-MAIN-PAGE");
				Log($"[dotcl] BUILD-MAIN-PAGE fboundp: {sym.Function != null}");
			}
			catch (Exception probeEx)
			{
				Log($"[dotcl] BUILD-MAIN-PAGE probe FAILED: {probeEx.GetType().Name}: {probeEx.Message}");
			}
		}
		catch (Exception ex)
		{
			Log($"[dotcl] InitializeDotcl failed: {ex}");
			// Don't rethrow — let the MAUI window come up so the user can at
			// least see the app started; the App.CreateWindow fallback will
			// surface errors.
		}
	}

#if ANDROID
	// On Android, fasls + manifest live inside the APK as MauiAssets
	// (added by Dotcl.targets). MauiAsset content is not directly readable
	// by File-based APIs, so extract it to FileSystem.AppDataDirectory.
	// The manifest is extracted first; then each fasl listed in it is
	// extracted by basename. Re-extracting every boot keeps dev-time fasl
	// updates fresh without manual cache wipes.
	private static string ResolveManifestPath()
	{
		var assetRoot = Microsoft.Maui.Storage.FileSystem.AppDataDirectory;
		var faslDir = System.IO.Path.Combine(assetRoot, "dotcl-fasl");
		System.IO.Directory.CreateDirectory(faslDir);

		var manifestPath = System.IO.Path.Combine(faslDir, "dotcl-deps.txt");
		ExtractAsset("dotcl-fasl/dotcl-deps.txt", manifestPath);

		foreach (var raw in System.IO.File.ReadAllLines(manifestPath))
		{
			var name = raw.Trim();
			if (name.Length == 0) continue;
			ExtractAsset($"dotcl-fasl/{name}", System.IO.Path.Combine(faslDir, name));
		}
		return manifestPath;
	}

	private static void ExtractAsset(string logicalName, string destPath)
	{
		try
		{
			using var src = Microsoft.Maui.Storage.FileSystem
				.OpenAppPackageFileAsync(logicalName).GetAwaiter().GetResult();
			using var dst = System.IO.File.Create(destPath);
			src.CopyTo(dst);
		}
		catch (Exception ex)
		{
			Log($"[dotcl] ExtractAsset({logicalName}) failed: {ex.Message}");
		}
	}
#else
	// Windows desktop: fasls + manifest are next to the exe via Dotcl.targets'
	// <None CopyToOutputDirectory>. Just point at the file directly.
	private static string ResolveManifestPath()
	{
		return System.IO.Path.Combine(
			AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
	}
#endif
}
