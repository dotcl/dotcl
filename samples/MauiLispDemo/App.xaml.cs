using DotCL;
using System.Diagnostics;

namespace MauiLispDemo;

public partial class App : Application
{
	public App()
	{
		InitializeComponent();
	}

	// Step 6c: the window's content Page is built entirely from Lisp.
	// Lisp defines MauiLispDemo.LispPage (inheriting ContentPage) via
	// dotnet:define-class, then BUILD-MAIN-PAGE instantiates it and fills
	// in the programmatic UI (Label etc.). We unwrap LispDotNetObject to
	// hand the raw Page to MAUI.
	protected override Window CreateWindow(IActivationState? activationState)
	{
		try
		{
			var result = DotclHost.Call("BUILD-MAIN-PAGE");
			if (result is LispDotNetObject dno && dno.Value is Page page)
			{
				LogLine($"[App] BUILD-MAIN-PAGE returned {dno.Value.GetType().FullName}");
				return new Window(page);
			}
			throw new InvalidOperationException(
				$"BUILD-MAIN-PAGE returned unexpected value: {result?.GetType().Name} ({result})");
		}
		catch (Exception ex)
		{
			LogLine($"[App] CreateWindow failed: {ex}");
			// Fallback so the window at least opens with the error text.
			return new Window(new ContentPage
			{
				Content = new Label
				{
					Text = $"[Lisp error] {ex.GetType().Name}: {ex.Message}",
					FontSize = 16,
					HorizontalOptions = LayoutOptions.Center,
					VerticalOptions = LayoutOptions.Center,
				},
			});
		}
	}

	private static void LogLine(string message)
	{
		var line = $"{DateTime.Now:HH:mm:ss.fff} {message}";
		try
		{
			var log = System.IO.Path.Combine(AppContext.BaseDirectory, "dotcl-maui.log");
			System.IO.File.AppendAllText(log, line + Environment.NewLine);
		}
		catch { }
		Debug.WriteLine(line);
	}
}
