using Microsoft.Maui.Controls;
using Microsoft.Maui.Controls.Xaml;
using System.Collections.ObjectModel;

namespace MauiLispDemo;

/// <summary>
/// Small non-generic wrappers around MAUI's generic XAML helpers. dotcl's
/// dotnet:static / dotnet:invoke don't know how to construct generic method
/// instances at runtime, so we trampoline through type-erased entry points
/// that take BindableObject.
/// </summary>
public static class XamlHelper
{
    /// <summary>
    /// Apply a XAML string to an existing BindableObject (page, view, etc.).
    /// Equivalent to the generic `Extensions.LoadFromXaml&lt;T&gt;(view, xaml)`.
    /// Returns the same view for fluent chaining (though Lisp side usually
    /// ignores the return).
    /// </summary>
    public static BindableObject LoadFromXaml(BindableObject view, string xaml)
        => Microsoft.Maui.Controls.Xaml.Extensions.LoadFromXaml(view, xaml);

    /// <summary>
    /// Read a XAML resource embedded in the MauiLispDemo assembly (via csproj
    /// &lt;EmbeddedResource&gt;) and return its contents as a string. Used by
    /// Lisp ctors to avoid sidecar .xaml files in the output directory;
    /// callers pass the resource's logical name (e.g.
    /// "MauiLispDemo.MainPage.xaml").
    /// </summary>
    public static string ReadEmbeddedXaml(string resourceName)
    {
        var asm = typeof(XamlHelper).Assembly;
        using var stream = asm.GetManifestResourceStream(resourceName)
            ?? throw new System.IO.FileNotFoundException(
                $"XAML resource not found in {asm.GetName().Name}: {resourceName}. " +
                $"Check csproj <EmbeddedResource>/<LogicalName>.");
        using var reader = new System.IO.StreamReader(stream);
        return reader.ReadToEnd();
    }

    /// <summary>
    /// Construct an empty <c>ObservableCollection&lt;object&gt;</c>. dotcl's
    /// dotnet:new doesn't know how to spell a generic type instantiation at
    /// runtime, so callers go through this non-generic trampoline. The
    /// collection is returned via the non-generic IList surface so it
    /// participates in CollectionView.ItemsSource binding (MAUI only requires
    /// IEnumerable + optional INotifyCollectionChanged, both of which are
    /// satisfied here).
    /// </summary>
    public static ObservableCollection<object> NewObservableCollection()
        => new ObservableCollection<object>();

    /// <summary>
    /// Append a line to dotcl-maui.log so Lisp-side diagnostics are visible
    /// even though the MAUI GUI process swallows stderr. The log file sits
    /// next to the exe (AppContext.BaseDirectory). Called from Lisp via
    /// (dotnet:static "MauiLispDemo.XamlHelper" "LogLine" "message").
    /// </summary>
    public static void LogLine(string message)
    {
        try
        {
            var log = System.IO.Path.Combine(System.AppContext.BaseDirectory,
                                             "dotcl-maui.log");
            var line = $"{System.DateTime.Now:HH:mm:ss.fff} [lisp] {message}"
                       + System.Environment.NewLine;
            System.IO.File.AppendAllText(log, line);
        }
        catch { /* best-effort */ }
    }
}
