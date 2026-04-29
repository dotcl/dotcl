using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using DotCL;
using DotCL.Emitter;

namespace DotCL.Contrib.DotclCs;

/// <summary>
/// In-memory C# → CIL S-expression disassembler, backing the dotcl-cs
/// contrib. Replaces the previous dotnet-CLI-subprocess path (runtime's
/// CsBuilder.cs, removed in C2).
///
/// Compile flow:
///   CSharpSyntaxTree.ParseText(body)
///     → CSharpCompilation.Create(... dynamic library ...).Emit(MemoryStream)
///     → Assembly.Load(bytes)
///     → first public static method → IlDisasm.DisassembleMethod
///
/// No temp files, no subprocess. ~50ms warm per call (was ~2.7s via dotnet
/// build CLI).
/// </summary>
public static class RoslynCompiler
{
    // Cached — trusted references for "normal" C# inline snippets.
    // Minimal: mscorlib so the typical `System.Math.Sin` etc. resolves.
    private static MetadataReference[]? _references;

    private static MetadataReference[] References()
    {
        if (_references != null) return _references;
        // Pull references from every assembly currently loaded that has a
        // non-dynamic Location. Good enough for user snippets that reference
        // System.* types — and covers the DotCL runtime too if a snippet ever
        // wants to interop there.
        var refs = new List<MetadataReference>();
        foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
        {
            if (a.IsDynamic) continue;
            string? loc = null;
            try { loc = a.Location; } catch { continue; }
            if (!string.IsNullOrEmpty(loc) && File.Exists(loc))
            {
                try { refs.Add(MetadataReference.CreateFromFile(loc)); } catch { }
            }
        }
        _references = refs.ToArray();
        return _references;
    }

    /// <summary>
    /// Compile BODY (a C# string containing `public static` method definitions
    /// inside an implicit DotclInlineCs class) and return the first public
    /// static method's IL disassembly as a dotcl-SIL-shaped S-expression list.
    /// Throws LispErrorException with accumulated diagnostics on compile
    /// failure.
    /// </summary>
    public static LispObject CompileAndDisassemble(string body)
    {
        var source = $"using System;\npublic static class DotclInlineCs {{\n{body}\n}}\n";
        var tree = CSharpSyntaxTree.ParseText(source);
        var options = new CSharpCompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            optimizationLevel: OptimizationLevel.Release);
        var compilation = CSharpCompilation.Create(
            "DotclInlineCs_" + Guid.NewGuid().ToString("N").Substring(0, 8),
            syntaxTrees: new[] { tree },
            references: References(),
            options: options);

        using var ms = new MemoryStream();
        var result = compilation.Emit(ms);
        if (!result.Success)
        {
            var errors = result.Diagnostics
                .Where(d => d.Severity == DiagnosticSeverity.Error)
                .Select(d => d.ToString())
                .ToArray();
            throw new LispErrorException(new LispError(
                "DOTCL-CS: C# compile failed:\n" + string.Join("\n", errors)));
        }

        var bytes = ms.ToArray();
        var asm = Assembly.Load(bytes);
        var t = asm.GetType("DotclInlineCs")
            ?? throw new LispErrorException(new LispError(
                "DOTCL-CS: DotclInlineCs class not found in compiled body"));
        var methods = t.GetMethods(BindingFlags.Public | BindingFlags.Static
            | BindingFlags.DeclaredOnly);
        if (methods.Length == 0)
            throw new LispErrorException(new LispError(
                "DOTCL-CS: no public static methods found in body"));

        return IlDisasm.DisassembleMethod(methods[0]);
    }
}
