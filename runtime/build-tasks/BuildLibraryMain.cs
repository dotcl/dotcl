using System.Runtime.CompilerServices;

namespace DotCL.Build.Tasks;

/// <summary>
/// <c>dotnet DotCL.Build.Tasks.dll --runtime DLL --core CORE [--contrib DIR]
/// build-library ...</c>: the library build the SDK runs, in a process of its own.
/// A library build loads the project's references and defines the library's
/// types, neither of which can be undone, so it does not run inside a build node
/// that outlives it.
/// </summary>
internal static class BuildLibraryMain
{
    private static int Main(string[] args)
    {
        string? runtime = null, core = null, contrib = null;
        int i = 0;
        for (; i < args.Length && args[i] != "build-library"; i++)
        {
            if (args[i] == "--runtime" && i + 1 < args.Length) runtime = args[++i];
            else if (args[i] == "--core" && i + 1 < args.Length) core = args[++i];
            else if (args[i] == "--contrib" && i + 1 < args.Length) contrib = args[++i];
            else { Console.Error.WriteLine($"unknown option '{args[i]}'"); return 2; }
        }
        if (i >= args.Length || core == null)
        {
            Console.Error.WriteLine("usage: --runtime DLL --core CORE [--contrib DIR] build-library (FILE... | --system ASD [--search-path DIR]...) [--reference DLL]... --output PATH");
            return 2;
        }
        var rest = args.Skip(i + 1).ToList();
        DotclBoot.InstallResolver(runtime);
        int code = 1;
        DotclBoot.RunOnLargeStack(() => code = Run(core, contrib, rest));
        return code;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static int Run(string core, string? contrib, List<string> rest)
    {
        DotclBoot.Boot(core, contrib);
        return DotclBuild.RunBuildLibraryCommand(rest, Console.Error);
    }
}
