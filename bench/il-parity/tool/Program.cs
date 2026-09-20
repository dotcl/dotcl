using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IlParity.Tool;

public static class Program
{
    public static int Main(string[] args)
    {
        // LF on every platform. The counts go into a file that is committed,
        // diffed and read back by a shell script; a line ending that depends on
        // where the tool ran would show up as a difference in all three.
        Console.Out.NewLine = "\n";
        if (args.Length == 0) { Usage(); return 2; }
        switch (args[0])
        {
            case "list":
                return List(args[1]);
            case "dump":
                return Dump(args[1], args.Length > 2 ? args[2] : null);
            case "compare":
                return Compare.Run(args[1], args[2], args[3], Console.Out) == 0 ? 0 : 1;
            case "counts":
                return Compare.Counts(args[1], args[2], args[3], Console.Out) == 0 ? 0 : 1;
            default:
                Usage();
                return 2;
        }
    }

    private static void Usage()
    {
        Console.Error.WriteLine("usage: ilparity list <assembly>");
        Console.Error.WriteLine("       ilparity dump <assembly> [method-substring]");
        Console.Error.WriteLine("       ilparity compare <case-dir> <Refs.dll> <impl.fasl>");
        Console.Error.WriteLine("       ilparity counts  <case-dir> <Refs.dll> <impl.fasl>");
    }

    private static int List(string path)
    {
        foreach (var kv in Il.ReadAssembly(path).OrderBy(k => k.Key, StringComparer.Ordinal))
            Console.WriteLine($"{kv.Value.Count,6}  {kv.Key}");
        return 0;
    }

    private static int Dump(string path, string filter)
    {
        foreach (var kv in Il.ReadAssembly(path).OrderBy(k => k.Key, StringComparer.Ordinal))
        {
            if (filter != null && kv.Key.IndexOf(filter, StringComparison.OrdinalIgnoreCase) < 0)
                continue;
            Console.WriteLine("=== " + kv.Key);
            foreach (var ins in kv.Value) Console.WriteLine("  " + ins);
        }
        return 0;
    }
}
