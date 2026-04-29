namespace DotCL;

public class LispPathname : LispObject
{
    public LispObject? Host { get; }
    public LispObject? Device { get; }
    public LispObject? DirectoryComponent { get; }
    public LispObject? NameComponent { get; }
    public LispObject? TypeComponent { get; }
    public LispObject? Version { get; }

    public LispPathname(LispObject? host, LispObject? device, LispObject? directory,
                        LispObject? name, LispObject? type, LispObject? version)
    {
        Host = host;
        Device = device;
        DirectoryComponent = directory;
        NameComponent = name;
        TypeComponent = type;
        Version = version;
    }

    /// <summary>
    /// Helper to get the string value from a component that may be LispString, Symbol(:WILD), or null/Nil.
    /// Returns null if not a LispString.
    /// </summary>
    private static string? StringValue(LispObject? component)
    {
        if (component is LispString ls) return ls.Value;
        return null;
    }

    /// <summary>
    /// Helper to check if a component represents :WILD
    /// </summary>
    private static bool IsWild(LispObject? component)
    {
        return component is Symbol s && s.Name == "WILD";
    }

    public static LispPathname FromString(string path)
    {
        // Normalize path separators
        path = path.Replace('\\', '/');

        LispObject? device = null;
        LispObject? host = null;
        LispObject? directory = null;
        LispObject? name = null;
        LispObject? type = null;

        // Extract drive letter on Windows (e.g., "C:")
        if (path.Length >= 2 && path[1] == ':')
        {
            device = new LispString(path[0].ToString().ToUpperInvariant());
            path = path[2..];
        }

        // Split directory and filename
        int lastSlash = path.LastIndexOf('/');
        string dirPart = "";
        string filePart;
        if (lastSlash >= 0)
        {
            dirPart = path[..(lastSlash + 1)];
            filePart = path[(lastSlash + 1)..];
        }
        else
        {
            filePart = path;
        }

        // Parse directory
        if (!string.IsNullOrEmpty(dirPart))
        {
            var dirs = new List<LispObject>();
            bool absolute = dirPart.StartsWith('/');
            dirs.Add(absolute ? Startup.Keyword("ABSOLUTE") : Startup.Keyword("RELATIVE"));

            var parts = dirPart.Split('/', StringSplitOptions.RemoveEmptyEntries);
            foreach (var part in parts)
            {
                if (part == "..") dirs.Add(Startup.Keyword("UP"));
                else if (part == ".") continue; // skip current dir
                else if (part == "**") dirs.Add(Startup.Keyword("WILD-INFERIORS"));
                else if (part == "*") dirs.Add(Startup.Keyword("WILD"));
                else dirs.Add(new LispString(part));
            }
            directory = Runtime.List(dirs.ToArray());
        }

        // Parse filename
        if (!string.IsNullOrEmpty(filePart))
        {
            int dotPos = filePart.LastIndexOf('.');
            if (dotPos > 0)
            {
                string namePart = filePart[..dotPos];
                string typePart = filePart[(dotPos + 1)..];
                name = namePart == "*" ? (LispObject)Startup.Keyword("WILD") : new LispString(namePart);
                type = typePart == "*" ? (LispObject)Startup.Keyword("WILD") : new LispString(typePart);
            }
            else if (dotPos == 0)
            {
                // Dotfile like ".gitignore"
                name = new LispString(filePart);
            }
            else
            {
                name = filePart == "*" ? (LispObject)Startup.Keyword("WILD") : new LispString(filePart);
            }
        }

        return new LispPathname(host, device, directory, name, type, null);
    }

    public virtual string ToNamestring()
    {
        var sb = new System.Text.StringBuilder();

        var deviceStr = StringValue(Device);
        if (deviceStr != null)
            sb.Append($"{deviceStr}:");

        if (DirectoryComponent is Cons dirList)
        {
            var cur = (LispObject)dirList;
            bool first = true;
            while (cur is Cons c)
            {
                if (first)
                {
                    first = false;
                    if (c.Car is Symbol sym && sym.Name == "ABSOLUTE")
                    {
                        sb.Append('/');
                        cur = c.Cdr;
                        continue;
                    }
                    else if (c.Car is Symbol sym2 && sym2.Name == "RELATIVE")
                    {
                        cur = c.Cdr;
                        continue;
                    }
                }
                if (c.Car is Symbol upSym && upSym.Name == "UP")
                    sb.Append("../");
                else if (c.Car is Symbol wildSym && wildSym.Name == "WILD")
                    sb.Append("*/");
                else if (c.Car is Symbol wildInfSym && wildInfSym.Name == "WILD-INFERIORS")
                    sb.Append("**/");
                else if (c.Car is LispString ds)
                    sb.Append($"{ds.Value}/");
                cur = c.Cdr;
            }
        }

        if (NameComponent != null && NameComponent is not Nil)
        {
            if (IsWild(NameComponent))
                sb.Append('*');
            else if (NameComponent is LispString nameStr)
                sb.Append(nameStr.Value);
        }
        if (TypeComponent != null && TypeComponent is not Nil)
        {
            if (IsWild(TypeComponent))
                sb.Append(".*");
            else if (TypeComponent is LispString typeStr)
                sb.Append($".{typeStr.Value}");
        }

        return sb.ToString();
    }

    public virtual LispPathname MergeWith(LispPathname defaults)
    {
        return new LispPathname(
            Host ?? defaults.Host,
            Device ?? defaults.Device,
            MergeDirectories(DirectoryComponent, defaults.DirectoryComponent),
            NameComponent ?? defaults.NameComponent,
            TypeComponent ?? defaults.TypeComponent,
            Version ?? defaults.Version
        );
    }

    protected static LispObject? MergeDirectories(LispObject? dir, LispObject? defaultDir)
    {
        if (dir == null) return defaultDir;
        if (defaultDir == null) return dir;

        // Check if dir is (:relative ...) and defaultDir is (:absolute ...)
        if (dir is Cons dirCons && defaultDir is Cons defCons)
        {
            var dirType = dirCons.Car as Symbol;
            var defType = defCons.Car as Symbol;

            if (dirType != null && dirType.Name == "RELATIVE"
                && defType != null && (defType.Name == "ABSOLUTE" || defType.Name == "RELATIVE"))
            {
                // Append relative components to default directory
                // Result: (:absolute/relative ...default-parts... ...relative-parts...)
                var result = new List<LispObject>();
                // Copy all from default (including :absolute/:relative)
                LispObject cur = defCons;
                while (cur is Cons c)
                {
                    result.Add(c.Car);
                    cur = c.Cdr;
                }
                // Append relative parts (skip :relative keyword)
                cur = dirCons.Cdr;
                while (cur is Cons c)
                {
                    result.Add(c.Car);
                    cur = c.Cdr;
                }
                return Runtime.List(result.ToArray());
            }
        }

        // Otherwise just use the pathname's own directory
        return dir;
    }

    public override string ToString() => $"#P\"{ToNamestring()}\"";
}

/// <summary>
/// Logical pathname: a pathname with a Host that maps to logical-pathname-translations.
/// CLHS 19.3: Logical pathnames use uppercase, semicolons as directory separators.
/// </summary>
public class LispLogicalPathname : LispPathname
{
    public LispLogicalPathname(LispObject? host, LispObject? device, LispObject? directory,
                               LispObject? name, LispObject? type, LispObject? version)
        : base(host, device, directory, name, type, version) { }

    /// <summary>
    /// Parse a logical pathname string like "HOST:dir;name.type.version"
    /// </summary>
    public static LispLogicalPathname FromLogicalString(string input)
    {
        string host = "";
        string rest = input;

        int colonPos = input.IndexOf(':');
        if (colonPos >= 0)
        {
            host = input[..colonPos].ToUpperInvariant();
            rest = input[(colonPos + 1)..];
        }

        // Split by semicolons for directories
        LispObject? directory = null;
        LispObject? name = null;
        LispObject? type = null;
        LispObject? version = null;

        var semiParts = rest.Split(';');
        string filePart;

        if (semiParts.Length > 1)
        {
            // Has directory components
            var dirs = new List<LispObject>();
            dirs.Add(Startup.Keyword("ABSOLUTE"));
            for (int i = 0; i < semiParts.Length - 1; i++)
            {
                var d = semiParts[i].ToUpperInvariant();
                if (d == "**") dirs.Add(Startup.Keyword("WILD-INFERIORS"));
                else if (d == "*") dirs.Add(Startup.Keyword("WILD"));
                else if (d.Length > 0) dirs.Add(new LispString(d));
            }
            directory = Runtime.List(dirs.ToArray());
            filePart = semiParts[^1];
        }
        else
        {
            filePart = rest;
        }

        // Parse name.type.version
        if (!string.IsNullOrEmpty(filePart))
        {
            filePart = filePart.ToUpperInvariant();
            var dotParts = filePart.Split('.');
            if (dotParts.Length >= 1 && dotParts[0].Length > 0)
                name = dotParts[0] == "*" ? (LispObject)Startup.Keyword("WILD") : new LispString(dotParts[0]);
            if (dotParts.Length >= 2 && dotParts[1].Length > 0)
                type = dotParts[1] == "*" ? (LispObject)Startup.Keyword("WILD") : new LispString(dotParts[1]);
            if (dotParts.Length >= 3 && dotParts[2].Length > 0)
            {
                if (dotParts[2] == "*") version = Startup.Keyword("WILD");
                else if (dotParts[2] == "NEWEST") version = Startup.Keyword("NEWEST");
                else if (int.TryParse(dotParts[2], out int v)) version = Fixnum.Make(v);
            }
        }

        return new LispLogicalPathname(
            string.IsNullOrEmpty(host) ? null : new LispString(host),
            null, directory, name, type, version);
    }

    public override string ToNamestring()
    {
        var sb = new System.Text.StringBuilder();
        if (Host is LispString hs)
        {
            sb.Append(hs.Value);
            sb.Append(':');
        }
        // Directory with semicolons
        if (DirectoryComponent is Cons dirList)
        {
            var cur = (LispObject)dirList;
            bool first = true;
            while (cur is Cons c)
            {
                if (first)
                {
                    first = false;
                    cur = c.Cdr;
                    continue;
                }
                if (c.Car is Symbol ws && ws.Name == "WILD-INFERIORS")
                    sb.Append("**;");
                else if (c.Car is Symbol w && w.Name == "WILD")
                    sb.Append("*;");
                else if (c.Car is LispString ds)
                    sb.Append($"{ds.Value};");
                cur = c.Cdr;
            }
        }
        if (NameComponent is Symbol ns && ns.Name == "WILD")
            sb.Append('*');
        else if (NameComponent is LispString nameStr)
            sb.Append(nameStr.Value);
        if (TypeComponent != null && TypeComponent is not Nil)
        {
            sb.Append('.');
            if (TypeComponent is Symbol ts && ts.Name == "WILD")
                sb.Append('*');
            else if (TypeComponent is LispString typeStr)
                sb.Append(typeStr.Value);
        }
        if (Version != null && Version is not Nil)
        {
            sb.Append('.');
            if (Version is Symbol vs) sb.Append(vs.Name);
            else sb.Append(Version);
        }
        return sb.ToString();
    }

    public override string ToString() => $"#P\"{ToNamestring()}\"";

    public override LispPathname MergeWith(LispPathname defaults)
    {
        return new LispLogicalPathname(
            Host ?? defaults.Host,
            Device ?? defaults.Device,
            MergeDirectories(DirectoryComponent, defaults.DirectoryComponent),
            NameComponent ?? defaults.NameComponent,
            TypeComponent ?? defaults.TypeComponent,
            Version ?? defaults.Version
        );
    }
}
