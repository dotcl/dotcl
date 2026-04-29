using System.Reflection.Emit;

namespace DotCL.Emitter;

enum VarKind { Local, Boxed, EnvSlot, Special }

class VarInfo
{
    public VarKind Kind { get; }
    public LocalBuilder? Local { get; }
    public int EnvIndex { get; }
    public Symbol? Symbol { get; }

    private VarInfo(VarKind kind, LocalBuilder? local, int envIndex, Symbol? symbol)
    {
        Kind = kind;
        Local = local;
        EnvIndex = envIndex;
        Symbol = symbol;
    }

    public static VarInfo MakeLocal(LocalBuilder local) =>
        new(VarKind.Local, local, -1, null);

    public static VarInfo MakeBoxed(LocalBuilder boxLocal) =>
        new(VarKind.Boxed, boxLocal, -1, null);

    public static VarInfo MakeEnvSlot(int index) =>
        new(VarKind.EnvSlot, null, index, null);

    public static VarInfo MakeSpecial(Symbol sym) =>
        new(VarKind.Special, null, -1, sym);
}

class LexicalScope
{
    private readonly Dictionary<string, VarInfo> _vars = new();
    private readonly LexicalScope? _parent;

    public LexicalScope? Parent => _parent;

    public LexicalScope(LexicalScope? parent = null) => _parent = parent;

    public void Define(string name, VarInfo info) => _vars[name] = info;

    public VarInfo? Lookup(string name)
    {
        if (_vars.TryGetValue(name, out var info)) return info;
        return _parent?.Lookup(name);
    }
}

class BlockInfo
{
    public object Tag { get; }
    public Label EndLabel { get; }
    public LocalBuilder ResultLocal { get; }

    public BlockInfo(object tag, Label endLabel, LocalBuilder resultLocal)
    {
        Tag = tag;
        EndLabel = endLabel;
        ResultLocal = resultLocal;
    }
}

class TagbodyInfo
{
    public object Id { get; }
    public Dictionary<string, int> TagIndices { get; }
    public Label DispatchLabel { get; }
    public LocalBuilder IndexLocal { get; }

    public TagbodyInfo(object id, Dictionary<string, int> tagIndices,
                       Label dispatchLabel, LocalBuilder indexLocal)
    {
        Id = id;
        TagIndices = tagIndices;
        DispatchLabel = dispatchLabel;
        IndexLocal = indexLocal;
    }
}
