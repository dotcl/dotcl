using System;
using PluginHost;

public sealed class UpperPlugin : IPlugin
{
    public string Name => "upper (C#)";
    public event Action<string> Completed;

    public string Run(string input, Action<string> progress)
    {
        progress("upper-casing");
        var result = input.ToUpperInvariant();
        Completed?.Invoke(result);
        return result;
    }
}
