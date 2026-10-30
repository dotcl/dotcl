using System;

namespace PluginHost
{
    public interface IPlugin
    {
        string Name { get; }
        // The host passes a callback for progress messages; the plugin calls it while it runs.
        string Run(string input, Action<string> progress);
        // Raised once with the result when Run finishes.
        event Action<string> Completed;
    }
}
