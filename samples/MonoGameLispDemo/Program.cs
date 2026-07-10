using DotCL;

// Quick sanity check: a pure C# Game subclass that clears to red. If this
// shows red, MonoGame works in this project; the issue is dotcl interop
// passing struct args to .NET methods.
if (args.Length > 0 && args[0] == "--csharp-sanity")
{
    using var sanity = new CsharpSanityGame();
    sanity.Run();
    return;
}

// Boot dotcl BEFORE constructing the Game so the Lisp side has a chance to
// (dotnet:define-class "Demo.LispGame" (Game) ...) and the dynamically
// emitted assembly is loaded. Then we instantiate the Lisp-defined type
// via DotclHost.Call("MAKE-GAME") and Run() it on the main thread.

DotclHost.Initialize();

// No need to force-load MonoGame's assembly: when the Lisp side first names a
// Microsoft.Xna.Framework.* type, resolve-type probes the app base directory
// (where MonoGame.Framework.dll is copied) and loads it on demand.

var manifestPath = Path.Combine(
    AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
Console.WriteLine($"[dotcl] manifest: {manifestPath}");
var loaded = DotclHost.LoadFromManifest(manifestPath);
Console.WriteLine($"[dotcl] LoadFromManifest loaded {loaded} fasls");

// MAKE-GAME (defined in main.lisp) returns a Demo.LispGame instance.
var gameObj = DotclHost.Call("MAKE-GAME");
if (gameObj is LispDotNetObject dno
    && dno.Value is Microsoft.Xna.Framework.Game game)
{
    Console.WriteLine($"[dotcl] running game: {game.GetType().FullName}");
    game.Run();
}
else
{
    throw new InvalidOperationException(
        $"MAKE-GAME returned unexpected: {gameObj?.GetType().Name}");
}
