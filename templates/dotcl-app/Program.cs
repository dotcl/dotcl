using DotCL;

// Boot the dotcl runtime, load the fasl the build compiled from app.lisp
// (deployed next to the app under dotcl-fasl/), and call the Lisp entry point.
DotclHost.Initialize();
var manifest = Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt");
DotclHost.LoadFromManifest(manifest);
DotclHost.Call("APP:APP-MAIN");
