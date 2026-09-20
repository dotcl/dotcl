# dotcl-kestrel

An HTTP server on ASP.NET Core's Kestrel. `dotcl-kestrel:run` takes an
application -- a function of one argument, the request as a property list,
returning `(status headers body)` -- so a Lack or Clack application runs here
unchanged. `dotcl-kestrel:stop` shuts a server down.

    (require "dotcl-kestrel")
    (dotcl-kestrel:run app :port 5000 :address "127.0.0.1")

The request body is read before the application is called and the response
written after it returns, both asynchronously, because Kestrel refuses
synchronous stream operations by default; pass `:allow-synchronous-io t` to hand
the application Kestrel's own stream instead. Needs the ASP.NET Core shared
framework to be present -- the contrib loads `Microsoft.AspNetCore` as it loads.
