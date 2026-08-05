using System.Net.Http;
using System.Reflection;

namespace DotCL;

/// <summary>
/// Minimal HTTP(S) transport: GET a URL and write the body to a file.
///
/// This exists because stock quicklisp speaks only plain HTTP over its own
/// socket code — it has no TLS at all — while everything worth fetching today
/// is https. On .NET the whole of TLS, redirects, gzip, proxies and certificate
/// validation comes from the BCL, so the transport is a thin wrapper rather
/// than the cl+ssl/OpenSSL dependency other implementations need.
///
/// Deliberately unaware of any particular host: no allowlist, no github.com
/// special case. Credentials are resolved by the caller and handed in as
/// headers, which is what lets the same code reach a private dist later.
/// </summary>
internal static class DotclHttp
{
    // AllowAutoRedirect is off on purpose. .NET drops the Authorization header
    // on every automatic redirect — right for a cross-origin hop to a signed URL
    // (a GitHub release asset redirects to objects.githubusercontent.com, and
    // the token must not follow it), wrong for a redirect that stays on the same
    // host and still needs the credential. Following redirects by hand keeps
    // that distinction ours to make; see the host check below.
    private static readonly HttpClient _http =
        new(new HttpClientHandler { AllowAutoRedirect = false });

    private static string AsString(LispObject o) =>
        o is LispString s ? s.Value : o.ToString()!;

    // Some hosts refuse a request with no User-Agent outright — api.github.com
    // answers 403 — so send one by default. A header supplied by the caller
    // wins, which is how a caller identifies itself as something more specific.
    private static readonly string DefaultUserAgent =
        "dotcl/" + (typeof(Runtime).Assembly
            .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
            ?.InformationalVersion ?? "unknown");

    /// <summary>
    /// (dotcl::%http-fetch url file headers max-redirects) =&gt; status-code
    ///
    /// HEADERS is an alist of (name . value) strings, or NIL. On a 2xx the body
    /// is written to FILE and the status is returned; on any other final status
    /// nothing is written and the status is returned so the caller can signal
    /// whatever condition its protocol expects.
    /// </summary>
    public static LispObject Fetch(LispObject[] args)
    {
        if (args.Length != 4)
            throw new LispErrorException(new LispProgramError(
                "%HTTP-FETCH: expected 4 arguments (url file headers max-redirects)"));

        string url = AsString(args[0]);
        string file = AsString(args[1]);
        int maxRedirects = args[3] is Fixnum n ? (int)n.Value : 10;

        var headers = new List<(string Name, string Value)>();
        for (LispObject c = args[2]; c is Cons cell; c = cell.Cdr)
            if (cell.Car is Cons pair)
                headers.Add((AsString(pair.Car), AsString(pair.Cdr)));

        var origin = new Uri(url);
        var current = origin;
        int redirects = 0;

        while (true)
        {
            using var request = new HttpRequestMessage(HttpMethod.Get, current);
            if (!headers.Any(h => string.Equals(h.Name, "User-Agent", StringComparison.OrdinalIgnoreCase)))
                request.Headers.TryAddWithoutValidation("User-Agent", DefaultUserAgent);
            foreach (var (name, value) in headers)
            {
                // Authorization travels only as far as the host the caller
                // resolved it for. Everything else follows the redirect.
                if (string.Equals(name, "Authorization", StringComparison.OrdinalIgnoreCase)
                    && !string.Equals(current.Host, origin.Host, StringComparison.OrdinalIgnoreCase))
                    continue;
                request.Headers.TryAddWithoutValidation(name, value);
            }

            // SendAsync + block rather than the synchronous Send: Send is net5+
            // and this assembly also targets netstandard2.0. Same for
            // ReadAsStreamAsync below.
            using var response = _http
                .SendAsync(request, HttpCompletionOption.ResponseHeadersRead)
                .GetAwaiter().GetResult();
            int status = (int)response.StatusCode;

            if (status >= 300 && status <= 399 && response.Headers.Location is Uri location)
            {
                if (++redirects > maxRedirects)
                    throw new LispErrorException(new LispError(
                        $"%HTTP-FETCH: too many redirects ({maxRedirects}) for {url}"));
                current = new Uri(current, location);
                continue;
            }

            if (status < 200 || status > 299)
                return Fixnum.Make(status);

            var dir = Path.GetDirectoryName(Path.GetFullPath(file));
            if (!string.IsNullOrEmpty(dir))
                Directory.CreateDirectory(dir);

            using (var body = response.Content.ReadAsStreamAsync().GetAwaiter().GetResult())
            using (var output = File.Create(file))
                body.CopyTo(output);

            return Fixnum.Make(status);
        }
    }
}
