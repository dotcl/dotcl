using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

// MCP protocol owns stdout, so logs must go to stderr.
// Capturing down to Trace leaves breadcrumbs in the client's (e.g. Claude Desktop)
// stderr pipe during debugging.
var builder = Host.CreateApplicationBuilder(args);
builder.Logging.AddConsole(options =>
    options.LogToStandardErrorThreshold = LogLevel.Trace);

builder.Services
    .AddMcpServer()
    .WithStdioServerTransport()
    .WithTools<McpServerDemo.DotclTools>();

await builder.Build().RunAsync();
