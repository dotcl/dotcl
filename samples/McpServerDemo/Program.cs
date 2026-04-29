using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

// MCP プロトコルが stdout を占有するので、log は必ず stderr 側に出す。
// Trace まで拾っておけば debug 時に client 側 (Claude Desktop 等) の
// stderr pipe で手がかりが残る。
var builder = Host.CreateApplicationBuilder(args);
builder.Logging.AddConsole(options =>
    options.LogToStandardErrorThreshold = LogLevel.Trace);

builder.Services
    .AddMcpServer()
    .WithStdioServerTransport()
    .WithTools<McpServerDemo.DotclTools>();

await builder.Build().RunAsync();
