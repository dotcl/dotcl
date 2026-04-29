namespace DotCL;

internal static class CliCompletion
{
    private static readonly string[] Flags = new[]
    {
        "--help", "--version", "--core", "--load", "--eval", "--script",
        "--resolve-deps", "--manifest-out", "--root-sources-out",
        "--compile-project", "--output", "--completion", "--asd-search-path",
    };

    private static readonly string[] FilePathFlags = new[]
    {
        "--core", "--load", "--script", "--resolve-deps",
        "--manifest-out", "--root-sources-out",
        "--compile-project", "--output", "--asd-search-path",
    };

    private static readonly string[] Subcommands = new[] { "repl" };

    private static readonly string[] CompletionShells = new[]
    {
        "pwsh", "bash", "zsh", "fish",
    };

    public static int Emit(string shell)
    {
        switch (shell)
        {
            case "pwsh":
                Console.WriteLine(PowerShellScript());
                return 0;
            case "bash":
                Console.WriteLine(BashScript());
                return 0;
            case "zsh":
                Console.WriteLine(ZshScript());
                return 0;
            case "fish":
                Console.WriteLine(FishScript());
                return 0;
            default:
                Console.Error.WriteLine($"dotcl: --completion: unknown shell '{shell}'");
                Console.Error.WriteLine($"  supported: {string.Join(", ", CompletionShells)}");
                return 2;
        }
    }

    private static string PowerShellScript()
    {
        var flagList = string.Join(",", Flags.Select(f => $"'{f}'"));
        var fileFlagList = string.Join(",", FilePathFlags.Select(f => $"'{f}'"));
        var subList = string.Join(",", Subcommands.Select(s => $"'{s}'"));
        var shellList = string.Join(",", CompletionShells.Select(s => $"'{s}'"));
        return $@"# dotcl PowerShell completion. Load via:
#   dotcl --completion pwsh | Out-String | Invoke-Expression
# Or persist in $PROFILE:
#   dotcl --completion pwsh >> $PROFILE
Register-ArgumentCompleter -CommandName 'dotcl' -Native -ScriptBlock {{
    param($wordToComplete, $commandAst, $cursorPosition)
    $flags    = @({flagList})
    $fileFlag = @({fileFlagList})
    $subs     = @({subList})
    $shells   = @({shellList})

    # Tokens fully completed before the cursor (excludes the partial word
    # currently being typed, whose EndOffset == cursor when EndOffset -lt fails).
    $tokens = @($commandAst.CommandElements |
        Where-Object {{ $_.Extent.EndOffset -lt $cursorPosition }} |
        ForEach-Object {{ $_.Extent.Text }})
    $prev = if ($tokens.Count -ge 1) {{ $tokens[$tokens.Count - 1] }} else {{ '' }}

    if ($prev -eq '--completion') {{
        $shells | Where-Object {{ $_ -like ""$wordToComplete*"" }} |
            ForEach-Object {{
                [System.Management.Automation.CompletionResult]::new(
                    $_, $_, 'ParameterValue', $_)
            }}
        return
    }}
    if ($fileFlag -contains $prev) {{
        Get-ChildItem -Path ""$wordToComplete*"" -ErrorAction SilentlyContinue |
            ForEach-Object {{
                [System.Management.Automation.CompletionResult]::new(
                    $_.Name, $_.Name, 'ProviderItem', $_.FullName)
            }}
        return
    }}

    $candidates = $flags + $subs
    $candidates | Where-Object {{ $_ -like ""$wordToComplete*"" }} |
        ForEach-Object {{
            [System.Management.Automation.CompletionResult]::new(
                $_, $_, 'ParameterName', $_)
        }}
}}";
    }

    private static string BashScript()
    {
        var flags = string.Join(" ", Flags);
        var subs = string.Join(" ", Subcommands);
        var fileFlagPattern = string.Join("|", FilePathFlags);
        var shellList = string.Join(" ", CompletionShells);
        return $@"# dotcl bash completion. Load via:
#   eval ""$(dotcl --completion bash)""
# Or persist:
#   dotcl --completion bash > /etc/bash_completion.d/dotcl
_dotcl_completions() {{
    local cur prev
    cur=""${{COMP_WORDS[COMP_CWORD]}}""
    prev=""${{COMP_WORDS[COMP_CWORD-1]}}""
    case ""$prev"" in
        --completion)
            COMPREPLY=( $(compgen -W ""{shellList}"" -- ""$cur"") )
            return ;;
        {fileFlagPattern})
            COMPREPLY=( $(compgen -f -- ""$cur"") )
            return ;;
    esac
    COMPREPLY=( $(compgen -W ""{flags} {subs}"" -- ""$cur"") )
}}
complete -F _dotcl_completions dotcl";
    }

    private static string ZshScript()
    {
        var flags = string.Join(" ", Flags);
        var subs = string.Join(" ", Subcommands);
        var fileFlagPattern = string.Join("|", FilePathFlags);
        var shellList = string.Join(" ", CompletionShells);
        return $@"# dotcl zsh completion. Load via:
#   source <(dotcl --completion zsh)
# Or persist as fpath function _dotcl.
_dotcl() {{
    local prev
    prev=""${{words[CURRENT-1]}}""
    case ""$prev"" in
        --completion) compadd {shellList}; return ;;
        {fileFlagPattern}) _files; return ;;
    esac
    compadd {flags} {subs}
}}
compdef _dotcl dotcl";
    }

    private static string FishScript()
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine("# dotcl fish completion. Load via:");
        sb.AppendLine("#   dotcl --completion fish | source");
        sb.AppendLine("# Or persist:");
        sb.AppendLine("#   dotcl --completion fish > ~/.config/fish/completions/dotcl.fish");
        foreach (var f in Flags)
        {
            var name = f.TrimStart('-');
            var arity = FilePathFlags.Contains(f)
                ? "-r -F"
                : "";
            sb.AppendLine($"complete -c dotcl -l {name} {arity}".TrimEnd());
        }
        foreach (var s in Subcommands)
        {
            sb.AppendLine($"complete -c dotcl -a {s}");
        }
        sb.Append("complete -c dotcl -n '__fish_seen_subcommand_from --completion' ")
          .AppendLine($"-a '{string.Join(" ", CompletionShells)}'");
        return sb.ToString().TrimEnd();
    }
}
