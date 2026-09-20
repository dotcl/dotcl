# Run the Android SDK's own Java Callable Wrapper generator against the
# Lisp-emitted assembly, without MSBuild.
#
# This is the same code path the _GenerateJavaStubs target drives
# (JavaTypeScanner -> CecilImporter -> CallableWrapperType.Generate), so it
# answers "would the Android build accept this type?" in about a second and
# without an Android project, an SDK install check or a device.
#
# Usage:  pwsh -NoProfile -File run-acw-generator.ps1 [-Dll <path>] [-OutDir <path>]

param(
  [string] $Dll    = (Join-Path $PSScriptRoot "out/LispAndroidSpike.dll"),
  [string] $OutDir = (Join-Path $PSScriptRoot "jcw-out")
)

$ErrorActionPreference = 'Stop'

function Get-NewestDir([string] $glob) {
  $d = Get-ChildItem $glob -Directory -ErrorAction SilentlyContinue |
       Sort-Object Name -Descending | Select-Object -First 1
  if (-not $d) { throw "not found: $glob" }
  return $d.FullName
}

$dotnetRoot  = Split-Path -Parent (Get-Command dotnet).Source
$sdkPack     = Get-NewestDir (Join-Path $dotnetRoot "packs/Microsoft.Android.Sdk.Windows/*")
$tools       = Join-Path $sdkPack "tools"
$androidRef  = Join-Path (Get-NewestDir (Join-Path $dotnetRoot "packs/Microsoft.Android.Ref.*/*")) "ref/net10.0"
$bclRef      = Join-Path (Get-NewestDir (Join-Path $dotnetRoot "packs/Microsoft.NETCore.App.Ref/*")) "ref/net10.0"

foreach ($d in @("Mono.Cecil.dll", "Java.Interop.Tools.Cecil.dll",
                 "Java.Interop.Tools.Diagnostics.dll",
                 "Java.Interop.Tools.JavaCallableWrappers.dll",
                 "Java.Interop.Tools.TypeNameMappings.dll")) {
  $p = Join-Path $tools $d
  if (Test-Path $p) { Add-Type -Path $p }
}

$traceLogger  = [Action[System.Diagnostics.TraceLevel, string]] { param($lvl, $m) Write-Host "  [$lvl] $m" }
$stringLogger = [Action[string, object[]]] { param($m, $a) Write-Host "  [log] $m" }

$readerParams = [Mono.Cecil.ReaderParameters]::new()
$resolver = [Java.Interop.Tools.Cecil.DirectoryAssemblyResolver]::new($traceLogger, $false, $readerParams)
foreach ($d in @((Split-Path -Parent (Resolve-Path $Dll)), $androidRef, $bclRef)) {
  $resolver.SearchDirectories.Add($d)
}

$paths = [System.Collections.Generic.List[string]]::new()
$paths.Add((Resolve-Path $Dll).Path)

$cache = [Java.Interop.Tools.Cecil.TypeDefinitionCache]::new()
$types = [Java.Interop.Tools.JavaCallableWrappers.JavaTypeScanner]::GetJavaTypes(
           $paths, $resolver, $stringLogger, $cache)

"java types found: " + $types.Count
foreach ($t in $types) { "  -> " + $t.FullName }

$readerOptions = [Java.Interop.Tools.JavaCallableWrappers.CallableWrapperReaderOptions]::new()
$writerOptions = [Java.Interop.Tools.JavaCallableWrappers.CallableWrapperWriterOptions]::new()

if (Test-Path $OutDir) { Remove-Item -Recurse -Force $OutDir }
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

foreach ($t in $types) {
  if ([Java.Interop.Tools.JavaCallableWrappers.JavaTypeScanner]::ShouldSkipJavaCallableWrapperGeneration($t, $cache)) {
    "skipped by generator: " + $t.FullName
    continue
  }
  $cw = [Java.Interop.Tools.JavaCallableWrappers.Adapters.CecilImporter]::CreateType($t, $cache, $readerOptions)
  "wrapper: package=" + $cw.Package + " name=" + $cw.Name + " extends=" + $cw.ExtendsType
  "  ctors=" + $cw.Constructors.Count + " methods=" + $cw.Methods.Count
  $cw.Generate($OutDir, $writerOptions)
  "  wrote: " + $cw.GetDestinationPath($OutDir)
}
