# Print the metadata of the Lisp-emitted assembly that the Android build cares
# about: its assembly references (the Mono.Android reference is what makes the
# build consider the assembly a source of Java types), the emitted type's base
# type, its custom attributes, and each method's override target.
#
# Usage:  pwsh -NoProfile -File verify-emitted-dll.ps1 [-Dll <path>] [-Type <full name>]

param(
  [string] $Dll  = (Join-Path $PSScriptRoot "out/LispAndroidSpike.dll"),
  [string] $Type = "LispSpike.LispEchoService"
)

$ErrorActionPreference = 'Stop'

$dotnetRoot = Split-Path -Parent (Get-Command dotnet).Source
$sdkPack = Get-ChildItem (Join-Path $dotnetRoot "packs/Microsoft.Android.Sdk.Windows/*") -Directory |
           Sort-Object Name -Descending | Select-Object -First 1
Add-Type -Path (Join-Path $sdkPack.FullName "tools/Mono.Cecil.dll")

$asm = [Mono.Cecil.AssemblyDefinition]::ReadAssembly((Resolve-Path $Dll).Path)
"assembly: " + $asm.Name.FullName
"assembly references:"
foreach ($r in $asm.MainModule.AssemblyReferences) { "  " + $r.FullName }

$t = $asm.MainModule.GetType($Type)
if (-not $t) { throw "type not found: $Type" }
"type: " + $t.FullName
"  base: " + $t.BaseType.FullName + "   (from " + $t.BaseType.Scope + ")"
foreach ($ca in $t.CustomAttributes) { "  attribute: " + $ca.AttributeType.FullName }
foreach ($m in $t.Methods) {
  $ps = ($m.Parameters | ForEach-Object { $_.ParameterType.FullName }) -join ', '
  $flags = @()
  if ($m.IsVirtual)    { $flags += 'virtual' }
  if ($m.IsNewSlot)    { $flags += 'newslot' }
  if ($m.IsReuseSlot)  { $flags += 'reuseslot' }
  if ($m.IsPublic)     { $flags += 'public' }
  "  " + $m.ReturnType.FullName + " " + $m.Name + "(" + $ps + ") [" + ($flags -join ' ') + "]"
  foreach ($o in $m.Overrides) { "      overrides " + $o.FullName }
}
