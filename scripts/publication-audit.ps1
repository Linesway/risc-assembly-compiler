param([string]$Root = (Split-Path -Parent $PSScriptRoot))

$ErrorActionPreference = 'Stop'
$rootPath = (Resolve-Path -LiteralPath $Root).Path
$paths = @('src', 'test', 'examples', 'README.md', 'LICENSE', 'build.sbt')
$files = foreach ($relative in $paths) {
    $path = Join-Path $rootPath $relative
    if (Test-Path -LiteralPath $path -PathType Container) {
        Get-ChildItem -LiteralPath $path -File -Recurse
    } elseif (Test-Path -LiteralPath $path -PathType Leaf) {
        Get-Item -LiteralPath $path
    }
}

$restricted = @($files | Where-Object {
    $_.Extension -in @('.scala', '.sc', '.md', '.sbt', '.lacs') -and
    (Select-String -LiteralPath $_.FullName -Quiet -Pattern 'may not be published|private study use by students registered')
})
$binaryArtifacts = @($files | Where-Object { $_.Extension -in @('.jar', '.pdf', '.zip', '.class') })

if ($restricted.Count -gt 0) {
    Write-Host "Publication restrictions found in $($restricted.Count) files:"
    $restricted | ForEach-Object { Write-Host "  $($_.FullName.Substring($rootPath.Length + 1))" }
}
if ($binaryArtifacts.Count -gt 0) {
    Write-Host 'Review bundled binary or document files:'
    $binaryArtifacts | ForEach-Object { Write-Host "  $($_.FullName.Substring($rootPath.Length + 1))" }
}
if ($restricted.Count -gt 0 -or $binaryArtifacts.Count -gt 0) { exit 1 }
Write-Host 'No restricted notices or bundled binaries found.'
