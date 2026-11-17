# Guard against install-tree explosions leaking into the shipped zip (jasp-issues #4586).
# Called by ZIP.cmd (the `zip` target) after packing; exits non-zero so the target fails.
#
# It catches, among other things, the case where junctions in the build tree are
# dereferenced into full copies while packing: module_libs should hold only the module
# package and its jasp* deps (a few dozen MB), plain R deps load from the binary_pkgs
# micro-libraries, so a zip that stores every package once per module (hundreds of
# thousands of entries) is a bug and must not ship.

param(
    [Parameter(Mandatory = $true)][string]$ZipPath,
    [int]$MaxEntries = 100000,
    [double]$MaxSizeGB = 4
)

if (-not (Test-Path -LiteralPath $ZipPath)) {
    Write-Output "ZIP check FAILED: zip not found at '$ZipPath'"
    exit 1
}

$sizeGB = (Get-Item -LiteralPath $ZipPath).Length / 1GB

Add-Type -AssemblyName System.IO.Compression.FileSystem
$zip = [System.IO.Compression.ZipFile]::OpenRead($ZipPath)
try {
    $entries = $zip.Entries.Count
    $moduleLibsEntries = 0
    $moduleLibsBytes = [long]0
    foreach ($e in $zip.Entries) {
        if ($e.FullName -like 'Modules/module_libs/*') {
            $moduleLibsEntries++
            $moduleLibsBytes += $e.Length
        }
    }
} finally {
    $zip.Dispose()
}
$moduleLibsGB = $moduleLibsBytes / 1GB

Write-Output ("ZIP check: {0:N0} entries, {1:N2} GB total; module_libs: {2:N0} entries, {3:N2} GB uncompressed" -f $entries, $sizeGB, $moduleLibsEntries, $moduleLibsGB)
Write-Output ("ZIP check limits: {0:N0} entries, {1:N1} GB" -f $MaxEntries, $MaxSizeGB)

if ($entries -gt $MaxEntries) {
    Write-Output "ZIP check FAILED: entry count above the limit. If module_libs dominates, junctions were dereferenced into real copies while packing instead of shipping the small copy entries."
    exit 1
}
if ($sizeGB -gt $MaxSizeGB) {
    Write-Output "ZIP check FAILED: zip size above the limit."
    exit 1
}
Write-Output "ZIP check OK"
