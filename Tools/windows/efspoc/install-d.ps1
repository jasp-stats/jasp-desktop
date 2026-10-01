#Installs EFSPoC onto the non-system package volume D:\WindowsApps (run from an ADMIN PowerShell).
#Purpose: test the documented "packages on non-system volumes are EFS-encrypted" behavior against our PoC.
$ErrorActionPreference = 'Continue'

Write-Host ("default volume: " + (Get-AppxDefaultVolume).PackageStorePath)

Remove-AppxPackage (Get-AppxPackage *EFSPoC*).PackageFullName -AllUsers -ErrorAction SilentlyContinue
$leftover = Get-AppxPackage *EFSPoC*
if ($leftover) { Write-Host ("WARNING still present after removal: " + $leftover.InstallLocation) }

Add-AppxPackage -Volume D:\WindowsApps -Path "$PSScriptRoot\EFSPoC.msix"

$pkg = Get-AppxPackage *EFSPoC*
Write-Host ("installed at: " + $pkg.InstallLocation)
Write-Host ("signature kind: " + $pkg.SignatureKind)

Write-Host "=== cipher /c on installed engine exe ==="
cipher /c ($pkg.InstallLocation + "\EFSPoCEngine.exe")

Write-Host "=== cipher /c on installed manifest ==="
cipher /c ($pkg.InstallLocation + "\AppxManifest.xml")
