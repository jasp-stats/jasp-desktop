# EFSPoC — Phase 3 proof-of-concept package

A tiny self-contained MSIX that answers the Phase 3 spike questions from
`windows-sandbox-efs-save-crash.md` **without** building the whole JASP MSIX
(the buildbot script wipes `build/` and expects buildbot paths — do not run it locally).

It mirrors JASP's exact MSIX manifest shape (`packagedClassicApp` + `mediumIL` +
`runFullTrust`), so whatever the Windows "Application Protected" EFS subsystem
does to JASP, it should do to this package on the same machine.

## What it does

1. **Launcher** (`EFSPoCLauncher.exe`, full trust, = stand-in for JASPDesktop):
   - derives the package-family AppContainer SID — API first
     (`DeriveAppContainerSidFromAppContainerName`), then the manual SHA-256
     fallback (needed per [WindowsAppSDK#787]: the API returns success-but-null
     for the caller's own family name) — and logs whether both derivations agree
   - logs whether the package `LocalCache` is EFS-encrypted (**the Phase 3 gate**)
   - writes a test file in `LocalCache` (full-trust side)
   - spawns the engine in an AppContainer under that SID (= the Phase 3 launch)
2. **Engine** (`EFSPoCEngine.exe`, in the package AppContainer = stand-in for JASPEngine):
   - dumps its token: AppContainer? which SID? `WIN://SYSAPPID` claims present?
   - **money test**: reads the launcher-created file and creates a new file in `LocalCache`
   - exit code `0` = both succeeded; bits: 1=not-AC, 2=read-failed, 4=create-failed

## Build + install

```
build.cmd                                  :: in a plain cmd window
:: then ONE-TIME in an ADMIN PowerShell (trust the self-signed cert):
::   $c = Get-ChildItem Cert:\CurrentUser\My -CodeSigningCert | ? Subject -eq 'CN=JASP EFSPoC'
::   Export-Certificate -Cert $c -FilePath $env:TEMP\efspoc.cer
::   Import-Certificate -FilePath $env:TEMP\efspoc.cer -CertStoreLocation Cert:\LocalMachine\TrustedPeople
:: then as normal user:
powershell -Command "Add-AppxPackage -Path .\EFSPoC.msix"
:: the AUMID uses the package FAMILY name (name + publisher hash), find yours with:
::   Get-AppxPackage *EFSPoC* | select PackageFamilyName
:: or simply launch "JASP EFS PoC" from the Start menu
start shell:AppsFolder\JASP.EFSPoC_qfqszy29ya4s6!EFSPoC
```

## Reading the results

Logs land in `%LOCALAPPDATA%\Packages\JASP.EFSPoC_<hash>\LocalCache\`:

| Log evidence | Meaning |
|---|---|
| `LocalCache attributes ... EFS-ENCRYPTED` | This machine applies Application-Protected encryption → it is the Phase 3 spike machine |
| `DeriveAppContainerSidFromAppContainerName ... (null ...)` + manual sid | #787 reproduced; fallback works (and `API and manual derivation agree: YES` validates the SHA-256 formula on machines where the API works) |
| Engine: `Token claim WIN://SYSAPPID: PRESENT/ABSENT` | Whether package identity survives into the AC child (the newly-researched risk) |
| Engine: money test `read ... OK` + `create ... OK` | **Phase 3 verdict: viable** — an engine under the package SID can work in the encrypted tree |
| Engine: `read ... FAILED err=6002` / `create ... FAILED err=6000` | Phase 3 fails even with the right SID → plan B (relocate engine working set / broker I/O through Desktop) |
| Engine: everything `OK` **and** LocalCache not encrypted | Machine doesn't do Application-Protected → Phase 3 stays parked; plumbing is validated for when an affected machine is found |

Also run manually afterwards:

```
cipher /c "%LOCALAPPDATA%\Packages\JASP.EFSPoC_*\LocalCache\launcher-file.txt"
```

`Compatibility Level: Application Protected` = gate passed. Compare with
`C:\Program Files\WindowsApps\JASP.EFSPoC_...` if LocalCache turns out unencrypted
(the encryption may apply to the install tree only — also a useful data point).

## Cleanup

```
powershell -Command "Get-AppxPackage *EFSPoC* | Remove-AppxPackage"
```
