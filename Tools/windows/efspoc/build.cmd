@echo off
rem Builds EFSPoC.msix: the Phase 3 proof-of-concept package (see README.md).
rem Only needs Visual Studio (for cl.exe + vcvars) and the Windows SDK (for makeappx/signtool). No Qt, no CMake.

setlocal
cd /d "%~dp0"

set MSVCDIR_DEFAULT=C:\Program Files\Microsoft Visual Studio\2022\Community
if "%MSVCDIR%"=="" set "MSVCDIR=%MSVCDIR_DEFAULT%"
if not exist "%MSVCDIR%\VC\Auxiliary\Build\vcvars64.bat" (
    echo [ERROR] vcvars64.bat not found under "%MSVCDIR%" - set MSVCDIR to your VS install.
    exit /b 1
)
call "%MSVCDIR%\VC\Auxiliary\Build\vcvars64.bat" >nul

echo [1/5] compiling...
if not exist layout mkdir layout
cl /nologo /EHsc /W4 /O2 Launcher.cpp /Fe:layout\EFSPoCLauncher.exe /Fo:layout\Launcher.obj advapi32.lib user32.lib userenv.lib bcrypt.lib || exit /b 1
cl /nologo /EHsc /W4 /O2 Engine.cpp   /Fe:layout\EFSPoCEngine.exe   /Fo:layout\Engine.obj   advapi32.lib || exit /b 1

echo [2/5] generating placeholder assets...
if not exist layout\Assets mkdir layout\Assets
powershell -NoProfile -Command ^
  "Add-Type -AssemblyName System.Drawing;" ^
  "foreach($s in @(@(150,150,'Square150x150Logo.png'),@(44,44,'Square44x44Logo.png'),@(50,50,'StoreLogo.png'))){" ^
  "  $bmp = New-Object System.Drawing.Bitmap($s[0],$s[1]);" ^
  "  $g = [System.Drawing.Graphics]::FromImage($bmp); $g.Clear([System.Drawing.Color]::SteelBlue); $g.Dispose();" ^
  "  $bmp.Save((Join-Path 'layout\Assets' $s[2])); $bmp.Dispose() }" || exit /b 1

echo [3/5] assembling layout...
copy /y AppxManifest.xml layout\ >nul || exit /b 1

echo [4/5] packing msix...
where makeappx.exe >nul 2>&1 || (echo [ERROR] makeappx.exe not in PATH - open a VS command prompt or adjust PATH. & exit /b 1)
if exist EFSPoC.msix del EFSPoC.msix
makeappx pack /d layout /p EFSPoC.msix /v || exit /b 1

echo [5/5] signing...
where signtool.exe >nul 2>&1 || (echo [ERROR] signtool.exe not in PATH. & exit /b 1)
rem Reuse a previously created code-signing cert, or create one now (current-user store, no admin needed).
for /f "tokens=*" %%i in ('powershell -NoProfile -Command "(Get-ChildItem Cert:\CurrentUser\My -CodeSigningCert | Where-Object Subject -eq 'CN=JASP EFSPoC' | Select-Object -First 1).Thumbprint"') do set CERTTHUMB=%%i
if "%CERTTHUMB%"=="" (
    for /f "tokens=*" %%i in ('powershell -NoProfile -Command "(New-SelfSignedCertificate -Type CodeSigningCert -Subject 'CN=JASP EFSPoC' -CertStoreLocation Cert:\CurrentUser\My).Thumbprint"') do set CERTTHUMB=%%i
    echo     created new self-signed cert CN=JASP EFSPoC, thumbprint %CERTTHUMB%
)
signtool sign /fd SHA256 /sha1 %CERTTHUMB% EFSPoC.msix || exit /b 1

echo.
echo ================================================================================
echo  Built and signed: %CD%\EFSPoC.msix
echo.
echo  ONE-TIME (elevated) - trust the cert so the package can install:
echo    powershell -Command "Import-PfxCertificate ..." or simply:
echo    $cert = Get-ChildItem Cert:\CurrentUser\My -CodeSigningCert ^| ? Subject -eq 'CN=JASP EFSPoC'
echo    Export-Certificate -Cert $cert -FilePath %TEMP%\efspoc.cer
echo    Import-Certificate -FilePath %TEMP%\efspoc.cer -CertStoreLocation Cert:\LocalMachine\TrustedPeople
echo  (run those two lines in an ADMIN PowerShell)
echo.
echo  THEN: install + run (normal user):
    powershell -Command "Add-AppxPackage -Path '%CD%\EFSPoC.msix'"
    powershell -Command "^(Get-AppxPackage *EFSPoC* ^| Get-StartAppInfo^)" 2^>nul
    start shell:AppsFolder\JASP.EFSPoC_qfqszy29ya4s6!EFSPoC
  (family-name hash in the AUMID can differ per signing cert - check Get-AppxPackage *EFSPoC*, or just launch "JASP EFS PoC" from the Start menu)
echo.
echo  RESULTS: %%LOCALAPPDATA%%\Packages\JASP.EFSPoC_*\LocalCache\poc-launcher.log and poc-engine.log
echo  Also run:  cipher /c "%%LOCALAPPDATA%%\Packages\JASP.EFSPoC_*\LocalCache\launcher-file.txt"
echo ================================================================================

endlocal
