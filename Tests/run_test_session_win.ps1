# Windows session runner for JASP accessibility tests.
# Mirrors Tests/run_test_session.sh:
#   .\run_test_session_win.ps1 -Test .\test_accessibility_win.py [-Narrator] [-KeepJasp] [-AxDebug]
#
# Starts JASP detached (survives this script), waits for it, exports JASP_PID,
# optionally toggles Narrator, runs the test with the a11y venv python.

param(
    [Parameter(Mandatory=$true)][string]$Test,
    [switch]$Narrator,
    [switch]$KeepJasp,
    [switch]$NoAxDebug,
    [string]$DataDir = "",
    [string]$FileArg = ""
)
$ErrorActionPreference = "Continue"

$repo = "C:\Virtuoos\jasp-desktop"
$buildDir = "$repo\jasp-build-screenreader"
$venvPy = "$repo\Tests\a11y-venv\Scripts\python.exe"
$qtPrefix = "C:\Qt\6.12.0-dev-msvc2022"

if (-not (Test-Path $venvPy)) { Write-Host "FATAL: venv python missing: $venvPy"; exit 2 }
if (-not (Test-Path "$buildDir\JASPDesktop.exe")) { Write-Host "FATAL: JASP not built: $buildDir"; exit 2 }

# ── start JASP detached (WMI so it outlives this console) ────────────
$existing = Get-Process -Name JASPDesktop -ErrorAction SilentlyContinue
if ($existing) {
    Write-Host "JASP already running (pid $($existing.Id)) - attaching"
    $jaspPid = $existing.Id
} else {
    Write-Host "Starting JASP (detached)..."
    $tmpCmd = "$env:TEMP\jasp-launch-$PID.cmd"
    $fileQuoted = if ($FileArg -ne "") { '"' + $FileArg.Trim('"') + '"' } else { "" }
    @"
@echo off
set PATH=$qtPrefix\bin;%PATH%
set R_HOME=$buildDir\R
set QTWEBENGINE_AX_DEBUG=$(if ($NoAxDebug) { "0" } else { "1" })
rem CDP: lets tests verify DOM-level a11y (roles, tabindex, focus moves)
set QTWEBENGINE_CHROMIUM_FLAGS=--remote-debugging-port=9223
cd /d $buildDir
JASPDesktop.exe --safeGraphics $fileQuoted > C:\Virtuoos\tools\jasp-run.log 2> C:\Virtuoos\tools\jasp-run-err.log
"@ | Set-Content -Path $tmpCmd -Encoding ascii
    $r = Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{ CommandLine = "cmd /c `"$tmpCmd`"" }
    if (-not $r.ProcessId) { Write-Host "FATAL: could not spawn JASP"; exit 2 }
    # the cmd wrapper exits quickly; find the real JASPDesktop process
    $jaspPid = $null
    for ($i = 0; $i -lt 40; $i++) {
        Start-Sleep -Milliseconds 500
        $p = Get-Process -Name JASPDesktop -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($p) { $jaspPid = $p.Id; break }
    }
    if (-not $jaspPid) { Write-Host "FATAL: JASPDesktop did not start"; exit 2 }
    Write-Host "JASP started (pid $jaspPid)"
}

# ── wait for the main window ─────────────────────────────────────────
for ($i = 0; $i -lt 60; $i++) {
    Start-Sleep -Seconds 1
    $p = Get-Process -Name JASPDesktop -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($p -and $p.MainWindowTitle) { break }
}
Write-Host "JASP ready (pid $jaspPid)"

# ── optional Narrator ────────────────────────────────────────────────
$narratorStarted = $false
if ($Narrator -and -not (Get-Process -Name Narrator -ErrorAction SilentlyContinue)) {
    Start-Process "C:\Windows\System32\Narrator.exe" | Out-Null
    $narratorStarted = $true
    Write-Host "Narrator started - waiting for it to settle..."
    Start-Sleep -Seconds 15
}

# ── run the test ─────────────────────────────────────────────────────
$env:JASP_PID = $jaspPid
if ($FileArg -ne "") { $env:JASP_FILE_LOADED = "1" }
$env:QTWEBENGINE_AX_DEBUG = $(if ($NoAxDebug) { "0" } else { "1" })
$env:PATH = "$qtPrefix\bin;$env:PATH"
$env:R_HOME = "$buildDir\R"
if ($DataDir -ne "") { $env:JASP_DATA_DIR = $DataDir }
$env:JASP_CDP_PORT = "9223"

Write-Host "=== running test: $Test ==="
& $venvPy $Test
$exitCode = $LASTEXITCODE

# ── cleanup ──────────────────────────────────────────────────────────
if ($narratorStarted) {
    Get-Process -Name Narrator -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
}
if (-not $KeepJasp) {
    Get-Process -Name JASPDesktop, JASPEngine, QtWebEngineProcess -ErrorAction SilentlyContinue |
        Stop-Process -Force -ErrorAction SilentlyContinue
}
Write-Host "=== test exit code: $exitCode ==="
exit $exitCode
