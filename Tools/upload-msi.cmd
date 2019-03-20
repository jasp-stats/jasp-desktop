@echo off
REM autorun
SETLOCAL EnableDelayedExpansion

SET ARCH=%1

REM ---------------------- Check arguments -------------------------------
if NOT "%1"=="32" (
if NOT "%1"=="64" (
echo "First argument could be architecture, i.e 32 or 64 bits, to upload"
echo "But for now the default of 64 will be assumed."
SET ARCH=64
))

FOR /F "tokens=*" %g IN ('git rev-parse --abbrev-ref HEAD') do (SET GIT_BRANCH=%g)
FOR /F "tokens=*" %g IN ('git rev-parse --verify HEAD')     do (SET GIT_COMMIT=%g)

scp ../../jasp-wix-installer-%ARCH%/JASP.dmg nachtjapon@static.jasp-stats.org:~/Nightlies/Windows/JASP-nightly-%GIT_BRANCH%-%GIT_COMMIT%.msi
