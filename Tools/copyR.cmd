@echo off
setlocal EnableDelayedExpansion

set startdir=%CD%


if "%1" == "" (
    echo Must give the path to R source as first argument to copyR.cmd!
    exit /b
)
set R_ROOT=%1

if "%2" == "" (
    echo Must give the path to the builddir as second argument to copyR.cmd!
    exit /b
)
set BUILDDIR=%2

if "%3" == "" (
    echo Must give the path to R destination as third argument to copyR.cmd!
    exit /b
)
set R_DEST=%3


set ARCH=%4
if "%4"=="" (
    set ARCH=x64
    echo ARCH was not specified so defaulting to x64
)

set RENV_DEST=%5
if "%5"=="" (
    echo Must give the path to the folder with renv-cache in it as fifth argument
    exit /b
)

set JASP_DESKTOP=%6
if "%6"=="" (
    echo Must give the path to jasp-desktop in it as sixth argument
    exit /b
)

echo Copying R %ARCH% from %R_ROOT% to %R_DEST%

rmdir /Q /S %R_DEST%
mkdir  %R_DEST%

cd %R_DEST%
mkdir bin
cd bin
copy %R_ROOT%\bin\*.exe /Y
copy %R_ROOT%\bin\*.sh /Y
xcopy %R_ROOT%\bin\%ARCH% /E /I %ARCH%
cd ..

mkdir modules
xcopy %R_ROOT%\modules\%ARCH% /E /I modules\%ARCH%

xcopy %R_ROOT%\etc /E /I etc
xcopy %R_ROOT%\etc /E /I Tcl
xcopy %R_ROOT%\share /E /I share
xcopy %R_ROOT%\include /E /I include

Echo Copying necessary parts of R-library
mkdir library

pushd %R_ROOT%\library
for /D %%G in ("*") DO (
    mkdir %R_DEST%\library\%%G
    pushd %R_DEST%\library\%%G
    call %JASP_DESKTOP%\Tools\copyRSub.cmd library\%%G %R_ROOT% %R_DEST% %%G
    popd
)
popd

echo copying renv-cache from %BUILDDIR% to %RENV_DEST%
cd %RENV_DEST%
mkdir renv-cache
cd renv-cache

rem structure of renv-cache: renv-cache/RENV_VERSION?/PKGNAME/VERSION/CHECKSUM_OR_SOMETHING/PKGNAME_AGAIN/THE_PKG
rem sadly enough windows doesn't allow for actually descriptive filenames in this loop... So dictionary:
rem G=RENV_VERSION H=PKGNAME I=VERSION J=CHECKSUM
Echo Copying necessary parts of R-libraries in renv-cache
cd %BUILDDIR%\renv-cache
for /D %%G in ("*") DO (
    rem setlocal EnableDelayedExpansion
    rem set RENV_VERSION=%%G
    pushd %%G

    for /D %%H in ("*") DO (
        rem setlocal EnableDelayedExpansion
        rem set PKGNAME=%%H
        pushd %%H

        for /D %%I in ("*") DO (
            rem setlocal EnableDelayedExpansion
            rem set PKGVERSION=%%i
            pushd %%I

            for /D %%J in ("*") DO (
                rem setlocal EnableDelayedExpansion
                rem set PKGCHECKSUM=%%J
                pushd %%J\%%H

                call %JASP_DESKTOP%\Tools\copyRSub.cmd renv-cache\%%G\%%H\%%I\%%J\%%H %BUILDDIR% %RENV_DEST% %%H
                
                rem ENDLOCAL
                popd
            )
            rem ENDLOCAL
            popd
        )
        rem ENDLOCAL
        popd
    )
    rem ENDLOCAL
    popd
)

echo Making R and renv-cache readonly
attrib +r %RENV_DEST%\renv-cache /d /s
attrib +r %R_DEST%\library       /d /s

echo Done!
cd %STARTDIR%

endlocal
