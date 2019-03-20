@echo off
REM autorun
SETLOCAL EnableDelayedExpansion

SET ARCH=%1

REM ---------------------- Check arguments -------------------------------
if NOT "%1"=="32" (
if NOT "%1"=="64" (
echo "First argument should be architecture i.e 32 or 64 bits"
echo "So e.g. run: 'autobuild 32' if you want to build an x86 version."
echo "But for now the default of 64-bit will be set."
SET ARCH=64
))


echo "Building %ARCH% bits JASP"

REM I am assuming we are starting from (SOMEDIR)\jasp-desktop\Tools  and that (SOMEDIR) contains folders like jasp-required-files and stuff like that
REM At least by default
SET STARTDIR=%CD%

rem --- default values ---
SET JASP_BASE_DIR_DEFAULT=%STARTDIR%\..\..
SET QTDIR_DEFAULT=C:\Qt
SET QTVER_DEFAULT=5.12.1
SET RTOOLSDIR_DEFAULT=C:\Rtools
SET WIX_DEFAULT=C:\Program Files (x86)\WiX Toolset v3.11
SET MSVCDIR_DEFAULT=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community

REM ---------------------- Setting up environmet -------------------------------


REM for the rest we will check if the user has overridden any particular values manually
if "%JASP_BASE_DIR%"=="" (
    SET "JASP_BASE_DIR=%JASP_BASE_DIR_DEFAULT%"
    echo Using default JASP_BASE_DIR the folder containing required files and where the wix folder will be built, of:
    echo "%JASP_BASE_DIR_DEFAULT%", to change set the JASP_BASE_DIR environment variable to your install location
)

SET JASP_DESKTOP_DEFAULT=%JASP_BASE_DIR%\jasp-desktop
SET JASP_REQUIRED_FILES_DIR_DEFAULT=%JASP_BASE_DIR%\jasp-required-files

if "%QTDIR%"=="" (
    SET "QTDIR=%QTDIR_DEFAULT%"
    echo Using default QTDIR "%QTDIR_DEFAULT%", to change set the QTDIR environment variable to your install location
)

if "%QTVER%"=="" (
    SET "QTVER=%QTVER_DEFAULT%"
    echo Using default QTVER "%QTVER_DEFAULT%", to change set the QTVER environment variable to your preferred version
)

if "%RTOOLSDIR%"=="" (
    SET "RTOOLSDIR=%RTOOLSDIR_DEFAULT%"
    echo Using default RTOOLSDIR "%RTOOLSDIR_DEFAULT%", to change set the RTOOLSDIR environment variable to your install location
)

if "%WIX%"=="" (
    SET "WIX=%WIX_DEFAULT%"
    echo Using default WIX "%WIX_DEFAULT%", to change set the WIX environment variable to your install location
)

if "%MSVCDIR%"=="" (
    SET "MSVCDIR=%MSVCDIR_DEFAULT%"
    echo Using default MSVCDIR "%MSVCDIR_DEFAULT%", to change set the MSVCDIR environment variable to your install location
)

if "%JASP_DESKTOP%"=="" (
    SET "JASP_DESKTOP=%JASP_DESKTOP_DEFAULT%"
    echo Using default JASP_DESKTOP "%JASP_DESKTOP_DEFAULT%", to change set the JASP_DESKTOP environment variable to your folder
)

if "%JASP_REQUIRED_FILES_DIR%"=="" (
    SET "JASP_REQUIRED_FILES_DIR=%JASP_REQUIRED_FILES_DIR_DEFAULT%"
    echo Using default JASP_REQUIRED_FILES_DIR "%JASP_REQUIRED_FILES_DIR_DEFAULT%", to change set the JASP_REQUIRED_FILES_DIR environment variable to your folder
)

rem lets check if we are in Tools
cd ..
if not exist Tools (
    echo This script MUST be run from %JASP_DESKTOP%\Tools! 
    exit /b 2
)
cd %STARTDIR%

SET JASP_WIX_DIR=jasp-wix-installer-%ARCH%
SET JASP_INSTALL_DIR=jasp-installer-files-%ARCH%
SET JASP_BUILD_DIR=jasp-build-%ARCH%
SET JASP_R_INTERFACE=JASP-R-Interface
SET JOM=%QTDIR%\Tools\QtCreator\bin\jom.exe

SET VCVARS_DIR="%MSVCDIR%\VC\Auxiliary\Build"

if "%ARCH%" == "64" (
echo "Mode 64 bits"

SET MINGWDIR=%RTOOLSDIR%\mingw_64\bin
SET QTVCDIR=%QTDIR%\%QTVER%\msvc2017_64\bin
SET WIXARCH="x64"
SET COPY_R_ARCH="x64"
) else (
echo "Mode 32 bits"

SET MINGWDIR=%RTOOLSDIR%\mingw_32\bin
SET QTVCDIR=%QTDIR%\%QTVER%\msvc2017\bin
SET WIXARCH="x86"
SET COPY_R_ARCH="i386"
)

rem now we are going to make sure that every directory that we've got now is actually there!

if not exist "%JASP_BASE_DIR%" (
    echo The desired JASP_BASE_DIR directory "%JASP_BASE_DIR%" does not exist!
    exit /b 11
)

if not exist "%JASP_DESKTOP%" (
    echo The desired jasp git directory "%JASP_DESKTOP%" does not exist!
    exit /b 11
)

if not exist "%JASP_REQUIRED_FILES_DIR%" (
    echo The desired jasp required files directory "%JASP_REQUIRED_FILES_DIR%" does not exist!
    exit /b 11
)

if not exist "%QTVCDIR%" (
    echo The desired QT binary directory "%QTVCDIR%" does not exist!
    exit /b 11
)

if not exist "%MINGWDIR%" (
    echo The desired Rtools binary directory "%MINGWDIR%" does not exist!
    exit /b 11
)

if not exist "%MSVCDIR%" (
    echo The desired Visual Studio directory "%MSVCDIR%" does not exist!
    exit /b 11
)

if not exist %VCVARS_DIR% (
    echo The desired Visual Studio environment setup CMD location %VCVARS_DIR% does not exist!
    exit /b 11
)

SET CPUS=6
REM setting it to 1 might avoid errors but also makes it take an hour...
REM %NUMBER_OF_PROCESSORS% manually changed to 1... because MSVC because loves to get all tangled up in internal errors when specifying "big" numbers like 8 or 24 :s

REM Setting up Visual Studio Environment
call %VCVARS_DIR%\vcvars%ARCH%.bat

SET PATH=%MINGWDIR%;%QTVCDIR%;%PATH%

REM you uncomment the following goto to skip building JASP, but it assumes that you did in fact build it previously in the normal location
rem goto skipbuilding
rem goto copyR


REM We are not updating the sources from this script. If we are running from buildbot it will update the sources for us and
REM otherwise we will just use whatever the user currently has on drive. 

cd %JASP_BASE_DIR%

REM ------------------------ Building JASP ----------------------
REM Make sure we have a fresh JASP_WIX_DIR etc
rmdir /Q /S %JASP_WIX_DIR%
mkdir %JASP_WIX_DIR%
cd %JASP_WIX_DIR%

rmdir /Q /S %JASP_BUILD_DIR%
mkdir %JASP_BUILD_DIR%

REM Copy the JASP Required libraries
if not exist %JASP_REQUIRED_FILES_DIR%\%ARCH% (
    echo Sadly enough the required binaries-folder "%JASP_REQUIRED_FILES_DIR%\%ARCH%" does not exist!
    exit /b 3
)
xcopy %JASP_REQUIRED_FILES_DIR%\%ARCH% %JASP_BUILD_DIR% /S

REM Make symbolic link to R
if not exist %JASP_REQUIRED_FILES_DIR%\R (
    echo Sadly enough the required R-folder "%JASP_REQUIRED_FILES_DIR%\R" does not exist!
    exit /b 4
)
cd %JASP_BUILD_DIR%
mklink /D R %JASP_REQUIRED_FILES_DIR%\R

REM set the cryptkey if it is in the environment
if not "%CRYPTKEY%"=="" (
    %QTVCDIR%\qmake -set ENVIRONMENT_CRYPTKEY %CRYPTKEY%
    echo Using custom cryptkey!
)

REM Build JASP-R-Interface
echo Building %JASP_R_INTERFACE%
mkdir %JASP_R_INTERFACE%
cd %JASP_R_INTERFACE%
echo %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro
%QTVCDIR%\qmake.exe %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro -spec win32-g++
%MINGWDIR%\mingw32-make.exe  -j%CPUS% || exit /B 5

REM Build JASP
cd ..
echo "Building JASP"
%QTVCDIR%\qmake.exe %JASP_DESKTOP%\JASP.pro -spec win32-msvc
%JOM% -j%CPUS%  || exit /B 6

%QTVCDIR%\qmake -set ENVIRONMENT_CRYPTKEY ""

:setup

REM ---------------------- Make setup ---------------------------------
REM ----- Prepare the Install folder -------
echo "Preparing the installer"

REM Cleanup
cd %JASP_BASE_DIR%\%JASP_WIX_DIR%
rmdir /Q /S %JASP_INSTALL_DIR%
mkdir  %JASP_INSTALL_DIR%

cd %JASP_INSTALL_DIR%

REM --- Copy icon ---
COPY %JASP_DESKTOP%\JASP-Desktop\icon.ico /Y

REM --- AGPL.txt ---
COPY %JASP_DESKTOP%\COPYING.txt  AGPL.txt /Y

REM --- Update Jasp and Engine -----
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\JASP.exe /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\JASPEngine.exe /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\*.R /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\*.dll /Y

%QTVCDIR%\windeployqt.exe --no-compiler-runtime -core -gui -webenginewidgets -webchannel -svg -network -printsupport -xml -qml -quick -quickwidgets --qmldir %JASP_DESKTOP%\JASP-Desktop JASP.exe

REM ---- Update Resources -------
XCOPY  %JASP_DESKTOP%\Resources /E /I Resources

REM ---- Update Help -------
XCOPY  %JASP_DESKTOP%\Resources\Help /E /I Help

:copyR
REM --- Update R ------
call %JASP_DESKTOP%\Tools\copyR.cmd %JASP_REQUIRED_FILES_DIR%\R %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%\R %COPY_R_ARCH%

:skipbuilding

echo Melting and Coalescing MSI

cd %JASP_BASE_DIR%\%JASP_WIX_DIR%

SET MERGEMODULENAME=Microsoft_VC141_CRT_%WIXARCH%.msm

COPY "%VCToolsRedistDir%\MergeModules\%MERGEMODULENAME%" /Y
"%WIX%\bin\heat.exe" dir .\%JASP_INSTALL_DIR% -cg JASPFiles -gg -scom -sreg -sfrag -srd -dr INSTALLLOCATION -var var.JASP_INSTALL_DIR -out JASPFilesFragment.wxs || exit /B 7

COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\jasp.wxi /Y
"%WIX%\bin\candle" -arch %WIXARCH% -dJASP_INSTALL_DIR=%JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%  JASPFilesFragment.wxs  || exit /B 8

COPY %JASP_DESKTOP%\Tools\wix\jasp.wxs /Y
"%WIX%\bin\candle" -dRedistMergeModule=%MERGEMODULENAME% -arch %WIXARCH% -dJASP_DESKTOP_DIR=%JASP_DESKTOP% -ext WixUIExtension -ext WixUtilExtension jasp.wxs || exit /B 9

"%WIX%\bin\light" -dRedistMergeModule=%MERGEMODULENAME% -ext WixUIExtension -ext WixUtilExtension -out JASP.msi JASPFilesFragment.wixobj jasp.wixobj || exit /B 10

cd %STARTDIR%

:end
REM --------------------------------------------------------------------
endlocal
