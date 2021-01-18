@echo off
rem autorun
SETLOCAL EnableDelayedExpansion

rem ---------------------- Check arguments -------------------------------
rem Arguments are: <# CPU's to use> <64/32>bit <full/wix>
SET CPUS=%1

if "%1"=="" (
SET CPUS=%NUMBER_OF_PROCESSORS%
)
echo Running autobuild in %CPUS% separate processes!

SET ARCH=%2


if NOT "%2"=="32" (
if NOT "%2"=="64" (
echo "First argument should be architecture i.e 32 or 64 bits"
echo "So e.g. run: 'autobuild 32' if you want to build an x86 version."
echo "But for now the default of 64-bit will be set."
SET ARCH=64
))

SET BUILDSTYLE=%3

rem set the third argument to full for a full build, to wix afterwards if that is all you are interested.
if "%3"=="" (
SET BUILDSTYLE=full
)


echo "Building %ARCH% bits JASP"

rem I am assuming we are starting from (SOMEDIR)\jasp-desktop\Tools  and that (SOMEDIR) contains folders like jasp-required-files and stuff like that
rem At least by default
SET STARTDIR=%CD%

rem --- default values ---
SET JASP_BASE_DIR_DEFAULT=%STARTDIR%\..\..
SET QTDIR_DEFAULT=C:\Qt
SET QTVER_DEFAULT=5.15.2
SET RTOOLSDIR_DEFAULT=C:\Rtools
SET WIX_DEFAULT=C:\Program Files (x86)\WiX Toolset v3.11
SET MSVCDIR_DEFAULT=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community

rem ---------------------- Setting up environmet -------------------------------


rem for the rest we will check if the user has overridden any particular values manually
if "%JASP_BASE_DIR%"=="" (
    SET "JASP_BASE_DIR=%JASP_BASE_DIR_DEFAULT%"
    echo Using default JASP_BASE_DIR the folder containing required files and where the wix folder will be built, of:
    echo "%JASP_BASE_DIR_DEFAULT%", to change set the JASP_BASE_DIR environment variable to your install location
) else (
    echo JASP_BASE_DIR: "%JASP_BASE_DIR%"
)

SET JASP_DESKTOP_DEFAULT=%JASP_BASE_DIR%\jasp-desktop
SET JASP_REQUIRED_FILES_DIR_DEFAULT=%JASP_BASE_DIR%\jasp-required-files

if "%QTDIR%"=="" (
    SET "QTDIR=%QTDIR_DEFAULT%"
    echo Using default QTDIR "%QTDIR_DEFAULT%", to change set the QTDIR environment variable to your install location
) else (
    echo QTDIR: "%QTDIR%"
)

if "%QTVER%"=="" (
    SET "QTVER=%QTVER_DEFAULT%"
    echo Using default QTVER "%QTVER_DEFAULT%", to change set the QTVER environment variable to your preferred version
) else (
    echo QTVER: "%QTVER%"
)

if "%RTOOLSDIR%"=="" (
    SET "RTOOLSDIR=%RTOOLSDIR_DEFAULT%"
    echo Using default RTOOLSDIR "%RTOOLSDIR_DEFAULT%", to change set the RTOOLSDIR environment variable to your install location
) else (
    echo RTOOLSDIR: "%RTOOLSDIR%"
)

if "%WIX%"=="" (
    SET "WIX=%WIX_DEFAULT%"
    echo Using default WIX "%WIX_DEFAULT%", to change set the WIX environment variable to your install location
) else (
    echo WIX: "%WIX%"
)

if "%MSVCDIR%"=="" (
    SET "MSVCDIR=%MSVCDIR_DEFAULT%"
    echo Using default MSVCDIR "%MSVCDIR_DEFAULT%", to change set the MSVCDIR environment variable to your install location
) else (
    echo MSVCDIR: "%MSVCDIR%"
)

if "%JASP_DESKTOP%"=="" (
    SET "JASP_DESKTOP=%JASP_DESKTOP_DEFAULT%"
    echo Using default JASP_DESKTOP "%JASP_DESKTOP_DEFAULT%", to change set the JASP_DESKTOP environment variable to your folder
) else (
    echo JASP_DESKTOP: "%JASP_DESKTOP%"
)

if "%JASP_REQUIRED_FILES_DIR%"=="" (
    SET "JASP_REQUIRED_FILES_DIR=%JASP_REQUIRED_FILES_DIR_DEFAULT%"
    echo Using default JASP_REQUIRED_FILES_DIR "%JASP_REQUIRED_FILES_DIR_DEFAULT%", to change set the JASP_REQUIRED_FILES_DIR environment variable to your folder
) else (
    echo JASP_REQUIRED_FILES_DIR: "%JASP_REQUIRED_FILES_DIR%"
)

echo checking if script is ran from Tools
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
SET MINGWDIR=%RTOOLSDIR%\mingw_64\bin
SET QTVCDIR=%QTDIR%\%QTVER%\msvc2019_64\bin
SET WIXARCH="x64"
SET COPY_R_ARCH="x64"
) else (
SET MINGWDIR=%RTOOLSDIR%\mingw_32\bin
SET QTVCDIR=%QTDIR%\%QTVER%\msvc2019\bin
SET WIXARCH="x86"
SET COPY_R_ARCH="i386"
)

echo JASP will be built for %ARCH% bits

echo checking for existence specified directories

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

rem Setting up Visual Studio Environment
call %VCVARS_DIR%\vcvars%ARCH%.bat

SET PATH=%MINGWDIR%;%QTVCDIR%;%PATH%

rem you uncomment the following goto to skip building JASP, but it assumes that you did in fact build it previously in the normal location (see BUILDSTYLE argument though)
rem goto skipbuilding
rem goto copyR

if "%BUILDSTYLE%"=="wix" GOTO skipbuilding


rem We are not updating the sources from this script. if we are running from buildbot it will update the sources for us and
rem otherwise we will just use whatever the user currently has on drive. 

cd %JASP_BASE_DIR%

if "%BUILDSTYLE%"=="full" (
echo Make sure we have a fresh JASP_WIX_DIR etc
    rmdir /Q /S %JASP_WIX_DIR%
    mkdir %JASP_WIX_DIR%
) else (
    echo Reusing old JASP_WIX_DIR
)

cd %JASP_WIX_DIR%

if "%BUILDSTYLE%"=="full" (
    echo Creating a fresh build dir
    rmdir /Q /S %JASP_BUILD_DIR%
    mkdir %JASP_BUILD_DIR%
) else (
    echo reusing old build dir
)
    

if "%BUILDSTYLE%"=="full" (
    echo Copy the required files for JASP
    if not exist %JASP_REQUIRED_FILES_DIR%\%ARCH% (
        echo Sadly enough the required binaries-folder "%JASP_REQUIRED_FILES_DIR%\%ARCH%" does not exist!
        exit /b 3
    )
    xcopy %JASP_REQUIRED_FILES_DIR%\%ARCH% %JASP_BUILD_DIR% /S

    echo Make symbolic link to R
    if not exist %JASP_REQUIRED_FILES_DIR%\R (
        echo Sadly enough the required R-folder "%JASP_REQUIRED_FILES_DIR%\R" does not exist!
        exit /b 4
    )
    cd %JASP_BUILD_DIR%
    mklink /D R %JASP_REQUIRED_FILES_DIR%\R
) else (
    echo Keeping the required files as they were.
    cd %JASP_BUILD_DIR%
)

echo Check for the existence of CRYPTKEY in the local environment and set it as a property in qmake if so.
if not "%CRYPTKEY%"=="" (
    %QTVCDIR%\qmake -set ENVIRONMENT_CRYPTKEY "%CRYPTKEY%"
    echo Using custom cryptkey (%CRYPTKEY%)
)

echo "Making sure QM files are generated"
%QTVCDIR%\qmake -set AM_I_BUILDBOT "I_AM_BUILDBOT" || exit 1

if "%BUILDSTYLE%"=="full" (
    echo Creating directory for JASP-R-Interface "%JASP_R_INTERFACE%"
    mkdir %JASP_R_INTERFACE%
)
cd %JASP_R_INTERFACE%
rem echo %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro
echo Building JASP-R-Interface in %JASP_R_INTERFACE%
if "%BUILDSTYLE%"=="full" (
    %QTVCDIR%\qmake.exe %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro -spec win32-g++
)
%MINGWDIR%\mingw32-make.exe  -j%CPUS% || exit /B 5

cd ..
echo "Building JASP"
if "%BUILDSTYLE%"=="full" (
    %QTVCDIR%\qmake.exe %JASP_DESKTOP%\JASP.pro -spec win32-msvc
)
%JOM% -j%CPUS%  || exit /B 6

%QTVCDIR%\qmake -set ENVIRONMENT_CRYPTKEY ""
%QTVCDIR%\qmake -set AM_I_BUILDBOT "" || exit 1

:setup

echo "Preparing the installer"

cd %JASP_BASE_DIR%\%JASP_WIX_DIR%
echo Creating a fresh install dir
rmdir /Q /S %JASP_INSTALL_DIR%
mkdir  %JASP_INSTALL_DIR%

cd %JASP_INSTALL_DIR%

echo copy icon
COPY %JASP_DESKTOP%\JASP-Desktop\icon.ico /Y

echo copy AGPL txt
COPY %JASP_DESKTOP%\COPYING.txt  AGPL.txt /Y

echo copying JASP.exe, JASPEngine.exe, *.R and *.dll from build dir
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\JASP.exe /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\JASPEngine.exe /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\*.R /Y
COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\*.dll /Y

echo Running windeployqt on JASP.exe
%QTVCDIR%\windeployqt.exe --no-compiler-runtime -core -gui -webenginewidgets -webchannel -svg -network -printsupport -xml -qml -quick -quickwidgets --qmldir %JASP_DESKTOP%\JASP-Desktop JASP.exe

echo copy resources
XCOPY  %JASP_DESKTOP%\Resources /E /I Resources

echo copy help
XCOPY  %JASP_DESKTOP%\Resources\Help /E /I Help

:copyR
set "RLOCATION=%JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%\R"
echo running copyR.cmd script as "%JASP_DESKTOP%\Tools\copyR.cmd %JASP_REQUIRED_FILES_DIR%\R %RLOCATION% %COPY_R_ARCH%"

call %JASP_DESKTOP%\Tools\copyR.cmd %JASP_REQUIRED_FILES_DIR%\R %RLOCATION% %COPY_R_ARCH%

echo copy JAGS to installer preparation folder
XCOPY %JASP_REQUIRED_FILES_DIR%\%ARCH%\JAGS /E /I %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%\JAGS

:skipbuilding

echo Making zip-version of installer
powershell Compress-Archive "%JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%\*" "%JASP_BASE_DIR%\%JASP_WIX_DIR%\JASP.zip"

echo Melting and Coalescing MSI

cd %JASP_BASE_DIR%\%JASP_WIX_DIR%

SET MERGEMODULENAME=Microsoft_VC142_CRT_%WIXARCH%.msm

echo VCToolsRedistDir: "%VCToolsRedistDir%"

COPY "%VCToolsRedistDir%\MergeModules\%MERGEMODULENAME%" /Y
"%WIX%\bin\heat.exe" dir .\%JASP_INSTALL_DIR% -cg JASPFiles -gg -scom -sreg -sfrag -srd -dr APPLICATIONFOLDER -var var.JASP_INSTALL_DIR -out JASPFilesFragment.wxs || exit /B 7

COPY %JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_BUILD_DIR%\jasp.wxi /Y
"%WIX%\bin\candle" -arch %WIXARCH% -dJASP_INSTALL_DIR=%JASP_BASE_DIR%\%JASP_WIX_DIR%\%JASP_INSTALL_DIR%  JASPFilesFragment.wxs  || exit /B 8

COPY %JASP_DESKTOP%\Tools\wix\jasp.wxs /Y
COPY %JASP_DESKTOP%\Tools\wix\jaspLicense.rtf /Y
"%WIX%\bin\candle" -dRedistMergeModule=%MERGEMODULENAME% -arch %WIXARCH% -dJASP_DESKTOP_DIR=%JASP_DESKTOP% -ext WixUIExtension -ext WixUtilExtension jasp.wxs || exit /B 9

"%WIX%\bin\light" -sval -dRedistMergeModule=%MERGEMODULENAME% -ext WixUIExtension -ext WixUtilExtension -out JASP.msi JASPFilesFragment.wixobj jasp.wixobj || exit /B 10

if "%BUILDSTYLE%"=="wix" GOTO end

cd %STARTDIR%

echo Remove readonly from r library again to avoid errors later on
attrib -r %RLOCATION%\library /d /s

:end
endlocal
