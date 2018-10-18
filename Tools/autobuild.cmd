REM autorun

REM ---------------------- Check arguments -------------------------------
if NOT "%1"=="32" (
if NOT "%1"=="64" (
echo "First arfument should be architecture i.e 32 or 64 bits"
echo "So e.g. run: 'autobuild 64' "
exit /b
))

echo "Correct arguments"
SET ARCH=%1

REM ---------------------- Setting up environmet -------------------------------
SET JASP_BASE_DIR=c:\Jasp\Build
SET JASP_INSTALL_DIR=c:\Jasp\Install-%ARCH%
SET JASP_BUILD_DIR=jasp-build-%ARCH%
SET JASP_GIT_DIR=jasp-desktop
SET JASP_DESKTOP=%JASP_BASE_DIR%\%JASP_GIT_DIR%
SET JASP_REQUIRED_FILES_DIR=jasp-required-files
SET JASP_R_INTERFACE=JASP-R-Interface
SET JOM=C:\Qt\Tools\QtCreator\bin\jom.exe
SET NSIS=C:\PROGRA~2\NSIS
SET VCVARS_DIR="C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build"

if "%ARCH%" == "64" (
echo "Mode 64 bits"
SET MINGWDIR=C:\Rtools\mingw_64\bin
SET QTVCDIR=C:\Qt\5.10.1\msvc2017_64\bin
SET INSTALLBIN=_x64
) else (
echo "Mode 32 bits"
SET MINGWDIR=C:\Rtools\mingw_32\bin
SET QTVCDIR=C:\Qt\5.10.1\msvc2015\bin
SET INSTALLBIN=_x86
)

REM Setting up Visual Studio Environment
call %VCVARS_DIR%\vcvars%ARCH%.bat

SET PATH=%MINGWDIR%;%QTVCDIR%;%PATH%


REM ---------------------- Update sources -------------------------------
REM and go to Root of Build
REM Get the latest version of development from github!
cd %JASP_DESKTOP%
git fetch origin
git checkout development
git pull
cd ..


REM ------------------------ Building JASP ----------------------
REM Cleanup
rmdir /Q /S %JASP_BUILD_DIR%
mkdir %JASP_BUILD_DIR%

REM Copy the JASP Required libraries
xcopy %JASP_REQUIRED_FILES_DIR%\%ARCH% %JASP_BUILD_DIR% /S

REM Make symbolic link to R
cd %JASP_BUILD_DIR%
mklink /D R ..\R

REM Build JASP-R-Interface
echo Start building %JASP_R_INTERFACE%
mkdir %JASP_R_INTERFACE%
cd %JASP_R_INTERFACE%
echo %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro
%QTVCDIR%\qmake.exe %JASP_DESKTOP%\%JASP_R_INTERFACE%\%JASP_R_INTERFACE%.pro -spec win32-g++
%MINGWDIR%\mingw32-make.exe

REM Build JASP
cd ..
echo "Start Building JASP"
%QTVCDIR%\qmake.exe %JASP_DESKTOP%\JASP.pro -spec win32-msvc
%JOM% -j4

:setup

REM ---------------------- Make setup ---------------------------------
REM ----- Prepair the Install folder -------
echo "Prepairing the installer"

REM Cleanup
rmdir /Q /S %JASP_INSTALL_DIR%
mkdir  %JASP_INSTALL_DIR%

cd %JASP_INSTALL_DIR%
mkdir %INSTALLBIN%

REM --- Copy icon ---
cd %JASP_INSTALL_DIR%
COPY %JASP_DESKTOP%\JASP-Desktop\icon.ico /Y

REM --- AGPL.txt ---
COPY %JASP_DESKTOP%\COPYING.txt  AGPL.txt /Y

REM --- Update Jasp and Engine -----
echo %JASP_BASE_DIR%\%JASP_BUILD_DIR%
COPY %JASP_BASE_DIR%\%JASP_BUILD_DIR%\JASP.exe /Y %INSTALLBIN%
COPY %JASP_BASE_DIR%\%JASP_BUILD_DIR%\JASPEngine.exe /Y %INSTALLBIN%
COPY %JASP_BASE_DIR%\%JASP_BUILD_DIR%\*.R /Y %INSTALLBIN%
COPY %JASP_BASE_DIR%\%JASP_BUILD_DIR%\*.dll /Y %INSTALLBIN%

REM ---- VcRedist -------
COPY %JASP_BASE_DIR%\vcredist\vcredist%INSTALLBIN%.exe  /Y  %INSTALLBIN%

cd %INSTALLBIN%
%QTVCDIR%\windeployqt.exe -core -gui -webenginewidgets -webchannel -svg -network -printsupport -xml -qml -quick -quickwidgets --qmldir %JASP_DESKTOP%\JASP-Desktop JASP.exe
cd ..

REM ---- Update Resources -------
XCOPY  %JASP_DESKTOP%\Resources /E /I Resources

REM ---- Update Help -------
XCOPY  %JASP_DESKTOP%\Resources\Help /E /I Help

REM --- Update R ------
mklink /D R %JASP_BASE_DIR%\R
cd ..

echo "Making the installer..."
%NSIS%\makensis.exe C:\Jassp\Build\jasp-desktop\Tools\make-win-installer-maker.nsi
%NSIS%\makensis.exe C:\Jasp\Build\jasp-desktop\Tools\make-win-installer.nsi

:end
REM --------------------------------------------------------------------


