ECHO OFF

CD Install

REM --- Update JASP install Folder ------
SET JASP-BUILD=c:\Jasp\Build
SET BUILD_DIR_32=%JASP-BUILD%\build-JASP-Release-32
SET BUILD_DIR_64=%JASP-BUILD%\build-JASP-Release-64
SET JASP-DESKTOP=%JASP-BUILD%\jasp-desktop
SET QT-MSVC-BIN-2015=C:\Qt\5.10.1\msvc2015\bin
SET QT-MSVC-BIN-2017=C:\Qt\5.10.1\msvc2017_64\bin

REM --- Copy icon ---
COPY %JASP-DESKTOP%\JASP-Desktop\icon.ico /Y


REM --- Update Jasp and Engine -----
COPY %BUILD_DIR_32%\JASP.exe /Y _x86
COPY %BUILD_DIR_32%\JASPEngine.exe /Y _x86
COPY %BUILD_DIR_32%\*.dll /Y _x86

COPY %BUILD_DIR_64%\JASP.exe /Y _x64
COPY %BUILD_DIR_64%\JASPEngine.exe /Y _x64
COPY %BUILD_DIR_64%\*.dll /Y _x64

REM ---- QT Core -------
REM %QT-MSVC-BIN-2015%\windeployqt.exe %BUILD_DIR_32%\JASP.exe --no-patchqt --release --libdir ./_x86 --plugindir _x86
REM %QT-MSVC-BIN-2017%\windeployqt.exe %BUILD_DIR_64%\JASPEngine.exe --compiler-runtime --release --libdir _x64
REM COPY %QT-MSVC-BIN-2017%\Qt5Core.dll _x64

cd _x86
%QT-MSVC-BIN-2015%\windeployqt.exe --compiler-runtime --release JASP.exe 
cd ..

cd _x64
%QT-MSVC-BIN-2017%\windeployqt.exe -core -gui -webenginewidgets -webchannel -svg -network -printsupport -xml -qml -quick -quickwidgets --qmldir C:\Jasp\Build\jasp-desktop\JASP-Desktop\resources JASP.exe 
cd ..
 
REM ---- Update Resources -------
RMDIR Resources /S /Q
XCOPY %JASP-DESKTOP%\Resources /E /I Resources

REM ---- Update Help -------
RMDIR Help /S /Q
XCOPY %JASP-DESKTOP%\Resources\Help /E /I Help

REM --- Update R ------
REM RMDIR R /S /Q
REM XCOPY %BUILD_DIR_64%\R /E /I R
REM XCOPY %BUILD_DIR_32%\R /E /Y R

CD ..

ECHO .........Done ..........
PAUSE

