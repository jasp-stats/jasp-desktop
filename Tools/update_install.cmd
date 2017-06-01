ECHO OFF

REM --- Update JASP install Folder ------
SET JASP-BUILD=c:\Jasp\Build
SET BUILD_DIR_32=%JASP-BUILD%\build-JASP-Release-32
SET BUILD_DIR_64=%JASP-BUILD%\build-JASP-Release-64
SET JASP-DESKTOP=%JASP-BUILD%\jasp-desktop

COPY %JASP-DESKTOP%\JASP-Desktop\icon.ico /Y

REM --- Update Jasp and Engine -----
COPY %BUILD_DIR_32%\JASP.exe /Y _x86
COPY %BUILD_DIR_32%\JASPEngine.exe /Y _x86
COPY %BUILD_DIR_64%\JASP.exe /Y _x64
COPY %BUILD_DIR_64%\JASPEngine.exe /Y _x64

REM ---- Update Resources -------
RMDIR Resources /S /Q
XCOPY %JASP-DESKTOP%\Resources /E /I Resources

REM ---- Update Help -------
RMDIR Help /S /Q
XCOPY %JASP-DESKTOP%\Resources\Help /E /I Help

REM --- Update R ------
RMDIR R /S /Q
XCOPY %BUILD_DIR_64%\R /E /I R
REM XCOPY %BUILD_DIR_32%\R /E /Y R


ECHO .........Done ..........
