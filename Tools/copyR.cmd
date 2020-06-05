@echo off
SETLOCAL EnableDelayedExpansion

set startdir=%CD%


if "%1" == "" (
    echo Must give the path to R source as first argument to copyR.cmd!
    exit /b
)
set SOURCEDIR=%1

if "%2" == "" (
    echo Must give the path to R destination as second argument to copyR.cmd!
    exit /b
)
set DESTDIR=%2


SET ARCH=%3
if "%3"=="" (
    SET ARCH=x64
    echo ARCH was not specified so defaulting to x64
)

echo Copying R %ARCH% from %SOURCEDIR% to %DESTDIR%

rmdir /Q /S %DESTDIR%
mkdir  %DESTDIR%

cd %DESTDIR%
mkdir bin
cd bin
COPY %SOURCEDIR%\bin\*.exe /Y
COPY %SOURCEDIR%\bin\*.sh /Y
XCOPY %SOURCEDIR%\bin\%ARCH% /E /I %ARCH%
cd ..

mkdir modules
XCOPY %SOURCEDIR%\modules\%ARCH% /E /I modules\%ARCH%

XCOPY %SOURCEDIR%\etc /E /I etc
XCOPY %SOURCEDIR%\share /E /I share

mkdir library
cd library

Echo Copying necessary parts of R-libraries
cd %SOURCEDIR%\library
FOR /D %%G in ("*") DO (
    echo %%G
    cd %SOURCEDIR%\library

    mkdir %DESTDIR%\library\%%G
    cd %DESTDIR%\library\%%G
    
    COPY %SOURCEDIR%\library\%%G\INDEX INDEX /Y >nul
    COPY %SOURCEDIR%\library\%%G\NAMESPACE NAMESPACE /Y >nul
    COPY %SOURCEDIR%\library\%%G\DESCRIPTION DESCRIPTION /Y >nul
    COPY %SOURCEDIR%\library\%%G\*.R . /Y >nul
    
    if exist %SOURCEDIR%\library\%%G\R    ( XCOPY %SOURCEDIR%\library\%%G\R    /Q /E /I R >nul )
    if exist %SOURCEDIR%\library\%%G\Meta ( XCOPY %SOURCEDIR%\library\%%G\Meta /Q /E /I Meta >nul )
    if exist %SOURCEDIR%\library\%%G\po   ( XCOPY %SOURCEDIR%\library\%%G\po   /Q /E /I po >nul )

    if exist %SOURCEDIR%\library\%%G\afm  ( XCOPY %SOURCEDIR%\library\%%G\afm  /Q /E /I R >nul )
    if exist %SOURCEDIR%\library\%%G\enc  ( XCOPY %SOURCEDIR%\library\%%G\enc  /Q /E /I R >nul )
    if exist %SOURCEDIR%\library\%%G\icc  ( XCOPY %SOURCEDIR%\library\%%G\icc  /Q /E /I R >nul )

    rem for issue https://github.com/jasp-stats/jasp-test-release/issues/416#issuecomment-591899068
    if "%%G"=="viridisLite"               ( XCOPY %SOURCEDIR%\library\%%G\data /Q /E /I data >nul )

    if exist %SOURCEDIR%\library\%%G\libs (
        mkdir %DESTDIR%\library\%%G\libs
        cd %DESTDIR%\library\%%G\libs
        
        COPY %SOURCEDIR%\library\%%G\libs\* /Y >nul
        XCOPY %SOURCEDIR%\library\%%G\libs\%ARCH% /Q /E /I %ARCH% >nul
        cd %DESTDIR%\library\%%G
    )
)

echo Making R readonly
attrib +r %DESTDIR%\library /d /s

echo Done!
cd %STARTDIR%

endlocal
