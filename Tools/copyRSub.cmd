@echo off
setlocal EnableDelayedExpansion

set REL_PATH=%1
set SRCE_DIR=%2
set DEST_DIR=%3
set PKG_NAME=%4

rem So many boostheaders... https://github.com/jasp-stats/INTERNAL-jasp/issues/1285
if "%PKG_NAME%" == "BH" ( exit /B 0 )

echo REL_PATH=%REL_PATH%

mkdir %DEST_DIR%\%REL_PATH%
pushd %DEST_DIR%\%REL_PATH%

IF "%PKG_NAME:~0,4%"=="jasp" (

    rem Because this is some kind of jasp pkg, well probably anyway, we might need anything in there. So no shortcuts, just copy *everything*
    pushd ..
    xcopy %SRCE_DIR%\%REL_PATH% /Y /Q /E /I %PKG_NAME% >nul
    popd

) ELSE (
    rem if you add things here, be sure to also add them to the function "copyRPkg" in make-dmg.sh
    copy %SRCE_DIR%\%REL_PATH%\INDEX INDEX                /Y >nul
    copy %SRCE_DIR%\%REL_PATH%\NAMESPACE NAMESPACE        /Y >nul
    copy %SRCE_DIR%\%REL_PATH%\DESCRIPTION DESCRIPTION    /Y >nul
    copy %SRCE_DIR%\%REL_PATH%\*.R .                      /Y >nul

    if exist %SRCE_DIR%\%REL_PATH%\R            ( xcopy %SRCE_DIR%\%REL_PATH%\R            /Y /Q /E /I R           >nul )
    if exist %SRCE_DIR%\%REL_PATH%\po           ( xcopy %SRCE_DIR%\%REL_PATH%\po           /Y /Q /E /I po          >nul )
    if exist %SRCE_DIR%\%REL_PATH%\afm          ( xcopy %SRCE_DIR%\%REL_PATH%\afm          /Y /Q /E /I afm         >nul )
    if exist %SRCE_DIR%\%REL_PATH%\enc          ( xcopy %SRCE_DIR%\%REL_PATH%\enc          /Y /Q /E /I enc         >nul )
    if exist %SRCE_DIR%\%REL_PATH%\lib          ( xcopy %SRCE_DIR%\%REL_PATH%\lib          /Y /Q /E /I lib         >nul )
    if exist %SRCE_DIR%\%REL_PATH%\icc          ( xcopy %SRCE_DIR%\%REL_PATH%\icc          /Y /Q /E /I icc         >nul )
    if exist %SRCE_DIR%\%REL_PATH%\Meta         ( xcopy %SRCE_DIR%\%REL_PATH%\Meta         /Y /Q /E /I Meta        >nul )
    if exist %SRCE_DIR%\%REL_PATH%\include      ( xcopy %SRCE_DIR%\%REL_PATH%\include      /Y /Q /E /I include     >nul )
    if exist %SRCE_DIR%\%REL_PATH%\shinythemes  ( xcopy %SRCE_DIR%\%REL_PATH%\shinythemes  /Y /Q /E /I shinythemes >nul )

    rem for issue https://github.com/jasp-stats/jasp-test-release/issues/823
    if exist %SRCE_DIR%\%REL_PATH%\template  	  ( xcopy %SRCE_DIR%\%REL_PATH%\template     /Y /Q /E /I template    >nul ) 

    rem for issue https://github.com/jasp-stats/jasp-test-release/issues/416#issuecomment-591899068
    if "%PKG_NAME%"=="viridisLite"                ( xcopy %SRCE_DIR%\%REL_PATH%\data         /Y /Q /E /I data        >nul )
    rem this could be done prettier with some kind of OR statement but this is easier. jaspCircular needs rao.table from data in circular
    if "%PKG_NAME%"=="circular"                   ( xcopy %SRCE_DIR%\%REL_PATH%\data         /Y /Q /E /I data        >nul )

    if exist %SRCE_DIR%\%REL_PATH%\libs (
        mkdir %DEST_DIR%\%REL_PATH%\libs
        pushd %DEST_DIR%\%REL_PATH%\libs
        
        copy %SRCE_DIR%\%REL_PATH%\libs\* /Y >nul
        xcopy %SRCE_DIR%\%REL_PATH%\libs\%ARCH% /Y /Q /E /I %ARCH% >nul
        popd
    )
)
popd
endlocal
