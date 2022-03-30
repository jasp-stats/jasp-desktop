rem Calling vcvars%ARCH%.bat

set MSVCDIR_DEFAULT=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community

if "%MSVCDIR%"=="" (
    set "MSVCDIR=%MSVCDIR_DEFAULT%"
    echo Using default MSVCDIR "%MSVCDIR_DEFAULT%", to change set the MSVCDIR environment variable to your install location
) else (
    echo MSVCDIR: "%MSVCDIR%"
)

set VCVARS_DIR="%MSVCDIR%\VC\Auxiliary\Build"

call %VCVARS_DIR%\vcvars%ARCH%.bat

mkdir build"

cmake .. -GNinja -DCMAKE_PREFIX_PATH=D:/Qt/6.2.4/msvc2019_64

cmake --build . --target all

cmake --build . --target install

cmake --build . --target collect-junctions

cmake --build . --target wix

cmake --build . --target zip

cmake --build . --target upload
