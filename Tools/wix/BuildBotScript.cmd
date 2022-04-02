rem BuildBotScript.cmd is to be used in our buildbot machine, and it would not work on any
rem other machine. It needs to know the path to the Qt folder, and the rest should all be
rem handled by the CMake.
rem 
rem Todo:
rem   - [ ] It would be nice if we find a way to consistently read these path variables, so
rem         that we don't have to adjust the script for each VC, or Qt update.

set MSVCDIR_DEFAULT=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community

if "%MSVCDIR%"=="" (
    set "MSVCDIR=%MSVCDIR_DEFAULT%"
    echo Using default MSVCDIR "%MSVCDIR_DEFAULT%", to change set the MSVCDIR environment variable to your install location
) else (
    echo MSVCDIR: "%MSVCDIR%"
)

set VCVARS_DIR="%MSVCDIR%\VC\Auxiliary\Build"

call %VCVARS_DIR%\vcvars64.bat

echo Navigate to source root
cd ..\..

cmake -E remove_directory build

cmake -E make_directory build

echo start build
cmake -S . -B build -GNinja -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=D:/Qt/6.2.4/msvc2019_64

cmake --build build --target all

cmake --build build --target install

cmake --build build --target collect-junctions

cmake --build build --target wix

cmake --build build --target zip

cmake --build build --target upload
