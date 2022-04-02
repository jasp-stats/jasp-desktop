#!/bin/bash
# 
# BuildBotScript.sh is to be used in our buildbot machine, and it would not work on any
# other machine. It needs to know the path to the Qt folder, and the rest should all be
# handled by the CMake.
# 
# Todo:
# 	- [ ] Find a way to retrieve Qt's path automatically, maybe the `master.cfg` can do it
# 	for us, just before calling this script.
# 	- [ ] Enable the arm64 build, when we can, if necessary.

cmake -E remove_directory build-x86_64
cmake -E make_directory build-x86_64

cmake -S . -B build-x86_64 -GNinja -DCMAKE_BUILD_TYPE=Release -DCMAKE_OSX_ARCHITECTURES=x86_64 -DCMAKE_PREFIX_PATH=/Users/buildbotworker/Qt/6.2.4/macos 
cmake --build build-x86_64 --target all
cmake --build build-x86_64 --target install
cmake --build build-x86_64 --target dmg
cmake --build build-x86_64 --target notarise
cmake --build build-x86_64 --target upload

# cmake -E remove_directory build-arm64
# cmake -E make_directory build-arm64

# cmake -S . -B build-arm64 -GNinja -DUSE_CONAN=ON -DCMAKE_PREFIX_PATH=@Qt6_DIR@ -DCMAKE_OSX_ARCHITECTURES=arm64
# cmake --build build-arm64 --target JASP
# cmake --build build-arm64 --target install
# cmake --build build-arm64 --target dmg
# cmake --build build-arm64 --target notarise
# cmake --build build-arm64 --target upload