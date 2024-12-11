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

cd ../../

cmake -E remove_directory build
cmake -E make_directory build

export QT_DIR="~/Qt/6.8.1/macos" #value is written in the automator script but can be overwritten with this (though it doesnt show in builbot env log)
export PATH="$QT_DIR/../../Tools/Ninja:$PATH"

cmake -S . -B build -GNinja -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$QT_DIR -DCMAKE_CXX_COMPILER=/usr/bin/clang++ -DCMAKE_C_COMPILER=/usr/bin/clang 
cmake --build build --target all
cmake --build build --target install
cmake --build build --target dmg
cmake --build build --target notarise
cmake --build build --target upload
