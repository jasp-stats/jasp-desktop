#!/bin/bash
# 

cmake -E remove_directory build-x86_64
cmake -E make_directory build-x86_64

cmake -S . -B build-x86_64 -GNinja -DCMAKE_PREFIX_PATH=/Users/buildbotworker/Qt/6.2.4/macos -DCMAKE_OSX_ARCHITECTURES=x86_64
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