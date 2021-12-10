# !/bin/sh
# Run this script from your Development/Frameworks folder
# It copies all the packages from your local installed packaes and fixes library Pathes

CURRENT_R_VERSION=4.1-arm64
python ./create-rframework.py
cd R.framework/Versions/$CURRENT_R_VERSION/Resources/lib
install_name_tool -id @executable_path/../Frameworks/R.framework/Versions/$CURRENT_R_VERSION/Resources/lib/libR.dylib libR.dylib
cd ../../../../..
