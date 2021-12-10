# !/bin/sh
# Run this script from your Development/Frameworks folder
# It copies all the packages from your local installed packaes and fixes library Pathes
# And removes code to prepare the Mac code-signing

CURRENT_R_VERSION=4.1-arm64
python ./create-rframework.py
cd R.framework/Versions/$CURRENT_R_VERSION/Resources/lib
install_name_tool -id @executable_path/../Frameworks/R.framework/Versions/$CURRENT_R_VERSION/Resources/lib/libR.dylib libR.dylib
cd ../../../../..
cd R.framework
cd Versions
ln -s ./$CURRENT_R_VERSION Current
cd Current
ln -s ./Resources/include Headers
ln -s ./Resources/lib/libR.dylib R
cd ../..
ln -s ./Versions/Current/Headers Headers
ln -s ./Versions/Current/Libraries Libraries
ln -s ./Versions/Current/PrivateHeaders PrivateHeaders
ln -s ./Versions/Current/Resources Resources
cd ..
find . -name '*.cpp' -exec rm {} \;
find . -name '*.c' -exec rm {} \;
find . -name '*.h' -exec rm {} \;
find . -name '*.f' -exec rm {} \;
