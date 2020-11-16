#!/bin/sh
#Ought to be run from make-clean-osx.sh because then the necessary variables are set
echo "Changing directory to $JASP_FULL_BUILD_DIR"
cd $JASP_FULL_BUILD_DIR

. versionScript.sh

R_FRAMEWORK=$JASP_ROOT_DIR/Frameworks/R.framework
JASP_DESKTOP=$JASP_FULL_GIT_DIR

if [ ! -d $R_FRAMEWORK ]
then
  echo "Wrong R Framework folder"
  exit 1
else
  echo "Using R Framework folder: $R_FRAMEWORK"
fi

if [ ! -d $JASP_DESKTOP ]; then
  echo "Wrong jasp-desktop folder"
  exit 1
else
  echo "Using jasp-desktop folder: $JASP_DESKTOP"
fi

# This script builds the JASP.dmg installer
# Check that your R.framework is unique (no other test versions).
# Check also that the right dylib are placed in the build-jasp-desktop-Release folder
# Then run this script from the build-jasp-desktop-Release folder

echo "Remove from last time"

rm -rf app
rm -rf JASP.zip
rm -f tmp.dmg
rm -f JASP*.dmg

echo "Create output tree under ./app"

mkdir app/
mkdir app/JASP.app/
mkdir app/JASP.app/Contents/
mkdir app/JASP.app/Contents/Libraries/
mkdir app/JASP.app/Contents/MacOS/
mkdir app/JASPEngine.app/
mkdir app/JASPEngine.app/Contents/
mkdir app/JASPEngine.app/Contents/MacOS

echo "Create a symbolic link to Applications"

cd app
ln -s /Applications .
cd ..

echo "Copy the two executables into place (after renaming jasp -> JASP)"
install_name_tool -add_rpath @loader_path/../Libraries JASP
install_name_tool -add_rpath @loader_path/../Libraries JASPEngine
cp JASP app/JASP.app/Contents/MacOS/
cp JASPEngine app/JASPEngine.app/Contents/MacOS/

echo "Copy any R-files in buildfolder"
cp *.R app/JASP.app/Contents/MacOS/

echo "Create apps from each executable"
echo "We do this to the JASPEngine, because this process fixes the rpaths"

$QT_KIT_FULL/bin/macdeployqt app/JASP.app/ -qmldir=$JASP_DESKTOP/Desktop
$QT_KIT_FULL/bin/macdeployqt app/JASPEngine.app/

echo "Copy the JASPEngine out of the JASPEngine.app into the JASP.app"
echo "This will now have had it's rpaths fixed"

mv app/JASPEngine.app/Contents/MacOS/JASPEngine app/JASP.app/Contents/MacOS/
rm -rf app/JASPEngine.app/

echo "Copy the R.framework in, the Resources, App info, icon, etc."
APP_R_FRAMEWORK=app/JASP.app/Contents/Frameworks/R.framework
cp -R $R_FRAMEWORK $APP_R_FRAMEWORK
cp -R $JASP_DESKTOP/Resources/* app/JASP.app/Contents/Resources
rm app/JASP.app/Contents/Resources/TestFiles.zip
cp -R R app/JASP.app/Contents/MacOS

echo "Copy the module( librarie)s from the buildfolder to Resources"
cp -R $JASP_FULL_BUILD_DIR/Modules app/JASP.app/Contents/Resources

#This is now made part of jasp-required-files: https://github.com/jasp-stats/jasp-required-files/commit/34cdebfda1e5bc27c30d5bf11cd07471449162e7
#echo "Make symbolic link to Frameworks in bin folder to let @executable_path/... stuff work from R executable as well."
#pushd $APP_R_FRAMEWORK/Versions/$CURRENT_R_VERSION/bin
#ln -s $APP_R_FRAMEWORK/.. Frameworks 
#popd

echo "Copying JAGS to executable folder"
cp -R ../jasp-required-files/JAGS app/JASP.app/Contents/MacOS

cd $APP_R_FRAMEWORK/Versions
ln -s $CURRENT_R_VERSION Current
cd Current
ln -s ./Resources/include Headers
ln -s ./Resources/lib/libR.dylib R
cd ../..
ln -s ./Versions/Current/Headers Headers
ln -s ./Versions/Current/Libraries Libraries
ln -s ./Versions/Current/PrivateHeaders PrivateHeaders
ln -s ./Versions/Current/Resources Resources
find . -name '*.cpp' -exec rm {} \;
find . -name '*.c' -exec rm {} \;
#find . -name '*.h' -exec rm {} \; See https://github.com/jasp-stats/INTERNAL-jasp/issues/1169 (Allow compiling of cpp in JASP with regards to dynamic modules etc. I dont think anything else then the headers is required for that. )
find . -name '*.f' -exec rm {} \;
cd ../../../../..

echo "Copy the Openssl from Qt to our Framework because OSF no longer supports tlsv1 traffic"
cp libcrypto.1.0.0.dylib app/JASP.app/Contents/Libraries/
cp libssl.1.0.0.dylib app/JASP.app/Contents/Libraries/

cp $JASP_DESKTOP/Tools/macOS/icon.icns app/JASP.app/Contents/Resources
cp $JASP_DESKTOP/Tools/macOS/entitlements.plist app/JASP.app/Contents/

#copy template for Info.plist and replace JASP_VERSION with the actual version
cp $JASP_DESKTOP/Tools/macOS/Info.plist.template app/JASP.app/Contents/Info.plist
sed -ie s/JASP_VERSION/$JASP_VERSION/g app/JASP.app/Contents/Info.plist

echo "Create the .dmg"
hdiutil create -size 1500m tmp.dmg -ov -volname "JASP" -fs HFS+ -srcfolder "app"
hdiutil convert tmp.dmg -format UDZO -o JASP.dmg
#mv JASP.dmg JASP-$JASP_VERSION.dmg #easier for upload-script to know dmg-name for sure
rm -f tmp.dmg
