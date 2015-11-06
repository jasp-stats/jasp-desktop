
# This script builds the JASP.dmg installer

# Remove from last time

rm -rf JASP.zip
rm -rf app/JASP.app/
rm -f tmp.dmg
rm -f JASP.dmg


# Create output tree

mkdir app/
mkdir app/JASP.app/
mkdir app/JASP.app/Contents/
mkdir app/JASP.app/Contents/MacOS/
mkdir app/JASPEngine.app/
mkdir app/JASPEngine.app/Contents/
mkdir app/JASPEngine.app/Contents/MacOS/

# Copy the two executables into place

cp JASP       app/JASP.app/Contents/MacOS/
cp JASPEngine app/JASPEngine.app/Contents/MacOS/

# Create apps from each executable
# We do this to the JASPEngine, because this process
# fixes the rpaths

~/Qt/5.4/clang_64/bin/macdeployqt app/JASP.app/
~/Qt/5.4/clang_64/bin/macdeployqt app/JASPEngine.app/

# Copy the JASPEngine out of the JASPEngine.app into the JASP.app
# This will now have had it's rpaths fixed

cp app/JASPEngine.app/Contents/MacOS/JASPEngine app/JASP.app/Contents/MacOS/
rm -rf app/JASPEngine.app/

# Copy the R.framework in, the Resources, App info, icon, etc.

cp -r R.framework app/JASP.app/Contents/Frameworks
cp -r Resources/* app/JASP.app/Contents/Resources

cp icon.icns app/JASP.app/Contents/Resources
cp Info.plist app/JASP.app/Contents

# Create the .dmg

hdiutil create -size 800m tmp.dmg -ov -volname "JASP" -fs HFS+ -srcfolder "app"
hdiutil convert tmp.dmg -format UDZO -o JASP.dmg
rm -f tmp.dmg
