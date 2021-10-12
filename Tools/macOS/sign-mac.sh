 #!/bin/bash

# set up your app name, version and optional background image
APP_NAME="JASP"
VERSION="0.15.0.0"
DMG_BACKGROUND_IMG="background.png"

# you should not need to change these
APP_PATH="${APP_NAME}.app"
VOL_NAME="${APP_NAME}-${VERSION}"
DMG_TMP="${VOL_NAME}-temp.dmg"
DMG_FINAL="${VOL_NAME}.dmg"

STAGING_DIR="./Install"
SIGNING_CERTIFICATE="Developer ID Application: Bruno Boutin (AWJJ3YVK9B)"

APP_EXE="${APP_PATH}/Contents/MacOS/${APP_NAME}"
APP_PLIST="${APP_PATH}/Contents/Info.plist"
APP_FRAMEWORKS="${APP_PATH}/Contents/Frameworks"
ENTITLEMENTS_PLIST="${APP_PATH}/Contents/entitlements.plist"

sign () {
	echo Signing "$1"
	codesign --force --verbose --sign "${SIGNING_CERTIFICATE}" --entitlements "${ENTITLEMENTS_PLIST}" --timestamp --options runtime "$1"
	
	echo Verifying "$1"
	codesign -vvvv --deep "$1"
}

signRecursively () {
	#Descend into each folder here unless it is a symbolic link (Because otherwise we get stuck in an infinite loop due to the fix for https://github.com/jasp-stats/jasp-test-release/issues/641)
	for d in "$1"/*; do
		if [[ ! -L "$d" && -d "$d" ]]; then
			signRecursively "$d" $2
		fi
	done

	#make sure all executable files, or those with dylib or so as extension are signed
	# do all this while avoiding signing files that are secretly a symlink
	for f in "$1"/*; do
		if [[ -f "$f" && (! -L "$f") && ( "$2" == "Aggressive" || -x "$f" || $f =~ [[:\<:]].+\.(dylib|so|plist)[[:\>:]] ) ]]; then
			sign "$f"
		fi
	done

	#Stupid workaround to get jasp signed after everything else
	for f in "$1"/*; do
		if [[ -f "$f" && $f = JASP.app/Contents/MacOS/JASP ]]; then
			echo Signing JASP last
			sign "$f"
		fi
	done

	if [[ -f $1/Resources/Info.plist ]]; then #this is a bundle! 
		echo Signing bundle $1
		sign "$1"
	fi
}

signDMG () {
	echo Signing "$1"
	codesign --verbose --sign "${SIGNING_CERTIFICATE}" --timestamp  --options runtime "$1"
	
	echo Verifying "$1"
	codesign -vvvv --deep "$1"
}

echo "Check the background image DPI and convert it if it isn't 72x72"
if [ ${DMG_BACKGROUND_IMG} != "" ]; then
	_BACKGROUND_IMAGE_DPI_H=`sips -g dpiHeight ${DMG_BACKGROUND_IMG} | grep -Eo '[0-9]+\.[0-9]+'`
	_BACKGROUND_IMAGE_DPI_W=`sips -g dpiWidth ${DMG_BACKGROUND_IMG} | grep -Eo '[0-9]+\.[0-9]+'`
	if [ $(echo " $_BACKGROUND_IMAGE_DPI_H != 72.0 " | bc) -eq 1 -o $(echo " $_BACKGROUND_IMAGE_DPI_W != 72.0 " | bc) -eq 1 ]; then
		echo "WARNING: The background image's DPI is not 72.  This will result in distorted backgrounds on Mac OS X 10.7+."
		echo "         I will convert it to 72 DPI for you."
		_DMG_BACKGROUND_TMP="${DMG_BACKGROUND_IMG%.*}"_dpifix."${DMG_BACKGROUND_IMG##*.}"
		sips -s dpiWidth 72 -s dpiHeight 72 ${DMG_BACKGROUND_IMG} --out ${_DMG_BACKGROUND_TMP}
		DMG_BACKGROUND_IMG="${_DMG_BACKGROUND_TMP}"
	fi
fi


echo clear out any old data
rm -rf "${STAGING_DIR}" "${DMG_TMP}" "${DMG_FINAL}"

echo copy over the stuff we want in the final disk image to our staging dir
mkdir -p "${STAGING_DIR}"

echo "clean up .dmg file"
chflags nouchg $1
xattr -rc $1

# Extracting app from dmg file
#We should actually check if the dmg was already mounted or not though...
echo "attaching image file"
hdiutil attach $1

#How do we know the name of the volume?
echo "copying application to temp directory"
cp -a /Volumes/JASP/JASP.app "${STAGING_DIR}/"

echo "detaching image file"
hdiutil detach /Volumes/JASP

pushd "${STAGING_DIR}"

echo "Remove stuff from R.framework"
rm "$APP_PATH/Contents/Frameworks/R.framework/.Rhistory"
rm "$APP_PATH/Contents/Frameworks/R.framework/.DS_Store"
unlink "$APP_PATH/Contents/Frameworks/R.framework/Headers"
unlink "$APP_PATH/Contents/Frameworks/R.framework/Libraries"
unlink "$APP_PATH/Contents/Frameworks/R.framework/PrivateHeaders"

# strip the executable
echo "Stripping ${APP_EXE}..."
strip -u -r "${APP_EXE}"

if hash upx 2>/dev/null; then
	echo "Compressing (UPX) ${APP_EXE}..."
	upx -9 "${APP_EXE}"
fi

# ... perform any other stripping/compressing of libs and executables
echo "presign actions"
rm "$APP_PATH/Contents/Info.pliste"

echo "Signing all executable files and relevant libraries"
signRecursively "$APP_PATH"

echo "Signing MacOS folder more aggressively (aka everything) because main application will otherwise not be signed"
signRecursively "$APP_PATH/Contents/MacOS" Aggressive

echo "Now try to sign JASP executable itself again"
echo sign "$APP_PATH/Contents/MacOS/JASP"
sign "$APP_PATH/Contents/MacOS/JASP"

echo "And the whole bundle"
sign "$APP_PATH"

echo "Making the R library readonly"  #https://github.com/jasp-stats/INTERNAL-jasp/issues/896
chmod -R a-w $APP_FRAMEWORKS/R.framework/Versions/*/Resources/library
chmod -R a+w $APP_FRAMEWORKS/R.framework/Versions/*/Resources/library/officer # Fixes https://github.com/jasp-stats/INTERNAL-jasp/issues/972 (Can't save as pptx on mac)
popd

#echo "Figure out the size of the DMG"
# figure out how big our DMG needs to be
#  assumes our contents are at least 1M!
#SIZE=`du -sh "${STAGING_DIR}" | sed 's/\([0-9]*\)M\(.*\)/\1/'`
#SIZE=`echo "${SIZE} + 1.0" | bc | awk '{print int($1+0.5)}'`
#echo "size=${SIZE}"

#if [ $? -ne 0 ]; then
#	echo "Error: Cannot compute size of staging dir"
#	exit
#fi

# create the temp DMG file
hdiutil create -srcfolder "${STAGING_DIR}" -volname "${VOL_NAME}" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDRW -size 3000M "${DMG_TMP}"
echo "Created DMG: ${DMG_TMP}"

# mount it and save the device
#We should actually check if the dmg was already mounted or not though... Otherwise you can get some weird errors...
DEVICE=$(hdiutil attach -readwrite -noverify "${DMG_TMP}" | egrep '^/dev/' | sed 1q | awk '{print $1}')
sleep 2

# add a link to the Applications dir
echo "Add link to /Applications"
pushd /Volumes/"${VOL_NAME}"
ln -s /Applications
popd

# add a background image
mkdir /Volumes/"${VOL_NAME}"/.background
cp "${DMG_BACKGROUND_IMG}" /Volumes/"${VOL_NAME}"/.background/

echo "tell the Finder to resize the window, set the background, change the icon size, place the icons in the right position, etc."
echo '
tell application "Finder"
	tell disk "'${VOL_NAME}'"
		open
		set current view of container window to icon view
		set toolbar visible of container window to false
		set statusbar visible of container window to false
		set the bounds of container window to {400, 100, 1000, 900}
		set viewOptions to the icon view options of container window
		set arrangement of viewOptions to not arranged
		set icon size of viewOptions to 72
		set background picture of viewOptions to file ".background:'${DMG_BACKGROUND_IMG}'"
		set position of item "'${APP_NAME}'.app" of container window to {150, 640}
		set position of item "Applications" of container window to {450, 640}
		close
		open
		update without registering applications
		delay 2
	end tell
end tell
' | osascript

sync

echo "unmount it"
hdiutil detach "${DEVICE}"

# now make the final image a compressed disk image
echo "Creating compressed image"
hdiutil convert "${DMG_TMP}" -format UDZO -imagekey zlib-level=9 -o "${DMG_FINAL}"

echo "signing final dmg file"
signDMG "${DMG_FINAL}"

echo "checking Gatekeeper acceptance"
spctl -a -t open --context context:primary-signature -v --verbose=4 --assess ${DMG_FINAL}

echo "clean up after making R.framework writable again"
chmod -R a+w ${STAGING_DIR}/$APP_FRAMEWORKS/R.framework
rm -rf "${DMG_TMP}"
rm -rf "${STAGING_DIR}"

echo 'Done.'
exit
