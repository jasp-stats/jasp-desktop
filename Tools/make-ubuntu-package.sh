#!/bin/sh
# Assumption: this is run from jasp-desktop/Tools/
MajorVersion=`grep -oP		'VersionMajor\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`
MinorVersion=`grep -oP		'VersionMinor\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp` 
RevisionVersion=`grep -oP	'VersionRevision\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`
BuildVersion=`grep -oP		'VersionBuildNumber\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`

JASPFolder=jasp-$MajorVersion.$MinorVersion.$RevisionVersion.7
JASPTar=jasp_$MajorVersion.$MinorVersion.$RevisionVersion.7.orig.tar.gz

echo Making ubuntu package $JASPFolder

# This is where we are gonna make our package:
mkdir ../../$JASPFolder
mkdir ../../$JASPFolder/$JASPFolder

# Now we cp our entire source directory there
cp -R ../* ../../$JASPFolder/$JASPFolder

#now we need to set up a folderstructure that the ubuntu debialbuild likes
UbuntuVersionThing=jasp_`grep -oP 'jasp \(\K[^)]+(?=\) \w+; urgency=high)' ubuntu/changelog`
UbuntuDebianFolderHolder=$UbuntuVersionThing.debian

echo $UbuntuDebianFolderHolder

mkdir ../../$JASPFolder/$UbuntuDebianFolderHolder
UbuntuDebianFolder=../../$JASPFolder/$UbuntuDebianFolderHolder/debian
mkdir $UbuntuDebianFolder

# and we copy the ubuntu "debian" folder to where debuild expects it to be.
cp -R ./ubuntu/* $UbuntuDebianFolder

# Then we make a "source-tarball" even though we are obviously working directly from our sources.
cd ../../$JASPFolder
tar --create --verbose --gzip --file $JASPTar $JASPFolder

# Now all that remains is to build the .deb!
cd $UbuntuDebianFolderHolder
debuild -S -sa -k3D646D04

echo Done building sourcepackage, to push to PPA: \''cd ../../'$JASPFolder'/ && dput "ppa:jasp-uva/jasp" '$UbuntuVersionThing'_source.changes'\'

