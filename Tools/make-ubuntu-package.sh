#!/bin/sh
# Assumption: this is run from jasp-desktop/Tools/
MajorVersion=`grep -oP		'VersionMajor\(\K[0123456789]+(?=\))' ../JASP-Common/appinfo.cpp`
MinorVersion=`grep -oP		'VersionMinor\(\K[0123456789]+(?=\))' ../JASP-Common/appinfo.cpp` 
RevisionVersion=`grep -oP	'VersionRevision\(\K[0123456789]+(?=\))' ../JASP-Common/appinfo.cpp`
BuildVersion=`grep -oP		'VersionBuildNumber\(\K[0123456789]+(?=\))' ../JASP-Common/appinfo.cpp`

JASPFolder=jasp-$MajorVersion.$MinorVersion.$RevisionVersion.0
JASPTar=jasp_$MajorVersion.$MinorVersion.$RevisionVersion.0.orig.tar.gz

echo Making ubuntu package $JASPFolder
echo Type your PPA-signkey:
read PPAKey

# This is where we are gonna make our package:
mkdir ../../$JASPFolder
mkdir ../../$JASPFolder/$JASPFolder

# Now we cp our entire source directory there
cp -R ../* ../../$JASPFolder/$JASPFolder

#now we need to set up a folderstructure that the ubuntu debialbuild likes
UbuntuDebianFolderHolder=grep -oP 'jasp \(\K[^)]+(?=\) xenial; urgency=low)' ubuntu/changelog

mkdir ../../$JASPFolder/$JASPFolder/$UbuntuDebianFolderHolder
mkdir ../../$JASPFolder/$JASPFolder/$UbuntuDebianFolderHolder/debian

# and we copy the ubuntu "debian" folder to where debuild expects it to be.
cp -R ./ubuntu/* ../../$JASPFolder/$JASPFolder/$UbuntuDebianFolderHolder/debian

# Then we make a "source-tarball" even though we are obviously working directly from our sources.
cd ../../$JASPFolder
tar --create --verbose --gzip --file $JASPTar $JASPFolder

# Now all that remains is to build the .deb!
cd $JASPFolder
echo debuild -S -sa -k3D6446D05 -pgpg2 --batch --passphrase $PPAKey

echo Done building, check ../../$JASPFolder for your package!

