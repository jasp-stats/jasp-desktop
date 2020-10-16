#!/bin/sh
# Assumption: this is run from jasp-desktop/Tools/
MajorVersion=`grep -oP		'VersionMajor\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`
MinorVersion=`grep -oP		'VersionMinor\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp` 
RevisionVersion=`grep -oP	'VersionRevision\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`
#BuildVersion=`grep -oP		'VersionBuildNumber\(\K[0123456789]+(?=\))' ../Common/appinfo.cpp`

JASPFolder=jasp-$MajorVersion.$MinorVersion.$RevisionVersion
JASPTar=jasp_$MajorVersion.$MinorVersion.$RevisionVersion.orig.tar.gz

echo Making debian package $JASPFolder

# This is where we are gonna make our package:
mkdir ../../$JASPFolder
mkdir ../../$JASPFolder/$JASPFolder

# Now we cp our entire source directory there
cp -R ../* ../../$JASPFolder/$JASPFolder

# and we place the debian folder where debuild expects it to be.
cp -R ./debian ../../$JASPFolder/$JASPFolder/

# Then we make a "source-tarball" even though we are obviously working directly from our sources.
cd ../../$JASPFolder
tar --create --verbose --gzip --file $JASPTar $JASPFolder

# Now all that remains is to build the .deb!
cd $JASPFolder
debuild

echo Done building, check ../../$JASPFolder for your package!

