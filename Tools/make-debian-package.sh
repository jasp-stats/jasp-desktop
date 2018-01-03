#!/bin/sh
# Assumption: this is run from jasp-desktop/Tools/
# This is where we are gonna make our package:
mkdir ../../jasp-0.8.5
mkdir ../../jasp-0.8.5/jasp-0.8.5

# Now we cp our entire source directory there
cp -R ../* ../../jasp-0.8.5/jasp-0.8.5

# and we place the debian folder where debuild expects it to be.
cp debian ../../jasp-0.8.5/jasp-0.8.5

# Then we make a "source-tarball" even though we are obviously working directly from our sources.
tar --create -verbose --gzip --file ../../jasp-0.8.5/jasp_0.8.5.0.orig.tar.gz ../../jasp-0.8.5/jasp-0.8.5

# Now all that remains is to build the .deb!
cd ../../jasp-0.8.5/jasp-0.8.5/debian
debuild

