#!/bin/sh
#Uploads the JASP.dmg to our server with the name: JASP-nightly-*branch*-*commit*, repository isn't mentioned because i figured the name was long enough already.
DEST_FILE=JASP-nightly-`git rev-parse --abbrev-ref HEAD`-`git rev-parse --verify HEAD`.dmg
echo Copying JASP.dmg to jasp-static with name $DEST_FILE
scp ../../jasp-build/JASP.dmg nachtjapon@static.jasp-stats.org:~/Nightlies/MacOS/$DEST_FILE
