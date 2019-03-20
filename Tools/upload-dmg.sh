#!/bin/sh
#Uploads the JASP.dmg to our server with the name: JASP-nightly-*branch*-*commit*, repository isn't mentioned because i figured the name was long enough already.
scp ../../jasp-build/JASP.dmg nachtjapon@static.jasp-stats.org:~/Nightlies/MacOS/JASP-nightly-`git rev-parse --abbrev-ref HEAD`-`git rev-parse --verify HEAD`.dmg
