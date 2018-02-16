#! /bin/sh
echo "Let's build ourselves a flatpak!"
orginaldir=$PWD
cp org.jasp.JASP.json ../../../org.jasp.JASP.json
cd ../../..
flatpak-builder --gpg-sign=528338C233D5B3D8 --force-clean --ccache --repo=jasp-repo jasp-build org.jasp.JASP.json
rm org.jasp.JASP.json
cd $originaldir

