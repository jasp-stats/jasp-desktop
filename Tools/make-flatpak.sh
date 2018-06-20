#! /bin/sh
echo "Let's build ourselves a flatpak!"
mkdir ../flatpak-builder-folder
mkdir ../flatpak-builder-folder/jasp-repo
cp flatpak/org.jasp.JASP.json ../flatpak-builder-folder/org.jasp.JASP.json
cd ../flatpak-builder-folder
flatpak-builder --gpg-sign=528338C233D5B3D8 --force-clean --ccache --repo=jasp-repo jasp-build org.jasp.JASP.json
flatpak build-update-repo jasp-repo --gpg-sign=528338C233D5B3D8 --generate-static-deltas

