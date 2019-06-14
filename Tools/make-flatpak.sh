#! /bin/sh
echo "Let's build ourselves a flatpak!"
mkdir ../flatpak-builder-folder
mkdir ../flatpak-builder-folder/jasp-repo

echo "Moving org.jaspstats.JASP.json to flatpak-builder-folder"
cp flatpak/org.jaspstats.JASP.json ../flatpak-builder-folder/org.jaspstats.JASP.json
cd ../flatpak-builder-folder

echo "Starting flatpak builder"
flatpak-builder --install-deps-from=flathub --gpg-sign=528338C233D5B3D8 --force-clean --ccache --repo=jasp-repo jasp-build org.jaspstats.JASP.json

echo "Update Repository"
flatpak build-update-repo jasp-repo --gpg-sign=528338C233D5B3D8 --generate-static-deltas

