#! /bin/sh
if [ -z "$1" ]
then
	echo "Enter the username that should be used to access static.jasp-stats.org!" 
	exit 1
fi

if [ "$2" == "prepare" ]
then	
	echo "Let's prepare to build a flatpak!"
else
	echo "Let's build ourselves a flatpak!"
fi

if [ -z "$2" ] || [ "$2" == "prepare" ]
then
	echo "No second argument given or it was 'prepare', so we need to collect all RPkgs!"
	echo "We make sure that we have all necessary R-Pkgs available locally and added to RPackages.json"
	cd flatpak
	R -e "source('gather-r-package-info.R'); createFlatpakJson();"

	echo "Now we make sure all of those packages are available on static.jasp-stats.org"
	cd pkg-source-files
	rsync -csav * $1@static.jasp-stats.org:static.jasp-stats.org/RPkgs/
	cd ../..

	echo "Make sure all directories necessary have been created"
	mkdir ../flatpak-builder-folder

	echo "Create an empty jasp-repo folder"
	mkdir ../flatpak-builder-folder/jasp-repo
	rm -rf ../flatpak-builder-folder/jasp-repo
	mkdir ../flatpak-builder-folder/jasp-repo

	echo "Moving org.jaspstats.JASP.json to flatpak-builder-folder"
	cp flatpak/org.jaspstats.JASP.json 	../flatpak-builder-folder/
	cp flatpak/RPackages.json 			../flatpak-builder-folder/
	cd ../flatpak-builder-folder

	echo "Getting current flatpak repository from static.jasp-stats.org"
	#cd jasp-repo
	#rsync -csav --del $1@static.jasp-stats.org:static.jasp-stats.org/flatpak/jasp-repo/* .
	#cd ..

	if [ "$2" == "prepare" ] 
	then
		echo "You didn't really want to build a flatpak right?"
		exit 0
	fi
else
	echo "A second argument was given which wasn't prepare and it means you do *not* want to wait for all R-pkgs and the online repository to be downloaded because you already waited long enough..."
	cd ../flatpak-builder-folder
fi

echo "Starting flatpak builder x64"
flatpak-builder --install-deps-from=flathub --gpg-sign=528338C233D5B3D8 --force-clean --ccache --repo=jasp-repo jasp-build org.jaspstats.JASP.json

#echo "Starting flatpak builder x86"
#flatpak-builder --arch=i386 --install-deps-from=flathub --gpg-sign=528338C233D5B3D8 --force-clean --ccache --repo=jasp-repo jasp-build org.jaspstats.JASP.json

#echo "Update Repository"
#flatpak build-update-repo jasp-repo --gpg-sign=528338C233D5B3D8 --generate-static-deltas

