Guide to Building JASP
======================

JASP depends on [Qt (5+)](http://qt-project.org), [R](http://cran.r-project.org) and [boost](http://boost.org).

Windows
-------

Building JASP under windows is the most temperamental, and the versions listed here are known to work; slight variations on these numbers probably won't work.

 - [Qt 5.2.1 win64 GCC 4.8.2](https://static.jasp-stats.org/development/x64-Qt-5.2.1+QtCreator-3.0.1-(gcc-4.8.2-seh).7z)
 - [R 3.1.2 win64](https://static.jasp-stats.org/development/R%20Win64%20for%20JASP%20%282015-02-12%29.zip)
 - [boost 1.54.0](https://static.jasp-stats.org/development/boost_1_54_0.7z)
 - [boost 1.54.0 binaries, win64 GCC 4.8.2](https://static.jasp-stats.org/development/boost_1_54_0-bin-win64-gcc-4.8.2.zip)

Qt: JASP for windows is built as 64-bit and is built with Mingw-W64. The Qt project does not ship a 64-bit version of Qt based on Mingw-W64, and so we rely on a version from [here](http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/Qt-Builds/). Once downloaded and unzipped to the desired location, it is necessary to run the `QtSDK-x86_64/qtbinpatcher.exe`, so it knows about it's current location.

dlls: It is necessary to copy the four files libgcc_s_seh-1.dll, libstdc++-6.dll, libwinpthread-1.dll and libgomp-1.dll from QtSDK-x86_64/bin into the build directory as well.

The directory structure should be as follows:

    [+] jasp-desktop  < from github >
    [+] boost_1_54_0
    [-] build-JASP- ... < build directory, created by QtCreator >
       [+] R
        - libgcc_s_seh-1.dll
        - libstdc++-6.dll
        - libwinpthread-1.dll
        - libgomp-1.dll
		- libboost_system-mt.a
		- libboost_filesystem-mt.a

 
Mac OS X
--------

 - [R 3.1.2](https://static.jasp-stats.org/development/R%20OSX%20for%20JASP%20%282015-02-12%29.zip)
 - [boost 1.54.0](https://static.jasp-stats.org/development/boost_1_54_0.tar.bz2)
 - [boost 1.54.0 binaries](https://static.jasp-stats.org/development/boost_1_54_0-bin-osx.zip)

XCode: Qt on OS X relies on XCode to function, you can install this through the App Store. It's easiest if you install this, run it, accept the license agreement, and then close it down before installing Qt.

Qt: building JASP on OS X is pretty robust, and most versions work. We currently use 5.4.0, but newer versions will probably work too. You can download it from [here](https://qt-project.org/downloads).

The directory structure should be as follows:

    [+] jasp-desktop  < from github >
    [+] boost_1_54_0
    [-] Frameworks
       [+] R.framework
    [+] build-JASP- ... < build directory, created by QtCreator >
	    - libboost_system-mt.a
	    - libboost_filesystem-mt.a

Linux
-----

Qt: Whatever comes with your distro.

R: We provide a specially crafted R.framework for Ubuntu 14.10 [here](https://static.jasp-stats.org/development/R%20U1410%20for%20JASP%20%282014-12-21%29.zip)

boost: Whatever comes with your distro.

The directory structure should be as follows:

    [+] jasp-desktop  < from github >
    [+] R
    [+] build-JASP- ... < build directory, created by QtCreator >
