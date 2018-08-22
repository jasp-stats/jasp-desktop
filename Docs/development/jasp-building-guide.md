
Guide to Building JASP
======================

The easiest way to build JASP is to use Qt Creator. After cloning the [jasp-desktop project](https://github.com/jasp-stats/jasp-desktop), open the JASP.pro file in the jasp-desktop directory, in Qt Creator. This project contains two runnable sub-projects, *JASP-Desktop* and *JASP-Engine*. In order to run JASP, *JASP-Desktop* is the correct project to run.

For those unfamiliar with using Qt Creator for development, there is some excellent documentation available [here](http://doc.qt.io/qtcreator/index.html).

Alternatively, those that are more comfortable using command line tools can use *QMake*, the manual for which is available [here](http://doc.qt.io/qt-5/qmake-manual.html). *QMake* projects (like JASP) are typically built in two steps; first *QMake* is called, which generates a Makefile for *Make*, and then *Make* is called with this generated Makefile.

We recommend building JASP in a separate directory to it's source directory. This can be achieved by calling *QMake* in a separate directory to the source, for example we might create a separate directory beside the *jasp-desktop* directory (perhaps called *jasp-build*), and then from this directory call:

    qmake ../jasp-desktop/JASP.pro
    make

This generates the Makefile in the *jasp-build* directory, and all resulting object files and executables will be output to this directory.

JASP requires several dependencies which are documented below.

JASP depends on:

 - [Qt (5.10 + QtWebEngine)](http://qt-project.org)
 - [R](http://cran.r-project.org)
 - [boost](http://boost.org)
 - [libarchive](http://libarchive.org/)
 - [zlib](http://zlib.net/)

Links to specific versions for each platform are provided below.

Windows
-------

Building JASP under windows is the most temperamental, and the versions listed here are known to work; slight variations on these numbers probably won't work.

 - [R 3.4.4 win64](https://static.jasp-stats.org/development/R3.4%20Win%20JASP%200.9.zip)
 - [boost 1.64.0](https://static.jasp-stats.org/development/boost_1_64_0.zip)
 - [boost 1.64.0 binaries, libarchive binaries](https://static.jasp-stats.org/development/Build-Binaries-Windows-64-qt510.zip) Update 14-08-2018 (incl. JASP-R-Interface-3.1).
 - [Visual Studio 2017] (https://www.visualstudio.com/downloads/) Download community version

Qt: JASP for windows is built as 64-bit and is built with Visual Studio 2017.

The directory structure should be as follows:

    [+] jasp-desktop  < from github >
	[+] boost_1_64_0
    [-] build-JASP- ... < build directory, created by QtCreator >
       [+] R
	- JASP-R-Interface.dll
	- JASP-R-Interface.lib
	- archive.dll.lib
	- libarchive.dll
	- libboost_date_time-vc141-mt-1_64.lib
	- libboost_date_time-vc141-mt-gd-1_64.lib
	- libboost_filesystem-vc141-mt-1_64.lib
	- libboost_filesystem-vc141-mt-gd-1_64.lib
	- libboost_system-vc141-mt-1_64.lib
	- libboost_system-vc141-mt-gd-1_64.lib
	- libeay32.dll
	- libgcc_s_seh-1.dll
	- libgomp-1.dll
	- libstdc++-6.dll
	- libwinpthread-1.dll
	- ssleay32.dll


Mac OS X
--------
To build JASP on mac you need to clone the JASP repository and download the following five packages:

 0. Clone the JASP repository
 1. [XCode](https://developer.apple.com/xcode/) Easiest would be via the App Store.
 2. [Qt 5.10.1 + QtWebEngine](https://download.qt.io/archive/qt/). Tick: MacOS and Qt WebEngine
 3. [R 3.4.4](https://static.jasp-stats.org/development/R3.4%20OSX%20JASP%200.9.zip) This contains R and the packages. Unzip this file and set it as a framework in the build folder as shown below.
 4. [boost 1.64.0](https://static.jasp-stats.org/development/boost_1_64_0.zip). Unzip this file in the JASP folder.
 5. [boost 1.64.0 binaries, libarchive binaries](https://static.jasp-stats.org/development/Build-Binaries-OSX-64-qt510.zip). Unzip this file in both release and debug build folders.

The directory structure should be as follows:

	[+] JASP
		[-] jasp-desktop  < from github >
		[-] buildDebug510 < Build debug directory for QtCreator >
			- libboost_system-clang-mt-1_64.a
			- libboost_filesystem-clang-mt-1_64.a
			- libarchive.a
			- libz.a
			- libJASP-R-Interface.1.0.0.dylib
			- libJASP-R-Interface.dylib
		[-] buildRelease510 < Build release directory for QtCreator  >
			- libboost_system-clang-mt-1_64.a
			- libboost_filesystem-clang-mt-1_64.a
			- libarchive.a
			- libz.a
			- libJASP-R-Interface.1.0.0.dylib
			- libJASP-R-Interface.dylib
		[-] boost_1_64_0
		[-] Frameworks
                        [-] R.framework
				[-] Versions
					[+] 3.3

 0. **Clone** the JASP repository into a folder of your choice. Our default choice is *~/desktop/JASP/* and cloning results in the creating of *~/desktop/JASP/jasp-desktop*.
 1. **XCode**: Qt on OS X relies on XCode to function, you can install this through the App Store. It's easiest if you install this, run it, accept the license agreement, and then close it down before installing Qt.
 2. **Qt**: Install Qt5.10.1 with MacOS and Qt WebEngine.

![Image of Qt Installer](https://static.jasp-stats.org/images/jasp2.InstallQt.png)

 2.a. **Configure Qt5.10.1**: Left top menu: Qt Creator - Preference. Left menu: "Build & Run", tab: "Kits". Auto-detect should give "Desktop Qt 5.10.1 clang 64bit". Click on this. Choose the compiler **Clang (x86 64bit in /usr/bin)** for both C and C++.

![Image of Qt Configuration](https://static.jasp-stats.org/images/jasp2a.ConfigureQt.png)

 2.b. **Configure project**: Click "Projects" in the left ribbon and provide the "debug build" and "release build" folders with the correct compilers. This should look like:

 ![Image of Project debug](https://static.jasp-stats.org/images/jasp2b.1.ConfigureProjectDebug.png)

 and like:

 ![Image of Project release](https://static.jasp-stats.org/images/jasp2b.2.ConfigureProjectRelease.png)

In both case, I've added the flag "-j4" to make use of all my four cores on my mac.

5. In the end, your folder should be structured as follows:

 ![Image of folder structure](https://static.jasp-stats.org/images/jasp5.FolderStructure.png)

where the blue files are the binaries that are added manually.

Linux
-----

### Build

#### Ubuntu (and alike)
To build JASP under Ubuntu (17.10+), debian, and derivatives, you will need:
```
sudo apt-get install libboost-dev r-base-core r-cran-rcpp r-cran-rinside libqt5widgets5 qtwebengine5-dev libqt5webchannel5-dev libqt5svg5-dev qt5-qmake libarchive-dev libboost-filesystem-dev libboost-system-dev libjsoncpp-dev qt5-default qtcreator
```

Then you start qtcreator and open JASP.pro, run qmake and build all. After that you should be able to run JASP.

#### Fedora
Under Fedora, you need these packages:
 - qt-devel
 - qt5-qtwebengine-devel (probably)
 - qt5-qtwebchannel-devel (probably)
 - boost-devel
 - libarchive-devel

And (under fedora only), in R (started as root so packages are installed systemwide), you need to install:

```
install.packages(c("Rcpp","RInside"))
```

Finally, under Fedora only, you need to create a symlink so that R is found:

```
sudo ln -s /usr/lib64/R/ /usr/lib/R
```

### Runtime
#### Ubuntu (and alike)
In order to run, you will need (Ubuntu and alike):
```
sudo apt-get install libjsoncpp1 r-base-core r-cran-rcpp r-cran-rinside r-cran-bayesfactor r-cran-lme4 r-cran-afex r-cran-car r-cran-effects r-cran-logspline r-cran-lsmeans r-cran-plotrix r-cran-rjson r-cran-vcd r-cran-vcdextra r-cran-ggplot2 r-cran-hypergeo libqt5webenginewidgets5 libqt5webengine5 libqt5webenginecore5 libqt5svg5 openssl
```

### Fedora
It works under Fedora, if you install these R packages manually in R:

```
install.packages(c("BayesFactor","lme4","afex","car","effects","logspline","hypergeo","rjson"))
```
