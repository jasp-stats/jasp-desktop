
Guide to Building JASP
======================

The easiest way to build JASP is to use Qt Creator. After cloning the [jasp-desktop project](https://github.com/jasp-stats/jasp-desktop), open the JASP.pro file in the jasp-desktop directory, in Qt Creator. This project contains two runnable sub-projects, *JASP-Desktop* and *JASP-Engine*. In order to run JASP, *JASP-Desktop* is the correct project to run.

For those unfamiliar with using Qt Creator for development, there is some excellent documentation available [here](http://doc.qt.io/qtcreator/index.html).

Alternatively, those that are more comfortable using command line tools can use *QMake*, the manual for which is available [here](http://doc.qt.io/qt-5/qmake-manual.html). *QMake* projects (like JASP) are typically built in two steps; first *QMake* is called, which generates a Makefile for *Make*, and then *Make* is called with this generated Makefile.

We recommend building JASP in a separate directory to it's source directory. This can be achieved by calling *QMake* in a separate directory to the source, for example we might create a separate directory beside the *jasp-desktop* directory (perhaps called *jasp-build*), and then from this directory call:

    qmake ../jasp-desktop/JASP.pro
    make # -j8 # <- to gain a considerable speedup on a typical quadcore system

This generates the Makefile in the *jasp-build* directory, and all resulting object files and executables will be output to this directory.

JASP requires several dependencies which are documented below.

JASP depends on:

 - [Qt (5.11 + QtWebEngine)](http://qt-project.org)
 - [R](http://cran.r-project.org)
 - [boost](http://boost.org)
 - [libarchive](http://libarchive.org/)
 - [zlib](http://zlib.net/)

Required files on Windows and Mac OS X
--------------
To be able to build JASP the listed dependencies are required and can be easily installed on your system through [jasp-required-files](https://github.com/jasp-stats/jasp-required-files.git)
Simply clone the git repository next to your jasp-desktop folder as follows:

	[+] jasp-desktop < from github >
	[+] jasp-required-files < from github >

Windows
-------

Building JASP under windows is the most temperamental but should pose no large problems. Beside the above described GitHub repositories, you will need to install the following preliminaries to build JASP on Windows, later on described more specific: 

- [Qt 5.12](https://www.qt.io/download) Download the Open Source version from (https://www.qt.io/download).
- [Visual Studio 2017](https://www.visualstudio.com/downloads/) Download the community version from (https://www.visualstudio.com/downloads/)
- [R Tools 3.5](https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe) Download from (https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe)

Besides installing and cloning the software above one needs to make some kits in Qt Creator to be able to build JASP and separately JASP-R-Interface. 

Assume that the root folder of your JASP build folder is <JASP>.
To build JASP follow the next steps:
	
1. Clone JASP sources from the jasp-desktop github repository.
	From <JASP> root folder in a terminal type:
	
	\> git clone https://github.com/jasp-stats/jasp-desktop.git
	
	Or from a browser go to https://github.com/jasp-stats/jasp-desktop and choose the Clone or Download option.
	
	<img src="https://static.jasp-stats.org/images/Clone-or-Download.png" width="400" height="56" />
	
	You should now have:  
	\<JASP\>\jasp-desktop
	
2. Clone some third party binaries, boost and used R-packages  from jasp-required-files repository on GitHub. 
	From \<JASP\> root folder in a terminal type:
	
	\> git clone https://github.com/jasp-stats/jasp-required-files.git
	
	You should now have:  
	\<JASP\>\jasp-required-files
	
3. Switch to the Windows branch in jasp-required-files. From \<JASP\> root folder in a terminal type:
	
	Warning in advance:  
Because the jasp-required-files folder contains binary files as well as R packages with text files it is necessary that git performs a checkout or commit without changing the line endings. Some packages might generate MD5 checksum errors if line endings are changed. It is possible to change this behavior of git configuration per repository. For more information on this subject see https://help.github.com/articles/dealing-with-line-endings/  
(To use a repostory specific setting for this : in the jasp-required-files folder type: \>git config core.autocrlf false)

	\> cd \<JASP\>\jasp-required-files  
	\> git checkout Windows  
	\> git branch  
	
	Should confirm that you are on the Windoows branch now.
	
	
4.	Create a build folder(s). From the \<JASP\> root folder for a 64-bit version e.g.:

	\> mkdir build-release-64  
	\> mkdir build-debug-64  
	
	Latter if you want to build a debug version. In the description it is assumed that you build a release version. 
	You should now have:  

	 \<JASP\>\build-release-64  
	 \<JASP\>\build-debug-64  
	
	The destinction between debug version and release only differs in the option you choose in QtCreator. Olnly the description for the release version is given.
	 
5.	Copy files to its expected location in the build folders:    
	From \<JASP\>\jasp-required-files\64\\* -> \<JASP\>\build-release-64  
	From \<JASP\>\jasp-required-files\R -> \<JASP\>\build-release-64\R  
	From \<JASP\>\jasp-required-files\boost_1_64_0 -> <JASP>\boost_1_64_0  
	
	P.S. Instead of copying the files it is preferred to generate a symbolic link to the R and boost folder. From the buildfolder or Jasp root folder:  
	\> cd <JASP>\build-release-64  
	\> mklink /D R ..\ jasp-required-files\R  
	\> cd \<JASP\>    
	\> mklink /D boost_1_64_0 .\jasp-required-files\boost_1_64_0  
		
	You should now have :  

	\<JASP\>\build-release-64\R    	
	\<JASP\>\build-release-64\*.lib and *.dll    
	\<JASP\>\boost_1_64_0  

6.	Install Qt 5.12  
	Go to https://www.qt.io/download  
	Choose Open Source and Download.  
	Start qt-unified-windows-x86-3.0.6-online.exe from your download folder.  
	(Skip some forms if you do not have a Qt account)  
	Use the default options but select the following components to install:

![Image of Qt Installer](https://static.jasp-stats.org/images/QtCreator-Components.png)  
  
  
You will now have QtCreator in \<QTINSTALLDIR\>\Tools\QtCreator\bin\qtcreator.exe (and in your start menu).        
For following updates of Qt you can use the MaintenanceTool for Qt in \<QTINSTALLDIR\>\MaintenanceTool.exe (and in your start menu).  


7. Install Microsoft Visual Studio 2017  
	Go to https://www.visualstudio.com/downloads/  
	Download Community version  
	Start vs_community_.. from your download folder.  
	Choose all the default options.	 
	For components to install only choose the Desktop development with C++ option:  
	
![Image of Qt Installer](https://static.jasp-stats.org/images/Visual-Studio-Options.png)  
	
8. Install RTools 3.5  
	Download from https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe  
	Start RTools35 from your download folder.  
	Choose the default options.  

	You will now have RTools 3.5 installed in C:\RTools  

9.	The last steps you have to do is configuring QtCreator with the proper kits to build JASP.    
	Start QtCreator and load, through File->Open File or Project, the JASP.pro file from <JASP>\jasp-desktop\JASP.pro.  
	Also load <JASP>\jasp-desktop\JASP-R-Interface\JASP-R-Interface.pro.  
	Both projects are built with a different kit in Qt because they are built with different compilers.  
	Select Manage Kits in QtCreator, through Project in the side panel, and go to the Compilers Tab and add manually the MINGW compiler needed to build JASP-R-Interface. After adding a new compiler select its location in RTools mingw folder.   
	The compiler tab should now be similar to:  

![Image of Qt Installer](https://static.jasp-stats.org/images/Compilers.png)    

	The Debuggers tab should now be similar to:  
	
![Image of Qt Installer](https://static.jasp-stats.org/images/Debuggers.png)   	

You should now create two kits, one for building JASP desktop and one for building JASP-R-Interface. Both are equal except for the Compiler versions. The kits should look similar to:

![Image of Qt Installer](https://static.jasp-stats.org/images/MSVC-Kit.png)   

And   

![Image of Qt Installer](https://static.jasp-stats.org/images/MINGW-Kit.png) 

Your deployment configuration should now show something like:  

![Image of Qt Installer](https://static.jasp-stats.org/images/JASP-Configuration.png)   
![Image of Qt Installer](https://static.jasp-stats.org/images/JASP-R-Interface-Configuration.png)   
  
  
10. From the Projects Options in the side panel check if your build folders are correct.
  
  
![Image of Qt Installer](https://static.jasp-stats.org/images/BuildDir.png)  

For the JASP-R-Interface the Build directory should be build-release-64\JASP-R-Interface.
 
11. Start building the JASP-R-Interface first, you will need this to finish the JASP build properly.

12. Now set JASP as Active project and build JASP.  



Mac OS X
--------
To build JASP you need to clone **jasp-desktop** and **jasp-required-files** repositories, install XCode, Command line tools, Qt and some packages in R. 

 0. Clone the repository **jasp-desktop**
 1. [XCode and command line tools](https://developer.apple.com/xcode/) Qt on OS X relies on XCode and Command line tools to function. You can install them through the App Store or the link provided above. Once you've installed both, you should: 
 - open XCode 
 - Go to "Preferences" - "Locations"
 - Tell XCode which "Command Line Tools" to use 
 - Close

 2. [Qt](https://www.qt.io/): **Install Qt 5.12**
![Image of Qt Installer](https://static.jasp-stats.org/images/jasp2.InstallQt.png)

 2.a. **Configure Qt5.12**: Once installed:
 - Go to "Qt Creator" - "Preference" - "Kits" 
 - Auto-detect should give "Desktop Qt 5.12.0 clang 64bit". Click on this. 
 - Choose the compiler **Clang (x86 64bit in /usr/bin)** for both C and C++.

![Image of Qt Configuration](https://static.jasp-stats.org/images/jasp2a.ConfigureQt.png)

 2.b. **Configure project**: Click "Projects" in the left ribbon and provide the "debug build" and "release build" folders with the correct compilers. This should look like:

 ![Image of Project debug](https://static.jasp-stats.org/images/jasp2b.1.ConfigureProjectDebug.png)

 and like:

 ![Image of Project release](https://static.jasp-stats.org/images/jasp2b.2.ConfigureProjectRelease.png)

In both case, I've added the flag "-j4" to make use of all my four cores on my mac.

 3. Clone the repository **jasp-required-files** and select the **MacOS** branch. These files can now be put in the folders as shown here:

 ![Image of folder structure](https://static.jasp-stats.org/images/jasp5.FolderStructure.png)

where the blue files are the binaries that are added manually. The process will be smoothened out in the near future.

 4. Install packages in your local R for JASP to build JASPGraph: 

``` 
 install.packages(c("ggplot2", "scales", "cowplot", "gridExtra", "stringr"))
```

The process will be smoothened out in the near future.

 5. Build JASP in Qt. 

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
