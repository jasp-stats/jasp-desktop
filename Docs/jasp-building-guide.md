Guide to Building JASP
======================

JASP depends on [Qt (5+)](http://qt-project.org), [R](http://cran.r-project.org) and [boost](http://boost.org).

Windows
-------

Building JASP under windows is the most temperamental, and the versions listed here are known to work; slight variations on these numbers probably won't work.

Qt: JASP for windows is built as 64-bit and is built with Mingw-W64. The Qt project does not ship a 64-bit version of Qt based on Mingw-W64, and so we rely on a version from [here](http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/Qt-Builds/). The x64 Qt 5.2.1 version is known to work and is recommended. It also includes Mingw-64. Once downloaded and unzipped to the desired location, it is necessary to run the `QtSDK-x86_64/qtbinpatcher.exe`, so it knows about it's current location.

R: We provide an R build which is known to work [here](http://jasp-stats.org/development/R%20Win64%20for%20JASP%20%282014-07-26%29.zip)

boost: 1.54.0 works, get it from [here](http://www.boost.org/users/history/version_1_54_0.html)

The directory structure should be as follows:

<pre>
[+] jasp-desktop
[+] boost_1_54_0
[-] build-JASP- ... < build directory, created by QtCreator >
   [+] R
</pre>
 
Mac OS X
--------
Qt: JASP build on OS X is pretty robust, and most versions works. We currently use 5.3.1, but newer versions will probably work too. You can download it from [here](https://qt-project.org/downloads).

R: We provide a specially crafted R.framework [here](http://jasp-stats.org/development/R%20OSX%20for%20JASP%20%282014-07-26%29.zip)

boost: 1.54.0 works, get it from [here](http://www.boost.org/users/history/version_1_54_0.html)

The directory structure should be as follows:

<pre>
 [+] jasp-desktop
 [+] boost_1_54_0
 [-] Frameworks
    [+] R.framework
 [+] build-JASP- ... < build directory, created by QtCreator >
</pre>

Linux
-----
This is pretty straight forward