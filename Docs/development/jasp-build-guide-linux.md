# Linux Build Guide

If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- Libraries
	- `autoconf`^1
	- `bison`^1
	- `boost (>=1.78)`^1
	- `cmake (>3.21)`^1
	- `gcc`^1
	- `gcc-fortran`^1
	- `git`^1
	- `glpk`
	- `flex`^1
	- `jags`^1,2^
	- `jsoncpp (>=1.9)`^1
	- `libarchive (>=3.5)`
	- `openssl (1.1.1m)`^1
	- `patchelf`^1
	- `readstat`^2
	- `sqlite`^1
	- `V8` (for `jaspProcess`)
	- `zlib`^1
	- `freexl`^1

- Qt (>= 6.7)
	- Qt Creator 13

*1: make dependencies (i.e., required for building but not for running JASP)*

*2: On Linux JASP's CMake script will download and install ReadStat and JAGS
for you when necessary.*

### Installing Dependencies

Based on your system, you can install the mentioned libraries using your package manager.

On Ubuntu, you can use `apt`.

```
sudo apt install libboost-dev libjsoncpp25 libjsoncpp-dev libarchive13 libarchive-dev libxcb-xkb-dev libxcb-xkb1 libxcb-xinerama0 libxcb-cursor0 libxkbcommon-dev libxkbcommon-x11-dev autoconf zlib1g zlib1g-dev cmake gfortran build-essential flex libssl-dev libgl1-mesa-dev libsqlite3-dev r-base libglpk-dev libminizip-dev libfreexl-dev
```

> ⚠️ Some of these libraries might not be up-to-date and as a result JASP will complain. If this happens, you need to download, make and install those libraries individually. Alternatively, you can use the [Linux version of Homebrew](https://docs.brew.sh/Homebrew-on-Linux) and install the up-to-dated libraries locally.

On Manjaro / Arch

```
sudo pacman -Syu autoconf bison boost cmake gcc gcc-fortran git glpk flex jags jsoncpp libarchive openssl patchelf r readstat zlib
# JAGS and Google's V8 are only available through the aur
# Note that only jaspProcess requires V8 and V8 takes a lot of time to compile
# To install them using the `yay` aur helper run:
yay -Syu jags v8-r
```

On Fedora/RHEL

```
sudo dnf group install "C Development Tools and Libraries"
sudo dnf install cairo-devel gsl-devel readline-devel readstat-devel gfortran libXt-devel libcurl-devel boost-devel lapack-devel libsqlite3x-devel openssl-devel libarchive-devel mesa-libGL-devel libxkbcommon-devel boost-static harfbuzz-devel fribidi-devel freetype-devel libpng-devel libtiff-devel libjpeg-devel cairo-devel mpfr-devel glpk-devel
```

We recommend you compile and install R from source to a location in your home folder and set this installation path in the `CUSTOM_R_PATH CMAKE` variable. 

Additionally recommend you install JAGS from [source](https://mcmc-jags.sourceforge.io/) and do not make use of platform binaries which may be provided by your distro. If needed you can add the JAGS path to the `CMAKE_PREFIX_PATH` variable.


### Qt Framework and Qt Creator

You also need Qt Creator and Qt 6 to be able to build and test JASP's libraries and GUI. For this, 

- You need to download and install the Qt installer from [Qt website](https://www.qt.io/download).
- After downloading the installer, run the installer and make sure that the following packages are selected for installation
	- **Qt**
		- **Qt 6.7.0** (or newest stable Qt)
			- [x] Desktop gcc 64-bit
			- [x] Qt 5 Compatibility Module
			- [x] Qt Shader Tools 
			- **Additional Libraries**
				- [x] Qt Web Engine
				- [x] Qt Web Channel
				- [x] Qt Positioning
		- **Developer and Designer Tools**
			- **Qt Creator 13**
			- [x] Qt Creator 13 Debug Symbols
			- [x] CMake
			- [x] Ninja

### Dependencies

On Linux JASP's CMake script will download and install ReadStat, JAGS and FreeXL for you when necessary.

### Installing Qt Creator / Qt

Similar to Windows and macOS, you can download and install the Qt Framework from Qt website, [here](https://www.qt.io/download). 

> 💡 Sometimes, dependeing on your Linux distribution, you might be able to get the Qt libraries using your package manager. For instance, if you are running any variant of an Arch Linux, e.g., Manjaro, `pacman` readily offers the latest build of the Qt libraries, and you can download it by running the following command, `pacman -Syu qt6`.

### Configuring and Building JASP Desktop

After installing all the libraries and the Qt library, open the Qt Creator, and open the `jasp-desktop/CMakeLists.txt`. This initiates the configuration of the `jasp-desktop/`. 

On Linux, you can configure the `jasp-desktop` to use a custom R installation on your system, to do so, you can set the `CUSTOM_R_PATH` variable to the home of your R installation, e.g., `/usr/lib64/R`.

In addition, if you do not wish to pollute your custom library directory, you can construct CMake to install all its libraries into the build folder. You can do this by setting the `LINUX_LOCAL_BUILD` to `ON`. In this case, JASP creates a new directory at `<build-folder>/R/library` and installs everything there.
