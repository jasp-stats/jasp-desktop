# Linux Build Guide

If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- Libraries
	- `boost (>=1.78)`
	- `jsoncpp (>=1.9)`
	- `libarchive (>=3.5)`
	- `cmake (>3.21)`
	- `openssl (1.1.1m)`
	- `autoconf`
	- `zlib`
	- `bison`
	- `flex`
- Qt (>= 6.2)
	- Qt Creator 7

### Installing Dependencies

Based on your system, you can install the mentioned libraries using your package manager.

On Ubuntu, you can use `apt`. 

```
sudo apt install boost jsoncpp libarchive autoconf zlib cmake gfortran build-essential r-base
```

> ⚠️ Some of these libraries might not be up-to-date and as a result JASP will complain. If this happens, you need to download, make and install those libraries individually. Alternatively, you can use the [Linux version of Homebrew](https://docs.brew.sh/Homebrew-on-Linux) and install the up-to-dated libraries locally.

On Manjaro / Arch

```
sudo pacman -Syu boost jsoncpp libarchive cmake zlib libarchive make autoconf qt6 gcc gcc-gfortran r
```

### Qt Framework and Qt Creator

You also need Qt Creator and Qt 6 to be able to build and test JASP's libraries and GUI. For this, 

- You need to download and install the Qt installer from [Qt website](https://www.qt.io/download).
- After downloading the installer, run the installer and make sure that the following packages are selected for installation
	- **Qt**
		- **Qt 6.2.4** (or newest stable Qt)
			- [x] Desktop gcc 64-bit
			- [x] Qt 5 Compatibility Module
			- [x] Qt Shader Tools 
			- **Additional Libraries**
				- [x] Qt Web Engine
				- [x] Qt Web Channel
				- [x] Qt Positioning
		- **Developer and Designer Tools**
			- **Qt Creator 7**
			- [x] Qt Creator 7 Debug Symbols
			- [x] CMake
			- [x] Ninja

### ReadStat and JAGS

On Linux JASP's CMake script will download and install ReadStat and JAGS for you when necessary.

### Installing Qt Creator / Qt

Similar to Windows and macOS, you can download and install the Qt Framework from Qt website, [here](https://www.qt.io/download). 

> 💡 Sometimes, dependeing on your Linux distribution, you might be able to get the Qt libraries using your package manger. For instance, if you are running any variant of an Arch Linux, e.g., Manjaro, `pacman` readily offers the latest build of the Qt libraries, and you can download it by running the following command, `pacman -Syu qt6`.

### Configuring and Building JASP Desktop

After installing all the libraries and the Qt library, open the Qt Creator, and open the `jasp-desktop/CMakeLists.txt`. This initiates the configuration of the `jasp-desktop/`. 

On Linux, you can configure the `jasp-desktop` to use a custom R installation on your system, to do so, you can set the `CUSTOM_R_PATH` variable to the home of your R installation, e.g., `/usr/lib64/R`.

In addition, if you do not wish to pollute your custom library directory, you can construct CMake to install all its libraries into the build folder. You can do this by setting the `LINUX_LOCAL_BUILD` to `ON`. In this case, JASP creates a new directory at `<build-folder>/R/library` and installs everything there.
