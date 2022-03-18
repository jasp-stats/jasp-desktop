# Linux Build Guide

On Linux, 

## Requirements

- Libraries
	- `boost (>1.78), jsoncpp, libarchive, autoconf, zlib, cmake (>3.21)`
- Qt Creator / Qt (> 6.2)


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

### ReadStat and JAGS

On Linux JASP's CMake script will download and install ReadStat and JAGS for you when necessary.

### Installing Qt Creator / Qt

Similar to Windows and macOS, you can download and install the Qt Framework from Qt website, [here](https://www.qt.io/download).

### Configuring and Building JASP Desktop

After installing all the libraries and the Qt library, open the Qt Creator, and open the `jasp-desktop/CMakeLists.txt`. This initiates the configuration of the `jasp-desktop/`. 

On Linux, you can configure the `jasp-desktop` to use a custom R installation on your system, to do so, you can set the `CUSTOM_R_PATH` variable to the home of your R installation, e.g., `/usr/lib64/R`.

In addition, if you do not wish to pollute your custom library directory, you can construct CMake to install all its libraries into the build folder. You can do this by setting the `LOCAL_LINUX_BUILD` to `ON`. In this case, JASP creates a new directory at `<build-folder>/R/library` and installs everything there.
