# CMake Build Guide [Under Development]



## macOS


### Requirements

- [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7 is in beta, and it has a much better CMake support!
- [Homebrew](http://brew.sh)


#### Install Xcode

You can install Xcode from the [Mac App Store](https://apps.apple.com/us/app/xcode/id497799835?mt=12). 

> ⚠️ This is going to take a while, and it has to be finished before you can configure with the rest of the process.

#### Homebrew Packages

You can download and install Homebrew using the following command. After running the following command, you may be prompted to install Xcode Command Line Tools. You need this package.

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

After installing Homebrew, you can start installing your requirements by running the following command in the Terminal.

```bash
brew install boost libarchive zlib bison flex jsoncpp pkg-config automake libiconv openssl autoconf brotli
```

If you are planning to build JAPS from the command line, we recommend installing Qt using Homebrew as well, `brew install qt`.

> ℹ️ Some of these libraries might be included in your Xcode installation, but due to differences between macOS and Xcode version, we recommend you to install those all the libraries via Homebrew anyway.

After installing everything, and based on your system configuration, you need to make sure that `pkg-config` can find them. You can do that by adding the path to their pkgconfig's `.rc` files to your `.bash_profile` or `.zshrc` file. Not all libraries need to be added to the `PKG_CONFIG_PATH`. You should have something similar to this at the bottom of your `.zshrc` file:

```
export PKG_CONFIG_PATH="/opt/homebrew/opt/xz/lib/pkgconfig:${PKG_CONFIG_PATH}"
export PKG_CONFIG_PATH="/opt/homebrew/opt/zlib/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/opt/homebrew/opt/libarchive/lib/pkgconfig:${PKG_CONFIG_PATH}"
export PKG_CONFIG_PATH="/opt/homebrew/opt/jsoncpp/lib/pkgconfig:${PKG_CONFIG_PATH}"
export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig:${PKG_CONFIG_PATH}"
```

#### ReadStat

On macOS, JASP's CMake script configure and build ReadStat the first time you need it, so, you don't need to do anything extra like in Windows.

### Qt Creator

After installing all your dependencies, you can open the `CMakeLists.txt` file inside the Qt Creator app. Qt Creator will be able to configure the JASP project using this file, however there are a few things that you still need to take care of:

1. Make sure that `INSTALL_JASP_REQUIRED_LIBRARIES` is set to `ON` in the Properties panel of the Qt Creator.
2. Similarly, you need to set your `GITHUB_PAT` variable to your [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
3. If you wish to install JASP modules, you can do so by switching the `INSTALL_R_MODULES` to `ON` as well.
4. Lastly, you need to make sure that `Add build library search path to DYLD_LIBRARY_PATH and DYLD_FRAMEWORK_PATH` is set to `OFF`. 
	- ⚠️ I'm not sure if this is only on my machine or it needs to be off everywhere, but if I have this on, JASP won't run inside the Qt Creator. I will investigate this later, but it's most likely it's because I'm having two very different version of Qt present on my system. 

After this parameters, re-run the CMake and wait until you see the following message. This means that JASP is configured correctly, and you can start building JASP.

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

---

## Windows


### Requirements

- [Microsoft Visual Studio](https://visualstudio.microsoft.com/downloads/)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7 is in beta, and it has a much better CMake support!
- [MSYS2](https://www.msys2.org/)
- [Conan](https://github.com/conan-io/conan/releases) > 1.45.0

#### Installing Visual Studio

Before everything, you need to download and install the Microsoft Visual Studio and make sure that it contains all the necessary C++ libraries and tools. Please follow the steps below:

1. Download the Visual Studio with C++ Community Edition 2022 from [Microsoft website](https://visualstudio.microsoft.com/downloads/)
2. Install the Visual Studio with C++ on your system, and make sure that you following items are selected for installation, or have been installed already
	- MSVC
	- [and some others that I need to add]

#### Installing Qt Creator and Qt 6

You also need Qt Creator and Qt 6 to be able to build and test JASP's libraries and GUI. For this, 

- You need to download and install the Qt installer from [Qt website](https://www.qt.io/download).
- After downloading the installer, run the installer and make sure that the following packages are selected for installation
	- Qt
		- Qt 6.2.3
		- Developer and Designer Tools
			- Qt Creator 7.0
			- Qt Creator Debug Symbols
			- CMake
			- Ninja

#### Installing MSYS2 

Download the MSYS2 from [here](https://www.msys2.org/) and install it in the **default** path. 

> ⚠️ This is important because JASP build system expect to find the MSYS2 in the following default path, i.e., `C:\msys64`


##### Installing MSYS2 Libraries and Packages

After installing MSYS2, you will find a new program in your Start Menu. Search for "MSYS2" in your Start Menu, and from the selection of applications that are showing up, run the one name "MSYS2 MinGW x64". At this point, you should be welcomed with a Terminal or a Command Prompt.

Copy and paste the following line into the Terminal and press Enter. With this command, we are installing some of required packages and libraries necessary for building JASP.

```bash
pacman -Syu mingw-w64-x86_64-toolchain mingw-w64-x86_64-boost mingw-w64-x86_64-jsoncpp bison flex libtool mercurial
```


##### ReadStat and JAGS

In addition to these libraries, you need to manually download and install the ReadStat library. You can do that by typing the following commands into the command line.

```
git clone https://github.com/WizardMac/ReadStat.git
git checkout tags/v1.1.7
cd ReadStat
./autogen.sh
./configure
make -j
make install
```

```
hg clone http://hg.code.sf.net/p/mcmc-jags/code-0
cd code-0
autoreconf -fi
./configure
make -j
make install
```

This will build and install these libraries inside the MSYS environment where JASP will look for them. If any of these steps goes wrong, JASP's build system cannot configure the build.

#### Installing Conan

We are using Conan to manage some of the dependencies of JASP on Windows, so you need to make sure that Conan is installed in your system.

You can download Conan from their [GitHub Release page](https://github.com/conan-io/conan/releases). Make sure that you are downloading 1.45.0 or higher, otherwise you might ran into some compatibility issues with Visual Studio and MSVC. You can also install Conan using Python, `pip install --upgrade conan`.

After installing Conan, you should make sure that Conan is configured correctly.

```bash
conan profile new default --detect
conan profile show default
```

For the most part, Conan detect your system correctly, and if you don't mind building missing binaries, you can just leave the config as is. You want to make sure that `compiler.version` is set to the version of your Visual Studio, and `compiler.runtime` is set to `MDd` if you are building for Debug.

```bash
conan profile update settings.compiler.version=16 default
conan profile update settings.compiler.runtime=MDd default
conan profile update settings.build_type=Debug default
```

> 💡 Although CMake and Qt Creator will run Conan process for you, if it's your very first time configuring JASP, it's recommended to run Conan separately first. You can test the Conan process by heading to the `jasp-desktop/` and typing `conan install .`. This will install all the dependencies in your Conan's repository and cache them for later use.

### Configuring and Building JASP

Now that you have all the requirements sorted out, you can clone the `jasp-desktop` repository, and start configuring the project.

```bash
git clone https://github.com/jasp-stats/jasp-desktop.git
cd jasp-desktop
git submodule --init update
```

If everything goes right, you should see a directory full of files and directories! 

#### Configuring JASP Desktop

At this point, you are ready to start configuring and building JASP. Open the Qt Creator, and select "File → Open Project", then find and open the `CMakeLists.txt` file inside the `jasp-desktop` folder. By opening this file, you are opening the entire JASP project, and and you will be prompted to select your build environment, and variables. You want make sure that you are selecting the Desktop Kit.

After doing that, Qt Creator starts preparing the `jasp-desktop` for you. If this is your first time preparing your project, CMake is going to configure **everything**, and as a result the *very first* configuration is going to be time confusing depending on your settings, internet speed, and computer performance. 

If everything else is prepared correctly, you will not receive any errors, and your project will be configured, and you will see a set of messages as below:

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

> ℹ️ CMake makes sure that it build the R-Interface using the MinGW x64 libraries every time (if necessary). 

Let the Qt Creator to finish the first configuration, and then you need to change a few parameters to be able to all the Modules, and access GitHub without problems. 

- Set the `GITHUB_PAT` to your [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token). 
- Set the `INSTALL_R_MODULES` to `ON`, and run CMake again. 

After setting these parameters, you can re-run the CMake and if everything goes fine you are going to see another `-- Generating done` message and you are ready to build.


### Notes

On Windows, 

- Junction will be replaced by Symlinks after building each Module. At the moment, the script is being run every time but it doesn't do anything if it sees symlinks. When we are sure that this is working, we only need to do this during the staging and preparation of the WIX.

---


## Linux 

On Linux, 

### Requirements

- Libraries
	- `boost (>1.78), jsoncpp, libarchive, autoconf, zlib, cmake (>3.21)`
- Qt Creator / Qt (> 6.2)


#### Installing Dependencies

Based on your system, you can install the mentioned libraries using your package manager.

On Ubuntu, you can use `apt`. 

```
sudo apt install boost jsoncpp libarchive autoconf zlib cmake gfortran build-essential r-base
```

> ⚠️ Some of these libraries might not be up-to-date and as a result JASP will complain. If this happens, you need to download, make and install those libraries individually. Alternatively, you can use the [Linux version of Homebrew](https://docs.brew.sh/Homebrew-on-Linux) and install the up-to-dated libraries locally.

On Manjaro / Arch

```
sudo pacman -Syu boost jsoncpp libarchive cmake zlib libarchive  gcc gcc-gfortran r
```

#### ReadStat and JAGS

On Linux JASP's CMake script will download and install ReadStat and JAGS for you when necessary.

#### Installing Qt Creator / Qt

Similar to Windows and macOS, you can download and install the Qt Framework from Qt website, [here](https://www.qt.io/download).

#### Configuring and Building JASP Desktop

After installing all the libraries and the Qt library, open the Qt Creator, and open the `jasp-desktop/CMakeLists.txt`. This initiates the configuration of the `jasp-desktop/`. 

On Linux, you can configure the `jasp-desktop` to use a custom R installation on your system, to do so, you can set the `CUSTOM_R_PATH` variable to the home of your R installation, e.g., `/usr/lib64/R`.

In addition, if you do not wish to pollute your custom library directory, you can construct CMake to install all its libraries into the build folder. You can do this by setting the `LOCAL_LINUX_BUILD` to `ON`. In this case, JASP creates a new directory at `<build-folder>/R/library` and installs everything there.


## Known Issues

- Some commands and targets might run when it's not really necessary. After I'm sure that everything works, I will look into them to reduce the redundant runs
- CMake can do some caching, there is already some but I can do better, for now, until everytihng works I like to be sure that caching doesn't mess anything, so, you might notice some redownloading, rebuilding, etc.
	- The solution is using `add_custom_command` and `add_custom_target`, but they need to be used with care
- Some platform specific compiler flags might be missing
- Some parts of CMake can be more granular and modular, but I have not done it because I'm worried about CMake variable scoping. For now, I will leave them like this, when we have the CI's in place, changing and testing these will be much easier.