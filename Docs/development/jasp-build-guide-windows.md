# Windows Build Guide


## Requirements

- [Microsoft Visual Studio](https://visualstudio.microsoft.com/downloads/)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7 is in beta, and it has a much better CMake support!
- [MSYS2](https://www.msys2.org/)
- [RTools](https://cran.r-project.org/bin/windows/Rtools/rtools40.html)
- [Conan](https://github.com/conan-io/conan/releases) > 1.45.0

### Installing Visual Studio

Before everything, you need to download and install the Microsoft Visual Studio and make sure that it contains all the necessary C++ libraries and tools. Please follow the steps below:

1. Download the Visual Studio with C++ Community Edition 2022 from [Microsoft website](https://visualstudio.microsoft.com/downloads/)
2. Install the Visual Studio with C++ on your system, and make sure that you following items are selected for installation, or have been installed already
	- MSVC
	- [and some others that I need to add]

### Installing Qt Creator and Qt 6

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

### Installing MSYS2 

Download the MSYS2 from [here](https://www.msys2.org/) and install it in the **default** path. 

> ⚠️ This is important because JASP build system expect to find the MSYS2 in the following default path, i.e., `C:\msys64`


#### Installing MSYS2 Libraries and Packages

After installing MSYS2, you will find a new program in your Start Menu. Search for "MSYS2" in your Start Menu, and from the selection of applications that are showing up, run the one name "MSYS2 MinGW x64". At this point, you should be welcomed with a Terminal or a Command Prompt.

Copy and paste the following line into the Terminal and press Enter. With this command, we are installing some of required packages and libraries necessary for building JASP.

```bash
pacman -Syu mingw-w64-x86_64-toolchain mingw-w64-x86_64-boost mingw-w64-x86_64-jsoncpp bison flex libtool mercurial
```


#### ReadStat and JAGS

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

This will build and install these libraries inside the MSYS environment where JASP will look for them. If any of these steps goes wrong, JASP's build system cannot configure the build.

### Installing Conan

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

## Configuring and Building JASP

Now that you have all the requirements sorted out, you can clone the `jasp-desktop` repository, and start configuring the project.

```bash
git clone https://github.com/jasp-stats/jasp-desktop.git
cd jasp-desktop
git submodule --init update
```

If everything goes right, you should see a directory full of files and directories! 

### Configuring JASP Desktop

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


## Notes

On Windows, 

- Junction will be replaced by Symlinks after building each Module. At the moment, the script is being run every time but it doesn't do anything if it sees symlinks. When we are sure that this is working, we only need to do this during the staging and preparation of the WIX.

---