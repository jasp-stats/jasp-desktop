# Windows Build Guide

If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](Docs/development/jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- [Microsoft Visual Studio](https://visualstudio.microsoft.com/downloads/)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7 is in beta, and it has a much better CMake support!
- [MSYS2](https://www.msys2.org/), for building some of the dependencies
- [RTools](https://cran.r-project.org/bin/windows/Rtools/rtools40.html), for building R modules
- [Conan](https://github.com/conan-io/conan/releases) > 1.45.0
- [WIX Toolset](https://wixtoolset.org), if you want to distribute JASP, i.e., creating an installer.

### Installing Visual Studio

Before everything, you need to download and install the Microsoft Visual Studio and make sure that it contains all the necessary C++ libraries and tools. Please follow the steps below:

- Download the Visual Studio with C++ Community Edition 2022 from [Microsoft website](https://visualstudio.microsoft.com/downloads/)
	- Head to the bottom of the page, select the Windows, and then for the languages and platforms, select the C++. This should give you an installer with all the C++ tools and libraries that we need.
- If you are installing the VS with C++ Community, you don't need to add or modify your installation, however, if you are downloading the general version, you need to make sure that the following packages are being installed.
	- During the installation, you will be asked asked to customize your installation, in this section, here, make sure to select the followings, 
		- From the left panel, you should select the "Desktop development with C++" module. This package includes several tools, e.g.,
			- Just-In-Time debugger
			- VC++ 2019 or VC++ 2022
			- C++ profiling tools
			- Visual C++ tools for CMake

### Installing Qt Creator and Qt 6

You also need Qt Creator and Qt 6 to be able to build and test JASP's libraries and GUI. For this, 

- You need to download and install the Qt installer from [Qt website](https://www.qt.io/download).
- After downloading the installer, run the installer and make sure that the following packages are selected for installation
	- Qt
		- Qt 6.2.4
		- Developer and Designer Tools
			- Qt Creator 7.0 Beta (Recommended)
			- Qt Creator Debug Symbols
			- CMake
			- Ninja

### Installing MSYS2 

Download the MSYS2 from [here](https://www.msys2.org/) and install it in the **default** path, i.e., `C:\msys64`.

> ⚠️ This is important because JASP build system expect to find the MSYS2 in the following default path, otherwise you need to specify your custom path to CMake, using the `MINGW_PATH` variable, e.g., `-DMINGW_PATH=D:\msys64`.

#### Installing MSYS2 Libraries and Packages

After installing MSYS2, you will find a new program in your Start Menu. Search for "MSYS2" in your Start Menu, and from the selection of applications that are showing up, run the one name "MSYS2 MinGW x64". At this point, you should be welcomed with a command prompt.

Copy and paste the following line into the Terminal and press Enter. With this command, we are installing some of required packages and libraries necessary for building JASP.

```bash
pacman -Syu mingw-w64-x86_64-toolchain mingw-w64-x86_64-boost mingw-w64-x86_64-jsoncpp bison flex make autoconf
```

#### Downloading and Building ReadStat (on MSYS2)

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

> ⚠️ Make sure that you select a box which prompt you for whether or not to add the Conan to your PATH. You should make sure that the box is checked and Conan can be found in your PATH; otherwise, you might run into a problem later.

After installing Conan, you should make sure that Conan is configured correctly. Open the Windows Terminal, or the Command Prompt, and run the following commands:

```bash
conan profile new default --detect
conan profile show default
```

You should see something like this. CMake later uses this template to modify some of its parameters and download and prepare the dependencies accordingly. For instance, if you are building a Debug version of JASP, CMake will change this profile to account for that.

```
[settings]
os=Windows
os_build=Windows
arch=x86_64
arch_build=x86_64
compiler=Visual Studio
compiler.version=16
build_type=Debug
[options]
[conf]
[build_requires]
[env]
```

> 💡 Although CMake and Qt Creator will run Conan process for you, if it's your very first time configuring JASP, and you ran into any problem, you can run the Conan command manually. If things go wrong, CMake configuration will stop and tells you what you should do to resolve the Conan issue. 

### Configuring JASP Desktop

At this point, you are ready to start configuring and building JASP. Open the Qt Creator, and select "File → Open Project", then find and open the `CMakeLists.txt` file inside the `jasp-desktop` folder. By opening this file, you are opening the entire JASP project, and and you will be prompted to "Manage Kits". Here you want to select the "Desktop Qt 6.2.3 MSVC2019 64bit" kit by checking the checkbox next to it. 

> ⚠️ This is an important step, and if you miss selecting the **Desktop** kit, you will not be able to build JASp.

After selecting your kit, you can select the "Create Configuration", and observe the Qt Creator starting to configure your project. You can see the progress of the CMake configuration in the "General Messages" output panel, usually Ctrl+6.

If this is your first time preparing your project, CMake is going to configure **everything**, and as a result the *very first* configuration is going to be time confusing depending on your settings, internet speed, and computer performance. During the configuration, CMake downloads and instance of R, unpack it and place it in the appropriate location inside the build folder. In addition, it looks for all the necessary libraries and programs required as well. If any of the requirement isn't met, CMake will stop the configuration and notify you about the problem. However, if everything else is prepared correctly, you will not receive any errors, and your project will be configured, and you will see a set of messages as below:

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

#### ⚠️ R-Interface 

CMake makes sure that it build the R-Interface using the MinGW x64 libraries every time (if necessary). So, unlike before, you don't need to anything special to have the R-Interface build and prepared, however, you need to make sure that the `C:\msys64\mingw64\bin` is in your PATH. You can add this address to your Build Environment path inside the Qt Creator.

Find the "Build Environment" section under the "Projects -> Build", and expand its details by clicking the "Details". Here, you need to find the `Path` variable, select it, press "Edit", and add the mentioned path to the list.

#### Configuring the CMake Variables

CMake can to some extend configure the JASP project for you. Here are a few of the variables that you can use to customize your build,

- `GITHUB_PAT`
	- If you don't have a `GITHUB_PAT`, you can create one by following this instruction: [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
	- After creating a new PAT, you can set that `GITHUB_PAT` variable in two ways,
		- If you are using the Qt Creator, you need to either set the `GITHUB_PAT` directly to your CMake variables, under the "Projects->Build->Current Configuration". Search the list for `GITHUB_PAT`, and paste your PAT value there. This will instruct the CMake to use your PAT whenever necessary. 
	- If you are using the command line, CMake looks for this variable in your environment variable and is able to automatically find and use it, if your `GITHUB_PAT` can be found in your environment variables.
		- Adding this like to your `.bash_profile`, or `.zshrc` will fasciliate this process, `export GITHUB_PAT=your github pat`
- `INSTALL_R_MODULES`
	- If you wish to install JASP modules, you need to toggle this option `ON`. In Qt Creator->Projects->Builds->Current Configuration, you can find this variable, and check the checkbox. Or if you are using a command line, you can include this in your command line by `-DINSTALL_R_MODULES=ON`.

Remember that you need to re-run the CMake configuration every time you change any of the variables.


### Building JASP

Before attempting to build JASP, configure the CMake again, by pressing the "Run CMake" button, and wait for everything to finish. If you didn't get any errors, you can start building JASP by pressing "Build" button.
