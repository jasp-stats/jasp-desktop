# macOS Build Guide

If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
- [Qt Framework and Qt Creator](https://www.qt.io/download)
- [Homebrew](http://brew.sh)
- CMake: Installed via Homebrew and Qt Creator
- Conan: Installed via Homebrew
- Third-party Libraries
	- R.framework, installed automatically
	- GNU Fortran, installed automatically
	- ReadStat, installed automatically
	- JAGS, installed automatically

#### Install Xcode

You can install Xcode from the [Mac App Store](https://apps.apple.com/us/app/xcode/id497799835?mt=12).

> ⚠️ This is going to take a while, and it has to be finished before you can configure with the rest of the process.

#### Qt Framework and Qt Creator

You also need Qt Creator and Qt 6 to be able to build and test JASP's libraries and GUI. For this, 

- You need to download and install the Qt installer from [Qt website](https://www.qt.io/download-open-source).
- Make sure you scroll all the way down to where the FAQ starts and find the button "Download the Qt online installer". This gives access to both proprietary and open-source installs. They want to hide the open-source installs real bad to get people to buy a developer license but it isn't necessary for us. You *do* need a Qt account but you can signup straight from the installer.
- After downloading the installer, run the installer and make sure that the following packages are selected for installation
	- **Qt**
		- **Qt 6.3.1** (or newest stable Qt)
			- [x] macOS
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

#### Homebrew and Homebrew Packages (Conan, CMake, etc.)

You can download and install Homebrew using the following command, which might prompt you to install Xcode Command Line Tools. If that's the case, please do so. 

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

> ⚠️ Please pay close attention to outputs of the following command, and if you ran into any problems installing Homebrew, check out their [Documentation](https://docs.brew.sh) and ask for help in [GitHub Discussions](https://github.com/orgs/Homebrew/discussions).

After installing Homebrew, you can start install the other requirements by running the following command in the Terminal.

```bash
brew install conan cmake bison flex pkg-config automake autoconf create-dmg parallel
```

> 💡 Optionally, you can install the Qt from the command line, and build JASP using that, e.g., `brew install qt`.


### Configuring and Building JASP with Qt

After installing all your dependencies, go to the Qt Creator app - go to open - your cloned `jasp-desktop` folder and open `CMakeLists.txt`. By doing so, Qt Creator configures the JASP project using CMake.

JASP's CMake configuration file provides several variables that allow you to customise your build. Here, we mention a few important ones, and you can find the rest of the variables for further customisation in your "Projects" tab.

- `GITHUB_PAT`
	- If you don't have a `GITHUB_PAT`, please check the [build introduction](jasp-building-guide.md)
	- After creating a new PAT, you can set that `GITHUB_PAT` variable in two ways,
		- If you are using the Qt Creator, under the "Project Settings" on the left panel, find and click on the "Environment" item. Here, you can "Add" a new variable, `GITHUB_PAT`, and for its value enter the PAT that you've created. 
	- If you are using the command line, CMake looks for this variable in your environment variable and is able to automatically find and use it, if your `GITHUB_PAT` can be found in your environment variables.
		- Adding the following to your `.bash_profile`, or `.zshrc` will fasciliate this process, `export GITHUB_PAT=your github pat`
- Lastly, you need to make sure that `Add build library search path to DYLD_LIBRARY_PATH and DYLD_FRAMEWORK_PATH` is set to `OFF`.
	- ⚠️ This is important, otherwise, Qt Creator cannot run JASP!

If you change any of these parameters, you need to reconfigure the CMake. This as soon as you press the `Run CMake`. At this point, Qt Creator rerun the CMake configuration, and prepares everything for a build. You can check the progress of the CMake configuration in the "General Messages" output panel. If you don't get any error, the last few messages will look like this, and you are ready to build JASP.

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

Now you can press the "Build" button, and start building JASP. If everything goes well, you have not missed anything, the build will be successful, and you should be able to run JASP by pressing the "Play" button in the left bottom corner.


### Troubleshooting

Here is a list of issues we encountered in the past and possible solutions for them.

#### When creating a .dmg installer

- If under Build Steps `install` and `dmg` targets are selected, and the following error occurs:

```
CMake Error at cmake_install.cmake:69 (file):
  file cannot create directory: /usr/local/JASP.app/Contents/Resources.
  Maybe need administrative privileges.
```

Check `CMAKE_INSTALL_PREFIX` variable. It should be set to `<path-to-your-build-folder>/Install`

- When `dmg` target is selected and the following error occurs:

```
AppleEvent timed out. (-1712)
Failed running AppleScript
```

1. Try `brew upgrade create-dmg`
2. Go to System preferences -> Security & Privacy -> Privacy -> Accessibility, and add the Terminal application (you may need to use your admin password to unlock the option). [See here.](https://github.com/create-dmg/create-dmg/issues/72#issuecomment-447400844)


- When `dmg` target is selected and the following error occurs:

```
hdiutil: attach failed - Device not configured
Device name:     
```

Try disabling Spotlight indexing: `sudo mdutil -i off`. [See here.](https://github.com/electron-userland/electron-builder/issues/4606#issuecomment-667641621)
