# macOS Build Guide
If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
- [Qt Framework and Qt Creator](https://www.qt.io/download)
- [Homebrew](http://brew.sh)
- [CMake](https://cmake.org), installed via Homebrew and Qt Creator
- [Conan](https://conan.io), installed via Homebrew
- Third-party Libraries
	- R.framework, installed automatically
	- GNU Fortran, installed automatically
	- ReadStat, installed automatically
	- JAGS, installed automatically

#### Install Xcode

You can install Xcode from the [Mac App Store](https://apps.apple.com/us/app/xcode/id497799835?mt=12).

> ⚠️ This is going to take a while, and it has to be finished before you can configure with the rest of the process.

#### Homebrew and Homebrew Packages (Conan, CMake, etc.)

You can download and install Homebrew using the following command. After running the following command, you may be prompted to install Xcode Command Line Tools. Please pay a close attention to outputs of the following command, and if you ran into any problems installing Homebrew, check out their [Documentation](https://docs.brew.sh) and ask for help in [GitHub Discussions](https://github.com/orgs/Homebrew/discussions).

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

After installing Homebrew, you can start installing your requirements by running the following command in the Terminal.

```bash
brew install conan cmake bison flex pkg-config automake autoconf create-dmg parallel
```

> 💡 Optionally, you can install the Qt from the command line, and build JASP using that, e.g., `brew install qt`.

### Qt Framework and Qt Creator

Download the [Qt Installer](https://www.qt.io/download), and run the installer. You may need to create a [Qt Account](https://login.qt.io/login) before you will be able to download and install the Qt Framework.

Follow the installation steps, and select the following items from the list of components in the "Select Components" step:

- **Qt**
	- **Qt 6.2.4** (or newest stable Qt)
		- [x] macOS
		- [x] Qt 5 Compatibility Module
		- **Additional Libraries**
			- [x] Qt Web Engine
			- [x] Qt Web Channel
			- [x] Qt WebSockets
	- **Developer and Designer Tools**
		- **Qt Creator 7**
		- [x] Qt Creator 7 Debug Symbols
		- [x] CMake
		- [x] Ninja

### Configuring and Building JASP

After installing all your dependencies, go to the Qt Creator app - go to open - your cloned `jasp-desktop` folder and open `CMakeLists.txt`. By doing so, Qt Creator configures the JASP project using CMake.

JASP's CMake configuration file provides several variables that allow you to customise your build. Here, we mention a few important ones, and you can find the rest of the variables for further customisation in your "Projects" tab.

- `GITHUB_PAT`
	- If you don't have a `GITHUB_PAT`, please make one with the instructions provided in [build introduction](jasp-building-guide.md).
	- After creating a new PAT, you can set that `GITHUB_PAT` variable in two ways,
		- If you are using the Qt Creator, you need to either set the `GITHUB_PAT` directly to your CMake variables, under the "Projects->Build->Current Configuration". Search the list for `GITHUB_PAT`, and paste your PAT value there. This will instruct the CMake to use your PAT whenever necessary.
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
