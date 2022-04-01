# macOS Build Guide

If you have not cloned the `jasp-desktop` repository, please head back to the [build introduction](Docs/development/jasp-building-guide.md), and after cloning the repo, and updating the submodules continue with the rest of this article.

## Requirements

- [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7
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

#### Homebrew Packages

You can download and install Homebrew using the following command. After running the following command, you may be prompted to install Xcode Command Line Tools. You need this package.

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

After installing Homebrew, you can start installing your requirements by running the following command in the Terminal.

```bash
brew install conan bison flex pkg-config automake autoconf create-dmg parallel
```

> 💡 Optionally, you can install the Qt from the command line, and build JASP using that, e.g., `brew install qt`. 

### Qt Creator

After installing all your dependencies, you can open the `CMakeLists.txt` file inside the Qt Creator app. By doing so, you are techincally opening the JASP project, and Qt Creator starts to configure the JASP project using this file.

JASP's CMake configuration file provides several variables that allow you to customize your build. Here, we mention a few important ones, and you can find the rest of the variables for further customization in your "Projects" tab.

- `GITHUB_PAT`
	- If you don't have a `GITHUB_PAT`, you can create one by following this instruction: [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
	- After creating a new PAT, you can set that `GITHUB_PAT` variable in two ways,
		- If you are using the Qt Creator, you need to either set the `GITHUB_PAT` directly to your CMake variables, under the "Projects->Build->Current Configuration". Search the list for `GITHUB_PAT`, and paste your PAT value there. This will instruct the CMake to use your PAT whenever necessary. 
	- If you are using the command line, CMake looks for this variable in your environment variable and is able to automatically find and use it, if your `GITHUB_PAT` can be found in your environment variables.
		- Adding this like to your `.bash_profile`, or `.zshrc` will fasciliate this process, `export GITHUB_PAT=your github pat`
- `INSTALL_R_MODULES`
	- If you wish to install JASP modules, you need to toggle this option `ON`. In Qt Creator->Projects->Builds->Current Configuration, you can find this variable, and check the checkbox. Or if you are using a command line, you can include this in your command line by `-DINSTALL_R_MODULES=ON`.
- Lastly, you need to make sure that `Add build library search path to DYLD_LIBRARY_PATH and DYLD_FRAMEWORK_PATH` is set to `OFF`. 
	- ⚠️ This is important, otherwise, Qt Creator cannot run JASP!

If you change any of these parameters, you need to reconfigure the CMake. This as soon as you press the `Run CMake`. At this point, Qt Creator rerun the CMake configuration, and prepares everything for a build. You can check the progress of the CMake configuration in the "General Messages" output panel. If you don't get any error, the last few messages will look like this, and you are ready to build JASP. 

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

Now you can press the "Build" button, and start building JASP. If everything goes well, you have not missed anything, the build will be successful, and you should be able to run JASP by pressing the "Play" button in the left bottom corner.