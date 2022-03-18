# macOS Build Guide


## Requirements

- [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12)
- [Qt Creator](https://www.qt.io/download) / Qt >= 6.2
    - Qt Creator 7 is in beta, and it has a much better CMake support!
- [Homebrew](http://brew.sh)
- [CMake](https://cmake.org), installed via Homebrew and Qt Creator
- [Conan](https://conan.io), installed via Homebrew
- Third-party Libraries
	- R.framwork, installed automatically 
	- gfortran, installed automatically
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
brew install conan bison flex pkg-config automake autoconf brotli
```

> If you are planning to build JAPS from the command line, we recommend installing Qt using Homebrew as well, `brew install qt`.

> ℹ️ Some of these libraries might be included in your Xcode installation, but due to differences between macOS and Xcode version, we recommend you to install those all the libraries via Homebrew anyway.

### Qt Creator

After installing all your dependencies, you can open the `CMakeLists.txt` file inside the Qt Creator app. Qt Creator will be able to configure the JASP project using this file, however there are a few things that you still need to take care of:

- You need to set your `GITHUB_PAT` variable
	- If you don't have a `GITHUB_PAT`, you can create one by following this instruction: [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
	- If you are using the command line, CMake looks for this variable in your environment variable and is able to set it automatically
	- If you are using the Qt Creator, you need to either set the `GITHUB_PAT` directly to your CMake variables, or adds it to the Environment variable
- If you wish to install JASP modules, you can do so by switching the `INSTALL_R_MODULES` to `ON`.
- Lastly, you need to make sure that `Add build library search path to DYLD_LIBRARY_PATH and DYLD_FRAMEWORK_PATH` is set to `OFF`. 
	- ⚠️ This is important, otherwise, Qt Creator cannot run JASP!

After these parameters, re-run the CMake and wait until you see the following message. This means that JASP is configured correctly, and you can start building JASP.

```
-- Configuring done
-- Generating done
-- Build files have been written to: <path-to-your-build-folder>
```

Now you can press "Build" and start building!

---