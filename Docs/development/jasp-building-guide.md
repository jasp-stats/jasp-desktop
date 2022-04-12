Guide for Building JASP
=======================

Before you start building JASP, you need to clone its repository. If you are not familiar with Git, and GitHub, you may want to read our brief [git tutorial](git-guide.md). 

## Clone the `jasp-desktop` Repository

If you know how git work, then we are going to start by cloning the `jasp-desktop` repository by writing the following commands into your terminal:

```bash
git clone https://github.com/jasp-stats/jasp-desktop.git
```

If everything goes right, you now have a folder named `jasp-desktop` in your current directory. Now, we need to update JASP submodules by executing the following commands:

```bash
cd jasp-desktop
git submodule update --init
```

## Make a GitHub Personal Access Token

If you haven't got a [GitHub Personal Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token), please make one that allows access to your repository, that is:

- repo:status
- repo:rep_deployment
- repo:public_repo

## Platform Specific Build Guides

Now that we have everything downloaded and updated, you can continue to your our system specific build guides, 

- [macOS](jasp-build-guide-macos.md)
- [Linux](jasp-build-guide-linux.md)
- [Windows](jasp-build-guide-windows.md)
