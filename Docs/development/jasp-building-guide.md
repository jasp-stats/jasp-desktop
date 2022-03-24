Guide for Building JASP
======================

Before you start building JASP, you need to clone its repository. If you are not familiar with Git, and GitHub, you may want to read our brief [git tutorial](Docs/development/git-guide.md). 

If you know how git work, then we are going to start by cloning the `jasp-desktop` repository by writing the following commands into your terminal:

```bash
git clone https://github.com/amirmasoudabdol/jasp-desktop.git
```

If everything goes right, you now have a folder named `jasp-desktop` in your current directory. Now, we need to update JASP submodules by executing the following commands:

```bash
cd jasp-desktop
git update submodule --init
```

Now that we have everything downloaded and updated, you can continue to your our system specific build guides, 

- [macOS](Docs/development/jasp-build-guide-macos.md)
- [Linux](Docs/development/jasp-build-guide-linux.md)
- [Windows](Docs/development/jasp-build-guide-windows.md)