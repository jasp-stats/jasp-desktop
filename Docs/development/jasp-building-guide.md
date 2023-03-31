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

## GITHUB_PAT
We use R both inside JASP and during building. During the buildprocess [renv](https://github.com/rstudio/renv) is used to recreate the r-library that a module expects.
As it does so it often queries [github](https://github.com), this can run foul of the rate limiter they have for anonymous requests. 

This look like:
```
Error: failed to resolve remote 'jasp-stats/jaspBase' -- failed to retrieve 'https://api.github.com/repos/jasp-stats/jaspBase' [error code 22]
In addition: Warning message:
curl: (22) The requested URL returned error: 403  
Traceback (most recent calls last):
26: pkgbuild::with_bui...
```

Luckily it is easy to solve, just create a personal access token just to let github know you are not trying to DDOS them (I guess).
You can create one under `Settings -> Developer settings -> Personal access tokens" or just [click here](https://github.com/settings/tokens/new).
You don't need to give it any scopes/permissions. 
![Shows the settings page on github](img/GithubPersonalToken.png)
This will generate a hashcode and you can copy it to a environment variable.
This can be done systemwide and that has the advantage that instances of R can also use it.

The simplest cross-platform way will be described here though, just open up the projects pane in qt creator.
Under the header 'Build Environment' press 'Details'. Then use 'Add' on the right of the panel to to include an environment variable called `GITHUB_PAT` and paste the token you create in github as the value.
![Shows the projects pane of qt creator with an example of a GITHUB_PAT](img/GithubPatEnv.png)
This can also be set in JASP itself under `Preferences -> Advanced`.

It is also possible to set it as a CMAKE variable if you prefer that.

## Platform Specific Build Guides

Now that we have everything downloaded and updated, you can continue to your our system specific build guides, 

- [macOS](jasp-build-guide-macos.md)
- [Linux](jasp-build-guide-linux.md)
- [Windows](jasp-build-guide-windows.md)
