# Working with development repository
JASP has switched to a more modular approach for building the application since 0.15 and this document describes (or ought to) how to work that.

First off, we now have the `development` branch where new features are developed.
There is also the `stable` branch where we fix bugs in the current release.

The way to work with those is in a bit of flux now (between the 0.14 and 0.15 releases) and the way to work with either might change in the near future.
Because of this I will not discuss the way to work with `stable` here now because hopefully that would become outdated quickly.

## Modular JASP
JASP used to contain all the code in one repository, everything was part of the branch you were working on.
So that included all R-code, all qml and everything else.
Worse still was the fact that all analyses were part of a single R-pkg, the `JASP` package (this has been renamed to `jaspBase`). 
This did not lend itself well to doing partial upgrades and also meant that it wasn't necessarily straightforward to merge or rebase sometimes.

To let the murky waters settle we've split off each and every jasp module to their own repository containing just the code for that particular module. We are working on making it easily possible to run those separately from JASP in R(studio), see [syntax mode](https://github.com/jasp-stats/INTERNAL-jasp/wiki/Syntax-Mode) for that. 
For an example have a look at [ANOVA](https://github.com/jasp-stats/jaspAnova) or [Frequencies](https://github.com/jasp-stats/jaspFrequencies).

Besides splitting off the modules we've also given the `jaspBase` it's own repository as we did for `jaspGraphs`.

## Submodules
But how do you use all these separate repositories then? Do you need to check these out somewhere? What is the right place for each one?

We can use [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules) for all of those tasks.
JASP now has a list of modules it needs, stored in [a hidden file `.gitmodules`](https://github.com/jasp-stats/jasp-desktop/blob/development/.gitmodules).
It tells `git` where to find the necessary modules.

### Checking out for the first time
So if you've never checked out `development` before you'll need to take the following steps in your favorite terminal:
```bash
cd jasp-desktop
git fetch --all
git checkout development && git pull
git submodule init
git submodule update --remote
```

`git submodule init` makes sure that the extra repositories are initialized correctly and only needs to be run when you didn't do it yet or a new submodule has been added to JASP since the last time you ran this command.

### Checking out regularly 
If all of the modules have been inited once then in the future you just need to run something like:
```bash
cd jasp-desktop
git fetch --all
git checkout development && git pull
git submodule update --remote
```

### Tracking branches
The recurring command `git submodule update --remote` is an important one as it makes sure that all of the code for each module is downloaded to your local system.
It also checks out the latest version of each modules' `master` branch.
This means that whenever a PR to one of the modules has been accepted the commits are added to that branch and everyone who builds their own copy of JASP then gets this updated code once they run `git submodule update --remote`.
In a nutshell it means that it'll keep all your modules up to date, which the command itself of course already seemed to imply quite strongly.

### Editing in submodules
Be careful with editing in the submodules though!
Because these are copies of the `jasp-stats` based repositories it is unlikely you have push-rights and even if you do you should go through the proper channels, aka PR + review.

To develop these modules it is better to follow the [module workflow described here](jasp-module-workflow.md).