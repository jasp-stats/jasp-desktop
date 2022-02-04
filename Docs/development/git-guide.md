Git Guide
=========

This document gives tips for working with `git` an `GitHub`, specifically in the context of the JASP module development workflow. This guide assumes you have `git` installed [(intructions)](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and that you have an account on `GitHub` [(make an account)](https://github.com/join).

**N.B.** We strongly recommend getting to know using `git` in terminal instead of relying on clients such as GiHub Desktop, GitKraken, etc (unless you know what you are doing). If you follow the guide below, it is less likely that you will get into trouble than if you use a client. Further, should something go wrong anyway, it is then easier for us to help you fix it.

**Tip:** As with any programming gimmick, read carefully what `git` tells you, and "google up" messages you do not undestand. In general, internet is your friend and the first one to give you good answers about `git`. Useful website is the official GitHub documentation [https://docs.github.com/](https://docs.github.com/). Most of the information presented in this guide can be found in this documentation as well



## JASP module development workflow

Each JASP module is its own GitHub repository hosted on [the JASP Statistics Project](https://github.com/jasp-stats) (henceforth referred to as `jasp-stats`). For example, the "Regression" module lives as a main repository at `jasp-stats`: [https://github.com/jasp-stats/jaspRegression](https://github.com/jasp-stats/jaspRegression). Everyone can contribute to any JASP module by changing the code in the repository associated with the module. However, in order to keep the `jasp-stats` repository clean, not everyone is permitted to make changes directly to the `jasp-stats` repository hosted on the the JASP Statistics Project. 

Instead, each contributor is required to keep their own "fork" (i.e., a copy) of the module repository, hosted on their own GitHub account, which they can modify as much as they like. Once a contributor is happy with the changes and wants them to appear in JASP itself, they make a "Pull Request" (henceforth: PR) to `jasp-stats`, which is like saying: "Hi, here are my proposed changes to the module, do you want to add them to the `jasp-stats` repository?". Someone from the JASP Team will review the changes, and if they approve the PR, the code is then "merged", meaning that the changes were added to the `jasp-stats` repository and the history of the project was updated to hold information about the new contribution.

In a nutshell: Each contributor keeps their own fork of the `jasp-stats`, maintain their own fork of the repository (i.e., keep it up to date with `jasp-stats` and adding new features), and through PRs propose changes to the `jasp-stats` repository. 

The following guide will use the "Regression" module as a concrete example. Keyword `username` will be used as a placeholder for your username on GitHub.

## Setting up your repository

These steps need to be done only once per module. Once everything it set up, you are ready to contribute to JASP!

### Fork the `jasp-stats` repository [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo)

Navigate to the repository containing the module you want to fork (copy), e.g., [https://github.com/jasp-stats/jaspRegression](https://github.com/jasp-stats/jaspRegression). On the top-right, click on `Fork` button and confirm. A fork of the module was created under your GitHub account, and you can access it at `https://github.com/username/jaspRegression`. Congratulations, you have your own copy of the module!

### Clone your repository [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository)

To be able to work on the code on your computer, you need to clone the repository to make a local copy on your machine. Navigate your browser to your module repository (e.g., `https://github.com/username/jaspRegression`), click on `Code` button, and copy the address under the "Clone" heading. The adress would look something like `https://github.com/username/jaspRegression.git`. Open the terminal (or git-bash on Windows) on your computer, navigate to the directory where you want to have the repository on your computer (using `cd`, [https://en.wikipedia.org/wiki/Cd_(command)](https://en.wikipedia.org/wiki/Cd_(command))) and then type

```
git clone https://github.com/username/jaspRegression.git
```

Now, the directory in which you called this command will contain another directory, `jaspRegression/`, which contains the local copy of the repository. Execute `cd jaspRegression` in the terminal to jump inside the repository.

### Connect your local clone to `jasp-stats` [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo#configuring-git-to-sync-your-fork-with-the-original-repository)

In order to be able to keep your repository in sync with the one under `jasp-stats`, you need to specify the `jasp-stats` repository as a new "remote", essentially making it possible to fetch updates from `jasp-stats` into your local clone whenever you ask for it.

Navigate your browser to the `jasp-stats` repo, and copy the address as in the previous point; the address would look something like `https://github.com/jasp-stats/jaspRegression.git`.

Open the terminal (or git-bash on Windows), navigate to the local clone on your computer, and type

```
git remote add upstream https://github.com/jasp-stats/jaspRegression.git
```

to confirm that the repository has been added, type `git remote -v`. Your repository should show up in the terminal as `origin`, the `jasp-stats` as upstream, like this:

```
origin    https://github.com/username/jaspRegression.git (fetch)
origin    https://github.com/username/jaspRegression.git (push)
upstream  https://github.com/jasp-stats/jaspRegression.git (fetch)
upstream  https://github.com/jasp-stats/jaspRegression.git (push)
```

## Rebasing your repository [(GitHub documentation)](https://docs.github.com/en/get-started/using-git/about-git-rebase)

To get updates from `jasp-stats` into your local repository and your GitHub repository, you will need to do a rebase. It is good idea to rebase often, and you should **always** rebase before making a PR.

To do a rebase, navigate your terminal to the location of your local clone. Then, type in

1. `git stash` - optional: this command stashes changes that you made in the repository, but did not commit
2. `git fetch upstream` - this command gets the code from the `jasp-stats` repository (if you set up your remote correctly, see above).
3. `git rebase upstream/master` - this command updates your local repository: the history of the repository on your computer becomes up-to-date with the repository on `jasp-stats` and its `master` branch.
4. `git push` - this command pushes the update to your remote repository on GitHub: now your local clone, as well as your GitHub repository, are up to date with `jasp-stats`.
5. `git stash pop` - optional: recovers the changes that you stashed before (**do not** call this command if you did not call `git stash` in the first place, or if `git stash` returned "No local changes to save")

### Merge conflicts during rebase? [(GitHub documentation)](https://docs.github.com/en/get-started/using-git/resolving-merge-conflicts-after-a-git-rebase)

In case that rebasing fails with git telling you it could not apply some commits, it means that you made some commits to your current project that are not compatible with the updates on `jasp-stats` (not compatible as in both versions attempt to make changes in the same lines of code). The conflict can range from trivial to complex, and so can the resolution. Follow the instructions for resolving merge conflicts [(GitHub documentation)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/addressing-merge-conflicts/resolving-a-merge-conflict-using-the-command-line), or seek an advice from other JASP contributor.

## Contributing to `jasp-stats`

### Make a feature branch

### Comitting changes

### Making a pull request

#### Push your feature branch to your repository

#### Create a pull request

#### Respond to reviews

#### Delete your feature branch


## Putting it all together

