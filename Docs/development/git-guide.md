Git Guide
=========

Git ([website](https://git-scm.com/), [Wiki](https://en.wikipedia.org/wiki/Git)) is a version control system that the JASP Team relies upon to manage different versions of JASP throughout its development. This document gives tips for working with `git` and GitHub, specifically in the context of the JASP module development workflow. A summary of the most important commands under the [Summary section](#summary)

For those familiar with Git, please read our core principles found in the [Principles section at the bottom](#principles)

## Prerequisites
This guide assumes you have `git` installed [(instructions)](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) and that you have an account on `GitHub` [(make an account)](https://github.com/join). We recommend that you activate the two-factor authentication [(2FA instructions here)](https://docs.github.com/en/authentication/securing-your-account-with-two-factor-authentication-2fa/configuring-two-factor-authentication) procedure to secure your `GitHub` account.

For Windows users: The standard recommendation for using `git` is by installing [Git BASH](https://gitforwindows.org/).

### Git
- GitHub documentation [https://docs.github.com/](https://docs.github.com/)
- Useful video tutorials ([example](https://youtu.be/8Dd7KRpKeaE))

We strongly recommend that you get to `git` itself instead of relying on clients such as GiHub Desktop, GitKraken, etc. These clients do provide a useful visual overview of git, but one should be careful to "push" and "pull" with these clients. If you follow this guide, it is less likely that you will get into trouble than if you use a client. Furthermore, should something go wrong anyways, it is then easier for us to help you fix it. As with any programming gimmick, read carefully what `git` tells you, and "google up" messages you do not understand. In general, internet is your friend and the first search results typically already gives you a good answer about `git`. A useful website is the official GitHub documentation [https://docs.github.com/](https://docs.github.com/), and there also exist useful video tutorials ([example](https://youtu.be/8Dd7KRpKeaE)). Most of the information presented in this guide can be already found in the `git` and GitHub documentation as well, but this guide is describing the workflow in context of JASP development to be more explicit.

### The terminal
- [Various cheatsheets](https://www.codecademy.com/resources/cheatsheets/language/bash) designed to help with the basic terminal and git commands.

Because we do not recommend any `git` client, the following guide assumes basic understanding of a terminal [(tutorial here)](https://code.tutsplus.com/tutorials/command-line-basics-and-useful-tricks-with-the-terminal--cms-29356), but one does not need to be very experienced with it. Search and click on the `Terminal`/`Console` application in MacOS/Linux, or one of the terminal applications in Windows (e.g., `PowerShell`, `Command Prompt`, `Git BASH`) to get started. The only really important thing to know about the terminal is that one can type a command inside of the terminal and execute it with pressing `Enter` to tell the terminal to do something.

In this guide, the only really important activity with the terminal is to be able to navigate between different folders on your computer, and running `git`. Whenever this guide says something like "navigate your terminal to a folder `jaspRegression`", this means to change the working directory of the terminal such that we are inside the folder called `jaspRegression`. The following commands will help that task done:

Note for Windows users: Paths mentioned below are specified using a backslash `\` on Windows instead of a forward slash `/`, and instead of using `~/` for the user root folder, absolute paths are specified by starting from the drive name, e.g., `C:\JASP\Development\Modules/jaspRegression`.

- `pwd`: This command prints the current working directory of the terminal
- `ls`: This command prints the contents of the current working directory (i.e., prints the files and folders)
- `tree`: This command prints the contents of the current working directory in a structural manner, also showing the contents of folders inside the working folder, etc. For example, executing `tree -L 2` shows the contents of the working directory recursively up to two levels, e.g., shows contents of the folders inside of the working directory.
- `mkdir`: This command coupled with a name creates "makes" a new "directory" (i.e., folder). For example, `mkdir myNewFolder` creates a folder called `myNewFolder` inside the current working directory.
- `cd`: This command "changes" the "directory" (i.e., folder) of the terminal, based on the address that follows the `cd` keyword. For example:
  - `cd jaspRegression`: Moves the terminal inside of the `jaspRegression` folder (as long as it exist within the current working directory). This is an example of a *relative* path: Writing just the name of the path you want to go to assumes that the starting point is wherever the current working directory is.
  - `cd ~/Desktop/JASP/Development/Modules/jaspRegression` or (on Windows `cd C:\JASP\Development\Modules\jaspRegression`(): Moves the terminal into the folder specified on that address. This is an example of an *absolute* path: Writing `~` in front of the path you want to go assumes that the starting point is the user root directory. Executing `cd` without anything moves the terminal to that user root directory.
  - `cd ../../`: Two dots `..` tell `cd` to move one level up in the folder structure. For example, if we execute `cd ~/Desktop/JASP/Development/Modules/jaspRegression` and then `cd ../../`, we would end up in the `~/Dektop/JASP/Development/` folder.

To run `git` in the terminal, we execute `git` commands - these start with typing `git` and follow some other keywords or arguments that specify what exactly we want `git` to do. For example, executing `git status` shows the status of the current `git` repository.

### Customising (the text editor used for Git)
The terminal, as well as git, can be customised to make it nicer for you to work with it. For example, see a customisation framework for the popular `zsh` shell, [Oh My Zsh](https://ohmyz.sh/).

At some points, `git` may require a text editor to complete some command. In those cases, it automatically opens the terminal text editor that is configured for the terminal. For many people, this will be `Vim` ([website](https://www.vim.org/), [Wiki](https://en.wikipedia.org/wiki/Vim_(text_editor))). Some people prefer other editors, for example `Nano` ([website](https://www.nano-editor.org/), [Wiki](https://en.wikipedia.org/wiki/GNU_nano)). To set `Nano` as the default text editor, you need to open the terminal configuration file (`.bashrc` or `.zshrc`) and add a line `EDITOR=nano`. In Windows, text editor settings should be available during installation of `Git BASH`.


## JASP module development workflow [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/github-flow)
In a nutshell: Each contributor keeps their own fork of the `jasp-stats/jaspRegression`, maintain their own fork of the repository (i.e., keep it up to date with `jasp-stats/jaspRegression` and adding new features), and through "pull requests" propose changes to the `jasp-stats/jaspRegression` repository.
- `jasp-stats` [the JASP Statistics Project](https://github.com/jasp-stats)

The following guide will use the "Regression" module as a concrete example. Keyword `userName` will be used as a placeholder for your user name on GitHub.

Each JASP module is its own GitHub repository hosted on [the JASP Statistics Project](https://github.com/jasp-stats) (henceforth referred to as `jasp-stats`). For example, the "Regression" module lives as a main repository at `jasp-stats/jaspRegression`: [https://github.com/jasp-stats/jaspRegression](https://github.com/jasp-stats/jaspRegression). Everyone can contribute to any JASP module by changing the code in the repository associated with the module. However, in order to keep the `jasp-stats/jaspRegression` repository clean, you are not permitted to make changes to the `jasp-stats/jaspRegression` repository directly.

Instead, each contributor is required to keep their own "fork" (i.e., a copy) of the module repository, hosted on their own GitHub account, which they can modify as much as they like. Once a contributor is happy with the changes and wants them to appear in JASP itself, they make a "Pull Request" (henceforth: PR) to `jasp-stats/jaspRegression`, which is like saying: "Hi, here are my proposed changes to the module on my "cloud" fork `userName/jaspRegression`, do you want to "pull" (i.e., add) them to the `jasp-stats/jaspRegression` repository?". Someone from the JASP Team will review the changes, and if they approve the PR, the code is then "merged", meaning that the changes were added to the `jasp-stats/jaspRegression` repository. This will then update the module with the proposed code.

Note: Arguably the most important repository is `jasp-stats`, but you probably will not work with very often (if you are an R contributor or translator). Translators might work with the `jasp-desktop` repository at [https://github.com/jasp-stats/jasp-desktop](https://github.com/jasp-stats/jasp-desktop). This repository holds the core code that is required to build the JASP application. Working with this repository requires additional steps, which are described in section [Git submodules: `jasp-stats/jasp-desktop`](#git-submodules-jasp-statsjasp-desktop).

## Setting up your repository
These steps need to be done only once per module. When everything is set up, you are ready to contribute to JASP!

### Fork the `jasp-stats/jaspRegression` repository [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo)
- Basically, make a "cloud" copy of a module. Example forking `jasp-stats/jaspRegression` creates your personal module on GitHub `userName/jaspRegression`.

Navigate to the repository containing the module you want to fork (copy), e.g., [https://github.com/jasp-stats/jaspRegression](https://github.com/jasp-stats/jaspRegression). On the top-right, click on `Fork` button and confirm. A fork of the module was created under your GitHub account, and you can access it at `https://github.com/userName/jaspRegression`. Congratulations, you have your own copy of the module!

### Clone your repository [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository)
- Cloning makes a copy of your personal ("cloud") module available on your ("local") machine.

To be able to work on the code on your computer, you need to "clone" the code from your repository. This copies the code from your "cloud" to your local machine. Navigate your browser to your module repository (e.g., `https://github.com/userName/jaspRegression`), click on `Code` button, and copy the address under the "Clone" heading. The address would look something like `https://github.com/userName/jaspRegression.git`. Open the terminal (or git-bash on Windows) on your computer, navigate to the folder where you want to have the repository on your computer (using change directory `cd`, [https://en.wikipedia.org/wiki/Cd_(command)](https://en.wikipedia.org/wiki/Cd_(command))). Thus, on Windows `C:\JASP\Development\Modules` or on Mac/linux  `cd ~/Desktop/JASP/Development/Modules/`, and then type

```
git clone https://github.com/userName/jaspRegression.git
```

Now, the folder in which you called this command will contain another folder, `jaspRegression/`, which contains the local copy of the repository. Execute `cd jaspRegression` in the terminal to jump inside the repository.

### Connect your local clone to `jasp-stats/jaspRegression` [(GitHub documentation)](https://docs.github.com/en/get-started/quickstart/fork-a-repo#configuring-git-to-sync-your-fork-with-the-original-repository)
In order to be able to keep your repository in sync with the one under `jasp-stats/jaspRegression`, you need to specify the `jasp-stats/jaspRegression` repository as a new "remote", essentially making it possible to fetch updates from `jasp-stats/jaspRegression` into your local clone whenever you ask for it.

Navigate your browser to the `jasp-stats` repo, and copy the address as in the previous point; the address would look something like `https://github.com/jasp-stats/jaspRegression.git`.

Open the terminal (or git-bash on Windows), navigate to the local clone on your computer, and type

```
git remote add upstream https://github.com/jasp-stats/jaspRegression.git
```

to confirm that the repository has been added, type `git remote -v`. Your repository should show up in the terminal as `origin` [Your own "cloud" copy], the `jasp-stats` as `upstream`, like this:

```
origin    https://github.com/userName/jaspRegression.git (fetch)
origin    https://github.com/userName/jaspRegression.git (push)
upstream  https://github.com/jasp-stats/jaspRegression.git (fetch)
upstream  https://github.com/jasp-stats/jaspRegression.git (push)
```

f that is the case, great! Your local clone of the repository stored on your computer is now able to communicate both with your personal fork of the `jaspRegression` module under your GitHub account, as well as the one stored under the `jasp-stats` project. The name "upstream" is an abbreviation of the main project `jasp-stats/jaspRegression` and "origin" is the abbreviation of your person fork `userName/jaspRegression`. I

## Rebasing your repository [(GitHub documentation)](https://docs.github.com/en/get-started/using-git/about-git-rebase) {#rebasing}
- Sometimes multiple people are working on a module and rebasing keeps you up to date.
- For instance, you made a fork `userName/jaspRegression` and cloned this to your (local) machine on the 1st of January. `anotherAuthor` made some big improvements, which are merged into `jasp-stats/jaspRegression` on the 2nd of February. To get `anotherAuthor`'s code on your (local) machine you need to rebase and then "push" what is on your local machine to you fork `userName/jaspRegression` ("cloud").

To get updates from `jasp-stats/jaspRegression` into your local repository and your GitHub repository, you will need to do a rebase. It is good idea to rebase often, and you should **always** rebase before making a PR.

To do a rebase, navigate your terminal to the location of your local clone. Then, type in

1. `git stash` - optional: this command stashes (temporarily hides) changes that you made on your local machine, but did not "commit" to your personal fork ("cloud"). Generally, you do not need to do this if you are sure you did not modify any files in the repository without committing the changes.
2. `git fetch upstream` - this "fetches" information on what has changed to the `jasp-stats/jaspRegression` repository (if you set up your remote correctly, see above). This only sniffs out the difference between the code on your local machine and that of `jasp-stats/jaspRegression`. The new code is **not** yet available on your (local) machine.
3. `git rebase upstream/master` - this command updates your local repository: the history of the repository on your computer becomes up-to-date with the repository on `jasp-stats/jaspRegression` and its `master` branch.
4. `git push` - this command pushes the update to your remote repository on GitHub ("cloud"): now your local clone, as well as your GitHub repository, are up to date with `jasp-stats/jaspRegression`.
5. `git stash pop` - optional: recovers the changes that you stashed before (**do not** call this command if you did not call `git stash` in the first place, or if `git stash` returned "No local changes to save")

### Merge conflicts during rebase? [(GitHub documentation)](https://docs.github.com/en/get-started/using-git/resolving-merge-conflicts-after-a-git-rebase)
- Git confused on which code to follow. A merge conflict can occur when multiple people work on the same code. Git is then confused about which update to follow.

In case that rebasing fails with git telling you it could not apply some commits, it means that you made some commits to your current project that are not compatible with the updates on `jasp-stats/jaspRegression` (not compatible as in both versions attempt to make changes on the same lines of code). The conflict can range from trivial to complex, and so can the resolution. Follow the instructions for resolving merge conflicts [(GitHub documentation)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/addressing-merge-conflicts/resolving-a-merge-conflict-using-the-command-line), or seek an advice from other JASP contributor.

Remember that the best solution of merge conflicts is to avoid them at all, and that involves rebasing your repository often to make sure you are always up to date. Even if you eventually hit some conflict, rebasing often can reduce the chance things get out of hand.

## Contributing to a module

### Implement your changes

#### Make a feature branch [(GitHub documentation)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches)
- Feature branches retrieve new code that solve a specific problem (e.g., adding a new analysis or solving a bug)

The `master` branch on the `jasp-stats/jaspRegression` repository is the default branch. Similarly, your the default branch on your own repository `userName/jaspRegression` is also the `master` branch. Your `master` branch should be kept up to date with the master branch of `jasp-stats/jaspRegression`, and you should not work on the master branch. Instead, when you work on a new feature or a bug fix, make a new branch. Here referred to as `myFeatureBranch`. Before making a new feature branch, it is usually good to go to the `master` branch (`git checkout master`), and rebase it so it's up-to-date with `jasp-stats/jaspRegression` [see the previous section on rebasing](#rebasing).

Once your master branch is up-to-date, execute the following to make a new feature branch:

```
git checkout -b myFeatureBranch
```

Here, `myFeatureBranch` is a placeholder for your new branch name. Usually, it makes sense to name the branch so that you can remember the purpose of the branch. For example, if you are working on adding "scarf plots" to some analysis, naming the branch something like `scarfPlots` is a good idea. Other examples are `postHocTestInLinearRegression`, or `fixConfidenceIntervalCorrelation`. Executing `git branch` should return a list of the branches that are currently in your local repository. Executing `git checkout [branchname]` allows you to jump between different branches. You can also check which branch you are currently on, and what kind of state this branch is currently in, by executing

```
git status
```

Make sure to switch to the new feature branch before making any changes. If you're not yet in your feature branch type:

```
git checkout myFeatureBranch
```

To publish the branch so it's also available on GitHub repository execute:

```
git push --set-upstream origin myFeatureBranch
```

#### Commit your changes
- Committing your changes means that you're happy with the code on your "local" machine and that you would like to commit it to your fork "cloud".
- A client shows provides a visual aid on this. For instance, GitHub desktop client shows the changed files nicely and this helps you "committing" changes to your fork. Note, however, that you should be careful with pushing and pulling with such a client; use terminal code `git ...` instead.

When implementing a feature, regularly commit your progress as you code. To commit changes, roughly the following workflow applies.

Check the status of your branch (what files were changed, created, deleted, etc.):

```
git status
```

If you want to see specific changes to the *code* in the files that were modified, execute `git diff [filename]` to open up an overview of what lines of code were deleted and what added. When you are done inspecting the changes, press the `Q` key on your keyboard to return to your terminal.

Add files you want to commit:

```
git add fileToCommit1 someFolder/fileToCommit2
```

And commit your changes:

```
git commit -m "some informative message"
```

Be descriptive in your commits, and commit often (usually when you implemented some logical sub-unit of your feature/bug fix). The flag `-m` allows you to write a brief commit message. If you would like to be more verbose, you may run the command just as is (`git commit`), which opens up the configured text editor. Once you save the commit message, the commit is made. If you close the text editor without saving the commit message, the commit will not be done.

Do not forget to rebase your repository often.

#### Push your changes
- A "push" uploads the code from your "local" machine to your fork ("cloud")

Do not forget to bring your GitHub repository up to date with your local repository by pushing your commits to GitHub:

```
git push
```

Remember, GitHub serves you as a back up. If you commit your changes but do not push them to GitHub, your work may be lost if your laptop gets stolen, or your house burns down and your computer with all your hard drive back ups with it! Note also that by working on a feature branch you cannot break your personal copy of the module (on your own master master). Hence, if you mess up (most likely not), then in the worst case scenario you can always go back to your own master branch. As you only push to `userName/jaspRegression`, the code in JASP itself `jasp-stats/jaspRegression` remains protected.


#### Squashing commits (optional)
Squashing commits is usually not necessary, but can come in handy in certain situations. For example, if you work on some feature branch and make multiple commits and at some point get merge conflicts when trying to rebase. In this situation, you need to resolve the conflicts, which is done one commit at a time. So if you made a lot of commits that each had some merge conflicts with `jasp-stats/jaspRegression`, you may end up doing a lot of work fixing the conflicts for each commit. Instead, you may squash your commits, which is basically combining multiple commits into one, and then resolve your conflicts only once.

There are multiple ways how to squash your commits. One way is to simply undo your last commits and committing them again as one commit. An example:

```
git reset --soft HEAD~9
git commit -m "a new commit message outlining the changes in the previous 9 commits"
```

The first command `reset` is used to "undo" your commits. The option `--soft` makes sure that while you undo the commits, you *keep* the changes. All your edits are still preserved, just not committed anymore. The last argument `HEAD~9` is about how many commits you want to undo. `HEAD` points to the current status of the repository, and `HEAD~9` means "the most recent nine commits".

While this way to squash commits is easy, it has its downside. The original commit messages of the commits that you undo are undone as well, and forever lost. In case you want to combine the individual messages into one message as well, you will need to use methods like `git rebase -i <last-commit-to-keep>`, or `git merge --squash`. These methods are more advanced, so make sure you understand what you are doing. For example by studying the responses in the following thread: [https://stackoverflow.com/questions/5189560/squash-my-last-x-commits-together-using-git](https://stackoverflow.com/questions/5189560/squash-my-last-x-commits-together-using-git).

If you already pushed your commits to your GitHub repository, and then squash your commits, your local repository has diverged from the GitHub repository. To push your squashed commits, you will need to `git push -f` or `git push -force` to override the GitHub repository with the new version on your computer.


### Pull requests [(GitHub documentation)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork)
- A pull request is a request of `jasp-stats/jaspRegression` ("cloud") to "pull" the changes you made in the `myFeatureBranch` of `userName/jaspRegression` (your personal "cloud").

Pull request (PR) is a way to review and merge your changes into the `jasp-stats/jaspRegression` repository in a controlled and structured way, so that we can be always be sure that the code on `jasp-stats/jaspRegression` is working.

#### Prefer smaller PRs
Usually, a PR should implement a logically coherent feature or a bug fix, and should not bring together many unrelated changes. For example, if your task is to fix two unrelated bugs in the same module, it makes sense to split the two fixes in two different PRs. If the bugs are related (for example, you cannot fix one without fixing the other), then you make one PR for both. There are exceptions, of course, such as when the changes are so trivial and small that it is easy to keep track of everything at once, or if your PR introduces refactoring or adding a large part of the code, which happens to also fix some bugs and add multiple more features. But in general, making rather smaller PRs helps to get the PR reviewed and merged quicker and is less error prone. Large PRs tend to go stale, get nasty merge conflicts, and are difficult to review and test properly.

#### Create a pull request
After committing your changes and pushing them to your own repository ("cloud"), you can make a PR. Navigate your browser to your repository (`https://github.com/userName/jaspRegression`) and click `Pull request` button.

1. Verify that:
  - base repository is set to: `jasp-stats/jaspRegression`, base: `master`
  - head repository is set to: `usename/jaspRegression`, compare: `myFeatureBranch`
2. Do not forget to provide a meaningful title of the PR, and a description giving additional information.
3. Use keywords such as `fixes` to link existing issues to the PR [(GitHub documentation)](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue). For instance, `fixes: https://github.com/jasp-stats/jasp-issues/issues/242`

#### Assign reviewers
A PR needs to be reviewed and approved by a JASP Team member to be merged into `jasp-stats/jaspRegression`. To signal that you consider your PR ready for review, assign reviewers by selecting team members in the list under heading `Reviewers`. To select the appropriate reviewer, follow these rules:

1. Assign a maintainer of the module/analysis that you are proposing to change with your PR. See below for table of current maintainers.
2. If you are a trainee/student and have a mentor from the JASP Team, assign your mentor.
3. If your PR fixes an issue or a bug posted in `jasp-stats/jasp-test-release`, or `jasp-stats/INTERNAL-jasp`, you can assign the author of that issue for a review. Testers who found a bug know how to reproduce the bug, and so they are suitable to check whether the bug has been fixed.
4. If you are not sure, or you are a maintainer of the module/analysis that you want to change with your PR, assign someone from [senior collaborators](https://github.com/orgs/jasp-stats/teams) or from [the core team](https://github.com/orgs/jasp-stats/teams/core).

<details>
	<summary>Table of module maintainers</summary>

| Module      | Analysis    | Maintainer |
| ----------- | ----------- | ---------- |
|jaspAnova|Classical analyses|Johnny|
|jaspAnova|Bayesian analyses|Don|
|jaspAudit|All|Koen|
|jaspBain|All|Koen|
|jaspBsts|All|Don|
|jaspCircular|All|Simon|
|jaspCochrane|All|Frantisek|
|jaspDescriptives|All|No one/any JASP Team member|
|jaspDistributions|All|Simon|
|jaspEquivalenceTests|All|Jill|
|jaspFactor|All|Don & Simon & Julius & Lorenzo|
|jaspFrequencies|All|Tim & Frantisek & Simon|
|jaspJags|All|Don & Jiashun|
|jaspLearnBayes|Binary classification|Simon|
|jaspLearnBayes|Counts|Frantisek|
|jaspLearnBayes|The problem of points|Jiashun|
|jaspLearnBayes|Buffon's needle|Jiashun|
|jaspMachineLearning|All|Koen & Don|
|jaspMetaAnalysis|Bayesian and classical meta-analysis|Sophie|
|jaspMetaAnalysis|All|Frantisek|
|jaspMixedModels|All|Frantisek|
|jaspNetwork|All|Don|
|jaspRegression|Correlation|Simon|
|jaspRegression|Classical linear and classical logistic regression|Qixiang & Simon & Don|
|jaspRegression|Classical GLMs|Qixiang & Simon|
|jaspRegression|Bayesian correlation|Alexander & Don & Simon|
|jaspRegression|Bayesian linear regression|Don|
|jaspReliability|All|Julius|
|jaspSem|All|Simon & Julius & Lorenzo|
|jaspSummaryStatistics|All|EJ & Akash|
|jaspTTests|All|Don|
|jaspVisualModeling|All|Dustin|
|jaspProphet|All|Malte & Simon|
|jaspProcessControl|All|SKF Team|

</details>

#### GitHub Actions
- GitHub Actions automatically tests your code for errors and consistency.

After creating a PR, GitHub actions will be executed, which check the code in your PR. Most notably, the unit tests are run on instances of Windows, Mac, and Linux machine, so that we check that the PR will not break something if it is being merged.

If any of the tests fail, the PR cannot be merged. You as an author of the PR are responsible to fix the issues with unit tests. A good idea is to run the tests on your computer (using `jaspTools::testAll()`) to check that the tests are not broken (and fix any problems) even before making a new PR. Successful checks are shown in the PR as this:

![Screenshot of successful tests](/Docs/development/img/git-guide/successful-checks.png)

If any of the tests fail, they will be shown with a red cross. Click on `details` to view the log of the test to see what went wrong. If you are unsure what you are seeing, or do not know how to fix the problem, ask for guidance someone from the JASP Team, or comment in the PR on GitHub.

#### Respond to reviews
- To include new code to your PR, just commit new code again to your feature branch

Reviewers may require some changes to the PR. Resolve the suggestions, commit the changes, and push to your GitHub repository.

In case you do not receive a review within a couple of days, ask the assigned reviewer whether they are aware that they were assigned or whether they have time to review. In case they decline, assign someone else. Assigned reviewers are responsible to review your work or notify you if they cannot do so, but you are responsible for the PR as a whole.

If you think the PR is urgent, write to the collective communication channels (e.g., Slack), or get in touch with a senior JASP Team member.

#### Delete your feature branch
- Don't continue working on your feature branch once it's merged.

After your PR was reviewed, approved and merged, you may delete the feature branch that you created.

To delete the branch locally on your computer, execute:

```
git branch -d myFeatureBranch
```

To delete the branch on your GitHub repository, execute:

```
git push origin --delete myFeatureBranch
```

Do not delete it if you for whatever reason want to keep the branch! Before making a new feature branch, please rebase your master branch.

#### Reviewing PRs [(GitHub documentation)](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/about-pull-request-reviews)
In case you have been assigned for a review, you may need to check out the code that is implemented in the PR, and try it out on your computer. To do this, you can add the author's GitHub repository as additional remote, and temporarily switch to their feature branch for testing. Assuming that the author's GitHub handle is `anotherAuthor`, execute

```
git remote add anotherAuthor https://github.com/anotherAuthor/jaspRegression.git
```

which adds `anotherAuthor` to your list of remotes in the jaspRegression repository (verify with `git remote -v`). Fetch their code:

```
git fetch anotherAuthor
```

and temporarily checkout their feature branch:

```
git checkout anotherAuthor/theirFeatureBranch
```

When you are done with the testing, switch to one of your branches as normally (e.g., `git checkout master`).

In case you have been assigned for a review, but do not have time or knowledge to do so, notify the PR's author, one of the senior JASP Team members, or simply write a comment in the PR. You may suggest other contributors you think are more suitable for a review.


## Summary of git commands {#summary}
Let's recap the git commands that were used in this guide, in a succession, during one cycle of a development workflow.

### Set up the repository
Clone your repo and add the `jasp-stats/jaspRegression` remote, check that remotes are set up correctly:

```
git clone https://github.com/userName/jaspRegression.git
git remote add upstream https://github.com/jasp-stats/jaspRegression.git
git remote -v
```

This needs to be done once per your local repository.

### Rebase

```
git stash
git fetch upstream
git rebase upstream/master
git push
git stash pop
```

Do this frequently.

### Make changes

Make a feature branch on your computer and on your GitHub repository. This needs to be done once for each feature branch:

```
git checkout -b myFeatureBranch
git push --set-upstream origin myFeatureBranch
```

Make your changes and commit them. Typical example of succession of commands during this phase would be something alike

```
git status
git add file1 folder1/ folder2/file2 ...
git commit -m "my commit message"
git push
```

Rebase again before making a PR.

You may need to make more commits and push them during the PR review.

### After PR

After the PR was approved and merged to `jasp-stats/jaspRegression`, delete the feature branch from your computer and from your GitHub repository:

```
git delete -b myFeatureBranch
git push origin --delete myFeatureBranch
```

This needs to be done once for each feature branch. Do not delete it if you want to keep the branch!

## Git submodules: `jasp-stats/jasp-desktop`
As mentioned previously, JASP modules are separate repositories on GitHub. There is one repository, `jasp-stats/jasp-desktop` ([https://github.com/jasp-stats/jasp-desktop](https://github.com/jasp-stats/jasp-desktop)), that has all the code required to build the JASP application. Usually, you do not need to do anything with this repository, and you should work on the separate "modules" repositories. You need `jasp-desktop` only if you want to build JASP ([Guide to Building JASP](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-building-guide.md)) or if you want to make changes to the core JASP code and not to the code associated with a specific analysis.

All other repositories are included in `jasp-desktop` as "git submodules". This makes `jasp-desktop` a little bit special because it requires some more steps to keep the submodules up to date. You can rebase as normally, but that will not rebase the individual submodules. Instead, run

```
git fetch --all
git submodule init
git submodule update --remote
```

`git fetch --all` fetches all remotes and submodules, `git submodule init` initiates all submodules in your local repo that were created, and `git submodule update --remote` updates local submodules to be up to date with the current version of the `master` branch of the submodules on `jasp-stats`. This means that whenever a PR to one of the modules has been accepted the commits are added to that branch and everyone who builds their own copy of JASP then gets this updated code once they run git submodule update --remote. In a nutshell it means that it'll keep all your modules up to date, which the command itself of course already seemed to imply quite strongly.

### Editing in submodules
Be careful with editing in the submodules though! Because these are copies of the `jasp-stats` based repositories it is unlikely you have push-rights and even if you do you should go through the proper channels, that is, PR + review.

### Important branches
All jaspModules (e.g., `jaspRegression`) on `jasp-stats` have a `master` branch, which is the default branch. This is not the case for `jasp-stats/jasp-desktop`. This repository has two important branches, `stable` and `development`. The `stable` branch, as its name suggest, is a version of `jasp-desktop` that is stable, usually very similar to the version that was in the latest release of JASP. This version should be always ready to be build in Qt without problems, and should just work. However, it may not contain the latest hot changes to the JASP application. For that version, there is the branch `development`, which contains the current version of the JASP application being developed for the next release. `development` is the default branch of `jasp-desktop`. **Be careful about which branch you use when you rebase**, i.e., `git rebase upstream/stable` or `git rebase upstream/development`.

## Summary of basic principles {#principles}
1. Do not use git clients for making PRs, unless you know what you are doing.
2. Learn to work with git comfortably.
3. Fork and clone jaspModules to make PRs, do not work inside `jasp-desktop` repositories.
4. Work in feature branches not on `master`.
5. Rebase often. **Do not** call `git pull` to sync your repository with `jasp-stats`. **Rebase**.
6. Push your changes regularly from your local repository to your GitHub repository, to back up your work in progress.
7. Prefer making more smaller PRs rather than making one PR implementing many unrelated features.
8. Make clean PRs
	- Make sure you do not have merge conflicts (rebase)
	- Test your code before making a PR
	- Document your changes by writing informative commit messages
	- Link issues to your PR
9. You are responsible for making sure your PR will be dealt with. Assign reviewers to your PRs. Communicate when your PR goes stale or if your PR needs special attention.
10. Be proactive when assigned to review: let the PR author know if you cannot review their PR.
11. You can add other people's forks as your remote to review or checkout their code.
