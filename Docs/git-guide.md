Rebasing your repo
===========

Rebasing your repo brings code from the main repo into your own; this means bringing the code you're working on, up-to-date with your colleagues.

It is a good idea to rebase often, and you should **always** rebase before doing a commit.

Once you have done a commit, you should **not** do additional rebases until your work has been merged into the main repo.

To do a rebase, you need to bring up a terminal at the location of the project. Under OS X, this is most easily done with the Github client, by selecting `Open in Terminal` from the `Repository` menu. Under Windows, this is most easily done with the Github client, by selecting `Open in Git Shell` from the gear menu at the top right.

Once you have a terminal, you can rebase your repo with the following commands:

    git stash
    git fetch upstream
    git rebase upstream/development
    git push
    git stash pop

### What this does:

 * `git stash` : stashes your changes (hides them temporarily)
 * `git fetch upstream`: fetches the code from the main repo
 * `git rebase upstream/development`: makes your repo the same as the main one
 * `git push`: pushes the changes to your repo on github
 * `git stash pop`: recovers/returns the changes that you stashed at the beginning

This results in the latest work from the main repo, with your changes on top.

