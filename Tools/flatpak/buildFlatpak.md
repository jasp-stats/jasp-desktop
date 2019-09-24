# JASP & Flatpak
So you want to build JASP as flatpak application?

The way I usually do this is making sure I've got the newest version of flatpak installed on my linux machine.
Then I work from a [jasp-desktop repo](git://github.com/jasp-stats/jasp-desktop.git) that is cloned to the same machine.
Furthermore you need to make sure you have the (secret) GPG keys installed on your machine. 

To get them, contact the JASP-team and when you get them use one of the answers [here](https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another) to make them available to flatpak-builder.

There is a script [Tools/make-flatpak.sh](https://github.com/jasp-stats/jasp-desktop/blob/development/Tools/make-flatpak.sh) that basically takes care of business for you.

```bash
#Assuming we are in jasp-desktop repo clone.
cd Tools
./make-flatpak.sh
```

This will take quite some time and you will be prompted to enter the passphrase for the GPG keys used to sign to repo.
It does however do  everything necessary except uploading it to static.jasp-stats.org.

Uploading it can be done quite easily though by moving to jasp-desktop/flatpak-builder-folder/jasp-repo and executing:
```bash
#Obviously you should replace %username% with the actual username!
rsync -csav * %username%@static.jasp-stats.org:static.jasp-stats.org/flatpak/jasp-repo
```
