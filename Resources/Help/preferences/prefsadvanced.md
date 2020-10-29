
Advanced Preferences
=========

With the advanced parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

## Modules options

### Remember Enabled Modules
If you've enabled this option then JASP will remember which modules are activated and make sure they remain that way even when you close JASP. So supposing `Summary Statistics` was enabled and JASP closes then after reopening JASP it will be enabled immediately.

### Developer Mode (Beta version)

This is where you specify if you want to use JASP modules or not.
A location folder can be selected where new modules reside.
Developers modules can be added directly from this folder or modules can be added from another specified location.

The CRAN repository URL determines where JASP will try to download the required packages specified in a module from.
The default is `https://cloud.r-project.org` but a good alternative (when packages seemingly can't be installed for instance) is `cran.r-project.org`.

## Windows Workarounds

### LC_CTYPE 
LC_CTYPE is a setting for the so-called "locale" and it determines, on windows, how characters are encoded. 
This is used internally by R and sadly enough it cannot handle international characters very well on Windows.
Setting this to "C" makes sure that all the output in the results looks good and can support all characters (unicode).

The problem is when you are running in a different locale, such as Korean, and you have a username that contains special characters from that locale.
In that scenario things might break if you install JASP per-user or if you try to install a dynamic module.
To work around that problem we will then not set LC_CTYPE to "C" but keep it as is, then JASP works.
The drawback is that some characters in the output will look bad.
Installing JASP in a different place (per-machine) should fix that.

The default setting "Let JASP guess the best setting for LC_CTYPE" handles this for you, our advice is to leave it enabled.

We apologize for this small inconvenience, we are working on it and hopefully have this fixed next release.


## Logging options

### Log to File
When you check this JASP will start logging many of the actions it performs to logfiles. 
Logging is especially useful when you are developing your own module, or run into a problem and wish to reach out to the development team.
The logs might help us give insight in the nature of your problem. Note that you might need to restart JASP for the logging process to start.
The number in the input field "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
The files can be viewed by pressing "Show logs".
