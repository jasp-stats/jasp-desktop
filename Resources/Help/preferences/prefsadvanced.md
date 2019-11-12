
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

The checkbox decides if package metadata should be generated every time.
Disable this option if you are transforming your R-package to a JASP Module or simply want to keep manual changes to DESCRIPTION and NAMESPACE.

The CRAN repository URL determines where JASP will try to download the required packages specified in a module from.
The default is `https://cloud.r-project.org` but a good alternative (when packages seemingly can't be installed for instance) is `cran.r-project.org`.


## Logging options

### Log to File
When you check this JASP will start logging many of the actions it performs to logfiles. 
Logging is especially useful when you are developing your own module, or run into a problem and wish to reach out to the development team.
The logs might help us give insight in the nature of your problem. Note that you might need to restart JASP for the logging process to start.
The number in the input field "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
The files can be viewed by pressing "Show logs".
