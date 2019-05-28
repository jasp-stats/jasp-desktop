
Advanced Preferences
=========

With the advanced parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

## User interface options

### Zoom

This number specifies how the JASP interface will be scaled.
All menus and results are scaled by this factor.
You can also use keyboard shortcuts instead:

- OSX:  &#8984; with `+`, `-` or `0`
- Windows and Linux: Ctrl with `+`, `-` or `0`

`-` decreases and `+` increases the scale while `0` resets the scaling to its default value.

### Scroll speed
This speed determines, in pixels per second, what the maximum flick / scroll speed is of certain moving elements in JASP.
Should you find that scrolling in the options goes too fast, or too slow, you can change this.

### Remember Modules Enabled
If you've enabled this option then JASP will remember which modules are activated and make sure they remain that way even when you close JASP. So supposing `Summary Statistics` was enabled and JASP closes then after reopening JASP it will be enabled immediately.


## Modules options

### Developer Mode (Beta version)

This is where you specify if you want to use JASP modules or not.
A location folder can be selected where new modules reside.
Developers modules can be added directly from this folder or modules
can be added from another specified location.
The checkbox decides if package metadata should be generated every time.
Disable this option if you are transforming your R-package to a JASP Module
or simply want to keep manual changes to DESCRIPTION and NAMESPACE.")


## Logging options

### Log to File
When you check this JASP will start logging many of the actions it performs to logfiles. 
Logging is especially useful when you are developing your own module, or run into a problem and wish to reach out to the development team.
The logs might help us give insight in the nature of your problem. Note that you might need to restart JASP for the logging process to start.
The number in the input field "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
The files can be viewed by pressing "Show logs".
