
Advanced Preferences
=========

With the advanced parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

## Plotting options

### Use PPI (Pixels pre inch) of screen in plots

This is where you specify the number of pixels per inch of the screen
to render your plots.

### Image Background Colour

The background colour of images in JASP results can be influenced with this option.
This is especially important when copying and/or pasting plots.


## User interface options

### Scaling

This number specifies how the JASP interface will be scaled.
All menus and results are scaled by this factor.
You can also use the keyboard shortcuts &#8984; with `+`, `-` or `0` (on MacOS) or Ctrl with `+`, `-` or `0` (on Windows & Linux) for
this purpose. `-` decreases and `+` increases the scale while `0` resets the scaling for you.

### Max Flick Velocity
This speed determines, in pixels per second, what the maximum flick / scroll speed is of certain moving elements in JASP.
Should you find that scrolling in the options goes to fast, or too slow, for your taste you can change this.


## Modules options

### Developer Mode (Beta version)

This is where you specify if you want to use JASP modules or not.
A location folder can be selected where new modules reside.
Developers modules can be added directly from this folder or modules
can be added from another specified location.
The checkbox decides if package metadata should be generated every time.
Disable this option if you are transforming your R-package to a JASP Module
or simply want to keep manual changes to DESCRIPTION and NAMESPACE.")

### Logging
You can check the box  "Log to file" and then JASP will start logging to files. You might need to restart JASP to have it fully applied.
The number in the spinbox "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
These files can be viewed by pressing "Show logs".
