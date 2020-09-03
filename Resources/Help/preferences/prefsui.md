
User Interface
=========

With the user interface parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

### Fonts
Here you can specify which font will be used by the interface (File Menu, Ribbon bar, Analysis options panel), the R, Lavaan or JAGS Code, or the results. If no choice is made, the default font is used.

### Themes
Here you can specify if you would like to have a "light" or a "dark" theme on the interface of JASP. The dark theme is much easier on the eyes if you are in a dark environment while the light (and default) theme is clearer in bright light.

### Preferred Language
Select the language in which you would like to use JASP.

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

### Safe Graphics
If this is enabled JASP will run in software rendering mode, which means your interface is slower but weird glitches or other problems might disappear. You will need to restart JASP for this option to take effect though!

### Use Native File Dialogs
Some users on certain systems (Windows) can run into trouble with the default native (or system) file dialogs.
When they try to open or save a file in JASP by clicking "Browse" in one of the file menus JASP crashes.
If you disable this option we use Qt file dialogs and they might not crash. Most users will not need this option disabled though.
