
Results Preferences
=========

Regarding analysis results in JASP you can choose the following options:
(All these settings will remain after restarting JASP.)

## Table options

### Display exact p-values

Here you can specify if p-values should be shown as "< .001" or if an exact value should be given.

### Use exponent notation

Here you can specify if scientific numbers should be displayed with exponent notation as in 1e-10, instead of normalized like 1×10<sup>-10</sup>.

### Fix the number of decimals

This represents the number of decimals JASP will show for numeric values.


## Plot options

### Use PPI (Pixels Per Inch) of screen in plots

Here you can specify the number of pixels per inch your plots should have. By default it uses the value derived from your screen. This value can be increased to create higher quality images.

### Image background colour

This option let's you toggle the background colour of plots between white and transparant. You may find this especially useful when copying and/or pasting plots.

## Miscellaneous options

### Show R syntax

Show R syntax required to call each analysis in R.

### Store analysis state and plot in jasp-files

When an analysis runs it creates temporary files that contain the state of the analysis and the same for each plot. 
The analysis uses this to avoid rerunning the same code over and over again by reusing old results whenever it can.
Enabling this option will allow you to store this in your jaspfile, so that the next time you load it with this version of jasp you can rerun much quicker. 
However, your filesize might increase, sometimes by a lot.

You might want to enable this option if you are working with analyses that can take a long time for certain steps, like bootstrapping with a large N.
In that case storing it might save you a lot of time if you've closed JASP in the meantime.
We've turned it off by default because in most cases the time it takes to rerun it once is not so bad and it makes all jaspfiles smaller.
And when loading a jaspfile made by an older version this data is discarded anyway.
