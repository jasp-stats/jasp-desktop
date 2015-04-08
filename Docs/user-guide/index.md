
JASP User Guide
===============

Hi, and welcome to the JASP User Guide!

Installation
------------

JASP is a "normal" piece of software. You download it, install it, and it works. :)

Download: https://jasp-stats.org/download

### Windows

Two versions of JASP are available for Windows, one for 32-bit and one for 64-bit systems. We recommend that you use the version which corresponds your Windows version.

More Info: ["Is my PC running the 32-bit or 64-bit version of Windows?"](http://windows.microsoft.com/en-US/windows7/find-out-32-or-64-bit)

If you absolutely cannot figure out what version of windows you are using, the 32-bit version will work on both.

### Mac OS X

JASP works on Mac OS X, version 10.8 (Mountain Lion) and above. JASP is installed by downloading and double clicking the .dmg file, and dragging the JASP icon to the Applications folder. This places JASP in your applications folder.

OS X actively discourages people from downloading software from the internet (rather wanting to channel people through their app store), so there will likely be some additional steps to make JASP run.

Newer versions of OS X need to have their "Gatekeeper" options relaxed, before they will allow software to be used that was downloaded from the internet. More Info: [About Gatekeeper](https://support.apple.com/en-us/HT202491).

You will need to change your Gatekeeper settings to "Allow applications to downloaded from anywhere".

Having relaxed the Gatekeeper settings, the next step is to 'right click' or 'command-click' JASP in your Applications directory, and select 'Open' from the menu. This will present a window saying that JASP is from an unidentified developer; selecting "Open" will start JASP.

(gatekeeper.png)

From then on, you will be able to start JASP in the usual way.

### Linux

JASP is available for Ubuntu 14.04 and 14.10 . It is possible to run JASP on other versions of Linux, but there can be issues, and the JASP team will have difficulty assisting you. We would like to support JASP on more Linux distributions, however, this represents a substantial undertaking, and the JASP team currently does not have the resources to support a wide range of distributions. In the future, we hope that members of the Linux community will be able to assist us.

Under Ubuntu, it is necessary to first install *libblas* and *liblapack*. These are available in the Ubuntu Software Centre.

JASP can then be run by unzipping the contents of the JASP zip files, and double-clicking on the resultant JASP executable.

Data Sets
---------

JASP comes with a handful of example data sets, which can be accessed from the 'File tab'. Selecting these will load up the data, allowing you to inspect and analyse it.

JASP can also open data sets in the .csv (comma separated volume) data format. In practice, .csv files are often delimitered with a range of different characters and not just commas. When opening a .csv file, JASP examines the file, and automatically determines what the delimiters are. This usually means that JASP can open any .csv file without intervention from the user. The only absolute requirement, is that the .csv file contains a header row; where the names of each of the columns appear in the first row.

If you find a .csv file that JASP opens incorrectly, you can submit this to the JASP team, and they can look at improving the .csv handling heuristics. It would be worth checking if the .csv file is reasonable, and whether other software is able to open it correctly.

When opening a .csv file, JASP makes a "best guess" to assign variable types. More details below.

Variable Types
--------------

In JASP there are 4 variable types:

1. Nominal Text
2. Nominal
3. Ordinal
4. Continuous

*Nominal Text* variables are categorical variables with no order, and with no meaningful numeric value. An example might be a variable called *Gender*, with two levels; *Male* and *Female*.

*Nominal* variables are categorical variables with no order, however they do have meaningful numeric values. An example might be a variable called *Group* with levels *1* and *2*.

*Ordinal* variables are categorical variables with a numeric value, and an inherent order. An example might be *Time point* with levels *1*, *2*, *3*, *4*, and *5*.

*Continuous* variables are variables with values which exist on a continuum, such as *Height* or *Weight*.

Some users prefer not to have to specify the variable types (which can be arduous, particularly for data sets with many columns), and so the variable types in JASP are generally not enforced. They usually serve only as guides; you can, for example, assign a nominal variable as a dependent variable in a t-test. In this situation, the variable is treated as a continuous variable.
(It should be noted that this is the same behaviour as SPSS)

#### Variable Type Assignment

When loading a .csv file, JASP automatically assigns variable types according to the following rules:

1. If the variable contains only integer values and missing values, and contains less than 25 unique values, then it is assigned a variable type of *Nominal*.
2. If the variable contains only integer values, floating point numbers, missing values, and +/- infinities, then it is assigned a type of *Continuous*.
3. Otherwise the variable is assigned a type of *Nominal Text*.

It should be noted that a value of *NA* is considered a text value in JASP, the same as in almost all statistical environments. However this occasionally trips up *R* users.

#### Changing Variable Types

Should these automatic assignments be incorrect for your particular dataset, it is possible to override these values. If you click the icon representing the variable type at the top of the column, a menu is produced allowing you to choose a different variable type.

Values are changed to the new data type, and any incompatibilities are converted to missing values. But be careful! JASP at present does not implement an *undo*, so if you change a *Nominal Text* column full of text values to *Nominal* or *Continuous*, it will convert the entire column to missing values. At present, there is no way to *undo* this and it will be necessary to reload the data set.

Analyses
--------

Having loaded a dataset, it is now possible to run analyses. Selecting an analysis from the Ribbon along the top, shows options for that analysis in the left panel, and results in the right panel. As the options are specified, the analysis results automatically update, providing immediate feedback.

When the analysis is specified the way you like, you can return to the data view by clicking the "OK" button. This dismisses the analysis options, and unselects the current analysis.

A user wishing to return to an earlier analysis, can simply select it by clicking on it. This brings up the options that were used to generate that analysis, and allows the user to make additional changes.
