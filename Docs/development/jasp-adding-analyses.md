Old guide
===========
This guide is terribly outdated, see https://github.com/jasp-stats/jasp-desktop/blob/stable/Docs/development/r-analyses-guide.md for up to date information.










Adding Analyses to JASP
=======================

Adding new analyses to JASP isn't super simple, for a few reasons. Mostly it boils down to; it's difficult to automatically build registries of available components (no introspection in C++), and it hasn't been a super-high priority. As such, there is some tedious boilerplate type steps you need to go through each time.

The steps are as follows:

 - Create the user interface
 - Add the user interface to `loadForm()` in mainwindow.cpp
 - Create a menu entry in the ribbon somewhere
 - Create the options JSON file
 - Create the corresponding R file

Create the user interface
-------------------------

The user interface is created in `JASP-Desktop/analysisforms/` and should follow the naming conventions there ({{analysisname}}form.cpp, .h, .ui)

User interface elements that will map onto analysis options should be promoted to Bound versions of each:

 - QCheckBox -> BoundCheckBox
 - QComboBox -> BoundComboBox
 - QGroupBox -> BoundGroupBox
 - QLineEdit -> BoundTextBox
 - QListView -> BoundListView
 - QTableView -> BoundTableView
 - QTableView -> BoundPairsTable
 - QTextEdit -> BoundTextEdit
 
These widgets should also be named to match the options they will be bound to.

Add the user interface to `loadForm()` in mainwindow.cpp
--------------------------------------------------------

`loadForm()` in `JASP-Desktop/mainwindow.cpp` contains a giant if-ladder, it should be pretty self explanatory what to do here (and also, why it would be nice if C++ supported introspection)

Create a menu entry in the ribbon somewhere
-------------------------------------------

Again, look in `JASP-Desktop/ribbons/ribbonanalysis.cpp`, should be pretty straight forward. To begin with, surround each menu addition with:

    #ifdef QT_DEBUG
        ....
    #endif

This way if you push out a release while the analysis is still being developed, users won't see it. Once the analysis is complete and ready for release, you can remove the `#ifdef`

Create the options JSON file
----------------------------

The options for each analysis are stored in `Resources/Library/`, and you should create one for the new analysis (note that this is the only place in the source tree where camelcase filenames are used).

The best way to create one of these is to look at existing ones.

Create the corresponding R file
-------------------------------

JASP analyses are implemented in R and stored in the `JASP-Engine/JASP/R` directory.


