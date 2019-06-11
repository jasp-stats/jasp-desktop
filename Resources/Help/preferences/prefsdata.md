
Data Preferences
=========

Regarding data handling in JASP you can choose the following options:
(All these settings will remain after restarting JASP.)

### Synchronize automatically on data file save

If you change a loaded data file in JASP from outside JASP
(e.g., in your preferred editor, see below), this checkbox determines
if results in JASP are automatically synchronized or not.
You can also start the synchronization manually from the main menu
or using keyboard shortcuts:

- OSX: &#8984; with Y
- Windows and Linux: Ctrl with Y

### Use default spreadsheet editor

In JASP you can open the datafile by double clicking the data pane.
This opens your data file in your preferred editor which you can specify here
or the default editor chosen by your operating system.

### Import threshold between Categorical or Scale

Importing data in JASP has a threshold value that determines if a column should be treated
as a Scale type or as a Categorical (Nominal or Ordinal) type. The default value of this parameter is 10.
This means that if you have fewer (or equal) than 10 different integers in the data, the column
gets the Ordinal type (Nominal type if only 2 different integers are found) else it will get the Scale type. Be aware that this value is used when
importing the data, so data needs to be reloaded (or synchronized) to take effect.

### Missing Value List

In this list you can specify when observations in your datafile should be treated as missing (e.g., if you coded missing observations to be 999, you can add this value here and JASP will treat all cells with the value 999 as missing).
You can delete values from this list by selecting them and pressing the minus button.
Clicking on "Reset" will restore all the default values.
