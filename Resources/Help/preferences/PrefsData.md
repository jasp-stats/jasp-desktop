
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

### Show missing values as

JASP shows missing values as blank in cells by default, you can also label them as others (e.g., it can even be defined as "😀" or other characters) in text field to display friendly on data pane. Note that this is different from a valid value label and means it won't appear in the results.

### Missing Value List

In the Missing Value list you can specify when observations in your datafile should be treated as missing (e.g., if you coded missing observations to be 999, you can add this value here and JASP will treat all cells with the value 999 as missing).
You can set here the default values that will be used for a new file. If you load a JASP file, it will load also the missing values used by this file: this values can be seen in the 'Workspace missing values' in the Data editing mode.
It is also possible to set custom missing values per column (by double-clicking the header of a column).
You can delete values from this list by selecting them and pressing the minus button.
Clicking on "Reset" will restore this list with the JASP default values.

### Windows workaround

Option here specifically for importing CSV files on Windows. JASP usually try to import `.csv` data file in UTF encoding and/or with a BOM.
If someone saves a CSV from excel in their "local codepage" however there is no BOM and there is no way to determine what the codepage is from the CSV.
Because JASP now runs exclusively in UTF-8 it can't just use the system codepage, and in any case this wouldnt help if you got the file from someone else (working in a different locale/codepage).
Without this workaround JASP just assumes it is in some kind of UTF variant, which tends to be true and also is the most interoperable way of sharing files between nations.
But to help microsoft office users a dropdown has been provided where the expected codepage can be chosen and if the workaround is turned on it will be used to decode such CSV's.
