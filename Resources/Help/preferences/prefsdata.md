
Data Preferences
=========

Regarding data handling in JASP you can choose the following options.
(All these settings will remain after restarting JASP.)

### Synchronize automatically on data file save.

If you change a loaded data file in JASP from outside JASP
(e.g. in your preferred editor see below), this checkbox determines
if results in JASP are automatically synchronized or not.
You can also start the synchonization manually from the main menu
or using the keyboard shortcuts Cmd + Y (Mac) or Ctrl + Y (Windows)

### Use default spreadsheet editor.

In JASP you can open the related datafile by double clicking the data pane.
This opens your data file in your prefered editor which you can specify here
or the default editor choosen by your operating system.

### Custom threshold between Scale or Nominal.

Importing data in JASP has a threshold value that determines if a column should be treated
as a Scale type or as a Nominal type. The default value of this parameter is 10.
This means the if you have less (or equal) than 10 different integers in the data, the column
gets the Nominal type else it will get the Scale type. This threshold value can be influenced here.

### Missing Value List

In this list you can specify if a data is treated as missing value or as real data.
All the values in the 'Missing Value List' are treated as mssing values.
You can delete values from this list by selecting them and pressing the minus button.
Reset will give you all the default values back.
