Database
=========

JASP supports reading data directly from a database through [Qt Sql](https://doc.qt.io/qt-6/sql-driver.html#supported-databases).

Support for most databases hasn't yet been tested but as the interface is basically a passthrough to qt's sql support it is made available for people to try. If you've managed to get an undocumented database running let us know with a feature request at [our issue page](https://github.com/jasp-stats/jasp-issues/issues/new/choose).

## Configuration

The configuration is roughly the arguments going into [QSqlDatabase](https://doc.qt.io/qt-6/qsqldatabase.html#details) and can mean different things depending on which database driver is loaded. So describing them here is not very practical, instead some examples will probably work best.

The JASP specifics are basically that you first set "Name", "Hostname", etc to the desired settings. Then you connect to your database and write a query that selects the right data. This could also be a procedure-call or from a view instead of a table.

Using the preview window to verify that it is the expected data you can then load the data into JASP using the button.

This however only gets the data as it is at the moment of importing. It is also reloaded when loading a jaspfile containing database info. But to get updates while you are working in JASP you can set the "Synching interval in minutes" to something other than 0.
This will make sure JASP checks for new data at that time interval whenever the file is loaded.

## Examples

### Sqlite

Here the simplest possible example for a database JASP can connect to and it is with a tiny selfcontained sql database called [sqlite](https://www.sqlite.org/index.html).

Databases made with sqlite are contained in a single file, while allowing for access by multiple processes at the "same time".
Install it to your system and use it to make a test database as follows:
```
cd somewhere/nice
sqlite3 helloWorld
# SQLite version 3.36.0 2021-06-18 18:58:49
# Enter ".help" for usage hints.
# sqlite> 
create table helloWorld ( aNumber int, aString varchar(30) );
insert into helloWorld values ( 1, "I"), (2, "am"), (3, "alive"), (4, ";)");
# check it worked:
select * from helloWorld;
# 1|I
# 2|am
# 3|alive
# 4|;)
```
Now you have a database you can load in JASP!

Open it and navigate to "Open" and "Database", which will look something like this after following the instructions:
![Filled in example of importing from sqlite ui](filemenu/SqliteImport.png)

Select the "Sqlite" database driver in the dropdownmenu.
You can use the "Browse" button to select the sqlite-database-file you've just made.
Set the other options as they are in the image, everything empty or 0 except for "Name").
Then press "Connect to database" to see if you can do that.

If it worked you can enter a query like `select * from helloWorld` like we've used before.
The output (a few lines) will be shown below in the preview panel and if you're satisfied simply choose "Load into JASP".

That will give you the data from the database:
![JASP showing the loaded data from the helloWorld table](filemenu/SqliteImported.png)