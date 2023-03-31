Database
=========

JASP supports reading data directly from a database through [Qt Sql](https://doc.qt.io/qt-6/sql-driver.html#supported-databases).

Support for most databases hasn't yet been tested but as the interface is basically a passthrough to qt's sql support it is made available for people to try. If you've managed to get an undocumented database running let us know with a feature request at [our issue page](https://github.com/jasp-stats/jasp-issues/issues/new/choose).

## Configuration

The configuration is roughly the arguments going into [QSqlDatabase](https://doc.qt.io/qt-6/qsqldatabase.html#details) and can mean different things depending on which database driver is loaded. So describing them here is not very practical, instead some examples will probably work best.

The JASP specifics are basically that you first set "Name", "Hostname", etc to the desired settings. Then you connect to your database and write a query that selects the right data. This could also be a procedure-call or from a view instead of a table.

As far as the username and password go, you can check "Remember Me" and it will then keep the password stored in the jasp-file (obfuscated a bit but reversible with enough effort). This can be used to have easy connection to the database, for instance when you want to run JASP unattended for reporting or something. If you *do* store the password in the file it is highly recommended to make a specific user for your database that only has read-access to the necessary tables/views.
If you do not check "Remember Me" whoever loads the database will be prompted for the password when they load the jasp-file, but only if the database connection is set to synchronize at a certain interval.

Using the preview window to verify that it is the expected data you can then load the data into JASP using the button.

This however only gets the data as it is at the moment of importing. It is also reloaded when loading a jaspfile containing database info. But to get updates while you are working in JASP you can set the "Synching interval in minutes" to something other than 0.
This will make sure JASP checks for new data at that time interval whenever the file is loaded.

## Examples

Below examples for [Sqlite](#sqlite), [SQL server](#sql-server) and [Snowflake Warehouse](#snowflake-warehouse).

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

### SQL Server

Here an example will be shown on how to read data from SQL Server, where the assumption is made you already have that up and running.
In this case the database is called `TestDB` and contains a table called `TestTafel` that is accessible to a database user called `JASP`.
It is running locally and a password is required to connect.

You might need to install [SQL Server Native Client](https://docs.microsoft.com/en-us/sql/relational-databases/native-client/applications/installing-sql-server-native-client?view=sql-server-ver16) to get this example to work.

Select the `ODBC` driver in the dropdownmenu, it doesn't really matter what you enter for `hostname` and `port` as it is ignored by the odbc it seems.

Then for the database "name" you enter something like:
`DRIVER={SQL Server Native Client 11.0};SERVER=DESKTOP-C7VK7RJ\SQLEXPRESS;DATABASE=TestDB`

As you can see only a small part of that is the name of the database and the rest is specifying which particular driver should be used to connect to the database (in this case `SQL Server Native Client`) and the server is the same as "Server name" in the connection dialog of Sql Server Management Studio (SSMS).

If you use a different driver from Sql Server Native Client you will have to replace `SQL Server Native Client 11.0` with the appropriate string.

You then enter username and password and press `Connect to database".

The "Query" textinput should then be enabled (or you see an error below in "Preview data") and you can enter a `SELECT` sql statement, or something else that returns data.

"Execute" the runs the query and if everything went alright then you should see something like this:
![JASP showing how to connect to a SQL Server database](filemenu/SqlServerConnectionExample.png)

The output (a few lines) will be shown below in the preview panel and if you're satisfied simply choose "Load into JASP".

### Snowflake Warehouse

Both described ways of connecting below make use of ODBC and you might need to get the `SnowflakeDSIIDriver` from somewhere.
#### Simple connect

This uses a SQL DB user, which of course needs to be present in the database.

The settings should look like:
```
DB Driver: ODBC Driver
Hostname: [your_instance].snowflakecomputing.com
Port: 1433
Name: DRIVER={SnowflakeDSIIDriver};SERVER=[your_instance].snowflakecomputing.com;WAREHOUSE=[your warehouse];DATABASE=[your database];
Username: [SQL User]
Password: [SQL User PW]
```

#### Using single sign on

More secure and flexible, Uses SSO login, can change which role is used.
Query must use fully qualified database.schema.table

Settings should look like:
```
DB Driver: ODBC Driver
Hostname: [your_instance].snowflakecomputing.com
Port: 1433
Name: DRIVER={SnowflakeDSIIDriver};SERVER=[your_instance].snowflakecomputing.com;WAREHOUSE=[your warehouse];ROLE=[select a role];AUTHENTICATOR=externalbrowser;
Username: [sso login]
Password:
```