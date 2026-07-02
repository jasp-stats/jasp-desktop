#include "allhelp.h"

QString AllHelp::Database()
{
	 return tr(R"for_c++_include(
Database
=========

JASP supports reading data directly from a database through [Qt Sql](https://doc.qt.io/qt-6/sql-driver.html#supported-databases).

Support for most databases hasn't yet been tested but as the interface is basically a passthrough to qt's sql support it is made available for people to try. If you've managed to get an undocumented database running let us know with a feature request at [our issue page](https://github.com/jasp-stats/jasp-issues/issues/new/choose).

The Qt database drivers are often wrappers around real drivers that must be installed and discoverable using the systems PATH environment variable.
For example the Postgress drivers can be found [here](https://www.postgresql.org/download/).

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
)for_c++_include");
}
QString AllHelp::PrefsAI()
{
	 return tr(R"for_c++_include(
AI Settings
=========

The JASP AI Agent is a built-in assistant that can run analyses, inspect data, annotate results, and answer statistical questions — directly inside JASP. It connects to any OpenAI-compatible API and can be shaped through personas, tool capabilities, and system prompts.

These settings let you connect the agent to your AI provider, customize its behavior, and control which JASP features it can access.

All settings persist across sessions.

---

## AI Service

### Enable / Disable
Toggles the entire AI feature on or off. When enabling, a confirmation dialog explains the terms of use and prompts you to back up your work. With AI disabled, all AI-related settings are hidden.

---

## Provider & Model

### Provider
Select your AI provider from the dropdown. JASP ships with several preconfigured providers (DeepSeek, OpenAI, Anthropic, Google Gemini, Mistral, and others). Each provider comes with one or more preset models. Choosing a provider populates the Model dropdown and resets the connection fields to that provider's defaults.

### Model
Choose a model from the selected provider. Switching the model updates the advanced fields (extra parameters, system prompt postfix, etc.) to match the model's shipped configuration.

**Note:** Switching provider or model clears the current chat conversation.

### Connection

#### Endpoint URL
The chat completions API endpoint. JASP sends requests here using the standard OpenAI-compatible JSON format. Any provider supporting the `/v1/chat/completions` convention should work.

**Example** (DeepSeek):
```
https://api.deepseek.com/v1/chat/completions
```

#### API Key
Your authentication key for the AI provider, stored securely in your operating system's credential store. The key is never saved in plain-text preferences.

#### Model
The model name sent in API requests. Must be a model available at the configured endpoint.

**Example**: `deepseek-v4-flash`

#### Test Connection
Sends a minimal request to verify that the endpoint URL and API key are valid. A green success message or red error message appears next to the button.

### Advanced

#### System Prompt Postfix
Text appended to the system prompt for this specific model. Use this to add model-specific instructions (e.g., encouraging assertiveness, setting timeout expectations, or defining workflow rules). This is appended after the Common System Prompt and the active persona prompt.

#### Include full tool schemas in request
Controls how tool definitions are sent to the AI in API requests.

- **On** (default): full JSON schemas (including parameter types like `integer`/`boolean`) are placed in the structured `tools` array. Helps models that struggle with type-safety in tool calls, but uses more tokens.
- **Off**: name-only stubs go in the `tools` array, and full schemas are included as a system message instead. Saves tokens and works well with models that don't need explicit type information (e.g., DeepSeek).

#### Extra parameters (JSON)
A JSON object merged into every API request body. Use this to pass provider-specific parameters that JASP doesn't expose directly — for example, `max_tokens` or `reasoning_effort`. The fields `model`, `stream`, `messages`, `tools`, and `text` are protected and will be ignored if included.

**Example**: `{ "max_tokens": 4096, "reasoning_effort": "medium" }`

#### Single chat token limit
Optionally caps the total tokens in a single API request. If the system prompt, conversation, and tools exceed the limit, the request is rejected with an error message. Use this to control costs or stay within provider limits. Approximately 4 characters ≈ 1 token. Default: 256,000.

#### Per-message extra fields (JSON)
A JSON object merged into every *message* in the API request (not the top-level body). Use this for per-message features like explicit caching.

**Example**: `{ "cache_control": { "type": "ephemeral" } }`

The fields `role`, `content`, and `text` are protected.

### Reset Model
Restores the currently selected model's extra parameters, system prompt postfix, and all advanced checkboxes back to their shipped defaults. Does not affect other models, personas, or global settings.

---

## Personas

Personas define the AI assistant's role, expertise, and writing style. JASP ships with several read-only system personas, and you can create your own.

Click the **+** tab to create a new persona. The green dot on a tab marks the currently active persona.

### Name
A display name for the persona (e.g., "Alfred the Assistant").

### Avatar
An optional square image shown next to the assistant's messages in the chat window. Accepted formats: PNG, JPG, GIF, SVG.

### Persona Prompt
Free-form instructions that define the persona's behavior, tone, expertise, and any role-specific rules. This text is combined with the **Common System Prompt** to form the complete system message sent to the AI.

### Set as Active
Makes the selected persona the active one used for all chat interactions.

### Duplicate
Creates an editable copy of the selected persona.

### Delete Persona
Removes a user-created persona. System personas cannot be deleted.

### Common System Prompt
A system prompt applied to every request, regardless of which persona is active. This is where you define global behavior rules, context about JASP, and output formatting preferences. The active persona's prompt is appended after this.

### Persona Capabilities

The AI agent uses JASP tools to interact with JASP — for example, listing modules, running analyses, reading data, inspecting results, and composing annotated output. The availability of these tools is controlled through capabilities.

#### Capabilities
Each capability is a named group of related tools. The checkbox grid shows every capability and its description. Checking a capability enables all the tools it requires; unchecking disables them.

**Important**: tools are shared across capabilities. If two capabilities both need the same tool, the tool stays enabled as long as at least one of those capabilities is checked. Unchecking every capability that needs a given tool will disable that tool.

#### Advanced
Expand this section to toggle individual tools directly, regardless of capability groupings. Capability checkboxes update automatically to reflect which capabilities are fully covered by the current tool set. 

On first use, all capabilities are enabled. Clicking a capability will adjust the tool set and may cascade — for example, enabling a broad capability may also auto-enable narrower capabilities that are now fully covered, and disabling a narrow capability may cause a broader one to lose coverage.

---

## Annotation

These settings control the prompt sent when clicking the **Annotate Analysis** button (the AI icon) in an analysis header.

### Use custom annotation prompt
When checked, the custom prompt below is used instead of the default. When unchecked, the default annotation prompt is sent.

### Annotation prompt
The text sent to the AI chat when you click the Annotate Analysis button on an analysis. The AI will see this as a user message along with the full conversation history.

---

## Chat Appearance

### My icon
Choose a custom avatar image to represent you in the chat window. Accepted formats: PNG, JPG, GIF, SVG. A default icon is used when no custom image is set.

---

## MCP

### Enable MCP server (Model Context Protocol)
Toggles the MCP server on or off. When enabled, JASP exposes an HTTP RPC server that allows external applications to control JASP — including the AI agent.

Use this to integrate JASP with MCP-compatible tools and workflows.

To change the port or bind IP address, see **Advanced Preferences** > **Remote control**.

---

## Reset AI Settings
Restores the endpoint, API key, model, system prompt, token limit, and all other AI settings to their factory defaults. Personas created by the user are removed; system personas are restored to their original shipped configuration.
)for_c++_include");
}
QString AllHelp::PrefsAdvanced()
{
	 return tr(R"for_c++_include(

Advanced Preferences
=========

With the advanced parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

## Modules options

### Remember enabled modules
If you've enabled this option then JASP will remember which modules are activated and make sure they remain that way even when you close JASP. So supposing `Summary Statistics` was enabled and JASP closes then after reopening JASP it will be enabled immediately.

### Developer mode

If you enable this you see a few extra options appear. One is "Generate markdown files for help", which will ignore any markdown helpfiles for an analysis and instead will only show generated markdown from each `info` field on each qml item and jaspObject. Another option is the "Module library URL" field, which allows you to change the URL of the JASP module library web application from [https://module-library.jasp-stats.org/](https://module-library.jasp-stats.org/) to a locally running instance.

#### Development module
Besides that it will show "Development module" where you can load an R-package installed/restored with `renv`. For this you need to know the name of your module (for instance: `jaspAnova`) and the R-library or `.libPath` that `renv` created for you. 

To use `renv` to install your package you first `git clone` or `git checkout` the code you want.
An example using `jaspAnova`:
```bash
# in a terminal:
git clone https://github.com/jasp-stats/jaspAnova
cd jaspAnova
R
```
If you don't have `renv` installed already you can get it by running `install.packages('renv')`.
Thus we are in the `jaspAnova` folder we just created and then in R:
```R

renv::restore(clean = TRUE)     # Project library is filled with renv.lock dependencies
renv::install('.')              # Install local pkg to project library

message("R Project library for developer mode:\n", .libPaths()[1])
```

This will print the project library you need for running your module. 
You make sure to copy that to the required preference field and do the same for the module name.

As a sidenote: running the first `renv` command for a project library might actually trigger `renv::init`, which will ask the user to agree to managing some files for the user. Just answer `Y` there.

#### Temporary project library
Often the default library is also the user's normal R library, for instance something like "C:/Users/A_User/AppData/Local/R/win-library/4.5". Restoring with `renv` there can mess up the entire normal library then, as it removes and adds packages based on the development module being installed. While this is very fast and often avoids reinstalling stuff it might be undesirable.
A simple way to avoid this is to tell `renv` to install to a temporary folder, keeping your usual library as-is.
```R
.libPaths(tempdir())
# restore, install and print project library
```
On Windows you would then get something like: `C:/Users/A_User/AppData/Local/Temp/RtmpYN3aqC`

#### Dedicated project library
You could of course also decide to make a dedicated project library somewhere and use that instead of `tempdir()`, or if the temporary directory it creates isn't accessible for JASP. 
As an example, you can create a directory somewhere and pass that to `renv`.
```R
.libPaths("~/jasp-renv-package-library")
```
This will load `jasp-renv-package-library` from your homefolder (which would make it `/home/username/jasp-renv-package-library` on macos/linux or `c:\Users\username\jasp-renv-package-library` on windows).
You then do the above `renv::restore` and `renv::install`.

#### Installing extra packages
Until we finish syntaxmode for JASP, which is now in beta, you might want to use `jaspTools` to run analyses in R. You can just run `renv::install('jaspTools') and it will be installed to your project library. The same goes for any other package(s) you might need.

If you've installed other packages while developing you might want to add them to the `renv.lock` file, is easy. However you probably don't want to add all the packages loaded to the `renv.lock` file, so we run it while excluding some packages:
```
renv::snapshot(exclude=c('jaspTools'))
```
If there are other packages you do not want listed as a dependency you can simply add them to the `exclude` list, but `jaspTools` should definitely not be a dependency of a jaspModule.

#### renv guide
To understand more about `renv` you should consider reading their [getting started guide](https://rstudio.github.io/renv/articles/renv.html). It will explain what a project library is, and why this system is helpful in the first place.

### Configuration file options

Here you can select a path to a `toml` configuration file for JASP.
This allows you to preload certain modules on a system, or set some default options for particular analyses. This file can be located on a users computer or come from a remote URL for workplace deployment.

<details>
<summary>Creating you own configuration file</summary>

To make your own configuration file you create a `toml` file somewhere, for instance `my-first-jaspconfig.toml`. An example:
```toml
Format = "0.1.0"
JASPVersion = "0.19.3"

EnabledModules = ["jaspAudit",]

[Constants]
rain = true

[Modules.jaspAudit.Constants]
high = 90
medium = 50
low = 40

[Modules.jaspAudit.Analyses.auditClassicalWorkflow.Constants]
high = 77 #these will take precedent over the Module wide and JASP global constants
low = 42

[Modules.jaspAudit.Analyses.auditClassicalWorkflow.Options]
min_precision_test = {Value = true, Lock = true} #comment
materiality_test = false #comment
min_precision_rel_val = 0.14
materiality_rel_val = {Value = 0.02, Lock= true}
```

These constants can be used by module developers to change some settings for particular users and incorporate that smoothly into their analysisforms (qml files).
They can be accessed as follows:
```qml
x = form.getConstant("constant", <default value if not defined>)
```

</details>

### Github personal access tokens
When a dynamic module is installed JASP uses R internally to get all dependencies and for this it requests info from https://github.com and often this goes well.
They do have a rate limiter there however, see https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting and that can cause the module installation to fail.

To work around this we have added some options to manage a so called personal access token, and it is then passed on to R through an environment variable called `GITHUB_PAT`. This is also very useful for doing stuff in RStudio.

We added a default one that is available for R inside of JASP, but this is shared with all JASP users and thus might (at some point) become oversaturated. In that case you might want to add your own.

This can be done by generating a new token for your github account at https://github.com/settings/tokens/new and then either copying the resulting code into the settings here in JASP or through your OS. If set in your OS, you might have done this for R already, JASP will automatically use it if set use "the default" value.

Please *DO NOT* give this personal access token **any permissions** at all, because it really isn't necessary for the way it is used in JASP.

Otherwise you can set it specifically in JASP by unchecking "Use default PAT" and then copying your token-code into the custom GITHUB_PAT textbox, this is probably the easiest if you do not know what an environment variable is.

## Logging options

### Log to file
When you check this JASP will start logging many of the actions it performs to logfiles. 
Logging is especially useful when you are developing your own module, or run into a problem and wish to reach out to the development team.
The logs might help us give insight in the nature of your problem. Note that you might need to restart JASP for the logging process to start.
The number in the input field "Max logfiles to keep" defines how many logfiles will be kept at maximum to conserve diskspace. Any extra, older, logfiles will be removed.
The files can be viewed by pressing "Show logs".


## Engine options
JASP uses R in the background to run all the analyses. Here you can change how many JASP is allowed to start at the most. When deciding how many maximum engines you might want it is helpful to keep in mind that right now each module is ran by only a single engine at the moment. So having 10 analyses from the ANOVA module will not go any faster by allowing maximally 10 engines. While running 10 analyses from 10 modules will go *way* faster.

### Sandbox
On Windows you will also see the "Sandbox engine" checkbox. When enabled this will isolate the engine process in such a way that none of the code we run in R is able to do anything with the rest of your system. No networking nor access to files on the system.
This is good, because we rely on a lot of opensource third party R packages from CRAN and Github. And while we have not heard of any supplychain attacks via the R package ecosystem we think it is better to avoid such problems as much as possible.

When developing R modules (and thus enabling "Developer mode") this will be disabled by default because otherwise the engine won't be able to load your development module.

On macOs the operating system already implements at least a file access sandbox for applications and will ask for permission before accessing files outside it's "JASP.app". 

The same goes for the flatpak distribution of JASP on Linux, which in essence is a sandbox, which mediates access through file dialogs and the like. Here however we ask for home-directory read-access so that we can get normal user readable paths to recent files.
This is entirely disableable by changing JASP's configuration per `flatpak override org.jaspstats.JASP --nofilesystem=~`.


### Show engines
This opens a window the developers use to keep an eye on what each engine is up top.
It could be interesting to see what engines are busy and with what, although the level isn't very detailed. It does however give you the option to manually kill and engine by right-clicking it. 
This can be helpful if it stopped responding during some particularly heavy yet unnecessary calculation.
)for_c++_include");
}
QString AllHelp::PrefsData()
{
	 return tr(R"for_c++_include(

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

### Import settings

#### Order by values

This will make sure labels are always sorted by their value on import and synchronisation.
It does however incur a slowdown that is usually negligible, but for certain cases quite a large one.
This would be the case for large, >500k rows of different scalar values (for instance, random ones).
In such a scenario you might consider disabling this, but for most people the default *on* setting is fine.

#### Threshold for Scale

Importing data in JASP has a threshold value that determines if a column should be treated
as a Scale type or as a Categorical (Nominal or Ordinal) type. The default value of this parameter is 10.
This means that if you have fewer (or equal) than 10 different integers in the data, the column
gets the Ordinal type (Nominal type if only 2 different integers are found) else it will get the Scale type. Be aware that this value is used when
importing the data, so data needs to be reloaded (or synchronized) to take effect.

#### Maximum allowed levels for scale when used as nominal/ordinal

Analyses may specify that some variables should be nominal or ordinal. But a scale variable might be used and interpreted as nominal or ordinal. However many scale variables cannot really be interpreted as nominal or ordinal and will generate too many levels, that most analyses cannot accept.
To prevent this, this setting specifies what is the maximum number of levels that will be accepted for a scale value to be interpretable as a nominal or ordinal.<br>
This setting is not used if an analysis specifies explicitly a maximum number of levels: i.e. for 'Grouping Variables' in Independent Samples T-Test, the maximun (and minimum) of levels of a variables is set to 2.<br>
But for most analyses this default setting is used: this prevent the user from using wrong variables, and make the engine running too long.

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
)for_c++_include");
}
QString AllHelp::PrefsResults()
{
	 return tr(R"for_c++_include(

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
)for_c++_include");
}
QString AllHelp::PrefsUI()
{
	 return tr(R"for_c++_include(

User Interface
=========

With the user interface parameters in JASP you can specify the following options:
(All these settings will remain after restarting JASP.)

### Fonts
Here you can specify which font will be used by the interface (File Menu, Ribbon bar, Analysis options panel), the R, Lavaan or JAGS Code, or the results. If no choice is made, the default font is used.

Use Qt's textrendering: JASP using Qt to render interface text by default. JASP will use native text rendering when the option unchecked. Try to check/uncheck this if you find that the text on JASP looks terrible(blurry or fuzzy...). This will take effect after restarting JASP.

### Themes
Here you can specify if you would like to have a "light" or a "dark" theme on the interface of JASP. The dark theme is much easier on the eyes if you are in a dark environment while the light (and default) theme is clearer in bright light.

### Preferred Language
Select the language in which you would like to use JASP.

If you would like to improve our translations, or to add a new language, please visit https://jasp-stats.org/translation-guidelines/

### Alternative Locale
This is by default on, and enables a selection menu of languages and territories. 
Whatever you select here is what will determine how your numbers in JASP are formatted, such as but not limited to, whether the decimal separator is a ',' or a '.' for instance.
It will also be used to format monetary columns in the output, and in the future also date, time and datetime columns. 
This will also determine what kind of thousands-separators are used.
If you disable using an alternative locale it will use the locale corresponding to the selected language.

### ALT-Navigation mode
Turn the ALT activated navigation mode on or off.

### Check for updates
JASP doesn't share any of your data when it gets updates, not even which version of JASP you are using.
It does share your IP-address with the server but that is required for internet to function.

The list of known issues it downloads is rarely used, mostly the issues are at [jasp-issues](https://github.com/jasp-stats/jasp-issues/issues). 
However if we realize a terrible error has slipped into an analysis this will show you *in the analysis* that there is something you should take into account. 
Luckily we almost never need to use it.

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
)for_c++_include");
}
QString AllHelp::computedcolumns()
{
	 return tr(R"for_c++_include(
<h1>Computed Columns</h1>

<details>
	<summary><b>Adding a Computed Column</b></summary>
	<img src="other/CreatingComputedColumn.gif"/>
	<p>When you have the dataview of JASP in front of you then in the top-right corner there should be a '+'-symbol.
		When you click this it opens a dialog that lets you create a computed column, you are required to enter a valid and unused column name for it.</p>
	<p>	You can choose whether you want to define it through R code or through the drag &amp; drop constructor, similar to those used in filtering.</p>
	<p>	The initial type for your column can be selected as well. This can be changed at any later point anyway, just like the other columns (through the type dropdown menu from the columnheader icon). </p>
	<p> You can also change any column to a computed column at any time via the variables window.
		 Or change it from a drag&amp;drop to a r-code computed column, it will give you the r-code of any drag&amp;drop columns, but not the other way round.</p>
</details>

<details>
	<summary><b>Using Drag &amp; Drop Constructor</b></summary>
	<p>Computing new data using the Drag &amp; Drop constructor is quite straight-forward, especially if you've already used it for filtering. Still, some examples follow:</p>
	<ul>
		<li>
			<b>Converting a Scale (Height Ratio -> Height)</b>
			<img src="other/ComputedColumnHeight.gif"/>
			<p>Supposing we have a column <i>Height Ratio</i> and we want to convert it back to proper heights. We assume that 180cm is the norm and with this knowledge the animation above should be clear.</p> 
		</li>
		<li>
				<b>Converting to Logical (as text)</b>
				<img src="other/ComputedColumnPopularVote.gif"/>
				<p>Supposing we have a column <i>Popular Vote</i> and we want to show clearly who got it and who didn't. We create a column with <i>Nominal Text</i> as type and specify the requested formula, see above animation for details.</p> 
			</li>
			<li>
				<b>Dividing a scalar (Height Ratio) into 3 named fields</b>
				<img src="other/ComputedColumnHeightCategoryDragNDrop.gif"/>
				<p>Supposing we have a column <i>Height Ratio</i> and we want divide the rows up based on if they are <i>Short</i> or <i>Tall</i> or not. We create a column with <i>Nominal Text</i> as type and use the <i>cut</i> function to divide <i>Height Ratio</i> into 3 levels. Afterwards we give each of the intervals a nice name to clarify what's what.</p> 
			</li>
	</ul>
	<p>A new feature missing from the rather old help gifs is the changing of a columntype within the constructor. Just click the icon and choose the type you want, the tooltip will show an example of the data.</p>
</details>

<details>
	<summary><b>Using R</b></summary>
	<p>Computing new data using R is quite straightforward, you can use many standard functions and can refer to other columns through name.</p>
	<p>Some examples:
			<ul>
				<li><p><b>Multiplying a scalar (Height Ratio) by some fixed value</b></p>
					<p>Make sure that your column is defined as <i>Scale</i>, even though the other types would also work.</p>
				<p class="code">Height Ratio * 180</p>
			</li>
				<li><p><b>Dividing a scalar (Height Ratio) into 3 named fields</b></p>
					<p>Make sure that your column is defined as <i>Nominal Text</i></p>
					<p class="code">cut(Height Ratio, 3, c("Short", "Average", "Tall"))</p>
				</li>
			</ul>
			There are of course many possibilities and you are only limited by the availability of (safe) functions in JASP. Should you miss something than don't hesitate to contact us so we may add it to the next version.
	</p>
	<p>You can also transform the type of the column by appending the wanted type. So suppose we have a column <code>chestnut</code> we can convert it to any type as: <code>chestnut.scale, chestnut.ordinal and chestnut.nominal</code>. </p>
</details>

<details>
	<summary><b>Rowwise operations</b></summary>
	<p>
		A special type of computed column creation is the rowwise operations of mean, variance etc.<br><br>
		These functions are:<br>
		<code>rowMean, rowSum, rowSD, rowVariance, rowMedian, rowMin, rowMax</code><br>
		They will probably return NA for when any of the column's rows contain one.
		So there are also variants like <code>rowMeanNaRm</code> available that drop NA first.<br><br>
		It can be used like: <code>rowMean(Column 1, Column 2, Column 3)</code>
	</p>
</details>

)for_c++_include");
}
QString AllHelp::easyfilterconstructor()
{
	 return tr(R"for_c++_include(
<h1>How to use the Easy Filter constructor</h1>

<p>The following examples will show you how you can create your own filters in a (relatively) simple manner: </p>

<p>A new feature missing from the rather old help gifs is the changing of a columntype within the constructor. 
	Just click the icon and choose the type you want, the tooltip will show an example of the data.
</p>

<p>
	Another feature missing in the gifs is:
	
	Keeping or dropping levels is about whether each level (a value and label) of a column is passed to the analysis, levels are inspectable via the "Label editor". 
	Keeping levels means that all levels defined on the column are passed to the analysis.
	Dropping levels means that only those levels that occur on rows that pass the filter are in the end end communicated to the analysis.
	As an example, suppose you have a column <code>contBinom</code> with two values/labels "0" and "1", and we filter all the "1"es out whereafter the analysis would get:
	<ul>
		<li>When keeping levels: "0", "1"</li>
		<li>When dropping levels: "0"</li>
	</ul>
	One caveat is that when the filter is set to drop levels, yet doesn't actually filter anything, it won't drop levels.
</p>

<details>
	<summary><b>Filtering a continuous column</b></summary>
	<p>
	This example shows how you can use the easy filter to check for equivalence or greater/less than and the like. 
	We will use the included dataset "Kitchen Rolls" and will filter on the ParticipantNumber-column.
	The steps taken in the video will be outlined underneath.
	</p>
	<img src="other/FilteringParticipant.gif"/>
	<ul>
		<li>First we click once the required column (ParticipantNumber) to add it to the overview.</li>
		<li>Then we select '=' or equivalence, which is added to or previously selected column.</li>
		<li>The empty space to the right of the equivalence can be filled up by dropping different other operators or columns, but for now we click it and then type the number '2'.</li>
		<li>Now we apply the filter by pressing the appropriate button and we will see that now only the row where ParticipantNumber is 2 is active.</li>
		<li>After that we select the '2' again to be able to edit it again. By clearing it completely we could remove it, but for now we just change it to '3'.</li>
		<li>Once more we apply our pass-through filter and again only the corresponding row is active.</li>
		<li>To select more than one row we can make use of the less-than operator or '&lt;' and so we remove our constructed formula and create a new one.</li>
		<li>We add the ParticipantNumber-column again</li>
		<li>Then add '&lt;' and enter '5' in the empty righthand side.</li> 
		<li>Applying the filter makes the result immediately visible.</li>
		<li>Next we clear everything and the animation repeats.</li>
	</ul>
</details>

<details>
	<summary><b>Filtering one or more categoricals</b></summary>
	<p>This example shows you how to filter a certain categorical variable alone and in combination with another. We will do this through both the variable window and the easy filter</p>	
	<img src="other/FilteringSex.gif"/>
	<ul>
		<li>First we open the variable window for the 'Sex' column by clicking the header.</li>
		<li>Here we turn off the 'M' value, we immediately see that all corresponding rows are turned inactive.</li>
		<li>Ticking 'M' back on again and turning off 'F' gives us the opposite situation.</li>
		<li>Next we look at a different categorical, after making sure both 'M' and 'F' are active, we open the variables window for 'Student'.</li>
		<li>Here we untick 'N' and we see that the only visible row containing this is turned inactive.</li>
		<li>This we combine with unticking 'M' in 'Sex' and see that now the rows corresponding with either 'M' or 'N' are both inactive.</li>
		<li>To do this with the easy filter constructor is also possible and to show this we first clean up the current selection by choosing "erase all".</li>
		<li>Then we open the easy filter and enter 'Sex', followed by '=' and we enter the text 'F' on the empty righthand side.</li>
		<li>To combine this with the other categorical we enter a new formula below by selecting 'Student', '=' and entering 'N'.</li>
		<li>After choosing 'Apply pass-through filter' we see only one row active where these values are matched.</li>
		<li>To show the opposite we remove the last formula and replace it with one that makes 'Student' be inequal to 'N'.</li>
		<li>After applying we now see that only the one row previously active is now the only inactive one, as expected.</li>
		<li>Next we clear everything and the animation repeats.</li>
	</ul>
</details>

<details>
	<summary><b>Filtering on standard deviation</b></summary>
	<p>This example shows you how to filter based on being larger than it's own standard deviation.</p>	
	<img src="other/FilteringStandardDeviation.gif"/>
	<ul>
		<li>We will filter 'mean_NEO', so we enter it as the basis of our first formula.</li>
		<li>Clicking the '>' symbol adds it to the formula.</li>
		<li>Because we want to check for the standard deviation we click the corresponding &sigma; symbol from the function overview to the right.</li>
		<li>We then drop another copy of 'mean_NEO' into this and apply our filter.</li>
		<li>Now we see that only rows 4 and 5 are inactive.</li>
		<li>To see the contrast with a larger value we multiply the standard deviation by adding '*' and typing 2 on the righthand side.</li>
		<li>After choosing apply we now see that only row 2 remains active.</li>
		<li>After that the filter is cleared and the animation repeats.</li>
	</ul>
	<p>Of course you can also use the other functions in the list on the right in combination with the operators above to check for many desired values.</p>
</details>

<details>
		<summary><b>Conditioned filtering - simple example</b></summary>
		<p>This is a simple example to show how you can use conditional filtering. In this case we just want the rows that show the maximum age.</p>	
		<img src="other/FilteringAgeConditional.gif"/>
		<ul>
			<li>We will filter 'Age', so we enter it as the basis of our first formula.</li>
			<li>Clicking the '=' symbol adds it to the formula.</li>
			<li>Then we add 'max(Age)' and apply it.</li>
			<li>We now see there is one active row showing a 51 year old female.</li>
			<li>To get the maximum age per sex we add the 'condition on'-operator or '|' followed by 'Sex'.</li>
			<li>After applying we see that besides the woman there is now also an active row with a 38 year old male.</li>
		</ul>
</details>

<details>
		<summary><b>Conditioned filtering - outliers per group</b></summary>
		<p>This is a simple example to show how you can filter outliers per group using conditional filtering.</p>	
		<img src="other/FilteringExtraConditional.gif"/>
		<ul>
			<li>We will filter the absolute value of 'extra'.</li>
			<li>To this is added '>' and &sigma; for the standard deviation</li>
			<li>'extra' is placed inside &sigma;() and the result multiplied by 2.</li>
			<li>After applying we see that there are 3 active rows that pass this criterium, all in group '2'</li>
			<li>To get the outliers per group we add the 'condition on'-operator or '|' followed by 'group'.</li>
			<li>After applying we see that there is one more active row, this time from group '1'.</li>
		</ul>
</details>
)for_c++_include");
}
QString AllHelp::plotediting()
{
	 return tr(R"for_c++_include(
Plot Editing
============

Although we try our best, sometimes plots created by JASP need some tweaks. The current editor allows you to modify the axes of a plot, but future versions of JASP should provide more options.

### Basic options

In the top left you can select which axis you want to modify. You can select either the x-axis or the y-axis. All of the options below modify either the x-axis or the y-axis, depending on which is currently selected.

#### Title

*   `Show title`: Should the title of the selected axis be shown?
*   `Title`: Specify what to put on the axis title. Usually, this is plain text but sometimes it is an R expression, for example when a title contains greek or mathematical symbols. See [Parse title as R expression](#parse_as_r_expression)) for more details on that.

#### Ticks

Ticks consist of two components: positions and labels. Very often, but not always, the labels simply show the positions, for example, a position at -4 is indicated with the label "-4".

If you want the labels and positions to be the same, it is convenient to select `Specify sequence`. If you select `Specify sequence` then the ticks are fully determined by three numbers. The left and right endpoints are determined by `from` and `to` respectively. After every `steps` numbers, there will be a label. For example if `from` is -4, `to` is 4, and `steps` is 2, then you would get ticks at -4, -2, 0, 2, 4.

Sometimes you may want the labels to differ from the positions, or maybe you do not want equally spaced labels. In that case, you can select `Set manually`. Once selected, a table is shown that allows for individually modifying the positions and labels. If you left-click on a column, two green plus icons appear and one red cross. The left and right plus icons allow you to insert a new position and label on the left and right of the selected column. The red cross allows you to delete the selected column.

Note that for discrete axes, the ticks cannot be adjusted.

### Advanced options

#### Parse title as R expression

Some figures have complicated titles, which cannot be typeset in plain text. For example, to put the symbol δ (delta) on an axis you can type "delta" and enable this option. However, you can also create more complicated equations, such as `over(alpha, beta)`. For more details, see [here](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html) for more details.

#### Limits

The limits consist of two values: `Upper limit` and `Lower limit`. These values determine the outer end points of an axis or plotable area. For example, even though the axis positions are going from 80 to 100 in steps of 10, it might be that there is a data point at 102. The limits provide some leeway to show all data without being forced to increase `to` to 105. You can select one of the following options

*   `Based on data`: Use an automatic default for the limits that is determined based on the data instead of following the value of the ticks.
*   `Based on ticks`: Automatically rescale the limits with the ticks. For example, if you increase ticks' `to` to some value larger than the `Upper limit`, this will automatically set `Upper limit` to `to`.
*   `Set manually`: Specify two values for the `Upper limit` and `Lower limit` manually.

When using `Set manually`, take these two warnings to heart:

1.  Any data that falls outside of the limits is not shown.
2.  To adjust the `Ticks` you now also must adjust the `Limits`. For example, if the `Upper limit` is set to 4, and you now set `to` to 8, then the right limit of the figure remains at 4, so no ticks larger than 4 will be shown.

### References

Almost all plots in JASP use the R package ggplot2. This package is an implementation of the grammer of graphics. For more information, see

*   Wickham, H. (2016). _[ggplot2: Elegant Graphics for Data Analysis](https://ggplot2-book.org/)_ (3rd ed.). Springer-Verlag.
*   [https://ggplot2.tidyverse.org/index.html](https://ggplot2.tidyverse.org/index.html)
*   Wilkinson, L. (2005). _[The grammar of graphics](https://dx.doi.org/10.1007/0-387-28695-0)_ (2nd ed.). Springer.
*   [https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html)
)for_c++_include");
}
QString AllHelp::rfilterconstructor()
{
	 return tr(R"for_c++_include(
How to use the R Filter
=======================

Using the R filter gives you the opportunity to write custom filters for your data. You can combine this easily with the results from the other filters, that is, those selected in the variable window or made in the easy filter. In fact, you can easily observe the code that is generated by those other filters in the top read-only textbox that starts with generatedFilter <-.

To refer to your data simply enter the columnname in the code-window, JASP will make sure that it refers to the correct data. If your columnname contains spaces you must enter these as well. You can also transform the type of the column by appending the wanted type. So suppose we have a column named `chestnut` we can convert it to any type as: `chestnut.scale, chestnut.ordinal and chestnut.nominal`.

Keeping or dropping levels is about whether each level (a value and label) of a column is passed to the analysis, levels are inspectable via the "Label editor". Keeping levels means that all levels defined on the column are passed to the analysis. Dropping levels means that only those levels that occur on rows that pass the filter are in the end end communicated to the analysis. As an example, suppose you have a column `contBinom` with two values/labels "0" and "1", and we filter all the "1"es out whereafter the analysis would get:

*   When keeping levels: "0", "1"
*   When dropping levels: "0"

One caveat is that when the filter is set to drop levels, yet doesn't actually filter anything, it won't drop levels.

Some examples, which will of course only work if your data contains those columns, should serve to clarify:

*   **Filtering the opposite of the easy-filter and label-selection**

	!generatedFilter

*   **Filtering on Gender and TestScore**

	Gender == "Female" & TestScore > 5

*   **Filtering on Gender and TestScore while taking into account easy-filter and label-selection**

	generatedFilter & Gender == "Female" & TestScore > 5

*   **Filtering on Age conditioned on Sex**

	(mean(Age) > Age) %|% Sex

	The operator %|% is a JASP-specific R-operator that makes sure the code to the left of it is run separately for each condition to it's right. In this case it means that the filter passes all rows on which Age is lower than the mean Age for that specific Sex. Be sure to add the parentheses around the exact expression that you wish to be conditioned though, the following would not work as expected: mean(Age) > Age %|% Sex. This would just condition Age on Sex, while leaving mean(Age) to be calculated for the whole column.
)for_c++_include");
}
QString AllHelp::variableslabeleditorhelp()
{
	 return tr(R"for_c++_include(
## **Variable View & Label Editor Help**

This menu allows you to manage how your data is structured, labeled, and interpreted by JASP. The sections below explain each setting and how to use it effectively.

### **Name, Long Name & Description**

* **Name:** A short name of the variable which will be shown in tables and along the axis of graphs.
* **Long Name:** Designed for reporting. *Note: Integration into output tables is a work in progress. In the future, this will allow JASP to automatically display long names in your output without cluttering your data view / spreadsheet.*
* **Description:** Space for notes, hypotheses, codebook definitions etc.

### **Types / Levels of Measurement**

* **Scale (Ruler icon):** Continuous numerical data (e.g., age, weight, test scores).
* **Ordinal (Staircase icon):** Categorical data with a logical, built-in progression (e.g., education level, Likert scales, tournament placement). *Note: JASP may initially import ordered numbers as Scale; simply click the icon to switch it to Ordinal.*
* **Nominal (Venn diagram icon):** Text/strings or categorical data without an inherent order (e.g., eye color, country, ID numbers).

### **Missing Values**

When a data cell is empty — whether because a measurement instrument failed or a participant skipped a question — it is treated as a **missing value**.

* By default, JASP recognizes empty cells as missing data if they contain:  
  NaN (not available number), NA, nan, . (period), blank space
* If your dataset uses a specific numeric code for missing data (such as \-99 or 999), you can add that code to the **Missing Values** list by activating **Use custom values**, entering them next to the +button and clicking the +button. JASP will then treat those entries as missing and exclude them from statistical calculations.

### **How JASP Imports Data**

When a dataset is imported, JASP automatically assigns a variable type based on the contents of the column.

If a column contains only numbers (excluding recognized missing values):

* **Nominal**: exactly 2 unique integer values  
* **Ordinal:** 3-10 unique integer values  
* **Scale:** 1 or more than 10 unique integer values, or at least 1 value containing a decimal.

If at least one of the values is not a number nor a missing value

* **Nominal:** 1-2 unique values or more than 10 unique values  
* **Ordinal:** 3 to 10 unique values

Some of these import settings can be customized via: ‘File → Preferences → Data → Import Settings’. There you can also change ‘**Missing Value List**’ and the ‘**Threshold for scale**’ (default 10).

### **Manually Changing Variable Types**

Types can be changed by clicking the variable type icon at most places in JASP. To do this for multiple variables at once, mark multiple columns in data edit view and change type once.

JASP will attempt to convert the variable to the selected type whenever possible. For example, if a variable is changed to **Scale** but contains non-numeric values, those values will be converted to missing values.

In some situations, variable types are enforced by an analysis or computed column. For example, a t-test requires the dependent variable to be treated as **Scale**. When JASP temporarily changes a variable type in this way, an asterisk (`*`) appears next to the type icon to indicate that the type has been overridden.

### **Computed Columns**

If a column was created using the Computed Column (+) formula builder, its editing behaviour depends on the variable type:

* **Scale Computed Columns:** The formula calculates the underlying numerical **values**. You may freely edit or add labels, but the calculated values themselves cannot be edited manually.  
* **Nominal / Ordinal Computed Columns:** The formula determines the **labels**. Therefore, you may edit the underlying numerical values, but the labels themselves are controlled by the computation and cannot be edited directly.

### **Level Manipulation & Ordering**

You can precisely control and order your data levels:

* **Use labels:** When working with massive datasets (tens of thousands of rows), rendering custom text labels can slow down performance. Toggling this off forces JASP to display raw numbers instead, significantly speeding up the interface. The **Use labels** toggle is disabled by default when importing data that contains no labels, or when creating a dataset from scratch. Enable it if you want to create, view, change, or sort labels.
* **Eye button:** Closed - Drops levels you have filtered or newly created from your result tables and graphs. Open - Includes filtered and newly created levels:
* **Automatically order labels by their value** *(1 to N arrow down button)*: Forces the labels to align strictly in ascending order based on their underlying numeric values.
* **Reverse order of all numerical values** *(1 to N circular arrows button)*: Inverts the numeric values assigned to your labels. (e.g., turns a scale of 1 \= Low, 3 \= High into 3 \= Low, 1 \= High).
* **Reverse order of all labels** *(Up/Down arrow button)*: Flips the text label order. *Note: If automatic ordering is enabled, it will be disabled automatically when this option is used.*
* **Move labels up manually** *(Up arrow button)*: Moves the selected label one position higher in the list. *(Turns off automatic ordering).*
* **Move labels down manually** *(Down arrow button)*: Moves the selected label one position lower in the list. *(Turns off automatic ordering).*
* **Reset all filter checkmarks** *(Erasor button)*: Only visible if you have filtered data in this column. Removes all filters from this variable if you have set them with the checkmark toggle.

💡 **Statistical Tip (Changing the Baseline):** The order of your levels matters deeply for advanced statistics like Regression or GLM. JASP uses the **very top level** in this list as the reference/baseline group for dummy coding. Dragging or moving a level to the top changes your model's statistical baseline\!

### **Filter, Value, Label, Remove, Add new level**

* **Filter (Check / Uncheck):** In this column, unchecking a level temporarily excludes all cases with that value from analyses. This provides a quick way to include or exclude groups without modifying the dataset itself.
* **Value:** This column shows a database of all different values within your raw data. The raw data and thus the values are used to compute the statistics, tables and graphs for the analyses you choose. If you change a value this replaces all values within that column with the new one you have set. This essentially is a "find and replace" functionality. You probably want to make a backup of your data (e.g. by copy pasting the column to a backup column next to it) befory you manipulate your data this way.
* **Label:** This column shows labels of your data values. They are used to make the axis of your graphs and also your tables more easily readable. They do not have an influence on the actual computations that produce your statistical results.
* **Remove (X / Delete):** Removes the selected value from this variable only and converts it to a missing value. All other data for that participant or case remain unchanged.
* **Add New Level (+):** Creates a category that does not currently appear in the data.
  * *Use Case:* Suppose a survey uses a response scale from 1 (*Strongly Disagree*) to 5 (*Strongly Agree*), but nobody selected 5 in the current sample. Because that level does not occur in the data, JASP will not display it automatically. Adding the level manually ensures that plots and summary tables show the complete intended response scale.
)for_c++_include");
}

