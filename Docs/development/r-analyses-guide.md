
JASP Guide For Implementing Analyses in R
=========================================

All analyses in JASP written in R should conform to [The JASP R style-guide](r-style-guide.md)


Function definition
-------------------

Analyses in JASP, when implemented in R, should be of the following form:

`AnalysisName <- function(dataset, options, perform="run", callback=function(...) 0, ...) {`

* `dataset` : will always be `NULL` in JASP. `dataset` is used when an analysis is run outside of JASP. Inside JASP you should read the dataset with the included functions (details below)
* `options` : a list containing values corresponding to the state of each of the user interface elements in the analysis' user interface
* `perform` : will either be equal to `"init"` or `"run"`, for initializing and running the analysis respectively
* `callback` : a function to call periodically to notify JASP that the analysis is still running, and (not implemented yet) to provide progress updates (such as percentage complete). this function will return a non-zero value if the user has aborted the analysis, and your function should terminate in response to this.

The function should return a series of nested lists containing the results. This results object is described below.


Useful functions inside JASP
----------------------------

An assortment of useful functions are available to JASP R analyses. These are defined in the file *common.R*.

### Reading Data from JASP

In order for the analysis to read the data from JASP, one of two functions must be called:

`.readDataSetHeader(columns, columns.as.numeric, columns.as.ordered, columns.as.factor)`
`.readDataSetToEnd(columns, columns.as.numeric, columns.as.ordered, columns.as.factor)`

- `columns` : a vector of column names to be read
- `columns.as.numeric` : a vector of column names to be read as numeric (marshalled as necessary)
- `columns.as.ordered` : a vector of column names to be read as ordered factors (marshalled as necessary)
- `columns.as.nominal` : a vector of column names to be read as unordered factors (marshalled as necessary)

These functions return a data.frame containing the columns requested marshalled (if necessary) to the type requested.

`.readDataSetHeader()` returns a data.frame with no data (zero rows), and is intended for initialization of an analysis. `.readDataSetToEnd()` returns a data.frame containing all the rows for the requested columns.

The names of the columns in these data.frames are encoded with *X-prepended-base64-encoding*. This is so that special characters can be supported. These names can be converted back and forth using the `.v` and `.unv` functions (below)

The beginning of an analysis function will typically looks as follows:

    if (is.null(dataset)) {
    
        if (perform == "run") {
        
            dataset <- .readDataSetToEnd(columns.as.numeric=...,)
            
        } else {
        
            dataset <- .readDataSetHeader(columns.as.numeric=...,)
        }
        
    } else {
    
        dataset <- .vdf(dataset, columns.as.numeric=...,)
    }

### Converting to and from *X-prepended-base64*

Column names in data frames read from JASP are encoded in *X-prepended-base64*. This allows us to use international characters in column names, etc.

`.v(column.names)`  
`.unv(dp.base64.names)`  
`.vf(formulas)`  
`.unvf(formulas)`

`.vdf(dataset, columns, columns.as.numeric, columns.as.ordered, columns.as.factor)`

`.v` returns a vector of column names converted to *X-prepended-base64-encoding*

`.unv` returns a vector of normal column names converted from *X-prepended-base64-encoding*

`.vf` translates formulas to *X-prepended-base64-encoding*

`.unvf` reverts formulas from *X-prepended-base64-encoding*

`.vdf` transforms the column names of a dataframe to be *X-prepended-base64-encoding*, other arguments are the same as
`.readDataSetToEnd()`

for example:

`.v("fred")`
returns
`"XZnJlZA"`

`.unv("XZnJlZA")`
returns
`"fred"`

`.unvf("XaXE ~ XZ2VuZGVy + Xc2Vz + XZ2VuZGVy:Xc2Vz")`
returns
`"iq ~ gender + ses + gender:ses"`

It is recommended to **only** use `.vf()` for display purposes. Using it to create formulas for models in R code will fail if the variable name contains colons. To create formulas, the variable names should be translated first using the `.v()` function, and then assembled into the formula string.


### Saving images

JASP accepts images as SVGs encoded as base64 data URIs. The following functions are useful for creating images in this format.

`.beginSaveImage(width, height)`  
`.endSaveImage(descriptor)`

`.beginSaveImage()` starts the image capturing process, and returns a descriptor. Once the analysis has performed all the rendering it needs, it should call `.endSaveImage()`, passing in the descriptor provided by `.beginSaveImage()`. `.endSaveImage()` returns a descriptor which can be assigned to the image object described below.


### Cleaning data

`.clean(value)`

`.clean()` sanitizes a value. It performs the following conversions:
* NaN is converted to "NaN"
* Inf and -Inf values are converted to the appropriate unicode symbols
* NULL values are converted to empty strings

### Footnotes

These functions simplify the creation of footnotes by automatically keeping track of what footnotes have already been created.

`.newFootnotes()`
`.addFootnote(footnotes, text, symbol=NULL)`
`as.list(footnotes)`

`.newFootnotes()` creates a new footnotes object

`.addFootnote(footnotes, text, symbol)`

* `footnotes` : a footnotes object created with `.newFootnotes()`
* `text` : the text of the footnote
* `symbol` : optional, the symbol to use. If omitted, a superscript letter is automatically used.

Adds a new footnote to the footnote object. If a footnote with matching text already exists, a duplicate footnote is *not* created (which is what you want). This function returns the index for the created (or existing) footnote, which can be placed in the `.footnotes` object of a data row (described below).

`as.list(footnotes)`

Converts a footnotes object created with `.newFootnotes()` to a list conforming to the `footnotes` component of `table` described below.


Initialization and Running
-----------------------------

When selected, an analysis will be called twice (at least), once to initialize it and once to run it; in each case, the `perform` argument will be equal to `"init"` and `"run"` respectively.

The purpose of the initialization is to provide an empty set of results, more or less instantly, that will be subsequently populated by the analysis. This immediate feedback provides for a nice responsive user experience.

**All** tables and images that will make up the final results should have empty images and tables created at initialization.

Initialization should happen very quickly, and use as few resources as possible. For example, a contingency table analysis of *Gender* and *Smoking Status* might create the following empty table:

<table>
	<tr>
		<td></td><td>Male</td><td>Female</td>
	</tr>
	<tr>
		<td>Smoker</td><td>.</td><td>.</td>
	</tr>
	<tr>
		<td>Non-smoker</td><td>.</td><td>.</td>
	</tr>
</table>

The creation of this table does not require reading the data from JASP at all; however it does require information on what levels the *Gender* and *Smoking Status* columns contain. To access this, `.readDataSetHeader()` will provide a data.frame containing columns (of zero length) that can be queried for what levels they contain.

In place of statistics that will be subsequently created by the analysis, a `"."` is returned. This indicates to the user that the results are yet to be calculated.

(It can be difficult to test the initialization code, because if the analysis proceeds very quickly, the empty tables/images will be replaced very quickly with the proper results, not allowing enough time for inspection. As a temporary measure, it can sometimes be convenient to override the value of `perform` to always be equal to `"init"` at the top of the analysis function.)


Results
-------

In the following section, R lists will be represented in JSON notation. In this format, named lists are represented with

    { "name" : "value", "name 2" : "value 2" }

and unnamed lists are represented with

    [ "valueWithouName", "valueWithoutName 2" ]

. JSON is a particularly good format for representing nested/hierachical structures, which is why we make use of it.

The analysis function should return a results bundle and must be a named list.

An example of a results bundle might be:

    {
        "results" : { ... },
        "status" : "the status",
        "state" : { ... }
        "keep" : [ ... ]
    }

- `results` : a results object, descriped below
- `status` : the status, this can be either `"inited"` (typically to be returned when `perform == "init"`) or `"complete"` (typically returned when `perform == "run"`)
- `state`  : arbitrary data that can be retrieved in a subsequent call of this analysis with a call to `.retrieveState()`
- `keep` : a list of file descriptors (from `.endSaveImage()`). This instructs the temporary file system to keep these files, and not delete them.

### Results

    {
        ".meta" : [
            { "name" : "descriptives", "type" : "table" },
            { "name" : "plot",         "type" : "image" },
        ],
        
        "descriptives" : { ... },
        "plot"         : { ... }
    }
    
These results contains two items; *descriptives* and *plot*. The *.meta* entry lists these two items, specifies the order in which they should appear, and specifies the type of each. The *descriptives* object is a table, and the *plot* object is an image.


### Table

A table object itself is a named list of the form:

    {
        "title"  : "The Table's Title",
        "schema" : { ... },
        "data"   : [ ... ],
        "footnotes" : [ ... ],
        "casesAcrossColumns" : false,
        "error" : { ... }
    }
    
- `title` : the title which appears at the top of the table
- `schema` : specifies the columns of the table
- `data`  : specifies the data, or rows of the table
- `footnotes` : optional, footnotes to appear at the bottom of the table
- `casesAcrossColumns` : optional, defaults to false, whether the rows and columns of the table should be swapped
- `error` : optional, specifies an error message to be displayed over the top of the table


#### schema

Taken from here: http://dataprotocols.org/json-table-schema/

`schema` is of the following form:

    {
        "fields" : [
            {
                "name"   : "column name",
                "title"  : "displayed column title",
                "type"   : "number",
                "format" : "dp:3"
            },
            {
                "name"   : "next column name",
                ...
            }
        ]
    }

- `name` : the column name (note the use of [ ] in the name invokes column folding, see below)
- `title` : optional, displayed at the top of the column; if not specified the column name is used
- `type` : one of `"string"`, `"number"`, `"integer"`
- `format` : format specifiers (multiple can be specified, separated with semicolons)
    - `dp:X` - format to X decimal places
    - `sf:X` - format to X significant figures
    - `p:X`  - if the value is less than X, substitute `p < X` in it's place (`p:.001` is common)
    - `pc`   - format the number as a percentage (multiply it by 100, and add a % sign) (does not work in conjunction with sf)

##### Column folding
Column folding is where multiple columns are folded into one. This can be done for a number of reasons, but the most common is because a single column requires heterogeneous formatting. For example, the correlation table has a column which contains an r-value, a p-value directly underneath, then another r-value, etc.; these require different formatting. To achieve this, two separate columns are created for r-value and p-value, each with their own formatting, but with special names which instruct the table renderer to combine or fold these columns into one. In the case of the r-value and the p-value, the column names:

- `value[pValue]`
- `value[rValue]`

might be chosen. The table renderer matches the name before the `[`, and knows to combine or fold these columns into one.
    
#### data

`data` represents the rows in the table and is of the form:

    [
        {
            "column 1 name" : "row 1 column 1 value",
            "column 2 name" : 15.44444,
            ".footnotes" : [ ... ]
            ".isMainRow" : false
            ...
        },
        {
            "column 1 name" : "row 2 column 1 value",
            "column 2 name" : 354.3333333333,
            ...
        },
        ...
    ]
    
The column names must correspond to those specified in the schema

#### .footnotes (in data)

It is recommended to use the footnotes functions described above to generate the indices.

`.footnotes` in row data describes the symbols (typically superscripts) which are displayed beside values

    [
        "column 1 name" : [ 0, 1 ]
        "column 2 name" : [ 0 ]
    ]

The arrays of values are indices which refer to the footnotes object in `table` (see below). The symbols are taken from there.

#### footnotes (in table)

It is recommended to use the footnotes functions described above, rather than creating these objects manually. These functions make it much easier.

`footnotes` is of the form:

    [
    	{
        	"symbol" : 0,
	        "text"   : "Footnote a text"
    	},
    	{
        	"symbol" : 1,
	        "text"   : "Footnote b text"
    	},
    	...
    ]
    
- `symbol` : can be either an integer, or a string. Integers correspond to superscripts, 0 is <sup>a</sup>, 1 is <sup>b</sup>, 2 is <sup>d</sup>, etc. (there's no <sup>c</sup> which is peculiar). If a string is specified, it used as the symbol itself.
- `text`   : the text of the footnote

#### error

`error` is of the form:

    {
        "errorType" : "badData",
        "errorMessage" : "The error message"
    }

- `errorType` : can be whatever. In the future a set of error types will be developed.
- `errorMessage` : optional, the message to be displayed. If an analysis produces multiple tables, it is generally best to only put the error message over the top table.


### image

`image` is of the form:

    {
        "title"  : "The images title",
        "width"  : 640,
        "height" : 480,
        "data"   : " ... "
    }
    
* `title` : The title of the image
* `width` : the width of the image
* `height`: the height of the image
* `data`  : the image, returned by `.endSaveImage()`

`data` is most easily produced with the functions:
    - `.beginSaveImage()`
    - `.endSaveImage()`


Callbacks
---------

Callbacks allow analyses to:

1. Provide partial results back to the UI
2. Respond to user actions made while the analysis is running

A callback function is passed as an argument into the analysis function. i.e.

`TTestOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) list(status="ok"), ...) {`

### Providing partial results

Providing partial results is useful, because it allows for analyses to progressively fill tables in as the results are calculated (rather than filling them all in in one go). At some point, we'll also have a progress bar.

To send partical results, the results list is simply passed into the callback:

`callback(results)`

Even if results haven't changed, it is good to call the callback periodically, as this allows you to respond to user actions (see below). In this case, you can simply call the callback with no arguments.

### Responding to user actions

During the analysis, the analysis can (and should) periodically call the callback to see if the user has changed their mind. If things have changed, this will be reflected in the *return value* of the callback. the callback will return something like:

    {
        "status" : ...
    }

where `status` can be:

- "ok",
- "aborted"
- "changed"
- "stopped"

`ok` indicates that the analysis has not changed, and should continue running
`aborted` indicates that the analysis has been removed by the user (and there's no point continuing), the analysis can call just call `return()` to terminate itself
`changed` indicates that the analysis has been changed by the user, more information below.
`stopped` indicates that the analysis has been stopped by the user (for analyses which are not *autorun* there is a button to stop them, but few analyses aren't autorun, so you can ignore the possibility if receiving "stopped"). "stopped" is for all intents and purposes the same as "aborted", however, if pass partial results into your call to `return()`, these will be displayed.

#### "changed"

When the status is "changed", there will also be an options object present in the callbacks return value; i.e. the return value will be:

    {
        "status" : "changed",
        "options" : {
            ....
        }
    }

The `options` object will contain the new options that the user has selected. Changes to options can have one of two consequences:

1. that the analysis can incorporate the changes and continue to run
2. that the analysis can not incorporate the changes, and must re-run from the beginning

In the instance of 1. (that the analysis can incorporate the changes), the analysis continues to run, and returns a results list appropriate for the new options. If the analysis returns a results list, then JASP assumes that the analysis was able to incorporate the changes.

In the instance of 2. (that the analysis is unable to incorporate the changes, and must re-run from the beginning), the analysis can simply return NULL (i.e. by calling `return()`). If the analysis returns NULL, then JASP assumes that the analysis was unable to incorporate the changes, and will re-run the analysis with the new options.

Generally, only changes to certain settings require the analysis to restart. To determine whether a restart is in order, the `.diff()` function can be useful.

`.diff(options.one, options.two)`

It will return a named list with TRUE/FALSE values indicating which options have changed or not. So, for example, if changes to the options `variables` or `limit` would require a restart, the code might look as follows:

    response <- callback()
    
    if (response$status == "aborted")
	    return()
	    
    if (response$status == "changed") {
    
        changes <- .diff(options, response$options)
        
        if (changes$variables || changes$limit)
            return()
    }
    
    # otherwise continue

### Error handling

#### Checking for errors

There are situations where you know an analysis cannot be performed. For example if an independent samples t-test is run with a grouping variable that only has one level, or when the data in a regression analysis has infinite values. To prevent an analysis from crashing we need to check the data and options beforehand. This can be done with the `.hasErrors()` function.

##### Calling the error check function

By finetuning the arguments given to `.hasErrors()` it can be used in a variety of situations. The main arguments are:

- `dataset`: JASP dataset. [required]
- `perform`: 'run' or 'init'. [required]
- `type`: character vector containing any of the following: 'infinity', 'factorLevels', 'variance', 'observations'. [required]
- `custom`: a (named/unnamed list of) function(s) that perform some check and return an error message. [optional]
- `message`: 'short' or 'default' [the default]. [optional]
- `exitAnalysisIfErrors`: TRUE or FALSE [the default]. [optional]

The optional argument `custom` may be used when a small, analysis-specific check has to be performed. In general, if a data check is still missing it should be added to `.hasErrors()` (so others may use it too; as described further below), however, it may be the case that the check is only appropriate for a single analysis. In this case it can be defined in the body of the analysis function and passed to `.hasErrors()`. Example of an object that may be supplied in `custom`:
```
customChecks <- list(
	function() {
		if (options$Data == 'varcov' && options$groupingVariable != '') {
			return('Multiple group analysis not (yet) supported when data is variance-covariance matrix')
		}
	},
	function() {
		if (options$Data == 'varcov' && isTRUE(options$includeMeanStructure)) {
			return('Mean structure can not be included when data is variance-covariance matrix')
		}
	})
```
Note that the functions are defined in the environment of the analysis and therefore have access to its variables. It is unlikely you will need to supply arguments, but if you wish to do so, make sure the functions are defined in a named list (e.g., list(meanstruct = function(arg1) {...}) where arg1 can be called by supplying meanstruct.arg1=... to `.hasErrors()`. See below for more details about supplying arguments to check functions).

The argument `message` is used to specify what sort of error message should be returned. When it is set to `short` (for use in footnotes) only the first error encountered will be included in the message. When it is set to `default` it includes all checks that fail and adds the opening statement "The following problem(s) occurred while running the analysis:". 

The argument `exitAnalysisIfErrors` can be used to prevent the analysis from continuing to run when a check fails. It would be sensible to set this to `TRUE` in the case of the independent samples t-test with the single level grouping variable. The t-test package will not be able to run and graphs or tables would be nonsensical. When it is set to `FALSE` the function will simply return to the calling environment regardless of whether any checks failed.

In addition to the main arguments, we can pass arguments to the checks listed after `type` (or to the functions in `custom`).
These arguments are always prefixed by their type name. So to check if the variance is zero in in the dependent variable 'dependentVar', we would call:

`.hasErrors(dataset=dataset, perform=perform, type='variance', variance.target='dependentVar')`

If we would also like to know if the grouping variable had exactly two factor levels and we wanted to exit if we found zero variance or something other than two levels:

```
.hasErrors(dataset=dataset, perform=perform, type=c('factorLevels', 'variance'), 
	factorLevels.target='groupingVar', factorLevels.amount='!= 2', 
	variance.target='dependentVar', 
	exitAnalysisIfErrors=TRUE)`
```

All check arguments:

| type         | argument      | description                                        |
|--------------|---------------|----------------------------------------------------|
| infinity     | target        | character vector of variable names                 |
|              | grouping      | character vector of grouping variable names        |
|              | groupingLevel | vector with the levels for each grouping variable  |
| factorLevels | target        | character vector of grouping variable names        |
|              | amount\*      | (vector of) string(s) (e.g. "!= 2")                |
| variance     | target        | character vector of variable names                 |
|              | grouping      | character vector of grouping variable names        |
|              | groupingLevel | vector with the levels for each grouping variable  |
|              | equalTo       | numeric value to compare for equality [default: 0] |
| observations | target        | character vector of variable names                 |
|              | grouping      | character vector of grouping variable names        |
|              | groupingLevel | vector with the levels for each grouping variable  |
|              | amount\*      | (vector of) string(s) (e.g. "> 5000")              |
\* = required argument

Note that when no target is provided to an error check, it will by default go over every variable in the dataset.

To prevent very long function calls, we can also prefix arguments by `all.` (e.g. instead of using observations.grouping = options$fixedFactor, variance.grouping = options$fixedFactor, etc. we use all.grouping=options$fixedFactor). When this prefix is used `.hasErrors()` will call each check with that specific value; granted a check actually uses a parameter with that name.

##### The return value

`.hasErrors()` will return a named list if any errors were encountered in the data (given `exitAnalysisIfErrors` is not `TRUE`). Each type of check that fails, will be included in the list as `checkName=varsThatFailed`, so in our previous example this would be `variance='dependentVar'`. In addition it will include a `message='...'` entry with the error message. If no errors were encountered it simply returns `FALSE`.

##### General use

There are two ways `.hasErrors()` can be used. Firstly, it may included at the start of the analysis if there are any 'dealbreakers'; problems with the data (or selected options) that would render further computations useless. The argument `exitAnalysisIfErrors` should be set to `TRUE` and for the remainder of the analysis 'dealbreakers' will have no further implications. 

Secondly, the function may be used when data errors only have a local effect. As an example, let's consider an independent samples t-test with multiple dependent variables. If only one of the dependent variables contains infinity, the t-test can still be performed on the other dependent variable. In this case `.hasErrors()` will have to be run multiple times with a different dependent as its target every time. If any errors are found the t-test for that dependent may be omitted and a footnote should be added (a footnote is generated by setting `message` to `short`). 

As an alternative to calling `.hasErrors()` multiple times on parts of the data, the function could be called just once for all variables. The named list that is returned - which details for each check which variables failed it - may then be queried. Note that this method is more complicated to implement, but would save computation time, as data checks are not repeated. The main complication comes from the fact that the relevant error must be distilled from the list and the error message must be manually generated through `.generateErrorMessage()`.

`.generateErrorMessage()` takes the arguments:

- `type`: single character string containing one of the `.hasErrors()` types. [required]
- `opening`: boolean, should the statement "The following problem(s) occurred while running the analysis:" be included (TRUE) or left out (FALSE) [the default]. [optional]
- `concatenate`: string with the error message it should be appended to. [optional]
- `grouping`: character vector of variables that were used to group the dependent variables on. [optional] 

`.generateErrorMessage()` expects the variables that are defined in the error messages to be supplied. All error messages can be found in commonmessages.R. Message variables are denoted by {{}}. If, for example, an error message contains {{levels}}, `.generateErrorMessage()` looks for the argument levels=... in its input.

#### Adding new error checks

It is possible you want to check something that is not implemented. To prevent everyone from reinventing the wheel, it should be added to `.hasErrors()`, so others may use it in the future. There are 3 steps to implementing a new check:

1. Write a function that can perform the check and place it at the bottom of the file `commonerrorcheck.R`. Try to make it as generic as possible so it could also be applied in other situations than your own. Some things to bear in mind:
  * Its name could, in principle, be whatever you like. But to be consistent and avoid masking, start with .check followed by some short statement in camelCase. 
  * It may take as many or as few arguments as you like. They can be optional or required, if they are optional they may be omitted in the call to `.hasErrors()`. Note that although `.hasErrors()` requires arguments to be prefixed, they do not need to be prefixed in the actual function definition.
  * It must return a named list with the entry `error`. If your check determines there is an error it should be set to `TRUE` and otherwise to `FALSE`. If your function performs a check on the data (rather than on options), it should also have the entry `errorVars` which contains the variable names that failed your check.
  An example:
  
  ```
  .checkImaginary <- function(dataset, target, someArg=NULL) {
  # This is a short description of my check.
  # args:
  #  dataset: JASP dataset.
  #  target: String vector indicating the target variables.
  #  someArg: Some description.
  
      result <- list(error=FALSE, errorVars=NULL)
      
      for (v in target) {
      
          *[some code]*
        
          if (imaginaryError) {
              result$error <- TRUE
              result$errorVars <- c(result$errorVars, v)
          }
        
      }
      
      return(result)
      
  }
  ```
2. Add a new entry to the `checks` list found at the top of `.hasErrors()`. The index value determines how your error check can be called from an analysis. The new entry should have a named list with the slots `callback` and `addGroupingMsg`. `callback` should be assigned the name of the function you just created. `addGroupingMsg` clarifies if your check allows grouping; this option adds the line "after grouping on {{grouping}}" when set to `TRUE` and grouping variables are found.

3. Create a message in `.messages()`, this function can be found in `commonmessages.R`. Variables must be put between {{}}. These will be automatically parsed in `.generateErrorMessage()`.
