
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

The names of the columns in these data.frames are encoded with *dot-prepended-base64-encoding*. This is so that special characters can be supported. These names can be converted back and forth using the `.v` and `.unv` functions (below)

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

### Converting to and from *dot-prepended-base64*

Column names in data frames read from JASP are encoded in *dot-prepended-base64*

`.v(column.names)`  
`.unv(dp.base64.names)`  
`.vf(formulas)`  
`.unvf(formulas)`

`.vdf(dataset, columns, columns.as.numeric, columns.as.ordered, columns.as.factor)`

`.v` returns a vector of column names converted to *dot-prepended-base64-encoding*

`.unv` returns a vector of normal column names converted from *dot-prepended-base64-encoding*

`.vf` translates formulas to *dot-prepended-base64-encoding*

`.unvf` reverts formulas from *dot-prepended-base64-encoding*

`.vdf` transforms the column names of a dataframe to be *dot-prepended-base64-encoding*, other arguments are the same as
`.readDataSetToEnd()`

for example:

`.v("fred")`
returns
`".ZnJlZA"`

`.unv(".ZnJlZA")`
returns
`"fred"`

`.unvf(".aXE ~ .Z2VuZGVy + .c2Vz + .Z2VuZGVy:.c2Vz")`
returns
`"iq ~ gender + ses + gender:ses"`

It is recommended to **only** use `.vf()` for display purposes. Using it to create formulas for models in R code will fail if the variable name contains colons. To create formulas, the variable names should be translated first using the `.v()` function, and then assembled into the formula string.


### Saving images

JASP accepts images as SVGs encoded as base64 data URIs. The following functions are useful for creating images in this format.

`.beginSaveImage(width, height)`  
`.endSaveImage(descriptor)`

`.beginSaveImage()` starts the image capturing process, and returns a descriptor. Once the analysis has performed all the rendering it needs, it should call `.endSaveImage()`, passing in the descriptor provided by `.beginSaveImage()`. `.endSaveImage()` returns a base64 encoded data URI which can be assigned to the image object described below.


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

*In the following section, R lists will be represented in JSON notation. In this format, named lists are represented with `{ "name" : "value", "name 2" : "value 2" }`, and unnamed lists are represented with `[ "valueWithouName", "valueWithoutName 2" ]`. JSON is a particularly good format for representing nested/hierachical structures, which is why we make use of it.*

The analysis function should return the results from the analysis, and must be a named list.

An example of a results list might be:

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
        "data"   : "data:image/svg+xml;base64,PD94bWwgdm ... "
    }
    
* `title` : The title of the image
* `width` : the width of the image
* `height`: the height of the image
* `data`  : base64 encoded data URI of an SVG image

`data` is most easily produced with the functions:
    - `.beginImageSave()`
    - `.endImageSave()`


