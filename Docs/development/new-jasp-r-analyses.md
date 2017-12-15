# Guide to rewriting R analyses

Previously, R analyses were designed to provide output that was as closely to JSON as possible.  
This meant that tables were represented as a nested list of rows, where each cell had the column name.  
Plots were also represented as a list, containing the saved png file and its width and height.  
To the R programmer these concepts are not sensible, as we have native structures for tables (data.frames) and plots (recordedplots, ggplot2 objects).  

In addition to having complicated output types, the R code was further complicated by the fact that its output needed a special description.  
This meant that on the one hand programmers had to tell the engine which elements were tables, which were plots, etc.  
and on the other how the tables should be parsed (e.g., is a column numeric? integer?).  

The changes to the code flow relate to the above issues.  
The ultimate goal is to create R-like output and separate description from code.  

This guide will detail the changes and show how they can be incorporated.  
Note that this guide assumes a lot of prior knowledge of how to program in JASP.  
It should be made much more friendly to new programmers still.

## Analysis description file
Up to this point it was only necessary to create a description file for the input of an analysis.  
This description file (JSON, but will become TOML; syntax is given for both) contains all the options that the analysis needs to run.  
A partial excerpt of the Binomial Test:

```
{
	"name": "BinomialTest",
	"options": [
		{
			"name": "variables",
			"type": "Variables"
		},
		{
			"format": "",
			"name": "testValue",
			"type": "Number",
			"value": 0.5,
			"max": 1,
			"min": 0
		},
		{
			"name": "hypothesis",
			"options": ["notEqualToTestValue", "greaterThanTestValue", "lessThanTestValue"],
			"default": "notEqualToTestValue",
			"type": "List"
		},
    ...
```

Up to this point, the description file was only tasked with the input into an analysis, despite the fact that the R code also provides output which must be described.  
Instead, the output description was included inline in the R code. Some examples:

```
descriptivesPlotsMeta <- list()

for (j in .indices(variables)){
  descriptivesPlotsMeta[[j]] <- list(name = variables[j], type = "collection",
                                     meta = "image")
}

results[["title"]] <- "Binomial Test"
results[[".meta"]] <- list(list(name = "binomial", type = "table"),
                           list(name = "descriptives", type = "object",
                                meta = descriptivesPlotsMeta))
```

```
fields <- list(
  list(name="case", title="", type="string", combine=TRUE),
  list(name="level", title="Level", type="string"),
  list(name="counts", title="Counts", type="integer"),
  ...
```

One of the largest changes is that this description is now included in the same description file.  
Five new types (`title`, `dataset`, `results`, `init` and `state`) were added to the description file in addition to `options` and `name`.  
These will be exemplified using the Binomial Test.

#### title
Simply the title of the analysis that must be displayed above the analysis results.  
For the Binomial Test this would be:

```
<- JSON syntax ->

  ...
	"title": "Binomial Test",
  ...
```
```
<- TOML syntax ->

title = "Binomial Test"
```

#### dataset
Instructions for the reading of the dataset that is then supplied to R. If no dataset is required simply omit the field.  
This replaces the inline `.readDataSetToEnd` and `.readDataSetHeader`.  
There are five options:

- `numeric`: all `options` that must be read as numeric.
- `factor`: all `options` that must be read as factors.
- `ordinal`: all `options` that must be read as ordered factors.
- `auto`: all `options` that must be read as the type they currently have in JASP Desktop.
- `excludeNA`: all `options` that receive listwise exclusion of `NA` values.

The same condition applies to all the above: data is only read when a UI option is actually populated by the user.

For the Binomial Test:
```
<- JSON syntax ->

  ...
  "dataset": {
    "factor": "variables"
  },
  ...
```
```
<- TOML syntax ->

[dataset]
factor = "variables"
```

And another example, given we have five UI fields that allow variables to be read and they are all of different types:

```
<- JSON syntax ->

  ...
  "dataset": {
    "numeric": "variables1",
    "factor": ["variables2", "variables3"],
    "ordinal": "variables4",
    "auto": "variables5",
    "excludeNA": "variables1"
  },
  ...
```
```
<- TOML syntax ->

[dataset]
numeric = "variables1"
factor = ["variables2", "variables3"]
ordinal = "variables4"
auto = "variables5"
excludeNA = "variables1"
```

#### results
This field describes the results that can be expected to be returned from an analysis.  
The JavaScript uses this to create the HTML tables the user ultimately sees.  
There are three main `type`'s of output elements:
- table
- image
- container

##### table
Describes what a table looks like; a table should have the following properties:
- `title`: the title which appears at the top of the table
- `type`: set to `table`
- `show`: boolean specifying if the table should be shown (only has effect when [`init`](#init) is set to `false`)
- `columns`: description of each column in the table, the descriptions may contain any of the following (\* denotes required fields):
  - `name`\*: the column name
  - `title`\*: optional, displayed at the top of the column; if not specified the column name is used
  - `type`\*: one of `string`, `number` or `integer`
  - `format`: format specifiers for `type` is `number` (multiple can be specified, separated with semicolons)
      - `dp:X` - format to X decimal places
      - `sf:X` - format to X significant figures
      - `p:X`  - if the value is less than X, substitute `p < X` in it's place (`p:.001` is common)
      - `pc`   - format the number as a percentage (multiply it by 100, and add a % sign) (does not work in conjunction with sf)
  - `combine`: boolean specifying if cells should be merged if they contain the same value
  - `show`: boolean specifying if the column should be shown (only has effect when [`init`](#init) is set to `false`)

##### image
Describes what an image looks like; an image should have the following properties:
- `title`: the title which appears at the top of the image
- `type`: set to `image`
- `width`: the width of the image
- `height`: the height of the image
- `show`: boolean specifying if the image should be shown (only has effect when [`init`](#init) is set to `false`)

##### container
An array of `table`'s and/or `image`'s that are grouped together in the output.  
A container only has two properties (`show` is not supported as the way the items are grouped is unclear):
- `type`: set to `container`
- `items`: an array of output items which should be grouped together (the way [the items are grouped](#meta) is determined from the results list that is returned from the R analysis).

For the Binomial Test we find:
```
<- JSON syntax ->

...
"results": {
  "binomial": {
    "type": "table",
    "title": "Binomial Test",
    "columns": [
      {
        "name": "case",
        "title": "",
        "type": "string",
        "combine": true
      },
      {
        "name": "level",
        "title": "Level",
        "type": "string"
      },
      {
        "name": "counts",
        "title": "Counts",
        "type": "integer"
      },
      {
        "name": "total",
        "title": "Total",
        "type": "string"
      },
      {
        "name": "proportion",
        "title": "Proportion",
        "type": "number",
        "format": "sf:4;dp:3"
      },
      {
        "name": "p",
        "title": "p",
        "type": "number",
        "format": "dp:3;p:.001"
      },
      {
        "name": "VovkSellkeMPR",
        "title": "VS-MPR\u002A",
        "type": "number",
        "format": "sf:4;dp:3"
      },
      {
        "name": "lowerCI",
        "title": "Lower",
        "type": "number",
        "format": "sf:4;dp:3"
      },
      {
        "name": "upperCI",
        "title": "Upper",
        "type": "number",
        "format": "sf:4;dp:3"
      }
    ]
  },
  "descriptives": {
    "type": "container",
    "items": {
      "descriptivesPlot": {
        "type": "image",
        "width": 160,
        "height": 300
      }
    }
  }
},
...
```

```
<- TOML syntax ->

[results]

[results.binomial]
title = "Binomial Test"
type = "table"

[[results.binomial.columns]]
combine = true
name = "case"
title = ""
type = "string"

[[results.binomial.columns]]
name = "level"
title = "Level"
type = "string"

[[results.binomial.columns]]
name = "counts"
title = "Counts"
type = "integer"

[[results.binomial.columns]]
name = "total"
title = "Total"
type = "string"

[[results.binomial.columns]]
format = "sf:4;dp:3"
name = "proportion"
title = "Proportion"
type = "number"

[[results.binomial.columns]]
format = "dp:3;p:.001"
name = "p"
title = "p"
type = "number"

[[results.binomial.columns]]
format = "sf:4;dp:3"
name = "VovkSellkeMPR"
title = "VS-MPR*"
type = "number"

[[results.binomial.columns]]
format = "sf:4;dp:3"
name = "lowerCI"
overTitle = "{{percent}}% Confidence Interval"
title = "Lower"
type = "number"

[[results.binomial.columns]]
format = "sf:4;dp:3"
name = "upperCI"
overTitle = "{{percent}}% Confidence Interval"
title = "Upper"
type = "number"

[results.descriptives]
type = "container"

[results.descriptives.items]

[results.descriptives.items.descriptivesPlot]
height = 300
type = "image"
width = 160
```


#### init
Boolean that specifies whether the analysis utilises the init phase.  
If this is set to `false` then each output element must use `show` for it to be shown by default.

Now the Binomial Test has some complicated nesting in its output elements, so `init` is `true`:

```
<- JSON syntax ->

  ...
  "init": true,
  ...
```
```
<- TOML syntax ->

init = true
```

But for a simple analysis it could be set to `false`.  
If we re-use the syntax for the Binomial Test table, but only want to see the columns `case` and `level` by default:

```
<- JSON syntax ->

"init": false,
  ...
  "binomial": {
    "type": "table",
    "title": "Binomial Test",
		"show": true,
    "columns": [
      {
        "name": "case",
        "title": "",
        "type": "string",
        "combine": true,
        "show": true
      },
      {
        "name": "level",
        "title": "Level",
        "type": "string",
        "show": true
      },
      {
        "name": "counts",
        "title": "Counts",
        "type": "integer"
      },
      ...
```
```
<- TOML syntax ->

init = false
...
[results.binomial]
title = "Binomial Test"
type = "table"
show = true

[[results.binomial.columns]]
combine = true
name = "case"
title = ""
type = "string"
show = true

[[results.binomial.columns]]
name = "level"
title = "Level"
type = "string"
show = true

[[results.binomial.columns]]
name = "counts"
title = "Counts"
type = "integer"
...
```

#### state
Instructions for the parsing of the state RData file that is then supplied to R. If no state is required simply omit the field.  
This replaces the inline `.retrieveState` and `.diff`.

The basic premise of the state is that calculations should not needlessly be performed multiple times.  
Calculated the main fit object once? Store it in the state and specify the `options` that must not change for the object to be reusable.  
For the Binomial Test:

```
<- JSON syntax ->

  ...
  "state": {
    "binomResults": ["variables", "confidenceIntervalInterval", "hypothesis", "testValue"],
    "descriptPlots": ["variables", "descriptivesPlots", "descriptivesPlotsConfidenceInterval", "plotWidth", "plotHeight"]
  }
```
```
<- TOML syntax ->

[state]
binomResults = ["variables", "confidenceIntervalInterval", "hypothesis", "testValue"]
descriptPlots = ["variables", "descriptivesPlots", "descriptivesPlotsConfidenceInterval", "plotWidth", "plotHeight"]

```

Multiple objects in the state may depend on the same set of options. To prevent needless repetition there is one reserved field:
- `baseSets`: sets of placeholders that each have `options` which are added to the relevant state items

To exemplify the use of `baseSets`:

```
<- JSON syntax ->

...
"state": {
  "baseSets": {
    "mainModelOpts": ["opt1", "opt2", "opt3", "opt4", "opt5"],
    "mainPlotOpts": ["opts6", "opts7", "opts8"]
  },
  "modelFit": ["mainModelOpts", "opts9"],
  "analysisPlots": ["mainModelOpts", "mainPlotOpts"]
}
```
```
<- TOML syntax ->

[state]
modelFit = ["mainModelOpts", "opts9"]
analysisPlots = ["mainModelOpts", "mainPlotOpts"]

[state.baseSets]
mainModelOpts = ["opt1", "opt2", "opt3", "opt4", "opt5"]
mainPlotOpts = ["opts6", "opts7", "opts8"]
```

In the example `modelFit` depends on `opts1` until `opts5` and `opts9`; `analysisPlots` depends on `opts1` until `opts8`.

## Analysis R code
The analysis definition remains of the same form as previously, with the changed dataset and state:

`AnalysisName <- function(dataset, options, perform="run", callback=function(...) 0, state=NULL, ...) {`

* `dataset` : data.frame with base64 encoded column names, if `perform`=`"init"` then only the header, otherwise the full columns according to [the JSON (TOML)](#dataset)
* `options` : a list containing values corresponding to the state of each of the user interface elements in the analysis' user interface
* `perform` : will either be equal to `"init"` or `"run"`, for initializing and running the analysis respectively (may be omitted if [`init`](#init) is set to `false`)
* `callback` : a function to call periodically to notify JASP that the analysis is still running, and (not implemented yet) to provide progress updates (such as percentage complete). this function will return a non-zero value if the user has aborted the analysis, and your function should terminate in response to this
* `state` : a list containing reusable items stored at the end of the previous analysis (granted the analysis uses the state system, omit otherwise).

In practice this means that the following items can be removed at the beginning of the analysis:

1. Reading of the `dataset`
```
if (is.null(dataset)) {
  if (perform == "run") {
    dataset <- .readDataSetToEnd(columns.as.numeric=NULL,
                                 columns.as.factor=variables,
                                 exclude.na.listwise=NULL)
  } else {
    dataset <- .readDataSetHeader(columns.as.numeric=NULL,
                                  columns.as.factor=variables)
  }
} else {
  dataset <- .vdf(dataset, columns.as.numeric=NULL,
                  columns.as.factor=variables)
}
```

2. Retrieving the `state` and parsing it
```
state <- .retrieveState()
	descriptPlots <- NULL
	binomResults <- NULL

	if (!is.null(state)){

		diff <- .diff(state$options, options)

		if (is.list(diff) && !any(diff[["variables"]],
				diff[["confidenceIntervalInterval"]],
				diff[["hypothesis"]],
				diff[["testValue"]])){

			binomResults <- state$binomResults
		}

		if (is.list(diff) && options[["descriptivesPlots"]] &&
				!any(diff[["variables"]], diff[["testValue"]],
				diff[["descriptivesPlots"]],
				diff[["descriptivesPlotsConfidenceInterval"]],
				diff[["plotWidth"]],
				diff[["plotHeight"]])){
			descriptPlots <- state$descriptPlots
		}

	}
```

3. Creating [`.meta`](#meta) and adding an analysis `title`
```
descriptivesPlotsMeta <- list()

for (j in .indices(variables)){
  descriptivesPlotsMeta[[j]] <- list(name = variables[j], type = "collection",
                                     meta = "image")
}

results[["title"]] <- "Binomial Test"
results[[".meta"]] <- list(list(name = "binomial", type = "table"),
                           list(name = "descriptives", type = "object",
                                meta = descriptivesPlotsMeta))
```

4. Creating `keep`
```
plotPaths <- list()
for (i in 1:length(descriptPlots)){
  if (is.list(descriptPlots[[i]])){
    for (j in 1:length(descriptPlots[[i]]$collection)){
      plotPaths[[length(plotPaths)+1]] <-
        descriptPlots[[i]]$collection[[j]]$data
    }
  }
}
```

This also means that return value from the analysis should omit keep. The structure should be

```
return(list(results=results, status="inited", state=state))
```

if `perform`=`run` or if `init` is `false`, and otherwise

```
return(list(results=results, status="complete", state=state))
```

#### .meta
Note that in number 3 of [this section](#analysis-r-code) we stated that .meta no longer needs to be specified explicitly.  
Rather, it is deduced from the description file and the structure of the results list.  
This means two things:
###### 1. The results list should adhere to certain constraints imposed by JASP (which are the same they've always been).  
For simple result items of type `image` or `table` this is not a problem, e.g. for,

```
results <- list()
results[["binomial"]] <- data.frame(...)
```

the structure is always the same and the meta will be taken from the description file.  
However, results that are nested lists within lists (and thus should be specified as type `container` in the description file) are a bit different.  
The nesting in these containers can be of two types and may be used together: `collection` and `object`.  
- `collection`: array of repeated elements of a single type

A collection may be an array of type `table`

```
results[["resultName"]] <- list(
	collection=list(
		data.frame(...), 
		data.frame(...)
	),
	title="Table Collection Title"
)
```

or type `image`

```
results[["resultName"]] <- list(
	collection=list(
		ggplot2(...), 
		ggplot2(...)
	),
	title="Plot Collection Title"
)
```

or have an object (more about that below) to obtain deeper nesting
```
results[["resultName"]] <- list(
	collection=list(
		list(
			priorPosterior=ggplot2(...),
			robustness=ggplot2(...),
			sequential=ggplot2(...),
			title="Plot Sub-Collection Title 1"
		),
		list(
			priorPosterior=ggplot2(...),
			robustness=ggplot2(...),
			sequential=ggplot2(...),
			title="Plot Sub-Collection Title 2"
		)
	),
	title="Plot Collection Title"
)
```

- `object`: set of named elements that may be of different types (`image`, `table`, or have a `collection`)

Again, for a `table`:

```
results[["resultName"]] <- list(
	modelTable=data.frame(...),
	descriptivesTable=data.frame(...),
	title="Table Object Title"
)
```

or an `image`

```
results[["resultName"]] <- list(
	priorPosterior=ggplot2(...),
	robustness=ggplot2(...),
	title="Plot Object Title"
)
```

or a mixture

```
results[["resultName"]] <- list(
	modelTable=data.frame(...),
	priorPosterior=ggplot2(...),
	title="Mixture Object Title"
)
```

We can use a collection in an object

```
results[["resultName"]] <- list(
	descriptivesTable=data.frame(...),
	descriptivesPlots=list(
		collection=list(
			ggplot2(...),
			ggplot2(...)
		),
		title="Plot Collection Title"
	),
	title="Mixture Object Title"
)
```

###### 2. You cannot add partially completed results (for type `container`).

Only add entries to the results list when it has been fully computed.  
This is bad:

```
# don't do this:
results[["resultName"]] <- list(
	descriptivesTable=list()
)
```

The backend will not know how to parse this to a proper `.meta` object.

#### Tables

To create a table in JASP simply use a `(jasp.)data.frame` or `matrix` and place this in the results list.  
It is no longer necessary to use `.clean` to clean your results or make use of row lists.
Neither is it necessary to specify `.isNewGroup` in combination with `combine` from the [description file](#table).

##### jasp.data.frame
The jasp.data.frame is a normal data.frame with improved methods for a number of S3 generics (`rbind`, `cbind`, `[]`, `subset`).  
The fact that `tableResults` is a `jasp.data.frame` has the following consequences:

* A data.frame is never reduced to a vector when it only has a single column or row
* Column names are preserved when row binding to an empty `jasp.data.frame`
* Strings are never converted to factors and subsequently, you can `rbind` new 'levels' easily
* `rbind` supports a mixed vector and types will be coerced to numeric if possible
* Attributes that have names starting with `jasp.` are never lost (the various possible attributes are listed below)

To create an empty data.frame with column names simply add the `colnames` argument

```
tableResults <- jasp.data.frame(colnames=c("colName1", "colName2", "colName3"))
```

Now you can rbind results

```
tableResults <- rbind(tableResults, c("varName", 99, 99))
```

And cbind

```
tableResults <- cbind(tableResults, colName4=99)
```

And subset

```
tableResults <- tableResults[1, ]
```

##### Initing
Granted that `init` is set to `true` in the description file, then the analysis will have to both initialize and run.  
If it is initializing then use dots to fill the cells.  
As an example, suppose we created an empty data.frame, then it is sufficient to return the following
```
tableResults <- jasp.data.frame(colnames=c("colName1", "colName2", "colName3"))
tableResults <- rbind(tableResults, rep(".", ncol(tableResults)))
results[["tableName"]] <- tableResults
```

##### Errors
Previously, when we knew we could not compute a table, errors were stored in the table list under the name `error`.  
Now, given we've moved away from table lists, this method is no longer possible.  
Instead, this information has to be conveyed in the attributes of the data.frame

```
if (status$error == TRUE) {
	tableResults <- jasp.data.frame(colnames=c("colName1", "colName2", "colName3"))
	tableResults <- rbind(tableResults, rep(".", ncol(tableResults)))
	attr(tableResults, "jasp.error") <- status$errorMessage
}
```

##### Footnotes
Footnotes can be added as was the case previously. Only this time it is not possible to place it directly in the table rows.  

To create a general note below a table

```
footnotes <- .newFootnotes()
.addFootnote(footnotes, symbol="<em>Note.</em>", txt="Some interesting footnote")
attr(tableResults, "jasp.footnote") <- footnotes
```

To add it to the fifth row of column "col1"

```
footnotes <- .newFootnotes()
.addFootnote(footnotes, txt="Some interesting footnote", row=5, cols="col1")
attr(tableResults, "jasp.footnote") <- footnotes
```

To add it to all rows of columns "col1" and "col2"

```
footnotes <- .newFootnotes()
.addFootnote(footnotes, txt="Some interesting footnote", cols=c("col1", "col2"))
attr(tableResults, "jasp.footnote") <- footnotes
```

##### Custom schema
It is possible that there are dynamic properties for some columns (or columns themselves are dynamic).  
As an example, the Binomial Test uses an `overTitle` that specifies the width of the confidence interval (e.g., 95%).  
Currently, it is not possible to make properties change based on a variable.  
Instead, these properties must be overwritten in the `jasp.schema`

```
schema <- list(
	list(name="lowerCI", overTitle=paste0(options$confidenceIntervalInterval * 100, "% Confidence Interval")),
	list(name="upperCI", overTitle=paste0(options$confidenceIntervalInterval * 100, "% Confidence Interval"))
)
attr(tableResults, "jasp.schema") <- schema
```

As a result, the column titled "lowerCI" will get an additional property `overTitle` with a dynamic value.  
It is also possible to add an entirely new column, which works in an identical way

```
schema <- list(
	list(name="newCol", type="string", title="New Col Title")
)
attr(tableResults, "jasp.schema") <- schema
```

##### Table name in a container
There is one situation where the name of a table may have to be added directly to the data.frame.
This is the case when the table is part of a `container` and its name cannot be found in its parents (the lists in which it's nested).  
Suppose we have a `container` named `resultName` in our description file, which holds a table named `resultsTable`.  
If our results look as follows

```
results[["resultName"]] <- list(
	resultsPlot=ggplot2(...),
	resultsTables=list(
		collection=list(
			data.frame(...)
		),
		title="Table Collection Title"
	),
	title="Mixture Object Title"
)

```

then it is not possible to pair the data.frame(s) with a corresponding `table` in the description file.  
In this case we must add the name

```
table <- data.frame(...)
attr(table, "jasp.name") <- "resultsTable"

results[["resultName"]] <- list(
	resultsPlot=ggplot2(...),
	resultsTables=list(
		collection=list(
			table
		),
		title="Table Collection Title"
	),
	title="Mixture Object Title"
)

```

Note that if the name can be inferred from the (indirect) parents of the table then the attribute is not necessary.

##### Dynamic titles
Although it is possible to add dynamic titles through the [custom schema](#custom-schema), a dedicated attribute is available.  
This is to create consistency between dynamic titles of tables and plots. Plots do not have a schema attribute.  
Suppose we want the title of the table to include a variable name, then

```
tableResults <- jasp.data.frame(colnames=c("colName1", "colName2", "colName3"))
attr(tableResults, "jasp.title") <- paste("Results -", varName)
```

If `varName` holds `contBinom` then the title will show `Results - contBinom`.

#### Plots
To create a plot in JASP simply use `ggplot2` (`recordedplot` is supported but discouraged as this is not editable) and place the object in the results list.  
Plots are no longer lists  and it is no longer necessary to use `.writeImage` to create the actual .png file.

##### Initing
Granted that `init` is set to `true` in the description file, then the analysis will have to both initialize and run.  
If it is initializing then supply the function `plot.new` as your plot

```
results[["resultName"]] <- plot.new
```

`plot.new` has a special meaning in JASP and will be interpreted to mean that the plot should be blank and a loading indicator should be shown.  

Of course, it is also possible to create your own `ggplot2` code to make an empty plot.

##### Errors
Previously, when we knew we could not compute a plot, or if the plot code itself returned an error, then errors were stored in the plot list under the name `error`.  
Now, given we've moved away from plot lists, this method is no longer possible.  
Instead, this information has to be conveyed in the attributes of the plot object

```
if (status$error == TRUE) {
	plotResults <- plot.new
	attr(plotResults, "jasp.error") <- status$errorMessage
}
```

If it turns out that the `ggplot2` code itself generates an error, then an empty plot will be shown with the associated error message on top.

##### Footnotes
To add a general footnote at the bottom of a plot simply add it in the `jasp.footnote` attribute

```
footnotes <- .newFootnotes()
.addFootnote(footnotes, symbol="<em>Note.</em>", txt="Some interesting footnote")
attr(plotResults, "jasp.footnote") <- footnotes
```

##### Plot name in a container
There is one situation where the name of a plot may have to be added directly to the `ggplot2` object
This is the case when the plot is part of a `container` and its name cannot be found in its parents (the lists in which it's nested).  
Suppose we have a `container` named `resultName` in our description file, which holds a plot named `resultsPlot`.  
If our results look as follows

```
results[["resultName"]] <- list(
	resultsTable=data.frame(...),
	resultsPlots=list(
		collection=list(
			ggplot2(...)
		),
		title="Plot Collection Title"
	),
	title="Mixture Object Title"
)

```

then it is not possible to pair the plot(s) with a corresponding `image` in the description file.  
In this case we must add the name

```
plot <- ggplot2(...)
attr(plot, "jasp.name") <- "resultsPlot"

results[["resultName"]] <- list(
	resultsTable=data.frame(...),
	resultsPlots=list(
		collection=list(
			plot
		),
		title="Table Collection Title"
	),
	title="Mixture Object Title"
)

```

Note that if the name can be inferred from the (indirect) parents of the plot then the attribute is not necessary.

##### Dynamic titles
Suppose we want the title of the plot to include the level of a variable, then

```
plotResults <- ggplot2(...)
attr(plotResults, "jasp.title") <- paste("Level -", levelVar)
```

If `levelVar` holds `1` then the title will show `Level - 1`.

## Stepwise implementation of the changes
It is not necessary to change an existing analysis entirely in one go.  
You can change it bit by bit until finally all the tables are data.frames, plots are objects and the unnecessary overhead (`.meta`, `keep`, etc.) is gone.  
However, there are certain steps that require certain actions to be taken first.  
These actions will mostly involve adding stuff to the JSON (TOML).

- To move data reading outside the analysis the entry [`dataset`](#dataset) is required
- To move state parsing outside the analysis the entry [`state`](#state) is required
- To add a title the entries [`title`](#title) and [`results`](#results) are required
- To remove .meta from an analysis the entry [`results`](#results) is required
- To remove keep from an analysis the entries [`results`](#results) and [`state`](#state) are required
- To remove initing from an analysis the entries [`init`](#init) and [`results`](#results) are required
- Changing tables from row lists to data.frames
	- tables can be converted on a table-by-table basis, as long as the [`results`](#results) entry is added
	- schema and title can be removed even when it is still a row list
	- .isNewGroup and status cannot be removed when it is still a row list
	- footnotes and errors cannot be changed to attributes when it is still a row list
- Changing plots from lists to objects
	- plots can be converted on a plot-by-plot basis, as long as the [`results`](#results) entry is added
	- title/width/height/status/data/obj cannot be removed when it is still a list and not an object
	- footnotes and errors cannot be changed to attributes when it is still a list and not an object

## Open issues
- Problem with analyses that have partially filled in results. Analysis meta cannot be properly deduced from those (see [here](#2-you-cannot-add-partially-completed-results-for-type-container)).
- Footnotes might not have the full flexibility yet that is needed (e.g. how to specify same footnote for several different rows, but not all rows)
- Titles must still be specified in the results list for containers
- Cannot create empty init’s for container type
- Init phase must save output objects, so not every time empty tables with dots are shown, but rather the already calculated result
- No placeholders / variable types yet in the JSON (e.g., stuff that may make it easier to specify dynamic columns)
- No support for `casesAcrossColumns`
- We do not use the `version` field in the JSON (TOML), should probably use this for the analysis conversion
- Result parser for callback with partial results need to be added (the engine needs row lists)
- At the moment does not support 'all columns' in data reading
- Does not support array of titles/array of names in tables (BF10, BF01, LogBF10 should be an usable option for a single column)
- Exact constraints on nesting arrays (collections) and objects needs to be given in the guide