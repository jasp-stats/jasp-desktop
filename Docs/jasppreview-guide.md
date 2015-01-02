Guide to JASPPreview
==================
The current version of JASPPreview is 0.5-1 and is available [here](https://static.jasp-stats.org/development/JASPPreview_0.5-1.tar.gz). It depends on [RJSONIO](http://cran.r-project.org/web/packages/RJSONIO/index.html), so you should install that first.

JASPPreview allows you to run code, and see what results will look like in JASP, without having to run the code in JASP itself. This allows people to work from an interactive R session, and see their results without having to go through the whole process of rebuilding and rerunning JASP.

JASPPreview provides the following functions:

- `prepare()`
- `preview(results)`

###prepare

`prepare` "source"s into your R environment, all the JASP specific functions that are available inside JASP ([documented here](r-analyses-guide.md)). This allows you to use these functions from within a normal R session.

Note that you will not be able to use `.readDataSetHeader()` or `.readDataSetToEnd()`, but JASP analyses are normally written that you can pass in a dataset anyway (which bypasses the calls to these functions).

###preview

`preview` takes a results list (what is returned from a JASP R analysis function, as [documented here](r-analyses-guide.md)), and opens a web browser showing what the results would look like in JASP.

##Example

The following is an example of typical usage of JASPPreview. First modifications are made to the analysis function, in this case Anova, and the function is sourced.
The function is then run with appropriate data, and the results object is produced. This results object is then passed to the preview function, which displays the tables as they would appear in JASP, in a web browser.

    library("JASPPreview")
    
    JASPPreview::prepare()
    
    data  <- read.table("~/Documents/ssgo-fred.csv", sep=",", header=TRUE)
    
    options <- list(
      contrasts = list(list(contrast="deviation", variable="trialType")),
      dependent = "rt",
      fixedFactors = list("trialType"),
      modelTerms = list(
        list(components = list("trialType"))
        ),
      sumOfSquares="type3",
      misc=list(effectSizeEstimates=FALSE)
    )
    
    results <- Anova(data, options)
    
    JASPPreview::preview(results)


