
JASP R Style-guide
==================
This guide describes good practice and good approaches to writing analyses in R for JASP (and probably in general too).

Good Practice
-------------

1. Do not use global variables, as this causes namespace problems.
2. functions from outside the JASP package must always be called with their full namespace, like these:
    * `stats::anova()`
    * `base::options()`
3. do not use:
    * `library()` (causes namespace problems)
    * `require()` (causes namespace problems)
    * `exists()`
    * `attach()` (causes namespace problems)
    * `assign()` (causes namespace problems)
    * `<<-` (This assigns to a global variable)
    * `append()` (This causes lists to have different columns with the same name)
4. Write code to be readable. It is common in the R programming community to do many things in a single line of code. This is either because it is less lines of code to write, or because it improves performance (or because it looks clever). Best practice is to only optimise code once performance has been demonstrated to be a problem. Start by writing code that is understandable. For instance, a for-loop is easier to read than an `lapply()`. When you do use an apply, please add a comment.
5. Do not mix functions for mark-up with functions that are used for the computations.
6. Try to save the output of the computations in a list and include the function call.
7. Use dividers to make your code more readable.
````
	## Data screening -----------------------------------------------
  # Tim's error handling
  .hasErrors(dataset=dataset, perform=perform, type=c('factorLevels', 'variance'),
    factorLevels.target='groupingVar', factorLevels.amount='!= 2',
    variance.target='dependentVar',
    exitAnalysisIfErrors=TRUE)`

  ## JSON mark-up initialisation  ----------------------------------------
  #
  result <- .createCorrelationMatrixTable(tableName=options$name)

  ## Retrieving data from state or compute -------------------------
  #
  outputTableElements <- .getOutputRowCorrelationMatrix(run=run, options=options,
                                                        state=state)

  ## Retrieving data from state or compute -------------------------
  #
  priorAndPosteriorPlot <- .getPriorAndPosteriorPlot(run=run, options=options,
                                                        state=state)

  ## State save ----------------------------------------------------
  #
  for (plot in plotsCorrelationMatrix) {
      keep <- c(keep, plot$data)
  }

  if (run) {
      status <- "complete"
      state <- list(options = options, bayesFactorObject = bayesFactorObject,
      		    rowsCorrelationMatrix = rowsCorrelationMatrix,
		    plotsCorrelationMatrix = plotsCorrelationMatrix, plotTypes = plotTypes)
  } else {
      status <- "inited"
  }
  
  return(list(results = results, status = status, state = state,keep = keep)
````
8. Remove trailing white space. In RStudio choose: Tools - Global options - Code - Saving and tick "Strip trailing horizontal whitespace when saving"
9. Suggest a reviewer for your code, after you submitted a pull request.


Style
-----
The JASP style guide is based on the [google style guide](https://google.github.io/styleguide/Rguide.xml).

1. General style: camelCasing
  - no underscores, no full stops, no hyphens between variables names and function names. Use verbs for functions and nouns for variables
    * bad: `this_variable.x`
    * GOOD: `firstVariable`
2. Defining functions:
  -  function names that aren't the name of an analysis (i.e. those privately used by the JASP R package) begin with a . and lowercase.
    * bad:
    ```
    correlationFunction()
    ```
    * GOOD:
    ```
    .getOutputRowCorrelationMatrix()
    ```     
  - Use descriptive names, longer variables names are okay.
    * bad:
    ```
    x <- 5
    y <- function(x){sqrt(x)}
    ```
    * GOOD:
    ```
    firstVariable <- 5
    .takeSquareRoot <- function(x){
        sqrt(x)
    }
    ```
  - Function definitions should first list arguments without default values, followed by those with default values. In both function definitions and function calls, multiple arguments per line are allowed; line breaks are only allowed between assignments.
    * bad:
    ```
    .bfPearsonCorrelation <- function(kappa=1, n, r, hyperGeoOverFlowThreshold=24, ciValue =
		      		      0.95) 
    ```
    * GOOD:
    ```
    .bfPearsonCorrelation <- function(n, r, kappa=1, hyperGeoOverFlowThreshold=24,
      				      ciValue = 0.95)
    ```     
3. Using functions:   
  - Use the full name space for functions outside of base.
    * bad: <br>
    ```
    library("utils")
    result <- modifyList(result, tempResult)
    ```
    * GOOD:
    ```
    # Please mention the names that are updated in result
    # betaA, betaB
    result <- utils::modifyList(x=result, val=tempResult)
    ```    
  - Pass arguments through a function by their argument name when possible.
    * bad:
    ```
    BayesFactor::ttest.tstat(7, 3)
    ```
    * GOOD:
    ```
    BayesFactor::ttest.tstat(t=7, n1=3)
    ```    
4. Function output:
  - Lists are R's work horses 
  - When calling a named column in a list always use the full name.
  - Don't merge lists using append as this can result in lists with the same names for different columns. Instead use `utils::modifyList`
5. Assignment:
  - use `<-`, not `=` (The "=" sign is reserved for arguments in a function call)
  - Don't use global assignment operator <<-
  - Don't use the right assignment operator `3 -> a`.
6. Semicolons:
  - Don't use them
    * bad:
    ```
    x <- 3; y <- 4; x <- y <- 6 ;
    ```
    * GOOD:
        ```
        x <- 3
        y <- 4
        ```
8. Indentations:
  - Use two spaces (Insert spaces for tab [tab width=2])
  
9. Spacing
  - Spaces: Between binary operators (e.g., `+`, `*`, `||`, `<-`, commas.
    * BAD:
     ```
     displayEstimate <-bfObject$x+bfObject$y
     correlationMatrix[1 ,2]
     correlationMatrix[1,]
     ```
    * GOOD:
     ```
     displayEstimate <- bfObject$x + bfObject$y
     correlationMatrix[1, 2]
     correlationMatrix[1, ]
     ```
10. Spacing Curly Braces:
  - first on same line, last on own line.
  - Surround else with braces
    * bad:
    ```
    if (is.null(r))
        return(NA)
    else if (is.na(r))  {return(NA)}
    else
    {
      result <- computeBf(r=r)
    }
    ```
    * GOOD:
      ```
      if (is.null(r)) {
          return(NA)
      } else if {
          return(NA)
      } else {
          result <- computeBf(r=r)
      }
      ```
11. Commenting Guidelines: all comments begin with # followed by a space; inline comments need four spaces before the #.
12. TODO Style: TODO(username)


Analysis Design
------------

The following is a general description or suggestion of good ways to write analyses for JASP. For simpler one or two table analyses, this may represent overkill, but for more complicated analyses this is a good way of approaching it.

The key features described here are:

1. neat encapsulation of related code into functions
2. neat transmission of errors between these functions

A well designed example of a more complicated JASP analysis is the ANOVA. It lives on github [here](https://github.com/jasp-stats/jasp-desktop/blob/development/Engine/JASP/R/anova.R).

We will begin with the `Anova()` function itself.

After the reading of the dataset, the analysis runs as follows:

	status <- .anovaCheck(dataset, options, perform)



	## Setup Contrasts

	if (perform == "run" && status$ready && status$error == FALSE)
		dataset <- .anovaSetupContrasts(dataset, options)



	## Perform ANOVA

	model <- NULL
	if (perform == "run" && status$ready && status$error == FALSE)
		model <- .anovaModel(dataset, options)



	## Create ANOVA Table

	result <- .anovaTable(dataset, options, perform, model, status)

	results[["anova"]] <- result$result
	status <- result$status



	## Create Contrasts Table

	result <- .anovaContrastsTable(dataset, options, perform, model, status)

	results[["contrasts"]] <- result$result
	status <- result$status



	## Create Post Hoc Table

	result <- .anovaPostHocTable(dataset, options, perform, status)

	results[["posthoc"]] <- result$result
	status <- result$status

This above code is fairly easy to read, and each section or part of the analysis is contained in it's own function. Now we will examine each part separately:

	status <- .anovaCheck(dataset, options, perform)

The .anovaCheck() function, checks that the analysis can run and returns a status object. The status object is a list with the fields `ready` and `error`. `ready` designates whether the analysis is ready to run or not. An example of an analysis not ready to run might be where the user has not specified a dependent variable, or has not specified any independent variables. Similarly, `error` indicates when the analysis can not run for an unexpected reason; for example, a specified independent variable may only have a single level. In this case, the status object returned by `.anovaCheck()` also contains an `errorMessage`. By reading the value of the status object, subsequent functions can be called or bypassed. This is demonstrated in the subsequent code:

	## Setup Contrasts

	if (perform == "run" && status$ready && status$error == FALSE)
		dataset <- .anovaSetupContrasts(dataset, options)


	## Perform ANOVA

	model <- NULL
	if (perform == "run" && status$ready && status$error == FALSE)
		model <- .anovaModel(dataset, options)

Finally this status object is passed into, and returned from the functions which generate the tables (The return values from these functions is a list with elements `result` (the table, which can be NULL if no table is required) and `status` (the status object):

	## Create ANOVA Table

	result <- .anovaTable(dataset, options, perform, model, status)

	results[["anova"]] <- result$result
	status <- result$status


	## Create Contrasts Table

	result <- .anovaContrastsTable(dataset, options, perform, model, status)

	results[["contrasts"]] <- result$result
	status <- result$status

In this way, the status object is passed from one function to the next. Earlier functions which encounter an error can notify later functions that an error has occurred, allowing them to adjust their behaviour accordingly. In this case, passing in a status object with either `error == TRUE` or `ready == FALSE`, causes the functions to not attempt to read values from the model,  but instead simply generate blank tables (like when `perform == "init"`).

This approach is good, because it provides both neat encapsulation of the different parts of the analysis, but still allows these encapsulated parts to communicate with one another in the case or errors.
