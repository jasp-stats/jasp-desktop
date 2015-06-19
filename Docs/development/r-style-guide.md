
JASP R Style-guide
==================
This guide describes good practice and good approaches to writing analyses in R for JASP (and probably in general too).

Good Practice
-------------

1. do not use global variables

2. functions from outside the JASP package must always be called with their full namespace, like these:
    * `stats::anova()`
    * `base::options()`
    
3. do not use:
    * `library()`
    * `require()`
    * `exists()`
    * `attach()`
    * `assign()`
    * `<<-`

4. Write code to be readable. It is common in the R programming community to do many things in a single line of code. This is either because it is less lines of code to write, or because it improves performance (or because it looks clever). In general it is better to write code which is more verbose but easier to read. for-loops, for example, can be much easier to read than a `lapply()`. Best practice is to only optimise code once performance has been demonstrated to be a problem.


Style
-----

All R code for JASP should follow the [google style guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).

However a couple of exceptions should be observed

1. function names that aren't the name of an analysis (i.e. those
privately used by the JASP R package) begin with a . and lowercase.
    * `NotLikeThis()`
    * `.butLikeThis()`

2. tabs are used rather than two spaces (you should set up your editor so that tabs appear four spaces wide)


Analysis Design
------------

The following is a general description or suggestion of good ways to write analyses for JASP. For simpler one or two table analyses, this may represent overkill, but for more complicated analyses this is a good way of approaching it.

The key features described here are:

1. neat encapsulation of related code into functions
2. neat transmission of errors between these functions

A well designed example of a more complicated JASP analysis is the ANOVA. It lives on github [here](https://github.com/jasp-stats/jasp-desktop/blob/development/JASP-Engine/JASP/R/anova.R).

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

