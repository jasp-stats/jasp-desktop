## How to Write R Analyses for JASP

R code for JASP should follow JASP's [R style guide](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/r-style-guide.md). 

This document will guide you through writing an R analysis for JASP. Two things should be noted before we get started. First, this guide assumes that you have knowledge about basic R concepts such as functions. Second, writing an R analysis is necessary but **not** sufficient to create a new module for JASP. For this goal, other files (such as the UI, json etc.) need to be created. The creation of these other files is not discussed here.

Every JASP R analysis will consist of several types of functions:
1. a single main analysis function that organizes the analysis and its output,
2. one or multiple results functions that compute the results that will be displayed,
3. one or multiple create functions that create output elements (tables, plots, containers), and
4. one or multiple fill up functions that fill up output elements with the results.

In the remainder of this document, you will learn how to write functions of all four types. Explanations will be illustrated using excerpts from a few JASP analysis, mainly the relatively simple Binomial Test.

### Step 1: Writing the Main Analysis Function

Each analysis in JASP needs a main analysis function. This main analysis function will provide an overview of all output elements and steps that are needed to conduct the full analyses.

#### Step 1.1: The Arguments

The main analysis function has the following arguments:
1: jaspResults which is the object that contains all results from this analysis and connects it to the output
2: dataset which is the dataset that is loaded in JASP (this may already be given or will be defined (i.e. read) in the main analysis function
3: options which is a list of options selected or deselected by the user
4: state which consists of the list of options from the last analysis (this determines whether results can be reused from that last analysis or need to be re-computed) and, optionally, a set of results that were saved in the state to be reused in the next run of the analysis

##### Example
```{r}
BinomialTest <- function(jaspResults, dataset, options, state = NULL) {
```

#### Step 1.2: Updating the Options

This is an optional step that can be used to add additional options that should be carried throughout the analysis. For example, in the Binomial Test analysis, several functions will need an hypothesis argument that can take the values "two.sided", "greater", or "less". However, the UI file does not allow a string value containing a "." (like in "two.sided"). Therefore, we can now create a new hypothesis variable that takes the corresponding string values. Note that this needs to be a new variable (i.e. we cannot just recode the hypothesis variable as this would screw up the state system that determines whether a table can be reused or needs to be newly computed.

##### Example
```{r}
if (options$hypothesis == "notEqualToTestValue") {
  options$hypothesisRec <- "two.sided"
} else if (options$hypothesis == "greaterThanTestValue") {
  options$hypothesisRec <- "greater"
} else {
  options$hypothesisRec <- "less"
}
```

#### Step 1.3: Defining the State (if empty)

If the state is not defined yet (i.e. is null), it needs to be defined as an empty list.

##### Example
```{r}
if (is.null(state)) {
  state <- list()
}
```

#### Step 1.4: Reading the Dataset

If the dataset has not been read yet, it needs to be read. Note that you can specify whether variables should be read as factors or numeric and whether cases with missing values for certain variables should be deleted.

##### Example
```{r}
if (is.null(dataset)) {
dataset <- .readDataSetToEnd(columns.as.numeric = options$variables, columns.as.factor = options$groupingVariable,
                             exclude.na.listwise = options$variables)
}
```

#### Step 1.5: Setting the Title

Each analysis needs a title. This title will be shown at the top of the output.

##### Example
```{r}
jaspResults$title <- "Binomial Test"
```

#### Step 1.6: Checking if Results Can Be Computed

Each analysis requires certain input (for example, dependent variables and independent variables). If this is not given, tables should still be displayed but will be filled with "." instead of actual results. Plots cannot be displayed without the input. Therefore, it makes sense to determine early in the analysis whether we are ready to compute the results for the analyses. For example, in the Binomial Test, at least one dependent variable is needed in order to compute results.

##### Example
```{r}
ready <- (length(options$variables) > 0)
```

#### Step 1.7: Checking for Errors

If we have the input we need, we still need to check for errors that will prevent the results from being computed. The error checks that should be conducted depend on the analysis. For the Binomial Test, we need to make sure that there is a least one factor level for each variable and that we do not have 0 observations for each of the levels of every variable.

##### Example
```{r}
if (ready) {
  # Error Check 1: Number of levels of the variables
  .hasErrors(dataset = dataset, perform = "run", type = 'factorLevels', factorLevels.target = options$variables,
             factorLevels.amount = '< 1', exitAnalysisIfErrors = TRUE)
	     
  # Error check 2: 0 observations for a level of a variable
  for (variable in options$variables) {	
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))

    for (level in levels) {
      .hasErrors(data[data == level], perform = "run", type = 'observations', observations.amount = c('< 1'),  
                 exitAnalysisIfErrors = TRUE)
    }
  }
}
```

Other common error checks include checking for weird data (too few observations, infinite values, variance = 0), and for a non-positive definite covariance matrix.

##### Example
```{r}
if (ready) {
  # Error check: Weird data for dependent variable in each level of the grouping variable
  .hasErrors(dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$variables, all.grouping = options$groupingVariable,
             observations.amount = c('< 3'), exitAnalysisIfErrors = TRUE)
```

##### Example
```{r}
  # Error check: Check for non-positive definite variance-covariance matrix
  covnwt <- stats::cov
  .hasErrors(dataset, perform = "run", type = c('varCovData'), exitAnalysisIfErrors = TRUE, 
	     varCovData.target = c(options$dependent.variable, options$main.effects.numeric),
	     varCovData.corFun = covnwt)
}
```

#### Step 1.8: Calling the Results Functions

If we are ready to compute the results and did not encounter any errors, it is time to call the results functions. Notably, this only needs to be done if this is the first run of the analysis or if the results computed during the last run of the analysis cannot be reused. Whereas this may sound complex, there is an essay way to check for it: We can just check whether the object for which we are computing the results is defined (i.e. not null) in jaspResults. If it is not defined (i.e. null), the results need to be computed.

##### Example
```{r}
# Compute Results for Binomial Table
if (ready == TRUE && is.null(jaspResults[["binomialTable"]])) {
  resultsTable <- .computeBinomialTableResults(dataset = dataset, options = options)
}
```

Most of the times, whether results need to be computed also depend on whether the user selected the corresponding options to require an analysis. As can be seen in the example below, the results for the "Descriptives Plots" in the Binomial Test do not need to be computed if the option for these plots was not selected.

##### Example
```{r}
# Compute Results for Binomial Plots
if (ready == TRUE && options$descriptivesPlots == TRUE && is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
  resultsPlots <- .computeBinomialPlotsResults(dataset = dataset, options = options)
}
```

#### Step 1.9: Calling the Create Functions

One of the last steps in the main analysis function is to call the create functions. These functions will create the framework for each table, plot, or container that will later appear in the output. Notably, in case of a table or a container including a table, the create functions can also be called if the required input is not given. In this case, the table will be created but filled with "." instead of actual results.

##### Example
```{r}
# Create Binomial Table
.createBinomialTable(jaspResults = jaspResults, options = options, ready = ready, resultsTable = resultsTable)
	
# Create Descriptives Plots Container (if wanted and if results can be computed)
if (ready == TRUE && options$descriptivesPlots == TRUE) {
  .createBinomialDescriptivesPlotsContainers(jaspResults = jaspResults, options = options, resultsPlots = resultsPlots)
}
```

#### Step 1.10: Updating and Returning the State

Finally, we need to update the options of the state with the current analysis options so that, in the next run, the new options can be compared to the current set of options. This will then determine which output elements can be reused and which ones need to be newly computed.

##### Example
```{r}
state[["options"]] <- options
return(state)
```

At this point, you may wonder why the fill up functions were not called in the main analysis function although being one of the four core functions of each analysis. This is because those functions are only needed if an output element is created. Therefore, these functions are called within the create functions.

#### Step 1.11: Time for Testing

Now, the main analysis is complete and it is time to test it. Below is an example of the complete main analysis function of the Binomial Test.

##### Example
```{r}
BinomialTest <- function(jaspResults, dataset, options, state = NULL) {
  
  # Update options
  if (options$hypothesis == "notEqualToTestValue") {
    options$hypothesisRec <- "two.sided"
  } else if (options$hypothesis == "greaterThanTestValue") {
    options$hypothesisRec <- "greater"
  } else {
    options$hypothesisRec <- "less"
  }
  
  # Define state if empty
  if (is.null(state)) {
	  state <- list()
	}
	
  # Read dataset
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.factor = options$variables)
  }
  
  # Update options (using dataset)
  options[["levels"]] <- NULL
  for (variable in options$variables) {
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    options[["levels"]] <- append(options$levels, list(levels))
    names(options$levels)[which(options$variables %in% variable)] <- variable
  }
  
  # Set title
  jaspResults$title <- "Binomial Test"
	
  # Check if results can be computed
  ready <- (length(options$variables) > 0)
	
  # Check for errors
  if (ready) {
	  
    # Error Check 1: Number of levels of the variables
    .hasErrors(dataset = dataset, perform = "run", type = 'factorLevels', factorLevels.target = options$variables,
               factorLevels.amount = '< 1', exitAnalysisIfErrors = TRUE)
	  
    # Error check 2: 0 observations for a level of a variable
    for (variable in options$variables) {
	    
      column <- dataset[[ .v(variable) ]]
      data <- column[!is.na(column)]
      levels <- levels(as.factor(data))

      for (level in levels) {
        .hasErrors(data[data == level], perform = "run", type = 'observations', observations.amount = c('< 1'), 
	           exitAnalysisIfErrors = TRUE)
      }
    }
  }
	
  # Compute Results for Binomial Table
  if (ready == TRUE && is.null(jaspResults[["binomialTable"]])) {
    resultsTable <- .computeBinomialTableResults(dataset = dataset, options = options)
  }
	
  # Compute Results for Binomial Plots
  if (ready == TRUE && options$descriptivesPlots == TRUE && is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
    resultsPlots <- .computeBinomialPlotsResults(dataset = dataset, options = options)
  }
	
  # Create Binomial Table
  .createBinomialTable(jaspResults = jaspResults, options = options, ready = ready, resultsTable = resultsTable)
	
  # Create Descriptives Plots Container (if wanted and if results can be computed)
  if (ready == TRUE && options$descriptivesPlots == TRUE) {
    .createBinomialDescriptivesPlotsContainers(jaspResults = jaspResults, options = options, resultsPlots = resultsPlots)
  }
	
  # Bring state up-to-date
  state[["options"]] <- options

  return(state)
}
```

### Step 2: Writing Results Functions

The goal of each results function is to return an object that contains the results. Results function can be written for tables or plots. However, how to compute the results strongly depends on the analysis. Therefore, results functions are the most flexible of the four core functions. Below are three examples. The first example contains only the elements that will be part of each results function (you may want to add additional arguments). The second example is the complete function used to compute the results for the table in the Binomial Test analysis. The third example is the complete function used to compute the results for a plot in the Binomial Test analysis.

##### Example
```{r}
.insertFunctionName <- function(dataset, options) { 

  # This will be the return object
  results <- list()
  
  # INSERT CODE HERE
  
  # Return results object
  return(results)
}
```

##### Example
```{r}
.computeBinomialTableResults <- function(dataset, options) {
  
  # This will be the return object
  results <- list()
  
  for (variable in options$variables) {
    
    results[[variable]] <- list()
    
    # Prepare for running the binomial test
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    
    for (level in levels) {
      
      nObs <- length(data)
      counts <- sum(data == level)
      proportion <- counts/nObs
      
      resultsBinom <- stats::binom.test(x = counts, n = nObs, p = options$testValue, alternative = options$hypothesisRec,
                                        conf.level = options$confidenceIntervalInterval)
      p <- resultsBinom$p.value
      if (p == FALSE) {
        p <- 0
      } else if (p == TRUE) {
        p <- 1
      }
      lowerCI <- resultsBinom$conf.int[1]
      upperCI <- resultsBinom$conf.int[2]
      
      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- list(variable = variable, level = level, counts = counts, total = nObs, 
                                           proportion = proportion, p = p, VovkSellkeMPR = .VovkSellkeMPR(p), 
                                           lowerCI = lowerCI, upperCI = upperCI)
    }
  }
  # Return results object
  return(results)
}
```

##### Example
```{r}
.computeBinomialPlotsResults <- function(dataset, options) {
  
  # This will be the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Prepare for running the binomial test
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    
    for (level in levels) {
      
      # Define base breaks function for y
      base_breaks_y <- function(x, testValue) {
        d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
        list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                          yend = yend),
                                   inherit.aes = FALSE, size = 1),
             ggplot2::scale_y_continuous(breaks = c(0,  round(testValue,3), 1)))
      }
      
      # Define plot position
      plotPosition <- ggplot2::position_dodge(0.2)
      
      # Compute data for plot
      nObs <- length(data)
      counts <- sum(data == level)
      proportion <- counts/nObs
      resultsBinom <- stats::binom.test(x = counts, n = nObs, p = options$testValue, alternative = "two.sided",
                                        conf.level = options$descriptivesPlotsConfidenceInterval)
      lowerCI <- resultsBinom$conf.int[1]
      upperCI <- resultsBinom$conf.int[2]
      
      summaryStat <- data.frame(label = level, proportion = proportion, lowerCI = lowerCI, upperCI = upperCI)
      dfTestValue <- data.frame(testValue = options$testValue)
      
      # Make plot
      descriptivesPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label, y = proportion, group = 1)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = 0.2, 
                               position = plotPosition) +
        ggplot2::geom_point(position = plotPosition, size = 4) +
        ggplot2::geom_hline(data = dfTestValue, ggplot2::aes(yintercept = options$testValue), linetype = "dashed") +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::ylim(min = 0, max = 1) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(size = 18),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_text(size = 18, vjust = -1),
                       axis.text.x = ggplot2::element_text(size = 15),
                       axis.text.y = ggplot2::element_text(size = 15),
                       panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank(),
                       legend.key = ggplot2::element_blank(),
                       legend.title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(size = 12),
                       axis.ticks = ggplot2::element_line(size = 0.5),
                       axis.ticks.margin = grid::unit(1, "mm"),
                       axis.ticks.length = grid::unit(3, "mm"),
                       plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
        base_breaks_y(summaryStat, dfTestValue$testValue)
      
      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- descriptivesPlot
    }
  }
  # Return results object
  return(results)
}
```

### Step 3: Writing Create Functions

The goal of each create function is to create an object that will be displayed in the output. Create function can be written for tables, plots, or containers. Whereas tables and plots are just tables and plots, containers need some further explanation. Containers consist of elements (again tables, plots, or containers) which will be grouped together. For example, if we have one descriptive plot for each variable, all of these plots can be grouped together in a descriptive plots container. Below, we will go through each example.

#### Step 3.1: Creating a Table

Every table consists of columns and rows. The general strategy in JASP is to specify the columns of a table in a create function and then add all of the required rows in a fill up function. Below, an overview is provided about how to create a table and the corresponding columns.

##### Step 3.1.1: The Arguments

A creating a table function has typically the following arguments:
1: jaspResults which is the object that contains all results from this analysis and connects it to the output
2: options which is a list of options selected or deselected by the user
3: ready which is a boolean variable that indicates whether all input is given that is needed to compute the results
4: resultsTable which is the object that contains the results for this table

###### Example
```{r}
.createBinomialTable <- function(jaspResults, options, ready, resultsTable) {
```

##### Step 3.1.2: Reusing a Table

If the table has been created before and if all options on which the table depends did not change compared to the last time the table was created, the table can be reused and no further steps are necessary. That is, the function can be terminated at this point.

###### Example
```{r}
if (!is.null(jaspResults[["binomialTable"]])) {
  return(NULL)
}
```

##### Step 3.1.3: Creating a New Table

If the table cannot be reused, it needs to be newly created. By doing so, it needs to be given a title that will be displayed in the output and added to the jaspResults objects as shown below. Furthermore, we need to specify the analysis options on which the table depends. If the value of any of these options changes, the table will be removed from the output and needs to be newly computed if it should still be displayed. However, if none of these options changes the next time the analysis is called, the table can be reused (see Step 3.1.2).

###### Example
```{r}
binomialTable <- createJaspTable(title = "Binomial Test")
jaspResults[["binomialTable"]] <- binomialTable
binomialTable$dependOnOptions(c("variables", "testValue", "hypothesis", "confidenceInterval", 
                                "confidenceIntervalInterval", "VovkSellkeMPR"))
```

##### Step 3.1.4: Adding Columns to the Table

Next, we need to add all columns to the table that should be displayed. Some columns are always part of the table. Others depend on whether the user selected the corresponding option or not. Each column needs a name, a title (although the title can be ""), and a type ("string", "integer", or "number"). If the type is "number", then the column also needs a format that specifies the number of digits and decimals. Other helpful options are "combine", which ensures that if multiple rows have the same value, that value is shown only in the first row, and "overtitle" which adds an overtitle for multiple columns (often used for confidence intervals).

###### Example
```{r}
binomialTable$addColumnInfo(name = "variable",      title = "Variable",     type = "string", combine = TRUE)
binomialTable$addColumnInfo(name = "level",         title = "Level",        type = "string")
binomialTable$addColumnInfo(name = "counts",        title = "Counts",       type = "integer")
binomialTable$addColumnInfo(name = "total",         title = "Total",        type = "integer")
binomialTable$addColumnInfo(name = "proportion",    title = "Proportion",   type = "number", format = "sf:4")
binomialTable$addColumnInfo(name = "p",             title = "p",            type = "number", format = "dp:3;p:.001")
if (options$VovkSellkeMPR) {
  binomialTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number", format = "sf:4")
}
if (options$confidenceInterval) {
  binomialTable$addColumnInfo(name = "lowerCI",       title = "Lower",        type = "number", format = "sf:4", 
                              overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
  binomialTable$addColumnInfo(name = "upperCI",       title = "Upper",        type = "number", format = "sf:4", 
                              overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
}
```

showSpecifiedColumsnOnly is another useful functionality provided by the jaspResults package that can be used in the create table function. If it is set as TRUE, only the columns that we added in this step are actually shown in the output (even if a row that is added to the table contains more statistics). This can be useful as results function are often blown up by complicated, nested if-statements in order to compute only the necessary statistics. By using showSpecifiedColumnsOnly, we can avoid this complexity by computing the results for all statistics (even those that the user does not require).

###### Example
```{r}
binomialTable$showSpecifiedColumnsOnly <- TRUE
```

##### Step 3.1.5: Calling the Fill Up Function

Now that the table has been created, it is time to fill it up with results. In order to do so, we need to call the fill up function.

###### Example
```{r}
.fillUpBinomialTable(binomialTable = binomialTable, options = options, ready = ready, resultsTable = resultsTable)
```

##### Step 3.1.6: Time for Testing

Now, the creating a table function is complete and it is time to test it. Below is an example of the complete creating a table function of the Binomial Test.

###### Example
```{r}
.createBinomialTable <- function(jaspResults, options, ready, resultsTable) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["binomialTable"]])) {
    return(NULL)
  }
  
  # Create table
  binomialTable <- createJaspTable(title = "Binomial Test")
  jaspResults[["binomialTable"]] <- binomialTable
  binomialTable$showSpecifiedColumnsOnly <- TRUE
  binomialTable$dependOnOptions(c("variables", "testValue", "hypothesis", "confidenceInterval", 
                                  "confidenceIntervalInterval", "VovkSellkeMPR"))
  
  # Add columns to table
  binomialTable$addColumnInfo(name = "variable",      title = "Variable",     type = "string", combine = TRUE)
  binomialTable$addColumnInfo(name = "level",         title = "Level",        type = "string")
  binomialTable$addColumnInfo(name = "counts",        title = "Counts",       type = "integer")
  binomialTable$addColumnInfo(name = "total",         title = "Total",        type = "integer")
  binomialTable$addColumnInfo(name = "proportion",    title = "Proportion",   type = "number", format = "sf:4")
  binomialTable$addColumnInfo(name = "p",             title = "p",            type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    binomialTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number", format = "sf:4")
  }
  if (options$confidenceInterval) {
    binomialTable$addColumnInfo(name = "lowerCI",       title = "Lower",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
    binomialTable$addColumnInfo(name = "upperCI",       title = "Upper",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
  }
  
  # Fill up table with results
  .fillUpBinomialTable(binomialTable = binomialTable, options = options, ready = ready, resultsTable = resultsTable)

  # This function does not return anything
}
```

#### Step 3.2: Creating a Container

Every container contains elements (tables, plots, or other containers). The main function of a container is to group such elements together.

##### Step 3.2.1: The Arguments

A creating a container function has typically the following arguments:
1: jaspResults which is the object that contains all results from this analysis and connects it to the output
2: options which is a list of options selected or deselected by the user
3: resultsX which is the object that contains the results for this container

###### Example
```{r}
.createBinomialDescriptivesPlotsContainers <- function(jaspResults, options, resultsPlots) {
```

##### Step 3.2.2: Reusing a Container

If the container has been created before and if all options on which the container depends did not change compared to the last time the container was created, the container can be reused and no further steps are necessary. That is, the function can be terminated at this point.

###### Example
```{r}
if (!is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
  return(NULL)
}
```

##### Step 3.2.3: Creating a New Container

If the container cannot be reused, it needs to be newly created. By doing so, it needs to be given a title that will be displayed in the output and added to the jaspResults objects as shown below. Furthermore, we need to specify the analysis options on which the container depends. If the value of any of these options changes, the container will be removed from the output and needs to be newly computed if it should still be displayed. However, if none of these options changes the next time the analysis is called, the container can be reused (see Step 3.2.2).

###### Example
```{r}
binomialDescriptivesPlotsContainerTotal <- createJaspContainer(title = "Descriptives Plots")
jaspResults[["binomialDescriptivesPlotsContainerTotal"]] <- binomialDescriptivesPlotsContainerTotal
binomialDescriptivesPlotsContainerTotal$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                          "descriptivesPlotsConfidenceInterval"))
```

##### Step 3.2.4: Adding Subcontainers to a Container

Sometimes, we want to have subcontainers within a container. For example, for the Binomial Test, we want to have one descriptive plot for each level of each dependent variable. That is, we want to have one container for all descriptives plots (see the last example) and then add a subcontainer for each dependent variable that contains all plots for the levels belonging to that variable. The steps for creating a subcontainer are almost identical to creating a container. The only difference is that the container will be added within the previously defined container (see the second row in the example below).

###### Example
```{r}
for (variable in options$variables) {
  binomialDescriptivesPlotsContainerVariable <- createJaspContainer(title = variable)
  binomialDescriptivesPlotsContainerTotal[[variable]] <- binomialDescriptivesPlotsContainerVariable
  binomialDescriptivesPlotsContainerVariable$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                               "descriptivesPlotsConfidenceInterval"))
}
```

##### Step 3.2.5: Calling the Fill Up Function

Now that the container has been created, it is time to fill it up with results. In order to do so, we need to call the fill up function.

###### Example
```{r}
.fillUpBinomialDescriptivesPlotsContainers(binomialDescriptivesPlotsContainerTotal, options, resultsPlots)
```

##### Step 3.2.6: Time for Testing

Now, the creating a container function is complete and it is time to test it. Below is an example of the complete creating a container function of the Binomial Test.

###### Example
```{r}
.createBinomialDescriptivesPlotsContainers <- function(jaspResults, options, resultsPlots) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
    return(NULL)
  }
  
  # Create container for all variables
  binomialDescriptivesPlotsContainerTotal <- createJaspContainer(title = "Descriptives Plots")
  jaspResults[["binomialDescriptivesPlotsContainerTotal"]] <- binomialDescriptivesPlotsContainerTotal
  binomialDescriptivesPlotsContainerTotal$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                            "descriptivesPlotsConfidenceInterval"))
  
  # Create subcontainer for each variable
  for (variable in options$variables) {
    binomialDescriptivesPlotsContainerVariable <- createJaspContainer(title = variable)
    binomialDescriptivesPlotsContainerTotal[[variable]] <- binomialDescriptivesPlotsContainerVariable
    binomialDescriptivesPlotsContainerVariable$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                                 "descriptivesPlotsConfidenceInterval"))
  }
  
  # Fill up containers with plots
  .fillUpBinomialDescriptivesPlotsContainers(binomialDescriptivesPlotsContainerTotal, options, resultsPlots)
  
  # This function does not return anything
}
```

#### Step 3.3: Creating a Plot

##### Step 3.3.1: The Arguments

A creating a plot function has typically the following arguments:
1: jaspResults which is the object that contains all results from this analysis and connects it to the output
2: options which is a list of options selected or deselected by the user
3: resultsPlots which is the object that contains the results for the plot

###### Example
```{r}
.createBinomialDescriptivesPlot <- function(jaspResults, options, resultsPlots) {
```

##### Step 3.3.2: Reusing a Plot

If the plot has been created before and if all options on which the plot depends did not change compared to the last time the plot was created, the plot can be reused and no further steps are necessary. That is, the function can be terminated at this point.

###### Example
```{r}
if (!is.null(jaspResults[["binomialDescriptivesPlot"]])) {
  return(NULL)
}
```

##### Step 3.3.3: Creating a New Plot

If the plot cannot be reused, it needs to be newly created. By doing so, it needs to be given a title that will be displayed in the output and added to the jaspResults objects as shown below. Furthermore, we need to specify the analysis options on which the plot depends. If the value of any of these options changes, the plot will be removed from the output and needs to be newly computed if it should still be displayed. However, if none of these options changes the next time the analysis is called, the plot can be reused (see Step 3.3.2).

###### Example
```{r}
binomialDescriptivesPlot <- createJaspPlot(plot = resultsPlots, title = "Plot")
jaspResults[["binomialDescriptivesPlot]] <- binomialDescriptivesPlot
binomialDescriptivesPlot$dependOnOptions(c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsConfidenceInterval"))
```

##### Step 3.3.5: Adding the Plot Result

Now that the plot has been created, it is time to fill it up with the result. However, this is very straightforward for a plot. Therefore, no extra fill up function is necessary but it can already be done when creating the plot (plot = descriptivesPlot).

###### Example
```{r}
binomialDescriptivesPlot <- createJaspPlot(plot = resultsPlots, title = "Plot")
```

##### Step 3.3.6: Time for Testing

Now, the creating a container function is complete and it is time to test it. Below is an example of the complete creating a plot function adapted from the Binomial Test.

###### Example
```{r}
.createBinomialDescriptivesPlot <- function(jaspResults, options, resultsPlots) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["binomialDescriptivesPlot"]])) {
    return(NULL)
  }
  
  descriptivesPlot <- resultsPlots
  binomialDescriptivesPlot <- createJaspPlot(plot = descriptivesPlot, title = level)
  jaspResults[["binomialDescriptivesPlot"]] <- binomialDescriptivesPlot
  binomialDescriptivesPlot$dependOnOptions(c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsConfidenceInterval"))
  
  # This function does not return anything
}
```

### Step 4: Writing Fill Up Functions

The goal of each fill up function is to fill up output elements that were created with create functions using objects that were computed in the results function. Fill up functions can be written for tables or containers. In principle, fill up functions can also be written for plots but as this is very straightforward it may not be worth to write a new function for this step (see 3.3.5).

#### Step 4.1: Filling Up a Table

##### Step 4.1.1: The Arguments

A filling up a table function has typically the following arguments:
1: xTable which is the table that was created in the create function and that we want to fill up
2: options which is a list of options selected or deselected by the user
3: ready which is a boolean variable that indicates whether all input is given that is needed to compute the results
4: resultsX which is the object that contains the results

###### Example
```{r}
.fillUpBinomialTable <- function(binomialTable, options, ready, resultsTable) {
```


Now if the analysis is called again and e.g. `sampleMode` changes, only the plot will be returned.
If there are state items you would like to keep indefinitely -- regardless of the options a user changes -- you must omit these in the stateKey. Elements of the statekey can also be a list instead of a character vector. The only reason to use a list is to mix expressions in the stateKey with options. This allows for more complicated dependency checks. For example:
```
# a descriptives plot for each variable
state[["plotDescriptives"]] <- plotDescriptives
stateKey[["plotDescriptives"]] <- list(expression("contNormal" %in% options[["variables"]]), "fixedFactors")
```
Expressions are evaluated in a stand-alone environment where two other variables are present:
`options`, the *new* options, and `state`, the state. These are the only objects that can be called. If the expression succeeds, TRUE should be returned. For all other outputs this element in the state will be discarded. If the expression gives an error, a warning is given inside QT.


Some analyses contain collections of object, e.g., a descriptive plot for every variable. Now if a variable is removed from the anlayses, all plots except for one are still useable. The stateKey can be specified individually for each element of a collection. An example is given below.
```
state <- list(
  descriptivesPlot = list(v1 = ggplotObjectV1,
                          v2 = ggplotObjectV2,
                          v3 = ggplotObjectV3)
)
stateKey <- list(
  descriptivesPlot = c("showDescriptivesPlot") # changing this removes the entire element 'descriptivesPlot'.
)
attr(stateKey[["descriptivesPlot"]], "collection") <- list(
  # each subselement also depends on the presence of a variable
  v1 = list(expression(v1 %in% options[["variables"]])),
  v2 = list(expression(v2 %in% options[["variables"]])),
  v3 = list(expression(v3 %in% options[["variables"]]))
)
# the above is the likely the most common case, hence there exists a convenience function
variables <- paste0("v", 1:3)
collectionKey <- .stateDependsOnVar(variables)
attr(stateKey[["descriptivesPlot"]], "collection") <- collectionKey
```
In the example above, all ggplot objects in the state depend on the option `showDescriptivesPlot`. In addition, each individual ggplot object depends on whether a variable is included in `options[["variables"]]`. The subelements of a collection are checked by name, or if there are no names in order. Subelements without checks are kept by default. Collections keys cannot be nested inside collection keys (there is no recursion). Also, it would be bad practice to organize the state that way.


If the results could be computed (ready == TRUE), it is time to add the results to the table that we created earlier (Step 3). Oftentimes, we need to add one row for each dependent variable or for each predictor. Therefore, the results are often added in for loops. In the example provided below, one row with results is added for each level of each dependent variable. The results are taken from the results argument computed in the corresponding results function.

###### Example
```{r}
for (variable in options$variables) {
  for (level in options$levels[[variable]]) {
    row <- resultsTable[[variable]][[level]]
    binomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
  }
}
```

##### Step 4.1.3: Adding Footnotes to Tables

The fill up function is also a good place to add footnotes to tables. Footnotes provide the user with useful information to interprete the results. Each footnote needs a message that will be displayed. It may also have a symbol (this can also be a text like "Warning"). 

###### Example
```{r}
if (options$VovkSellkeMPR) {
  binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
}
```

If the footnote should be connected with a specific row, column, or cell, it can be matched by using the arguments "row_names" and "col_names". Notably, this requires to name the rows when they are added to the table (see "rowNames" in the last example).

###### Example
```{r}
if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
  message <- .messages('footnote', 'leveneSign')
  independentSamplesTTestTableParametric$addFootnote(message = message, col_names = "p", row_names = variable)
}
```

##### Step 4.1.4: Adding Dots to Tables (If Not Ready)

If the results could NOT be computed (ready == FALSE), a single row with dots should be added to the table such that the table can nonetheless be displayed in the output but also makes it clear that input is missing.

###### Example
```{r}
if (ready == TRUE) {

  # INSERT CODE

} else {
  row <- list(variable = ".", level = ".", counts = ".", total = ".", proportion = ".", p = ".", 
              VovkSellkeMPR = ".", lowerCI = ".", upperCI = ".")
  binomialTable$addRows(row)
}
```

##### Step 4.1.5: Time for Testing

Now, the filling up a table function is complete and it is time to test it. Below is an example of the complete filling up a table function from the Binomial Test.

###### Example
```{r}
.fillUpBinomialTable <- function(binomialTable, options, ready, resultsTable) {
  
  # If results can be computed, compute them and add row for each level of each variable
  if (ready == TRUE) {
    
    for (variable in options$variables) {
      for (level in options$levels[[variable]]) {
        row <- resultsTable[[variable]][[level]]
        binomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
      }
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
    # Add footnote: Alternative hypothesis
    if (options$hypothesisRec == "two.sided") {
      binomialTable$addFootnote(message = .messages("footnote", "binomNeq", value=options$testValue), symbol="<em>Note.</em>")
    } else if (options$hypothesisRec == "greater") {
      binomialTable$addFootnote(message = .messages("footnote", "binomGreater", value=options$testValue), symbol="<em>Note.</em>")
    } else if (options$hypothesisRec == "less") {
      binomialTable$addFootnote(message = .messages("footnote", "binomLess", value=options$testValue), symbol="<em>Note.</em>")
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", level = ".", counts = ".", total = ".", proportion = ".", p = ".", 
                VovkSellkeMPR = ".", lowerCI = ".", upperCI = ".")
    binomialTable$addRows(row)
  }
  
  # This function does not return anything
}
```

#### Step 4.2: Filling Up a Container

Filling up a container functions are more variable because what needs to be done depends on the structure of container (does it consists of subcontainers, tables, or plots?). Here we provide an example for filling up a container with plots.

##### Step 4.2.1: The Arguments

A filling up a container with plots function has typically the following arguments:
1: jaspResults which is the object that contains all results from this analysis and connects it to the output
2: options which is a list of options selected or deselected by the user
3: resultsX which is the object that contains the results

###### Example
```{r}
.fillUpBinomialDescriptivesPlotsContainers <- function(binomialDescriptivesPlotsContainerTotal, options, resultsPlots) {
```

##### Step 4.2.2: Adding the Results to a Container

Similarly to filling up a table, we can use for loops to add results to the container that we created earlier (Step 3). In the example provided below, one plot is added for each level of each dependent variable. The results are taken from the results argument computed in the corresponding results function. Note that for plots, we do not offer an equivalent to the table with a dotted line. Instead, we only call the fill up function if all necessary input is given (ready == TRUE).

###### Example
```{r}
for (variable in options$variables) {
  for (level in options$levels[[variable]]) {
    descriptivesPlot <- resultsPlots[[variable]][[level]]
    binomialDescriptivesPlot <- createJaspPlot(plot = descriptivesPlot, title = level)
    binomialDescriptivesPlotsContainerTotal[[variable]][[level]] <- binomialDescriptivesPlot
    binomialDescriptivesPlot$dependOnOptions(c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsConfidenceInterval"))
  }
}
```

##### Step 4.2.3: Time for Testing

Now, the filling up a container with plots function is complete and it is time to test it. Below is an example of the complete filling up a container with plots function from the Binomial Test.

###### Example
```{r}
.fillUpBinomialDescriptivesPlotsContainers <- function(binomialDescriptivesPlotsContainerTotal, options, resultsPlots) {
  
  for (variable in options$variables) {
    for (level in options$levels[[variable]]) {
      descriptivesPlot <- resultsPlots[[variable]][[level]]
      binomialDescriptivesPlot <- createJaspPlot(plot = descriptivesPlot, title = level)
      binomialDescriptivesPlotsContainerTotal[[variable]][[level]] <- binomialDescriptivesPlot
      binomialDescriptivesPlot$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                 "descriptivesPlotsConfidenceInterval"))
      
    }
  }
  
  # This function does not return anything
}
```
