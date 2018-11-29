#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# NB: this file has custom code folding enabled. If you're in atom, install the
# "custom-folds" package. In other editors you might be able to define
# the <editor-fold> and </editor-fold> as start- and endpoints of a code fold.

Descriptives <- function(jaspResults, dataset, options, state=NULL)
{
  variables <- unlist(options$variables)
  splitName <- options$splitby
  makeSplit <- splitName != ""
  
  if(is.null(state))
    state <- list()

  if (is.null(dataset)) {
      if (makeSplit) {
        dataset         <- readDataSetToEnd(columns.as.numeric=variables, columns.as.factor=splitName)
        dataset.factors <- readDataSetToEnd(columns=variables, columns.as.factor=splitName)
      } else {
        dataset         <- readDataSetToEnd(columns.as.numeric=variables)
        dataset.factors <- readDataSetToEnd(columns=variables)
      }
  }

  # If user requests split, create a list of datasets, one for each level
  if (makeSplit)
  {
    splitFactor     <- dataset[[v(splitName)]]
    splitLevels     <- levels(splitFactor)
    splitDat        <- lapply(splitLevels, function(l) { return(dataset.factors[splitFactor == l,]) })
    names(splitDat) <- splitLevels
  }

  # Initialise the results
  jaspResults$title <- "Descriptives"

  .descriptivesDescriptivesTable(dataset, options, jaspResults)

  # Frequency table
  if (options$frequencyTables)
  {
    if(is.null(jaspResults[["tables"]]))
    {
      jaspResults[["tables"]] <- createJaspContainer("Frequency Tables")
      jaspResults[["tables"]]$dependOnOptions(c("frequencyTables", "splitby"))
    }

    .descriptivesFrequencyTables(dataset.factors, options, jaspResults[["tables"]])

    if (jaspResults[["tables"]]$length > 0 && is.null(jaspResults[["frequenciesHeading"]]))
    {
      jaspResults[["frequenciesHeading"]] <- createJaspHtml("Frequencies", "h1")
      jaspResults[["frequenciesHeading"]]$copyDependenciesFromJaspObject(jaspResults[["tables"]])
      jaspResults[["frequenciesHeading"]]$dependOnOptions("variables")
    }
  }



  # Correlation plot
  if (options$plotCorrelationMatrix)
  {
    if(is.null(jaspResults[["matrixPlot"]]))
    {
      if (makeSplit)
      {
        jaspResults[["matrixPlot"]] <- createJaspContainer(title="Correlation plots")
        corrPlot <- jaspResults[["matrixPlot"]]
        corrPlot$dependOnOptions(c("plotCorrelationMatrix", "splitby"))


        for (i in 1:length(splitLevels))
          corrPlot[[splitLevels[i]]] <- .descriptivesMatrixPlot(splitDat[[i]], options, splitLevels[i])

      } else
        jaspResults[["matrixPlot"]] <- .descriptivesMatrixPlot(dataset, options, "Correlation plot") # Create one plot
    }
  }

  # Distribution plots
  if (options$plotVariables)
  {
    if(is.null(jaspResults[["distributionPlots"]]))
    {
      jaspResults[["distributionPlots"]] <- createJaspContainer("Distribution Plots")
      jaspResults[["distributionPlots"]]$dependOnOptions(c("plotVariables", "splitby"))
    }

    distPlots <- jaspResults[["distributionPlots"]]

    for (var in variables)
      if(is.null(distPlots[[var]]))
      {
        if(makeSplit) distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = splitDat, options = options, variable = var)
        else          distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = dataset,  options = options, variable = var)
      }

    if(distPlots$length == 0)
      jaspResults[["distributionPlots"]] <- NULL
  }

  # Box plots
  if (options$splitPlots)
  {
    if(is.null(jaspResults[["splitPlots"]]))
    {
      jaspResults[["splitPlots"]] <- createJaspContainer("Boxplots")
      jaspResults[["splitPlots"]]$dependOnOptions(c("splitPlots", "splitby"))
    }

    splitPlots <- jaspResults[["splitPlots"]]

    for (var in variables)
      if(is.null(splitPlots[[var]]))
      {
        splitPlots[[var]] <- .descriptivesSplitPlot(dataset = dataset, options = options, variable = var)
        splitPlots[[var]]$setOptionMustContainDependency("variables", var)
      }

    if(splitPlots$length == 0)
      jaspResults[["splitPlots"]] <- NULL
  }


  state[["options"]] <- options

  return(state)
}

.descriptivesDescriptivesTable <- function(dataset, options, jaspResults) {
# This was refactored unhampered by an overwhelming knowledge of what the code was supposed to do by JCG in april 2018 for use with jaspResults

  if(!is.null(jaspResults[["stats"]])) return() #The options for this table didn't change so we don't need to rebuild it

  wantsSplit              <- options$splitby != ""
  variables               <- unlist(options$variables)
  equalGroupsNo           <- options$percentileValuesEqualGroupsNo
  percentilesPercentiles  <- unique(options$percentileValuesPercentilesPercentiles)
  stats                   <- createJaspTable("Descriptive Statistics")
  jaspResults[["stats"]]  <- stats
  stats$transpose         <- TRUE

  stats$dependOnOptions(c("splitby", "variables", "percentileValuesEqualGroupsNo", "percentileValuesPercentilesPercentiles", "mean", "standardErrorMean",
    "median", "mode", "standardDeviation", "variance", "skewness", "kurtosis", "range", "minimum", "maximum", "sum", "percentileValuesQuartiles", "percentileValuesEqualGroups", "percentileValuesPercentiles"))

  if (wantsSplit)
  {
    stats$transposeWithOvertitle <- TRUE
    stats$addColumnInfo(name="Variable",  title="", type="string")
    stats$addColumnInfo(name="Level",     title="", type="string")
  } else {
    stats$addColumnInfo(name="Variable",  title="", type="string")
  }
  stats$addColumnInfo(name="Valid",   type="integer")
  stats$addColumnInfo(name="Missing", type="integer")

  if (options$mean)                 stats$addColumnInfo(name="Mean",                        type="number", format="sf:4")
  if (options$standardErrorMean)    stats$addColumnInfo(name="Std. Error of Mean",          type="number", format="sf:4")
  if (options$median)               stats$addColumnInfo(name="Median",                      type="number", format="sf:4")
  if (options$mode)                 stats$addColumnInfo(name="Mode",                        type="number", format="sf:4")
  if (options$standardDeviation)    stats$addColumnInfo(name="Std. Deviation",              type="number", format="sf:4")
  if (options$variance)             stats$addColumnInfo(name="Variance",                    type="number", format="sf:4")
  if (options$skewness) {           stats$addColumnInfo(name="Skewness",                    type="number", format="sf:4")
                                    stats$addColumnInfo(name="Std. Error of Skewness",      type="number", format="sf:4") }
  if (options$kurtosis) {           stats$addColumnInfo(name="Kurtosis",                    type="number", format="sf:4")
                                    stats$addColumnInfo(name="Std. Error of Kurtosis",      type="string", format="sf:4") }
  if (options$range)                stats$addColumnInfo(name="Range",                       type="number", format="sf:4")
  if (options$minimum)              stats$addColumnInfo(name="Minimum",                     type="number", format="sf:4")
  if (options$maximum)              stats$addColumnInfo(name="Maximum",                     type="number", format="sf:4")
  if (options$sum)                  stats$addColumnInfo(name="Sum",                         type="number", format="sf:4")

  if (options$percentileValuesQuartiles) {
                                    stats$addColumnInfo(name="q1", title="25th percentile", type="number", format="sf:4")
                                    stats$addColumnInfo(name="q2", title="50th percentile", type="number", format="sf:4")
                                    stats$addColumnInfo(name="q3", title="75th percentile", type="number", format="sf:4")
  }

  if (options$percentileValuesEqualGroups)  # I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way
    for (i in seq(equalGroupsNo - 1))
      stats$addColumnInfo(name=paste("eg", i, sep=""), title=paste(as.integer(100 * i / equalGroupsNo), "th percentile", sep=""), type="number", format="sf:4")

  if (options$percentileValuesPercentiles)
    for (i in percentilesPercentiles)
      stats$addColumnInfo(name=paste("pc", i, sep=""), title=paste(i, "th percentile", sep=""), type="number", format="sf:4")

  # lets just add footnotes once instead of a gazillion times..
  shouldAddNominalTextFootnote      <- FALSE
  shouldAddModeMoreThanOnceFootnote <- FALSE

  # Find the number of levels to loop over
  if (wantsSplit)
  {
    split       <- dataset[[ v(options$splitby) ]]
    splitLevels <- levels(split)
    nLevels     <- length(levels(split))

    for (variable in variables)
      for (l in 1:nLevels)
      {
        column    <- dataset[[ v(variable) ]][split==splitLevels[l]]
        subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable = variable, Level = splitLevels[l]), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote)

        shouldAddNominalTextFootnote      <- subReturn$shouldAddNominalTextFootnote
        shouldAddModeMoreThanOnceFootnote <- subReturn$shouldAddModeMoreThanOnceFootnote

        stats$addRows(subReturn$resultsCol)
      }

  }
  else #we dont want to split
  {
    for (variable in variables)
    {
      column    <- dataset[[ v(variable) ]]
      subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable=variable), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote)

      shouldAddNominalTextFootnote      <- subReturn$shouldAddNominalTextFootnote
      shouldAddModeMoreThanOnceFootnote <- subReturn$shouldAddModeMoreThanOnceFootnote

      stats$addRows(subReturn$resultsCol)
    }
  }


  if (shouldAddNominalTextFootnote)
    stats$addFootnote(message="Not all values are available for <i>Nominal Text</i> variables", symbol="<i>Note.</i>")

  if(shouldAddModeMoreThanOnceFootnote)
    stats$addFootnote(message="More than one mode exists, only the first is reported", col_names="Mode")

  return(stats)
}

.descriptivesDescriptivesTable_subFunction <- function(column, resultsCol, options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote)
{
  equalGroupsNo           <- options$percentileValuesEqualGroupsNo
  percentilesPercentiles  <- unique(options$percentileValuesPercentilesPercentiles)

  rows        <- length(column)
  na.omitted  <- na.omit(column)

  resultsCol[["Valid"]]   <- length(na.omitted)
  resultsCol[["Missing"]] <- rows - length(na.omitted)

  if (base::is.factor(na.omitted) && (options$mean || options$mode || options$median || options$minimum || options$standardErrorMean || options$kurtosis || options$skewness || options$percentileValuesQuartiles || options$variance || options$standardDeviation || options$percentileValuesPercentiles || options$sum || options$maximum))
    shouldAddNominalTextFootnote <- TRUE

  resultsCol[["Mean"]]                    <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$mean,              na.omitted, mean)
  resultsCol[["Median"]]                  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$median,            na.omitted, median)
  resultsCol[["Sum"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$sum,               na.omitted, sum)
  resultsCol[["Maximum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$maximum,           na.omitted, max)
  resultsCol[["Minimum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$minimum,           na.omitted, min)
  resultsCol[["Range"]]                   <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$range,             na.omitted, function(param) { range(param)[2] - range(param)[1]})
  resultsCol[["Std. Deviation"]]          <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$standardDeviation, na.omitted, sd)
  resultsCol[["Std. Error of Mean"]]      <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$standardErrorMean, na.omitted, function(param) { sd(param)/sqrt(length(param))} )
  resultsCol[["Variance"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$variance,          na.omitted, var)
  resultsCol[["Kurtosis"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesKurtosis)
  resultsCol[["Std. Error of Kurtosis"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesSEK)
  resultsCol[["Skewness"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSkewness)
  resultsCol[["Std. Error of Skewness"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSES)

  if (options$mode)
  {

    if (base::is.factor(na.omitted) == FALSE)
    {
        mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))

        if (length(mode) > 1)
          shouldAddModeMoreThanOnceFootnote <- TRUE

        resultsCol[["Mode"]] <- clean(mode[1])
    }
    else
      resultsCol[["Mode"]] <- ""
  }
  else
    resultsCol[["Mode"]] <- NULL

  if (options$percentileValuesQuartiles)
  {
    if (base::is.factor(na.omitted) == FALSE)
    {
      resultsCol[["q1"]] <- clean(quantile(na.omitted, c(.25), type=6, names=F))
      resultsCol[["q2"]] <- clean(quantile(na.omitted, c(.5),  type=6, names=F))
      resultsCol[["q3"]] <- clean(quantile(na.omitted, c(.75), type=6, names=F))
    } else {
      resultsCol[["q1"]] <- ""
      resultsCol[["q2"]] <- ""
      resultsCol[["q3"]] <- ""
    }
  } else {
    resultsCol[["q1"]] <- NULL
    resultsCol[["q2"]] <- NULL
    resultsCol[["q3"]] <- NULL
  }

  equalGroupNames <- NULL
  if (options$percentileValuesEqualGroups)
    equalGroupNames <- paste("eg", seq(equalGroupsNo - 1), sep="")

  percentileNames <- NULL
  if (options$percentileValuesPercentiles)
    percentileNames <- paste("pc", percentilesPercentiles, sep="")

  for (row in names(resultsCol))
  {
    if (substr(row, 1, 2) == "eg" && ((row %in% equalGroupNames) == FALSE))
      resultsCol[[row]] <- NULL

    if (substr(row, 1, 2) == "pc" && ((row %in% percentileNames) == FALSE))
      resultsCol[[row]] <- NULL
  }

  if (base::is.factor(na.omitted) == FALSE)
  {
    if (options$percentileValuesEqualGroups)
      for (i in seq(equalGroupsNo - 1))
        resultsCol[[paste("eg", i, sep="")]] <- clean(quantile(na.omitted, c(i / equalGroupsNo), type=6, names=F))

    if (options$percentileValuesPercentiles)
      for (i in percentilesPercentiles)
        resultsCol[[paste("pc", i, sep="")]] <- clean(quantile(na.omitted, c(i / 100), type=6, names=F))
  }else{
    if (options$percentileValuesEqualGroups)
      for (i in seq(equalGroupsNo - 1))
        resultsCol[[paste("eg", i, sep="")]] <- ""

    if (options$percentileValuesPercentiles)
      for (i in percentilesPercentiles)
        resultsCol[[paste("pc", i, sep="")]] <- ""
  }

  return(list(resultsCol=resultsCol, shouldAddNominalTextFootnote=shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote=shouldAddModeMoreThanOnceFootnote))
}


.descriptivesDescriptivesTable_subFunction_OptionChecker <- function(optionToCheck, na.omitted, function_to_use)
{
  if (!optionToCheck)
    return(NULL)

  if (base::is.factor(na.omitted))
    return("")

  return(clean(function_to_use(na.omitted)))

}

.descriptivesFrequencyTables <- function(dataset, options, freqTabs) {
  splitName   <- options$splitby
  wantsSplit  <- splitName!=""
  splitFactor <- dataset[[v(splitName)]]
  splitLevels <- levels(splitFactor)

  for (variable in options$variables)
  {

    column <- dataset[[v(variable)]]
    if (!is.factor(column)){
      next
    }

    #if (variable %in% names(stateTabs) &&
    #  "data" %in% names(stateTabs[[variable]])) {
    #  freqTabs[[length(freqTabs) + 1]] <- stateTabs[[variable]]
    #  next
    #}


    if(!is.null(freqTabs[[variable]]))
      next

    freqTabs[[variable]] <- createJaspTable(paste("Frequencies for", variable))

    freqTab <- freqTabs[[variable]]
    freqTab$setOptionMustContainDependency("variables", variable)

    if (wantsSplit) freqTab$addColumnInfo(name = "factor", title = splitName, type = "string", combine=TRUE)

    freqTab$addColumnInfo(name="Level",               type="string", title=variable)
    freqTab$addColumnInfo(name="Frequency",           type="integer")
    freqTab$addColumnInfo(name="Percent",             type="number", format="dp:1")
    freqTab$addColumnInfo(name="Valid Percent",       type="number", format="dp:1")
    freqTab$addColumnInfo(name="Cumulative Percent",  type="number", format="dp:1")

    rows <- list()

    if (wantsSplit)
    {
      for (lev in splitLevels)  # also loop over the levels
      {
        t         <- table(column[splitFactor==lev])
        total     <- sum(t)
        alltotal  <- length(column[splitFactor==lev])
        cFreq     <- 0

        for (i in seq_along(names(t)))
        {
          row                         <- list()
          row[["factor"]]             <- lev
          row[["Level"]]              <- names(t)[i]
          row[["Frequency"]]          <- as.vector(t[i])
          cFreq                       <- cFreq + row[["Frequency"]]
          row[["Percent"]]            <- row[["Frequency"]]/alltotal*100
          row[["Valid Percent"]]      <- row[["Frequency"]]/total*100
          row[["Cumulative Percent"]] <- cFreq/total*100
          row[[".isNewGroup"]]        <- i==1
          rows[[length(rows) + 1]]    <- row
        }

        rows[[length(rows) + 1]] <- list(
          "factor"              = "",
          "Level"               = "Missing",
          "Frequency"           = alltotal - total,
          "Percent"             = (alltotal - total)/alltotal*100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = ""
        )

        rows[[length(rows) + 1]] <- list(
          "factor"              = "",
          "Level"               = "Total",
          "Frequency"           = alltotal,
          "Percent"             = 100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = ""
        )
      }

    } else {
      t         <- table(column)
      total     <- sum(t)
      cFreq     <- 0
      alltotal  <- length(column)

      for (lev in names(t))
      {
        row                         <- list()
        row[["Level"]]              <- lev
        row[["Frequency"]]          <- as.numeric(t[lev])
        cFreq                       <- cFreq + row[["Frequency"]]
        row[["Percent"]]            <- row[["Frequency"]]/alltotal*100
        row[["Valid Percent"]]      <- row[["Frequency"]]/total*100
        row[["Cumulative Percent"]] <- cFreq/total*100
        rows[[length(rows) + 1]]    <- row
      }

      rows[[length(rows) + 1]] <- list(
        "Level"               = "Missing",
        "Frequency"           = alltotal - total,
        "Percent"             = (alltotal - total)/alltotal*100,
        "Valid Percent"       = "",
        "Cumulative Percent"  = ""
      )

      rows[[length(rows) + 1]] <- list(
        "Level"               = "Total",
        "Frequency"           = alltotal,
        "Percent"             = 100,
        "Valid Percent"       = "",
        "Cumulative Percent"  = ""
      )
    }

    freqTab$addRows(rows)
    freqTab$status <- "complete"
  }
}

.descriptivesMatrixPlot <- function(dataset, options, name)
{
  variables <- unlist(options$variables)
  l         <- length(variables)
  depends   <- c("plotCorrelationMatrix", "variables", "splitBy")

  if (l == 0) #Nothing to plot I guess?
    return(NULL)

  if (nrow(dataset) < 3)
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: Too few rows", dependencies=depends))

  # check variables
  d         <- vector("character",  length(v(variables)))
  sdCheck   <- vector("logical",    length(v(variables)))
  infCheck  <- vector("logical",    length(v(variables)))


  for (i in seq_along(v(variables)))
  {
    variable2check  <- na.omit(dataset[[v(variables)[i]]])
    d[i]            <- class(variable2check)
    sdCheck[i]      <- sd(variable2check) > 0
    infCheck[i]     <- all(is.finite(variable2check))
  }


  numericCheck      <- d == "numeric" | d == "integer"
  variables         <- v(variables)
  variable.statuses <- vector("list", length(variables))

  for (i in seq_along(variables))
  {
    variable.statuses[[i]]$unplotable     <- FALSE
    variable.statuses[[i]]$plottingError  <- NULL

    if (!numericCheck[i])                       variable.statuses[[i]]$plottingError <- "Variable is not continuous"
    else if (!infCheck[i])                      variable.statuses[[i]]$plottingError <- "Variable contains infinity"
    else if (!is.na(sdCheck[i]) & !sdCheck[i])  variable.statuses[[i]]$plottingError <- "Variable has zero variance"

    variable.statuses[[i]]$unplotable <- !is.null(variable.statuses[[i]]$plottingError)
  }

  .matrixPlotFunc <- function()
  {
    if (l == 1) {
      par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 0, 0, 0))
      if (!variable.statuses[[1]]$unplotable)
      {
        .plotMarginalCor(dataset[[variables[1]]])
        mtext(text = unv(variables)[1], side = 1, cex=1.9, line = 3)
      } else
        .displayError(variable.statuses[[1]]$plottingError)
    }
    else
    {
      par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))

      for (row in seq_len(l))
        for (col in seq_len(l))
          if (row == col)
          {
            if (!variable.statuses[[row]]$unplotable) .plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
            else                                      .displayError(variable.statuses[[row]]$plottingError)
          }
          else if (col > row)
          {
            if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable)
              .plotScatterDescriptives(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
            else
            {
              errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
              .displayError(errorMessages[1])
            }
          }
          else
            plot(1, type= "n", axes= FALSE, ylab="", xlab="")

      textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
      for (t in seq_along(textpos))
      {
        mtext(text = unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
        mtext(text = unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
      }
    }
  }

  return(createJaspPlot(plot=.matrixPlotFunc, width=250 * l + 20, aspectRatio=1, title=name, dependencies=depends))
}

.descriptivesFrequencyPlots <- function(dataset, options, variable)
{

  if (options$splitby != "" )
  {
    # return a collection
    split <- names(dataset)

    plotResult <- createJaspContainer(title=variable)
    plotResult$setOptionMustContainDependency("variables",  variable)
    plotResult$setOptionMustBeDependency("splitBy",         options$splitBy)

    for (l in split)
    {
      plotResult[[l]] <- .descriptivesFrequencyPlots_SubFunc(column=dataset[[l]][[v(variable)]], variable=l, width=options$plotWidth, height=options$plotHeight)
      plotResult[[l]]$copyDependenciesFromJaspObject(plotResult)
    }


    return(plotResult)

  }
  else
  {


    column <- dataset[[ v(variable) ]]
    aPlot <- .descriptivesFrequencyPlots_SubFunc(column=column[!is.na(column)], variable=variable, width=options$plotWidth, height=options$plotHeight)
    aPlot$setOptionMustContainDependency("variables",  variable)
    aPlot$setOptionMustBeDependency("splitBy",         options$splitBy)

    return(aPlot)
  }
}

.descriptivesFrequencyPlots_SubFunc <- function(column, variable, width, height)
{
  if (any(is.infinite(column)))   return(createJaspPlot(plot=function() { .barplotJASP(variable=variable, dontPlotData=TRUE) }, title=variable, width=width, height=height, error="badData", errorMessage="Plotting is not possible: Variable contains infinity"))
  else if (length(column) < 3)    return(createJaspPlot(plot=function() { .barplotJASP(variable=variable, dontPlotData=TRUE) }, title=variable, width=width, height=height, error="badData", errorMessage="Plotting is not possible: Too few rows (left)"))
  else if (
    (length(column) > 0 && is.factor(column)) ||
    (is.numeric(column) && all(!is.na(column) & (column %% 1 == 0)) && length(unique(column)) <= 24)
   )
  {
    if (!is.factor(column))
      column <- as.factor(column)

    return(createJaspPlot(plot=function() { .barplotJASP(column, variable) }, title=variable, width=width, height=height))
  }
  else if (length(column) > 0 && !is.factor(column))
    return(createJaspPlot(plot=function() { .plotMarginal(column, variableName=variable) }, title=variable, width=width, height=height))
}

.descriptivesSplitPlot <- function(dataset, options,  variable)
{
  depends <- c("splitPlotColour", "splitPlotViolin", "splitPlotBoxplot", "splitPlotJitter", "splitPlotOutlierLabel")


  # Initialisation plot
  .initSplitPlot <- function()
  {
    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
    axis(2, at=0:1, labels=FALSE, cex.axis= 1.4, ylab="")
    mtext(text = variable, side = 1, cex=1.5, line = 3)
  }

  # Define custom y axis function
  base_breaks_y <- function(x)
  {
    b <- pretty(x)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), size = 0.75, inherit.aes=FALSE), ggplot2::scale_y_continuous(breaks=b))
  }

  # Plot

  # we need to know which index in y is related to which index in the actual data, so we should not forget the NAs somehow, lets make a list of indices.
  yWithNA         <- dataset[[v(variable)]]
  y               <- na.omit(dataset[[v(variable)]])
  yIndexToActual  <- y
  yWithNAIndex    <- 1
  yNoNAIndex      <- 1
  while(yWithNAIndex <= length(yWithNA))
  {
    if(!is.na(yWithNA[[yWithNAIndex]]))
    {
      yIndexToActual[[yNoNAIndex]] <- row.names(dataset)[[yWithNAIndex]]
      yNoNAIndex <- yNoNAIndex + 1
    }

    yWithNAIndex <- yWithNAIndex + 1
  }


  if (!is.numeric(y))
    return(createJaspPlot(plot=.initSplitPlot, title=variable, width=options$plotWidth, height=options$plotHeight, error="badData", errorMessage="Plotting is not possible: Variable is not numeric!", dependencies=depends))
  if (length(y) == 0)
     return(createJaspPlot(plot=.initSplitPlot, title=variable, width=options$plotWidth, height=options$plotHeight, error="badData", errorMessage="Plotting is not possible: Variable only contains NA!", dependencies=depends))
  else if (!(options$splitPlotViolin || options$splitPlotBoxplot || options$splitPlotJitter))
    return(createJaspPlot(plot=.initSplitPlot, title=variable, width=options$plotWidth, height=options$plotHeight, error="badData", errorMessage="Plotting is not possible: No plot type selected!", dependencies=depends))
  else
  {
    if (is.null(dataset[[v(options$splitby)]])){
      group     <- factor(rep("",length(y)))
      xlab      <- "Total"
      boxWidth  <- 0.2
      vioWidth  <- 0.3
    } else {
      group     <- as.factor(dataset[[v(options$splitby)]])[!is.na(dataset[[v(variable)]])]
      xlab      <- options$splitby
      boxWidth  <- 0.4
      vioWidth  <- 0.6
    }

    plotDat <- data.frame(group = group, y = y)
    row.names(plotDat) <- yIndexToActual

    # Identify outliers to label
    plotDat$outlier <- FALSE

    for (level in levels(plotDat$group))
    {
      v         <- plotDat[plotDat$group == level,]$y
      quantiles <- quantile(v, probs=c(0.25,0.75))
      IQR       <- quantiles[2] - quantiles[1]

      plotDat[plotDat$group == level,]$outlier <- v < (quantiles[1]-1.5*IQR) | v > (quantiles[2]+1.5*IQR)
    }

    plotDat$label <- ifelse(plotDat$outlier, row.names(plotDat),"")

    if (options$splitPlotColour)
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x=group, y, fill=group)) + ggplot2::scale_fill_hue(c=60, l=80) + ggplot2::scale_colour_hue(c=60, l=80)
    else
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x=group, y, fill=group)) + ggplot2::scale_fill_manual(values=rep("grey", nlevels(group))) + ggplot2::scale_colour_manual(values=rep("grey", nlevels(group)))

    if (options$splitPlotViolin && options$splitPlotBoxplot && options$splitPlotJitter)
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2) +
        ggplot2::geom_boxplot(size = 0.75, width = boxWidth, outlier.shape = NA) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, fill = "transparent", scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    else if (options$splitPlotBoxplot && options$splitPlotViolin)
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2) +
        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, fill = "transparent", scale = "width")
    else if (options$splitPlotBoxplot && options$splitPlotJitter)
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2 ) +
        ggplot2::geom_boxplot(size = 0.75, outlier.shape = NA, width = boxWidth) +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    else if (options$splitPlotViolin && options$splitPlotJitter)
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = 0.75*boxWidth, scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    else if (options$splitPlotViolin)
      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, scale = "width", width = 0.75*boxWidth)
    else if (options$splitPlotBoxplot)
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar",size = 0.75, width = boxWidth/2 ) +
        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth)
    else if (options$splitPlotJitter)
      p <- p + ggplot2::geom_jitter(size = 2.5, ggplot2::aes(colour = group), position = ggplot2::position_jitter(width=0.1, height = 0))

    if (options$splitPlotOutlierLabel && (options$splitPlotBoxplot || options$splitPlotJitter))
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label=label), hjust=-0.3)


    ### Theming & Cleaning
    p <- p +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(variable) +
      base_breaks_y(y) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor=   ggplot2::element_blank(),
        plot.title=         ggplot2::element_text(size = 18),
        panel.grid.major=   ggplot2::element_blank(),
        axis.title.x=       ggplot2::element_text(size = 18, vjust=0.1),
        axis.title.y=       ggplot2::element_text(size = 18, vjust=0.9),
        axis.text.x=        ggplot2::element_text(size = 15),
        axis.text.y=        ggplot2::element_text(size = 15),
        panel.background=   ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background=    ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.background=  ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.border=       ggplot2::element_blank(),
        axis.line=          ggplot2::element_blank(),
        legend.key=         ggplot2::element_blank(),
        axis.ticks=         ggplot2::element_line(size = 0.5),
        axis.ticks.length=  grid::unit(3, "mm"),
        axis.ticks.margin=  grid::unit(1,"mm"),
        plot.margin=        grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
        legend.position=    "none")

    return(createJaspPlot(plot=p, title=variable, width=options$plotWidth, height=options$plotHeight, dependencies=depends))
  }
}


# <editor-fold> HELPER FUNCTIONS BLOCK ----

.plotMarginal <- function(variable, variableName, cexYlab= 1.3, lwd= 2, rugs= FALSE){

  variable <- na.omit(variable)

  par(mar= c(5, 4.5, 4, 2) + 0.1)

  density <- density(variable)
  h       <- hist(variable, plot = FALSE)
  jitVar  <- jitter(variable)
  yhigh   <- max(max(h$density), max(density$y))
  ylow    <- 0
  xticks  <- pretty(c(variable, h$breaks), min.n= 3)

  plot(1, xlim= range(xticks), ylim= c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")

  h       <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
  ax1     <- axis(1, line = 0.3, at= xticks, lab= xticks, cex.axis = 1.2)
  mtext(text = variableName, side = 1, cex=1.5, line = 3)
  par(las=0)
  ax2     <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), cex.axis= 1.5, mgp= c(3, 0.7, 0))

  if(rugs)
    rug(jitVar)

  lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)
}

.barplotJASP <- function(column, variable, dontPlotData= FALSE){

  if (dontPlotData) {

    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

    axis(1, at=0:1, labels=FALSE, cex.axis= 1.4, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis= 1.4, ylab="")

    mtext(text = variable, side = 1, cex=1.5, line = 3)

    return()
  }

  maxFrequency <- max(summary(column))

  i <- 1
  step <- 1

  while (maxFrequency / step > 9) {

    if (i == 2) {

      step <- 2 * step
      i <- i + 1

    } else if (i %% 3 == 0) {

      step <- 2.5 * step
      i <- i + 1

    } else {

      step <- 2 * step
      i <- i + 1
    }

  }

  yticks <- 0

  while (yticks[length(yticks)] < maxFrequency) {

    yticks <- c(yticks, yticks[length(yticks)] + step)
  }


  yLabs <- vector("character", length(yticks))

  for(i in seq_along(yticks))
    if(yticks[i] < 10^6)
      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
    else
      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)

  distLab <- max(nchar(yLabs))/1.8

  par(mar= c(5, 7.2, 4, 2) + 0.1)
  barplot(summary(column), cex.names= 1.3, axes= FALSE, ylim= range(yticks))
  axis(2, las=1, at= yticks, labels= yLabs, cex.axis= 1.4)
  mtext(text = variable, side = 1, cex=1.5, line = 3)
  mtext(text = "Frequency", side = 2, cex=1.5, line = distLab+2, las=0)
}

.plotScatterDescriptives <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2){

  d     <- data.frame(xx= xVar, yy= yVar)
  d     <- na.omit(d)
  xVar  <- d$xx
  yVar  <- d$yy

  # fit different types of regression
  fit <- vector("list", 1)# vector("list", 4)

  fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
  fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
  fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
  fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)

  # find parsimonious, best fitting regression model
  Bic <- vector("numeric", 4)

  for (i in 1:4)
    Bic[i] <- BIC(fit[[i]])



  bestModel <- which.min(Bic)

  xlow    <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
  xhigh   <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
  xticks  <- pretty(c(xlow, xhigh))

  ylow    <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
  yhigh   <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
  yticks  <- pretty(c(ylow, yhigh))

  plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)

  .poly.pred(fit[[bestModel]], line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)

  par(las=1)

  axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis)
  axis(2, line= 0.2, labels= yticks, at= yticks, cex.axis= cexYAxis)
}

.descriptivesKurtosis <- function(x) {

  # Kurtosis function as in SPSS:
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
  # http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis

  n   <- length(x)
  s4  <- sum((x - mean(x))^4)
  s2  <- sum((x - mean(x))^2)
  v   <- s2 / (n-1)
  a   <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  b   <- s4 / (v^2)
  c   <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))

  kurtosis <- a * b + c

  return(kurtosis)
}

.descriptivesSkewness <- function(x) {

  # Skewness function as in SPSS (for samlpes spaces):
  # http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005

  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  z <- (x - m) / s  # z scores
  a <- n / ((n - 1) * (n - 2))

  skewness <- sum(z^3) * a

  return(skewness)
}

.descriptivesSES <- function(x) {

  # Standard Error of Skewness
  # Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf

  n   <- length(x)
  SES <- sqrt((6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))))

  return(SES)
}

.descriptivesSEK <- function(x) {

  # Standard Error of Kurtosis
  # Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf

  n   <- length(x)
  SEK <- 2 * .descriptivesSES(x) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))

  return(SEK)
}

# </editor-fold> HELPER FUNCTIONS BLOCK
