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

Descriptives <- function(jaspResults, dataset, options) {
  variables <- unlist(options$variables)
  splitName <- options$splitby
  makeSplit <- splitName != ""
  numberMissingSplitBy <- 0

  if (is.null(dataset)) {
    if (makeSplit) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = splitName)
      dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=splitName)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric=variables)
      dataset.factors <- .readDataSetToEnd(columns=variables)
    }
  }

  if (makeSplit && length(variables) > 0) {
    splitFactor      <- dataset[[.v(splitName)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]
    
    numberMissingSplitBy <- sum(is.na(splitFactor))
    
    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)
    # create a list of datasets, one for each level
    splitDat         <- split(dataset[.v(variables)],         splitFactor)
    splitDat.factors <- split(dataset.factors[.v(variables)], splitFactor)
  }

  .descriptivesDescriptivesTable(dataset, options, jaspResults, numberMissingSplitBy=numberMissingSplitBy)

  # Frequency table
  if (options$frequencyTables) {
    if(is.null(jaspResults[["tables"]])) {
      jaspResults[["tables"]] <- createJaspContainer(gettext("Frequency Tables"))
      jaspResults[["tables"]]$dependOn(c("frequencyTables", "splitby"))
      jaspResults[["tables"]]$position <- 3
    }

    .descriptivesFrequencyTables(dataset.factors, options, jaspResults[["tables"]])
  }

  # Correlation plot
  if (options$plotCorrelationMatrix && length(variables) > 1) {
    if(is.null(jaspResults[["matrixPlot"]])) {
      if (makeSplit) {
        jaspResults[["matrixPlot"]] <- createJaspContainer(title=gettext("Correlation plots"))
        corrPlot <- jaspResults[["matrixPlot"]]
        corrPlot$dependOn(c("plotCorrelationMatrix", "splitby", "variables"))

        for (i in 1:length(splitLevels))
          corrPlot[[splitLevels[i]]] <- .descriptivesMatrixPlot(splitDat.factors[[i]], options, splitLevels[i])

      } else {
        jaspResults[["matrixPlot"]] <- .descriptivesMatrixPlot(dataset.factors, options, gettext("Correlation plot")) # Create one plot
      }

      jaspResults[["matrixPlot"]]$position <- 6
    }
  }

  # Distribution plots
  if (options$plotVariables) {
    if(is.null(jaspResults[["distributionPlots"]])) {
      jaspResults[["distributionPlots"]] <- createJaspContainer(gettext("Distribution Plots"))
      jaspResults[["distributionPlots"]]$dependOn(c("plotVariables", "splitby", "distPlotDensity", "distPlotRug"))
      jaspResults[["distributionPlots"]]$position <- 5
    }

    distPlots <- jaspResults[["distributionPlots"]]

    for (var in variables) {
      if(is.null(distPlots[[var]])) {
        if (makeSplit) {
          distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = splitDat.factors, options = options, variable = var)
        } else {
          distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = dataset.factors, options = options, variable = var)
        }
      }
    }
  }

  # Box plots
  if (options$splitPlots) {
    if(is.null(jaspResults[["splitPlots"]])) {
      jaspResults[["splitPlots"]] <- createJaspContainer(gettext("Boxplots"))
      jaspResults[["splitPlots"]]$dependOn(c("splitPlots", "splitby"))
      jaspResults[["splitPlots"]]$position <- 7
    }

    splitPlots <- jaspResults[["splitPlots"]]

    for (var in variables) {
      if(is.null(splitPlots[[var]]) && .descriptivesIsNumericColumn(dataset.factors, var)) {
        splitPlots[[var]] <- .descriptivesSplitPlot(dataset = dataset, options = options, variable = var)
        splitPlots[[var]]$dependOn(optionContainsValue=list(variables=var))
      }
    }
  }

  # QQ plots
  if (options$descriptivesQQPlot) {
    if(is.null(jaspResults[["QQPlots"]])) {
      if(length(variables)>1 || length(levels(dataset[[.v(splitName)]]))>1) #there will be more than one Q-Q Plot
        jaspResults[["QQPlots"]] <- createJaspContainer(gettext("Q-Q Plots"))
      else #only one Q-Q Plot
        jaspResults[["QQPlots"]] <- createJaspContainer(gettext("Q-Q Plot"))
      jaspResults[["QQPlots"]]$dependOn(c("descriptivesQQPlot", "splitby"))
      jaspResults[["QQPlots"]]$position <- 8
    }
    QQPlots <- jaspResults[["QQPlots"]]
    if(makeSplit) {
      qqSplitFactor     <- dataset[[.v(splitName)]]
      if(length(qqSplitFactor)==0)
        return(createJaspPlot(error=gettext("Plotting is not possible: Variable only contains NA!"), dependencies="splitby"))
      #gives the different split values
      qqSplitLevels     <- levels(qqSplitFactor)
      # remove missing values from the grouping variable
      dataset           <- dataset[!is.na(qqSplitFactor), ]
      for(var in variables){
        if(!is.null(QQPlots[[var]]) || !.descriptivesIsNumericColumn(dataset.factors, var))
          next
        deeperQQPlots <- createJaspContainer(paste0(var))
        deeperQQPlots$dependOn(optionContainsValue=list(variables=var))
        QQPlots[[var]] <- deeperQQPlots
        #splits dataset according to split values
        qqSplitData     <- split(dataset, qqSplitFactor)
        for( lev in 1:length(qqSplitLevels)){
          QQPlots[[var]][[paste0(var, lev)]] <- .descriptivesQQPlot(dataset=qqSplitData[[lev]], options=options, qqvar=var, levelName=qqSplitLevels[lev])
        }
      }
    }
    else { #no split
      for(var in variables){
        if(is.null(QQPlots[[var]]) && .descriptivesIsNumericColumn(dataset.factors, var)) {
          QQPlots[[var]] <- .descriptivesQQPlot(dataset=dataset, options=options, qqvar=var)
        }
      }
    }
  }

  # Pie charts
  if (options$descriptivesPiechart) {

    if(is.null(jaspResults[["pieCharts"]])) {
      jaspResults[["pieCharts"]] <- createJaspContainer(gettext("Pie charts"))
      jaspResults[["pieCharts"]]$dependOn(c("splitby", "descriptivesPiechart", "colorPalette"))
      jaspResults[["pieCharts"]]$position <- 9
    }

    piePlots <- jaspResults[["pieCharts"]]
    JASPgraphs::setGraphOption("palette", options[["colorPalette"]])
    for (var in variables) {
      # skip non-categorical variables
      if(is.double(dataset.factors[[.v(var)]]))next
      
      if(is.null(piePlots[[var]])) {
        if (makeSplit) {
          piePlots[[var]] <- .descriptivesPieChart(dataset = splitDat.factors, options = options, variable = var)
        } else {
          piePlots[[var]] <- .descriptivesPieChart(dataset = dataset.factors, options = options, variable = var)
        }
      }
    }
  }

  # Scatter plots
  if (options[["scatterPlot"]]) {
    if(is.null(jaspResults[["scatterPlots"]])) {
      jaspResults[["scatterPlots"]] <- createJaspContainer(gettext("Scatter Plots"))
      jaspResults[["scatterPlots"]]$dependOn(c("splitby", "scatterPlot", "graphTypeAbove", "graphTypeRight", "addSmooth",
                                               "addSmoothCI", "addSmoothCIValue", "regressionType", "showLegend",
                                               "colorPalette"))
      jaspResults[["scatterPlots"]]$position <- 10
    }
    .descriptivesScatterPlots(jaspResults[["scatterPlots"]], dataset.factors, variables, splitName, options)
  }
  return()
}

.descriptivesDescriptivesTable <- function(dataset, options, jaspResults, numberMissingSplitBy=0) {
  if (!is.null(jaspResults[["stats"]])) return() #The options for this table didn't change so we don't need to rebuild it

  wantsSplit              <- options$splitby != ""
  variables               <- unlist(options$variables)
  equalGroupsNo           <- options$percentileValuesEqualGroupsNo
  percentilesPercentiles  <- unique(options$percentileValuesPercentilesPercentiles)
  stats                   <- createJaspTable(gettext("Descriptive Statistics"))
  stats$transpose         <- TRUE
  stats$position          <- 1
  
  if (numberMissingSplitBy) 
    stats$addFootnote(message=gettextf("Excluded %1$i rows from the analysis that correspond to the missing values of the split-by variable %2$s", numberMissingSplitBy, options$splitby))
  

  stats$dependOn(c("splitby", "variables", "percentileValuesEqualGroupsNo", "percentileValuesPercentilesPercentiles", "mean", "standardErrorMean",
    "median", "mode", "standardDeviation", "variance", "skewness", "kurtosis", "shapiro", "range", "iqr", "mad","madrobust", "minimum", "maximum", "sum", "percentileValuesQuartiles", "percentileValuesEqualGroups", "percentileValuesPercentiles"))

  if (wantsSplit) {
    stats$transposeWithOvertitle <- TRUE
    stats$addColumnInfo(name="Variable",  title="", type="string")
    stats$addColumnInfo(name="Level",     title="", type="string")
  } else {
    stats$addColumnInfo(name="Variable",  title="", type="string")
  }

  stats$addColumnInfo(name="Valid",   title=gettext("Valid"), type="integer")
  stats$addColumnInfo(name="Missing", title=gettext("Missing"), type="integer")

  if (options$mean)                 stats$addColumnInfo(name="Mean",                        title=gettext("Mean"), 				            type="number")
  if (options$standardErrorMean)    stats$addColumnInfo(name="Std. Error of Mean",          title=gettext("Std. Error of Mean"),      type="number")
  if (options$median)               stats$addColumnInfo(name="Median",                      title=gettext("Median"),                  type="number")
  if (options$mode)                 stats$addColumnInfo(name="Mode",                        title=gettext("Mode"),                    type="number")
  if (options$standardDeviation)    stats$addColumnInfo(name="Std. Deviation",              title=gettext("Std. Deviation"),          type="number")
  if (options$mad)                  stats$addColumnInfo(name="MAD",                         title=gettext("MAD"),                     type="number")
  if (options$madrobust)            stats$addColumnInfo(name="MAD Robust",                  title=gettext("MAD Robust"),              type="number")
  if (options$iqr)                  stats$addColumnInfo(name="IQR",                         title=gettext("IQR"),                     type="number")
  if (options$variance)             stats$addColumnInfo(name="Variance",                    title=gettext("Variance"),                type="number")
  if (options$skewness) {           stats$addColumnInfo(name="Skewness",                    title=gettext("Skewness"),                type="number")
                                    stats$addColumnInfo(name="Std. Error of Skewness",      title=gettext("Std. Error of Skewness"),  type="number") }
  if (options$kurtosis) {           stats$addColumnInfo(name="Kurtosis",                    title=gettext("Kurtosis"),                type="number")
                                    stats$addColumnInfo(name="Std. Error of Kurtosis",      title=gettext("Std. Error of Kurtosis"),  type="number") }
  if (options$shapiro) {            stats$addColumnInfo(name="Shapiro-Wilk",                title=gettext("Shapiro-Wilk"),            type="number")
                                    stats$addColumnInfo(name="P-value of Shapiro-Wilk",     title=gettext("P-value of Shapiro-Wilk"), type="pvalue") }
  if (options$range)                stats$addColumnInfo(name="Range",                       title=gettext("Range"),                   type="number")
  if (options$minimum)              stats$addColumnInfo(name="Minimum",                     title=gettext("Minimum"),                 type="number")
  if (options$maximum)              stats$addColumnInfo(name="Maximum",                     title=gettext("Maximum"),                 type="number")
  if (options$sum)                  stats$addColumnInfo(name="Sum",                         title=gettext("Sum"),                     type="number")

  if (options$percentileValuesQuartiles) {
                                    stats$addColumnInfo(name="q1", title="25th percentile", type="number")
                                    stats$addColumnInfo(name="q2", title="50th percentile", type="number")
                                    stats$addColumnInfo(name="q3", title="75th percentile", type="number")
  }

  if (options$percentileValuesEqualGroups)  {# I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way
    tempPercentNames <- 1/equalGroupsNo * 1:(equalGroupsNo-1) * 100
    
    for (i in seq_along(tempPercentNames)) 
      stats$addColumnInfo(name=paste("eg", i, sep=""), title=gettextf("%gth percentile", round(tempPercentNames[i], 2)), type="number")
    
  }

  if (options$percentileValuesPercentiles) {
    
    for (i in percentilesPercentiles) 
      stats$addColumnInfo(name=paste("pc", i, sep=""), title=gettextf("%gth percentile", i), type="number")
    
  }
  
  jaspResults[["stats"]] <- stats
  
  # lets just add footnotes once instead of a gazillion times..
  shouldAddNominalTextFootnote      <- FALSE
  shouldAddModeMoreThanOnceFootnote <- FALSE

  # Find the number of levels to loop over
  if (wantsSplit) {
    split       <- dataset[[.v(options$splitby)]]
    splitLevels <- levels(split)
    nLevels     <- length(levels(split))
    
    for (variable in variables) {
      for (l in 1:nLevels) {
        column    <- dataset[[ .v(variable) ]][split==splitLevels[l]]
        subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable = variable, Level = splitLevels[l]), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote)
        
        shouldAddNominalTextFootnote      <- subReturn$shouldAddNominalTextFootnote
        shouldAddModeMoreThanOnceFootnote <- subReturn$shouldAddModeMoreThanOnceFootnote
        
        stats$addRows(subReturn$resultsCol, rowNames = paste0(variable, l))
        
        if (subReturn$shouldAddIdenticalFootnote)
          stats$addFootnote(message = gettext("All values are identical"),
                            colNames = c("Skewness", "Kurtosis", "Shapiro-Wilk", "P-value of Shapiro-Wilk"),
                            rowNames = paste0(variable, l))
        
        if (subReturn$shouldAddExplainEmptySet)
          stats$addFootnote(message  = gettextf("Infimum (minimum) of an empty set is %s, supremum (maximum) of an empty set is %s.", "\u221E", "-\u221E"),
                            colNames = c("Minimum", "Maximum"),
                            rowNames = paste0(variable, l))
      }
    }
  } else { #we dont want to split
    for (variable in variables) {
      column    <- dataset[[.v(variable)]]
      subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable=variable), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote)

      shouldAddNominalTextFootnote      <- subReturn$shouldAddNominalTextFootnote
      shouldAddModeMoreThanOnceFootnote <- subReturn$shouldAddModeMoreThanOnceFootnote

      stats$addRows(subReturn$resultsCol, rowNames = variable)
      
      if (subReturn$shouldAddIdenticalFootnote)
        stats$addFootnote(message = gettext("All values are identical"),
                          colNames = c("Skewness", "Kurtosis", "Shapiro-Wilk", "P-value of Shapiro-Wilk"),
                          rowNames = variable)
      
      if (subReturn$shouldAddExplainEmptySet)
        stats$addFootnote(message  = gettextf("Infimum (minimum) of an empty set is %s, supremum (maximum) of an empty set is %s.", "\u221E", "-\u221E"),
                          colNames = c("Minimum", "Maximum"),
                          rowNames = variable)
    }
  }


  if (shouldAddNominalTextFootnote) 
    stats$addFootnote(message=gettext("Not all values are available for <i>Nominal Text</i> variables"))
  
  if(shouldAddModeMoreThanOnceFootnote) 
    stats$addFootnote(message=gettext("More than one mode exists, only the first is reported"), colNames="Mode")
  
  return(stats)
}

.descriptivesDescriptivesTable_subFunction <- function(column, resultsCol, options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote) {
  equalGroupsNo           <- options$percentileValuesEqualGroupsNo
  percentilesPercentiles  <- unique(options$percentileValuesPercentilesPercentiles)

  rows        <- length(column)
  na.omitted  <- na.omit(column)

  resultsCol[["Valid"]]   <- length(na.omitted)
  resultsCol[["Missing"]] <- rows - length(na.omitted)

  if (base::is.factor(na.omitted) && (options$mean || options$mode || options$median || options$minimum || options$standardErrorMean || options$iqr || options$mad || options$madrobust || options$kurtosis || options$shapiro || options$skewness || options$percentileValuesQuartiles || options$variance || options$standardDeviation || options$percentileValuesPercentiles || options$sum || options$maximum)) {
    shouldAddNominalTextFootnote <- TRUE
  }
  
  shouldAddIdenticalFootnote <- all(na.omitted[1] == na.omitted) && (options$skewness || options$kurtosis || options$shapiro)  
  
  resultsCol[["Mean"]]                    <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$mean,              na.omitted, mean)
  resultsCol[["Std. Error of Mean"]]      <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$standardErrorMean, na.omitted, function(param) { sd(param)/sqrt(length(param))} )
  resultsCol[["Median"]]                  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$median,            na.omitted, median)
  resultsCol[["Std. Deviation"]]          <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$standardDeviation, na.omitted, sd)
  resultsCol[["MAD"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$mad,               na.omitted, function(param) { mad(param, constant = 1) } )
  resultsCol[["MAD Robust"]]              <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$madrobust,         na.omitted, mad)
  resultsCol[["IQR"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$iqr,               na.omitted, .descriptivesIqr)
  resultsCol[["Variance"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$variance,          na.omitted, var)
  resultsCol[["Kurtosis"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesKurtosis)
  resultsCol[["Std. Error of Kurtosis"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesSEK)
  resultsCol[["Skewness"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSkewness)
  resultsCol[["Std. Error of Skewness"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSES)
  resultsCol[["Shapiro-Wilk"]]            <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$shapiro,           na.omitted, function(param) { res <- try(shapiro.test(param)$statistic); if(isTryError(res)) NA else res })
  resultsCol[["P-value of Shapiro-Wilk"]] <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$shapiro,           na.omitted, function(param) { res <- try(shapiro.test(param)$p.value);   if(isTryError(res)) NA else res })
  resultsCol[["Range"]]                   <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$range,             na.omitted, function(param) { range(param)[2] - range(param)[1]})
  resultsCol[["Minimum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$minimum,           na.omitted, min)
  resultsCol[["Maximum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$maximum,           na.omitted, max)
  resultsCol[["Sum"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$sum,               na.omitted, sum)

  # should explain supremum and infimum of an empty set?
  if((options$minimum || options$maximum) && resultsCol[['Valid']] == 0) shouldAddExplainEmptySet <- TRUE else shouldAddExplainEmptySet <- FALSE
  
  if (options$mode) {
    if (base::is.factor(na.omitted) == FALSE) {
      mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))

      if (length(mode) > 1) 
        shouldAddModeMoreThanOnceFootnote <- TRUE

      resultsCol[["Mode"]] <- .clean(mode[1])
    } else {
      resultsCol[["Mode"]] <- ""
    }
  } else {
    resultsCol[["Mode"]] <- NULL
  }
    
  if (options$percentileValuesQuartiles) {
    if (base::is.factor(na.omitted) == FALSE) {
      resultsCol[["q1"]] <- .clean(quantile(na.omitted, c(.25), names=F))
      resultsCol[["q2"]] <- .clean(quantile(na.omitted, c(.5), names=F))
      resultsCol[["q3"]] <- .clean(quantile(na.omitted, c(.75), names=F))
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
    
  for (row in names(resultsCol)) {
    
    if (substr(row, 1, 2) == "eg" && ((row %in% equalGroupNames) == FALSE)) 
      resultsCol[[row]] <- NULL

    if (substr(row, 1, 2) == "pc" && ((row %in% percentileNames) == FALSE)) 
      resultsCol[[row]] <- NULL
    
  }

  if (base::is.factor(na.omitted) == FALSE) {
    if (options$percentileValuesEqualGroups) {
      
      for (i in seq(equalGroupsNo - 1)) 
        resultsCol[[paste("eg", i, sep="")]] <- .clean(quantile(na.omitted, c(i / equalGroupsNo), names=F))
      
    }
    
    if (options$percentileValuesPercentiles) {
      
      for (i in percentilesPercentiles) 
        resultsCol[[paste("pc", i, sep="")]] <- .clean(quantile(na.omitted, c(i / 100), names=F))

    }
  } else {
    if (options$percentileValuesEqualGroups) {
      
      for (i in seq(equalGroupsNo - 1)) 
        resultsCol[[paste("eg", i, sep="")]] <- ""

    }

    if (options$percentileValuesPercentiles) {
      
      for (i in percentilesPercentiles) 
        resultsCol[[paste("pc", i, sep="")]] <- ""
      
    }
  }

  return(list(resultsCol=resultsCol, 
              shouldAddNominalTextFootnote=shouldAddNominalTextFootnote,
              shouldAddModeMoreThanOnceFootnote=shouldAddModeMoreThanOnceFootnote,
              shouldAddIdenticalFootnote=shouldAddIdenticalFootnote,
              shouldAddExplainEmptySet=shouldAddExplainEmptySet))
}


.descriptivesDescriptivesTable_subFunction_OptionChecker <- function(optionToCheck, na.omitted, function_to_use) {
  if (!optionToCheck)
    return(NULL)

  if (base::is.factor(na.omitted))
    return("")

  return(.clean(function_to_use(na.omitted)))
}

.descriptivesFrequencyTables <- function(dataset, options, freqTabs) {
  splitName   <- options$splitby
  wantsSplit  <- splitName!=""
  splitFactor <- dataset[[.v(splitName)]]
  splitLevels <- levels(splitFactor)

  for (variable in options$variables) {
    column <- dataset[[.v(variable)]]
    
    if (!is.factor(column)) 
      next

    if(!is.null(freqTabs[[variable]]))
      next

    freqTab <- createJaspTable(gettextf("Frequencies for %s", variable))
    freqTab$dependOn(optionContainsValue=list(variables=variable))

    if (wantsSplit) freqTab$addColumnInfo(name = "factor", title = splitName, type = "string", combine=TRUE)

    freqTab$addColumnInfo(name="Level",               title=variable,                       type="string")
    freqTab$addColumnInfo(name="Frequency",           title=gettext("Frequency"),           type="integer")
    freqTab$addColumnInfo(name="Percent",             title=gettext("Percent"),             type="number", format="dp:1")
    freqTab$addColumnInfo(name="Valid Percent",       title=gettext("Valid Percent"),       type="number", format="dp:1")
    freqTab$addColumnInfo(name="Cumulative Percent",  title=gettext("Cumulative Percent"),  type="number", format="dp:1")
  
    freqTabs[[variable]] <- freqTab
    
    rows <- list()

    if (wantsSplit) {
      for (lev in splitLevels){  # also loop over the levels
        t         <- table(column[splitFactor==lev])
        total     <- sum(t)
        alltotal  <- length(column[splitFactor==lev])
        cFreq     <- 0

        for (i in seq_along(names(t))) {
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
          "Level"               = gettext("Missing"),
          "Frequency"           = alltotal - total,
          "Percent"             = (alltotal - total)/alltotal*100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = "",
          ".isNewGroup"         = FALSE
        )

        rows[[length(rows) + 1]] <- list(
          "factor"              = "",
          "Level"               = gettext("Total"),
          "Frequency"           = alltotal,
          "Percent"             = 100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = "",
          ".isNewGroup"         = FALSE
        )
      }
    } else {
      t         <- table(column)
      total     <- sum(t)
      cFreq     <- 0
      alltotal  <- length(column)

      for (lev in names(t)) {
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
        "Level"               = gettext("Missing"),
        "Frequency"           = alltotal - total,
        "Percent"             = (alltotal - total)/alltotal*100,
        "Valid Percent"       = "",
        "Cumulative Percent"  = ""
      )

      rows[[length(rows) + 1]] <- list(
        "Level"               = gettext("Total"),
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

.descriptivesMatrixPlot <- function(dataset, options, name) {
  variables <- unlist(options$variables)

  l         <- length(variables)
  depends   <- c("plotCorrelationMatrix", "variables", "splitby")

  if (l == 0) #Nothing to plot
    return(NULL)

  variable.statuses <- vector("list", length(variables))

  for (i in seq_along(variables)) {
    errorMessage <- .descriptivesCheckPlotErrors(dataset, variables[i], obsAmount = "< 3")
    if (!is.null(errorMessage))
      variable.statuses[[i]]$error <- errorMessage
    else
      variable.statuses[[i]]$error <- ""
  }

  plotMat <- matrix(list(), l, l)
  axisBreaks <- vector("list", l)

  # minor adjustments to plot margin to avoid cutting off the x-axis labels
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm"))

  oldFontSize <- JASPgraphs::getGraphOption("fontsize")
  JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)

  # first do the diagonal and store breaks
  for (row in seq_along(variables)) {
    if (variable.statuses[[row]]$error != "") {
      plotMat[[row, row]] <- .displayError(errorMessage=variable.statuses[[row]]$error)
    } else {
      plotMat[[row, row]] <- .plotMarginalCorDescriptives(dataset[[.v(variables[[row]])]]) + adjMargin
      axisBreaks[[row]] <- JASPgraphs::getAxisBreaks(plotMat[[row, row]])
    }
  }

  # now do off-diagonal and use the same breaks
  for (row in seq_len(l-1)) {
    for (col in seq(row+1, l)) {
      if (variable.statuses[[row]]$error != "") {
        plotMat[[row, col]] <- .displayError(errorMessage=variable.statuses[[row]]$error)
      } else if (variable.statuses[[col]]$error != "") {
        plotMat[[row, col]] <- .displayError(errorMessage=variable.statuses[[col]]$error)
      } else {
        plotMat[[row, col]] <- .plotScatterDescriptives(
          xVar    = dataset[[.v(variables[[col]])]],
          yVar    = dataset[[.v(variables[[row]])]],
          xBreaks = axisBreaks[[col]]$x,
          yBreaks = axisBreaks[[row]]$x
        ) + adjMargin
      }
    }
  }

  JASPgraphs::setGraphOption("fontsize", oldFontSize)

  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = variables, topLabels = variables,
                                scaleXYlabels = NULL, labelPos = labelPos)

  return(createJaspPlot(plot=p, width=250 * l + 20, aspectRatio=1, title=name, dependencies=depends))
}

# temporaryly copied from correlation.R in koenderks
#### histogram with density estimator ####
.plotMarginalCorDescriptives <- function(variable, xName = NULL, yName = gettext("Density")) {
  variable <- na.omit(variable)
  isNumeric <- !(is.factor(variable) || (is.integer(variable) && length(unique(variable)) <= 10))

  if (isNumeric) {
    p <- ggplot2::ggplot(data = data.frame(x = variable))
    h <- hist(variable, plot = FALSE)
    hdiff <- h$breaks[2L] - h$breaks[1L]
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(variable, h$breaks), min.n = 3)
    dens <- h$density
    yBreaks <- c(0, 1.2*max(h$density))

    p <- p + ggplot2::geom_histogram(
      mapping  = ggplot2::aes(x = x, y = ..density..),
      binwidth = hdiff,
      fill     = "grey",
      col      = "black",
      size     = .3,
      center   = hdiff / 2,
      stat     = "bin"
    ) +
      ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
  } else {

    p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
    hdiff <- 1L
    xBreaks <- unique(variable)
    yBreaks <- c(0, max(table(variable)))

    p <- p + ggplot2::geom_bar(
      mapping  = ggplot2::aes(x = x),
      fill     = "grey",
      col      = "black",
      size     = .3,
      stat     = "count"
    ) +
      ggplot2::scale_x_discrete(name = xName, breaks = xBreaks)
  }

  yLim <- range(yBreaks)

  if (isNumeric) {
    density <- density(variable)
    p <- p + ggplot2::geom_line(data = data.frame(x = density$x, y = density$y),
                                mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black")
  }

  thm <- ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  )
  p <- p +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = c("", ""), limits = yLim) +
    ggplot2::theme()
  return(JASPgraphs::themeJasp(p) + thm)
}

.poly.predDescriptives <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)

  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }

    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
      f <- paste(f, temp, sep="+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text=f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd)
    return(plot)
  }
}


.plotScatterDescriptives <- function(xVar, yVar, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL) {
  
  isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
  isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
  bothNumeric <- isNumericX && isNumericY
  d <- data.frame(x = xVar, y = yVar)
  d <- na.omit(d)

  if (!isNumericX)
    d$x <- as.factor(d$x)

  if (!isNumericY)
    d$y <- as.factor(d$y)

  if (is.null(xBreaks))
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)

  fit <- NULL
  
  if (bothNumeric) {
    fit <- lm(y ~ poly(x, 1, raw = TRUE), d)
    lineObj <- .poly.predDescriptives(fit, line = FALSE, xMin= xBreaks[1], xMax = xBreaks[length(xBreaks)], lwd = lwd)
    rangeLineObj <- c(lineObj[1], lineObj[length(lineObj)])
    yLimits <- range(c(pretty(yVar)), rangeLineObj)

    if (!all(is.na(yLimits))) { # this is NA in case both x and y only contain a single unique value
      if (is.null(yBreaks) || yLimits[1L] <= yBreaks[1L] || yLimits[2L] >= yBreaks[length(yBreaks)])
        yBreaks <- JASPgraphs::getPrettyAxisBreaks(yLimits)
    }
  } else if (is.null(yBreaks)) {
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
  }

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    JASPgraphs::geom_point()

  if (bothNumeric) {
    xr <- range(xBreaks)
    dfLine <- data.frame(x = xr, y = rangeLineObj)
    p <- p + ggplot2::geom_line(data = dfLine, ggplot2::aes(x = x, y = y), size = .7, inherit.aes = FALSE)
  }

  if (isNumericX) {
    p <- p + ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
  } else {
    p <- p + ggplot2::scale_x_discrete(name = xName)
  }
  if (isNumericY) {
    p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_discrete(name = yName)
  }

  return(JASPgraphs::themeJasp(p))
}


### empty Plot with error message ###
.displayErrorDescriptives <- function(errorMessage=NULL) {
  df <- data.frame(
    x = 0, y = 1,
    # automatically places \n after about 40 characters (but does not split words)
    label = stringr::str_wrap(errorMessage, width = 40)
  )
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::geom_text(size = .4*JASPgraphs::getGraphOption("fontsize")) +
    JASPgraphs::getEmptyTheme()
  return(p)
}


.descriptivesFrequencyPlots <- function(dataset, options, variable) {
  
  if (options$splitby != "" ) {
    # return a collection
    split <- names(dataset)

    plotResult <- createJaspContainer(title=variable)
    plotResult$dependOn(options=c("splitby", "binWidthType"), optionContainsValue=list(variables=variable))

    for (l in split) {
      plotResult[[l]] <- .descriptivesFrequencyPlots_SubFunc(dataset=dataset[[l]], variable=variable, width=options$plotWidth, height=options$plotHeight, displayDensity = options$distPlotDensity, rugs = options$distPlotRug, title = l, binWidthType = options$binWidthType)
      plotResult[[l]]$dependOn(optionsFromObject=plotResult)
    }

    return(plotResult)
  } else {
    column <- dataset[[.v(variable)]]
    aPlot <- .descriptivesFrequencyPlots_SubFunc(dataset=dataset, variable=variable, width=options$plotWidth, height=options$plotHeight, displayDensity = options$distPlotDensity, rugs = options$distPlotRug, title = variable, binWidthType = options$binWidthType)
    aPlot$dependOn(options=c("splitby", "binWidthType"), optionContainsValue=list(variables=variable))

    return(aPlot)
  }
}

.descriptivesFrequencyPlots_SubFunc <- function(dataset, variable, width, height, displayDensity, rugs, title, binWidthType = NA) {
  freqPlot <- createJaspPlot(title=title, width=width, height=height)
  
  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 3")
  column <- dataset[[.v(variable)]]
  column <- column[!is.na(column)]
  if (!is.null(errorMessage))
    freqPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  else if (length(column) > 0 && is.factor(column))
    freqPlot$plotObject <- .barplotJASP(column, variable)
  else if (length(column) > 0 && !is.factor(column))
    freqPlot$plotObject <- .plotMarginal(column, variableName=variable, displayDensity = displayDensity, rugs = rugs, binWidthType = binWidthType)
  
  return(freqPlot)
}

.descriptivesSplitPlot <- function(dataset, options,  variable) {
  depends <- c("splitPlotColour", "splitPlotViolin", "splitPlotBoxplot", "splitPlotJitter", "splitPlotOutlierLabel")
  
  # Initialisation plot
  # .initSplitPlot <- function()
  # {
  #   plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
  #   axis(2, at=0:1, labels=FALSE, cex.axis= 1.4, ylab="")
  #   mtext(text = variable, side = 1, cex=1.5, line = 3)
  # }

  # Define custom y axis function
  base_breaks_y <- function(x) {
    b <- pretty(x)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), size = 0.75, inherit.aes=FALSE), ggplot2::scale_y_continuous(breaks=b))
  }
  
  thePlot <- createJaspPlot(title=variable, width=options$plotWidth, height=options$plotHeight, dependencies=depends)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 1")
  if (!is.null(errorMessage)) {
    thePlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (!(options$splitPlotViolin || options$splitPlotBoxplot || options$splitPlotJitter)) {
    thePlot$setError(gettext("Plotting is not possible: No plot type selected!"))
  } else {
    # we need to know which index in y is related to which index in the actual data, so we should not forget the NAs somehow, lets make a list of indices.
    yWithNA         <- dataset[[.v(variable)]]
    y               <- na.omit(dataset[[.v(variable)]])
    yIndexToActual  <- y
    yWithNAIndex    <- 1
    yNoNAIndex      <- 1
    
    while(yWithNAIndex <= length(yWithNA)) {
      
      if(!is.na(yWithNA[[yWithNAIndex]])) {
        
        yIndexToActual[[yNoNAIndex]] <- row.names(dataset)[[yWithNAIndex]]
        yNoNAIndex                   <- yNoNAIndex + 1
      }
      
      yWithNAIndex <- yWithNAIndex + 1
    }
    
    if (is.null(dataset[[.v(options$splitby)]])){
      group     <- factor(rep("",length(y)))
      xlab      <- "Total"
      boxWidth  <- 0.2
      vioWidth  <- 0.3
    } else {
      group     <- as.factor(dataset[[.v(options$splitby)]])[!is.na(dataset[[.v(variable)]])]
      xlab      <- options$splitby
      boxWidth  <- 0.4
      vioWidth  <- 0.6
    }

    plotDat <- data.frame(group = group, y = y)
    row.names(plotDat) <- yIndexToActual

    # Identify outliers to label. Note that ggplot uses the unchangeable quantiles(type=7), 
    # if we ever change the quantile type then the boxplot needs to be overwritten with stat_summary(geom='boxplot')
    plotDat$outlier <- FALSE

    for (level in levels(plotDat$group)) {
      v         <- plotDat[plotDat$group == level,]$y
      quantiles <- quantile(v, probs=c(0.25,0.75))
      obsIQR       <- quantiles[2] - quantiles[1]
      plotDat[plotDat$group == level,]$outlier <- v < (quantiles[1]-1.5*obsIQR) | v > (quantiles[2]+1.5*obsIQR)
    }

    plotDat$label <- ifelse(plotDat$outlier, row.names(plotDat),"")

    if (options$splitPlotColour) {
      thePlot$dependOn("colorPalette") # only add color as dependency if the user wants it
      palette <- options[["colorPalette"]]
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x=group, y, fill=group)) + JASPgraphs::scale_JASPfill_discrete(palette) + JASPgraphs::scale_JASPcolor_discrete(palette)
    } else {
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x=group, y, fill=group)) + ggplot2::scale_fill_manual(values=rep("grey", nlevels(group))) + ggplot2::scale_colour_manual(values=rep("grey", nlevels(group)))
    }

    if (options$splitPlotViolin && options$splitPlotBoxplot && options$splitPlotJitter) {
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2) +
        ggplot2::geom_boxplot(size = 0.75, width = boxWidth, outlier.shape = NA) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, fill = "transparent", scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    } else if (options$splitPlotBoxplot && options$splitPlotViolin) {
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2) +
        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth, fill = "transparent", scale = "width")
    } else if (options$splitPlotBoxplot && options$splitPlotJitter) {
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2 ) +
        ggplot2::geom_boxplot(size = 0.75, outlier.shape = NA, width = boxWidth) +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    } else if (options$splitPlotViolin && options$splitPlotJitter) {
      p <- p +
        ggplot2::geom_violin(trim = F, size = 0.75, width = 0.75*boxWidth, scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1, position = ggplot2::position_jitter(width=0.05, height = 0), fill = "transparent")
    } else if (options$splitPlotViolin) {
      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, scale = "width", width = 0.75*boxWidth)
    } else if (options$splitPlotBoxplot) {
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar",size = 0.75, width = boxWidth/2 ) +
        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth)
    } else if (options$splitPlotJitter) {
      p <- p + ggplot2::geom_jitter(size = 2.5, ggplot2::aes(colour = group), position = ggplot2::position_jitter(width=0.1, height = 0))
    }
    
    if (options$splitPlotOutlierLabel && (options$splitPlotBoxplot || options$splitPlotJitter))
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label=label), hjust=-0.3)
    
    ### Theming & Cleaning
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(y)
    p <- p +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(variable) +
      ggplot2::scale_y_continuous(breaks = yBreaks) + #, limits = yLimits) +
      JASPgraphs::geom_rangeframe(sides = "l") +
      JASPgraphs::themeJaspRaw()

    thePlot$plotObject <- p
  }
  return(thePlot)
}


.plotMarginal <- function(column, variableName,
                          rugs = FALSE, displayDensity = FALSE, binWidthType) {
  column <- as.numeric(column)
  variable <- na.omit(column)

  if(length(variable) == 0)
    return(NULL)
  
  if(binWidthType == "doane"){
    sigma.g1 <- sqrt((6*(length(variable) - 2)) / ((length(variable) + 1)*(length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  }
  
  if(binWidthType == 'fd' & nclass.FD(variable) > 10000) # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000

  h <- hist(variable, plot = FALSE, breaks = binWidthType)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  ylow <- 0
  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity) {
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = gettext("Counts"), xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  } else {
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = gettext("Density"), xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )
  }
    

  if (displayDensity) {
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  } else {
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )
  }
  
  if (rugs)
    p <- p + ggplot2::geom_rug(data = data.frame(variable),mapping  = ggplot2::aes(x = variable), sides="b")
    

  # JASP theme
  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.barplotJASP <- function(column, variable, dontPlotData= FALSE) {
  p <- JASPgraphs::drawAxis(xName = variable, xBreaks = 1:5, yBreaks = 1:5)

  if (dontPlotData) return(JASPgraphs::themeJasp(p))

  tb <- as.data.frame(table(column))
  p  <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
    ggplot2::xlab(variable) +
    ggplot2::ylab(gettext("Counts"))

  # JASP theme
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}

# .barplotJASP <- function(column, variable, dontPlotData= FALSE){

#   if (dontPlotData) {

#     plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

#     axis(1, at=0:1, labels=FALSE, cex.axis= 1.4, xlab="")
#     axis(2, at=0:1, labels=FALSE, cex.axis= 1.4, ylab="")

#     mtext(text = variable, side = 1, cex=1.5, line = 3)

#     return()
#   }

#   maxFrequency <- max(summary(column))

#   i <- 1
#   step <- 1

#   while (maxFrequency / step > 9) {

#     if (i == 2) {

#       step <- 2 * step
#       i <- i + 1

#     } else if (i %% 3 == 0) {

#       step <- 2.5 * step
#       i <- i + 1

#     } else {

#       step <- 2 * step
#       i <- i + 1
#     }

#   }

#   yticks <- 0

#   while (yticks[length(yticks)] < maxFrequency) {

#     yticks <- c(yticks, yticks[length(yticks)] + step)
#   }


#   yLabs <- vector("character", length(yticks))

#   for(i in seq_along(yticks))
#     if(yticks[i] < 10^6)
#       yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
#     else
#       yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)

#   distLab <- max(nchar(yLabs))/1.8

#   par(mar= c(5, 7.2, 4, 2) + 0.1)
#   barplot(summary(column), cex.names= 1.3, axes= FALSE, ylim= range(yticks))
#   axis(2, las=1, at= yticks, labels= yLabs, cex.axis= 1.4)
#   mtext(text = variable, side = 1, cex=1.5, line = 3)
#   mtext(text = "Frequency", side = 2, cex=1.5, line = distLab+2, las=0)
# }

# .plotScatterDescriptives <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2){

#   d     <- data.frame(xx= xVar, yy= yVar)
#   d     <- na.omit(d)
#   xVar  <- d$xx
#   yVar  <- d$yy

#   # fit different types of regression
#   fit <- vector("list", 1)# vector("list", 4)

#   fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
#   fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
#   fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
#   fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)

#   # find parsimonious, best fitting regression model
#   Bic <- vector("numeric", 4)

#   for (i in 1:4)
#     Bic[i] <- BIC(fit[[i]])



#   bestModel <- which.min(Bic)

#   xlow    <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
#   xhigh   <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
#   xticks  <- pretty(c(xlow, xhigh))

#   ylow    <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
#   yhigh   <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
#   yticks  <- pretty(c(ylow, yhigh))

#   plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)

#   .poly.pred(fit[[bestModel]], line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)

#   par(las=1)

#   axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis)
#   axis(2, line= 0.2, labels= yticks, at= yticks, cex.axis= cexYAxis)
# }

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

.descriptivesIqr <- function(x) {
  # Interquartile range based on the stats package
  return(stats::IQR(x))
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

.descriptivesQQPlot <- function(dataset, options,  qqvar, levelName=NULL) {
  #to put a subtitle if there is a split
  title <- qqvar
  if(!is.null(levelName))
    title <- levelName
  
  descriptivesQQPlot <- createJaspPlot(width=400, aspectRatio=1, title=title)
  
  if (!is.null(qqvar)) {
    
    errorMessage <- .descriptivesCheckPlotErrors(dataset, qqvar, obsAmount = "< 1")
    if (!is.null(errorMessage)) {
      descriptivesQQPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
    } else {
      varCol<-dataset[[.v(qqvar)]]
      varCol<-varCol[!is.na(varCol)]
  
      standResid <- as.data.frame(stats::qqnorm(varCol, plot.it=FALSE))
  
      standResid <- na.omit(standResid)
      xVar <- standResid$x
      yVar <- standResid$y
      yVar<-yVar-mean(yVar)
      yVar<-yVar/(sd(yVar))
  
      # Format x ticks
      xlow   <- min(pretty(xVar))
      xhigh  <- max(pretty(xVar))
      xticks <- pretty(c(xlow, xhigh))
  
      # Format y ticks
      ylow   <- min(pretty(yVar))
      yhigh  <- max(pretty(yVar))
      yticks <- pretty(c(ylow, yhigh))
  
      # format axes labels
      xLabs <- JASPgraphs::axesLabeller(xticks)
      yLabs <- JASPgraphs::axesLabeller(yticks)
      
      p <- JASPgraphs::drawAxis(xName = gettext("Theoretical Quantiles"), yName = gettext("Standardised Residuals"), xBreaks = xticks, yBreaks = xticks, yLabels = xLabs, xLabels = xLabs, force = TRUE)
      p <- p + ggplot2::geom_line(data = data.frame(x = c(min(xticks), max(xticks)), y = c(min(xticks), max(xticks))), mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1)
      p <- JASPgraphs::drawPoints(p, dat = data.frame(xVar, yVar), size = 3)
  
      # JASP theme
      descriptivesQQPlot$plotObject <- JASPgraphs::themeJasp(p)
    }
  }
  
  if (is.null(levelName))
    descriptivesQQPlot$dependOn(optionContainsValue=list(variables=qqvar))
  return(descriptivesQQPlot)
}

.descriptivesPieChart <- function(dataset, options, variable) {
  if (options$splitby != "" ) {
    # return a collection
    split <- names(dataset)

    plotResult <- createJaspContainer(title=variable)
    plotResult$dependOn(optionContainsValue=list(variables=variable))

    for (l in split) {
      plotResult[[l]] <- .descriptivesPieChart_SubFunc(dataset=dataset[[l]], variable=variable, width=options$plotWidth, height=options$plotHeight, title = l,
                                                       palette = options[["colorPalette"]])
      plotResult[[l]]$dependOn(optionsFromObject=plotResult)
    }

    return(plotResult)
  } else {
    aPlot <- .descriptivesPieChart_SubFunc(dataset=dataset, variable=variable, width=options$plotWidth, height=options$plotHeight, title = variable,
                                           palette = options[["colorPalette"]])
    aPlot$dependOn(options="splitby", optionContainsValue=list(variables=variable))

    return(aPlot)
  }
}

.descriptivesPieChart_SubFunc <- function(dataset, variable, width, height, title, palette) {
  pieChart <- createJaspPlot(title=title, width=width, height=height)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 3")
  column <- dataset[[.v(variable)]]
  column <- column[!is.na(column)]
  if (!is.null(errorMessage)) {
    pieChart$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (length(column) > 0) {
    tb  <- as.data.frame(table(column))
    pieChart$plotObject <- JASPgraphs::plotPieChart(tb[,2],tb[,1], legendName = variable,
                                                   palette = palette)
  }

  return(pieChart)
}

.descriptivesScatterPlots <- function(jaspContainer, dataset, variables, split, options, name = NULL, dependOnVariables = TRUE) {

  JASPgraphs::setGraphOption("palette", options[["colorPalette"]])
  if (!is.null(split) && split != "") {
    group <- dataset[, .v(split)]
    legendTitle <- split

  } else {
    group <- NULL
    legendTitle <- NULL
  }

  # remove non-numeric variables
  numerics <- sapply(variables, .descriptivesIsNumericColumn, dataset=dataset)
  variablesB64 <- .v(variables)
  variablesB64 <- variablesB64[numerics]
  variables    <- variables[numerics]

  nvar <- length(variables)
  # Set's a message with instruction for user using jaspHtml
  if (nvar < 2L) {
  #   msg <- if (length(numerics) > 1L) { # basically all user variables have the wrong type...
  #     "These plots can only be shown for scale variables."
  #   } else {
  #     "Please enter two variables."
  #   }
  #   jaspContainer[["scatterplotMsg"]] <- createJaspHtml(text = msg, dependencies = "variables")
    return()
  }

  for (i in 1:(nvar - 1L)) for (j in (i + 1L):nvar) {
    v1 <- variables[i]
    v2 <- variables[j]
    
    if (!is.null(name))
      plotName <- name
    else
      plotName <- paste(v1, "-", v2)
    
    if (is.null(jaspContainer[[plotName]])) {
      scatterPlot <- createJaspPlot(title = plotName)
      if (dependOnVariables)
        scatterPlot$dependOn(optionContainsValue = list(variables = c(v1, v2)))
      
      scatterData <- dataset[, c(variablesB64[i], variablesB64[j])]
      errorMessage <- .descriptivesCheckPlotErrors(scatterData, c(v1, v2), obsAmount = "< 2")
      if (is.null(errorMessage)) {
        scatterData <- na.omit(scatterData)
        scatterData <- apply(scatterData, 2, as.numeric) # ensure nominal ints are numeric
        
        p <- try(JASPgraphs::JASPScatterPlot(
          x                 = scatterData[, variablesB64[i]],
          y                 = scatterData[, variablesB64[j]],
          group             = group,
          xName             = v1,
          yName             = v2,
          showLegend        = options[["showLegend"]],
          addSmooth         = options[["addSmooth"]],
          addSmoothCI       = options[["addSmoothCI"]],
          smoothCIValue     = options[["addSmoothCIValue"]],
          forceLinearSmooth = options[["regressionType"]] == "linear",
          plotAbove         = options[["graphTypeAbove"]],
          plotRight         = options[["graphTypeRight"]],
          legendTitle       = legendTitle
        ))
        
        if (isTryError(p))
          errorMessage <- .extractErrorMessage(p)
      }

      if (!is.null(errorMessage))
        scatterPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
      else
        scatterPlot$plotObject <- p

      jaspContainer[[plotName]] <- scatterPlot
    }
  }
}

.descriptivesCheckPlotErrors <- function(dataset, vars, obsAmount) {
  errors <- .hasErrors(dataset, all.target=vars, message="short", type=c("infinity", "observations"), observations.amount = obsAmount)
  if (!isFALSE(errors))
    return(errors$message)
  
  return(NULL)
}

.descriptivesIsNumericColumn <- function(dataset, colName) {
  column <- na.omit(dataset[[.v(colName)]])
  if (is.factor(column) && !anyNA(suppressWarnings(as.numeric(levels(column)))))
    return(TRUE)
  else if (is.numeric(column))
    return(TRUE)
  else
    return(FALSE)
}