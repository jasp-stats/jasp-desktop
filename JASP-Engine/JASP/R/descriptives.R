#
# Copyright (C) 2013-2017 University of Amsterdam
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

Descriptives <- function(dataset=NULL, options, perform="run", 
                         callback=function(...) 0, ...) {
  
  # <editor-fold> DATASET LOADING BLOCK ----
  # requested variables
  variables <- unlist(options$variables)
  splitName <- options$splitby
  
  # booleans
  makeSplit <- splitName != ""
  run <- perform == "run"
  
  # load data
  if (is.null(dataset)) {
    if (run) {
      if (makeSplit) {
        dataset <- .readDataSetToEnd(columns.as.numeric=variables,
                                     columns.as.factor=splitName)
        dataset.factors <- .readDataSetToEnd(columns=variables,
                                             columns.as.factor=splitName)
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric=variables)
        dataset.factors <- .readDataSetToEnd(columns=variables)
      }
    
    } else {
      if (makeSplit) {
        dataset <- .readDataSetHeader(columns.as.numeric=variables,
                                      columns.as.factor=splitName)
        dataset.factors <- .readDataSetHeader(columns=variables,
                                              columns.as.factor=splitName)
      } else {
        dataset <- .readDataSetHeader(columns.as.numeric=variables)
        dataset.factors <- .readDataSetHeader(columns=variables)
      }
    }
  }
  
  
  # If user requests split, create a list of datasets, one for each level
  if (makeSplit) {
    splitFactor <- dataset[[.v(splitName)]]
    splitLevels <- levels(splitFactor)
    splitDat <- lapply(splitLevels, function(l){
      return(dataset.factors[splitFactor == l,])
    })
    names(splitDat) <- splitLevels
  }
  
  # </editor-fold> DATASET LOADING BLOCK
  
  # <editor-fold> ERROR HANDLING BLOCK ----
  
  #TODO error handling
  
  # </editor-fold> ERROR HANDLING BLOCK
  
  # <editor-fold> STATE SYSTEM BLOCK ----
  
  # Load state
  state <- .retrieveState()
  
  # Init generic result list with one element per variable
  genericResult <- lapply(variables, function(v) NULL)
  names(genericResult) <- variables
  
  # Assign this generic result to all results objects
  freqTabs <- distPlots <- boxPlots <- genericResult

  mainTable <- NULL # mainTable per-variable state logic is inside its function
  corrPlot <- NULL # This is a single object, so not per-variable
  
  # Check which have been made and fill objects as needed
  if (!is.null(state)) {
    prevSplit <- state$options$splitby != ""
    diff <- .diff(state$options, options)
    
    # diff checking 
    if (is.list(diff)) {
      stateVars <- variables
      if (diff[["variables"]]) {
        # variables changed, now check which variables we can get info from
        stateVars <- variables[variables %in% unlist(state$options$variables)]
      }
      
      # mainTable
      if (!any(diff[["variables"]],
               diff[["splitby"]],
               diff[["percentileValuesQuartiles"]],
               diff[["percentileValuesEqualGroups"]],
               diff[["percentileValuesPercentiles"]],
               diff[["percentileValuesEqualGroupsNo"]],
               diff[["percentileValuesPercentilesPercentiles"]],
               diff[["mean"]],
               diff[["median"]],
               diff[["mode"]],
               diff[["sum"]],
               diff[["statisticsValuesAreGroupMidpoints"]],
               diff[["standardDeviation"]],
               diff[["variance"]],
               diff[["range"]],
               diff[["minimum"]],
               diff[["maximum"]],
               diff[["standardErrorMean"]],
               diff[["skewness"]],
               diff[["kurtosis"]])) {
        # Main table can be completely reused
        mainTable <- state$results$stats
      }
      
      # frequencyTables
      if (options$frequencyTables &&
          !any(diff[["splitby"]],
               diff[["frequencyTables"]])) {
        for (var in stateVars) {
          # find table
          whichtab <- which(sapply(state$results$tables, 
                                   function(x) return(x$name),
                                   simplify = TRUE) == var)
          # add to results object
          if (length(whichtab) > 0) {
            freqTabs[[var]] <- state$results$tables[[whichtab]]
          }
        }
      }
      
      # Distribution plots
      if (options$plotVariables &&
          !any(diff[["splitby"]],
               diff[["plotVariables"]],
               diff[["plotWidth"]],
               diff[["plotHeight"]])) {
        for (var in stateVars) {
          
          # find plot
          # if we are splitting, the plots are nested differently
          if (prevSplit){
            distPlots[[var]] <- state$results$plots$
                                  distributionPlots[[var]]
          } else {
            distPlots[[var]] <- state$results$plots$
                                    distributionPlots$collection[[var]]
          }
        }
      }
      
      # Correlation matrix plot
      if (options$plotCorrelationMatrix &&
          !any(diff[["splitby"]],
               diff[["plotCorrelationMatrix"]],
               diff[["variables"]])) {
        corrPlot <- state$results$plots$matrixPlot
      }
      
      # Boxplots
      if (options$splitPlots &&
          !any(diff[["splitby"]],
               diff[["splitPlots"]],
               diff[["splitPlotBoxplot"]],
               diff[["splitPlotViolin"]],
               diff[["splitPlotJitter"]],
               diff[["splitPlotOutlierLabel"]],
               diff[["splitPlotColour"]],
               diff[["plotWidth"]],
               diff[["plotHeight"]])) {
        for (var in stateVars){
          # find plot
          whichPlot <- which(sapply(state$results$plots$splitPlots$collection,
                                    function(x) x$name,
                                    simplify = TRUE) == var)
          
          # Assign to results object
          boxPlots[[var]] <- state$results$plots$
                               splitPlots$collection[[whichPlot]]
          
        }
      }
    }
  }
  
  # </editor-fold> STATE SYSTEM BLOCK
  
  # <editor-fold> METADATA BLOCK ----
  
  # Initialise the results
  results <- list()
  meta <- list()
  
  if (makeSplit){
    # Distribution plots becomes a collection for each variable
    distrMeta <- lapply(variables, function(v) {
                   return(list(name = v, type="collection", meta="image"))
                 })
    # matrixPlot becomes a collection.
    plotsMeta <- list(
                  list(name="distributionPlots", type="object", meta=distrMeta),
                  list(name="matrixPlot", type="collection", meta="image"),
                  list(name="splitPlots", type="collection", meta="image")
                 )
  } else {
    plotsMeta <- list(
                  list(name="distributionPlots", type="collection", meta="image"),
                  list(name="matrixPlot", type="image"),
                  list(name="splitPlots", type="collection", meta="image")
                 )
  }  

  meta[[1]] <- list(name="title", type="title")
  meta[[2]] <- list(name="stats", type="table")
  meta[[3]] <- list(name="frequenciesHeading", type="h1")
  meta[[4]] <- list(name="tables", type="tables")
  meta[[5]] <- list(name="plots", type="object", meta=plotsMeta)

  results[[".meta"]] <- meta
  results[["title"]] <- "Descriptives"
  
  # </editor-fold> METADATA BLOCK
  
  # <editor-fold> RESULT GENERATION BLOCK ----
  
  # Stats table
  if (is.null(mainTable)){
    last.stats.table <- NULL
    if (is.list(state) && !diff[["splitby"]])
      last.stats.table <- state[["results"]][["stats"]]
  
    results[["stats"]] <- .descriptivesDescriptivesTable(dataset, options, run, 
                                                         last.stats.table)
  } else {
    results[["stats"]] <- mainTable
  }
  
  # Frequency table
  if (options$frequencyTables){
    results[["tables"]] <- .descriptivesFrequencyTables(dataset.factors, options, run, 
                                                        freqTabs)
  }
  
  if (length(results[["tables"]]) > 0)
    results[["frequenciesHeading"]] <- "Frequencies"
  
  # Correlation plot
  if (options$plotCorrelationMatrix && is.null(corrPlot)){
    if (makeSplit){
      # Create different plots
      corrPlot <- list()
      for (i in 1:length(splitLevels)) {
        corrPlot[["collection"]][[i]] <- .descriptivesMatrixPlot(splitDat[[i]],
                                                                 options,
                                                                 splitLevels[i],
                                                                 run)
      }
      corrPlot[["title"]] <- "Correlation plots"
    } else {
      # Create one plot
      corrPlot <- .descriptivesMatrixPlot(dataset, options, "Correlation plot", 
                                          run)
    }
  }
  
  
  # Distribution plots
  if (options$plotVariables && length(variables) > 0) {
    for (var in variables) {
      if (is.null(distPlots[[var]])) {
        if (makeSplit){
          distPlots[[var]] <- .descriptivesFrequencyPlots(
            dataset = splitDat,
            options = options,
            run = run,
            variable = var
          )          
        } else {
          distPlots[["collection"]][[var]] <- .descriptivesFrequencyPlots(
            dataset = dataset.factors,
            options = options,
            run = run,
            variable = var
          )
        }
      } else {
        if (!makeSplit) {
          # Move plot to the collection
          distPlots[["collection"]][[var]] <- distPlots[[var]]
          distPlots <- distPlots[names(distPlots)!=var]
        }
      }
    }
    distPlots[["title"]] <- "Distribution plots"
  }
  
  
  # Split plots
  splitPlots <- NULL
  if (options$splitPlots && length(variables) > 0) {
    splitPlots <- list("title" = "Boxplots", "collection" = list())
    for (var in variables) {
      if (is.null(boxPlots[[var]])) {
        splitPlots[["collection"]][[var]] <- .descriptivesSplitPlot(
          dataset = dataset,
          options = options,
          run = run,
          variable = var
        )
      } else {
        splitPlots[["collection"]][[var]] <- boxPlots[[var]]
      }
    }
  }
  
  
  results[["plots"]] <- list(distributionPlots = distPlots,
                             matrixPlot = corrPlot,
                             splitPlots = splitPlots,
                             title = "Plots")
  
  # </editor-fold> RESULT GENERATION BLOCK
  
  # <editor-fold> RETURN RESULTS BLOCK ----
  
  # Select the plots to keep 
  plotPaths <- list()
  if (makeSplit) {
    # distPlots are nested
    if (options$plotVariables) {
      for (d in distPlots) {
        if ("collection" %in% names(d)) {
          for (e in d[["collection"]]){
            plotPaths[[length(plotPaths)+1]] <- e[["data"]]
          }
        }
      }
    }
    
    
    # corrPlot is a collection
    if (options$plotCorrelationMatrix) {
      for (c in corrPlot[["collection"]]) {
        plotPaths[[length(plotPaths)+1]] <- c[["data"]]
      }
    }
  } else {
    # distPlots is a collection
    if (options$plotVariables){
      for (d in distPlots[["collection"]]) {
        plotPaths[[length(plotPaths)+1]] <- d[["data"]]
      }
    }
      
    # corrPlot is a single plot
    if (options$plotCorrelationMatrix) {
      plotPaths[[length(plotPaths)+1]] <- corrPlot[["data"]]
    }
    
  }
  
  if (options$splitPlots) {
    for (s in splitPlots[["collection"]]) {
      plotPaths[[length(plotPaths)+1]] <- s[["data"]]
    }
  }
  
  
  if (perform == "run") {
    state <- list()
    state[["options"]] <- options
    state[["results"]] <- results

    return(list(results=results, status="complete", state=state,
                keep=plotPaths))

  } else {

    return(list(results=results, status="inited", state=state,
                keep=plotPaths))

  }
  
  # </editor-fold> RETURN RESULTS BLOCK
  
}

.descriptivesDescriptivesTable <- function(dataset, options, run, 
                                           last.table=NULL) {
  # NB: This is for the most part a legacy function from the previous version of 
  # the descriptives. Hence it might look different than the rest.
  wantsSplit <- options$splitby != ""
  variables <- unlist(options$variables)
  equalGroupsNo <- options$percentileValuesEqualGroupsNo
  percentilesPercentiles  <- options$percentileValuesPercentilesPercentiles

  stats.results <- list()

  stats.results[["title"]] <- "Descriptive Statistics"
  stats.results[["casesAcrossColumns"]] <- TRUE # flips axes!

  
  # <editor-fold> SCHEMA CREATION ----
  fields <- list()
  
  if (wantsSplit) {
    stats.results[["overTitle"]] <- TRUE # allows overTitle with flipped axes!
    fields[[length(fields) + 1]] <- list(name="Variable", title="", type="string")
    fields[[length(fields) + 1]] <- list(name="Level", title="", type="string")
  } else {
    fields[[length(fields) + 1]] <- list(name="Variable", title="", type="string")
  }
  fields[[length(fields) + 1]] <- list(name="Valid", type="integer")
  fields[[length(fields) + 1]] <- list(name="Missing", type="integer")


  if (options$mean)
    fields[[length(fields) + 1]] <- list(name="Mean", type="number", format="sf:4")
  if (options$standardErrorMean)
    fields[[length(fields) + 1]] <- list(name="Std. Error of Mean", type="number", format="sf:4")
  if (options$median)
    fields[[length(fields) + 1]] <- list(name="Median", type="number", format="sf:4")
  if (options$mode)
    fields[[length(fields) + 1]] <- list(name="Mode", type="number", format="sf:4")
  if (options$standardDeviation)
    fields[[length(fields) + 1]] <- list(name="Std. Deviation", type="number", format="sf:4")
  if (options$variance)
    fields[[length(fields) + 1]] <- list(name="Variance", type="number", format="sf:4")

  if (options$skewness) {

    fields[[length(fields) + 1]] <- list(name="Skewness", type="number", format="sf:4")
    fields[[length(fields) + 1]] <- list(name="Std. Error of Skewness", type="number", format="sf:4")
  }

  if (options$kurtosis) {

    fields[[length(fields) + 1]] <- list(name="Kurtosis", type="number", format="sf:4")
    fields[[length(fields) + 1]] <- list(name="Std. Error of Kurtosis", type="text", format="sf:4")
  }

  if (options$range)
    fields[[length(fields) + 1]] <- list(name="Range", type="number", format="sf:4")
  if (options$minimum)
    fields[[length(fields) + 1]] <- list(name="Minimum", type="number", format="sf:4")
  if (options$maximum)
    fields[[length(fields) + 1]] <- list(name="Maximum", type="number", format="sf:4")
  if (options$sum)
    fields[[length(fields) + 1]] <- list(name="Sum", type="number", format="sf:4")

  if (options$percentileValuesQuartiles) {

    fields[[length(fields) + 1]] <- list(name="q1", title="25th percentile", type="number", format="sf:4")
    fields[[length(fields) + 1]] <- list(name="q2", title="50th percentile", type="number", format="sf:4")
    fields[[length(fields) + 1]] <- list(name="q3", title="75th percentile", type="number", format="sf:4")
  }

  if (options$percentileValuesEqualGroups) {  # I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way

    for (i in seq(equalGroupsNo - 1))
      fields[[length(fields) + 1]] <- list(name=paste("eg", i, sep=""), title=paste(as.integer(100 * i / equalGroupsNo), "th percentile", sep=""), type="number", format="sf:4")
  }

  if (options$percentileValuesPercentiles) {

    for (i in percentilesPercentiles)
      fields[[length(fields) + 1]] <- list(name=paste("pc", i, sep=""), title=paste(i, "th percentile", sep=""), type="number", format="sf:4")
  }

  stats.results[["schema"]] <- list(fields=fields)

  footnotes <- .newFootnotes()
  
  # </editor-fold> SCHEMA CREATION

  note.symbol <- "<i>Note.</i>"
  na.for.categorical <- "Not all values are available for <i>Nominal Text</i> variables"

  stats.values <- list()
  
  # Find the number of levels to loop over
  
  if (wantsSplit) {
    split <- dataset[[ .v(options$splitby) ]]
    splitLevels <- levels(split)
    nLevels <- length(levels(split))
    
    # <editor-fold> COLUMN GENERATION ----
    
    for (variable in variables) {
      
      # see if we already have info
      whichCols <- sapply(last.table$data, 
                          function(x) x$Variable == variable,
                          simplify = TRUE)
      if (length(whichCols)>0){
        variable.results <- last.table$data[whichCols]  
      } else {
        variable.results <- list()
      }
                    
      
    
      for (l in 1:nLevels) {
        resultsCol <- list(Variable = variable,
                           Level = splitLevels[l])
        
        # get col
        for (col in variable.results) {
          if (!is.null(col$Level) && col$Level == splitLevels[l]){
            resultsCol <- col
            break
          }
        }
        
                
        column <- dataset[[ .v(variable) ]][split==splitLevels[l]]
    
        if (perform == "run") {
    
          rows <- length(column)
          na.omitted <- na.omit(column)
    
          resultsCol[["Valid"]] = length(na.omitted)
          resultsCol[["Missing"]] = rows - length(na.omitted)
        } else {  
          na.omitted <- column
        }
    
        if (options$mean) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Mean"]] <- .clean(mean(na.omitted))
    
          } else {
    
            resultsCol[["Mean"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Mean"]] <- NULL
        }
    
        if (options$median) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Median"]] <- .clean(median(na.omitted))
    
          } else {
    
            resultsCol[["Median"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        } else {
    
          resultsCol[["Median"]] <- NULL
        }
    
        if (options$mode) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))
    
              if (length(mode) > 1) {
    
                index <- .addFootnote(footnotes, "More than one mode exists, only the first is reported")
                resultsCol[[".footnotes"]] <- list(Mode=list(index))
              }
    
              resultsCol[["Mode"]] <- .clean(mode[1])
    
            }
    
          } else {
    
            resultsCol[["Mode"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        } else {
    
          resultsCol[["Mode"]] <- NULL
        }
    
        if (options$sum) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Sum"]] <- .clean(sum(na.omitted))
    
          } else {
    
            resultsCol[["Sum"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Sum"]] <- NULL
        }
    
        if (options$maximum) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Maximum"]] <- .clean(max(na.omitted))
    
          } else {
    
            resultsCol[["Maximum"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Maximum"]] <- NULL
        }
    
        if (options$minimum) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Minimum"]] <- .clean(min(na.omitted))
    
          } else {
    
            resultsCol[["Minimum"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        } else {
    
          resultsCol[["Minimum"]] <- NULL
        }
    
        if (options$range) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Range"]] <- .clean(range(na.omitted)[2]-range(na.omitted)[1])
    
          } else {
    
            resultsCol[["Range"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Range"]] <- NULL
        }
    
        if (options$standardDeviation) {
    
          if (base::is.factor(na.omitted) == FALSE){
    
            if (perform == "run")
              resultsCol[["Std. Deviation"]] <- .clean(sd(na.omitted))
    
          } else {
    
            resultsCol[["Std. Deviation"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Std. Deviation"]] <- NULL
        }
    
        if (options$standardErrorMean) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Std. Error of Mean"]] <- .clean(sd(na.omitted)/sqrt(length(na.omitted)))
    
          } else {
    
            resultsCol[["Std. Error of Mean"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Std. Error of Mean"]] <- NULL
        }
    
        if (options$variance) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run")
              resultsCol[["Variance"]] <- .clean(var(na.omitted))
    
          } else {
    
            resultsCol[["Variance"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Variance"]] <- NULL
        }
    
        if (options$kurtosis) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              resultsCol[["Kurtosis"]] <- .clean(.descriptivesKurtosis(na.omitted))
              resultsCol[["Std. Error of Kurtosis"]] <- .clean(.descriptivesSEK(na.omitted))
            }
    
          } else {
    
            resultsCol[["Kurtosis"]] <- ""
            resultsCol[["Std. Error of Kurtosis"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
    
        } else {
    
          resultsCol[["Kurtosis"]] <- NULL
          resultsCol[["Std. Error of Kurtosis"]] <- NULL
        }
    
        if (options$skewness) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              resultsCol[["Skewness"]] <- .clean(.descriptivesSkewness(na.omitted))
              resultsCol[["Std. Error of Skewness"]] <- .clean(.descriptivesSES(na.omitted))
    
            }
    
          } else {
    
            resultsCol[["Skewness"]] <- ""
            resultsCol[["Std. Error of Skewness"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        } else {
    
          resultsCol[["Skewness"]] <- NULL
          resultsCol[["Std. Error of Skewness"]] <- NULL
        }
    
        if (options$percentileValuesQuartiles) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              resultsCol[["q1"]] <- .clean(quantile(na.omitted, c(.25), type=6, names=F))
              resultsCol[["q2"]] <- .clean(quantile(na.omitted, c(.5), type=6, names=F))
              resultsCol[["q3"]] <- .clean(quantile(na.omitted, c(.75), type=6, names=F))
    
            }
    
          } else {
    
            resultsCol[["q1"]] <- ""
            resultsCol[["q2"]] <- ""
            resultsCol[["q3"]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        } else {
    
          resultsCol[["q1"]] <- NULL
          resultsCol[["q2"]] <- NULL
          resultsCol[["q3"]] <- NULL
        }
    
    
        equalGroupNames <- NULL
        if (options$percentileValuesEqualGroups)
          equalGroupNames <- paste("eg", seq(equalGroupsNo - 1), sep="")
    
        for (row in names(resultsCol)) {
    
          if (substr(row, 1, 2) == "eg" && ((row %in% equalGroupNames) == FALSE))
            resultsCol[[row]] <- NULL
        }
    
        if (options$percentileValuesEqualGroups) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              for (i in seq(equalGroupsNo - 1))
                resultsCol[[paste("eg", i, sep="")]] <- .clean(quantile(na.omitted, c(i / equalGroupsNo), type=6, names=F))
    
            }
    
          } else {
    
            for (i in seq(equalGroupsNo - 1))
              resultsCol[[paste("eg", i, sep="")]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        }
    
    
        percentileNames <- NULL
        if (options$percentileValuesPercentiles)
          percentileNames <- paste("pc", percentilesPercentiles, sep="")
    
        for (row in names(resultsCol)) {
    
          if (substr(row, 1, 2) == "pc" && ((row %in% percentileNames) == FALSE))
            resultsCol[[row]] <- NULL
        }
    
        if (options$percentileValuesPercentiles) {
    
          if (base::is.factor(na.omitted) == FALSE) {
    
            if (perform == "run") {
    
              for (i in percentilesPercentiles)
                resultsCol[[paste("pc", i, sep="")]] <- .clean(quantile(na.omitted, c(i / 100), type=6, names=F))
    
            }
    
          } else {
    
            for (i in percentilesPercentiles)
              resultsCol[[paste("pc", i, sep="")]] <- ""
            .addFootnote(footnotes, na.for.categorical, note.symbol)
          }
        }
        
        variable.results[[l]] <- resultsCol
        
      }
      
      stats.values <- c(stats.values, variable.results)

    }
    # </editor-fold> COLUMN GENERATION
    
  } else {
    
    # <editor-fold> COLUMN GENERATION ----
    
    for (variable in variables) {
  
      variable.results <- list(Variable=variable)
  
      for (col in last.table$data) {
  
        if (col$Variable == variable) {
  
          variable.results <- col
          break
        }
      }
      
      
      column <- dataset[[ .v(variable) ]]
  
      if (perform == "run") {
  
        rows <- nrow(dataset)
        na.omitted <- na.omit(column)
  
        variable.results[["Valid"]] = length(na.omitted)
        variable.results[["Missing"]] = rows - length(na.omitted)
      }
      else {
  
        na.omitted <- column
      }
  
  
  
      if (options$mean) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Mean"]] <- .clean(mean(na.omitted))
  
        } else {
  
          variable.results[["Mean"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Mean"]] <- NULL
      }
  
      if (options$median) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Median"]] <- .clean(median(na.omitted))
  
        } else {
  
          variable.results[["Median"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      } else {
  
        variable.results[["Median"]] <- NULL
      }
  
      if (options$mode) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))
  
            if (length(mode) > 1) {
  
              index <- .addFootnote(footnotes, "More than one mode exists, only the first is reported")
              variable.results[[".footnotes"]] <- list(Mode=list(index))
            }
  
            variable.results[["Mode"]] <- .clean(mode[1])
  
          }
  
        } else {
  
          variable.results[["Mode"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      } else {
  
        variable.results[["Mode"]] <- NULL
      }
  
      if (options$sum) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Sum"]] <- .clean(sum(na.omitted))
  
        } else {
  
          variable.results[["Sum"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Sum"]] <- NULL
      }
  
      if (options$maximum) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Maximum"]] <- .clean(max(na.omitted))
  
        } else {
  
          variable.results[["Maximum"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Maximum"]] <- NULL
      }
  
      if (options$minimum) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Minimum"]] <- .clean(min(na.omitted))
  
        } else {
  
          variable.results[["Minimum"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      } else {
  
        variable.results[["Minimum"]] <- NULL
      }
  
      if (options$range) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Range"]] <- .clean(range(na.omitted)[2]-range(na.omitted)[1])
  
        } else {
  
          variable.results[["Range"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Range"]] <- NULL
      }
  
      if (options$standardDeviation) {
  
        if (base::is.factor(na.omitted) == FALSE){
  
          if (perform == "run")
            variable.results[["Std. Deviation"]] <- .clean(sd(na.omitted))
  
        } else {
  
          variable.results[["Std. Deviation"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Std. Deviation"]] <- NULL
      }
  
      if (options$standardErrorMean) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Std. Error of Mean"]] <- .clean(sd(na.omitted)/sqrt(length(na.omitted)))
  
        } else {
  
          variable.results[["Std. Error of Mean"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Std. Error of Mean"]] <- NULL
      }
  
      if (options$variance) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run")
            variable.results[["Variance"]] <- .clean(var(na.omitted))
  
        } else {
  
          variable.results[["Variance"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Variance"]] <- NULL
      }
  
      if (options$kurtosis) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            variable.results[["Kurtosis"]] <- .clean(.descriptivesKurtosis(na.omitted))
            variable.results[["Std. Error of Kurtosis"]] <- .clean(.descriptivesSEK(na.omitted))
          }
  
        } else {
  
          variable.results[["Kurtosis"]] <- ""
          variable.results[["Std. Error of Kurtosis"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
  
      } else {
  
        variable.results[["Kurtosis"]] <- NULL
        variable.results[["Std. Error of Kurtosis"]] <- NULL
      }
  
      if (options$skewness) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            variable.results[["Skewness"]] <- .clean(.descriptivesSkewness(na.omitted))
            variable.results[["Std. Error of Skewness"]] <- .clean(.descriptivesSES(na.omitted))
  
          }
  
        } else {
  
          variable.results[["Skewness"]] <- ""
          variable.results[["Std. Error of Skewness"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      } else {
  
        variable.results[["Skewness"]] <- NULL
        variable.results[["Std. Error of Skewness"]] <- NULL
      }
  
      if (options$percentileValuesQuartiles) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            variable.results[["q1"]] <- .clean(quantile(na.omitted, c(.25), type=6, names=F))
            variable.results[["q2"]] <- .clean(quantile(na.omitted, c(.5), type=6, names=F))
            variable.results[["q3"]] <- .clean(quantile(na.omitted, c(.75), type=6, names=F))
  
          }
  
        } else {
  
          variable.results[["q1"]] <- ""
          variable.results[["q2"]] <- ""
          variable.results[["q3"]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      } else {
  
        variable.results[["q1"]] <- NULL
        variable.results[["q2"]] <- NULL
        variable.results[["q3"]] <- NULL
      }
  
  
      equalGroupNames <- NULL
      if (options$percentileValuesEqualGroups)
        equalGroupNames <- paste("eg", seq(equalGroupsNo - 1), sep="")
  
      for (row in names(variable.results)) {
  
        if (substr(row, 1, 2) == "eg" && ((row %in% equalGroupNames) == FALSE))
          variable.results[[row]] <- NULL
      }
  
      if (options$percentileValuesEqualGroups) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            for (i in seq(equalGroupsNo - 1))
              variable.results[[paste("eg", i, sep="")]] <- .clean(quantile(na.omitted, c(i / equalGroupsNo), type=6, names=F))
  
          }
  
        } else {
  
          for (i in seq(equalGroupsNo - 1))
            variable.results[[paste("eg", i, sep="")]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      }
  
  
      percentileNames <- NULL
      if (options$percentileValuesPercentiles)
        percentileNames <- paste("pc", percentilesPercentiles, sep="")
  
      for (row in names(variable.results)) {
  
        if (substr(row, 1, 2) == "pc" && ((row %in% percentileNames) == FALSE))
          variable.results[[row]] <- NULL
      }
  
      if (options$percentileValuesPercentiles) {
  
        if (base::is.factor(na.omitted) == FALSE) {
  
          if (perform == "run") {
  
            for (i in percentilesPercentiles)
              variable.results[[paste("pc", i, sep="")]] <- .clean(quantile(na.omitted, c(i / 100), type=6, names=F))
  
          }
  
        } else {
  
          for (i in percentilesPercentiles)
            variable.results[[paste("pc", i, sep="")]] <- ""
          .addFootnote(footnotes, na.for.categorical, note.symbol)
        }
      }
  
      stats.values[[length(stats.values) + 1]] <- variable.results
  
    }
    # </editor-fold> COLUMN GENERATION
    
  }
  
  stats.results[["data"]] <- stats.values
  stats.results[["footnotes"]] <- as.list(footnotes)
  stats.results
}

.descriptivesFrequencyTables <- function(dataset, options, run, stateTabs) {
  splitName <- options$splitby
  wantsSplit <- splitName!=""
  splitFactor <- dataset[[.v(splitName)]]
  splitLevels <- levels(splitFactor)
  
  freqTabs <- list()

  for (variable in options$variables) {
    freqTab <- list()
    
    column <- dataset[[.v(variable)]]
    if (!is.factor(column)){
      next
    }
    
    if (variable %in% names(stateTabs) && 
        "data" %in% names(stateTabs[[variable]])) {
      print(paste(variable, "in state"))
      freqTabs[[length(freqTabs) + 1]] <- stateTabs[[variable]]
      next
    }
    
    
    fields <- list()
    if (wantsSplit) {
      fields[[1]] <- list(name = "factor", title = splitName, type = "string", combine=TRUE)
    }
    fields[[length(fields) + 1]] <- list(name="Level", type="string", title=variable)
    fields[[length(fields) + 1]] <- list(name="Frequency", type="integer")
    fields[[length(fields) + 1]] <- list(name="Percent", type="number", format="dp:1")
    fields[[length(fields) + 1]] <- list(name="Valid Percent", type="number", format="dp:1")
    fields[[length(fields) + 1]] <- list(name="Cumulative Percent", type="number", format="dp:1")
    
    freqTab[["title"]] <- paste("Frequencies for", variable)
    freqTab[["name"]] <- variable
    freqTab[["schema"]] <- list(fields=fields)
    
    rows <- list()
    if (run) {
      if (wantsSplit) {
        # also loop over the levels
        for (lev in splitLevels) {
          t <- table(column[splitFactor==lev])
          total <- sum(t)
          cFreq <- 0
          print(lev)
          print(t)
          
          for (i in seq_along(names(t))) {
            row <- list()
            row[["factor"]] <- lev
            row[["Level"]] <- names(t)[i]
            row[["Frequency"]] <- as.vector(t[i])
            cFreq <- cFreq + row[["Frequency"]]
            row[["Percent"]] <- row[["Frequency"]]/total*100
            row[["Valid Percent"]] <- row[["Frequency"]]/total*100
            row[["Cumulative Percent"]] <- cFreq/total*100
            if (i==1) {
              row[[".isNewGroup"]] <- TRUE
            } else {
              row[[".isNewGroup"]] <- FALSE
            }
            rows[[length(rows) + 1]] <- row
          }
          
          rows[[length(rows) + 1]] <- list(
            "factor" = "",
            "Level" = "Total",
            "Frequency" = total,
            "Percent" = 100,
            "Valid Percent" = 100,
            "Cumulative Percent" = ""
          )
        }
        print(rows)
        
      } else {
        t <- table(column)
        total <- sum(t)
        cFreq <- 0
        
        for (lev in names(t)) {
          row <- list()
          row[["Level"]] <- lev
          row[["Frequency"]] <- as.numeric(t[lev])
          cFreq <- cFreq + row[["Frequency"]]
          row[["Percent"]] <- row[["Frequency"]]/total*100
          row[["Valid Percent"]] <- row[["Frequency"]]/total*100
          row[["Cumulative Percent"]] <- cFreq/total*100
          rows[[length(rows) + 1]] <- row
        }
        
        rows[[length(rows) + 1]] <- list(
          "Level" = "Total",
          "Frequency" = total,
          "Percent" = 100,
          "Valid Percent" = 100,
          "Cumulative Percent" = ""
        )
      }
      
      freqTab[["data"]] <- rows
      freqTab[["status"]] <- "complete"
    } else {
      # init
      
			data <- list()

			for (level in levels(column))
				data[[length(data)+1]] <- list(Level=level)

			data[[length(data)+1]] <- list(Level="Total", "Cumulative Percent"="")

			freqTab[["data"]] <- data
      freqTab[["status"]] <- "inited"

      
    }
    
    
    freqTabs[[length(freqTabs) + 1]] <- freqTab
  }
  freqTabs
  
}

.descriptivesMatrixPlot <- function(dataset, options, name, run) {

  matrix.plot <- NULL

  if (run && length(unlist(options$variables)) > 0) {
    
    if (nrow(dataset) < 3) {
      matrix.plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Too few rows")
      matrix.plot[["status"]] <- "complete"
      return(matrix.plot)
    }
    
    variables <- unlist(options$variables)
    l <- length(variables)
    # check variables
    d <- vector("character", length(.v(variables)))
    sdCheck <- vector("numeric", length(.v(variables)))
    infCheck <- vector("logical", length(.v(variables)))

    for (i in seq_along(.v(variables))) {
      variable2check <- na.omit(dataset[[.v(variables)[i]]])
      d[i] <- class(variable2check)
      sdCheck[i] <- sd(variable2check) > 0
      infCheck[i] <- all(is.finite(variable2check))
    }

    numericCheck <- d == "numeric" | d == "integer"
    variables <- .v(variables)
    variable.statuses <- vector("list", length(variables))

    for (i in seq_along(variables)) {

      variable.statuses[[i]]$unplotable <- FALSE
      variable.statuses[[i]]$plottingError <- NULL

      if ( ! (numericCheck[i] && sdCheck[i] && infCheck[i])) {

        variable.statuses[[i]]$unplotable <- TRUE

        if ( ! numericCheck[i]) {
          variable.statuses[[i]]$plottingError <- "Variable is not continuous"
        } else if ( ! infCheck[i]) {
          variable.statuses[[i]]$plottingError <- "Variable contains infinity"
        } else if ( ! sdCheck[i]) {
          variable.statuses[[i]]$plottingError <- "Variable has zero variance"
        }

      }
    }

    if (l > 0) {
      width <- 250 * l + 20
      height <- 250 * l + 20
      
      matrix.plot <- list()
      plot <- list()
      plot[["title"]] <- name
      plot[["width"]]  <- width
      plot[["height"]] <- height
      matrix.plot <- plot
    }
    
    if (length(variables) > 0) {
      .matrixPlotFunc <- function(){
        if (l == 1) {
          par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 0, 0, 0))
          if ( ! variable.statuses[[1]]$unplotable) {
            .plotMarginalCor(dataset[[variables[1]]])
            mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)
          } else {
            .displayError(variable.statuses[[1]]$plottingError)
          }
        } else if (l > 1) {
          par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))
          for (row in seq_len(l)) {
            for (col in seq_len(l)) {
              if (row == col) {
                if ( ! variable.statuses[[row]]$unplotable) {
                    .plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
                } else {
                  .displayError(variable.statuses[[row]]$plottingError)
                }
              }
              if (col > row) {
                if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                  .plotScatterDescriptives(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
                } else {
                  errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                  .displayError(errorMessages[1])
                }
              }
              if (col < row) {
                plot(1, type= "n", axes= FALSE, ylab="", xlab="")
              }
            }
          }
        }

        if (l > 1) {
          textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
          for (t in seq_along(textpos)) {
            mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
            mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
          }
        }
      }

      imgObj <- .writeImage(width, height, plot=.matrixPlotFunc)

      plot <- matrix.plot
      plot[["data"]]  <- imgObj[["png"]]
      plot[["obj"]] <- imgObj[["obj"]]
      plot[["convertible"]] <- TRUE
      
      matrix.plot <- plot

    }
    
  } else {
    # Initialise the plot
    variables <- unlist(options$variables)
    variables <- .v(variables)
    l <- length(variables)
    if (l > 0) {
      
        width <- 250 * l + 20
        height <- 250 * l + 20
      
      plot <- list()
      plot[["title"]] <- name
      plot[["width"]]  <- width
      plot[["height"]] <- height
      matrix.plot <- plot
    }
  }
  
  matrix.plot
}

.descriptivesFrequencyPlots <- function(dataset, options, run, variable) {
    
  plotResult <- NULL
  if (options$splitby != "" ) {
    # return a collection
    coll <- list()
    split <- names(dataset)
    for (l in split){
      plot <- list()
      plot[["title"]] <- l
      plot[["name"]]  <- variable
      plot[["width"]]  <- options$plotWidth
      plot[["height"]] <- options$plotHeight
      plot[["custom"]] <- list(width="plotWidth", height="plotHeight")
      
      select <- split==l
      column <- dataset[[l]][[.v(variable)]]
      
      if (run == FALSE) {

        plotFunc <- function(){
          .barplotJASP(variable=variable, dontPlotData=TRUE)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
                
      } else if (any(is.infinite(column))) {

        plotFunc <- function(){
          .barplotJASP(variable=variable, dontPlotData=TRUE)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc,
                                obj = FALSE)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
        plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Variable contains infinity")
        plot[["status"]] <- "complete"

      } else if (length(column) < 3) {
        
        plotFunc <- function(){
          .barplotJASP(variable=variable, dontPlotData=TRUE)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc,
                                obj = FALSE)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
        plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Too few rows")
        plot[["status"]] <- "complete"
        
      } else if (length(column) > 0 && is.factor(column) || is.numeric(column) && length(column) > 0  && all(column %% 1 == 0) && length(unique(column)) <= 24) {

        if ( ! is.factor(column)) {

          column <- as.factor(column)
        }

        plotFunc <- function(){
          .barplotJASP(column, variable)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
        plot[["convertible"]] <- TRUE
        plot[["status"]] <- "complete"

      } else if (length(column) > 0 && !is.factor(column)) {

        if (any(is.infinite(column))) {

          plotFunc <- function(){
            .barplotJASP(variable=variable, dontPlotData=TRUE)
          }
          imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc,
                                  obj = FALSE)
                                  
          plot[["data"]] <- imageObj[["png"]]
          plot[["obj"]] <- imageObj[["obj"]]
          plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Variable contains infinity")
          plot[["status"]] <- "complete"

        } else {

          plotFunc <- function(){
            .plotMarginal(column, variableName=variable)
          }
          imageObj <- .writeImage(options$plotWidth, options$plotHeight, 
                                  plotFunc)
                                  
          plot[["data"]] <- imageObj[["png"]]
          plot[["obj"]] <- imageObj[["obj"]]
          plot[["convertible"]] <- TRUE
          plot[["status"]] <- "complete"
        }

      }
      coll[[l]] <- plot
    }
    
    plotResult <- list(collection = coll,
                       title = variable)
    
  } else {
    
    plot <- list()
    
    plot[["title"]] <- variable
    plot[["name"]]  <- variable
    plot[["width"]]  <- options$plotWidth
    plot[["height"]] <- options$plotHeight
    plot[["custom"]] <- list(width="plotWidth", height="plotHeight")

    column <- dataset[[ .v(variable) ]]
    column <- column[!is.na(column)]

    if (run == FALSE) {

      plotFunc <- function(){
        .barplotJASP(variable=variable, dontPlotData=TRUE)
      }
      imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc)
                              
      plot[["data"]] <- imageObj[["png"]]
      plot[["obj"]] <- imageObj[["obj"]]
              
    } else if (any(is.infinite(column))) {

      plotFunc <- function(){
        .barplotJASP(variable=variable, dontPlotData=TRUE)
      }
      imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc,
                              obj = FALSE)
                              
      plot[["data"]] <- imageObj[["png"]]
      plot[["obj"]] <- imageObj[["obj"]]
      plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Variable contains infinity")
      plot[["status"]] <- "complete"

    } else if (length(column) > 0 && is.factor(column) || is.numeric(column) && length(column) > 0  && all(column %% 1 == 0) && length(unique(column)) <= 24) {

      if ( ! is.factor(column)) {

        column <- as.factor(column)
      }

      plotFunc <- function(){
        .barplotJASP(column, variable)
      }
      imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc)
                              
      plot[["data"]] <- imageObj[["png"]]
      plot[["obj"]] <- imageObj[["obj"]]
      plot[["convertible"]] <- TRUE
      plot[["status"]] <- "complete"

    } else if (length(column) > 0 && !is.factor(column)) {

      if (any(is.infinite(column))) {

        plotFunc <- function(){
          .barplotJASP(variable=variable, dontPlotData=TRUE)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, plotFunc,
                                obj = FALSE)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
        plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Variable contains infinity")
        plot[["status"]] <- "complete"

      } else {

        plotFunc <- function(){
          .plotMarginal(column, variableName=variable)
        }
        imageObj <- .writeImage(options$plotWidth, options$plotHeight, 
                                plotFunc)
                                
        plot[["data"]] <- imageObj[["png"]]
        plot[["obj"]] <- imageObj[["obj"]]
        plot[["convertible"]] <- TRUE
        plot[["status"]] <- "complete"
      }

    }
    
    plotResult <- plot
  }
      
  plotResult
}

.descriptivesSplitPlot <- function(dataset, options, run, variable){

  # Initialisation plot
  .initSplitPlot <- function(){
    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="",
         ylab="")
    axis(2, at=0:1, labels=FALSE, cex.axis= 1.4, ylab="")
    mtext(text = variable, side = 1, cex=1.5, line = 3)
  }


  # Define custom y axis function
  base_breaks_y <- function(x){
    b <- pretty(x)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y,
                                                    xend=xend, yend=yend),
                               size = 0.75,
                               inherit.aes=FALSE),
         ggplot2::scale_y_continuous(breaks=b))
  }

  # Plot
  splitPlot <- list()


  y <- na.omit(dataset[[.v(variable)]])

  splitPlot[["title"]] <- variable
  splitPlot[["name"]]  <- variable
  splitPlot[["width"]]  <- options$plotWidth
  splitPlot[["height"]] <- options$plotHeight
  splitPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")

 if (run == FALSE){
      
    image <- .writeImage(width = options$plotWidth, height = options$plotHeight,
                        plot = .initSplitPlot, obj = FALSE)
    splitPlot[["data"]] <- image[["png"]]

  } else if (!is.numeric(y)) {

    image <- .writeImage(width = options$plotWidth, height = options$plotHeight,
                        plot = .initSplitPlot, obj = FALSE)
    splitPlot[["data"]] <- image[["png"]]
    splitPlot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Variable is not numeric!")
    splitPlot[["status"]] <- "complete"

  } else if (!(options$splitPlotViolin || options$splitPlotBoxplot ||
             options$splitPlotJitter)) {

    image <- .writeImage(width = options$plotWidth, height = options$plotHeight,
                        plot = .initSplitPlot, obj = FALSE)
    splitPlot[["data"]] <- image[["png"]]
    splitPlot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: No plot type selected!")
    splitPlot[["status"]] <- "complete"
      
  } else {

    if (is.null(dataset[[.v(options$splitby)]])){
      group <- factor(rep("",length(y)))
      xlab <- "Total"
      boxWidth <- 0.2
      vioWidth <- 0.3
    } else {
      group <- as.factor(dataset[[.v(options$splitby)]])[!is.na(dataset[[.v(variable)]])]
      xlab <- options$splitby
      boxWidth <- 0.4
      vioWidth <- 0.6
    }

    plotDat <- data.frame(group = group, y = y)

    # Identify outliers to label
    plotDat$outlier <- FALSE
    for (level in levels(plotDat$group)){
      v <- plotDat[plotDat$group == level,]$y
      quantiles <- quantile(v, probs=c(0.25,0.75))
      IQR <- quantiles[2]-quantiles[1]
      plotDat[plotDat$group == level,]$outlier <-
        v < (quantiles[1]-1.5*IQR) | v > (quantiles[2]+1.5*IQR)
    }

    plotDat$label <- ifelse(plotDat$outlier,row.names(plotDat),"")

    if (options$splitPlotColour){
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = group, y,
                                                 fill = group)) +
        ggplot2::scale_fill_hue(c=60, l=80) +
        ggplot2::scale_colour_hue(c=60, l=80)
    } else {
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = group, y,
                                                 fill = group)) +
        ggplot2::scale_fill_manual(values=rep("grey", nlevels(group))) +
        ggplot2::scale_colour_manual(values=rep("grey", nlevels(group)))
    }

    if (options$splitPlotViolin && options$splitPlotBoxplot && options$splitPlotJitter){

      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth,
                                    scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2) +
        ggplot2::geom_boxplot(size = 0.75, width = boxWidth,
                              outlier.shape = NA) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth,
                             fill = "transparent", scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1,
                             position = ggplot2::position_jitter(width=0.05, height = 0),
                             fill = "transparent")

    } else if (options$splitPlotBoxplot && options$splitPlotViolin){

      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth,
                                    scale = "width") +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2)+

        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth) +
        ggplot2::geom_violin(trim = F, size = 0.75, width = vioWidth,
                             fill = "transparent", scale = "width")

    } else if (options$splitPlotBoxplot && options$splitPlotJitter) {

      p <- p + ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth/2 ) +

        ggplot2::geom_boxplot(size = 0.75, outlier.shape = NA, width = boxWidth) +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1,
                             position = ggplot2::position_jitter(width=0.05, height = 0),
                             fill = "transparent")

    } else if (options$splitPlotViolin && options$splitPlotJitter){

      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, width = 0.75*boxWidth,
                                    scale = "width") +
        ggplot2::geom_jitter(size = 2.5, shape = 1, stroke = 1,
                             position = ggplot2::position_jitter(width=0.05, height = 0),
                             fill = "transparent")

    } else if (options$splitPlotViolin){

      p <- p + ggplot2::geom_violin(trim = F, size = 0.75, scale = "width",
                                    width = 0.75*boxWidth)

    } else if (options$splitPlotBoxplot){

      p <- p + ggplot2::stat_boxplot(geom = "errorbar",size = 0.75, width = boxWidth/2 )+

        ggplot2::geom_boxplot(size = 0.75, outlier.size = 1.5, width = boxWidth)

    } else if (options$splitPlotJitter){

      p <- p + ggplot2::geom_jitter(size = 2.5, ggplot2::aes(colour = group),
                                    position = ggplot2::position_jitter(width=0.1, height = 0))

    }

    if (options$splitPlotOutlierLabel && (options$splitPlotBoxplot || options$splitPlotJitter)){
      p <- p + ggplot2::geom_text(ggplot2::aes(label=label), hjust=-0.3)
    }

    ### Theming & Cleaning
    p <- p + ggplot2::xlab(xlab) +
      ggplot2::ylab(variable) +
      base_breaks_y(y) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 18),
        panel.grid.major = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 18, vjust=0.1),
        axis.title.y = ggplot2::element_text(size = 18, vjust=0.9),
        axis.text.x = ggplot2::element_text(size = 15),
        axis.text.y = ggplot2::element_text(size = 15),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.border = ggplot2::element_blank(),
        axis.line =  ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_line(size = 0.5),
        axis.ticks.length = grid::unit(3, "mm"),
        axis.ticks.margin = grid::unit(1,"mm"),
        plot.margin = grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
        legend.position = "none")


    image <- .writeImage(width = options$plotWidth, 
                         height = options$plotHeight, 
                         plot = p)
    
    splitPlot[["data"]] <- image[["png"]]
    splitPlot[["obj"]] <- image[["obj"]]
    splitPlot[["convertible"]] <- TRUE
    splitPlot[["status"]] <- "complete"

  }

  splitPlot
  
}


# <editor-fold> HELPER FUNCTIONS BLOCK ----

.plotMarginal <- function(variable, variableName, cexYlab= 1.3, lwd= 2, rugs= FALSE){
  
  variable <- na.omit(variable)
  
  par(mar= c(5, 4.5, 4, 2) + 0.1)

  density <- density(variable)

  h <- hist(variable, plot = FALSE)
  jitVar <- jitter(variable)
  yhigh <- max(max(h$density), max(density$y))
  ylow <- 0
  xticks <- pretty(c(variable, h$breaks), min.n= 3)

  plot(1, xlim= range(xticks), ylim= c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
  h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
  ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks, cex.axis = 1.2)
  mtext(text = variableName, side = 1, cex=1.5, line = 3)
  par(las=0)
  ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), cex.axis= 1.5, mgp= c(3, 0.7, 0))

  if(rugs){
    rug(jitVar)
  }

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

  for(i in seq_along(yticks)){

    if(yticks[i] < 10^6){

      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)

    } else{

      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
    }
  }

  distLab <- max(nchar(yLabs))/1.8

  par(mar= c(5, 7.2, 4, 2) + 0.1)
  barplot(summary(column), cex.names= 1.3, axes= FALSE, ylim= range(yticks))
  axis(2, las=1, at= yticks, labels= yLabs, cex.axis= 1.4)
  mtext(text = variable, side = 1, cex=1.5, line = 3)
  mtext(text = "Frequency", side = 2, cex=1.5, line = distLab+2, las=0)
}

.plotScatterDescriptives <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2){

	d <- data.frame(xx= xVar, yy= yVar)
	d <- na.omit(d)
	xVar <- d$xx
	yVar <- d$yy

	# fit different types of regression
	fit <- vector("list", 1)# vector("list", 4)

	fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
	fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
	fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
	fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)

	# find parsimonious, best fitting regression model
	Bic <- vector("numeric", 4)

	for (i in 1:4) {

		Bic[i] <- BIC(fit[[i]])

	}

	bestModel <- which.min(Bic)

	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))

	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
	yticks <- pretty(c(ylow, yhigh))

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

  n <- length(x)
  s4 <- sum((x - mean(x))^4)
  s2 <- sum((x - mean(x))^2)
  v <- s2 / (n-1)
  a <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  b <- s4 / (v^2)
  c <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
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

  n <- length(x)
  SES <- sqrt((6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))))
  return(SES)
}

.descriptivesSEK <- function(x) {

  # Standard Error of Kurtosis
  # Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf

  n <- length(x)
  SEK <- 2 * .descriptivesSES(x) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
  return(SEK)
}

# </editor-fold> HELPER FUNCTIONS BLOCK
