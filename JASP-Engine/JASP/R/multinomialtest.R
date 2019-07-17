#
# Copyright (C) 2018 University of Amsterdam
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

MultinomialTest <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .multinomReadData(dataset, options)
  
  ready   <- options$factor != ""
  # Error checking
  errors <- .multinomCheckErrors(dataset, options)
  
  # Compute the results
  chisqResults <- .chisquareTest(jaspResults, dataset, options, ready)
  
  # Output tables and plots
  .multinomialMainTable(        jaspResults, chisqResults, options, ready)
  .multinomialDescriptivesTable(jaspResults, chisqResults, options, ready)
  .multinomialContainerPlots(   jaspResults, chisqResults, options)
  .multinomialDescriptivesPlot( jaspResults, chisqResults, options, ready)
  
  return()
}

# Preprocessing functions ----
.multinomReadData <- function(dataset, options) {
  fact <- NULL
  asnum <- NULL
  if (options$factor != "") {
    fact <- options$factor
    if (options$counts != "") {
      asnum <- options$counts
      if (options$exProbVar != "") 
        asnum <- c(asnum, options$exProbVar)
    }
  }
  if (!is.null(dataset)) {
    #return(dataset)
    return(.vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact))
  } else {
    return(.readDataSetToEnd(columns.as.numeric  = asnum, 
                             columns.as.factor   = fact,
                             exclude.na.listwise = NULL))
  }
}

.multinomCheckErrors <- function(dataset, options) {
  #need .hasErrors Checks
  .hasErrors(dataset, 
             type = c('negativeValues', 'infinity'), 
             all.target = c(options$counts), 
             exitAnalysisIfErrors = TRUE)
}

# Results functions ----
.chisquareTest <- function(jaspResults, dataset, options, ready) {
  # Run chi-square test and return jaspResults object
  #
  # Args:
  #   jaspResults:
  #   dataset: input dataset
  #   options: user options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   jaspResults Object (chisquare test)
  if (!ready) 
    return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateChisqResults"]])) 
    return(jaspResults[["stateChisqResults"]]$object)
  
  chisqResults <- NULL
  fact <- options$factor
  
  if(fact != ""){
    # first determine the hypotheses
    factorVariable <- dataset[[.v(fact)]]
    factorVariable <- factorVariable[!is.na(factorVariable)]
    factorVariable <- as.factor(factorVariable)
    nlev <- nlevels(factorVariable)
    
    if (options$counts != "") {
      counts <- dataset[[.v(options$counts)]]
      # omit count entries for which factor variable is NA
      counts <- counts[!is.na(factorVariable)]
      # check for invalid counts
      c <- dataset[[.v(options$counts)]]
      .checkCountsMultinomial(counts, nlev)
      if (length(c) != length(levels(factorVariable)))
        .quitAnalysis("Invalid counts: the number of counts does not equal the number of categories. Check your count dataset!")
      factorVariable <- factor(rep(factorVariable, c), levels = levels(factorVariable))
    } 
    
    dataTable <- table(factorVariable)
    
    hyps <- .multinomialHypotheses(dataset, options, nlev)
    
    # create a named list with as values the chi-square result objects
    chisqResults <- lapply(hyps, function(h) {
      # catch warning message and append to object if necessary
      csr <- NULL
      warn <- NULL
      csr <- withCallingHandlers(
        chisq.test(x = dataTable, p = h, rescale.p = TRUE,
                   simulate.p.value = options$simulatepval),
        warning = function(w) {
          warn <<- w$message
        }
      )
      csr[["warn"]] <- warn
      return(csr)
    })
  }
  # Save results to state
  jaspResults[["stateChisqResults"]] <- createJaspState(chisqResults)
  jaspResults[["stateChisqResults"]]$dependOn(c("factor", "counts", "exProbVar", 
                                                "hypothesis", "tableWidget", "VovkSellkeMPR"))
  
  # Return results object
  return(chisqResults)
}

.multinomComputeResults <- function(jaspResults, dataset, options, chisqResults, ready) {
  # Turn chi-square object into jaspResults 
  # object and creates results for descripties table
  #
  # Args:
  #   jaspResults:
  #   chisqResults:
  #   dataset: input dataset
  #   options: user options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   jaspResults Object (chisquare and descriptives results)
  
  if (!ready) return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateMultinomialResults"]])) 
    return(jaspResults[["stateMultinomialResults"]]$object)
  
  results <- list()
  if(!is.null(chisqResults)){
    
    # fill in results one row at a time
    for (r in 1:length(chisqResults)) {
      df   <- chisqResults[[r]][["parameter"]][["df"]]
      pVal <- chisqResults[[r]][["p.value"]]
      if (is.na(df)) 
        df <- "-" # This happens when the simulate.p.value option is TRUE, i.e. using monte carlo
      results[[r]] <- list(case = names(chisqResults)[r],
                           chisquare = .clean(chisqResults[[r]][["statistic"]][["X-squared"]]),
                           df = .clean(df),
                           p = pVal)
      
      if (options$VovkSellkeMPR)
        results[[r]][["VovkSellkeMPR"]] <- .VovkSellkeMPR(pVal)
    }
  } 
  
  # Save results to state
  jaspResults[["stateMultinomialResults"]] <- createJaspState(results)
  jaspResults[["stateMultinomialResults"]]$dependOn(c("factor", "counts", "exProbVar", "tableWidget",
                                                      "VovksellkeMPR", "hypothesis"))
  
  # Return results object
  return(results)
}

.multinomDescriptivesResults <- function(jaspResults, dataset, options, chisqResults, ready){
  if(!options$descriptives)
    return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateDescriptivesResults"]])) 
    return(jaspResults[["stateDescriptivesResults"]]$object)
  
  results <- list()
  
  if (options$countProp == "descCounts")
    div <- 1
  else 
    div <- sum(chisqResults[[1]][["observed"]])
  
  nms <- names(chisqResults)
  tableFrame <- data.frame(
    factor   = names(chisqResults[[1]][["observed"]]),
    observed = as.numeric(chisqResults[[1]][["observed"]])/div,
    stringsAsFactors = FALSE
  )
  
  for (r in chisqResults)
    tableFrame <- cbind(tableFrame, r[["expected"]]/div)
  
  if (length(nms) == 1)
    colnames(tableFrame)[-(1:2)] <- "expected"
  else 
    colnames(tableFrame)[-(1:2)] <- nms
  
  # Add confidenceInterval to the tableFrame
  if (options$confidenceInterval){
    ciDf       <- .multComputeCIs(chisqResults[[1]][["observed"]], 
                                  options$confidenceIntervalInterval, 
                                  scale = options$countProp)
    tableFrame <- cbind(tableFrame, ciDf)
  }
  
  for (i in 1:nrow(tableFrame))
    results[["data"]][[i]] <- as.list(tableFrame[i,])
  
  # Save results to state
  jaspResults[["stateDescriptivesResults"]] <- createJaspState(results)
  jaspResults[["stateDescriptivesResults"]]$dependOn(c("factor", "counts", "exProbVar", "confidenceInterval", "tableWidget",
                                                      "confidenceIntervalInterval", "VovksellkeMPR", "hypothesis"))
  
  # Return results object
  return(results)
}

# Output functions ----
.multinomialMainTable <- function(jaspResults, chisqResults, options, ready) {
  # Transform chi-square test object into table for JASP
  # chisqResults = list(H1 = obj, H2 = obj, ....)
  #
  # Args:
  #   jaspResults:
  #   multinomResults:
  #   options: input options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   Chi square table
  
  if (!is.null(jaspResults[["chisqTable"]])) return()
  
  # Create table
  chisqTable <- createJaspTable(title = "Multinomial Test")
  chisqTable$dependOn(optionsFromObject = jaspResults[["stateMultinomialResults"]])
  chisqTable$showSpecifiedColumnsOnly <- TRUE
  
  # Add columns to table
  chisqTable$addColumnInfo(name = "case",      title = "", type = "string", combine = TRUE)
  chisqTable$addColumnInfo(name = "chisquare", title = "\u03C7\u00B2", type = "number", format = "sf:4;dp:3")
  chisqTable$addColumnInfo(name = "df", title = "df", type="integer")
  chisqTable$addColumnInfo(name = "p",  title = "p",  type="number", format="dp:3;p:.001")
  
  # include Vovk-Selke p-ratio as columns
  if (options$VovkSellkeMPR) {
    chisqTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number", format = "sf:4;dp:3")
    message <- "Vovk-Sellke Maximum <em>p</em>-Ratio: Based the <em>p</em>-value, 
    the maximum possible odds in favor of H\u2081 over H\u2080 equals 
    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 
    (Sellke, Bayarri, & Berger, 2001)."
    chisqTable$addFootnote(message, symbol = "\u002A")
  }
  
  jaspResults[["chisqTable"]] <- chisqTable
  
  .multinomialReturnOrFill(jaspResults, dataset, options, chisqResults, ready, 
                           .multinomComputeResults,  chisqTable)
}

.multinomialDescriptivesTable <- function(jaspResults, chisqResults, options, ready) {
  # Create multinomial descriptives table
  #
  # Args:
  #   jaspResults
  #   chisqResults:
  #   multinomResults:
  #   options: user options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   Descriptives table
  
  if(!options$descriptives) 
    return()
  
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()
  
  fact <- NULL
  if (options$factor != "")
    fact <- options$factor
  
  descriptivesTable <- createJaspTable(title = "Descriptives")
  descriptivesTable$dependOn(optionsFromObject = jaspResults[["stateDescriptivesResults"]])
  
  if(options$factor == ""){
    descriptivesTable$addColumnInfo(name="factor", title="Factor", type = "string")
    if (options$countProp == "descCounts")
      descriptivesTable$addColumnInfo(name="observed", title="Observed", type = "integer")
    else
      descriptivesTable$addColumnInfo(name="expected", title="Expected", type = "number", format = "sf:4;dp:3")
    
    if (options$confidenceInterval){
      interval <- 100 * options$confidenceIntervalInterval
      title <- paste0(interval, "% Confidence Interval")
      descriptivesTable$addColumnInfo(name="lowerCI", title="Lower", type = "number", format = "sf:4;dp:3", overtitle = title)
      descriptivesTable$addColumnInfo(name="upperCI", title="Upper", type = "number", format = "sf:4;dp:3", overtitle = title)
      message <- "Confidence intervals are based on independent binomial distributions."
      descriptivesTable$addFootnote(message, symbol = "<em>Note.</em>")
    }
  }
  else {
    descriptivesTable$addColumnInfo(name="factor", title = fact, type = "string")
    if (options$countProp == "descCounts")
      descriptivesTable$addColumnInfo(name="observed", title="Observed", type = "integer")
    else
      descriptivesTable$addColumnInfo(name="observed", title="Observed", type = "number", format = "sf:4;dp:3")
    
    nms <- names(chisqResults)
    
    if (length(nms) == 1) {
      if (options$countProp == "descCounts")
        descriptivesTable$addColumnInfo(name="expected", title = paste0("Expected: ", nms), type = "integer")
      else
        descriptivesTable$addColumnInfo(name="expected", title = paste0("Expected: ", nms), type = "number", format = "sf:4;dp:3")
      
    } else {
      for (i in 1:length(nms)) {
        if (options$countProp == "descCounts")
          descriptivesTable$addColumnInfo(name=nms[i], title = nms[i], type = "integer", overtitle = "Expected")
        else
          descriptivesTable$addColumnInfo(name=nms[i], title = nms[i], type = "number", format = "sf:4;dp:3", overtitle = "Expected")
      }
    }
    
    if (options$confidenceInterval){
      interval <- 100 * options$confidenceIntervalInterval
      title <- paste0(interval, "% Confidence Interval")
      descriptivesTable$addColumnInfo(name="lowerCI", title="Lower", type = "number",  format = "sf:4;dp:3", overtitle = title)
      descriptivesTable$addColumnInfo(name="upperCI", title="Upper", type = "number",  format = "sf:4;dp:3", overtitle = title)
      message <- "Confidence intervals are based on independent binomial distributions."
      descriptivesTable$addFootnote(message, symbol = "<em>Note.</em>")
      ciInfo <- multinomResults[["descriptives"]][["data"]][[paste0(interval, "% Confidence Interval")]]
      if (any(is.nan(unlist(ciInfo[, c('lowerCI', 'upperCI')])))){
        descriptivesTable$addFootnote(message = "Could not compute confidence intervals.")
      }
    }
  }  
  
  jaspResults[["descriptivesTable"]] <- descriptivesTable
  
  .multinomialReturnOrFill(jaspResults, dataset, options, chisqResults, ready, 
                           .multinomDescriptivesResults, descriptivesTable)
}

.multinomialContainerPlots <- function(jaspResults, chisqResults, options) {
  # Create container for descriptives plot
  #
  # Args:
  #   jaspResults:
  #   chisqResults:
  #   options: user options
  #   
  #
  # Return:
  #   jaspContainer for Descriptives plot
  if (!options$descriptivesPlot) return()
  
  if (is.null(jaspResults[["descriptivesPlot"]])) {
    jaspResults[["descriptivesPlot"]] <- createJaspContainer("Descriptives plot")
    jaspResults[["descriptivesPlot"]]$dependOn(c("descriptivesPlotConfidenceInterval", "countProp"))
  }
}

.multinomialDescriptivesPlot <- function(jaspResults, chisqResults, options, ready) {
  # Create multinomial descriptives plot
  #
  # Args:
  #   jaspResults:
  #   chisqResults:
  #   options: user options
  #   ready: need factor field filled in order to compute results return blank plot if "no factors" error 
  #   
  #
  # Return:
  #   Descriptives plot
  
  if(!options$descriptivesPlot)
    return()
  
  if(!is.null(jaspResults[["descriptivesPlot"]][["plot"]])) 
    return()
  
  if (!ready)
    p <- NULL
  else {
    # Generate the plot
    
    # Counts or props
    if (options$countProp == "descCounts") {
      div <- 1
      yname <- "Observed counts"
    } else {
      div <- sum(chisqResults[[1]][["observed"]])
      yname <- "Observed proportions"
    }
    
    # we need to reverse the factor's levels because of the coord_flip later
    f <- names(chisqResults[[1]][["observed"]])
    plotFrame <- data.frame(
      factor = factor(f, levels = rev(f)),
      obs = as.numeric(chisqResults[[1]][["observed"]])/div
    )
    
    # Calculate confidence interval
    if (options$descriptivesPlotConfidenceInterval){
      ciDf       <- .multComputeCIs(chisqResults[[1]][["observed"]], options$descriptivesPlotConfidenceInterval, 
                                    ifErrorReturn = 0, scale = options$countProp)
      plotFrame  <- cbind(plotFrame, ciDf)
    }
    
    # Define custom y axis function
    base_breaks_y <- function(x){
      b <- pretty(c(0,x))
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y,
                                                      xend=xend, yend=yend),
                                 size = 0.75,
                                 inherit.aes=FALSE),
           ggplot2::scale_y_continuous(breaks=b))
    }
    
    # Determine y-axis margin: If CIs could not be computed, use observed counts
    plotFrame$yAxisMargin <- plotFrame$upperCI
    for(i in 1:nrow(plotFrame))
      if(plotFrame$upperCI[i] == 0)
        plotFrame$yAxisMargin[i] <- plotFrame$obs[i]
    
    # Create plot
    p <- ggplot2::ggplot(data = plotFrame,
                         mapping = ggplot2::aes(x = factor, y = obs)) +
      ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                        fill = "grey") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ciDf$lowerCI,
                                          ymax = ciDf$upperCI),
                             size = 0.75, width = 0.3) +
      base_breaks_y(plotFrame$yAxisMargin) +
      ggplot2::xlab(options$factor) +
      ggplot2::ylab(yname)
    
    p <- JASPgraphs::themeJasp(p, horizontal = TRUE, xAxis = FALSE)
    
  }
  descriptivesPlot <- createJaspPlot(plot=p, width=500, aspectRatio=0.7, title="")
  jaspResults[["descriptivesPlot"]][["plot"]] <- descriptivesPlot
  return(descriptivesPlot)
}

# Extra results functions
.multinomialHypotheses <- function(dataset, options, nlevels) {
  # Transform input into a list of hypotheses
  # This function transforms the input into a list of hypotheses
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels:
  #
  # Return:
  #   hypotheses
  
  hyps <- list()
  if (options$exProbVar == "" && options$hypothesis == "multinomialTest") {
    # Expected probabilities are simple now
    hyps[["Multinomial"]] <- rep(1/nlevels, nlevels)
  } else {
    # First, generate a table with expected probabilities based on input
    expectedDf <- .generateExpectedProps(dataset, options, nlevels)
    # assign each hypothesis to the hyps object
    hyps <- as.list(expectedDf)
  }
  return(hyps)
}

.generateExpectedProps <- function(dataset, options, nlevels) {
  # Parse expected probabilities/counts
  # This function returns a data frame with in each named column the expected
  # probabilities. The column names are the hypothesis names.
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels:
  #
  # Return:
  #   expected Probabilities
  
  if (options$exProbVar != "") {
    if (options$counts == "") 
      stop("Expected counts not supported without observed counts!")
    # use only exProbVar
    fact   <- dataset[[.v(options$factor)]]
    eProps <- dataset[.v(options$exProbVar)]
    colnames(eProps) <- options$exProbVar
    rownames(eProps) <- fact
    
    # Exclude missing values
    eProps           <- na.omit(eProps)
    # Check for invalid expected counts
    .checkCountsMultinomial(eProps[[1]], nlevels, expectedCounts = TRUE)
    
    return(eProps)
    
  } else if (length(options$tableWidget) > 0) {
    eProps <- sapply(options$tableWidget, function(x) {
      vals <- unlist(x$values)
      if (sum(vals) == 0)
        vals <- rep(1, length(vals))
      else 
        vals
    })
    
    colnames(eProps) <- sapply(seq_along(options$tableWidget), function(x) paste0("H\u2080 (", letters[x], ")"))
    rownames(eProps) <- options$tableWidget[[1]]$levels
    
    return(as.data.frame(eProps))
  } else 
    stop("No expected counts entered!")
}

.checkCountsMultinomial <- function(counts, nlevels, expectedCounts = FALSE){
  
  if(expectedCounts)
    variable <- "Invalid expected counts: "
  else 
    variable <- "Invalid counts: "

  # discard missing values
  counts <- counts[!is.na(counts)]
  
  if (nlevels != length(counts)) 
    .quitAnalysis(paste0(variable, "variable does not match the number of levels of factor."))
  
  # only applies for observed counts, expected counts can be proportions
  if (!expectedCounts && !all(counts == round(counts)))
    .quitAnalysis(paste0(variable, "variable must contain only integer values."))
}

.multinomialReturnOrFill <- function(jaspResults, dataset, options, 
                                     chisqResults, ready, .resultfunction, table){
  if (!ready) 
    return()
  res <- try(.resultfunction(jaspResults, dataset, options, chisqResults, ready))
  if(isTryError(res))
    table$setError(res)
  else {
    for (level in 1:length(res)) {
      row <- res[[level]]
      table$addRows(row)
    }
  }
}
