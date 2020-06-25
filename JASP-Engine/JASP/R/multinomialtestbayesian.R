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


MultinomialTestBayesian <- function(jaspResults, dataset, options, ...) {

  dataset            <- .multinomReadData(dataset, options)

  .multinomCheckErrors(dataset, options)

  multinomialResults <- .computeMultinomialResults(jaspResults, dataset, options)

  .createMultBayesMainTable(jaspResults, options, multinomialResults)
  .createMultBayesDescriptivesTable(jaspResults, options, multinomialResults)
  .createMultBayesDescriptivesPlot(jaspResults, options, multinomialResults)

  return()
}

#' Compute results for multinomial table
#'
#' @param jaspResults
#' @param dataset
#' @param options user input options
#'
#' @return multinomialResults results table
.computeMultinomialResults <- function(jaspResults, dataset, options) {

  # Take results from state if possible
  if (!is.null(jaspResults[["stateMultBayesResults"]]))
    return(jaspResults[["stateMultBayesResults"]]$object)

  # This will be the object that we fill with results
  multinomialResults <- list(mainTable         = list(),
                             descriptivesTable = list(),
                             descriptivesPlot  = list())

  # First, we perform a precalculation of variables we use throughout the analysis
  multinomialResults$specs <- .multBayesCalcSpecs(dataset, options)

  if (!multinomialResults$specs[["ready"]])
    return(multinomialResults)
  
  # Prepare for running the Bayesian Multinomial test
  factorVariable <- multinomialResults$specs$factorVariable
  countVariable  <- multinomialResults$specs$countVariable
  fact           <- dataset[[.v(factorVariable)]]
  fact           <- as.factor(fact)
  nlev           <- nlevels(fact)
  prior          <- options$priorCounts[[1]]
  a              <- setNames(prior$values, prior$levels)
  factNms        <- levels(fact)

  if (options$counts != "") {
    counts <- dataset[[.v(options$counts)]]
    # omit count entries for which factor variable is NA
    counts <- counts[!is.na(fact)]
    dataTable        <- counts
    names(dataTable) <- factNms
  } else {
    dataTable <- table(fact)
  }

  # Determine observed counts and factor levels
  N <- sum(dataTable)

  # Extract hypotheses
  hyps  <- .multinomialHypotheses(dataset, options, nlev) # exact equality constraints
  nms   <- multinomialResults[["specs"]][["hypNames"]]
  nhyps <- length(hyps)

  # Results for main table: Bayesian multinomial test
  multinomialResults$mainTable <- vector('list', length = nhyps)

  for(h in 1:nhyps){
    multinomialResults$mainTable[[nms[h]]] <- .multBayesBfEquality(alphas = a, counts = dataTable, thetas = hyps[[h]])
  }

  multinomialResults$mainTable[["prior"]]   <- a
  multinomialResults$mainTable[["levels"]]  <- factNms
  multinomialResults$mainTable[["nlevels"]] <- nlev
  multinomialResults$mainTable[["hypNames"]]<- nms
  multinomialResults$mainTable[["nhyps"]]   <- nhyps

  #  Results for descriptives plot
  multinomialResults$descriptivesPlot[["descProps"]]  <- .multComputeCIs(dataTable, options$descriptivesPlotCredibleInterval, ifErrorReturn = 0, scale = 'descProbs')
  multinomialResults$descriptivesPlot[["descCounts"]] <- multinomialResults$descriptivesPlot[["descProps"]] * N
  multinomialResults$descriptivesPlot[["descProps"]][["observed"]]  <- as.numeric(dataTable)/N
  multinomialResults$descriptivesPlot[["descCounts"]][["observed"]] <- as.numeric(dataTable)
  multinomialResults$descriptivesPlot[["descProps"]][["fact"]]  <- factNms
  multinomialResults$descriptivesPlot[["descCounts"]][["fact"]] <- factNms

  # Results for descriptives table
  multinomialResults$descriptivesTable[["descProps"]][["fact"]]      <- factNms
  multinomialResults$descriptivesTable[["descCounts"]][["fact"]]     <- factNms
  multinomialResults$descriptivesTable[["descProps"]][["observed"]]  <- as.numeric(dataTable)/N
  multinomialResults$descriptivesTable[["descCounts"]][["observed"]] <- as.numeric(dataTable)

  for(h in 1:nhyps) {
    multinomialResults$descriptivesTable[["descProps"]][[nms[h]]]    <- multinomialResults$mainTable[[nms[h]]]$expected/N
    multinomialResults$descriptivesTable[["descCounts"]][[nms[h]]]   <- multinomialResults$mainTable[[nms[h]]]$expected
  }

  multinomialResults$descriptivesTable[["descProps"]]    <- setNames(as.data.frame(multinomialResults$descriptivesTable[["descProps"]]), c("fact", "observed", nms))
  multinomialResults$descriptivesTable[["descCounts"]]   <- setNames(as.data.frame(multinomialResults$descriptivesTable[["descCounts"]]), c("fact", "observed", nms))
  multinomialResults$descriptivesTable[["descPropsCI"]]  <- .multComputeCIs(dataTable, options$credibleIntervalInterval, scale = "descProbs")
  multinomialResults$descriptivesTable[["descCountsCI"]] <- .multComputeCIs(dataTable, options$credibleIntervalInterval, scale = "descCounts") 

  # Save results to state
  defaultOptions <- multinomialResults$specs$defaultOptions
  jaspResults[["stateMultinomialBayesianResults"]] <- createJaspState(multinomialResults)
  jaspResults[["stateMultinomialBayesianResults"]]$dependOn(defaultOptions)

  return(multinomialResults)
}

#' Create and return the multinomial table
.createMultBayesMainTable <- function(jaspResults, options, multinomialResults){

  if (!is.null(jaspResults[["multinomialTable"]])) return()

  # Create table
  defaultOptions                    <- multinomialResults$specs$defaultOptions
  multinomialTable                  <- createJaspTable(title = gettext("Bayesian Multinomial Test"))
  multinomialTable$position         <- 1
  multinomialTable$dependOn(c(defaultOptions, "bayesFactorType"))

  # Bayes factor type
  if (options$bayesFactorType == "BF01") {
    bf.title <- "BF\u2080\u2081"
  } else if (options$bayesFactorType == "BF10") {
    bf.title <- "BF\u2081\u2080"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(\u0042\u0046\u2081\u2080)"
  }

  # Add columns
  multinomialTable$addColumnInfo(name = "case",    title = "",                type = "string", combine = TRUE)
  multinomialTable$addColumnInfo(name = "level",   title = gettext("Levels"), type = "integer")
  multinomialTable$addColumnInfo(name = "BF",      title = bf.title,          type = "number")

  jaspResults[["multinomialTable"]] <- multinomialTable

  # Show empty table if no variable is selected
  if(!multinomialResults$specs[["ready"]])
    return()

  # Add rows
  fact  <- multinomialResults[["specs"]][["factorVariable"]]
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["specs"]][["hypNames"]]

  for (h in 1:nhyps)
    multinomialTable$addRows(list(
      case  = nms[h],
      level = multinomialResults[["mainTable"]][["nlevels"]],
      BF    = multinomialResults[["mainTable"]][[nms[h]]][["BF"]][[options$bayesFactorType]]
    ))

}


#' Create and return the Descriptives table
#' If descriptives is not selected: do not create a table
#'
#' @param jaspResults
#' @param options user input options
#' @param multinomialResults results table from .computeMultinomialResults() function
#' @param bayesianAnalaysis set TRUE for Bayesian multinomial and FALSE for frequentist multinomial
#'
#' @return descriptivesTable descriptives table
.createMultBayesDescriptivesTable <- function(jaspResults, options, multinomialResults, bayesianAnalysis = TRUE){

  if(!options[["descriptives"]])
    return()

  # Create table
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))

  # settings for Bayesian and frequentist analysis
  if(bayesianAnalysis){
    ciRequested   <- options$credibleInterval
    ciInterval    <- options$credibleIntervalInterval
    ciType  <- gettext("Credible")
    tableFootnote <- gettext("Credible intervals are based on independent binomial distributions with flat priors.")
    descriptivesTable$dependOn(c("countProp", "descriptives", "credibleIntervalInterval"))
  } else {
    ciRequested   <- options$confidenceInterval
    ciInterval    <- options$confidenceIntervalInterval 
    ciType  <- gettext("Confidence")
    tableFootnote <- gettext("Confidence intervals are based on independent binomial distributions.")
    descriptivesTable$dependOn(c("countProp", "descriptives", "confidenceIntervalInterval"))
  }

  descriptivesTable$showSpecifiedColumnsOnly <- TRUE
  descriptivesTable$position <- 2

  factorVariable <- multinomialResults[["specs"]][["factorVariable"]]

  if(options$countProp == "descCounts")
    numberType <- "integer"
  else
    numberType <- "number"

  # Add columns
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["specs"]][["hypNames"]]

  descriptivesTable$addColumnInfo(name = "fact",     title = factorVariable,      type = "string", combine = TRUE)
  descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = numberType)

  # If no variable is selected, adjust column title
  if(is.null(nhyps)){
    descriptivesTable$addColumnInfo(name = "expected", title = gettext("Expected"), type = numberType)
  } else if(nhyps == 1) {
      descriptivesTable$addColumnInfo(name = nms, title = gettextf("Expected: %s", nms), type = numberType)
  } else if(nhyps > 1) {
    for(h in 1:nhyps){
      descriptivesTable$addColumnInfo(name = nms[h], title = nms[h], type = numberType, overtitle = gettext("Expected"))
    }
  }

  if (ciRequested) {
      overTitle <- gettextf("%1$s%% %2$s Interval", paste0(100 * ciInterval), ciType)
      descriptivesTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number",
                                      overtitle = overTitle)
      descriptivesTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number",
                                      overtitle = overTitle)
    } 

  jaspResults[["multinomialDescriptivesTable"]] <- descriptivesTable

  # Show empty table if no variable is selected
  if(!multinomialResults$specs[["ready"]])
    return()

  # Add rows
  descDF <- multinomialResults[["descriptivesTable"]][[options$countProp]]
  if(options$countProp == "descCounts"){
    descDF[nms] <- round(descDF[nms])
  }

  if(ciRequested){
    ciInfo <- multinomialResults[["descriptivesTable"]][[paste0(options$countProp, "CI")]]
    descDF <- cbind(descDF, ciInfo)
    descriptivesTable$addFootnote(tableFootnote)
    
    if (any(is.nan(unlist(descDF[, c('lowerCI', 'upperCI')])))) {
      descriptivesTable$addFootnote(message = gettextf("Could not compute %s Intervals.", tolower(ciType)))
    }
  } 
  
  descriptivesTable$setData(descDF)
}

#' Create Descriptives plot for Multinomial Bayesian Test
#'
#' @param jaspResults
#' @param options user input options
#' @param multinomialResults results table from .computeMultinomialResults() function
#'
#' @return descriptivesPlot descriptives plot object
.createMultBayesDescriptivesPlot <- function(jaspResults, options, multinomialResults) {
  # If there is no data OR descriptives plot is not selected: do not create a plot
  if(!options$descriptivesPlot)
    return()

  factorVariable   <- multinomialResults[["specs"]][["factorVariable"]]
  descriptivesPlot <- .multBayesPlotHelper(factorVariable, options, multinomialResults)

  jaspResults[["descriptivesPlot"]] <- createJaspPlot(plot = descriptivesPlot, title = gettext("Descriptives plot"), width = 480, height = 320)
  jaspResults[["descriptivesPlot"]]$dependOn(c("descriptivesPlot", "factor", "counts", "descriptivesPlotsCredibleInterval"))

  descriptivesPlot$position <- 2
}


#' Helper function - compute credible interval based on independent binomial distributions with flat Beta priors
#'
#' @param counts
#' @param CI
#'
#' @return ciDf credible/confidence intervals as data frame
.multComputeCIs <- function(counts, CI, ifErrorReturn = NaN, scale) {

  # set function behaviour, if analysis crashes
  errorReturn <- rep(ifErrorReturn, 2)
  div         <- ifelse(scale == 'descCounts', sum(counts), 1)
  N           <- sum(counts)
  
  ciDf   <- data.frame(lowerCI = NA, upperCI = NA)
  for(i in seq_along(counts)){
    # return results on count scale. If function crashes, return table with NaN's
    tryCatch(binomResult <- binom.test(counts[i], N, conf.level = CI)$conf.int * div, 
             error = function(e) {binomResult <<- errorReturn})
    ciDf[i, ] <- binomResult
  }
  
  return(ciDf)
}

#' Helper function - Generate descriptives plot
#'
#' @param factorVariable
#' @param options user input options
#' @param multinomialResults results table from .computeMultinomialResults() function
#'
#' @return p ggplot2 object
.multBayesPlotHelper <- function(factorVariable, options, multinomialResults) {
  # Default Plot
  if(!multinomialResults$specs[["ready"]])
    return()

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

  # Counts or props
  if (options$countProp == "descCounts") {
    yname <- gettext("Observed Counts")
  } else {
    yname <- gettext("Observed Proportions")
  }

  # Prepare data for plotting
  plotFrame <- multinomialResults[["descriptivesPlot"]][[options$countProp]]
  # We need to reverse the factor's levels because of the coord_flip later
  plotFrame$fact <- factor(plotFrame$fact, levels = rev(plotFrame$fact))
  
  # Determine y-axis margin: If CIs could not be computed, use observed counts
  plotFrame$yAxisMargin <- plotFrame$upperCI
  for(i in 1:nrow(plotFrame)){
    if(plotFrame$upperCI[i] == 0){
      plotFrame$yAxisMargin[i] <- plotFrame$obs[i]
    }   
  }

  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = fact, y = observed)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                      fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = plotFrame[["lowerCI"]],
                                        ymax = plotFrame[["upperCI"]]),
                           size = 0.75, width = 0.3) +
    base_breaks_y(plotFrame$yAxisMargin) +
    ggplot2::xlab(factorVariable) +
    ggplot2::ylab(yname) +
    ggplot2::coord_flip()

    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

  return(p)
}

#' Function for Analysis - compute Bayes factor and expected counts
#'
#' @param alphas
#' @param counts
#' @param thetas
#'
#' @return list list containing BF and expected counts
.multBayesBfEquality <- function(alphas, counts, thetas) {

  # if needed: rescale
  if(sum(thetas) != 1) {
    thetas <- thetas/sum(thetas)
  }

  # expected counts under the null hypothesis (Binomial median)
  expected <- setNames(sum(counts)*thetas, names(counts))

  # compute Bayes factor
  lbeta.xa <- sum(lgamma(alphas + counts)) - lgamma(sum(alphas + counts))
  lbeta.a  <- sum(lgamma(alphas)) - lgamma(sum(alphas))

  # in this case, counts*log(thetas) should be zero, omit to avoid numerical issue with log(0)
  if (any(rowSums(cbind(thetas, counts)) == 0)) {
    LogBF10 <- (lbeta.xa-lbeta.a)
  } else {
    LogBF10 <- (lbeta.xa-lbeta.a) + (0 - sum(counts * log(thetas)))
  }

  BF <- data.frame(LogBF10 = .clean(LogBF10),
                   BF10    = .clean(exp(LogBF10)),
                   BF01    = .clean(1/exp(LogBF10)))

  return(list(BF       = BF,
              expected = expected))
}

#' Init function - calculate specifications
#' @param dataset
#' @param options user input options
#'
#' @return specs
.multBayesCalcSpecs <- function(dataset, options){
  specs <- list()

  # Default options
  specs$defaultOptions <- c("tableWidget", "priorCounts","factor", "counts", "exProbVar", "hypothesis",
                            "credibleIntervalInterval")

  # Ready statement
  specs$ready <- options$factor != "" && !is.null(dataset)

  # Specify factor variable
  specs$factorVariable <- "Factor"
  specs$countVariable  <- NULL
  specs$exProbVariable <- NULL
  if (options$factor != "") {
    specs$factorVariable <- options$factor
    # Specify count variable
    if (options$counts != "") {
      specs$countVariable <- options$counts
      }
    if (options$exProbVar != "") {
        specs$exProbVariable <- options$exProbVar
      }
  }

  if(options$exProbVar != "") {
    specs$hypNames <- specs$exProbVariable
  } else if(options$hypothesis == "multinomialTest") {
    specs$hypNames <- "Multinomial"
  } else if(options$hypothesis == "expectedProbs") {
    specs$hypNames <- sapply(seq_along(options$tableWidget), function(x) paste0("H\u2080 (", letters[x], ")"))
  } else if(!is.null(specs$exProbVariable)) {
    specs$hypNames <- specs$exProbVariable
  }

  return(specs)
}