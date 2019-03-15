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

  jaspResults$title  <- "Bayesian Multinomial Test"
  dataset            <- .multinomialBayesReadData(dataset, options)

  .multinomialBayesCheckErrors(dataset, options)

  multinomialResults <- .computeMultinomialResults(jaspResults, dataset, options)

  .createMultBayesMainTable(jaspResults, options, multinomialResults)
  .createMultBayesDescriptivesTable(jaspResults, options, multinomialResults)
  .createMultBayesDescriptivesPlot(jaspResults, options, multinomialResults)

  return()
}


#' Funciton checks for errors
#'   1. Number of levels of the variables must be bigger than 1
#'   2. 0 observations for a level of a variable
#'
#' @param dataset
#' @param options user input options
.multinomialBayesCheckErrors <- function(dataset, options) {

  if (options$factor == "")
    return()

  fact <- options$factor

  # Error Check 1: Number of levels of the variables must be bigger than 1
  .hasErrors(dataset              = dataset,
             perform              = "run",
             type                 = "factorLevels",
             factorLevels.target  = fact,
             factorLevels.amount  = '< 1',
             exitAnalysisIfErrors = TRUE)

  # Error check 2: 0 observations for a level of a variable
  column <- dataset[[ .v(fact) ]]
  data   <- column[!is.na(column)]
  levels <- levels(as.factor(data))

  for (level in levels) {
    .hasErrors(dataset              = data[data == level],
               perfrom              = "run",
               type                 = "observations",
               observations.amount  = c('< 1'),
               exitAnalysisIfErrors = TRUE)
  }
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
  fact           <- as.factor(fact[!is.na(fact)])
  nlev           <- nlevels(fact)
  prior          <- options$priorCounts[[1]]
  a              <- setNames(prior$values, prior$levels)

  # If applicable, convert counts variable to factor
  if (!is.null(countVariable)) {
    counts <- dataset[[.v(countVariable)]]
    counts <- counts[!is.na(counts)]
    # Check for invalid counts
    .checkCountsMultinomial(counts, nlev)
    fact <- factor(rep(fact, counts), levels = levels(fact))
  }

  # Determine observed counts and factor levels
  t <- table(fact)
  N <- sum(t)

  # Extract hypotheses
  hyps  <- .multinomialHypotheses(dataset, options, nlev) # exact equality constraints
  nms   <- multinomialResults[["specs"]][["hypNames"]]
  nhyps <- length(hyps)

  # Results for main table: Bayesian multinomial test
  multinomialResults$mainTable <- vector('list', length = nhyps)

  for(h in 1:nhyps){
    multinomialResults$mainTable[[nms[h]]] <- .multBayesBfEquality(alphas = a, counts = t, thetas = hyps[[h]])
  }

  multinomialResults$mainTable[["prior"]]   <- a
  multinomialResults$mainTable[["levels"]]  <- levels(fact)
  multinomialResults$mainTable[["nlevels"]] <- length(levels(fact))
  multinomialResults$mainTable[["hypNames"]]<- nms
  multinomialResults$mainTable[["nhyps"]]   <- nhyps

  #  Results for descriptives plot
  multinomialResults$descriptivesPlot[["descProps"]]  <- .multComputeCIs(t, options$descriptivesPlotCredibleInterval)
  multinomialResults$descriptivesPlot[["descCounts"]] <- multinomialResults$descriptivesPlot[["descProps"]] * N
  multinomialResults$descriptivesPlot[["descProps"]][["observed"]]  <- as.numeric(t)/N
  multinomialResults$descriptivesPlot[["descCounts"]][["observed"]] <- as.numeric(t)
  multinomialResults$descriptivesPlot[["descProps"]][["fact"]]  <- levels(fact)
  multinomialResults$descriptivesPlot[["descCounts"]][["fact"]] <- levels(fact)

  # Results for descriptives table
  multinomialResults$descriptivesTable[["descProps"]][["fact"]]      <- levels(fact)
  multinomialResults$descriptivesTable[["descCounts"]][["fact"]]     <- levels(fact)
  multinomialResults$descriptivesTable[["descProps"]][["observed"]]  <- as.numeric(t)/N
  multinomialResults$descriptivesTable[["descCounts"]][["observed"]] <- as.numeric(t)

  for(h in 1:nhyps) {
    multinomialResults$descriptivesTable[["descProps"]][[nms[h]]]    <- multinomialResults$mainTable[[nms[h]]]$expected/N
    multinomialResults$descriptivesTable[["descCounts"]][[nms[h]]]   <- multinomialResults$mainTable[[nms[h]]]$expected
  }

  multinomialResults$descriptivesTable[["descProps"]]    <- setNames(as.data.frame(multinomialResults$descriptivesTable[["descProps"]]), c("fact", "observed", nms))
  multinomialResults$descriptivesTable[["descCounts"]]   <- setNames(as.data.frame(multinomialResults$descriptivesTable[["descCounts"]]), c("fact", "observed", nms))
  multinomialResults$descriptivesTable[["descPropsCI"]]  <- .multComputeCIs(t, options$credibleIntervalInterval)
  multinomialResults$descriptivesTable[["descCountsCI"]] <- .multComputeCIs(t, options$credibleIntervalInterval) * N

  # Save results to state
  defaultOptions <- multinomialResults$specs$defaultOptions
  jaspResults[["stateMultinomialBayesianResults"]] <- createJaspState(multinomialResults)
  jaspResults[["stateMultinomialBayesianResults"]]$dependOnOptions(defaultOptions)

  return(multinomialResults)
}

#' Create and return the multinomial table
.createMultBayesMainTable <- function(jaspResults, options, multinomialResults){

  if (!is.null(jaspResults[["multinomialTable"]])) return()

  # Create table
  defaultOptions                    <- multinomialResults$specs$defaultOptions
  multinomialTable                  <- createJaspTable(title = "Bayesian Multinomial Test")
  multinomialTable$position         <- 1
  multinomialTable$dependOnOptions(c(defaultOptions, "bayesFactorType"))

  # Bayes factor type
  if (options$bayesFactorType == "BF01") {
    bf.title <- "BF\u2080\u2081"
  } else if (options$bayesFactorType == "BF10") {
    bf.title <- "BF\u2081\u2080"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(\u0042\u0046\u2081\u2080)"
  }

  # Add columns
  multinomialTable$addColumnInfo(name = "case",    title = "",       type = "string", combine = TRUE)
  multinomialTable$addColumnInfo(name = "level",   title = "Levels", type = "integer")
  multinomialTable$addColumnInfo(name = "BF",      title = bf.title, type = "number", format="sf:4;dp:3")

  jaspResults[["multinomialTable"]] <- multinomialTable

  # Show empty table if no variable is selected
  if(!multinomialResults$specs[["ready"]])
    return()

  # Add rows
  fact  <- multinomialResults[["specs"]][["factorVariable"]]
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["specs"]][["hypNames"]]

  for (h in 1:nhyps) {
    row <- data.frame(case  = nms[h],
                       level = multinomialResults[["mainTable"]][["nlevels"]],
                       BF    = multinomialResults[["mainTable"]][[nms[h]]][["BF"]][[options$bayesFactorType]]
    )
    multinomialTable$addRows(row)
  }
}


#' Create and return the Descriptives table
#' If descriptives is not selected: do not create a table
#'
#' @param jaspResults
#' @param options user input options
#' @param multinomialResults results table from .computeMultinomialResults() function
#'
#' @return descriptivesTable descriptives table
.createMultBayesDescriptivesTable <- function(jaspResults, options, multinomialResults){

  if(!options[["descriptives"]])
    return()

  # Create table
  descriptivesTable                          <- createJaspTable(title = "Descriptives")
  descriptivesTable$dependOnOptions(c("countProp", "descriptives", "credibleIntervalInterval"))
  descriptivesTable$showSpecifiedColumnsOnly <- TRUE
  descriptivesTable$position                 <- 2

  factorVariable                             <- multinomialResults[["specs"]][["factorVariable"]]

  if(options$countProp == "descCounts") {
    numberType <- "integer"
    format <- NULL
  } else {
    numberType <- 'number'
    format = "sf:4;dp:3"
  }

  # Add columns
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["specs"]][["hypNames"]]

  descriptivesTable$addColumnInfo(name = "fact",   title = factorVariable, type = "string", combine = TRUE)
  descriptivesTable$addColumnInfo(name="observed", title = "Observed",     type = numberType, format = format)

  # If no variable is selected, adjust column title
  if(is.null(nhyps)){
    descriptivesTable$addColumnInfo(name = "expected", title = "Expected", type = numberType, format = format)
  } else if(nhyps == 1) {
      descriptivesTable$addColumnInfo(name = nms, title = paste0("Expected: ", nms), type = numberType, format = format)
  } else if(nhyps > 1) {
    for(h in 1:nhyps){
      descriptivesTable$addColumnInfo(name = nms[h], title = nms[h], type = numberType, , format = format, overtitle = "Expected")
    }
  }

  if (options$credibleInterval) {
    descriptivesTable$addColumnInfo(name = "lowerCI", title = "Lower", type = "number", format = "sf:4;dp:3",
                                    overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
    descriptivesTable$addColumnInfo(name = "upperCI", title = "Upper", type = "number", format = "sf:4;dp:3",
                                    overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
  }

  jaspResults[["multinomialDescriptivesTable"]] <- descriptivesTable

  # Show empty table if no variable is selected
  if(!multinomialResults$specs[["ready"]])
    return()

  # Add rows
  descDF <- multinomialResults[["descriptivesTable"]][[options$countProp]]
  if(options$countProp == "descCounts"){
    descDF[nms] <- round(descDF[nms])
    descDF[nms] <- apply(descDF[nms], 2, as.integer)
  }

  if(options$credibleInterval) {
    ciInfo <- multinomialResults[["descriptivesTable"]][[paste0(options$countProp, "CI")]]
    descDF <- cbind(descDF, ciInfo)
    descriptivesTable$addFootnote(message = "Credible intervals are based on marginal beta distributions.",
                                  symbol = "<em>Note.</em>")
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

  jaspResults[["descriptivesPlot"]] <- createJaspPlot(plot = descriptivesPlot, title = "Descriptives plot", width = 480, height = 320)
  jaspResults[["descriptivesPlot"]]$dependOnOptions(c("descriptivesPlot", "factor", "counts",
                                                      "descriptivesPlotsCredibleInterval"))

  descriptivesPlot$position <- 2
}


#' Helper function - compute credible interval
#'
#' @param counts
#' @param credibleInterval
#'
#' @return medianCI median credible interval as data frame
.multComputeCIs <- function(counts, credibleInterval) {

  # based on marginal beta distributions with uniform Dirichlet prior
  N             <- sum(counts)
  observedProps <- counts/N
  alphas        <- rep(1, length(counts))
  lower         <- (1 - credibleInterval)/2
  upper         <- 1 - lower

  medianCI           <- matrix(NA, ncol = 2, nrow = length(counts))
  colnames(medianCI) <- c('lowerCI', 'upperCI')
  for(i in seq_along(counts)){
    k <- counts[i]
    a <- alphas[i]
    b <- sum(alphas[-i])

    medianCI[i, ] <- qbeta(c(lower, upper), a + k, b + N - k)
  }

  return(as.data.frame(medianCI))
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
    yname <- "Observed Counts"
  } else {
    yname <- "Observed Proportions"
  }

  # Prepare data for plotting
  plotFrame <- multinomialResults[["descriptivesPlot"]][[options$countProp]]
  # We need to reverse the factor's levels because of the coord_flip later
  plotFrame$fact <- factor(plotFrame$fact, levels = rev(plotFrame$fact))

  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = fact, y = observed)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                      fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = plotFrame[["lowerCI"]],
                                        ymax = plotFrame[["upperCI"]]),
                           size = 0.75, width = 0.3) +
    base_breaks_y(plotFrame[["upperCI"]]) +
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


#' Multiomial Bayes - Read dataset
#'
#' @param dataset
#' @param options user input options
#'
#' @return dataset
.multinomialBayesReadData <- function(dataset, options) {

  # First, we load the variables into the R environment
  asnum <- NULL
  fact  <- NULL

  if (options$factor != "") {
    fact <- options$factor
    if (options$counts != "") {
      asnum <- options$counts
      if (options$exProbVar != "") {
        asnum <- c(asnum, options$exProbVar)
      }
    }
  }

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = asnum, columns.as.factor = fact,
                                 exclude.na.listwise = NULL)
  } else {
    dataset <- .vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact)
  }

  return(dataset)
}
