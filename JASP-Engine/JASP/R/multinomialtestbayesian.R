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

MultinomialTestBayesian <- function(jaspResults, dataset, options, state = NULL){

  # Set title
  jaspResults$title <- "Bayesian Multinomial Test"

  # Read dataset
  dataset <- .multinomialBayesReadData(dataset, options)

  # Error checking
  .multinomialBayesCheckErrors(dataset, options)

  # Compute Results for Tables
  multinomialResults <- .computeMultinomialResults(jaspResults, dataset, options) # main table and descriptives info

  # Output tables and plots
  .createMultBayesTable(jaspResults, options, multinomialResults) # main table
  descriptivesTable <- .createMultBayesDescriptivesTable(jaspResults, options, multinomialResults)
  descriptivesPlot  <- .createMultBayesDescriptivesPlot(jaspResults, options, multinomialResults)

  return()

}

# Results functions
.multBayesCalcSpecs <- function(dataset, options){
  specs <- list()

  # Default options
  specs$defaultOptions <- c("tableWidget", "priorCounts","factor", "counts", "exProbVar", "hypothesis",
                            "credibleIntervalInterval")

  # Ready statement
  ready <- options$factor != "" && !is.null(dataset)
  specs$ready <- ready

  # Specify factor variable  
  specs$factorVariable <- NULL
  specs$countVariable  <- NULL
  if (options$factor != "") {
    specs$factorVariable <- options$factor
    # Specify count variable
    if (options$counts != "") {
      countVariable <- options$counts
      if (options$exProbVar != "") {
        countVariable <- c(countVariable, options$exProbVar)
      }
      specs$countVariable <- countVariable
    }
  } 
  return(specs)
}
.computeMultinomialResults <- function(jaspResults, dataset, options) {

  # Take results from state if possible
  if (!is.null(jaspResults[["stateMultBayesResults"]])) return(jaspResults[["stateMultBayesResults"]]$object)

  # This will be the object that we fill with results
  multinomialResults <- list(mainTable         = list(), 
                             descriptivesTable = list(), 
                             descriptivesPlot  = list())

  # First, we perform a precalculation of variables we use throughout the analysis
  multinomialResults$specs <- .multBayesCalcSpecs(dataset, options)

  # Prepare for running the Bayesian Multinomial test
  factorVariable <- multinomialResults$specs$factorVariable
  countVariable  <- multinomialResults$specs$countVariable
  prior          <- options$priorCounts
  fact           <- dataset[[.v(factorVariable)]]
  fact           <- fact[!is.na(fact)]
  a              <- setNames(prior$values, prior$levels)
  # If applicable, convert counts variable to factor
  if (!is.null(countVariable)) {
    c <- dataset[[.v(countVariable)]]
    if (length(c) != length(levels(fact))) {
      .quitAnalysis("Invalid counts: the number of counts does not equal the number of categories. 
                    Check your count dataset!")
    }
    fact <- factor(rep(fact, c), levels = levels(fact))
  }
  # Determine observed counts and factor levels
  t    <- table(fact)
  N    <- sum(t)
  nlev <- nlevels(fact)

  # Extract hypotheses
  hyps  <- .multinomialHypotheses(dataset, options, nlev) # exact equality constraints
  nms   <- names(hyps)
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
  multinomialResults$descriptivesPlot[["descProps"]]  <- .multMedianAndCIs(t, a, options$descriptivesPlotCredibleInterval, TRUE)
  multinomialResults$descriptivesPlot[["descCounts"]] <- multinomialResults$descriptivesPlot[["descProps"]] * N
  multinomialResults$descriptivesPlot[["descProps"]][["fact"]]  <- levels(fact)
  multinomialResults$descriptivesPlot[["descCounts"]][["fact"]] <- levels(fact)

  # Results for descriptives table
  multinomialResults$descriptivesTable[["descProps"]][["fact"]]     <- levels(fact)
  multinomialResults$descriptivesTable[["descCounts"]][["fact"]]    <- levels(fact)
  multinomialResults$descriptivesTable[["descProps"]][["observed"]]  <- as.numeric(t)/N
  multinomialResults$descriptivesTable[["descCounts"]][["observed"]] <- as.numeric(t)
  for(h in 1:nhyps){
    multinomialResults$descriptivesTable[["descProps"]][[nms[h]]]    <- multinomialResults$mainTable[[nms[h]]]$expected/N
  }
    for(h in 1:nhyps){
    multinomialResults$descriptivesTable[["descCounts"]][[nms[h]]]   <- multinomialResults$mainTable[[nms[h]]]$expected
    }  
  multinomialResults$descriptivesTable[["descProps"]]    <- as.data.frame(multinomialResults$descriptivesTable[["descProps"]])
  multinomialResults$descriptivesTable[["descCounts"]]   <- as.data.frame(multinomialResults$descriptivesTable[["descCounts"]])
  multinomialResults$descriptivesTable[["descPropsCI"]]  <- .multMedianAndCIs(t, a, options$credibleIntervalInterval)
  multinomialResults$descriptivesTable[["descCountsCI"]] <- .multMedianAndCIs(t, a, options$credibleIntervalInterval) * N
  
  # Save results to state
  defaultOptions <- multinomialResults$specs$defaultOptions
  jaspResults[["stateMultinomialBayesianResults"]] <- createJaspState(multinomialResults)
  jaspResults[["stateMultinomialBayesianResults"]]$dependOnOptions(defaultOptions)
  
  return(multinomialResults)  # return the out object
}
.createMultBayesTable <- function(jaspResults, options, multinomialResults){
  if (!is.null(jaspResults[["multinomialTable"]])) return()
  
  # Create table
  defaultOptions                    <- multinomialResults$specs$defaultOptions
  multinomialTable                  <- createJaspTable(title = "Bayesian Multinomial Test")
  jaspResults[["multinomialTable"]] <- multinomialTable
  multinomialTable$dependOnOptions(c(defaultOptions, "bayesFactorType"))

  # Bayes factor type
  if (options$bayesFactorType == "BF01") {
    bf.title <- "BF\u2080\u2081"
  } else if (options$bayesFactorType == "BF10") {
    bf.title <- "BF\u2081\u2080"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(\u0042\u0046\u2081\u2080)"
  }

  # Add columns to the table
  multinomialTable$addColumnInfo(name = "case",    title = "",       type = "string", combine = TRUE)
  multinomialTable$addColumnInfo(name = "level",   title = "Levels", type = "integer")
  multinomialTable$addColumnInfo(name = "BF",      title = bf.title, type = "number", format="sf:4;dp:3")
  
  # Add rows
  fact  <- multinomialResults[["specs"]][["factorVariable"]]
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["mainTable"]][["hypNames"]]
  for (h in 1:nhyps) {
    row <- data.frame(case  = nms[h],
                      level = multinomialResults[["mainTable"]][["nlevels"]],
                      BF    = multinomialResults[["mainTable"]][[nms[h]]][["BF"]][[options$bayesFactorType]]
    )
    multinomialTable$addRows(row, rowNames =  nms[h])
  }

  # Add footnotes
  for (h in 1:nhyps) {
    if (!is.null(multinomialResults[["mainTable"]][[nms[h]]][["warn"]])) {
      multinomialResults$addFootnote(message   = multinomialResults[["mainTable"]][[nms[h]]][["warn"]],
                                     col_names = bf.title,
                                     row_names = nms[h],
                                     symbol    = "<em>Note.</em>")
    }
  }
}
.createMultBayesDescriptivesTable <- function(jaspResults, options, multinomialResults){
  if (!is.null(jaspResults[["multinomialDescriptivesTable"]])) return()

  # Create table
  factorVariable                                <- multinomialResults[["specs"]][["factorVariable"]]
  descriptivesTable                             <- createJaspTable(title = "Descriptives")
  jaspResults[["multinomialDescriptivesTable"]] <- descriptivesTable
  descriptivesTable$dependOnOptions(c("countProp", "descriptives", "credibleIntervalInterval"))

  descriptivesTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to the table  
  nhyps <- multinomialResults[["mainTable"]][["nhyps"]]
  nms   <- multinomialResults[["mainTable"]][["hypNames"]]

  descriptivesTable$addColumnInfo(name = "fact",     title = factorVariable, type = "string", combine = TRUE)
  descriptivesTable$addColumnInfo(name = "observed", title = "Observed", type = "integer")

  if(nhyps > 1){
    for(h in 1:nhyps){
      descriptivesTable$addColumnInfo(name = nms[h], title = nms[h], type = "integer", overtitle = "Expected")
      }
  } else {
    descriptivesTable$addColumnInfo(name = nms, title = paste0("Expected: ", nms), type = "integer")
  }

  if (options$credibleInterval) {
    descriptivesTable$addColumnInfo(name = "lowerCI", title = "Lower", type = "number", format = "sf:4",
                                overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
    descriptivesTable$addColumnInfo(name = "upperCI", title = "Upper", type = "number", format = "sf:4",
                                overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
  }

  # Add rows
  descRow <- multinomialResults[["descriptivesTable"]][[options$countProp]]

  if(options$credibleInterval){
    ciInfo  <- multinomialResults[["descriptivesTable"]][[paste0(options$countProp, "CI")]]
    descRow <- cbind(descRow, ciInfo)
    descriptivesTable$addFootnote(message = "Credible intervals are based on marginal beta distributions.",
                                  symbol = "<em>Note.</em>")
  }
  descriptivesTable$addRows(descRow, rowNames = row.names(descRow))
  
}
.createMultBayesDescriptivesPlot <- function(jaspResults, options, multinomialResults){
  if (!options$descriptivesPlot) return()
browser()
  # Create container for plot
  if (is.null(jaspResults[["descriptivesPlot"]])) {
    jaspResults[["descriptivesPlot"]] <- createJaspContainer("Descriptives Plots")
    jaspResults[["descriptivesPlot"]]$dependOnOptions("descriptivesPlots")
  }
  
  # Create pointer towards main container, created before
  pct <- jaspResults[["descriptivesPlot"]]
    
  # If the plot for this variable already exists, we can skip recalculating the plots
  factorVariable <- multinomialResults[["specs"]][["factorVariable"]]
  
  # If the plot for this variable already exists, we can skip recalculating the plots
  if (!is.null(pct[[factorVariable]])) next
  
  pct[[factorVariable]] <- createJaspContainer(factorVariable)

  descriptivesPlot <- .multBayesPlotHelper(factorVariable, options, multinomialResults)
  pct[[factorVariable]][["multPlot"]] <- createJaspPlot(plot = descriptivesPlot, title = factorVariable, width = 160, height = 320)
  pct[[factorVariable]][["multPlot"]]$dependOnOptions("descriptivesPlots")

}
# Helper functions 
.multMedianAndCIs <- function(counts, alphas, credibleInterval, computeMedian = FALSE){
  
  N <- sum(counts)
  
  lower <- (1 - credibleInterval)/2
  upper <- 1 - lower
  
  # compute median and credible intervals
  if(computeMedian == TRUE){
    medianCI           <- matrix(NA, ncol = 3, nrow = length(counts))
    colnames(medianCI) <- c('lowerCI', 'upperCI', 'median')
    for(i in seq_along(counts)){
      k <- counts[i]
      a <- alphas[i]
      b <- sum(alphas[-i])
      medianCI[i, ] <- qbeta(c(lower, upper, 0.5), a + k, b + N - k)
    }
  } else {
    medianCI           <- matrix(NA, ncol = 2, nrow = length(counts))
    colnames(medianCI) <- c('lowerCI', 'upperCI')
    for(i in seq_along(counts)){
      k <- counts[i]
      a <- alphas[i]
      b <- sum(alphas[-i])
      medianCI[i, ] <- qbeta(c(lower, upper), a + k, b + N - k)
    }
  }
  return(as.data.frame(medianCI))
}
.multBayesPlotHelper <- function(factorVariable, options, multinomialResults){
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
    yname <- "Posterior parameter estimates"
  } else {
    yname <- "Posterior parameter estimates"
  }

  # Prepare data for plotting
  plotFrame <- multinomialResults[["descriptivesPlot"]][[options$countProp]]

  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = fact, y = median)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                      fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = plotFrame[["lowerCI"]],
                                        ymax = plotFrame[["upperCI"]]),
                           size = 0.75, width = 0.3) +
    base_breaks_y(plotFrame[["upperCI"]]) +
    ggplot2::xlab(factorVariable) +
    ggplot2::ylab(yname) +
    ggplot2::coord_flip() +
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

return(p)
}

# Functions for Analysis
.multBayesBfEquality      <- function(alphas, counts, thetas){
  warn <- NULL
  if(sum(thetas) != 1){
    thetas <- thetas/sum(thetas)
    warn   <- "Parameters have been rescaled."
  }
  
  # expected counts under the null hypothesis (Binomial median)
  expected <- setNames(sum(counts)*thetas, names(counts))
  
  # compute Bayes factor
  lbeta.xa <- sum(lgamma(alphas + counts)) - lgamma(sum(alphas + counts))
  lbeta.a  <- sum(lgamma(alphas)) - lgamma(sum(alphas))
  
  if (any(rowSums(cbind(thetas, counts)) == 0)) {
    
    # in this case, counts*log(thetas) should be zero, omit to avoid numerical issue with log(0)
    
    LogBF10 <- (lbeta.xa-lbeta.a)
    
  } else {
    
    LogBF10 <- (lbeta.xa-lbeta.a) + (0 - sum(counts * log(thetas)))
    
  }
  
  BF <- data.frame(LogBF10 = LogBF10,
                   BF10    = exp(LogBF10),
                   BF01    = 1/exp(LogBF10))
  
  return(list(BF       = BF,
              warn     = warn,
              expected = expected))
  
}

# Init functions ----
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
.multinomialBayesCheckErrors <- function(dataset, options) {
  fact  <- NULL
  if (options$factor != "") {
    fact <- options$factor
  }
  
  # Error Check 1: Number of levels of the variables must be bigger than 1
  .hasErrors(dataset              = dataset, 
             perform              = "run", 
             type                 = 'factorLevels', 
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
               type                 = 'observations', 
               observations.amount  = c('< 1'),  
               exitAnalysisIfErrors = TRUE)
  }
}

