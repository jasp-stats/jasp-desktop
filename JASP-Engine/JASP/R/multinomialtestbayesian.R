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
  .createMultBayesTable(jaspResults, options, multinomialResults) # results table
  .createMultBayesDescriptivesTable(jaspResults, options, multinomialResults)
  #     descriptivesTable <- .createMultBayesDescriptivesTable(jaspResults = jaspResults, options = options, descriptivesTable = descriptivesTable)
  #   .createMultBayesDescriptivesPlot(jaspResults = jaspResults, options = options, descriptivesPlot = descriptivesPlot)
  #   descriptivesPlot <- .computeMultBayesDescriptivesTableResultsPlot(jaspResults = jaspResults, options = options, priorPosteriorPlot = priorPosteriorPlot)

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
  multinomialResults$descriptivesPlot[["descProps"]][["Level"]]  <- levels(fact)
  multinomialResults$descriptivesPlot[["descCounts"]][["Level"]] <- levels(fact)

  # Results for descriptives table
  multinomialResults$descriptivesTable[["descProps"]][["Level"]]     <- levels(fact)
  multinomialResults$descriptivesTable[["descCounts"]][["Level"]]    <- levels(fact)
  multinomialResults$descriptivesTable[["descProps"]][["Observed"]]  <- as.numeric(t)/N
  multinomialResults$descriptivesTable[["descCounts"]][["Observed"]] <- as.numeric(t)
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
      descriptivesTable$addColumnInfo(name = nms[h], title = nms[h], type = "number", format = "sf:4", 
                                      overtitle = "Expected") 
    }
  } else {
    descriptivesTable$addColumnInfo(name = "expected", title = paste0("Expected: ", nms), type = "number", format = "sf:4")
  }

  if (options$credibleInterval) {
    descriptivesTable$addColumnInfo(name = "lowerCI", title = "Lower", type = "number", format = "sf:4",
                                overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
    descriptivesTable$addColumnInfo(name = "upperCI", title = "Upper", type = "number", format = "sf:4",
                                overtitle = paste0(100 * options$credibleIntervalInterval, "% Credible Interval"))
  }

  # Add rows
  row <- multinomialResults[["descriptivesTable"]][[options$countProp]]
  if(options$credibleInterval){
    ciInfo <- multinomialResults[["descriptivesTable"]][[paste0(options$countProp, "CI")]]
    row    <- cbind(row, ciInfo)
    descriptivesTable$addFootnote(message = "Credible intervals are based on marginal beta distributions.",
                                  symbol = "<em>Note.</em>")

  }
  descriptivesTable$addRows(row, rowNames = row.names(row))
  
}

# Functions for JASP output: Descriptives
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

# Functions for Analysis
.multBayesBfEquality      <- function(alphas, counts, thetas){
  # Function calculates the Bayes factor for the Bayesian multinomial test
  # alphas: vector with alpha parameters (To-Do)
  # counts: vector of observed data
  # thetas: vector of test values
  #
  # output: list consisting of
  #         - Bayes factor in favor of alternative hypothesis
  #         - warning message if parameters were rescaled
  #         - expected counts
  
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

# # Old functions
# MultinomialTestBayesian <- function (dataset = NULL, options, perform = "run",
#                                      callback = function(...) 0,  ...) {
#   
#   # First, we load the variables into the R environment
#   factorVariable<- NULL
#   asnum <- NULL
#   if (options$factor != "") {
#     factorVariable<- options$factor
#     if (options$counts != "") {
#       asnum <- options$counts
#       if (options$exProbVar != "") {
#         asnum <- c(asnum, options$exProbVar)
#       }
#     }
#   }
#   
#   if (is.null(dataset)) {
#     if (perform == "run") {
#       dataset <- .readDataSetToEnd(columns.as.numeric = asnum, columns.as.factor = fact,
#                                    exclude.na.listwise = NULL)
#     } else {
#       dataset <- .readDataSetHeader(columns.as.numeric = asnum, columns.as.factor = fact)
#     }
#   } else {
#     dataset <- .vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact)
#   }
#   
#   results <- list() # Initialise results object
#   
#   # Then, we retrieve the state and initialise the output objects
#   state <- .retrieveState()
#   
#   multinomialResults <- NULL # result of the chi-square test
#   descriptivesTable <- NULL # expected versus observed
#   descriptivesPlot <- NULL # barplot of factor levels
#   
#   # Then, we can fill the output objects with old info if its option did not
#   # change.
#   if (!is.null(state)) {
#     diff <- .diff(options, state$options) # a list of TRUE/FALSE
#     
#     if (is.list(diff)){
#       
#       if (!any(diff[["factor"]], diff[["counts"]],
#                diff[["credibleIntervalInterval"]],
#                diff[["bayesFactorType"]],
#                diff[["hypothesis"]], diff[["exProbVar"]],
#                diff[["expectedProbs"]], diff[["simulatepval"]],
#                (options[["hypothesis"]] == "expectedProbs" &&
#                 diff[["tableWidget"]])
#       )) {
#         
#         multinomialResults <- state[["multinomialResults"]]
#         
#         # the following depend on multinomialResults so in same if-statement
#         if (!any(diff[["credibleInterval"]],
#                  diff[["countProp"]])) {
#           descriptivesTable <- state[["descriptivesTable"]]
#         }
#         
#         if (!any(diff[["descriptivesPlotCredibleInterval"]],
#                  diff[["countProp"]], diff[["plotWidth"]],
#                  diff[["plotHeight"]])) {
#           descriptivesPlot <- state[["descriptivesPlot"]]
#         }
#       }
#       
#       #... etcetera
#       # TODO
#     }
#   }
#   
#   # Meta information
#   results[["title"]] <- "Bayesian Multinomial Test"
#   results[[".meta"]] <- list(list(name = "chisq", type = "table"),
#                              list(name = "descriptivesTable", type = "table"),
#                              list(name = "descriptivesPlot", type = "image"))
#   
#   # chi-square Table
#   # Generate results
#   if (is.null(multinomialResults)) {
#     multinomialResults <- .computeMultBayesTableResults(dataset, options, fact, perform)
#   }
#   
#   results[["chisq"]] <- .multinomialBayesTable(multinomialResults, options, perform)
#   
#   # Descriptives Table
#   if (options[["descriptives"]]) {
#     # Generate descriptives table
#     if (is.null(descriptivesTable)) {
#       descriptivesTable <- .computeMultBayesDescriptivesTableResults(multinomialResults, fact, options, perform)
#     }
#     
#     results[["descriptivesTable"]] <- descriptivesTable
#   } else {
#     
#     results[["descriptivesTable"]] <- NULL
#   }
#   
#   # Bayesian Multinomial Descriptives Plot
#   if (options[["descriptivesPlot"]]) {
#     # Generate descriptives plots
#     if (is.null(descriptivesPlot)) {
#       descriptivesPlot <- .computeMultBayesDescriptivesTableResultsPlot(multinomialResults, options, perform)
#     }
#     
#     plotPath <- list(descriptivesPlot$data) # for keep later
#     
#     results[["descriptivesPlot"]] <- descriptivesPlot
#   } else {
#     
#     results[["descriptivesPlot"]] <- NULL
#     plotPath <- list()
#   }
#   
#   if (perform == "run") {
#     
#     state <- list()
#     state[["options"]] <- options
#     state[["multinomialResults"]] <- multinomialResults
#     state[["descriptivesTable"]] <- descriptivesTable
#     state[["descriptivesPlot"]] <- descriptivesPlot
#     
#     return(list(results=results, status="complete", state=state,
#                 keep = plotPath))
#   } else {
#     return(list(results=results, status="inited", state=state,
#                 keep = plotPath))
#   }
# }
# .multinomialBayesTable <- function(multinomialResults, options) {
#   # Transform chi-square test object into table for JASP
#   # multinomialResults = list(H1 = obj, H2 = obj, ....)
#   #
#   # Args:
#   #   multinomialResults:
#   #   options: input options
#   #   perform: init or run
#   #
#   # Return:
#   #   Chi square table
#   table <- list()
#   footnotes <- .newFootnotes()
#   table[["title"]] <- "Bayesian Multinomial Test"
#   
#   # Bayes factor type
#   if (options$bayesFactorType == "BF01") {
#     bf.title <- "BF\u2080\u2081"
#   } else if (options$bayesFactorType == "BF10") {
#     bf.title <- "BF\u2081\u2080"
#   } else if (options$bayesFactorType == "LogBF10") {
#     bf.title <- "Log(\u0042\u0046\u2081\u2080)"
#   }
#   
#   # include fields
#   fields <- list(
#     list(name="case", title="", type="string", combine=TRUE),
#     list(name="levels", title="Levels", type="integer"),
#     list(name="BF", title = bf.title, type="number", format="sf:4;dp:3")
#   )
#   
#   # include footnotes
#   table[["schema"]] <- list(fields = fields)
#   
#   message <- list()
#   
#   for (r in 1:length(multinomialResults)) {
#     
#     if (!is.null(multinomialResults[[r]][["warn"]])) {
#       .addFootnote(footnotes, symbol = "<em>Note.</em>", text = multinomialResults[[r]][["warn"]])
#     }
#   }
#   
#   table[["footnotes"]] <- as.list(footnotes)
#   
#   # fill in results one row at a time
#   if (!is.null(multinomialResults)) {
#     
#     for (r in 1:length(multinomialResults)) {
#       table[["data"]][[r]] <- list(case = names(multinomialResults)[r],
#                                    levels = multinomialResults[[r]][["levels"]],
#                                    BF = multinomialResults[[r]][["BF"]])
#       table[["status"]] <- "complete"
#     }
#   } else {
#     # init state?
#     data <- list(list(case = ".", levels = ".", BF = ".",
#                       lowerCI = ".", upperCI = "."))
#     
#     table[["data"]] <- data
#   }
#   
#   return(table)
# }