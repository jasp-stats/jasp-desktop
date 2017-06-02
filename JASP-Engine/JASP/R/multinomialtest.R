#
# Copyright (C) 2017 University of Amsterdam
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

MultinomialTest <- function(dataset = NULL, options, perform = "run",
               callback = function(...) 0,  ...) {
    
  # First, we load the variables into the R environment
  factor <- NULL
  asnum <- NULL
  if (options$factor != "") {
    factor <- options$factor
    if (options$counts != "") {
      asnum <- options$counts
      if (options$exProbVar != "") {
        asnum <- c(asnum, options$exProbVar)
      }
    }
  }

  
  
  if (is.null(dataset)) {
    if (perform == "run") {
      dataset <- .readDataSetToEnd(columns.as.numeric=asnum,
                                   columns.as.factor=factor,
                                   exclude.na.listwise=NULL)
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric=asnum,
                                    columns.as.factor=factor)
    }
  } else {
    dataset <- .vdf(dataset, columns.as.numeric=asnum,
                    columns.as.factor=factor)
  }
  
  results <- list() # Initialise results object
  
  
  # Then, we retrieve the state and initialise the output objects
  state <- .retrieveState()
  
  chisqResults <- NULL # result of the chi-square test
  descriptivesTable <- NULL # expected versus observed
  descriptivesPlot <- NULL # barplot of factor levels
  
  
  # Then, we can fill the output objects with old info if its option did not 
  # change.
  if (!is.null(state)) {
    diff <- .diff(options, state$options) # a list of TRUE/FALSE
    
    if (is.list(diff)){
      if (!any(diff[["factor"]], diff[["confidenceIntervalInterval"]],
              diff[["hypothesis"]], diff[["counts"]], diff[["exProbVar"]],
              diff[["expectedProbs"]])){
                
        chisqResults <- state[["chisqResults"]]
        
      }
      #... etcetera
      # TODO if (ahsdflkjah) descriptivesTable <- state[["descriptivesTable"]]
    }
    
  }
  
  
  # Meta information
  results[["title"]] <- "Multinomial Test"
  results[[".meta"]] <- list(list(name = "chisq", type = "table"),
                             list(name = "descriptivesTable", type = "table"),
                             list(name = "descriptivesPlot", type = "image"))
  
  
  # chi-square Table
  # Generate results
  if (is.null(chisqResults)) {
    chisqResults <- .chisquareTest(dataset, options, factor, perform)
  }
  
  results[["chisq"]] <- .chisqTable(chisqResults, options, perform)
    
  
  # Descriptives Table
  if (options[["descriptives"]]) {
    # Generate descriptives table
    if (is.null(descriptivesTable)) {
      descriptivesTable <- .multinomialDescriptives(dataset, options, factor, perform)
    }
    
    results[["descriptivesTable"]] <- descriptivesTable
    
  } else {
    
    results[["descriptivesTable"]] <- NULL
    
  }
  
  
  # Multinomial Descriptives Plot
  if (options[["descriptivesPlot"]]) {
    # Generate descriptives plots
    if (is.null(descriptivesPlot)) {
      descriptivesPlot <- .multinomialDescriptivesPlot(dataset, options, factor, perform)
    }
    
    plotPath <- list(descriptivesPlot$data) # for keep later
    
    results[["descriptivesPlot"]] <- descriptivesPlot
    
  } else {
    
    results[["descriptivesPlot"]] <- NULL
    plotPath <- list()
    
  }
  
  
  if (perform == "run") {

    state <- list()
    state[["options"]] <- options
    state[["chisqResults"]] <- chisqResults
    state[["descriptivesTable"]] <- descriptivesTable
    state[["descriptivesPlot"]] <- descriptivesPlot

    return(list(results=results, status="complete", state=state,
                keep = plotPath))

  } else {

    return(list(results=results, status="inited", state=state,
                keep = plotPath))

  }

  
  
}

# Run chi-square test and return object
.chisquareTest <- function(dataset, options, factor, perform){
  
  chisqResults <- NULL
  
  if (perform == "run" && !is.null(factor)) {
    # first determine the hypotheses
    f <- dataset[[.v(factor)]]
    f <- f[!is.na(f)]
    nlev <- nlevels(f)
    val <- table(f)
    hyps <- .multinomialHypotheses(options, nlev)

    # create a named list with as values the chi-square result objects
    
    
    chisqResults <- lapply(hyps, function(h) {
      # catch warning message and append to object if necessary
      csr <- NULL
      warn <- NULL
      csr <- withCallingHandlers(
        chisq.test(x = val, p = h, rescale.p = TRUE),
        warning = function(w){
         warn <<- w$message
        }
      )
      csr[["warn"]] <- warn
      return(csr)
    })
  }
  
  # return the out object
  return(chisqResults)
}

# Transform chi-square test object into table for JASP
# chisqResults = list(H1 = obj, H2 = obj, ....)
.chisqTable <- function(chisqResults, options, perform) {
  # TODO
}

# Create multinomial descriptives table
.multinomialDescriptives <- function(dataset, options, factor, perform) {
  # Expected vs. Observed table
  table <- list()
  table[["title"]] <- "Descriptives table"
  
  countProp = "descCounts"
  
  if (countProp=="descCounts"){
    numbertype = list(type="integer")
  } else {
    numbertype = list(type="number", format="sf:4;dp:3")
  }
  
  
  if (is.null(factor)){
    # If we have no variable init table with generic name
    
    fields <- list(
      list(name="factor", title="Factor", type = "string"),
      list(name="observed", title="Observed", numbertype),
      list(name="expected", title="Expected". numbertype)
    )
    rows <- list(factor = ".", observed = ".", expected = ".")
  
  } else if (perform != "run") {
    # If we have a variable init table with factor name
    
    fields <- list(
      list(name="factor", title=factor, type = "string"),
      list(name="observed", title="Observed", numbertype),
      list(name="expected", title="Expected". numbertype)
    )
    rows <- list(factor = ".", observed = ".", expected = ".")
    
    
  } else {
    
    # now perform is run and we want to create the full table
    
    fields <- list(
      list(name="factor", title=factor, type = "string"),
      list(name="observed", title="Observed", numbertype),
      list(name="expected", title="Expected". numbertype)
    )
    
    f <- dataset[[.v(factor)]]
    f <- f[!is.na(f)]
    nlev <- nlevels(f)
    val <- table(f)  
    hyps <- .multinomialHypotheses(options, nlev)
    
  }
  
  table[["schema"]] <- fields
  table[["data"]] <- rows
  
  return(table)
  
  
  #if (!is.null(factor) && perform == "run"){
  if (FALSE){
    
    
    if (length(hyps) == 1) {
      fields[[length(fields)+1]] <- list(name=names(hyps), 
                                         title = paste0("Expected: ", names(hyps)),
                                         type = "integer")
    } else {
      for (i in 1:length(hyps)) {
        n <- names(hyps)[i]
        fields[[length(fields)+1]] <- list(name=n, title=n, type="integer", 
                                           overTitle = "Expected")
      }
    }
    table[["schema"]] <- fields
    #TODO
    
  } else {
    
    fields[[length(fields)+1]] <- list(name="expected", 
                                       title = "Expected",
                                       type = "integer")
    table[["schema"]] <- fields
    row <- list()
    if (is.null(factor)){
      factor <- ""
    }
    row[[1]] <- list(level=factor, observed=".", expected=".")
    
    
    table[["data"]] <- row
  }
  
  
  
}

.multinomialDescriptivesPlot <- function(dataset, options, perform) {
  # TODO
}

.multinomialHypotheses <- function(options, nlevels){
  # This function transforms the input into a list of hypotheses
  hyps <- list()
  if (options$hypothesis == "multinomialTest"){
    hyps[["Multinomial"]] <- rep(1/nlevels, nlevels)
  } else {
    # assign each hypothesis to the hyps object
    for (i in 1:length(options$hyptable)) {
      n <- colnames(options$hyptable)[i]
      hyps[[n]] <- options$hyptable[,i]
    }
  }
  return(hyps)
}
