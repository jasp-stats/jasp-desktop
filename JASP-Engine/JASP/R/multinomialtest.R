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
  factor <- options$factor
  counts <- options$counts
  exProbVar <- options$exProbVar  
  
  if (is.null(dataset)) {
    if (perform == "run") {
      dataset <- .readDataSetToEnd(columns.as.numeric=c(counts, exProbVar),
                                   columns.as.factor=factor,
                                   exclude.na.listwise=NULL)
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric=NULL,
                                    columns.as.factor=factor)
    }
  } else {
    dataset <- .vdf(dataset, columns.as.numeric=c(counts, exProbVar),
                    columns.as.factor=factor)
  }
  
  results <- list() # Initialise results object
  
  
  # Then, we retrieve the state and initialise the output objects
  state <- retrieveState()
  
  chisqResults <- NULL # result of the chi-square test
  descriptivesTable <- NULL # expected versus observed
  descriptivesPlot <- NULL # barplot of factor levels
  
  
  # Then, we can fill the output objects with old info if its option did not 
  # change.
  if (!is.null(state)){
    diff <- .diff(options, state$options) # a list of TRUE/FALSE
    
    if (is.list(diff)){
      if !any(diff[["factor"]], diff[["confidenceIntervalInterval"]],
              diff[["hypothesis"]], diff[["counts"]], diff[["exProbVar"]],
              diff[["expectedProbs"]]){
                
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
  if (is.null(chisqResults)){
    chisqResults <- .chisquareTest(dataset, options, factor, perform)
  }
  
  results[["chisq"]] <- .chisqTable(chisqResults, options, perform)
    
  
  # Descriptives Table
  if (options[["descriptives"]]){
    # Generate descriptives table
    if (is.null(descriptivesTable)){
      descriptivesTable <- .multinomialDescriptives(dataset, options, perform)
    }
    
    results[["descriptivesTable"]] <- descriptivesTable
    
  } else {
    
    results[["descriptivesTable"]] <- NULL
    
  }
  
  
  # Multinomial Descriptives Plot
  if (options[["descriptivesPlot"]]){
    # Generate descriptives plots
    if (is.null(descriptivesPlot)){
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

  # first determine the hypotheses
  levels <- nlevels(factor)
  hyps <- list()
  if (options$multinomialtest){
    hyps[["H1"]] <- rep(1/levels, levels)
  } else {
    # assign each hypothesis to the dadkj
    for (h in options$hyptable){
      # TODO
    }
  }
  
  return()
}

# Transform chi-square test object into table for JASP
.chisqTable <- function(chisqResults, options, perform){
  # TODO
}

# Create multinomial descriptives table
.multinomialDescriptives <- function(dataset, options, perform){
  # TODO
}

.multinomialDescriptivesPlot <- function(dataset, options, perform){
  #TODO
}
