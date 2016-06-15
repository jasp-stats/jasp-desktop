#
# Copyright (C) 2015 University of Amsterdam
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

BinomialTest <- function(dataset = NULL, options, perform = "run",
						   callback = function(...) 0,  ...) {

	variables <- unlist(options$variables)

	if (is.null(dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.numeric=NULL, 
			                             columns.as.factor=variables, 
			                             exclude.na.listwise=NULL)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=NULL, 
			                              columns.as.factor=variables)
		}
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=NULL, 
		                columns.as.factor=variables)
	}
	
	
	## Retrieve State
	
	state <- .retrieveState()
	
	binomResults <- NULL
	
	if (!is.null(state)) {  # is there state?
	  
	  diff <- .diff(options, state$options)  # compare old and new options
	  
	  if (is.list(diff) && any(diff) == TRUE) {
	    
	    binomResults <- state$binomResults
	    
	  }
	}
	
	
	results <- list()
	
	results[["title"]] <- "Binomial Test"
	results[[".meta"]] <- list(list(name="binomial", type="table"))
	
	if (is.null(binomResults)){
	  binomResults <- .binomialTest(dataset, options, variables, perform)
	}
	
	results[["binomial"]] <- .binomialTable(binomResults, options, variables, 
	                                        perform)
	

	
	# Save state
	state[["options"]] <- options
	state[["binomResults"]] <- binomResults
	
	
	if (perform == "init") {
	  
	  return(list(results=results, status="inited", state=state))
	  
	} else {
	  
	  return(list(results=results, status="complete", state=state))	
	  
	}
	
}
	
.binomialTable <- function(r, options, variables, perform){
  
  table <- list()
  table[["title"]] <- "Binomial Test"
  
  fields <- list(
    list(name="case", title="", type="string", combine=TRUE),
    list(name="level", title="Level", type="string"),
    list(name="counts", title="Counts", type="integer"),
    list(name="total", title="Total", type="integer"),
    list(name="proportion", title="Proportion", type="number", 
         format="sf:4;dp:3"),
    list(name="p", title="p", type="number", format="dp:3;p:.001")
  )
  
  if (options$confidenceInterval) {
    
    interval <- 100 * options$confidenceIntervalInterval
    title <- paste0(interval, "% Confidence Interval")
    
    fields[[length(fields)+1]] <- list(name = "lowerCI",
                                       type = "number",
                                       format = "sf:4;dp:3", title = "Lower",
                                       overTitle = title)
    fields[[length(fields)+1]] <- list(name = "upperCI", 
                                       type = "number",
                                       format = "sf:4;dp:3", title = "Upper",
                                       overTitle = title)
  }
  
  table[["schema"]] <- list(fields = fields)
  
  
  footnotes <- .newFootnotes()
  
  if (options$hypothesis == "notEqualToTestValue") {

    message <- paste0("Proportions tested against value: ", options$testValue, 
                      ".")
    
  } else if (options$hypothesis == "greaterThanTestValue") {

    note <- "For all tests, the alternative hypothesis specifies that the 
    proportion	is greater than "
    message <- paste0(note, options$testValue, ".")
    
  } else {

    note <- "For all tests, the alternative hypothesis specifies that the
    proportion is less than "
    message <- paste0(note, options$testValue, ".")
    
  }
  
  .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
  table[["footnotes"]] <- as.list(footnotes)
  
  if (!is.null(r)){
    
    table[["data"]] <- r
    table[["status"]] <- "complete"
    
  } else {
    
    data <- list()
    
    if (is.null(variables)){
      variables <- ""
    } 
    
    for (var in variables){
      data[[length(data) + 1]] <- list(case=var, level=".", counts=".", 
                                       total=".",  proportion=".", p=".",
                                       lowerCI=".", upperCI=".")
    }
    
    table[["data"]] <- data
    
  }
  
  return(table)
}




.binomialTest <- function(dataset, options, variables, perform){
  if (perform == "run" && !is.null(variables)) {
    
    
    if (options$hypothesis == "notEqualToTestValue") {
      
      hyp <- "two.sided"
      
    } else if (options$hypothesis == "greaterThanTestValue") {
      
      hyp <- "greater"
      
    } else {
      
      hyp <- "less"
      
    }
    
    data <- list()
    
    for (var in variables) {
      
      d <- dataset[[.v(var)]]
      d <- d[!is.na(d)]
      
      levels <- levels(d)
      n <- length(d)
      
      # !! Test each level in each variable against test value !!
      for (lev in levels) {
        
        counts <- sum(d == lev)
        prop <- counts/n
        
        r <- stats::binom.test(counts, n, p = options$testValue, 
                               alternative = hyp, 
                               conf.level = options$confidenceIntervalInterval)
        
        p <- r$p.value
        cilo <- r$conf.int[1]
        ciup <- r$conf.int[2]
        
        if (p == FALSE) {
          p <- 0
        } else if (p == TRUE) {
          p <- 1
        }
        
        row <- list(case = var, level = lev, counts = .clean(counts), 
                    total = .clean(n), proportion = .clean(prop), p = .clean(p), 
                    lowerCI = .clean(cilo), upperCI = .clean(ciup))
        
        if (lev == levels[1]) {
          row[[".isNewGroup"]] <- TRUE
        } else {
          row[[".isNewGroup"]] <- FALSE
        }
        
        data[[length(data)+1]] <- row
        
      }
      
    }
    
    binomResults <- data
    
  } else {
    
    binomResults <- NULL
      
  }
  
  return(binomResults)
      
}



