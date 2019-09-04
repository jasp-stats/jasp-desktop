#
# Copyright (C) 2019 University of Amsterdam
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


.ldGetFitContainer <- function(jaspResults, options, name, title, position, errors = FALSE){
  if(!is.null(jaspResults[[name]])){
    fitContainer <- jaspResults[[name]]
  } else{
    fitContainer <- createJaspContainer(title = title)
    fitContainer$position <- position
    
    fitContainer$dependOn(c("variable", "simulateNow"))
    
    jaspResults[[name]] <- fitContainer
  }

  if(!isFALSE(errors)){
    fitContainer$setError(errors$message)
  }
  
  return(fitContainer)
}

.ldEstimatesTable <- function(container, options, ci.possible, se.possible, method){
  if(!options$outputEstimates) return()
  if(!is.null(container[['estParametersTable']])) return()
  
  tab <- createJaspTable(title = "Estimated Parameters")
  tab$dependOn(c("outputEstimates", "outputSE", "ciInterval", "ciIntervalInterval", "parametrization", method))
  tab$position <- 1
  tab$showSpecifiedColumnsOnly <- TRUE
  tab$setExpectedSize(rows = length(options$pars) - length(options$fix.pars))
  
  tab$addColumnInfo(name = "parName", title = "Parameter", type = "string")
  tab$addColumnInfo(name = "estimate", title = "Estimate", type = "number")
  
  #"\u03BC\u0302"
  if(options$outputSE && se.possible){
    tab$addColumnInfo(name = "se", title = "SE", type = "number")
  } else if(options$outputSE) {
    tab$addFootnote("Standard errors are unavailable with this method")
  }
  
  if(options$ciInterval && ci.possible){
    tab$addColumnInfo(name = "lower", title = "Lower", type = "number",
                      overtitle = sprintf("%s%% CI", options[['ciIntervalInterval']]*100))
    tab$addColumnInfo(name = "upper", title = "Upper", type = "number",
                      overtitle = sprintf("%s%% CI", options[['ciIntervalInterval']]*100))
  } else if(options$ciInterval){
    tab$addFootnote("Confidence intervals are unavailable with this method.")
  }
  
  if(method == "methodMLE"){
    tab$addCitation(.ldAllTextsList$references$fitdistrplus)
    
    if(options$ciInterval || options$outputSE){
      tab$addCitation(.ldAllTextsList$references$car)
    }
    
    if(options$ciInterval && !options$outputSE){
      tab$addFootnote("Confidence intervals were calculated using the delta method.")
    } else if(!options$ciInterval && options$outputSE){
      tab$addFootnote("Standard errors were calculated using the delta method.")
    } else if(options$ciInterval && options$outputSE){
      tab$addFootnote("Standard errors and confidence intervals were calculated using the delta method.")
    }
  }
  
  
  container[['estParametersTable']] <- tab
  
  return(tab)
}

### MLE stuff ----
.ldMLEResults <- function(mleContainer, variable, options, ready, distName, structureFun){
  if(!ready) return()
  if(!is.null(mleContainer[['mleResults']])) return(mleContainer[['mleResults']]$object)
  
  starts <- options$pars
  if(!is.null(options$fix.pars)){
    starts[names(options$fix.pars)] <- NULL 
  }
  
  results <- list()
  results$fitdist <- try(fitdistrplus::fitdist(data = variable, distr = distName, method = "mle", 
                                               start = starts, fix.arg = options$fix.pars,
                                               keepdata = FALSE, optim.method = "L-BFGS-B",
                                               lower = options$lowerBound, upper = options$upperBound))
  
  if(inherits(results$fitdist, "try-error")){
    results$fitdist <- try(MASS::fitdistr(x = variable, densfun = options$pdfFun, start = starts, 
                                          lower = options$lowerBound, upper = options$upperBound))
  }
  
  if(inherits(results$fitdist, "try-error")){
    results$fitdist <- try(fitdistrplus::fitdist(data = variable, distr = distName, method = "mle", 
                                                 start = starts, fix.arg = options$fix.pars,
                                                 keepdata = FALSE))
  } else{
    results$fitdist$convergence <- 0
  }
  
  if(inherits(results$fitdist, "try-error")){
    mleContainer$setError("Estimation failed: try adjusting parameter values, check outliers, or feasibility of the distribution fitting the data.")
    return()
  } 
  
  results$structured <- structureFun(results$fitdist, options)
  
  mleContainer[['mleResults']] <- createJaspState(object = results, dependencies = c(options$parValNames, "ciIntervalInterval"))
  
  return(results)
}

