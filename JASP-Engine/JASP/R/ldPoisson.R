#
# Copyright (C) 2013-2020 University of Amsterdam
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

LDpoisson <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsPoisson(options)
  
  #### Show poisson section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Poisson distribution"), 
                      parSupportMoments = .ldPoissonParsSupportMoments,
                      formulaPMF        = .ldFormulaPoissonPMF, 
                      formulaCMF        = .ldFormulaPoissonCDF)
  
  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options)
  
  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready && is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
    
    variable <- dataset[[.v(options[['variable']])]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<2",
                         limits.min = options$support$min, limits.max = options$support$max, 
                         exitAnalysisIfErrors = FALSE)
    errors <- .ldCheckInteger(variable, errors)
  }
  
  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")
  
  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillPoissonEstimatesTable)
  
  return()
}

### options ----
.ldRecodeOptionsPoisson <- function(options){
  
  options[['parValNames']] <- c("lambda")
  
  options[['pars']]   <- list(lambda = options[['lambda']])
    
  options[['pdfFun']] <- stats::dpois
  options[['cdfFun']] <- stats::ppois
  options[['qFun']]   <- stats::qpois
  options[['rFun']]   <- stats::rpois
  options[['distNameInR']] <- "pois"
  
  options <- .ldOptionsDeterminePlotLimits(options, FALSE)
 
  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0)
  options$upperBound <- c(Inf)
  
  options$transformations <- c(lambda = "lambda")
  
  options
}

### text fill functions -----
.ldPoissonParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("rate: %s", "\u03BB \u2208 \u211D: \u03BB \u003E 0")
    
    support <- "x \u2208 {0, 1, 2, ...}"
    
    moments <- list()
    moments$expectation <- "\u03BB"
    moments$variance <- "\u03BB"
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaPoissonPMF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonQF <- function(options){
  text <- "<MATH>
    Q(x; <span style='color:red'>\u03BB</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillPoissonEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  res <- results$structured
  res$parName <- c("\u03BB")
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}