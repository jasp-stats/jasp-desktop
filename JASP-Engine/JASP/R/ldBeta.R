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

LDbeta <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsBeta(options)
  
  #### Show beta section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("beta distribution"), 
                      parSupportMoments = .ldBetaParsSupportMoments,
                      formulaPDF        = .ldFormulaBetaPDF, 
                      formulaCDF        = .ldFormulaBetaCDF, 
                      formulaQF         = .ldFormulaBetaQF)
  
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
  }
  
  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "continuous")
  
  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillBetaEstimatesTable)
  
  return()
}

### options ----
.ldRecodeOptionsBeta <- function(options){
  options[['parValNames']] <- c("alpha", "beta")
  
  options[['pars']]   <- list(shape1 = options[['alpha']], shape2 = options[['beta']])
  options[['pdfFun']] <- stats::dbeta
  options[['cdfFun']] <- stats::pbeta
  options[['qFun']]   <- stats::qbeta
  options[['rFun']]   <- stats::rbeta
  options[['distNameInR']] <- "beta"
  
  options <- .ldOptionsDeterminePlotLimits(options)
  
  options$support <- list(min = 0, max = 1)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)
  
  options$transformations <- c(alpha = "shape1", beta = "shape2")
  
  options
}

### text fill functions -----

.ldBetaParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")
    
    support <- "x \u2208 [0, 1]"
    
    moments <- list()
    moments$expectation <- "&alpha; (&alpha; + &beta;)<sup>-1</sup>"
    moments$variance    <- "&alpha;&beta; (&alpha; + &beta;)<sup>-2</sup> (&alpha; + &beta; + 1)<sup>-1</sup>"
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaBetaPDF <- function(options){
  text <- "<MATH>
  f(x; <span style='color:red'>&alpha;</span>, <span style='color:blue'>&beta;</span>) = 
  </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBetaCDF <- function(options){
  text <- "<MATH>
  F(x; <span style='color:red'>&alpha;</span>, <span style='color:blue'>&beta;</span>) = 
  </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBetaQF <- function(options){
  text <- "<MATH>
  Q(p; <span style='color:red'>&alpha;</span>, <span style='color:blue'>&beta;</span>) = 
  </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillBetaEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  pars <- c(alpha = "\u03B1", beta = "\u03B2")

  res <- results$structured
  res <- res[res$par %in% names(pars),]
  res$parName <- c(pars)
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}
