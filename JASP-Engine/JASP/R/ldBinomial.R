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

LDbinomial <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsBinomial(options)
  
  #### Show binomial section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("binomial distribution"), 
                      parSupportMoments = .ldBinomialParsSupportMoments,
                      formulaPMF        = .ldFormulaBinomialPMF, 
                      formulaCMF        = .ldFormulaBinomialCDF)
  
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
                         observations.amount = "<1",
                         limits.min = options$support$min, limits.max = options$support$max, 
                         exitAnalysisIfErrors = FALSE)
    errors <- .ldCheckInteger(variable, errors)
  }
  
  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")
  
  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillBinomialEstimatesTable)
  
  return()
}

### options ----
.ldRecodeOptionsBinomial <- function(options){
  
  options[['parValNames']] <- c("prob", "size")
  
  options[['pars']]   <- list(prob = options[['prob']], size = options[['size']])
  options[['fix.pars']] <- list(size = options[['size']])
    
  options[['pdfFun']] <- stats::dbinom
  options[['cdfFun']] <- stats::pbinom
  options[['qFun']]   <- stats::qbinom
  options[['rFun']]   <- stats::rbinom
  options[['distNameInR']] <- "binom"
  
  options[['range_x']] <- c(options[['min_x']], options[['max_x']])
  
  options[['highlightmin']] <- options[['min']]
  options[['highlightmax']] <- options[['max']]
 
  options$support <- list(min = 0, max = options[['size']])
  options$lowerBound <- c(0)
  options$upperBound <- c(1)
  
  options$transformations <- c(prob = "prob")
  
  options
}

### text fill functions -----
.ldBinomialParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of success: %s", "p \u2208 \u211D: 0 \u2264 p \u2264 1")
    pars[[2]] <- gettextf("number of trials: %s",       "n \u2208 \u2124: n \u2265 0")
    
    support <- "x \u2208 {0, 1, ..., n}"
    
    moments <- list()
    moments$expectation <- "np"
    moments$variance <- "np(1-p)"
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaBinomialPMF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>p</span>, <span style='color:blue'>n</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBinomialCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>p</span>, <span style='color:blue'>n</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBinomialQF <- function(options){
  text <- "<MATH>
    Q(x; <span style='color:red'>p</span>, <span style='color:blue'>n</span>) = 
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillBinomialEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  res <- results$structured
  res$parName <- c("p")
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$addFootnote(message = gettextf("Parameter n was fixed at value %s.", options[['size']]))
  table$setData(res)
  
  return()
}
