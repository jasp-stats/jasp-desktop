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

LDlogistic <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDLogistic(options)
  
  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("logistic distribution"), 
                      parSupportMoments = .ldLogisticParsSupportMoments,
                      formulaPDF        = .ldFormulaLogisticPDF, 
                      formulaCDF        = .ldFormulaLogisticCDF, 
                      formulaQF         = .ldFormulaLogisticQF)
  
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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillLogisticEstimatesTable)
  
  return()
}

.recodeOptionsLDLogistic <- function(options){
  options[['parValNames']] <- c("mu", "sigma")
  
  options[['pars']]   <- list(location = options[['mu']], scale = options[['sigma']])
  options[['pdfFun']] <- stats::dlogis
  options[['cdfFun']] <- stats::plogis
  options[['qFun']]   <- stats::qlogis
  options[['rFun']]   <- stats::rlogis
  options[['distNameInR']] <- "logis"
  
  options <- .ldOptionsDeterminePlotLimits(options)
  
  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, 0)
  options$upperBound <- c(Inf, Inf)
  
  options$transformations <- c(mu = "location", sigma = "scale")
  
  options
}

### text fill functions -----
.ldLogisticParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: &mu; %s","\u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&sigma; \u2208 \u211D<sup>+</sup>")
    
    support <- "x \u2208 \u211D"
    
    moments <- list()
    moments$expectation <- gettext("&mu;")
    moments$variance <- gettext("(&sigma; &pi;)<sup>2</sup> / 3")
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaLogisticPDF <- function(options){
  if(options[['parametrization']] == "scale"){
    text <- "<MATH>
    f(x; <span style='color:red'>&beta;</span>) = 
    </MATH>"
  } else {
    text <- "<MATH>
    f(x; <span style='color:red'>&lambda;</span>) = <span style='color:red'>&lambda;</span>exp(-<span style='color:red'>&lambda;</span>x)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaLogisticCDF <- function(options){
  if(options$parametrization == "scale"){
    text <- "<MATH>
    F(x; <span style='color:red'>&beta;</span>) = 
    </MATH>"
  } 
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaLogisticQF <- function(options){
  if(options$parametrization == "rate"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&beta;</span>) = 
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillLogisticEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  par <- c(location = "\u03BC", scale = "\u03C3")
  res <- results$structured
  res$parName <- par
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}