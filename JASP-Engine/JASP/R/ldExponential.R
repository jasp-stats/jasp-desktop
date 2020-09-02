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

LDexponential <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsExponential(options)
  
  #### Show exponential section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("exponential distribution"), 
                      parSupportMoments = .ldExponentialParsSupportMoments,
                      formulaPDF        = .ldFormulaExponentialPDF, 
                      formulaCDF        = .ldFormulaExponentialCDF, 
                      formulaQF         = .ldFormulaExponentialQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillExponentialEstimatesTable)
  
  return()
}

### options ----
.ldRecodeOptionsExponential <- function(options){
  if(options$parametrization == "scale"){
    options$rate <- 1/options$par
  } else {
    options$rate <- options$par
  }
  
  options[['parValNames']] <- c("par")
  
  options[['pars']]   <- list(rate = options$rate)
  options[['pdfFun']] <- stats::dexp
  options[['cdfFun']] <- stats::pexp
  options[['qFun']]   <- stats::qexp
  options[['rFun']]   <- stats::rexp
  options[['distNameInR']] <- "exp"
  
  options[['range_x']] <- c(options[['min_x']], options[['max_x']])
  
  if(options[['highlightType']] == "minmax"){
    options[['highlightmin']] <- options[['min']]
    options[['highlightmax']] <- options[['max']]
  } else if(options[['highlightType']] == "lower"){
    options[['highlightmin']] <- options[['range_x']][1]
    options[['highlightmax']] <- options[['lower_max']]
  } else if(options[['highlightType']] == "upper"){
    options[['highlightmin']] <- options[['upper_min']]
    options[['highlightmax']] <- options[['range_x']][2]
  } else{
    options[['highlightmin']] <- options[['highlightmax']] <- NULL
  }
  
  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0)
  options$upperBound <- c(Inf)
  
  options$transformations <- c(rate = "rate", scale = "1/rate")
  
  options
}

### text fill functions -----
.ldExponentialParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- switch(options[['parametrization']],
                        scale = gettextf("scale: %s", "&beta; \u2208 \u211D<sup>+</sup>"),
                        gettextf("rate: %s", "&lambda; \u2208 \u211D<sup>+</sup>"))
    
    support <- "x \u2208 \u211D<sup>+</sup>"
    
    moments <- list()
    moments$expectation <- switch(options[['parametrization']],
                                  scale = "&beta;",
                                  "&lambda;<sup>-1</sup>")
    moments$variance <- switch(options[['parametrization']],
                               scale = "&beta;<sup>2</sup>",
                               "&lambda;<sup>-2</sup>")
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaExponentialPDF <- function(options){
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

.ldFormulaExponentialCDF <- function(options){
  if(options$parametrization == "scale"){
    text <- "<MATH>
    F(x; <span style='color:red'>&beta;</span>) = 
    </MATH>"
  } else{
    
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaExponentialQF <- function(options){
  if(options$parametrization == "rate"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&beta;</span>) = 
    </MATH>"
  } else{
    
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillExponentialEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  par <- c(rate = "\u03BB", scale = "\u03B2")[options$parametrization]
  res <- results$structured
  res <- res[res$par %in% names(par),]
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
