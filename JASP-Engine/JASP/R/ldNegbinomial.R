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

LDnegbinomial <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsNegbinomial(options)
  
  #### Show negbinomial section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("negative binomial distribution"), 
                      parSupportMoments = .ldNegbinomialParsSupportMoments,
                      formulaPMF        = .ldFormulaNegbinomialPMF, 
                      formulaCMF        = .ldFormulaNegbinomialCDF)
  
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
  dataContainer <- .ldGetDataContainer(jaspResults, options, errors)
  
  readyDesc <- ready && (isFALSE(errors) || (is.null(errors$infinity) && is.null(errors$observations)))
  .ldSummaryContinuousTableMain(dataContainer, variable, options, readyDesc)
  .ldObservedMomentsTableMain  (dataContainer, variable, options, readyDesc)
  .ldPlotHistogram             (dataContainer, variable, options, readyDesc, "discrete")
  .ldPlotECDF                  (dataContainer, variable, options, readyDesc)
  
  
  #### Fit data and assess fit ----
  
  readyFit <- ready && isFALSE(errors)
  #### Maximum Likelihood ----
  if(options$methodMLE){
    mleContainer <- .ldGetFitContainer(jaspResults, options, "mleContainer", "Maximum likelihood", 7, errors)
    
    # parameter estimates
    mleEstimatesTable  <- .ldEstimatesTable(mleContainer, options, TRUE, TRUE, "methodMLE")
    mleResults   <- .ldMLEResults(mleContainer, variable, options, readyFit, options$distNameInR)
    .ldFillNegbinomialEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    # fit assessment
    mleFitContainer    <- .ldGetFitContainer(mleContainer, options, "mleFitAssessment", "Fit Assessment", 8)

    # fit statistics
    mleFitStatistics   <- .ldFitStatisticsTable(mleFitContainer, options, "methodMLE")
    mleFitStatisticsResults <- .ldFitStatisticsResults(mleContainer, mleResults$fitdist, variable, options, readyFit)
    .ldFillFitStatisticsTable(mleFitStatistics, mleFitStatisticsResults, options, readyFit)
    #return()
    # fit plots
    .ldFitPlots(mleFitContainer, mleResults$fitdist$estimate, options, variable, readyFit)
    
  }
  
  #### Method of moments ----
  
  #### Unbiased estimate ----
  
  return()
}

### options ----
.ldRecodeOptionsNegbinomial <- function(options){
  if(options$parametrization == "prob"){
    options$mu <- options$size*options$par / (1-options$par)
  } else {
    options$mu <- options$par
  }
  
  options[['parValNames']] <- c("size", "par")
  
  options[['pars']]   <- list(size = options[['size']], mu = options[['mu']])
    
  options[['pdfFun']] <- stats::dnbinom
  options[['cdfFun']] <- stats::pnbinom
  options[['qFun']]   <- stats::qnbinom
  options[['rFun']]   <- stats::rnbinom
  options[['distNameInR']] <- "nbinom"
  
  options[['range_x']] <- c(options[['min_x']], options[['max_x']])
  
  options[['highlightmin']] <- options[['min']]
  options[['highlightmax']] <- options[['max']]
 
  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)
  
  options$transformations <- c(size = "size", prob = "size / (size + mu)", mu = "mu")
  
  options
}

### text fill functions -----
.ldNegbinomialParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- switch(options[['parametrization']],
                        prob = gettextf("number of successes: %s", "k \u2208 \u211D: \u03D5 \u2265 0"),
                               gettextf("dispersion: %s",          "\u03D5 \u2208 \u211D: \u03D5 \u2265 0"))
    pars[[2]] <- switch(options[['parametrization']],
                        prob = gettextf("probability of success: %s", "p \u2208 \u211D: 0 \u2264 p \u2264 1"),
                               gettextf("mean: %s",                   "\u03BC \u2208 \u211D: \u03BC \u2265 0"))
    
    support <- "x \u2208 {0, 1, 2, ...}"
    
    moments <- list()
    moments$expectation <- switch(options[['parametrization']],
                                  prob = "pk/(1-p)",
                                         "\u03BC")
    moments$variance <- switch(options[['parametrization']],
                               prob = "pk/(1-p)<sup>2</sup>",
                                      "\u03BC + \u03BC<sup>2</sup>/\u03D5")
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaNegbinomialPMF <- function(options){
  if(options$parametrization == "prob"){
    text <- "<MATH>
    f(x; <span style='color:red'>\u03D5</span>, <span style='color:blue'>p</span>) = 
    </MATH>"
  } else{
    text <- "<MATH>
    f(x; <span style='color:red'>\u03D5</span>, <span style='color:blue'>\u03BC</span>) = 
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaNegbinomialCDF <- function(options){
  if(options$parametrization == "prob"){
    text <- "<MATH>
    F(x; <span style='color:red'>\u03D5</span>, <span style='color:blue'>p</span>) = 
    </MATH>"
  } else{
    text <- "<MATH>
    F(x; <span style='color:red'>\u03D5</span>, <span style='color:blue'>\u03BC</span>) = 
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaNegbinomialQF <- function(options){
  if(options$parametrization == "prob"){
    text <- "<MATH>
    Q(p; <span style='color:red'>\u03D5</span>, <span style='color:blue'>p</span>) = 
    </MATH>"
  } else{
    text <- "<MATH>
    Q(p; <span style='color:red'>\u03D5</span>, <span style='color:blue'>\u03BC</span>) = 
    </MATH>"
  }
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillNegbinomialEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  res <- results$structured
  if(options$parametrization == "prob"){
    res$parName <- c("k", "p", "\u03BC")
    res <- res[res$par != "mu",,drop=FALSE]
  } else{
    res$parName <- c("\u03D5", "p", "\u03BC")
    res <- res[res$par != "prob",,drop=FALSE]
  }
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}