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

LDchisq <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsChisq(options)
  
  #### Show chisq section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettextf("%s<sup>2</sup> distribution","\u03A7"), 
                      parSupportMoments = .ldChisqParsSupportMoments,
                      formulaPDF        = .ldFormulaChisqPDF, 
                      formulaCDF        = .ldFormulaChisqCDF, 
                      formulaQF         = .ldFormulaChisqQF)
  
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
  
  readyFit <- ready && isFALSE(errors)
  #### Maximum Likelihood ----
  if(options$methodMLE){
    mleContainer <- .ldGetFitContainer(jaspResults, options, "mleContainer", "Maximum likelihood", 7, errors)
    
    # parameter estimates
    mleEstimatesTable  <- .ldEstimatesTable(mleContainer, options, TRUE, TRUE, "methodMLE")
    mleResults   <- .ldMLEResults(mleContainer, variable, options, readyFit, options$distNameInR)
    .ldFillChisqEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    
    # fit assessment
    mleFitContainer    <- .ldGetFitContainer(mleContainer, options, "mleFitAssessment", "Fit Assessment", 8)
    
    # fit statistics
    mleFitStatistics   <- .ldFitStatisticsTable(mleFitContainer, options, "methodMLE")
    mleFitStatisticsResults <- .ldFitStatisticsResults(mleContainer, mleResults$fitdist, variable, options, readyFit)
    .ldFillFitStatisticsTable(mleFitStatistics, mleFitStatisticsResults, options, readyFit)
    # fit plots
    .ldFitPlots(mleFitContainer, mleResults$fitdist$estimate, options, variable, readyFit)
    
  }
  
  #### Method of moments ----
  
  #### Unbiased estimate ----
  
  return()
}

### options ----
.ldRecodeOptionsChisq <- function(options){
  options[['parValNames']] <- c("df", "ncp")
  
  options[['pars']]   <- list(df = options[['df']], ncp = options[['ncp']])
  options[['pdfFun']] <- stats::dchisq
  options[['cdfFun']] <- stats::pchisq
  options[['qFun']]   <- stats::qchisq
  options[['rFun']]   <- stats::rchisq
  options[['distNameInR']] <- "chisq"
  
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
  options$lowerBound <- c(0,  -Inf)
  options$upperBound <- c(Inf, Inf)
  
  options$transformations <- c(df = "df", ncp = "ncp")
  
  options
}

### text fill functions -----
.ldChisqParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("degree of freedom: %s", "k \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("non-centrality: %s",    "&lambda; \u2208 \u211D")
    
    support <- "x \u2208 \u211D<sup>+</sup>"
    
    moments <- list()
    moments$expectation <- "k + &lambda;"
    moments$variance <- "2(k + 2 &lambda;)"
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaChisqPDF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>k</span>, <span style='color:blue'>&lambda;</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaChisqCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>k</span>, <span style='color:blue'>&lambda;</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaChisqQF <- function(options){
  text <- "<MATH>
    Q(p; <span style='color:red'>k</span>, <span style='color:blue'>&lambda;</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillChisqEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  par1 <- c(df = "k")
  par2 <- c(ncp = "\u03BB")
  res <- results$structured
  res <- res[res$par %in% names(c(par1, par2)),]
  res$parName <- c(par1, par2)
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}
