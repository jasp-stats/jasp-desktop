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

LDf <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsF(options)
  
  #### Show f section ----
  .ldIntroText(jaspResults, options, gettext("F-distribution"))
  .ldFParsSupportMoments(jaspResults, options)
  
  
  pdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPDF", gettext("Probability Density Function"), 3)
  .ldFillPDFContainer(pdfContainer, options, .ldFormulaFPDF)
  
  cdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCDF", gettext("Cumulative Distribution Function"), 4)
  .ldFillCDFContainer(cdfContainer, options, .ldFormulaFCDF)
  
  qfContainer  <- .ldGetPlotContainer(jaspResults, options, "plotQF", gettext("Quantile Function"), 5)
  .ldFillQFContainer(qfContainer,   options, .ldFormulaFQF)
  
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
  dataContainer <- .ldGetDataContainer(jaspResults, options, errors)
  
  readyDesc <- ready && (isFALSE(errors) || (is.null(errors$infinity) && is.null(errors$observations)))
  .ldSummaryContinuousTableMain(dataContainer, variable, options, readyDesc)
  .ldObservedMomentsTableMain  (dataContainer, variable, options, readyDesc)
  .ldPlotHistogram             (dataContainer, variable, options, readyDesc)
  .ldPlotECDF                  (dataContainer, variable, options, readyDesc)
  
  
  #### Fit data and assess fit ----
  
  readyFit <- ready && isFALSE(errors)
  #### Maximum Likelihood ----
  if(options$methodMLE){
    mleContainer <- .ldGetFitContainer(jaspResults, options, "mleContainer", "Maximum likelihood", 7, errors)
    
    # parameter estimates
    mleEstimatesTable  <- .ldEstimatesTable(mleContainer, options, TRUE, TRUE, "methodMLE")
    mleResults   <- .ldMLEResults(mleContainer, variable, options, readyFit, options$distNameInR)
    .ldFillFEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    
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
.ldRecodeOptionsF <- function(options){
  options[['parValNames']] <- c("df1", "df2", "ncp")
  
  options[['pars']]   <- list(df1 = options[['df1']], df2 = options[['df2']], ncp = options[['ncp']])
  options[['pdfFun']] <- df
  options[['cdfFun']] <- pf
  options[['qFun']]   <- qf
  options[['rFun']]   <- rf
  options[['distNameInR']] <- "f"
  
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
  options$lowerBound <- c(0, 0, -Inf)
  options$upperBound <- c(Inf, Inf, Inf)
  
  options$transformations <- c(df1 = "df1", df2 = "df2", ncp = "ncp")
  
  options
}

### text fill functions -----
.ldFParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("degree of freedom: %s", "df 1 \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("degree of freedom: %s",  "df 2 \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("non-centrality: %s",     "ncp \u2208 \u211D")
    
    support <- "x \u2208 \u211D<sup>+</sup>"
    
    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, NA)
  }
}

.ldFormulaFPDF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>df 1</span>, 
    <span style='color:green'>df 2</span>, 
    <span style='color:blue'>ncp</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaFCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>df 1</span>, 
    <span style='color:green'>df 2</span>, 
    <span style='color:blue'>ncp</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaFQF <- function(options){
  text <- "<MATH>
    Q(p; <span style='color:red'>df 1</span>, 
    <span style='color:green'>df 2</span>, 
    <span style='color:blue'>ncp</span>)
    </MATH>"
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillFEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  res <- results$structured
  res$parName <- c("df 1", "df 2", "ncp")
  
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  
  table$setData(res)
  
  return()
}