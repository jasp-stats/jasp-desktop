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

LDexponential <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsExponential(options)
  
  #### Show exponential section ----
  .ldIntroText(jaspResults, options, "exponential")
  .ldExponentialParsSupportMoments(jaspResults, options)
  
  
  pdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPDF", "Probability Density Function", 3)
  .ldFillPDFContainer(pdfContainer, options, .ldFormulaExponentialPDF)
  
  cdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCDF", "Cumulative Exponential Function", 4)
  .ldFillCDFContainer(cdfContainer, options, .ldFormulaExponentialCDF)
  
  qfContainer  <- .ldGetPlotContainer(jaspResults, options, "plotQF", "Quantile Function", 5)
  .ldFillQFContainer(qfContainer,   options, .ldFormulaExponentialQF)
  
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
    mleResults   <- .ldMLEResults(mleContainer, variable, options, readyFit, options$distNameInR,
                                  .ldExponentialMethodMLEStructureResults)
    .ldFillExponentialEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    
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
.ldRecodeOptionsExponential <- function(options){
  if(options$parametrization == "scale"){
    options$rate <- 1/options$par
  } else {
    options$rate <- options$par
  }
  
  options[['parValNames']] <- c("par")
  
  options[['pars']]   <- list(rate = options$rate)
  options[['pdfFun']] <- dexp
  options[['cdfFun']] <- pexp
  options[['qFun']]   <- qexp
  options[['rFun']]   <- rexp
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
  
  options
}

### text fill functions -----
.ldExponentialParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    formulas <- createJaspHtml(title = "Parameters, Support, and Moments")
    formulas$dependOn(c("parsSupportMoments", "parametrization"))
    formulas$position <- 2
    
    text <- "<b>Parameters</b>"
    
    text2 <- "<b>Support</b>
    x \u2208 \u211D<sup>+</sup>"
    
    text3 <- "<b>Moments</b> 
    E(X) = %s;
    Var(X) = %s"
    
    if(options[['parametrization']] == "scale"){
      text <- paste(text, "scale: &beta; \u2208 \u211D<sup>+</sup>")
      text3 <- sprintf(text3, "&beta;", "&beta;<sup>2</sup>")
    } else {
      text <- paste(text, "rate: &lambda; \u2208 \u211D<sup>+</sup>")
      text3 <- sprintf(text3, "&lambda;<sup>-1</sup>", "&lambda;<sup>-2</sup>")
    }
    
    formulas$text <- paste(text, text2, text3, sep = "<br><br>")
    
    jaspResults[['parsSupportMoments']] <- formulas
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
    table$addFootnote("The optimization did not converge, try adjusting the parameter values.", symbol = "<i>Warning.</i>")
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = "<i>Warning.</i>")
  }
  
  table$setData(res)
  
  return()
}

.ldExponentialMethodMLEStructureResults <- function(fit, options){
  if(is.null(fit)) return()
  
  transformations <- c(rate = "rate", scale = "1/rate")
  
  res <- sapply(transformations, function(tr) car::deltaMethod(fit$estimate, tr, fit$vcov, level = options$ciIntervalInterval))
  rownames(res) <- c("estimate", "se", "lower", "upper")
  res <- t(res)
  res <- cbind(par = rownames(res), res)
  res <- as.data.frame(res)
  
  return(res)
}