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

LDgammaInverse <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDgammaInverse(options)
  
  #### Show gammaInverse section ----
  .ldIntroText(jaspResults, options, "inverse gamma distribution")
  .ldGammaInverseParsSupportMoments(jaspResults, options)
  
  
  pdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPDF", "Probability Density Function", 3)
  .ldFillPDFContainer(pdfContainer, options, .ldFormulaGammaInversePDF)
  
  cdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCDF", "Cumulative GammaInverse Function", 4)
  .ldFillCDFContainer(cdfContainer, options, .ldFormulaGammaInverseCDF)
  
  qfContainer  <- .ldGetPlotContainer(jaspResults, options, "plotQF", "Quantile Function", 5)
  .ldFillQFContainer(qfContainer,   options, .ldFormulaGammaInverseQF)
  
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
                                  .ldGammaInverseMethodMLEStructureResults)
    .ldFillGammaInverseEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    
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
.recodeOptionsLDgammaInverse <- function(options){
  if(options$parametrization == "scale"){
    options$rate <- 1/options$par2
  } else if(options$parametrization == "mean"){
    options$rate <- options$shape / options$par2
  } else {
    options$rate <- options$par2
  }
  
  options[['parValNames']] <- c("shape", "par2")
  
  options[['pars']]   <- list(shape = options[['shape']], rate = options[['rate']])
  options[['pdfFun']] <- invgamma::dinvgamma
  options[['cdfFun']] <- invgamma::pinvgamma
  options[['qFun']]   <- invgamma::qinvgamma
  options[['rFun']]   <- invgamma::rinvgamma
  
  # distribution from packages need to be exported to the global namespace (for fitting)
  #dinvgamma <<- invgamma::dinvgamma
  #pinvgamma <<- invgamma::pinvgamma
  #qinvgamma <<- invgamma::qinvgamma
  #rinvgamma <<- invgamma::rinvgamma
  options[['distNameInR']] <- "invgamma"
  
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
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)
  
  options
}

### text fill functions -----
.ldGammaInverseParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    formulas <- createJaspHtml(title = "Parameters, Support, and Moments")
    formulas$dependOn(c("parsSupportMoments", "parametrization"))
    formulas$position <- 2
    
    text <- "<b>Parameters</b> </br>"
    
    text2 <- "<b>Support</b>
    x \u2208 \u211D<sup>+</sup>"
    
    text3 <- "<b>Moments</b> 
    E(X) = %s
    Var(X) = %s"
    
    if(options[['parametrization']] == "scale"){
      
      text <- paste(text,       "shape: k \u2208 \u211D<sup>+</sup> </br>")
      text <- paste(text, "scale: &theta; \u2208 \u211D<sup>+</sup>")
      
      text3 <- sprintf(text3, "k&theta;", "k&theta;<sup>2</sup>")
      
    } else if(options[['parametrization']] == "mean"){
      
      text <- paste(text,    "shape k \u2208 \u211D<sup>+</sup> </br>")
      text <- paste(text, "mean: &mu; \u2208 \u211D<sup>+</sup>")
      
      text3 <- sprintf(text3, "&mu;", "&mu;<sup>2</sup>k<sup>-1</sup>")
      
    } else{
      
      text <- paste(text, "shape: &alpha; \u2208 \u211D<sup>+</sup> </br>")
      text <- paste(text,   "rate: &beta; \u2208 \u211D<sup>+</sup>")
      
      text3 <- sprintf(text3, "&alpha;&beta;<sup>-1</sup>", "&alpha;&beta;<sup>-2</sup>")
      
    }
    
    formulas$text <- paste(text, text2, text3, sep = "<br><br>")
    
    jaspResults[['parsSupportMoments']] <- formulas
  }
}

.ldFormulaGammaInversePDF <- function(options){
  if(options[['parametrization']] == "scale"){
    text <- "<MATH>
    f(x; <span style='color:red'>&alpha;</span>, <span style='color:blue'>&beta;</span>) = 
(2&pi;<span style='color:blue'>&sigma;&sup2;</span>)<sup>-&frac12;</sup> 
exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;&sup2;</span>]
    </MATH>"
  } else if(options[['parametrization']] == "mean"){
    text <- "<MATH>
    f(x; <span style='color:red'>k</span>, <span style='color:blue'>&mu;</span>) = 
    (2&pi;<span style='color:blue'>&sigma;</span>&sup2;)<sup>-&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;</span>&sup2;]
    </MATH>"
  } else {
    text <- "<MATH>
    f(x; <span style='color:red'>k</span>, <span style='color:blue'>&theta;</span>) = 
    (<span style='color:blue'>&tau;&sup2;</span> &frasl; 2&pi;)<sup>&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;&sup2;</span> &frasl; 2]
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGammaInverseCDF <- function(options){
  if(options$parametrization == "scale"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "mean"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGammaInverseQF <- function(options){
  if(options$parametrization == "scale"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "mean"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillGammaInverseEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  par1 <- c(shape = c(scale = "k", rate = "\u03B1", mean = "k")[[options$parametrization]])
  par2 <- c(scale = "\u03B8", rate = "\u03B2", mean = "\u03BC")[options$parametrization]
  res <- results$structured
  res <- res[res$par %in% names(c(par1, par2)),]
  res$parName <- c(par1, par2)
  
  if(results$fitdist$convergence != 0){
    table$addFootnote("The optimization did not converge, try adjusting the parameter values.", symbol = "<i>Warning.</i>")
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = "<i>Warning.</i>")
  }
  
  table$setData(res)
  
  return()
}

.ldGammaInverseMethodMLEStructureResults <- function(fit, options){
  if(is.null(fit)) return()
  
  transformations <- c(shape = "shape", scale = "1/rate",  rate = "rate", mean = "shape/rate")
  
  res <- sapply(transformations, function(tr) car::deltaMethod(fit$estimate, tr, fit$vcov, level = options$ciIntervalInterval))
  rownames(res) <- c("estimate", "se", "lower", "upper")
  res <- t(res)
  res <- cbind(par = rownames(res), res)
  res <- as.data.frame(res)
  
  return(res)
}