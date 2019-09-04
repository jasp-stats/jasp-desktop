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

LDdistribution <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsDistribution(options)
  
  #### Show distribution section ----
  .ldIntroText(jaspResults, options, "name of distr")
  .ldDistributionParsSupportMoments(jaspResults, options)
  
  
  pdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotPDF", "Probability Density Function", 3)
  .ldFillPDFContainer(pdfContainer, options, .ldFormulaDistributionPDF)
  
  cdfContainer <- .ldGetPlotContainer(jaspResults, options, "plotCDF", "Cumulative Distribution Function", 4)
  .ldFillCDFContainer(cdfContainer, options, .ldFormulaDistributionCDF)
  
  qfContainer  <- .ldGetPlotContainer(jaspResults, options, "plotQF", "Quantile Function", 5)
  .ldFillQFContainer(qfContainer,   options, .ldFormulaDistributionQF)
  
  #### Generate and Display data section ----
  # simulate and read data
  #.simulateData(jaspResults, options)
  
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
                                  .ldDistributionMethodMLEStructureResults)
    .ldFilldistributionEstimatesTable(mleEstimatesTable, mleResults, options, readyFit)
    
    
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
.ldRecodeOptionsDistribution <- function(options){
  if(options$parametrization == "sigma2"){
    options$sd <- sqrt(options$varValue)
  } else if(options$parametrization == "sigma"){
    options$sd <- options$varValue
  } else if(options$parametrization == "tau2"){
    options$sd <- sqrt(1/options$varValue)
  } else if(options$parametrization == "tau"){
    options$sd <- 1/options$varValue
  }
  
  options[['parValNames']] <- c("mu", "varValue")
  
  options[['pars']]   <- list(mean = options[['mu']], sd = options[['sd']])
  options[['pdfFun']] <- dnorm
  options[['cdfFun']] <- pnorm
  options[['qFun']]   <- qnorm
  options[['rFun']]   <- rnorm
  options[['distNameInR']] <- "norm"
  
  options[['range_x']] <- c(-options[['range']], options[['range']])
  
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
  
  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, 0)
  options$upperBound <- c(Inf, Inf)
  
  options
}

### text fill functions -----
.ldDistributionParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    formulas <- createJaspHtml(title = "Parameters, Support, and Moments")
    formulas$dependOn(c("parsSupportMoments", "parametrization"))
    formulas$position <- 2
    
    text <- "<b>Parameters</b>
    mean: &mu; \u2208 \u211D
    "
    
    text2 <- "<b>Support</b>
    x \u2208 \u211D"
    
    text3 <- "<b>Moments</b> 
    E(X) = &mu;
    Var(X) = "
    
    if(options[['parametrization']] == "sigma2"){
      text <- paste(text,
                    "variance: &sigma;<sup>2</sup> \u2208 \u211D<sup>+</sup>
                    ")
      text3 <- paste0(text3, "&sigma;<sup>2</sup>")
    } else if(options[['parametrization']] == "sigma"){
      text <- paste(text,
                    "standard deviation: &sigma; \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "&sigma;<sup>2</sup>")
    } else if(options[['parametrization']] == "tau2"){
      text <- paste(text,
                    "precision: &tau;<sup>2</sup> \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "1/&tau;<sup>2</sup>")
    } else{
      text <- paste(text,
                    "square root of precision: &tau; \u2208 \u211D<sup>+</sup>")
      text3 <- paste0(text3, "1/&tau;<sup>2</sup>")
    }
    
    formulas$text <- paste(text, text2, text3, sep = "<br><br>")
    
    jaspResults[['parsSupportMoments']] <- formulas
  }
}

.ldFormulaDistributionPDF <- function(options){
  if(options[['parametrization']] == "sigma2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>) = 
(2&pi;<span style='color:blue'>&sigma;&sup2;</span>)<sup>-&frac12;</sup> 
exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;&sup2;</span>]
    </MATH>"
  } else if(options[['parametrization']] == "sigma"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>) = 
    (2&pi;<span style='color:blue'>&sigma;</span>&sup2;)<sup>-&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;</span>&sup2;]
    </MATH>"
  } else if(options[['parametrization']] == "tau2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>) = 
    (<span style='color:blue'>&tau;&sup2;</span> &frasl; 2&pi;)<sup>&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;&sup2;</span> &frasl; 2]
    </MATH>"
  } else if(options[['parametrization']] == "tau"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>) = 
    <span style='color:blue'>&tau;</span> &frasl; (2&pi;)<sup>&frac12;</sup> 
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;</span>&sup2; &frasl; 2]
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaDistributionCDF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaDistributionQF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }
  
  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillDistributionEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  par1 <- c(mu = "\u03BC")
  par2 <- c(sigma2 = "\u03C3\u00B2", sigma = "\u03C3", 
            tau2   = "\u03C4\u00B2", tau   = "\u03C4")[options$parametrization]
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

.ldDistributionMethodMLEStructureResults <- function(fit, options){
  if(is.null(fit)) return()
  
  transformations <- c(mu = "mean", sigma2 = "sd^2", sigma = "sd", tau2 = "1/sd^2", tau = "1/sd")
  
  res <- sapply(transformations, function(tr) car::deltaMethod(fit$estimate, tr, fit$vcov, level = options$ciIntervalInterval))
  rownames(res) <- c("estimate", "se", "lower", "upper")
  res <- t(res)
  res <- cbind(par = rownames(res), res)
  res <- as.data.frame(res)
  
  return(res)
}