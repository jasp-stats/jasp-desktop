#
# Copyright (C) 2013-2015 University of Amsterdam
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

TTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
  
  if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
  if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
  if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
  if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
  if(is.null(options()$BFprogress)) options(BFprogress = interactive())
  if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)
  
  
  all.variables <- unlist(options$variables)
  
  if (is.null(dataset))
  {
    if (perform == "run") {
      
      if (options$missingValues == "excludeListwise") {
        
        dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
        
      } else {
        
        dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
      }
      
    } else {
      
      dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
    }
  }
  
  results <- list()
  
  meta <- list()
  
  meta[[1]] <- list(name="ttest", type="table")
  meta[[2]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"),
                                                                  list(name = "descriptivesPlots", type = "collection", meta="image")))
  meta[[3]] <- list(name="inferentialPlots", type="collection", meta=list(	name="plotGroups", type="object",
                                                                           meta=list(
                                                                             list(name="PriorPosteriorPlot", type="image"),
                                                                             list(name="BFrobustnessPlot", type="image"),
                                                                             list(name="BFsequentialPlot", type="image")
                                                                           )))
  
  results[[".meta"]] <- meta
  results[["title"]] <- "Bayesian One Sample T-Test"
  
  ttest <- list()
  
  ttest[["title"]] <- "Bayesian One Sample T-Test"
  
  if (options$effectSizeStandardized == "default") {
    ttest[["citation"]] <- list(
      "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
      "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
  } else if (options$effectSizeStandardized == "informative") {
    ttest[["citation"]] <- list(
      "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479")
  }
  
  
  bf.type <- options$bayesFactorType
  
  
  if (bf.type == "BF10") {
    
    BFH1H0 <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "BF\u2081\u2080"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "BF\u208A\u2080"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "BF\u208B\u2080"
    }
    
  } else if (bf.type == "LogBF10") {
    
    BFH1H0 <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
    }
    
  } else if (bf.type == "BF01") {
    
    BFH1H0 <- FALSE
    
    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "BF\u2080\u2081"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "BF\u2080\u208A"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "BF\u2080\u208B"
    }
  }
  
  
  if (options$hypothesis == "notEqualToTestValue") {
    
    fields <- list(
      list(name="Variable", type="string", title=""),
      list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
      list(name="error", type="number", format="sf:4;dp:3", title="error %"))
    
  } else {
    
    fields <- list(
      list(name="Variable", type="string", title=""),
      list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
      list(name="error", type="number", format="sf:4;dp:3;~", title="error %"))
  }
  
  ttest[["schema"]] <- list(fields=fields)
  
  results[["ttest"]] <- ttest
  
  footnotes <- .newFootnotes()
  
  if (options$hypothesis == "greaterThanTestValue") {
    
    note <- "For all tests, the alternative hypothesis specifies that the mean
					is greater than "
    message <- paste0(note, options$testValue, ".")
    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
    
  } else if (options$hypothesis == "lessThanTestValue") {
    
    note <- "For all tests, the alternative hypothesis specifies that the mean
					is less than "
    message <- paste0(note, options$testValue, ".")
    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
    
  } else {
    
    if (options$testValue != 0) {
      
      message <- paste("All tests, hypothesis is population mean is different from ", options$testValue, sep="")
      .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
    }
  }
  
  if (options$hypothesis == "notEqualToTestValue") {
    nullInterval <- NULL
    oneSided <- FALSE
  }
  if (options$hypothesis == "greaterThanTestValue") {
    nullInterval <- c(0, Inf)
    oneSided <- "right"
  }
  if (options$hypothesis == "lessThanTestValue") {
    nullInterval <- c(-Inf, 0)
    oneSided <- "left"
  }
  
  if (options$descriptives || options$descriptivesPlots)
    results[["descriptives"]] <- list(title="Descriptives")
  
  plotGroups <- list()
  
  ttest.rows <- list()
  plots.ttest <- list()
  plotTypes <- list()
  plotVariables <- list()
  descriptPlotVariables <- list()
  descriptivesPlots <- list()
  errorFootnotes <- rep("no", length(options$variables))
  
  state <- .retrieveState()
  
  diff <- NULL
  
  if (!is.null(state)) {
    
    diff <- .diff(options, state$options)
    
  }
  
  status <- rep("ok", length(options$variables))
  plottingError <- rep("error", length(options$variables))
  BF10post <- numeric(length(options$variables))
  
  i <- 1
  
  for (variable in options[["variables"]])
  {
    
    if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis) {
      
      plotGroups[[i]] <- list()
      plotGroups[[i]][["title"]] <- variable
      plotGroups[[i]][["name"]] <- variable
    }
    
    if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
      (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
      diff$testValue == FALSE && diff$missingValues == FALSE)))) {
      
      index <- which(state$options$variables == variable)
      
      if (state$errorFootnotes[index] == "no") {
        
        ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]
        
      } else {
        
        index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
        
        errorFootnotes[i] <- state$errorFootnotes[index]
        
        ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
      }
      
      BF10post[i] <- state$BF10post[index]
      status[i] <- state$status[index]
      plottingError[i] <- state$plottingError[index]
      
    } else {
      
      ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", error=".")
    }
    
    
    if (options$descriptivesPlots) {
      
      if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
         (is.list(diff) && (diff$testValue == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE &&
                            diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        
        index <- which(state$descriptPlotVariables == variable)
        
        descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]
        
        
      } else {
        
        descriptivesPlot <- list()
        
        descriptivesPlot[["title"]] <- variable
        descriptivesPlot[["width"]] <- options$plotWidth
        descriptivesPlot[["height"]] <- options$plotHeight
        descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
        descriptivesPlot[["status"]] <- "waiting"
        descriptivesPlot[["data"]] <- ""
        
        descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
      }
      
      descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable
      
    }
    
    if (options$plotPriorAndPosterior){
      
      
      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
          options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        
        index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")
        
        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]
        
      } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                   diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                   diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                   diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                   diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                   diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                                   diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
                 !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # if the requested plot already exists use it
        
        index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")
        
        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]
        
      } else {
        
        plot <- list()
        
        plot[["title"]] <- "Prior and Posterior"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"
        
        .plotFunc <- function() {
          .plotPosterior.summarystats.ttest(addInformation = options$plotPriorAndPosteriorAdditionalInf, dontPlotData = TRUE)
        }
        
        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]

        plots.ttest[[length(plots.ttest)+1]] <- plot
      }
      
      plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[length(plots.ttest)]]
      
      
      if (options$plotPriorAndPosteriorAdditionalInfo) {
        
        plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
        
      } else {
        
        plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
      }
      
      plotVariables[[length(plotVariables)+1]] <- variable
    }
    
    if (options$plotBayesFactorRobustness){
      
      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                             diff$informativeNormalStd == FALSE))) && "robustnessPlot" %in% state$plotTypes) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        
        index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")
        
        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]
        
      } else {
        
        plot <- list()
        
        plot[["title"]] <- "Bayes Factor Robustness Check"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"

        .plotFunc <- function() {
          .plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
        }
        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
        
        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]
        
        plots.ttest[[length(plots.ttest)+1]] <- plot
      }
      
      plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[length(plots.ttest)]]
      
      plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
      plotVariables[[length(plotVariables)+1]] <- variable
    }
    
    if (options$plotSequentialAnalysis){
      
      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
          options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        
        index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")
        
        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]
        
      } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                   diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                   diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                   diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                   diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                   diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                                   diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
                 !options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {
        
        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists use it
        
        index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")
        
        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]
        
      } else {
        
        plot <- list()
        
        plot[["title"]] <- "Sequential Analysis"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"

        .plotFunc <- function() {
          .plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, options = options)
        }
        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
        
        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]
        
        plots.ttest[[length(plots.ttest)+1]] <- plot
      }
      
      if (options$plotSequentialAnalysisRobustness) {
        
        plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"
        
      } else {
        
        plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
      }
      
      plotVariables[[length(plotVariables)+1]] <- variable
      plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[length(plots.ttest)]]
    }
    
    
    i <- i + 1
  }
  
  ttest[["data"]] <- ttest.rows
  ttest[["footnotes"]] <- as.list(footnotes)
  results[["ttest"]] <- ttest
  
  if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)
    results[["inferentialPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1 ||
                                                         sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)) > 1,
                                                       "Inferential Plots", "Inferential Plot"), collection=plotGroups)
  
  if (options$descriptivesPlots)
    results[["descriptives"]][["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
  
  if (options$descriptives) {
    
    descriptivesComplete <- FALSE
    
    descriptives <- list()
    
    descriptives[["title"]] <- "Descriptives"
    descriptives[["cases"]] <- I(options$variables)
    
    fields <- list(
      list(name="v",    title="",   type="string"),
      list(name="N",    title="N",  type="integer"),
      list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
      list(name="sd",   title="SD", type="number",   format="sf:4;dp:3"),
      list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))
    
    ## add credible interval values if asked for in plot
    if (options$descriptivesPlots) {
      interval <- 100 * options$descriptivesPlotsCredibleInterval
      title <- paste0(interval, "% Credible Interval")
      fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
                                           format = "sf:4;dp:3", title = "Lower",
                                           overTitle = title)
      fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
                                           format = "sf:4;dp:3", title = "Upper",
                                           overTitle = title)
    }
    
    descriptives[["schema"]] <- list(fields=fields)
    descriptives.results <- list()
    
    variables <- options[["variables"]]
    if (length(variables) == 0)
      variables = "."
    
    for (variable in variables) {
      
      if (perform == "run" && length(options[["variables"]]) > 0) {
        
        data <- na.omit(dataset[[ .v(variable) ]])
        
        if (class(data) != "factor") {
          
          posteriorSummary <- .posteriorSummaryGroupMean(variable=data, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
          ciLower <- .clean(posteriorSummary$ciLower)
          ciUpper <- .clean(posteriorSummary$ciUpper)
          
          n    <- .clean(length(data))
          mean <- .clean(mean(data))
          stdDeviation <- .clean(sd(data))
          stdErrorMean <- .clean(sd(data)/sqrt(length(data)))
          
          result <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, lowerCI = ciLower, upperCI = ciUpper)
        } else {
          
          n <- .clean(length(data))
          result <- list(v=variable, N=n, mean="", sd="", se="", lowerCI="", upperCI="")
        }
        
        descriptivesComplete <- TRUE
        
      } else {
        
        result <- list(v=variable, N=".", mean=".", sd= ".", se=".", lowerCI=".", upperCI=".")
        
      }
      
      descriptives.results[[length(descriptives.results)+1]] <- result
    }
    
    descriptives[["data"]] <- descriptives.results
    
    if (descriptivesComplete)
      descriptives[["status"]] <- "complete"
    
    results[["descriptives"]][["descriptivesTable"]] <- descriptives
  }
  
  
  if (perform == "run") {
    
    
    i <- 1
    
    tValue <- rep(NA, length(options[["variables"]]))
    n <- rep(NA, length(options[["variables"]]))
    
    for (variable in options[["variables"]])
    {
      
      if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE)))) {
        
        index <- which(state$options$variables == variable)
        
        if (state$errorFootnotes[index] == "no") {
          
          ttest.rows[[i]] <- state$results$ttest$data[[index]]
          
        } else {
          
          index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
          
          errorFootnotes[i] <- state$errorFootnotes[index]
          
          ttest.rows[[i]] <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
        }
        
        BF10post[i] <- state$BF10post[index]
        tValue[i] <- state$tValue[index]
        n[i] <- state$n[index]
        status[i] <- state$status[index]
        plottingError[i] <- state$plottingError[index]
        
      } else {
        
        result <- try (silent = TRUE, expr = {
          
          variableData <- dataset[[ .v(variable) ]]
          variableData <- variableData[ ! is.na(variableData) ]
          
          variableData <- variableData - options$testValue
          r <- .generalTtestBF(x = variableData, oneSided = oneSided, options = options)
          bf.raw <- r[["bf"]]
          tValue[i] <- r[["tValue"]]
          n[i] <- r[["n1"]]
          
          if (bf.type == "BF01")
            bf.raw <- 1 / bf.raw
          
          BF10post[i] <- bf.raw
          BF <- .clean(bf.raw)
          
          if (options$bayesFactorType == "LogBF10") {
            
            BF <- log(BF10post[i])
            BF <- .clean(BF)
          }
          
          error <- .clean(r[["error"]])
          
          list(Variable=variable, BF=BF, error=error)
        })
        
        
        if (class(result) == "try-error") {
          
          errorMessage <- .extractErrorMessage(result)
          
          if (errorMessage == "x or y must not contain missing or infinite values.") {
            
            errorMessage <- paste("Bayes factor is undefined - the sample contains infinity")
            
            status[i] <- "error"
            plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample contains infinity"
            
            #} else if (errorMessage == "data are essentially constant") {
            #
            #	errorMessage <- paste("Bayes factor is undefined - the sample contains all the same value (zero variance)")
            #
          } else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
            
            errorMessage <- "Bayes factor is undefined - too few observations"
            
            status[i] <- "error"
            plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample has too few observations"
          }
          
          status[i] <- "error"
          plottingError[i] <- paste("Plotting is not possible:", errorMessage)
          
          errorFootnotes[i] <- errorMessage
          
          index <- .addFootnote(footnotes, errorMessage)
          
          result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
          
          ttest.rows[[i]] <- result
          
        } else {
          
          if (is.na(bf.raw)) {
            
            status[i] <- "error"
            plottingError[i] <- "Plotting is not possible: Bayes factor could not be calculated"
          } else if(bf.raw == Inf & (options$plotPriorAndPosterior | options$plotBayesFactorRobustness | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
            
            status[i] <- "error"
            plottingError[i] <- "Plotting is not possible: Bayes factor is infinite"
          } else if (is.infinite(1 / bf.raw)) {
            
            status[i] <- "error"
            plottingError[i] <- "Plotting is not possible: The Bayes factor is too small"
          }
          
          ind <- which(variableData == variableData[1])
          idData <- sum((ind+1)-(1:(length(ind))) == 1)
          
          if(idData > 1 & (options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
            
            #seqFootnote <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")
            #plotSequentialStatus <- "error"
            # status[i] <- "sequentialNotPossible"
            # plottingError[i] <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")
          }
          
          
          ttest.rows[[i]] <- result
        }
      }
      
      i <- i + 1
    }
    
    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)
    ttest[["status"]] <- "complete"
    results[["ttest"]] <- ttest
    
    if ( ! .shouldContinue(callback()))
      return()
    
    i <- 1
    descriptInd <- 1
    z <- 1
    
    for (variable in options[["variables"]])
    {
      
      variableData <- dataset[[ .v(variable) ]]
      variableData <- variableData[ ! is.na(variableData) ]
      variableDataDescriptivesPlot <- variableData
      variableData <- variableData - options$testValue
      
      if (options$descriptivesPlots) {
        
        
        if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$testValue == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE &&
                               diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it
          
          index <- which(state$descriptPlotVariables == variable)
          
          descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
          
          
        } else {
          
          results[["descriptives"]][["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"
          
          if ( ! .shouldContinue(callback(results)))
            return()
          
          plot <- descriptivesPlots[[descriptInd]]
          
          p <- try(silent= FALSE, expr= {
            
            obj <- .plotGroupMeanBayesOneSampleTtest(variable=variableDataDescriptivesPlot, variableName=variable, testValueOpt=options$testValue,
                                                     descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
            content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = obj, obj = TRUE)
            
            plot[["convertible"]] <- TRUE
            plot[["obj"]] <- content[["obj"]]
            plot[["data"]] <- content[["png"]]
            
          })
          
          if (class(p) == "try-error") {
            
            errorMessageTmp <- .extractErrorMessage(p)
            errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
            plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
          }
          
          plot[["status"]] <- "complete"
          
          descriptivesPlots[[descriptInd]] <- plot
        }
        
        results[["descriptives"]][["descriptivesPlots"]][["collection"]] <- descriptivesPlots
        
        descriptInd <- descriptInd + 1
        
        if ( ! .shouldContinue(callback(results)))
          return()
        
      }
      
      if (options$plotPriorAndPosterior) {
        
        if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) &&
            options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it
          
          index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")
          
          plots.ttest[[z]] <- state$plotsTtest[[index]]
          
        } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                  (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                     diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                     diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                     diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                     diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                     diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                                     diff$informativeNormalStd == FALSE))) &&
                   !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # if the requested plot already exists use it
          
          index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")
          
          plots.ttest[[z]] <- state$plotsTtest[[index]]
          
        } else {
          
          results[["inferentialPlots"]][["collection"]][[i]][["PriorPosteriorPlot"]][["status"]] <- "running"
          
          if ( ! .shouldContinue(callback(results)))
            return()
          
          plot <- plots.ttest[[z]]
          
          if (status[i] != "error") {
            
            p <- try(silent= FALSE, expr= {
              
              .plotFunc <- function() {
                .plotPosterior.summarystats.ttest(t = tValue[i], n1 = n[i],
                                                  oneSided = oneSided, BF = BF10post[i], BFH1H0 = BFH1H0,
                                                  rscale = options$priorWidth,
                                                  addInformation = options$plotPriorAndPosteriorAdditionalInfo,
                                                  options = options)
              }
              
              content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
              plot[["convertible"]] <- TRUE
              plot[["obj"]] <- content[["obj"]]
              plot[["data"]] <- content[["png"]]
              
            })
            
            if (class(p) == "try-error") {
              
              errorMessage <- .extractErrorMessage(p)
              
              if (errorMessage == "not enough data") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              } else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              }
              
              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
            }
          } else {
            
            plot[["error"]] <- list(error="badData", errorMessage= plottingError[i])
          }
          
          plot[["status"]] <- "complete"
          
          plots.ttest[[z]] <- plot
        }
        
        plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[z]]
        results[["inferentialPlots"]][["collection"]] <- plotGroups
        
        z <- z + 1
        
        if ( ! .shouldContinue(callback(results)))
          return()
      }
      
      if (options$plotBayesFactorRobustness) {
        
        if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) && "robustnessPlot" %in% state$plotTypes) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it
          
          index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")
          
          plots.ttest[[z]] <- state$plotsTtest[[index]]
          
        } else {
          
          results[["inferentialPlots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "running"
          
          if ( ! .shouldContinue(callback(results)))
            return()
          
          plot <- plots.ttest[[z]]
          
          if (options$effectSizeStandardized == "informative") {
            plot[["error"]] <- list(error="badData", errorMessage="Bayes factor robustness check plot currently not supported for informed prior.")
          } else if (status[i] != "error") {
            
            p <- try(silent= FALSE, expr= {

              .plotFunc <- function() {
                .plotBF.robustnessCheck.ttest (x= variableData, oneSided= oneSided, BF10post=BF10post[i], rscale = options$priorWidth, BFH1H0= BFH1H0)
              }
              content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
              
              plot[["convertible"]] <- TRUE
              plot[["obj"]] <- content[["obj"]]
              plot[["data"]] <- content[["png"]]
              
            })
            
            if (class(p) == "try-error") {
              
              errorMessage <- .extractErrorMessage(p)
              
              if (errorMessage == "not enough data") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              } else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              }
              
              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
            }
            
          } else {
            
            plot[["error"]] <- list(error="badData", errorMessage= plottingError[i])
          }
          
          plot[["status"]] <- "complete"
          
          plots.ttest[[z]] <- plot
        }
        
        plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[z]]
        
        results[["inferentialPlots"]][["collection"]] <- plotGroups
        
        z <- z + 1
        
        if ( ! .shouldContinue(callback(results)))
          return()
      }
      
      if (options$plotSequentialAnalysis) {
        
        if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) &&
            options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it
          
          index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")
          
          plots.ttest[[z]] <- state$plotsTtest[[index]]
          
        } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                  (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                     diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                     diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                     diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                     diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                     diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                                     diff$informativeNormalStd == FALSE))) &&
                   !options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {
          
          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists use it
          
          index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")
          
          plots.ttest[[z]] <- state$plotsTtest[[index]]
          
        } else {
          
          results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "running"
          
          if ( ! .shouldContinue(callback(results)))
            return()
          
          plot <- plots.ttest[[z]]
          
          if (options$plotSequentialAnalysisRobustness && options$effectSizeStandardized == "informative") {
            plot[["error"]] <- list(error="badData", errorMessage="Sequential analysis robustness check plot currently not supported for informed prior.")
          } else if (status[i] != "error" && status[i] != "sequentialNotPossible") {
            
            p <- try(silent= FALSE, expr= {

              .plotFunc <- function() {
                .plotSequentialBF.ttest (x= variableData, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0, BF10post=BF10post[i],
                                         plotDifferentPriors= options$plotSequentialAnalysisRobustness, options = options)
              }
              content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
              
              plot[["convertible"]] <- TRUE
              plot[["obj"]] <- content[["obj"]]
              plot[["data"]] <- content[["png"]]
              
            })
            
            if (class(p) == "try-error") {
              
              errorMessage <- .extractErrorMessage(p)
              
              if (errorMessage == "not enough data") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              } else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
                
                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
              }
              
              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
            }
            
          } else {
            
            plot[["error"]] <- list(error="badData", errorMessage=plottingError[i])
          }
          
          plot[["status"]] <- "complete"
          
          plots.ttest[[z]] <- plot
        }
        
        plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[z]]
        
        results[["inferentialPlots"]][["collection"]] <- plotGroups
        
        z <- z + 1
        
        if ( ! .shouldContinue(callback(results)))
          return()
      }
      
      
      i <- i + 1
    }
  }
  
  keep <- NULL
  
  for (plot in plots.ttest)
    keep <- c(keep, plot$data)
  
  for (plot in descriptivesPlots)
    keep <- c(keep, plot$data)
  
  if (perform == "init") {
    
    return(list(results=results, status="inited", state=state, keep=keep))
    
  } else {
    
    return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes,
                                                               plotVariables=plotVariables, descriptPlotVariables=descriptPlotVariables,
                                                               descriptivesPlots=descriptivesPlots, status=status, plottingError=plottingError,
                                                               BF10post=BF10post, errorFootnotes=errorFootnotes, tValue = tValue, n = n), keep=keep))
  }
  
}

.oneSidedTtestBFRichard <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, iterations=10000) {
  
  # sample from delta posterior
  samples <- BayesFactor::ttestBF(x=x, y=y, paired=paired, posterior=TRUE, iterations=iterations, rscale=r)
  
  if (is.null(y) || paired) {
    
    N <- length(x)
    varBeta <- samples[,'sig2'] / (1 * N + 1/samples[,'g'])
    
    if (paired) {
      
      meanBeta <- sum(x - y) * varBeta / samples[,'sig2']
      
    } else {
      
      meanBeta <- sum(x) * varBeta / samples[,'sig2']
    }
    
  } else {
    
    sumN <- length(y) + length(x)
    diffN <- length(y) - length(x)
    
    varBeta <- samples[,'sig2'] / (sumN/4 + 1/samples[,'g'])
    
    meanBeta <- varBeta / samples[,'sig2'] * ((sum(x) - sum(y)) + samples[,'mu'] * (diffN)) / 2
  }
  
  logProbMin <- BayesFactor::logSummaryStats(pnorm(0, meanBeta, sqrt(varBeta), log=TRUE))$logMean
  
  BF <- BayesFactor::ttestBF(x, y, paired=paired, rscale=r)
  BF10 <- BayesFactor::extractBF(BF, onlybf = TRUE, logbf=TRUE)
  
  if (oneSided == "right") {
    
    logProbPlus = pexp(-logProbMin, log=TRUE)
    BFplus1 = log(2) + logProbPlus
    BFplus0 <- BFplus1 + BF10
    
    return(exp(BFplus0))
    
  } else if (oneSided == "left") {
    
    BFmin1 <- log(2) + logProbMin
    BFmin0 <- BFmin1 + BF10
    return(exp(BFmin0))
  }
}

.oneSidedTtestBFRichardAdaptive <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, nTests=5, nIterations=2000, criterion=.02, nMaxIterations=2050000) {
  
  variability <- criterion + 1
  
  while (variability > criterion && nIterations < nMaxIterations) {
    
    BF <- numeric(nTests)
    
    for (i in seq_len(nTests)) {
      
      BF[i] <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, r=r, oneSided = oneSided, iterations = nIterations)
      
    }
    
    variability <- sd(abs(log(BF))) / mean(abs(log(BF)))
    nIterations <- 2 * nIterations
    
  }
  
  return(mean(BF))
  
}

.likelihoodShiftedT <- function(par, data) {
  
  - sum(log( dt((data - par[1]) / par[2], par[3]) / par[2]))
  
}

.dposteriorShiftedT <- function(x, parameters, oneSided) {
  
  if (oneSided == FALSE) {
    
    dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]
    
  } else if (oneSided == "right") {
    
    ifelse (x >= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=FALSE) , 0 )
    
  } else if (oneSided == "left") {
    
    ifelse (x <= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE), 0)
    
  }
  
}

.qShiftedT <- function(q, parameters, oneSided) {
  
  if (oneSided == FALSE) {
    
    qt(q, df=parameters[3]) * parameters[2] + parameters[1]
    
  } else if (oneSided == "right") {
    
    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)
    
    qt(areaSmallerZero + q * (1 - areaSmallerZero), df=parameters[3]) * parameters[2] + parameters[1]
    
  } else if (oneSided == "left") {
    
    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)
    
    qt(q * areaSmallerZero, df=parameters[3]) * parameters[2] + parameters[1]
    
  }
}

# pdf cauchy prior
.dprior <- function(x, r, oneSided= oneSided){
  
  if (oneSided == "right") {
    
    y <- ifelse(x < 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }
  
  if (oneSided == "left") {
    
    y <- ifelse(x > 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }	else {
    
    return(1/(pi*r*(1+(x/r)^2)))
  }
}

.plotPosterior.ttest <- function(x= NULL, y= NULL, paired= FALSE, oneSided= FALSE, BF, BFH1H0, callback=function(...) 0, iterations= 10000, rscale= "medium", lwd= 2, cexPoints= 1.5,
                                 cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addInformation= TRUE, dontPlotData=FALSE) {
  
  if (addInformation) {
    
    par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
    
  } else {
    
    par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
  }
  
  if (dontPlotData) {
    
    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
    
    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
    
    mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
    mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
    
    return()
  }
  
  if (rscale == "medium") {
    r <- sqrt(2) / 2
  }
  if (rscale == "wide") {
    r <- 1
  }
  if (rscale == "ultrawide") {
    r <- sqrt(2)
  }
  if (mode(rscale) == "numeric") {
    r <- rscale
  }
  
  if (oneSided == FALSE) {
    nullInterval <- NULL
  }
  if (oneSided == "right") {
    nullInterval <- c(0, Inf)
  }
  if (oneSided == "left") {
    nullInterval <- c(-Inf, 0)
  }
  
  # sample from delta posterior
  samples <- BayesFactor::ttestBF(x=x, y=y, paired=paired, posterior = TRUE, iterations = iterations, rscale= r)
  
  delta <- samples[,"delta"]
  
  if ( ! .shouldContinue(callback()))
    return()
  
  # fit shifted t distribution
  if (is.null(y)) {
    
    deltaHat <- mean(x) / sd(x)
    N <- length(x)
    df <- N - 1
    sigmaStart <- 1 / N
    
  } else if (paired) {
    
    deltaHat <- mean(x - y) / sd(x - y)
    N <- length(x)
    df <- N - 1
    sigmaStart <- 1 / N
    
  } else if (!is.null(y) && !paired) {
    
    N1 <- length(x)
    N2 <- length(y)
    sdPooled <- sqrt(((N1 - 1) * var(x) + (N2 - 1) * var(y)) / (N1 + N2))
    deltaHat <- (mean(x) - mean(y)) / sdPooled
    df <- N1 + N2 - 2
    sigmaStart <- sqrt(N1 * N2 / (N1 + N2))
  }
  
  if (sigmaStart < .01)
    sigmaStart <- .01
  
  parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="BFGS")$par)
  
  if (class(parameters) == "try-error") {
    
    parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="Nelder-Mead")$par)
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  if (BFH1H0) {
    
    BF10 <- BF
    BF01 <- 1 / BF10
    
  } else {
    
    BF01 <- BF
    BF10 <- 1 / BF01
  }
  
  
  # set limits plot
  xlim <- vector("numeric", 2)
  
  if (oneSided == FALSE) {
    
    xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
    
    if (length(x) < 10) {
      
      if (addInformation) {
        
        stretch <- 1.52
      } else {
        
        stretch <- 1.4
      }
      
    } else {
      
      stretch <- 1.2
    }
    
  }
  
  if (oneSided == "right") {
    
    #if (length(delta[delta >= 0]) < 10)
    #	return("Plotting is not possible: To few posterior samples in tested interval")
    
    xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])
    
    if (any(is.na(xlim))) {
      
      xlim[1] <- min(-2, .qShiftedT(0.01, parameters, oneSided="right"))
      xlim[2] <- max(2, .qShiftedT(0.99, parameters, oneSided="right"))
      
    }
    
    stretch <- 1.32
  }
  
  if (oneSided == "left") {
    
    #if (length(delta[delta <= 0]) < 10)
    #	return("Plotting is not possible: To few posterior samples in tested interval")
    
    xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
    xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])
    
    if (any(is.na(xlim))) {
      
      xlim[1] <-  min(-2, .qShiftedT(0.01, parameters, oneSided="left"))
      xlim[2] <- max(2,.qShiftedT(0.99, parameters, oneSided="left"))
      
    }
    
    stretch <- 1.32
  }
  
  xticks <- pretty(xlim)
  
  ylim <- vector("numeric", 2)
  
  ylim[1] <- 0
  dmax <- optimize(function(x).dposteriorShiftedT(x, parameters=parameters, oneSided= oneSided), interval= range(xticks), maximum = TRUE)$objective
  ylim[2] <- max(stretch * .dprior(0,r, oneSided= oneSided), stretch * dmax)# get maximum density
  
  if ( ! .shouldContinue(callback()))
    return()
  
  # calculate position of "nice" tick marks and create labels
  yticks <- pretty(ylim)
  xlabels <- formatC(xticks, 1, format= "f")
  ylabels <- formatC(yticks, 1, format= "f")
  
  # compute 95% credible interval & median:
  if (oneSided == FALSE) {
    
    CIlow <- quantile(delta, probs = 0.025)[[1]]
    CIhigh <- quantile(delta, probs = 0.975)[[1]]
    medianPosterior <- median(delta)
    
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
      
      CIlow <- .qShiftedT(0.025, parameters, oneSided=FALSE)
      CIhigh <- .qShiftedT(0.975, parameters, oneSided=FALSE)
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided=FALSE)
    }
  }
  
  if (oneSided == "right") {
    
    CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
    CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
    medianPosterior <- median(delta[delta >= 0])
    
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
      
      CIlow <- .qShiftedT(0.025, parameters, oneSided="right")
      CIhigh <- .qShiftedT(0.975, parameters, oneSided="right")
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided="right")
    }
  }
  
  if (oneSided == "left") {
    
    CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
    CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
    medianPosterior <- median(delta[delta <= 0])
    
    if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
      
      CIlow <- .qShiftedT(0.025, parameters, oneSided="left")
      CIhigh <- .qShiftedT(0.975, parameters, oneSided="left")
      medianPosterior <- .qShiftedT(0.5, parameters, oneSided="left")
    }
    
  }
  
  posteriorLine <- .dposteriorShiftedT(x= seq(min(xticks), max(xticks),length.out = 1000), parameters=parameters, oneSided = oneSided)
  
  xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
  
  plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
  
  lines(seq(min(xticks), max(xticks),length.out = 1000),posteriorLine, lwd= lwd)
  lines(seq(min(xticks), max(xticks),length.out = 1000), .dprior(seq(min(xticks), max(xticks),length.out = 1000), r=r, oneSided= oneSided), lwd= lwd, lty=3)
  
  axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
  axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
  
  
  if (nchar(ylabels[length(ylabels)]) > 4) {
    
    mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
    
  } else if (nchar(ylabels[length(ylabels)]) == 4) {
    
    mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
    
  } else if (nchar(ylabels[length(ylabels)]) < 4) {
    
    mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
    
  }
  
  mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
  
  points(0, .dprior(0,r, oneSided= oneSided), col="black", pch=21, bg = "grey", cex= cexPoints)
  
  if (oneSided == FALSE) {
    
    heightPosteriorAtZero <- .dposteriorShiftedT(0, parameters=parameters, oneSided=oneSided)
    
  } else if (oneSided == "right") {
    
    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
    heightPosteriorAtZero <- posteriorLineLargerZero[1]
    
  } else if (oneSided == "left") {
    
    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
    heightPosteriorAtZero <- posteriorLineLargerZero[length(posteriorLineLargerZero)]
  }
  
  points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
  
  # 95% credible interval
  
  # enable plotting in margin
  par(xpd=TRUE)
  
  yCI <- grconvertY(dmax, "user", "ndc") + 0.04
  yCI <- grconvertY(yCI, "ndc", "user")
  
  arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
  
  medianText <- formatC(medianPosterior, digits= 3, format="f")
  
  if (addInformation) {
    
    # display BF10 value
    offsetTopPart <- 0.06
    
    yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
    
    xx <- min(xticks)
    
    if (BF10 >= 1000000 | BF01 >= 1000000) {
      
      BF10t <- formatC(BF10,3, format = "e")
      BF01t <- formatC(BF01,3, format = "e")
    }
    
    if (BF10 < 1000000 & BF01 < 1000000) {
      
      BF10t <- formatC(BF10,3, format = "f")
      BF01t <- formatC(BF01,3, format = "f")
    }
    
    if (oneSided == FALSE) {
      
      text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos = 4)
    }
    
    if (oneSided == "right") {
      
      text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos = 4)
    }
    
    if (oneSided == "left") {
      
      text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
      text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos = 4)
    }
    
    yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
    
    CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
    medianLegendText <- paste("median =", medianText)
    
    text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
    text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
    
    # probability wheel
    if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
      xx <- grconvertX(0.44, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 5) {
      xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 6) {
      xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 7) {
      xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 8) {
      xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) > 8) {
      xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
    }
    
    yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
    
    # make sure that colored area is centered
    radius <- 0.06 * diff(range(xticks))
    A <- radius^2 * pi
    alpha <- 2 / (BF01 + 1) * A / radius^2
    startpos <- pi/2 - alpha/2
    
    # draw probability wheel
    plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
    
    yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
    
    if (oneSided == FALSE) {
      
      text(xx, yy, "data|H1", cex= cexCI)
      text(xx, yy2, "data|H0", cex= cexCI)
    }
    
    if (oneSided == "right") {
      
      text(xx, yy, "data|H+", cex= cexCI)
      text(xx, yy2, "data|H0", cex= cexCI)
    }
    
    if (oneSided == "left") {
      
      text(xx, yy, "data|H-", cex= cexCI)
      text(xx, yy2, "data|H0", cex= cexCI)
    }
    
    # add legend
    CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
    
    medianLegendText <- paste("median =", medianText)
  }
  
  mostPosterior <- mean(delta > mean(range(xticks)))
  
  if (mostPosterior >= .5) {
    
    legendPosition <- min(xticks)
    legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1, x.intersp= .6, seg.len= 1.2)
  } else {
    
    legendPosition <- max(xticks)
    legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1,  x.intersp= .6, seg.len= 1.2)
  }
}

.plotSequentialBF.ttest <- function(x= NULL, y= NULL, paired= FALSE, BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4,
                                    cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.6, cexTextBF= 1.4, cexText=1.2, cexLegend= 1.2, cexEvidence= 1.6,	lwdAxis= 1.2, plotDifferentPriors= FALSE,
                                    BFH1H0= TRUE, dontPlotData= FALSE, level1=NULL, level2= NULL, subDataSet=NULL, options) {
  
  #### settings ####
  
  if (!plotDifferentPriors) {
    
    evidenceText <-  TRUE
  } else {
    
    evidenceText <-  FALSE
  }
  
  
  if (rscale == "medium") {
    
    r <- sqrt(2) / 2
  }
  
  if (rscale == "wide") {
    
    r <- 1
  }
  
  if (rscale == "ultrawide") {
    
    r <- sqrt(2)
  }
  
  if (mode(rscale) == "numeric") {
    
    r <- rscale
  }
  
  
  if (oneSided == FALSE) {
    
    nullInterval <- NULL
  }
  
  if (oneSided == "right") {
    
    nullInterval <- c(0, Inf)
  }
  
  if (oneSided == "left") {
    
    nullInterval <- c(-Inf, 0)
  }
  
  
  par(mar= c(5.6, 6, 7, 7) + 0.1, las=1)
  
  
  if (dontPlotData) {
    
    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
    
    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
    
    mtext("n", side = 1, cex = cexXlab, line= 2.5)
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
    
    return()
  }
  
  
  if (is.null(y) || paired) {
    
    BF10 <- vector("numeric", max(length(x), length(y)))
    BF10w <- vector("numeric", max(length(x), length(y)))
    BF10u <- vector("numeric", max(length(x), length(y)))
    
    idData <- 1
    
    if (is.null(y)) {
      
      ind <- which(x == x[1])
      idData <- sum((ind+1)-(1:(length(ind))) == 1)
      
    } else {
      
      idData <- 1
      
      
      for (i in 2:(min(c(length(x), length(y))))) {
        
        previous  <- c(x[i-1], y[i-1])
        
        if (all(c(x[i], y[i]) == previous)) {
          
          idData <- idData + 1
          
        } else if (x[i] == y[i]) {
          
          idData <- idData + 1
          
        } else {
          
          break
        }
      }
    }
    
    if ( ! .shouldContinue(callback()))
      return()
    
    BF10[1:idData] <- 1
    BF10w[1:idData] <- 1
    BF10u[1:idData] <- 1
    
    
    if (idData < length(x)) {
      
      i <- idData + 1
      
    } else {
      
      i <- idData
      
    }
    
    if (idData < length(y)) {
      
      j <- idData + 1
      
    } else {
      
      j <- idData
      
    }
    
    k <- idData + 1
    
    
    while ((i <= length(x) | j <= length(y)) & k <= length(BF10)) {
      
      bfObject <- .generalTtestBF(x = x[1:i], y = y[1:j], paired = paired, oneSided = oneSided, options = options)
      BF10[k] <- bfObject[["bf"]]
      # if (oneSided == FALSE) {
      # 
      # 	BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale=r, nullInterval = nullInterval)
      # 	BF10[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
      # 
      # } else {
      # 
      # 	BF10[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r=r, oneSided=oneSided)
      # }
      
      k <- k + 1
      
      if (i < length(x)) {
        
        i <- i + 1
      }
      if (j < length(y)) {
        
        j <- j + 1
      }
      
      if ( ! .shouldContinue(callback()))
        return()
    }
    
    
    BF10 <- BF10[is.finite(BF10)]
    
    if ( ! .shouldContinue(callback()))
      return()
    
    if (plotDifferentPriors) {
      
      if (idData < length(x)) {
        
        i <- idData + 1
        
      } else {
        
        i <- idData
        
      }
      
      if (idData < length(y)) {
        
        j <- idData + 1
        
      } else {
        
        j <- idData
        
      }
      
      k <- idData + 1
      
      
      while ((i <= length(x) | j <= length(y)) & k <= length(BF10u)) {
        
        if (oneSided == FALSE) {
          
          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale="ultrawide", nullInterval = nullInterval)
          BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
          
        } else {
          
          BF10u[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="ultrawide", oneSided=oneSided)
        }
        
        k <- k + 1
        
        if (i < length(x)) {
          
          i <- i + 1
        }
        if (j < length(y)) {
          
          j <- j + 1
        }
        
        if ( ! .shouldContinue(callback()))
          return()
      }
      
      
      BF10u <- BF10u[is.finite(BF10u)]
      
      if ( ! .shouldContinue(callback()))
        return()
      
      
      if (idData < length(x)) {
        
        i <- idData + 1
        
      } else {
        
        i <- idData
        
      }
      
      if (idData < length(y)) {
        
        j <- idData + 1
        
      } else {
        
        j <- idData
        
      }
      
      k <- idData + 1
      
      
      while ((i <= length(x) | j <= length(y)) & k <= length(BF10w)) {
        
        if (oneSided == FALSE) {
          
          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale= "wide", nullInterval = nullInterval)
          BF10w[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
          
        } else {
          
          BF10w[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="wide", oneSided=oneSided)
        }
        
        k <- k + 1
        
        if (i < length(x)) {
          
          i <- i + 1
        }
        if (j < length(y)) {
          
          j <- j + 1
        }
        
        if ( ! .shouldContinue(callback()))
          return()
      }
      
      BF10w <- BF10w[is.finite(BF10w)]
      
      if ( ! .shouldContinue(callback()))
        return()
      
    }
    
  } else if (!is.null(y) && !paired) {
    
    idData <- 1
    
    xx <- numeric()
    yy <- numeric()
    
    BF10 <- vector("numeric", nrow(subDataSet))
    BF10w <- vector("numeric", nrow(subDataSet))
    BF10u <- vector("numeric", nrow(subDataSet))
    
    for (i in seq_len(nrow(subDataSet))) {
      
      if (subDataSet[i, 2] == level1) {
        
        xx <- c(xx, subDataSet[i, 1])
        
      } else if (subDataSet[i, 2] == level2) {
        
        yy <- c(yy, subDataSet[i, 1])
        
      }
      
      if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
        
        bfObject <- .generalTtestBF(x = xx, y = yy, paired = paired, oneSided = oneSided, options = options)
        BF10[i] <- bfObject[["bf"]]
        # if (oneSided == FALSE) {
        # 
        # 	BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= r, nullInterval = nullInterval)
        # 	BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
        # 
        # } else if (oneSided == "right") {
        # 
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r=r)
        # 
        # } else if (oneSided == "left") {
        # 
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r=r)
        # }
        
      } else {
        
        BF10[i] <- 1
      }
    }
    
    
    if (plotDifferentPriors) {
      
      xx <- numeric()
      yy <- numeric()
      
      for (i in seq_len(nrow(subDataSet))) {
        
        if (subDataSet[i, 2] == level1) {
          
          xx <- c(xx, subDataSet[i, 1])
          
        } else if (subDataSet[i, 2] == level2) {
          
          yy <- c(yy, subDataSet[i, 1])
          
        }
        
        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
          
          if (oneSided == FALSE) {
            
            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "ultrawide", nullInterval = nullInterval)
            BF10u[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
            
          } else if (oneSided == "right") {
            
            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="ultrawide")
            
          } else if (oneSided == "left") {
            
            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="ultrawide")
          }
          
        } else {
          
          BF10u[i] <- 1
        }
      }
      
      xx <- numeric()
      yy <- numeric()
      
      for (i in seq_len(nrow(subDataSet))) {
        
        if (subDataSet[i, 2] == level1) {
          
          xx <- c(xx, subDataSet[i, 1])
          
        } else if (subDataSet[i, 2] == level2) {
          
          yy <- c(yy, subDataSet[i, 1])
          
        }
        
        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
          
          if (oneSided == FALSE) {
            
            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "wide", nullInterval = nullInterval)
            BF10w[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
            
          } else if (oneSided == "right") {
            
            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="wide")
            
          } else if (oneSided == "left") {
            
            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="wide")
          }
          
        } else {
          
          BF10w[i] <- 1
        }
      }
    }
  }
  
  ####################### scale y axis ###########################
  
  if (plotDifferentPriors) {
    
    BF <- c(BF10, BF10u, BF10w)
    
  } else {
    
    BF <- BF10
    
  }
  
  
  if (!BFH1H0) {
    
    BF <- 1 / BF
    BF10 <- 1 / BF10
    
    if (plotDifferentPriors) {
      
      BF10u  <- 1 / BF10u
      BF10w <- 1 / BF10w
    }
  }
  
  
  # y-axis labels larger than 1
  
  y1h <- "1"
  
  i <- 1
  
  while (eval(parse(text= y1h[i])) < max(BF)) {
    
    if (grepl(pattern = "e",y1h[i])) {
      
      newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y1h[i], "0", sep= "")
    }
    
    if (eval(parse(text=newy)) >= 10^6) {
      
      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }
    
    y1h <- c(y1h, newy)
    i <- i + 1
  }
  
  
  y3h <- "3"
  
  i <- 1
  
  while (eval(parse(text= y3h[i])) < max(BF)) {
    
    if (grepl(pattern = "e",y3h[i])) {
      
      newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y3h[i], "0", sep= "")
    }
    
    if (as.numeric(newy) >= 10^6) {
      
      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }
    
    y3h <- c(y3h, newy)
    
    i <- i + 1
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  yhigh <- vector("numeric", length(y1h) + length(y3h))
  
  o <- 1
  e <- 1
  
  for (i in seq_along(yhigh)) {
    
    if (i %% 2 == 1) {
      
      yhigh[i] <- y1h[o]
      o <- o + 1
    }
    
    if (i %% 2 == 0) {
      
      yhigh[i] <- y3h[e]
      e <- e + 1
    }
  }
  
  yhighLab <- as.character(yhigh)
  
  
  # y-axis labels smaller than 1
  
  y1l <- "1/1"
  
  i <- 1
  
  while (eval(parse(text= y1l[i])) > min(BF)) {
    
    if (grepl(pattern = "e",y1l[i])) {
      
      newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y1l[i], "0", sep= "")
    }
    
    if (eval(parse(text= newy)) <= 10^(-6)) {
      
      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }
    
    y1l <- c(y1l, newy)
    i <- i + 1
  }
  
  
  y3l <- "1/3"
  
  i <- 1
  
  while (eval(parse(text= y3l[i])) > min(BF)) {
    
    if (grepl(pattern = "e",y3l[i])) {
      
      newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y3l[i], "0", sep= "")
    }
    
    if (newy == "1/3e+9") {
      
      newy <- "1/3e+09"
    }
    
    if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {
      
      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
      newy <- sub(".33", "", newy)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }
    
    y3l <- c(y3l, newy)
    i <- i + 1
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  ylow <- vector("numeric", length(y1l) + length(y3l))
  o <- 1
  e <- 1
  
  for (i in seq_along(ylow)) {
    
    if (i %% 2 == 1) {
      
      ylow[i] <- y1l[o]
      o <- o + 1
    }
    
    if (i %% 2 == 0) {
      
      ylow[i] <- y3l[e]
      e <- e + 1
    }
  }
  
  yLab <- c(rev(ylow[-1]), yhighLab)
  
  
  # remove 3's if yLab vector is too long
  omit3s <- FALSE
  
  if (length(yLab) > 9) {
    
    omit3s <- TRUE
    
    ind <- which(yLab == "3")
    
    yLabsHigh <- yLab[ind:length(yLab)]
    
    if (length(yLabsHigh) > 1) {
      
      yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
    } else {
      
      yLabsHigh <- character(0)
    }
    
    yLabsLow <- yLab[1:(ind-1)]
    yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
    
    yLab1s <- c(yLabsLow, yLabsHigh)
    
    
    if (max(BF) > eval(parse(text= yLab1s[length(yLab1s)]))) {
      
      for (i in 1:2) {
        
        if(grepl(pattern = "e",yLab1s[length(yLab1s)])){
          
          newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {
          
          newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
        }
        
        if (eval(parse(text=newy)) >= 10^6) {
          
          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        }
        
        yLab1s <- c(yLab1s, newy)
      }
    }
    
    
    if (yLab1s[1] == "1") {
      
      yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
    }
    
    if (yLab1s[length(yLab1s)] == "1") {
      
      yLab1s <- c(yLab1s, "10")
    }
    
    if (min(BF) < eval(parse(text= yLab1s[1]))) {
      
      for (i in 1:2) {
        
        if (grepl(pattern = "e",yLab1s[1])) {
          
          newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {
          
          newy <- paste(yLab1s[1], "0", sep= "")
        }
        
        if (eval(parse(text= newy)) <= 10^(-6)) {
          
          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
          newy <-  sub("-", "+", x = newy)
          newy <- substring(newy, nchar(newy)-4, nchar(newy))
          newy <- paste0("1/", newy)
        }
      }
      
      yLab1s <- c(newy, yLab1s)
    }
    
    yLab <- yLab1s
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  while (length(yLab) > 9) {
    
    ind <- which(yLab == "1")
    
    if (ind == 1) {
      
      yLabLow <- character(0)
    } else {
      
      yLabLow <- yLab[1:(ind-1)]
    }
    
    if (ind == length(yLab)) {
      
      yLabHigh <- character(0)
    } else {
      
      yLabHigh <- yLab[(ind+1):length(yLab)]
    }
    
    if (length(yLabLow) > 1) {
      
      yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
    } else {
      
      yLabLow <- yLabLow
    }
    
    
    if (length(yLabHigh) > 1) {
      
      yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
    } else {
      
      yLabHigh <- yLabHigh
    }
    
    if (length(yLabLow) == 1) {
      
      yLabLow <- paste("1/", yLabHigh[1], sep="")
    }
    
    if (length(yLabHigh) == 1) {
      
      yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
    }
    
    yLab <- c(rev(yLabLow), "1", yLabHigh)
  }
  
  
  while (eval(parse(text=yLab[1])) > min(BF)) {
    
    for (i in 1:2) {
      
      interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
      pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
      
      newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
      yLab <- c(newy, yLab)
    }
  }
  
  while (eval(parse(text=yLab[length(yLab)])) < max(BF)) {
    
    for (i in 1:2) {
      
      interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed= TRUE)[[1]][2])
      pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
      newy <- paste(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
      yLab <- c( yLab, newy)
    }
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  yAt <- vector("numeric", length(yLab))
  
  for (i in seq_along(yLab)) {
    
    yAt[i] <- log(eval(parse(text= yLab[i])))
  }
  
  
  ####################### plot ###########################
  
  xLab <- pretty(c(0, length(BF10)+2))
  xlim <- range(xLab)
  ylow <- log(eval(parse(text= yLab[1])))
  yhigh <- log(eval(parse(text= yLab[length(yLab)])))
  
  if (is.infinite(yhigh)) {
    
    yhigh <- 1e+308
  }
  
  
  ylim <- c(ylow, yhigh)
  
  plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
  
  
  for (i in seq_along(yAt)) {
    
    lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
  }
  
  lines(xlim, rep(0, 2), lwd= lwd)
  
  axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
  axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)
  
  # enable plotting in margin
  par(xpd= TRUE)
  xx <- grconvertX(0.79, "ndc", "user")
  
  yAthigh <- yAt[yAt >= 0]
  
  if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {
    
    for (i in 1:(length(yAthigh)-1)) {
      yy <- mean(c(yAthigh[i], yAthigh[i+1]))
      
      if (yAthigh[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }
    
    yAtlow <- rev(yAt[yAt <= 0])
    
    for (i in 1:(length(yAtlow)-1)) {
      
      yy <- mean(c(yAtlow[i], yAtlow[i+1]))
      
      if (yAtlow[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }
    
    if ( ! .shouldContinue(callback()))
      return()
    
    axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
    
    xx <- grconvertX(0.96, "ndc", "user")
    yy <- grconvertY(0.5, "npc", "user")
    
    text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
  }
  
  if (omit3s) {
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }
  }
  
  if (omit3s == FALSE) {
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
  }
  
  mtext("n", side = 1, cex = cexXlab, line= 2.5)
  
  xx <- grconvertX(0.1, "npc", "user")
  yy1 <- yAt[length(yAt)-1]
  yy2 <- yAt[length(yAt)]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
  
  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
  
  xxt <- grconvertX(0.28, "npc", "user")
  
  if (oneSided == FALSE) {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  if (oneSided == "right") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  if (oneSided == "left") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  
  yy1 <- yAt[2]
  yy2 <- yAt[1]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
  
  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
  
  if (oneSided == FALSE) {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
    }
  }
  
  if (oneSided == "right"){
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
    }
  }
  
  if (oneSided == "left") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
    }
  }
  
  
  # display BF10 value
  if (idData < length(BF10)) {
    
    BF10e <- BF10post
    
  } else {
    
    BF10e <- 1
  }
  
  if (BFH1H0) {
    
    BF01e <- 1 / BF10e
    
  } else {
    
    BF01e <- BF10e
    BF10e <- 1 / BF01e
  }
  
  # display BF10 value
  
  offsetTopPart <- 0.06
  
  xx <- min(xLab)
  yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
  yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
  
  if (BF10e >= 1000000 | BF01e >= 1000000) {
    
    BF10t <- formatC(BF10e,3, format = "e")
    BF01t <- formatC(BF01e,3, format = "e")
  }
  
  if (BF10e < 1000000 & BF01e < 1000000) {
    
    BF10t <- formatC(BF10e, 3, format = "f")
    BF01t <- formatC(BF01e, 3, format = "f")
  }
  
  if (oneSided == FALSE) {
    
    text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }
  
  if (oneSided == "right") {
    
    text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }
  
  if (oneSided == "left") {
    
    text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }
  
  
  # probability wheel
  
  if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
    xx <- grconvertX(0.44, "ndc", "user")
  }
  
  if (max(nchar(BF10t), nchar(BF01t)) == 5) {
    xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
  }
  
  if (max(nchar(BF10t), nchar(BF01t)) == 6) {
    xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user")
  }
  
  if (max(nchar(BF10t), nchar(BF01t)) == 7) {
    xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }
  
  if (max(nchar(BF10t), nchar(BF01t)) == 8) {
    xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }
  
  if (max(nchar(BF10t), nchar(BF01t)) > 8) {
    xx <- grconvertX(0.445 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }
  
  yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
  
  
  # make sure that colored area is centered
  
  radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", "user")
  A <- radius^2*pi
  alpha <- 2 / (BF01e + 1) * A / radius^2
  startpos <- pi/2 - alpha/2
  
  if ( ! .shouldContinue(callback()))
    return()
  
  # draw probability wheel
  
  plotrix::floating.pie(xx, yy,c(BF10e, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
  
  yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
  yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
  
  if (oneSided == FALSE) {
    
    text(xx, yy, "data|H1", cex= 1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }
  
  if (oneSided == "right") {
    
    text(xx, yy, "data|H+", cex=  1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }
  
  if (oneSided == "left") {
    
    text(xx, yy, "data|H-", cex=  1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }
  
  if (length(BF10) <= 60) {
    
    points(log(BF10), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
  } else {
    
    lines(log(BF10), col="black", lwd = 2.7) # user prior
  }
  
  if (plotDifferentPriors) {
    
    if (length(BF10) <= 60) {
      
      points(log(BF10u), pch=21, bg= "white", cex= 0.7, lwd= 1.3) # "ultrawide" prior
      points(log(BF10w), pch=21, bg= "black", cex= 0.7, lwd= 1.3) # "wide" prior
      
    } else {
      
      greycol <- rgb(0,0,0, alpha=0.95)
      greycol2 <- rgb(0,0,0, alpha=0.5)
      lines(log(BF10u), col= greycol2, cex= 0.7, lwd= 1.3, lty= 1) # "ultrawide" prior
      lines(log(BF10w), col= greycol, cex= 0.7, lwd= 1.3, lty=3) # "wide" prior
    }
  }
  
  BFevidence <- BF10e
  
  if (evidenceText) {
    
    if (BF10e < 1) {
      BFevidence <- 1 / BF10e
    }
    if (BFevidence >= 1 & BFevidence <= 3) {
      lab <- "Anecdotal"
    }
    if (BFevidence > 3 & BFevidence <= 10) {
      lab <- "Moderate"
    }
    if (BFevidence > 10 & BFevidence <= 30) {
      lab <- "Strong"
    }
    if (BFevidence > 30 & BFevidence <= 100) {
      lab <- "Very strong"
    }
    if (BFevidence > 100) {
      lab <- "Extreme"
    }
    xxT <- max(xLab)
    yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
    
    if (BF10e >= 1) {
      
      if (oneSided == FALSE) {
        text(xxT, yyT, paste("Evidence for H1:\n", lab), cex= 1.4, pos= 2, offset= -.2)
      }
      if (oneSided == "right") {
        text(xxT, yyT, paste("Evidence for H+:\n", lab), cex= 1.4, pos= 2, offset= -.2)
      }
      if (oneSided == "left") {
        text(xxT, yyT, paste("Evidence for H-:\n", lab), cex= 1.4, pos= 2, offset= -.2)
      }
    }
    
    if (BF10e < 1) {
      text(xxT, yyT, paste("Evidence for H0:\n", lab), cex= 1.4, pos= 2, offset= -.2)
    }
    
  } else {
    
    # add legend
    xx <- grconvertX(0.56, "ndc", "user")
    yy <- grconvertY(0.872 + offsetTopPart, "ndc", "user")
    
    BFind <- sort(c(BF10[length(x)], BF10u[length(x)], BF10w[length(x)]), decreasing = TRUE, index.return=TRUE)$ix
    legend <- c("user prior", "ultrawide prior", "wide prior")
    
    if (length(BF10) <= 60) {
      
      pt.bg <-  c("grey", "white", "black")
      pt.cex <-  c(cexPoints, 0.7, 0.7)
      legend(xx, yy, legend = legend[BFind], pch=rep(21,3), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=rep(1.3,3), pt.cex= pt.cex[BFind])
    } else {
      
      xx <- grconvertX(0.55, "ndc", "user")
      lty <- c(1, 1, 3)
      lwd <- c(2.7, 1.3, 1.3)
      col <- c("black", greycol2, greycol)
      legend(xx, yy, legend = legend[BFind], lty= lty[BFind], bty= "n", cex= cexLegend, lwd= lwd[BFind], col= col[BFind], seg.len= .7)
    }
  }
}


.plotBF.robustnessCheck.ttest <- function(x= NULL, y= NULL, paired= FALSE, BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2,
                                          cexYXlab= 1.5,  cexText=1.2, cexLegend= 1.4, lwdAxis= 1.2, cexEvidence= 1.6, BFH1H0 = TRUE, dontPlotData= FALSE) {
  
  
  #### settings ####
  if (rscale == "medium") {
    r <- sqrt(2) / 2
  }
  if (rscale == "wide") {
    r <- 1
  }
  if (rscale == "ultrawide") {
    r <- sqrt(2)
  }
  if (mode(rscale) == "numeric") {
    r <- rscale
  }
  
  if (oneSided == FALSE) {
    nullInterval <- NULL
  }
  if (oneSided == "right") {
    nullInterval <- c(0, Inf)
  }
  if (oneSided == "left") {
    nullInterval <- c(-Inf, 0)
  }
  
  
  par(mar= c(5, 6, 6, 7) + 0.1, las=1)
  
  if (dontPlotData) {
    
    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
    
    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
    
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
    
    mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)
    
    return()
  }
  
  #### get BFs ###
  if(r > 1.5)
  {
    rValues <- seq(0.0005, 2, length.out = 535)
  }
  else
  {
    rValues <- seq(0.0005, 1.5, length.out = 400)
  }
  
  # BF10
  BF10 <- vector("numeric", length(rValues))
  
  for (i in seq_along(rValues)) {
    
    if (oneSided == FALSE) {
      
      BF <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval=nullInterval, rscale=rValues[i])
      BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
      
    } else {
      
      BF10[i] <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r=rValues[i])
    }
    
    if ( ! .shouldContinue(callback()))
      return()
  }
  
  # BF10 "medium" prior
  if (oneSided == FALSE) {
    
    BF10m <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval=nullInterval, rscale= "medium")
    BF10m <- BayesFactor::extractBF(BF10m, logbf = FALSE, onlybf = F)[1, "bf"]
    
  } else {
    
    BF10m <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="medium")
  }
  
  BF10mText <- BF10m
  
  # BF10 "wide" prior
  if (oneSided == FALSE) {
    
    BF10w <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "wide")
    BF10w <- BayesFactor::extractBF(BF10w, logbf = FALSE, onlybf = F)[1, "bf"]
    
  } else {
    
    BF10w <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="wide")
  }
  
  BF10wText <- BF10w
  
  # BF10 "ultrawide" prior
  if (oneSided == FALSE) {
    
    BF10ultra <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "ultrawide")
    BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, onlybf = F)[1, "bf"]
    
  } else {
    
    BF10ultra <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="ultrawide")
  }
  
  BF10ultraText <- BF10ultra
  
  # BF10 user prior
  BF10user <- BF10post
  BF10userText <- BF10user
  
  if ( ! .shouldContinue(callback()))
    return()
  
  ####################### scale y axis ###########################
  
  BF <- c(BF10, BF10m, BF10w, BF10ultra, BF10user)
  
  if (!BFH1H0) {
    
    BF <- 1 / BF
    BF10 <- 1 / BF10
    BF10m  <- 1 / BF10m
    BF10w <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
    # BF10user <- 1 / BF10user
  }
  
  # y-axis labels larger than 1
  y1h <- "1"
  i <- 1
  
  while (eval(parse(text= y1h[i])) < max(BF10)) {
    
    if (grepl(pattern = "e",y1h[i])) {
      
      newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y1h[i], "0", sep= "")
    }
    
    if (eval(parse(text=newy)) >= 10^6) {
      
      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }
    
    y1h <- c(y1h, newy)
    i <- i + 1
  }
  
  y3h <- "3"
  i <- 1
  
  while (eval(parse(text= y3h[i])) < max(BF10)) {
    
    if (grepl(pattern = "e",y3h[i])) {
      
      newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y3h[i], "0", sep= "")
    }
    
    if (as.numeric(newy) >= 10^6) {
      
      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }
    
    y3h <- c(y3h, newy)
    i <- i + 1
  }
  
  yhigh <- vector("numeric", length(y1h) + length(y3h))
  o <- 1
  e <- 1
  
  for (i in seq_along(yhigh)) {
    
    if (i %% 2 == 1) {
      
      yhigh[i] <- y1h[o]
      o <- o + 1
    }
    
    if (i %% 2 == 0) {
      
      yhigh[i] <- y3h[e]
      e <- e + 1
    }
  }
  
  yhighLab <- as.character(yhigh)
  
  # y-axis labels smaller than 1
  y1l <- "1/1"
  i <- 1
  
  while (eval(parse(text= y1l[i])) > min(BF10)) {
    
    if (grepl(pattern = "e",y1l[i])) {
      
      newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y1l[i], "0", sep= "")
    }
    
    if (eval(parse(text= newy)) <= 10^(-6)) {
      
      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }
    
    y1l <- c(y1l, newy)
    i <- i + 1
  }
  
  y3l <- "1/3"
  i <- 1
  
  while (eval(parse(text= y3l[i])) > min(BF10)) {
    
    if (grepl(pattern = "e",y3l[i])) {
      
      newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {
      
      newy <- paste(y3l[i], "0", sep= "")
    }
    
    if (newy == "1/3e+9") {
      newy <- "1/3e+09"
    }
    
    if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {
      
      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
      newy <- sub(".33", "", newy)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }
    
    y3l <- c(y3l, newy)
    i <- i + 1
  }
  
  ylow <- vector("numeric", length(y1l) + length(y3l))
  o <- 1
  e <- 1
  
  if ( ! .shouldContinue(callback()))
    return()
  
  for (i in seq_along(ylow)) {
    
    if (i %% 2 == 1) {
      ylow[i] <- y1l[o]
      o <- o + 1
    }
    if (i %% 2 == 0) {
      ylow[i] <- y3l[e]
      e <- e + 1
    }
  }
  
  yLab <- c(rev(ylow[-1]), yhighLab)
  
  # remove 3's if yLab vector is too long
  omit3s <- FALSE
  
  if (length(yLab) > 9) {
    
    omit3s <- TRUE
    
    ind <- which(yLab == "3")
    
    yLabsHigh <- yLab[ind:length(yLab)]
    
    if (length(yLabsHigh) > 1) {
      
      yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
    } else {
      
      yLabsHigh <- character(0)
    }
    
    yLabsLow <- yLab[1:(ind-1)]
    yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
    
    yLab1s <- c(yLabsLow, yLabsHigh)
    
    if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)]))) {
      
      for (i in 1:2) {
        
        if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
          
          newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
                                                                                                                     split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {
          
          newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
        }
        
        if (eval(parse(text=newy)) >= 10^6) {
          
          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        }
        
        yLab1s <- c(yLab1s, newy)
      }
    }
    
    if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)-1]))) {
      
      if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
        
        newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
                                                                                                                   split = "+", fixed=TRUE)[[1]][2])+1, sep="")
      } else {
        
        newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
      }
      
      if (eval(parse(text=newy)) >= 10^6) {
        
        newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      }
      
      yLab1s <- c(yLab1s, newy)
    }
    
    if (yLab1s[1] == "1") {
      
      yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
    }
    if (yLab1s[length(yLab1s)] == "1") {
      
      yLab1s <- c(yLab1s, "10")
    }
    
    if (min(BF10) < eval(parse(text= yLab1s[1]))) {
      
      for (i in 1:2) {
        
        if (grepl(pattern = "e",yLab1s[1])) {
          
          newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {
          
          newy <- paste(yLab1s[1], "0", sep= "")
        }
        
        if (eval(parse(text= newy)) <= 10^(-6)) {
          
          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
          newy <-  sub("-", "+", x = newy)
          newy <- substring(newy, nchar(newy)-4, nchar(newy))
          newy <- paste0("1/", newy)
        }
      }
      
      yLab1s <- c(newy, yLab1s)
    }
    
    if (min(BF10) < eval(parse(text= yLab1s[2]))) {
      
      if (grepl(pattern = "e",yLab1s[1])) {
        
        newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
      } else {
        
        newy <- paste(yLab1s[1], "0", sep= "")
      }
      
      if (eval(parse(text= newy)) <= 10^(-6)) {
        
        newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        newy <-  sub("-", "+", x = newy)
        newy <- substring(newy, nchar(newy)-4, nchar(newy))
        newy <- paste0("1/", newy)
      }
      
      
      yLab1s <- c(newy, yLab1s)
    }
    
    yLab <- yLab1s
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  while (length(yLab) > 9) {
    
    ind <- which(yLab == "1")
    
    if (ind == 1) {
      
      yLabLow <- character(0)
    } else {
      
      yLabLow <- yLab[1:(ind-1)]
    }
    
    if (ind == length(yLab)) {
      
      yLabHigh <- character(0)
    } else {
      
      yLabHigh <- yLab[(ind+1):length(yLab)]
    }
    
    if (length(yLabLow) > 1) {
      
      yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
    } else {
      
      yLabLow <- yLabLow
    }
    
    
    if (length(yLabHigh) > 1) {
      
      yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
    } else {
      
      yLabHigh <- yLabHigh
    }
    
    if (length(yLabLow) == 1) {
      
      yLabLow <- paste("1/", yLabHigh[1], sep="")
    }
    if (length(yLabHigh) == 1) {
      
      yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
    }
    
    yLab <- c(rev(yLabLow), "1", yLabHigh)
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  
  while (eval(parse(text=yLab[2])) > min(BF10)) {
    
    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[2])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2])
    pot <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) + interval
    
    if (nchar(pot) == 1)
      pot <- paste("0", pot, sep="")
    
    newy <- paste("1/1e", "+", pot, sep="")
    yLab <- c(newy, yLab)
    
    # interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
    # pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
    #
    # newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
    # yLab <- c(newy, yLab)
  }
  
  
  while (eval(parse(text=yLab[length(yLab)-1])) < max(BF10)) {
    
    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)-1])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2])
    # pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
    pot <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) + interval
    
    if (nchar(pot) == 1)
      pot <- paste("0", pot, sep="")
    
    newy <- paste(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
    yLab <- c( yLab, newy)
  }
  
  
  yAt <- vector("numeric", length(yLab))
  
  for (i in seq_along(yLab)) {
    
    yAt[i] <- log(eval(parse(text= yLab[i])))
  }
  
  
  ####################### plot ###########################
  
  if(r > 1.5)
  {
    xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
  }
  else
  {
    xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
  }
  
  xlim <- range(xLab)
  ylow <- log(eval(parse(text= yLab[1])))
  yhigh <- log(eval(parse(text= yLab[length(yLab)])))
  ylim <- c(ylow, yhigh)
  
  plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
  
  
  for (i in seq_along(yAt)) {
    
    lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
  }
  
  lines(xlim, rep(0, 2), lwd= lwd)
  
  axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
  axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)
  
  # enable plotting in margin
  par(xpd= TRUE)
  xx <- grconvertX(0.79, "ndc", "user")
  
  yAthigh <- yAt[yAt >= 0]
  
  if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {
    
    for (i in 1:(length(yAthigh)-1)) {
      yy <- mean(c(yAthigh[i], yAthigh[i+1]))
      
      if (yAthigh[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }
    
    yAtlow <- rev(yAt[yAt <= 0])
    
    for (i in 1:(length(yAtlow)-1)) {
      
      yy <- mean(c(yAtlow[i], yAtlow[i+1]))
      
      if (yAtlow[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }
    
    axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
    
    xx <- grconvertX(0.96, "ndc", "user")
    yy <- grconvertY(0.5, "npc", "user")
    text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
  }
  
  if (omit3s) {
    
    if (eval(parse(text= yLab[1])) <= 1/10^6) {
      
      line <- 4.75
      
    } else {
      
      line <- 4.3
    }
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }
  }
  
  if (omit3s == FALSE) {
    
    if (oneSided == FALSE) {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
    
    if (oneSided == "right") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
    
    if (oneSided == "left") {
      
      if (BFH1H0) {
        
        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {
        
        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
  }
  
  mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)
  
  xx <- grconvertX(0.1, "npc", "user")
  yy1 <- yAt[length(yAt)-1]
  yy2 <- yAt[length(yAt)]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
  
  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
  
  xxt <- grconvertX(0.28, "npc", "user")
  
  if (oneSided == FALSE) {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  if (oneSided == "right") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  if (oneSided == "left") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    }
  }
  
  yy1 <- yAt[2]
  yy2 <- yAt[1]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
  
  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
  
  if (oneSided == FALSE) {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
    }
  }
  
  if (oneSided == "right") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
    }
  }
  
  if (oneSided == "left") {
    
    if (BFH1H0) {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
    } else {
      
      text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
    }
  }
  
  if ( ! .shouldContinue(callback()))
    return()
  
  # display BF10
  lines(rValues,log(BF10), col="black", lwd = 2.7)
  
  # display "wide", user, and "ultrawide" prior BFs
  points(r, log(BF10user), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
  points(1, log(BF10w), pch=21, bg= "black", cex= 1.1, lwd= 1.3) # "wide" prior
  points(sqrt(2), log(BF10ultra), pch=21, bg= "white", cex= 1.1, lwd= 1.3) # "ultrawide" prior
  
  #### add legend
  # BF values
  
  # BFuser
  
  if (BFH1H0) {
    
    BF01userText <- 1 / BF10userText
    
  } else {
    
    BF10userText <- 1 / BF10userText
    BF01userText <- 1 / BF10userText
  }
  
  if (BF10userText >= 1000000 | BF01userText >= 1000000) {
    
    BF10usert <- format(BF10userText, digits= 4, scientific = TRUE)
    BF01usert <- format(BF01userText, digits= 4, scientific = TRUE)
  }
  if (BF10userText < 1000000 & BF01userText < 1000000) {
    
    BF10usert <- formatC(BF10userText, 3, format = "f")
    BF01usert <- formatC(BF01userText, 3, format = "f")
  }
  
  if (oneSided == FALSE) {
    
    if( BF10userText >= BF01userText) {
      userBF <- bquote(BF[10]==.(BF10usert))
    } else {
      userBF <- bquote(BF[0][1]==.(BF01usert))
    }
  }
  if (oneSided == "right") {
    
    if (BF10userText >= BF01userText) {
      userBF <- bquote(BF["+"][0]==.(BF10usert))
    } else {
      userBF <- bquote(BF[0]["+"]==.(BF01usert))
    }
  }
  if (oneSided == "left") {
    
    if (BF10userText >= BF01userText) {
      userBF <- bquote(BF["-"][0]==.(BF10usert))
    } else {
      userBF <- bquote(BF[0]["-"]==.(BF01usert))
    }
  }
  
  # BFwide
  BF01wText <- 1 / BF10wText
  
  if (BF10wText >= 1000000 | BF01wText >= 1000000) {
    BF10wt <- format(BF10wText, digits= 4, scientific = TRUE)
    BF01wt <- format(BF01wText, digits= 4, scientific = TRUE)
  }
  if (BF10wText < 1000000 & BF01wText < 1000000) {
    BF10wt <- formatC(BF10wText, 3, format = "f")
    BF01wt <- formatC(BF01wText, 3, format = "f")
  }
  
  if (oneSided == FALSE) {
    
    if (BF10wText >= BF01wText) {
      wBF <- bquote(BF[10]==.(BF10wt))
    } else {
      wBF <- bquote(BF[0][1]==.(BF01wt))
    }
  }
  if (oneSided == "right") {
    
    if (BF10wText >= BF01wText) {
      wBF <- bquote(BF["+"][0]==.(BF10wt))
    } else {
      wBF <- bquote(BF[0]["+"]==.(BF01wt))
    }
  }
  if (oneSided == "left") {
    
    if (BF10wText >= BF01wText) {
      wBF <- bquote(BF["-"][0]==.(BF10wt))
    } else {
      wBF <- bquote(BF[0]["-"]==.(BF01wt))
    }
  }
  
  # BFultrawide
  BF01ultraText <- 1 / BF10ultraText
  
  if (BF10ultraText >= 1000000 | BF01ultraText >= 1000000) {
    
    BF10ultrat <- format(BF10ultraText, digits= 4, scientific = TRUE)
    BF01ultrat <- format(BF01ultraText, digits= 4, scientific = TRUE)
  }
  if (BF10ultraText < 1000000 & BF01ultraText < 1000000) {
    
    BF10ultrat <- formatC(BF10ultraText, 3, format = "f")
    BF01ultrat <- formatC(BF01ultraText, 3, format = "f")
  }
  
  if (oneSided == FALSE) {
    
    if (BF10ultraText >= BF01ultraText) {
      ultraBF <- bquote(BF[10]==.(BF10ultrat))
    } else {
      ultraBF <- bquote(BF[0][1]==.(BF01ultrat))
    }
  }
  
  if (oneSided == "right") {
    
    if (BF10ultraText >= BF01ultraText) {
      ultraBF <- bquote(BF["+"][0]==.(BF10ultrat))
    } else{
      ultraBF <- bquote(BF[0]["+"]==.(BF01ultrat))
    }
  }
  
  if (oneSided == "left") {
    
    if (BF10ultraText >= BF01ultraText) {
      ultraBF <- bquote(BF["-"][0]==.(BF10ultrat))
    } else {
      ultraBF <- bquote(BF[0]["-"]==.(BF01ultrat))
    }
  }
  
  xx <- grconvertX(0.2, "ndc", "user")
  yy <- grconvertY(0.965, "ndc", "user")
  
  BFind <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, index.return=TRUE)$ix
  BFsort <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, index.return=TRUE)$x
  
  legend <- c("user prior:", "ultrawide prior:", "wide prior:")
  pt.bg <-  c("grey", "white", "black")
  pt.cex <-  c(cexPoints, 1.1, 1.1)
  
  legend(xx, yy, legend = legend[BFind], pch=rep(21,3), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=rep(1.3,3), pt.cex= pt.cex[BFind])
  
  xx <- grconvertX(0.5, "ndc", "user")
  y1 <- grconvertY(0.902, "ndc", "user")
  y2 <- grconvertY(0.852, "ndc", "user")
  y3 <- grconvertY(0.802, "ndc", "user")
  yy <- c(y1, y2, y3)
  
  text(xx, yy[BFsort== BF10userText], userBF, cex= 1.3,pos = 4)
  text(xx, yy[BFsort== BF10ultraText], ultraBF, cex= 1.3, pos= 4)
  text(xx, yy[BFsort== BF10wText], wBF, cex= 1.3, pos= 4)
}

.qt.shiftedT <- function(prob, parameters) {
  
  qt(prob, parameters[3]) * parameters[2] + parameters[1]
  
}

.posteriorSummaryGroupMean <- function(variable, descriptivesPlotsCredibleInterval=.95) {
  
  # Assumes that data are normally distributed
  # Jeffreys prior on mu and sigma: p(mu, sigma) proportional to 1/sigma
  # Compare Gelman et al. "Bayesian Data Analysis" for derivation of marginal posterior distribution of mu (inference for unknown mean and variance of a normal distribution)
  if (is.null(variable)) return(NULL)
  
  ciLower <- (1 - descriptivesPlotsCredibleInterval) / 2
  ciUpper <- ciLower + descriptivesPlotsCredibleInterval
  
  df <- length(variable) - 1
  location <- mean(variable)
  scale <- sd(variable) / sqrt(length(variable))
  
  outTmp <- .qt.shiftedT(c(ciLower, .5, ciUpper), parameters=c(location, scale, df))
  out <- list(ciLower=outTmp[1], median=outTmp[2], ciUpper=outTmp[3])
  
  return(out)
  
}

.base_breaks_y2 <- function(x, testValue){
  
  values <- c(testValue, x$ciLower, x$ciUpper)
  ci.pos <- c(min(values), max(values))
  b <- pretty(ci.pos)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
       ggplot2::scale_y_continuous(breaks=c(min(b), testValue, max(b))))
}

.plotGroupMeanBayesOneSampleTtest <- function(variable=1:10, variableName="test1", testValueOpt=0, descriptivesPlotsCredibleInterval=.95) {
  
  
  variable <- na.omit(variable)
  
  if (any(is.infinite(variable)))
    stop("Plotting not possible: Variable contains infinity")
  
  testValue <- data.frame("testValue" = testValueOpt) # default zero
  posteriorSummary <- .posteriorSummaryGroupMean(variable=variable, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
  summaryStat <- data.frame(groupingVariable=variableName, dependent=posteriorSummary$median, ciLower=posteriorSummary$ciLower, ciUpper=posteriorSummary$ciUpper)
  
  pd <- ggplot2::position_dodge(.2)
  
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
    ggplot2::geom_line(position=pd, size = .7) +
    ggplot2::geom_point(position=pd, size=4) +
    ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(	panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
                    panel.grid.major=ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
                    axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size=15),
                    panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                    plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                    legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                    panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                    legend.key = ggplot2::element_blank(),
                    legend.title = ggplot2::element_text(size=12),
                    legend.text = ggplot2::element_text(size = 12),
                    axis.ticks = ggplot2::element_line(size = 0.5),
                    axis.ticks.margin = grid::unit(1,"mm"),
                    axis.ticks.length = grid::unit(3, "mm"),
                    axis.ticks.x = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
    .base_breaks_y2(summaryStat, testValueOpt)
  
  # print(p)
  return(p)
}

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS INFORMED BAYESIAN T-TESTS
#-------------------------------------------------------------------------------

.A <- function(t, n, nu, mu.delta, g) {
  
  Re(hypergeo::genhypergeo(U = (nu + 1)/2, L = 1/2,
                           z = mu.delta^2*t^2/
                             (2*(1/n + g)*((1 + n*g)*nu + t^2))))
  
}

.B <- function(t, n, nu, mu.delta, g) {
  
  out <- mu.delta*t/sqrt(1/2*(1/n + g)*((1 + n*g)*nu + t^2)) *
    exp(lgamma((nu + 2)/2) - lgamma((nu + 1)/2)) *
    Re(hypergeo::genhypergeo(U = (nu + 2)/2, L = 3/2,
                             z = mu.delta^2*t^2/
                               (2*(1/n + g)*((1 + n*g)*nu + t^2))))
  
  return(out)
  
}

.C <- function(delta, t, n, nu) {
  
  Re(hypergeo::genhypergeo(U = (nu + 1)/2, L = 1/2,
                           z = n*t^2*delta^2/(2*(nu + t^2))))
  
}

.D <- function(delta, t, n, nu) {
  
  out <- t*delta*sqrt(2*n/(nu + t^2))*
    exp(lgamma((nu + 2)/2) - lgamma((nu + 1)/2))*
    Re(hypergeo::genhypergeo(U = (nu + 2)/2, L = 3/2,
                             z = n*t^2*delta^2/(2*(nu + t^2))))
  
  return(out)
  
}

.term_normalprior <- function(t, n, nu, mu.delta, g) {
  
  (1 + n*g)^(-1/2) * exp(-mu.delta^2/(2*(1/n + g))) *
    (1 + t^2/(nu*(1 + n*g)))^(-(nu + 1)/2) *
    (.A(t, n, nu, mu.delta, g) + .B(t, n, nu, mu.delta, g))
  
}

.integrand <- function(g, t, n, nu, mu.delta, r, kappa) {
  
  tmp <- .term_normalprior(t = t, n = n, nu = nu, mu.delta = mu.delta, g = g)
  pg_log <- kappa/2*(2*log(r) + log(kappa/2)) - lgamma(kappa/2) -
    (kappa/2 + 1)*log(g) - r^2*kappa/(2*g)
  pg <- exp(pg_log)
  out <- tmp*pg
  
  return(out)
  
}

.dtss <- function(delta, mu.delta, r, kappa, log = FALSE) {
  
  out <- - log(r) + lgamma((kappa + 1)/2) - .5*(log(pi) + log(kappa)) -
    lgamma(kappa/2) - (kappa + 1)/2 * log(1 + ((delta - mu.delta)/r)^2/kappa)
  
  if ( ! log)
    out <- exp(out)
  
  return(out)
  
}

.dprior_informative <- function(delta, oneSided = FALSE, options) {
  
  if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
    out <- .dtss(delta, mu.delta = options[["informativeCauchyLocation"]],
                 r = options[["informativeCauchyScale"]],
                 kappa = 1)
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/integrate(.dtss, 0, Inf,
                                                mu.delta = options[["informativeCauchyLocation"]],
                                                r = options[["informativeCauchyScale"]],
                                                kappa = 1)$value)
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/integrate(.dtss, -Inf, 0,
                                                mu.delta = options[["informativeCauchyLocation"]],
                                                r = options[["informativeCauchyScale"]],
                                                kappa = 1)$value)
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "t") {
    out <- .dtss(delta,
                 mu.delta = options[["informativeTLocation"]],
                 r = options[["informativeTScale"]],
                 kappa = options[["informativeTDf"]])
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/integrate(.dtss, 0, Inf,
                                                mu.delta = options[["informativeTLocation"]],
                                                r = options[["informativeTScale"]],
                                                kappa = options[["informativeTDf"]])$value)
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/integrate(.dtss, -Inf, 0,
                                                mu.delta = options[["informativeTLocation"]],
                                                r = options[["informativeTScale"]],
                                                kappa = options[["informativeTDf"]])$value)
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
    out <- dnorm(delta, mean = options[["informativeNormalMean"]],
                 sd = options[["informativeNormalStd"]])
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/pnorm(0, mean = options[["informativeNormalMean"]],
                                            sd = options[["informativeNormalStd"]],
                                            lower.tail = FALSE))
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/pnorm(0, mean = options[["informativeNormalMean"]],
                                            sd = options[["informativeNormalStd"]],
                                            lower.tail = TRUE))
    }
  }
  
  out[out < 0] <- 0
  return(out)
  
}

.posterior_t_tmp <- function(delta, t, ny, nx = NULL, independentSamples = FALSE,
                             prior.location, prior.scale, prior.df,
                             rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, ny*nx/(ny + nx), ny)
  nu <- ifelse(independentSamples, ny + nx - 2, ny - 1)
  
  mu.delta <- prior.location
  r <- prior.scale
  kappa <- prior.df
  
  numerator <- exp(-neff/2*delta^2)*(1 + t^2/nu)^(-(nu + 1)/2)*
    (.C(delta, t, neff, nu) + .D(delta, t, neff, nu))*
    .dtss(delta, mu.delta, r, kappa)
  
  denominator <- integrate(.integrand, lower = 0, upper = Inf,
                           t = t, n = neff, nu = nu, mu.delta = mu.delta,
                           r = r, kappa = kappa, rel.tol = rel.tol)$value
  
  out <- numerator/denominator
  
  if ( is.na(out))
    out <- 0
  
  return(out)
  
}

.posterior_t <- Vectorize(.posterior_t_tmp, "delta")

.cdf_t <- function(x, t, ny, nx = NULL, independentSamples = FALSE,
                   prior.location, prior.scale, prior.df) {
  
  integrate(.posterior_t, lower = -Inf, upper = x, t = t, ny = ny, nx = nx,
            independentSamples = independentSamples,
            prior.location = prior.location, prior.scale = prior.scale,
            prior.df = prior.df)$value
  
}

.quantile_t <- function(q, t, ny, nx = NULL,
                        independentSamples = FALSE,
                        prior.location, prior.scale,
                        prior.df, tol = 0.0001, max.iter = 100) {
  
  # compute quantiles via Newton-Raphson method
  
  x.cur <- Inf
  # get reasonable starting value
  delta <- seq(-2, 2, length.out = 400)
  dens <- .posterior_t(delta, t = t, ny = ny, nx = nx,
                       independentSamples = independentSamples,
                       prior.location = prior.location,
                       prior.scale = prior.scale,
                       prior.df = prior.df)
  x.new <- delta[which.max(dens)]
  i <- 1
  
  while (abs(x.cur - x.new) > tol && i < max.iter) {
    
    x.cur <- x.new
    x.new <- x.cur - (.cdf_t(x.cur, t = t, ny = ny, nx = nx,
                             independentSamples = independentSamples,
                             prior.location = prior.location,
                             prior.scale = prior.scale,
                             prior.df = prior.df) - q)/
      .posterior_t(x.cur, t = t, ny = ny, nx = nx,
                   independentSamples = independentSamples,
                   prior.location = prior.location, prior.scale = prior.scale,
                   prior.df = prior.df)
    i <- i + 1
    
  }
  
  return(x.new)
  
}

.ciPlusMedian_t <- function(t, ny, nx = NULL, independentSamples = FALSE,
                            prior.location, prior.scale, prior.df,
                            ci = .95, oneSided = FALSE, tol = 0.0001, max.iter = 100) {
  
  lower <- (1 - ci)/2
  upper <- ci + (1 - ci)/2
  med <- .5
  
  postAreaSmaller0 <- .cdf_t(x = 0, t = t, ny = ny, nx = nx,
                             independentSamples = independentSamples,
                             prior.location = prior.location,
                             prior.scale = prior.scale, prior.df = prior.df)
  
  # avoid numerical issues by making sure that area in [0, 1]
  if (postAreaSmaller0 < 0) {
    postAreaSmaller0 <- 0
  } else if (postAreaSmaller0 > 1) {
    postAreaSmaller0 <- 1
  }
  
  if (oneSided == "right") {
    
    lower <- postAreaSmaller0 + (1 - postAreaSmaller0)*lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0)*upper
    med <- postAreaSmaller0 + (1 - postAreaSmaller0)*med
    
  } else if (oneSided == "left") {
    
    lower <- postAreaSmaller0*lower
    upper <- postAreaSmaller0*upper
    med <- postAreaSmaller0*med
    
  }
  
  ciLower <- .quantile_t(lower, t = t, ny = ny, nx = nx,
                         independentSamples = independentSamples,
                         prior.location = prior.location,
                         prior.scale = prior.scale,
                         prior.df = prior.df)
  ciUpper <- .quantile_t(upper, t = t, ny = ny, nx = nx,
                         independentSamples = independentSamples,
                         prior.location = prior.location,
                         prior.scale = prior.scale,
                         prior.df = prior.df)
  median <- .quantile_t(med, t = t, ny = ny, nx = nx,
                        independentSamples = independentSamples,
                        prior.location = prior.location,
                        prior.scale = prior.scale,
                        prior.df = prior.df)
  
  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
  
}

.posterior_normal_tmp <- function(delta, t, ny, nx = NULL,
                                  independentSamples = FALSE, prior.mean,
                                  prior.variance,
                                  rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, ny*nx/(ny + nx), ny)
  nu <- ifelse(independentSamples, ny + nx - 2, ny - 1)
  
  mu.delta <- prior.mean
  g <- prior.variance
  
  numerator <- exp(-neff/2*delta^2)*(1 + t^2/nu)^(-(nu + 1)/2)*
    (.C(delta, t, neff, nu) + .D(delta, t, neff, nu))*
    dnorm(delta, mu.delta, sqrt(g))
  
  denominator <- .term_normalprior(t = t, n = neff, nu = nu,
                                   mu.delta = mu.delta, g = g)
  
  out <- numerator/denominator
  
  if ( is.na(out))
    out <- 0
  
  return(out)
  
}

.posterior_normal <- Vectorize(.posterior_normal_tmp, "delta")

.cdf_normal <- function(x, t, ny, nx = NULL, independentSamples = FALSE,
                        prior.mean, prior.variance) {
  
  integrate(.posterior_normal, lower = -Inf, upper = x, t = t, ny = ny, nx = nx,
            independentSamples = independentSamples,
            prior.mean = prior.mean, prior.variance = prior.variance)$value
  
}

.quantile_normal <- function(q, t, ny, nx = NULL,
                             independentSamples = FALSE,
                             prior.mean, prior.variance,
                             tol = 0.0001, max.iter = 100) {
  
  # compute quantiles via Newton-Raphson method
  
  x.cur <- Inf
  # get reasonable starting value
  delta <- seq(-2, 2, length.out = 400)
  dens <- .posterior_normal(delta, t = t, ny = ny, nx = nx,
                            independentSamples = independentSamples,
                            prior.mean = prior.mean,
                            prior.variance = prior.variance)
  x.new <- delta[which.max(dens)]
  i <- 1
  
  while (abs(x.cur - x.new) > tol && i < max.iter) {
    
    x.cur <- x.new
    x.new <- x.cur - (.cdf_normal(x.cur, t = t, ny = ny, nx = nx,
                                  independentSamples = independentSamples,
                                  prior.mean = prior.mean,
                                  prior.variance = prior.variance) - q)/
      .posterior_normal(x.cur, t = t, ny = ny, nx = nx,
                        independentSamples = independentSamples,
                        prior.mean = prior.mean, prior.variance = prior.variance)
    i <- i + 1
    
  }
  
  return(x.new)
  
}

.ciPlusMedian_normal <- function(t, ny, nx = NULL, independentSamples = FALSE,
                                 prior.mean, prior.variance, ci = .95,
                                 oneSided = FALSE, tol = 0.0001, max.iter = 100) {
  
  lower <- (1 - ci)/2
  upper <- ci + (1 - ci)/2
  med <- .5
  
  postAreaSmaller0 <- .cdf_normal(x = 0, t = t, ny = ny, nx = nx,
                                  independentSamples = independentSamples,
                                  prior.mean = prior.mean,
                                  prior.variance = prior.variance)
  
  # avoid numerical issues by making sure that area in [0, 1]
  if (postAreaSmaller0 < 0) {
    postAreaSmaller0 <- 0
  } else if (postAreaSmaller0 > 1) {
    postAreaSmaller0 <- 1
  }
  
  if (oneSided == "right") {
    
    lower <- postAreaSmaller0 + (1 - postAreaSmaller0)*lower
    upper <- postAreaSmaller0 + (1 - postAreaSmaller0)*upper
    med <- postAreaSmaller0 + (1 - postAreaSmaller0)*med
    
  } else if (oneSided == "left") {
    
    lower <- postAreaSmaller0*lower
    upper <- postAreaSmaller0*upper
    med <- postAreaSmaller0*med
    
  }
  
  ciLower <- .quantile_normal(lower, t = t, ny = ny, nx = nx,
                              independentSamples = independentSamples,
                              prior.mean = prior.mean,
                              prior.variance = prior.variance)
  ciUpper <- .quantile_normal(upper, t = t, ny = ny, nx = nx,
                              independentSamples = independentSamples,
                              prior.mean = prior.mean,
                              prior.variance = prior.variance)
  median <- .quantile_normal(med, t = t, ny = ny, nx = nx,
                             independentSamples = independentSamples,
                             prior.mean = prior.mean,
                             prior.variance = prior.variance)
  
  return(list(ciLower = ciLower, median = median, ciUpper = ciUpper))
  
}

.dposterior_informative <- function(delta, t, n1, n2 = NULL, paired = FALSE, oneSided = FALSE, options) {
  if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
    out <- .posterior_t(delta, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                        prior.location = options[["informativeCauchyLocation"]],
                        prior.scale = options[["informativeCauchyScale"]],
                        prior.df = 1)
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/(1 - .cdf_t(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                                  prior.location = options[["informativeCauchyLocation"]],
                                                  prior.scale = options[["informativeCauchyScale"]],
                                                  prior.df = 1)))
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/.cdf_t(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                             prior.location = options[["informativeCauchyLocation"]],
                                             prior.scale = options[["informativeCauchyScale"]],
                                             prior.df = 1))
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "t") {
    out <- .posterior_t(delta, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                        prior.location = options[["informativeTLocation"]],
                        prior.scale = options[["informativeTScale"]],
                        prior.df = options[["informativeTDf"]])
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/(1 - .cdf_t(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                                  prior.location = options[["informativeTLocation"]],
                                                  prior.scale = options[["informativeTScale"]],
                                                  prior.df = options[["informativeTDf"]])))
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/.cdf_t(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                             prior.location = options[["informativeTLocation"]],
                                             prior.scale = options[["informativeTScale"]],
                                             prior.df = options[["informativeTDf"]]))
    }
  } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
    out <- .posterior_normal(delta, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                             prior.mean = options[["informativeNormalMean"]],
                             prior.variance = options[["informativeNormalStd"]]^2)
    if (oneSided == "right") {
      out <- ifelse(delta < 0, 0, out/(1 - .cdf_normal(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                                       prior.mean = options[["informativeNormalMean"]],
                                                       prior.variance = options[["informativeNormalStd"]]^2)))
    } else if (oneSided == "left") {
      out <- ifelse(delta > 0, 0, out/.cdf_normal(0, t = t, ny = n1, nx = n2, independentSamples = ! paired && !is.null(n2),
                                                  prior.mean = options[["informativeNormalMean"]],
                                                  prior.variance = options[["informativeNormalStd"]]^2))
    }
  }
  
  out[out < 0] <- 0
  return(out)
  
}

.bf10_t <- function(t, ny, nx = NULL, independentSamples = FALSE, prior.location,
                    prior.scale, prior.df, oneSided, rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, ny*nx/(ny + nx), ny)
  nu <- ifelse(independentSamples, ny + nx - 2, ny - 1)
  
  mu.delta <- prior.location
  r <- prior.scale
  kappa <- prior.df
  int <- integrate(.integrand, lower = 0, upper = Inf,
                   t = t, n = neff, nu = nu, mu.delta = mu.delta,
                   r = r, kappa = kappa,
                   rel.tol = rel.tol)
  numerator <- int$value
  denominator <- (1 + t^2/nu)^(-(nu + 1)/2)
  
  BF10 <- numerator/denominator
  
  error <- exp(log(int[[2]]) - log(BF10))
  
  if (oneSided == FALSE) {
    return(list(bf = BF10, error = error))
  } else if (oneSided %in% c("left", "right")) {
    priorAreaSmaller0 <- integrate(.dtss, lower = -Inf, upper = 0,
                                   mu.delta = prior.location, r = prior.scale,
                                   kappa = prior.df)$value
    postAreaSmaller0 <- .cdf_t(x = 0, t = t, ny = ny, nx = nx,
                               independentSamples = independentSamples,
                               prior.location = prior.location,
                               prior.scale = prior.scale, prior.df = prior.df)
    
    # avoid numerical issues by making sure that area in [0, 1]
    if (priorAreaSmaller0 < 0) {
      priorAreaSmaller0 <- 0
    } else if (priorAreaSmaller0 > 1) {
      priorAreaSmaller0 <- 1
    }
    if (postAreaSmaller0 < 0) {
      postAreaSmaller0 <- 0
    } else if (postAreaSmaller0 > 1) {
      postAreaSmaller0 <- 1
    }
    
    if (oneSided == "left") {
      BFmin1 <- postAreaSmaller0/priorAreaSmaller0
      BFmin0 <- BFmin1 * BF10
      return(list(bf = BFmin0, error = error))
    } else if (oneSided == "right") {
      BFplus1 <- (1 - postAreaSmaller0)/(1 - priorAreaSmaller0)
      BFplus0 <- BFplus1 * BF10
      return(list(bf = BFplus0, error = error))
    }
  }
}

.bf10_normal <- function(t, ny, nx = NULL, independentSamples = FALSE,
                         prior.mean, prior.variance, oneSided) {
  
  neff <- ifelse(independentSamples, ny*nx/(ny + nx), ny)
  nu <- ifelse(independentSamples, ny + nx - 2, ny - 1)
  
  mu.delta <- prior.mean
  g <- prior.variance
  numerator <- .term_normalprior(t = t, n = neff, nu  = nu,
                                 mu.delta = mu.delta, g = g)
  denominator <- (1 + t^2/nu)^(-(nu + 1)/2)
  
  BF10 <- numerator/denominator
  
  if (oneSided == FALSE) {
    return(BF10)
  } else if (oneSided %in% c("left", "right")) {
    priorAreaSmaller0 <- pnorm(0, mean = prior.mean, sd = sqrt(prior.variance))
    postAreaSmaller0 <- .cdf_normal(x = 0, t = t, ny = ny, nx = nx,
                                    independentSamples = independentSamples,
                                    prior.mean = prior.mean,
                                    prior.variance = prior.variance)
    if (oneSided == "left") {
      BFmin1 <- postAreaSmaller0/priorAreaSmaller0
      BFmin0 <- BFmin1 * BF10
      return(BFmin0)
    } else if (oneSided == "right") {
      BFplus1 <- (1 - postAreaSmaller0)/(1 - priorAreaSmaller0)
      BFplus0 <- BFplus1 * BF10
      return(BFplus0)
    }
  }
}

#-------------------------------------------------------------------------------
# GENERAL T-TEST BF FUNCTION
#-------------------------------------------------------------------------------
.generalTtestBF <- function(x = NULL, y = NULL, paired = FALSE,
                            oneSided = FALSE, options) {
  
  tValue <- unname(t.test(x, y, paired = paired, var.equal = TRUE)$statistic)
  
  n1 <- length(x)
  n2 <- ifelse(paired, 0, length(y))
  
  if(options[["effectSizeStandardized"]] == "default") {
    
    ### default zero-centered Cauchy prior ###
    
    if (oneSided == FALSE) {
      nullInterval <- c(-Inf, Inf)
    } else if (oneSided == "right") {
      nullInterval <- c(0, Inf)
    } else if (oneSided == "left") {
      nullInterval <- c(-Inf, 0)
    }
    
    bfObject <- BayesFactor::ttest.tstat(
      t = tValue,
      n1 = n1,
      n2 = n2,
      rscale = options$priorWidth,
      nullInterval = nullInterval)
    bf <- exp(bfObject$bf)
    error <- 100*bfObject$properror
    
  } else if (options[["effectSizeStandardized"]] == "informative") {
    
    ### informed prior ###
    
    # Note that strictly speaking, in case of the independent samples t-test,
    # for the informed prior n1 corresponds to nx and n2 to ny and not vice-versa.
    # However, since in the expression for the Bayes factor they only appear
    # as an "effective" sample size and in the degrees of freedom for which it does
    # not matter whether we swap the two, we retain this order for easier extension
    # of the one-sample case.
    
    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      bfObject <- .bf10_t(t = tValue, ny = n1, nx = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeCauchyLocation"]],
                          prior.scale = options[["informativeCauchyScale"]],
                          prior.df = 1)
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      bfObject <- .bf10_t(t = tValue, ny = n1, nx = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeTLocation"]],
                          prior.scale = options[["informativeTScale"]],
                          prior.df = options[["informativeTDf"]])
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      bf <- .bf10_normal(t = tValue, ny = n1, nx = n2, oneSided = oneSided,
                         independentSamples = ! paired && !is.null(y),
                         prior.mean = options[["informativeNormalMean"]],
                         prior.variance = options[["informativeNormalStd"]]^2)
      error <- NULL
    }
    
  }
  
  return(list(bf = bf, error = error, tValue = tValue, n1 = n1, n2 = n2))
  
}

