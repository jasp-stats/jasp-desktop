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

LSbinomialtesting   <- function(jaspResults, dataset, options, state = NULL){
  
  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("plotsPosteriorMarginalBF", "plotsPosteriorBF"))
  
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)
  
  # introductory text
  if (options[["introText"]])
    .introductoryTextLS(jaspResults, options, "binTest")
  
  # evaluate the expressions in priors
  if (ready["priors"])
    options[["priors"]] <- .evaluatePriors(options[["priors"]])
  # scale the prior probabilities
  if (ready["priors"])
    options[["priors"]] <- .scalePriors(options[["priors"]])
  
  # load, check, transform and process data
  if (ready["data"])
    data <- .readDataBinomialLS(dataset, options)
  
  # data summary table ifrequested (but not ifthe data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "binTest")
  
  ### inference
  # summary table
  .testsBinomialLS(jaspResults, data, ready, options)
  
  # prior parameter
  if (options[["plotsPrior"]]){
    if (options[["plotsPriorType"]] != "conditional")
      .plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["plotsPriorType"]] == "conditional")
      .plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # prior predictive
  if (options[["plotsPredictions"]]){
    if (options[["plotsPredictionType"]] != "conditional")
      .plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["plotsPredictionType"]] == "conditional")
      .plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["predictionPlotTable"]])
      .tablePredictions2LS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # predictive accuracy
  if (options[["plotsPredictiveAccuracy"]])
    .plotsPredAccuracyBinomial2LS(jaspResults, data, ready, options)
  
  # posterior parameter
  if (options[["plotsPosterior"]]){
    if (options[["plotsPosteriorType"]] != "conditional")
      .plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["plotsPosteriorType"]] == "conditional")
      .plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if (options[["plotsBoth"]]){
    if (options[["plotsBothType"]] != "conditional")
      .plotsBothBinomialLS2(jaspResults, data, ready, options)
    if (options[["plotsBothType"]] == "conditional")
      .plotsBothIndividualBinomial2LS(jaspResults, data, ready, options)
  }
  
  
  ### sequential analysis
  if (options[["plotsIterative"]])
    .plotsIterativeOverlyingBinomial2LS(jaspResults, data, ready, options)
  if (options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]])
    .tableIterativeBinomial2LS(jaspResults, data, ready, options)
  
  
  ### posterior predictive
  if (options[["predictionTable"]])
    .tablePredictionsBinomialLS2(jaspResults, data, ready, options)
  if (options[["plotsPredictionsPost"]]){
    if (options[["plotsPredictionPostType"]] != "conditional")
      .plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["plotsPredictionPostType"]] == "conditional")
      .plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["predictionPostPlotTable"]])
      .tablePredictions2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  return()
}

.testsBinomialLS              <- function(jaspResults, data, ready, options){
  
  if (is.null(jaspResults[["testsContainer"]])){
    testsContainer <- createJaspContainer("Hypotheses")
    testsContainer$position <- 2
    jaspResults[["testsContainer"]] <- testsContainer 
  } else
    testsContainer <- jaspResults[["testsContainer"]]
  
  
  if (options[["introText"]] && is.null(testsContainer[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("tests", options, "binTest")
    
    testsContainer[['introText']] <- introText    
  }
  
  
  if (is.null(testsContainer[['testsTable']])){
    
    testsTable <- createJaspTable(title = gettext("Testing Summary"))
    
    testsTable$position <- 2
    testsTable$dependOn(c(.dataDependenciesBinomialLS, "bfType", "bfTypevsName", "bayesFactorType"))
    
    bfTypeName <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )
    
    testsTable$addColumnInfo(name = "hypothesis",   title = gettext("Hypothesis"),          type = "string")
    testsTable$addColumnInfo(name = "prior",        title = gettext("P(H)"),                type = "number")
    testsTable$addColumnInfo(name = "logLik",       title = gettext("log(likelihood)"),     type = "number")
    testsTable$addColumnInfo(name = "posterior",    title = gettext("P(H|data)"),           type = "number")
    testsTable$addColumnInfo(name = "bf",           title = bfTypeName,                    type = "number")
    
    testsTable$setExpectedSize(length(options[["priors"]]))
    
    testsContainer[["testsTable"]] <- testsTable
    
    if (ready["data"] && !ready["priors"])
      return()
    else if (!ready["data"]){
      
      if ((options[["dataType"]] == "dataVariable" && options[["selectedVariable"]]  != "") ||
          (options[["dataType"]] == "dataSequence" && options[["dataSequenceInput"]] != ""))
        testsTable$addFootnote(gettext("Please specify successes and failures."))
      
      return()
    }else if (ready["priors"]){
      
      tempResults <- .testBinomialLS(data, options[["priors"]])
      
      for(i in 1:length(options[["priors"]])){
        
        tempRow <- list(
          hypothesis  = options[["priors"]][[i]]$name,
          prior       = tempResults$prior[i],
          logLik      = tempResults$logLik[i], 
          posterior   = tempResults$posterior[i])
        
        if (options[["bfType"]] == "inclusion")
          tempBF <- (tempResults$posterior[i] / (1-tempResults$posterior[i])) / (tempResults$prior[i] / (1-tempResults$prior[i]))
        else if (options[["bfType"]] == "best")
          tempBF <- exp(tempResults$logLik[i]) / exp(tempResults$logLik[which.max(tempResults$logLik)])
        else if (options[["bfType"]] == "vs")
          tempBF <- exp(tempResults$logLik[i]) / exp(tempResults$logLik[sapply(options[["priors"]], function(p)p$name) == options[["bfTypevsName"]]])
        
        if (options[["bayesFactorType"]] == "BF10")
          tempRow$bf <- tempBF
        else if (options[["bayesFactorType"]] == "BF01")
          tempRow$bf <- 1/tempBF          
        else if (options[["bayesFactorType"]] == "LogBF10")
          tempRow$bf <- log(tempBF)
        
        testsTable$addRows(tempRow)
      }
      
      # add footnote clarifying what dataset was used
      testsTable$addFootnote(gettextf(
        "These results are based on %i %s and %i %s.",
        data[["nSuccesses"]], ifelse(data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]],  ifelse(data[["nFailures"]]  == 1, gettext("failure"), gettext("failures"))
      ))
      
    }
    
  }
  
  return()
}
.plotsSimpleBinomial2LS       <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlots2LS(jaspResults, options, "binTest", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsSimple <- createJaspPlot(
      width  = if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "joint" &&
                   options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] != "stacked")
        700 else 530,
      height = 400)
    
    plotsSimple$position <- 2
    plotsSimple$dependOn(c(.dataDependenciesBinomialLS,
                           ifelse(type == "Prior", "plotsPriorJointType",            "plotsPosteriorJointType"),
                           ifelse(type == "Prior", "plotsPriorMarginalCI",           "plotsPosteriorMarginalCI"),
                           ifelse(type == "Prior", "plotsPriorMarginalType",         "plotsPosteriorMarginalType"),
                           ifelse(type == "Prior", "plotsPriorMarginalCoverage",     "plotsPosteriorMarginalCoverage"),
                           ifelse(type == "Prior", "plotsPriorMarginalLower",        "plotsPosteriorMarginalLower"),
                           ifelse(type == "Prior", "plotsPriorMarginalUpper",        "plotsPosteriorMarginalUpper"),
                           ifelse(type == "Prior", "plotsPriorMarginalEstimate",     "plotsPosteriorMarginalEstimate"),
                           ifelse(type == "Prior", "plotsPriorMarginalEstimateType", "plotsPosteriorMarginalEstimateType"),
                           if (type == "Posterior") c("plotsPosteriorObserved", "plotsPosteriorMarginalBF"),
                           "colorPalette", "scaleSpikes"))
    
    
    containerPlots[[paste0("plots",type)]] <- plotsSimple
    
    if (!all(ready))return()
    
    allLines    <- c()
    allArrows   <- c()
    legend       <- NULL
    tempResults <- .testBinomialLS(data, options[["priors"]])
    
    if (any(is.nan(tempResults$posterior))){
      plotsSimple$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
      return()
    }
    
    for(i in 1:length(options[["priors"]])){
      
      if (options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPP       <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP$yEnd  <- exp(log(dfArrowPP$yEnd)+log(tempResults[i, tolower(type)]))
        dfArrowPP$g     <- options[["priors"]][[i]]$name
        
        allArrows      <- c(allArrows, list(dfArrowPP))
        legend          <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      } else if (options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$y <- exp(log(dfLinesPP$y)+log(tempResults[i, tolower(type)]))
        dfLinesPP$g <- options[["priors"]][[i]]$name
        
        allLines    <- c(allLines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      }
    }
    
    if (type == "Posterior" && options[["plotsPosteriorObserved"]]){
      dfPoints <- data.frame(
        x = data[["nSuccesses"]]/(data[["nSuccesses"]] + data[["nFailures"]]),
        y = 0,
        g = "Observed"
      )
    } else
      dfPoints <- NULL
    
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "joint"){
      
      if (options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "overlying")
        p <- .plotOverlyingLS(allLines, allArrows, dfPoints, xName = xName, palette = options[["colorPalette"]])  
      else if (options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "stacked")
        p <- .plotStackedLS(allLines, allArrows, legend, dfPoints, xName = xName)
      
    } else if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "marginal"){
      
      allLinesNew <- c()
      allSpikes   <- list()
      if (length(allLines) > 0){
        
        for(i in 1:length(allLines)){
          
          if (i == 1)
            allLinesNew[[1]] <- allLines[[i]]
          else
            allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y
          
        }
        allLinesNew[[1]]$g <- "__marginal"
      }
      
      if (length(allArrows) > 0){
        for(i in 1:length(allArrows)){
          allArrows[[i]]$g <- "__marginal"
        }
      }
      
      if (type == "Prior"){
        for(i in 1:length(options[["priors"]])){
          if (options[["priors"]][[i]]$type == "spike"){
            allSpikes <- c(
              allSpikes, 
              list(data.frame(y = options[["priors"]][[i]]$PH, x = options[["priors"]][[i]]$parPoint, g = "__marginal"))
            )
          }
        }
      } else {
        tempResults <- .testBinomialLS(data, options[["priors"]])
        for(i in 1:length(options[["priors"]])){
          if (options[["priors"]][[i]]$type == "spike"){
            allSpikes <- c(
              allSpikes, 
              list(data.frame(y = tempResults$posterior[i], x = options[["priors"]][[i]]$parPoint, g = "__marginal"))
            )
          }
        }
        
      }
      
      if (options[[ifelse(type == "Prior", "plotsPriorMarginalCI", "plotsPosteriorMarginalCI")]]){
        
        if (options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "central"){
          
          dfCI <- .marginalCentralBinomialLS(allLinesNew[[1]], allSpikes, 
                                             options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])
          
        } else if (options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "HPD"){
          
          dfCI <- .marginalHPDBinomialLS(allLinesNew[[1]], allSpikes,
                                         options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])    
          
        } else if (options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "custom"){
          
          dfCI <- .marginalCustomBinomialLS(allLinesNew[[1]], allSpikes,
                                            lCI = options[[ifelse(type == "Prior", "plotsPriorMarginalLower", "plotsPosteriorMarginalLower")]],
                                            uCI = options[[ifelse(type == "Prior", "plotsPriorMarginalUpper", "plotsPosteriorMarginalUpper")]])
          
        } else if (options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "support"){
          
          dfCI <- .marginalSupportBinomialLS(data, options[["priors"]], allLinesNew[[1]], allSpikes, options[["plotsPosteriorMarginalBF"]])
          
        }
        
      } else
        dfCI <- NULL
      
      if (options[[ifelse(type == "Prior", "plotsPriorMarginalEstimate", "plotsPosteriorMarginalEstimate")]]){
        
        dfPointEstimate <- .dataPointMarginalBinomial(if (type == "Prior") NULL else data, options, allLinesNew[[1]], allSpikes, N = NULL,
                                                      type = "parameter", type2 = type,
                                                      estimate = options[[ifelse(type == "Prior", "plotsPriorMarginalEstimateType", "plotsPosteriorMarginalEstimateType")]])
      } else
        dfPointEstimate <- NULL
      
      p <- .plotOverlyingLS(allLinesNew, allArrows, dfPoints, dfPointEstimate, CI = dfCI, xName = xName, noLegend = T)
      
    }
    
    plotsSimple$plotObject <- p
  }
  
  return()
}
.plotsIndividualBinomial2LS   <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlots2LS(jaspResults, options, "binTest", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsIndividual <- createJaspContainer()
    
    plotsIndividual$position <- 2
    plotsIndividual$dependOn(c(.dataDependenciesBinomialLS,
                               ifelse(type == "Prior", "plotsPriorEstimate",     "plotsPosteriorEstimate"),
                               ifelse(type == "Prior", "plotsPriorEstimateType", "plotsPosteriorEstimateType"),
                               ifelse(type == "Prior", "plotsPriorCI",           "plotsPosteriorCI"),
                               ifelse(type == "Prior", "plotsPriorTypeCI",       "plotsPosteriorTypeCI"),
                               ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                               ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                               ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper"),
                               if (type == "Posterior") c("plotsPosteriorObserved", "plotsPosteriorBF"),
                               "scaleSpikes"))
    
    containerPlots[[paste0("plots",type)]] <- plotsIndividual
    
    
    if (all(!ready) || (ready["data"] && !ready["priors"])){
      
      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready["data"] && ready["priors"]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      if (type == "Prior"){
        tempData <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else
        tempData <- data
      
      tempResults <- .testBinomialLS(data, options[["priors"]])
      
      if (type == "Posterior" && options[["plotsPosteriorObserved"]]){
        dfPoints <- data.frame(
          x = data[["nSuccesses"]]/(data[["nSuccesses"]] + data[["nFailures"]]),
          y = 0,
          g = "Observed"
        )
      } else
        dfPoints <- NULL
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIndividual[[options[["priors"]][[i]]$name]] <- tempPlot
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL
        
        if (options[[ifelse(type == "Prior", "plotsPriorCI", "plotsPosteriorCI")]]){
          
          if (options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "central")
            dfCI <- .dataCentralBinomialLS(tempData, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          else if (options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "HPD")
            dfCI <- .dataHPDBinomialLS(tempData, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          else if (options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "custom")
            dfCI <- .dataCustomBinomialLS(tempData, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
                                          options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]], type = "parameter")  
          else if (options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "support")
            dfCI <- .dataSupportBinomialLS(tempData, options[["priors"]][[i]], options[["plotsPosteriorBF"]])  
          
        }
        
        if (options[["priors"]][[i]]$type == "spike")
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
        else if (options[["priors"]][[i]]$type == "beta"){
          
          dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
          dfLinesPP$y <- dfLinesPP$y
          
          if (!is.null(dfCI)){
            for(r in 1:nrow(dfCI)){
              tempCILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$xStart[r] & dfLinesPP$x <= dfCI$xEnd[r],]
              tempCILinesPP$g <- paste(c(as.character(dfCI$g), r), collapse = "")
              tempCILinesPP   <- rbind.data.frame(
                data.frame(x = dfCI$xStart[r], y = 0, g = tempCILinesPP$g[1]),
                tempCILinesPP,
                data.frame(x = dfCI$xEnd[r], y = 0, g = tempCILinesPP$g[1])
              )
              dfCILinesPP <- rbind.data.frame(dfCILinesPP, tempCILinesPP)
            }
          }
          
        }
        
        if (options[[ifelse(type == "Prior", "plotsPriorEstimate", "plotsPosteriorEstimate")]]){
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["priors"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse(type == "Prior", "plotsPriorEstimateType", "plotsPosteriorEstimateType")]])
        } else
          dfPointEstimate <- NULL
        
        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfPointEstimate, dfCI, dfCILinesPP, dfPoints, c(0,1), xName, nRound = 3)
        tempPlot$plotObject <- p
      }
      
    }
  }
  
  return()
}
.plotsPredictionsBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)
  
  if (is.null(containerPlots[[paste0("plotsPredictions",type)]])){
    
    plotsPredictions <- createJaspPlot(
      width  = if (options[[ifelse(type == "Prior", "plotsPredictionType",  "plotsPredictionPostType")]] == "joint" &&
                   options[[ifelse(type == "Prior", "plotsPredictionJointType", "plotsPredictionPostJointType")]] != "stacked")
        700 else 530,
      height = 400)
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesBinomialLS,
                                ifelse(type == "Prior", "plotsPredictionMarginalEstimate",     "plotsPredictionPostMarginalEstimate"),
                                ifelse(type == "Prior", "plotsPredictionMarginalEstimateType", "plotsPredictionPostMarginalEstimateType"),
                                ifelse(type == "Prior", "plotsPredictionMarginalCI",           "plotsPredictionPostMarginalCI"),
                                ifelse(type == "Prior", "plotsPredictionMarginalTypeCI",       "plotsPredictionPostMarginalTypeCI"),
                                ifelse(type == "Prior", "plotsPredictionMarginalCoverage",     "plotsPredictionPostMarginalCoverage"),
                                ifelse(type == "Prior", "plotsPredictionMarginalLower",        "plotsPredictionPostMarginalLower"),
                                ifelse(type == "Prior", "plotsPredictionMarginalUpper",        "plotsPredictionPostMarginalUpper"),
                                ifelse(type == "Prior", "plotsPredictionJointType",            "plotsPredictionPostJointType"),
                                ifelse(type == "Prior", "plotsPredictionsObserved",            "predictionPostPlotProp"),
                                "colorPalette"
    ))
    
    containerPlots[[paste0("plotsPredictions",type)]] <- plotsPredictions
    
    
    if (!all(ready) || (data[["nSuccesses"]] == 0 && data[["nFailures"]] == 0))
      return()
    else {
      
      if (type == "Prior"){
        predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
        tempResults <- .testBinomialLS(data, options[["priors"]])
        tempData    <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else if (type == "Posterior"){
        predictionN  <- options[["predictionN"]]
        tempResults <- .testBinomialLS(data, options[["priors"]])
        tempData    <- data
        
        if (any(is.nan(tempResults$posterior))){
          plotsPredictions$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
          return()
        }
      }
      
      if (type == "Posterior" && options[["predictionPostPlotProp"]]){
        xName  <- gettext("Predicted sample proportions")
        yName  <- gettext("Density")
        xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        proportions <- options[["predictionPostPlotProp"]]
        nRound <- 3
      } else {
        xName  <- gettext("Predicted number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, predictionN + .5)
        nRound <- 0
        proportions <- FALSE
      }
      
      
      allLines  <- c()
      legend     <- NULL
      
      for(i in 1:length(options[["priors"]])){
        
        dfHist   <- .dataHistBinomialLS2(tempData, options[["priors"]][[i]], predictionN)
        dfHist$g <- options[["priors"]][[i]]$name
        dfHist$y <- dfHist$y*tempResults[i,ifelse(type == "Prior","prior","posterior")]
        
        if (type == "Posterior" && options[["predictionPostPlotProp"]])
          dfHist$x <- dfHist$x/predictionN
        
        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        allLines<- c(allLines, list(dfHist))
      }
      
      if (type == "Prior"){
        if (options[["plotsPredictionsObserved"]])
          dfPoint <- data.frame(x = data[["nSuccesses"]], y = 0)
        else
          dfPoint <- NULL
      } else
        dfPoint <- NULL
      
      if (options[[ifelse(type == "Prior","plotsPredictionType", "plotsPredictionPostType")]] == "joint"){
        
        if (options[[ifelse(type == "Prior", "plotsPredictionJointType", "plotsPredictionPostJointType")]] == "overlying"){
          p <- .plotOverlyingLS(allLines, NULL, dfPoints = dfPoint, xName = xName, yName = yName, xRange = xRange,
                                palette = options[["colorPalette"]], nRound = nRound,
                                discrete = TRUE, proportions = proportions)
        } else if (options[[ifelse(type == "Prior", "plotsPredictionJointType", "plotsPredictionPostJointType")]] == "stacked"){
          p <- .plotStackedLS(allLines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange,
                              proportions = proportions, discrete = TRUE)
        }
        
        
        
      } else if (options[[ifelse(type == "Prior","plotsPredictionType", "plotsPredictionPostType")]] == "marginal"){
        
        if (length(allLines) > 0){
          
          for(i in 1:length(allLines)){
            
            if (i == 1)
              allLinesNew <- allLines[[i]]
            else
              allLinesNew$y <- allLinesNew$y + allLines[[i]]$y
            
          }
          allLinesNew$g <- "__marginal"
        }
        
        allLinesNew   <- allLinesNew[seq(1,nrow(allLinesNew),2),]
        if (type == "Posterior" && options[["predictionPostPlotProp"]])
          allLinesNew$x <- allLinesNew$x + .5/predictionN
        else
          allLinesNew$x <- allLinesNew$x + .5
        
        if (type == "Prior"){
          if (options[["plotsPredictionsObserved"]])
            xBlacked <- data[["nSuccesses"]]
          else
            xBlacked <- NULL
        } else
          xBlacked <- NULL
        
        if (options[[ifelse(type == "Prior", "plotsPredictionMarginalCI", "plotsPredictionPostMarginalCI")]]){
          
          if (options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "central"){
            
            dfCI <- .marginalCentralBinomialLS(allLinesNew, NULL, options[["plotsPredictionMarginalCoverage"]], 0, predictionN, TRUE)        
            
          } else if (options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "HPD"){
            
            dfCI <- .marginalHPDBinomialLS(allLinesNew, list(),
                                           options[[ifelse(type == "Prior", "plotsPredictionMarginalCoverage", "plotsPredictionPostMarginalCoverage")]],
                                           0, predictionN, TRUE)    
            
          } else if (options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "custom"){
            
            dfCI <- .marginalCustomBinomialLS(allLinesNew, list(),
                                              lCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]],
                                              uCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]],
                                              TRUE)
            
            if (options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]
                > predictionN){
              
              plotsPredictions$setError("The upper CI limit is higher than the number of future 
                                       observations. Please change the value of the upper CI limit 
                                       in the settings panel.")
              
              return()
            }
            if (options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]]
                > predictionN){
              
              plotsPredictions$setError("The lower CI limit is higher than the number of future 
                                       observations. Please change the value of the lower CI limit 
                                       in the settings panel.")
              
              return()
            }
            if (options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]] > 
                options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]){
              
              plotsPredictions$setError("The lower CI limit is higher than the upper CI limit.
                                       Please change the value of the CI limits 
                                       in the settings panel.")
              
              return()
            }
            
          }
        } else
          dfCI <- NULL
        
        if (type == "Posterior" && options[["predictionPostPlotProp"]])
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        else
          xRange <- c(0, predictionN)
        
        if (options[[ifelse(type == "Prior", "plotsPredictionMarginalEstimate", "plotsPredictionPostMarginalEstimate")]]){
          
          dfPointEstimate <- .dataPointMarginalBinomial(if (type == "Prior") NULL else data, options, allLinesNew, NULL, N = predictionN,
                                                        type = "prediction", type2 = type,
                                                        estimate = options[[ifelse(type == "Prior", "plotsPredictionMarginalEstimateType", "plotsPredictionPostMarginalEstimateType")]],
                                                        prop = if (type == "Posterior") options[["predictionPostPlotProp"]] else FALSE)
        } else
          dfPointEstimate <- NULL
        
        p <- .plotPredictionLS(allLinesNew, dfPointEstimate, dfCI, xRange = xRange, xName = xName, yName = yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)
        
      } else
        p <- .plotStackedLS(allLines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange, proportions = proportions)
      
      plotsPredictions$plotObject <- p
    }
  }
  
  return()
}
.plotsPredictionsIndividualBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)
  
  if (is.null(containerPlots[[paste0("plotsPredictions",type)]])){
    
    plotsPredictionsIndividual <- createJaspContainer()
    
    plotsPredictionsIndividual$position <- 2
    plotsPredictionsIndividual$dependOn(c(.dataDependenciesBinomialLS,
                                          ifelse(type == "Prior", "plotsPredictionEstimate",     "plotsPredictionPostEstimate"),
                                          ifelse(type == "Prior", "plotsPredictionEstimateType", "plotsPredictionPostEstimateType"),
                                          ifelse(type == "Prior", "plotsPredictionCI",           "plotsPredictionPostCI"),
                                          ifelse(type == "Prior", "plotsPredictionTypeCI",       "plotsPredictionPostTypeCI"),
                                          ifelse(type == "Prior", "plotsPredictionCoverage",     "plotsPredictionPostCoverage"),
                                          ifelse(type == "Prior", "plotsPredictionLower",        "plotsPredictionPostLower"),
                                          ifelse(type == "Prior", "plotsPredictionUpper",        "plotsPredictionPostUpper"),
                                          ifelse(type == "Prior", "plotsPredictionsObserved",    "predictionPostPlotProp"),
                                          "colorPalette"
    ))
    
    
    containerPlots[[paste0("plotsPredictions",type)]] <- plotsPredictionsIndividual
    
    
    if (all(!ready) || (ready["data"] && !ready["priors"])){
      
      plotsPredictionsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if ((!ready["data"] && ready["priors"]) || (data[["nSuccesses"]] == 0 & data[["nFailures"]] == 0)){
      
      for(i in 1:length(options[["priors"]])){
        plotsPredictionsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                      width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      if (type == "Prior"){
        predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
        tempResults  <- .testBinomialLS(data, options[["priors"]])
        tempData    <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else if (type == "Posterior"){
        predictionN  <- options[["predictionN"]]
        tempResults  <- .testBinomialLS(data, options[["priors"]])
        tempData    <- data
      }
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsPredictionsIndividual[[options[["priors"]][[i]]$name]] <- tempPlot
        
        if (type == "Posterior" && options[["predictionPostPlotProp"]]){
          xName  <- gettext("Predicted sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
          proportions <- options[["predictionPostPlotProp"]]
        } else {
          xName  <- gettext("Predicted number of successes")
          yName  <- gettext("Probability")
          xRange <- c(0, predictionN)
          proportions <- FALSE
        }
        
        
        dfCI   <- NULL
        dfHist <- NULL
        
        if (options[[ifelse(type == "Prior","plotsPredictionCI","plotsPredictionPostCI")]]){
          
          if (options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(data, options[["priors"]][[i]],
                                           options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                                           n = predictionN,type = "prediction")
            
          } else if (options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(data, options[["priors"]][[i]],
                                       options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                                       n = predictionN, type = "prediction")
            
          } else if (options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(data, options[["priors"]][[i]],
                                          options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]],
                                          options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]],
                                          n = predictionN, type = "prediction")
            
            if (options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]] > predictionN){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The upper CI limit is higher than the number of future observations. Please, change the value of the upper CI limit in the settings panel."))
              
              return()
            }
            if (options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]]  > predictionN){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the number of future observations. Please, change the value of the lower CI limit in the settings panel."))
              
              return()
            }
            if (options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]] 
                > options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]]){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the upper CI limit. Please, change the value of the CI limits in the settings panel."))
              
              return()
            }
            
          }
        }
        
        dfHist  <- .dataHistBinomialLS(tempData, options[["priors"]][[i]], predictionN)
        
        if (type == "Prior"){
          if (options[["plotsPredictionsObserved"]])
            xBlacked <- data[["nSuccesses"]]
          else
            xBlacked <- NULL
        } else
          xBlacked <- NULL
        
        if (type == "Posterior" && options[["predictionPostPlotProp"]]){
          dfHist$x <- dfHist$x/predictionN
          if (options[["plotsPredictionPostCI"]]){
            dfCI$xStart <- dfCI$xStart/predictionN
            dfCI$xEnd   <- dfCI$xEnd  /predictionN
          }
          nRound <- 3
        } else
          nRound <- 0
        
        if (options[[ifelse(type == "Prior", "plotsPredictionEstimate", "plotsPredictionPostEstimate")]]){
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["priors"]][[i]], N = predictionN, type = "prediction",
                                                        estimate = options[[ifelse(type == "Prior", "plotsPredictionEstimateType", "plotsPredictionPostEstimateType")]],
                                                        prop = ifelse(type == "Prior", FALSE, options[["predictionPostPlotProp"]])
          )
        } else
          dfPointEstimate <- NULL
        
        p <- .plotPredictionLS(dfHist, dfPointEstimate, dfCI, xRange, xName, yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)
        tempPlot$plotObject <- p
      }
    }
  }
  
  return()
}
.tablePredictions2LS                    <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)
  
  if (is.null(containerPlots[["tablePredictions"]])){
    
    tablePredictions <- createJaspTable()
    
    tablePredictions$position <- 3
    tablePredictions$dependOn(c(
      .dataDependenciesBinomialLS,
      ifelse(type == "Prior", "predictionPlotTable", "predictionPostPlotTable")
    ))
    containerPlots[["tablePredictions"]] <- tablePredictions
    
    
    if (type == "Prior") {
      tempData <- list(
        nSuccesses = 0,
        nFailures  = 0
      )
      tempN <- data$nSuccesses + data$nFailures
    } else if (type == "Posterior") {
      tempData <- data
      tempN <- options[["predictionN"]]
    }
    
    
    if (type == "Posterior" && options[["predictionPostPlotProp"]]){
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Proportion of Successes"), type = "number")
      tablePredictions$addColumns(c(0:tempN)/tempN)
    } else {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Successes"), type = "integer")
      tablePredictions$addColumns(0:tempN)
    }
    
    
    if (ready["priors"]){
      if (options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]] %in% c("joint", "conditional")){
        for(i in seq_along(options[["priors"]])){
          tablePredictions$addColumnInfo(name = paste0("hyp_", i), title = gettextf("P(Successes|%s)", options[["priors"]][[i]]$name), type = "number")
        }
      } else if (options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]] == "marginal")
        tablePredictions$addColumnInfo(name = "marginal", title = gettextf("P(Successes)"), type = "number")
    } else
      return()
    
    
    if (!ready["data"] && type != "Prior"){
      
      if ((options[["dataType"]] == "dataVariable" && options[["selectedVariable"]] != "") ||
          (options[["dataType"]] == "dataSequence" && options[["dataSequenceInput"]]    != ""))
        tablePredictions$addFootnote(gettext("Please specify successes and failures."))
      
      return()
    }
    
    
    tempResults <- .testBinomialLS(tempData, options[["priors"]])
    tempProb    <- NULL
    
    for(i in 1:length(options[["priors"]])){
      tempProb <- cbind(tempProb, .predictBinomialValuesLS(tempData, options[["priors"]][[i]], tempN))
    }
    
    if (options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]] == "conditional"){
      for(i in 1:length(options[["priors"]])){
        tablePredictions$addColumns(tempProb[,i])
      }
    } else if (options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]] == "joint"){
      for(i in 1:length(options[["priors"]])){
        tablePredictions$addColumns(tempProb[,i]*tempResults[i,"posterior"])
      }
    } else if (options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]] == "marginal"){
      tablePredictions$addColumns(apply(tempProb*matrix(tempResults[,"posterior"], byrow = T, ncol = length(options[["priors"]]), nrow = tempN + 1), 1, sum))
    }
    
  }
  return()
}
.plotsPredAccuracyBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerPredictiveAccuracy <- .containerPredictiveAccuracyLS(jaspResults, options, "binTest")
  
  if (is.null(containerPredictiveAccuracy[["plotsPredAccuracy"]])){
    
    plotsPredAccuracy <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsPredAccuracy$position <- 2
    plotsPredAccuracy$dependOn(c(.dataDependenciesBinomialLS, "colorPalette"))
    
    containerPredictiveAccuracy[["plotsPredAccuracy"]] <- plotsPredAccuracy
    
    
    if (!all(ready) || (data[["nSuccesses"]] == 0 && data[["nFailures"]] == 0))
      return()
    else {
      
      predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
      tempResults <- .testBinomialLS(data, options[["priors"]])
      
      dfHistAll   <- NULL
      xRange       <- c(0, predictionN)
      xName        <- gettext("Hypothesis")
      yName        <- gettext("Probability")
      
      if (options[["plotsPredictiveAccuracyType"]] == "conditional")
        tempY <- exp(tempResults[,"logLik"])
      else if (options[["plotsPredictiveAccuracyType"]] == "joint")
        tempY <- exp(tempResults[,"logLik"])*tempResults[,"prior"]       
      else if (options[["plotsPredictiveAccuracyType"]] == "marginal")
        tempY <- tempResults[,"posterior"]
      
      dfHistAll <- data.frame(
        "x" = 1:length(options[["priors"]]),
        "y" = tempY,
        "g" = sapply(options[["priors"]],function(x)x$name))
      
      if (any(is.nan(dfHistAll$y))){
        plotsPredAccuracy$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
        return()
      }
      
      p <- .plotAccuracyLS(dfHistAll, xName = xName, yName = yName)
      plotsPredAccuracy$plotObject <- p
      
    }
  }
  
  return()
}
.plotsIterativeOverlyingBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "binTest")
  
  if (is.null(containerSequentialTests[["plotsIterative"]])){
    
    plotsIterative <- createJaspPlot(width = 700, height = 400)
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.dataDependenciesBinomialLS, "colorPalette",
                              "bfTypeSequential", "bayesFactorTypeSequential", "bfTypevsNameSequential"))
    containerSequentialTests[["plotsIterative"]] <- plotsIterative
    
    if (length(data[["y"]]) == 0)
      return()
    if (!all(ready))
      return()
    
    if (options[["plotsIterativeType"]] == "BF"){
      if (options[["bfTypeSequential"]] == "vs" &&  options[["bfTypevsNameSequential"]] == ""){
        plotsIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if (length(options[["priors"]]) < 2){
        plotsIterative$setError("At least 2 hypotheses need to be specified.")
        return()
      }
      if (options[["bfTypeSequential"]] == "best")
        theBest <- which.max(.testBinomialLS(data, options[["priors"]])$logLik)
    }
    
    results <- NULL
    iterSeq <- 0:length(data[["y"]])
    
    for(i in iterSeq){
      
      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )
      
      tempResults <- .testBinomialLS(tempData, options[["priors"]])
      
      if (options[["plotsIterativeType"]] == "conditional"){
        yName  <- gettext("Conditional probability")
        tempY  <- exp(tempResults[,"logLik"])
      } else if (options[["plotsIterativeType"]] == "joint"){
        yName  <- gettext("Joint probability")
        tempY  <- exp(tempResults[,"logLik"])*tempResults[,"prior"]       
      } else if (options[["plotsIterativeType"]] == "marginal"){
        yName  <- gettext("Posterior probability")
        tempY  <- tempResults[,"posterior"]
      } else if (options[["plotsIterativeType"]] == "BF"){
        
        if (options[["bfTypeSequential"]] == "inclusion"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            (tempResults$posterior[h] / (1-tempResults$posterior[h])) / (tempResults$prior[h] / (1-tempResults$prior[h]))
          )
        } else if (options[["bfTypeSequential"]] == "best"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[theBest])
          )
        } else if (options[["bfTypeSequential"]] == "vs"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[sapply(options[["priors"]], function(p)p$name) == options[["bfTypevsNameSequential"]]])
          )
        }
        
        if (options[["bayesFactorTypeSequential"]] == "BF10")
          tempY <- tempBF
        else if (options[["bayesFactorTypeSequential"]] == "BF01")
          tempY <- 1/tempBF          
        else if (options[["bayesFactorTypeSequential"]] == "LogBF10")
          tempY <- log(tempBF)
        
        yName <- switch(
          options[["bayesFactorTypeSequential"]],
          "BF10"    = bquote("BF"["10"]),
          "BF01"    = bquote("BF"["01"]),
          "LogBF10" = bquote(italic("log")*"(BF)"["10"])
        )
      }
      
      results <- rbind.data.frame(results, tempY)
      
    }
    
    
    plotDataLines <- list()
    for(h in 1:length(options[["priors"]])){
      if (options[["plotsIterativeType"]] == "BF" && options[["bfTypeSequential"]] == "vs"){
        if (options[["bfTypevsNameSequential"]] == options[["priors"]][[h]]$name)next
      }
      
      tempLines   <- NULL
      tempLines   <- rbind(tempLines, data.frame(
        x    = iterSeq,
        y    = results[,h],
        name = options[["priors"]][[h]]$name
      ))
      plotDataLines <- c(plotDataLines, list(tempLines))
      
    }
    
    xName  <- gettext("Observation")
    
    if (options[["plotsIterativeType"]] == "BF")
      BFlog <- options[["bayesFactorTypeSequential"]] == "LogBF10"
    else
      BFlog <- NULL
    
    p <- .plotIterativeLS(plotDataLines, NULL, xName = xName, yName = yName, xStart = 0, palette = options[["colorPalette"]], BFlog = BFlog)
    
    plotsIterative$plotObject <- p
  }
  
  return()
}
.tableIterativeBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "binTest")
  
  if (is.null(containerSequentialTests[["tableIterative"]])){
    
    tableIterative <- createJaspTable()
    
    tableIterative$position <- 3
    tableIterative$dependOn(c(.dataDependenciesBinomialLS, "plotsIterativeUpdatingTable"))
    containerSequentialTests[["tableIterative"]] <- tableIterative
    
    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observations"), type = "integer")
    if (ready["priors"]){
      for(i in 1:length(options[["priors"]])){
        tableIterative$addColumnInfo(
          name  = options[["priors"]][[i]]$name,  
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }
    
    
    if (!all(ready))
      return()
    
    if (options[["plotsIterativeType"]] == "BF"){
      if (options[["bfTypeSequential"]] == "vs" &&  options[["bfTypevsNameSequential"]] == ""){
        tableIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if (length(options[["priors"]]) < 2){
        tableIterative$setError(gettext("At least 2 hypotheses need to be specified."))
        return()
      }
      if (options[["bfTypeSequential"]] == "best")
        theBest <- which.max(.testBinomialLS(data, options[["priors"]])$logLik)
    }
    
    
    results <- NULL
    
    if (length(data[["y"]]) > 1)
      iterSeq <- 1:length(data[["y"]])
    else
      iterSeq <- 1
    
    for(i in iterSeq){
      
      tempRow     <- list() 
      tempRow[["iteration"]] <- i
      
      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )
      tempResults <- .testBinomialLS(tempData, options[["priors"]])
      
      if (options[["plotsIterativeType"]] == "conditional")
        tempY <- exp(tempResults[,"logLik"])
      else if (options[["plotsIterativeType"]] == "joint")
        tempY <- exp(tempResults[,"logLik"])*tempResults[,"prior"]       
      else if (options[["plotsIterativeType"]] == "marginal")
        tempY <- tempResults[,"posterior"]
      else if (options[["plotsIterativeType"]] == "BF"){
        
        if (options[["bfTypeSequential"]] == "inclusion"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            (tempResults$posterior[h] / (1-tempResults$posterior[h])) / (tempResults$prior[h] / (1-tempResults$prior[h]))
          )
        } else if (options[["bfTypeSequential"]] == "best"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[theBest])
          )
        } else if (options[["bfTypeSequential"]] == "vs"){
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[sapply(options[["priors"]], function(p)p$name) == options[["bfTypevsNameSequential"]]])
          )
        }
        
        tempY <- switch(
          options[["bayesFactorTypeSequential"]],
          "BF10"    = tempBF,
          "BF01"    = 1/tempBF,
          "LogBF10" = log(tempBF)
        )
        
      }
      
      for(h in 1:length(options[["priors"]])){
        tempRow[[options[["priors"]][[h]]$name]] <- tempY[h]
      }
      
      tableIterative$addRows(tempRow)
    }
  }
  
  return()
}
.plotsBothBinomialLS2      <- function(jaspResults, data, ready, options){
  
  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "binTest")
  
  if (is.null(containerBoth[["plotsBoth"]])){
    
    plotsBoth <- createJaspContainer()
    
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.dataDependenciesBinomialLS, "plotsBothSampleProportion"))
    
    containerBoth[["plotsBoth"]] <- plotsBoth
    
    
    if (!all(ready))
      return()
    
    allLines    <- c()
    allArrows   <- c()
    legend       <- NULL
    tempResults <- .testBinomialLS(data, options[["priors"]])
    
    if (any(is.nan(tempResults$posterior))){
      plotsBothError <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
      plotsBoth[["plotsBothError"]] <- plotsBothError
      plotsBothError$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
      return()
    }
    
    for(i in 1:length(options[["priors"]])){
      
      if (options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPPprior       <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPPposterior   <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPPprior$g     <- "Prior"
        dfArrowPPposterior$g <- "Posterior"
        dfArrowPPprior$yEnd     <- exp(log(dfArrowPPprior$yEnd)     + log(tempResults[i, "prior"]))
        dfArrowPPposterior$yEnd <- exp(log(dfArrowPPposterior$yEnd) + log(tempResults[i, "posterior"]))
        
        allArrows      <- c(allArrows, list(rbind(dfArrowPPposterior, dfArrowPPprior)))
        
      } else if (options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP$y[dfLinesPP$g == "prior"]     <- exp(log(dfLinesPP$y[dfLinesPP$g == "prior"])+log(tempResults[i, "prior"]))
        dfLinesPP$y[dfLinesPP$g == "posterior"] <- exp(log(dfLinesPP$y[dfLinesPP$g == "posterior"])+log(tempResults[i, "posterior"]))
        
        allLines   <- c(allLines, list(dfLinesPP))
      }
    }
    
    if (options[["plotsBothSampleProportion"]]){
      dfPointsPP <- .dataProportionBinomialLS(data)
      if (is.nan(dfPointsPP$x))dfPointsPP <- NULL
    } else
      dfPointsPP <- NULL
    
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if (options[["plotsBothType"]] == "joint"){
      
      spikesI <- 1
      betasI  <- 1
      
      for(i in 1:length(options[["priors"]])){
        tempPlotsBoth <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        plotsBoth[[paste0("plotsBoth_",i)]] <- tempPlotsBoth
        
        if (options[["priors"]][[i]]$type == "spike"){
          tempP   <- .plotPriorPosteriorLS(NULL, allArrows[spikesI], dfPoints = dfPointsPP, xName = xName)
          spikesI <- spikesI + 1
        } else if (options[["priors"]][[i]]$type == "beta"){
          tempP   <- .plotPriorPosteriorLS(allLines[betasI], NULL, dfPoints = dfPointsPP, xName = xName)
          betasI  <- betasI + 1        
        }
        
        tempPlotsBoth$plotObject <- tempP
      }
      
      
    } else if (options[["plotsBothType"]] == "marginal"){
      
      plotsBothPlot <- createJaspPlot(width = 700, height = 400)
      plotsBoth[["plotsBothPlot"]] <- plotsBothPlot
      
      allLinesNew <- c()
      
      if (length(allLines) > 0){
        
        for(i in 1:length(allLines)){
          
          if (i == 1){
            allLinesNew[[1]] <- allLines[[i]]
          } else {
            allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y
          }
          
        }
      }
      
      p <- .plotPriorPosteriorLS(allLinesNew, allArrows, dfPoints = dfPointsPP, xName = xName)
      plotsBothPlot$plotObject <- p
      
    }
  }
  
  return()
}
.plotsBothIndividualBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "binTest")
  
  if (is.null(containerBoth[["plotsBoth"]])){
    
    plotsBoth <- createJaspContainer()
    
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.dataDependenciesBinomialLS, "plotsBothSampleProportion"))
    
    containerBoth[["plotsBoth"]] <- plotsBoth
    
    
    if (all(!ready) || (ready["data"] && !ready["priors"])){
      
      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready["data"] && ready["priors"]){
      
      for(i in 1:length(options[["priors"]])){
        plotsBoth[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 700, height = 400)
        
        plotsBoth[[options[["priors"]][[i]]$name]] <- tempPlot
        
        dfArrowPP <- NULL
        dfLinesPP <- NULL
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        if (options[["priors"]][[i]]$type == "spike")
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
        else if (options[["priors"]][[i]]$type == "beta"){
          dfLinesPP  <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
          
          if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])){
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- "Prior = Posterior"
          }
          
        }
        
        if (options[["plotsBothSampleProportion"]]){
          dfPointsPP <- .dataProportionBinomialLS(data)
          if (is.nan(dfPointsPP$x))dfPointsPP <- NULL
        } else
          dfPointsPP <- NULL 
        
        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName)
        tempPlot$plotObject <- p
      }
    }
  }
  
  return()
}
.tablePredictionsBinomialLS2    <- function(jaspResults, data, ready, options){
  
  containerPredictions <- .containerPredictionsLS(jaspResults, options, "binTest")
  
  if (is.null(containerPredictions[["predictionsTable"]])){
    
    predictionsTable <- createJaspTable()
    
    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.dataDependenciesBinomialLS, "predictionN", "predictionTableEstimate"))
    
    estimateText <- .estimateTextLS(options[["predictionTableEstimate"]])
    
    predictionsTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),                         type = "string")
    predictionsTable$addColumnInfo(name = "posterior",     title = gettextf("Posterior (%s)", "\u03B8"),     type = "string")
    predictionsTable$addColumnInfo(name = "prob",          title = gettext("P(H|data)"),                     type = "number")
    predictionsTable$addColumnInfo(name = "posteriorEst",  title = gettextf("Posterior %s", estimateText),   type = "number")
    predictionsTable$addColumnInfo(name = "predictive",    title = gettextf("Prediction (Successes)"),       type = "string")
    predictionsTable$addColumnInfo(name = "predictiveEst", title = gettextf("Prediction %s", estimateText),  type = "number")
    
    predictionsTable$setExpectedSize(length(options[["priors"]]))
    
    containerPredictions[["predictionsTable"]] <- predictionsTable
    
    if (ready["data"] && !ready["priors"])
      return()
    else if (!ready["data"]){
      
      if ((options[["dataType"]] == "dataVariable" && options[["selectedVariable"]] != "") ||
          (options[["dataType"]] == "dataSequence" && options[["dataSequenceInput"]]    != ""))
        predictionsTable$addFootnote(gettext("Please specify successes and failures."))
      
      return()
      
    } else {
      
      tempTests <- .testBinomialLS(data, options[["priors"]])
      tempMeans <- NULL
      margEst   <- .predictionTableEstimate(data, options, options[["predictionTableEstimate"]])
      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){
        
        tempResults    <- .estimateBinomialLS(data, options[["priors"]][[i]])
        tempPrediction <- .predictBinomialLS(data, options[["priors"]][[i]], options)
        
        tempRow <- list(
          hypothesis      = options[["priors"]][[i]][["name"]],
          posterior       = tempResults[["distribution"]],
          prob            = tempTests[i, "posterior"],
          posteriorEst    = tempResults[[options[["predictionTableEstimate"]]]],
          predictive      = tempPrediction[["distribution"]],
          predictiveEst   = tempPrediction[[options[["predictionTableEstimate"]]]]
        )
        
        predictionsTable$addRows(tempRow)
      }
      
      predictionsTable$addRows(list(
        hypothesis     = "Marginal",
        posteriorEst   = margEst[["posteriorEst"]],
        predictiveEst  = margEst[["predictionEst"]]
      ))
      
      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %s %s is based on %s %s and %s %s.",
        options[["predictionN"]], ifelse(options[["predictionN"]] == 1, gettext("observation"), gettext("observations")),
        data[["nSuccesses"]], ifelse(data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]], ifelse(data[["nFailures"]] == 1, gettext("failure"), gettext("failures"))
      ))
      
    }
  }
  
  return()  
}
.predictionTableEstimate <- function(data, options, estimate){
  
  ### posterior estimate
  allLines    <- c()
  allArrows   <- c()
  legend       <- NULL
  tempResults <- .testBinomialLS(data, options[["priors"]])
  for(i in 1:length(options[["priors"]])){
    
    if (options[["priors"]][[i]]$type == "spike"){
      
      dfArrowPP       <- .dataArrowBinomialLS(options[["priors"]][[i]])
      dfArrowPP$yEnd <- exp(log(dfArrowPP$yEnd)+log(tempResults[i, "posterior"]))
      dfArrowPP$g     <- options[["priors"]][[i]]$name
      
      allArrows      <- c(allArrows, list(dfArrowPP))
      legend          <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
      
    } else if (options[["priors"]][[i]]$type == "beta"){
      
      dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(tempResults[i, "posterior"]))
      dfLinesPP$g <- options[["priors"]][[i]]$name
      
      allLines   <- c(allLines, list(dfLinesPP))
      legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
      
    }
  }
  
  allLinesNew <- c()
  allSpikes    <- list()
  if (length(allLines) > 0){
    
    for(i in 1:length(allLines)){
      
      if (i == 1){
        allLinesNew[[1]] <- allLines[[i]]
      } else {
        allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y
      }
      
    }
    allLinesNew[[1]]$g <- "__marginal"
  }
  
  if (length(allArrows) > 0){
    for(i in 1:length(allArrows)){
      allArrows[[i]]$g <- "__marginal"
    }
  }
  
  tempResults <- .testBinomialLS(data, options[["priors"]])
  for(i in 1:length(options[["priors"]])){
    if (options[["priors"]][[i]]$type == "spike"){
      allSpikes <- c(
        allSpikes, 
        list(data.frame(y = tempResults$posterior[i], x = options[["priors"]][[i]]$parPoint, g = "__marginal"))
      )
    }
  }
  
  posteriorEst <- .dataPointMarginalBinomial(data, options, allLinesNew[[1]], allSpikes, N = options[["predictionN"]],
                                             type = "parameter", type2 = "Posterior",
                                             estimate = estimate)$x
  
  ### prediction estimate
  tempResults <- .testBinomialLS(data, options[["priors"]])
  allLines  <- c()
  legend     <- NULL
  
  for(i in 1:length(options[["priors"]])){
    
    dfHist   <- .dataHistBinomialLS2(data, options[["priors"]][[i]], options[["predictionN"]])
    dfHist$g <- options[["priors"]][[i]]$name
    dfHist$y <- dfHist$y*tempResults[i,"posterior"]
    
    
    # it's not beta, but I'm lazzy to rewrite a function I wanna use
    legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
    allLines<- c(allLines, list(dfHist))
  }
  
  if (length(allLines) > 0){
    
    for(i in 1:length(allLines)){
      
      if (i == 1){
        allLinesNew <- allLines[[i]]
      } else {
        allLinesNew$y <- allLinesNew$y + allLines[[i]]$y
      }
      
    }
    allLinesNew$g <- "__marginal"
  }
  
  allLinesNew   <- allLinesNew[seq(1,nrow(allLinesNew),2),]
  allLinesNew$x <- allLinesNew$x + .5
  
  predictionEst <- .dataPointMarginalBinomial(data, options, allLinesNew, NULL, N = options[["predictionN"]],
                                              type = "prediction", type2 = "Posterior",
                                              estimate = estimate)$x
  
  return(list(
    posteriorEst  = posteriorEst,
    predictionEst = predictionEst
  ))  
}