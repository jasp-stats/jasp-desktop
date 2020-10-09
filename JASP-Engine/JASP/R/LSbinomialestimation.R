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



LSbinomialestimation   <- function(jaspResults, dataset, options, state = NULL){
  
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)
  
  # introductory text
  if (options[["introText"]])
    .introductoryTextLS(jaspResults, options, "binEst")
  
  # evaluate the expressions in priors
  if (ready["priors"])
    options[["priors"]] <- .evaluatePriors(options[["priors"]])
  
  # load, check, transform and process data
  if (ready["data"])
    data <- .readDataBinomialLS(dataset, options)
  
  # data summary table ifrequested (but not ifthe data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "binEst")
  
  
  ### inference 
  # estimated parameter values
  .estimatesBinomialLS(jaspResults, data, ready, options)
  
  # prior
  if (options[["plotsPrior"]]){
    if (options[["plotsPriorType"]] != "individual")
      .plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Prior")
    if (options[["plotsPriorType"]] == "individual")
      .plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # posterior
  if (options[["plotsPosterior"]]){
    if (options[["plotsPosteriorType"]] != "individual")
      .plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["plotsPosteriorType"]] == "individual").plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if (options[["plotsBoth"]])
    .plotsBothBinomialLS(jaspResults, data, ready, options)
  
  ### sequential analysis
  # point estimate
  if (options[["plotsIterative"]]){
    if (options[["plotsIterativeType"]] == "overlying")
      .plotsIterativeOverlyingBinomialLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeType"]] == "stacked")
      .plotsIterativeStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # point estimate table
  if (options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]])
    .tableIterativeBinomialLS(jaspResults, data, ready, options)
  
  # interval
  if (options[["plotsIterativeInterval"]]){
    if (options[["plotsIterativeIntervalType"]] == "overlying")
      .plotsIterativeIntervalOverlyingBinomialLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeIntervalType"]] == "stacked")
      .plotsIterativeIntervalStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # interval estimate table
  if (options[["plotsIterativeInterval"]] && options[["plotsIterativeIntervalUpdatingTable"]])
    .tableIterativeIntervalBinomialLS(jaspResults, data, ready, options)
  
  # posterior updating table
  if (options[["doIterative"]] && options[["dataType"]] != "dataCounts")
    .estimatesSequentialBinomialLS(jaspResults, data, ready, options)
  
  
  ### prediction
  if (options[["predictionTable"]])
    .tablePredictionsBinomialLS(jaspResults, data, ready, options)
  
  # plot
  if (options[["plotsPredictions"]]){
    if (options[["predictionPlotType"]] != "individual")
      .plotsPredictionsBinomialLS(jaspResults, data, ready, options)
    if (options[["predictionPlotType"]] == "individual")
      .plotsPredictionsIndividualBinomialLS(jaspResults, data, ready, options)
    if (options[["predictionPlotTable"]])
      .tablePosteriorPredictions(jaspResults, data, ready, options)
  }
  
  return()
}

# main functions
.estimatesBinomialLS               <- function(jaspResults, data, ready, options){
  
  estimatesContainer <- .estimatesContainerLS(jaspResults, options, "binEst")
  
  if (is.null(estimatesContainer[['estimatesTable']])){
    
    estimatesTable <- createJaspTable(title = gettextf("Estimation Summary"))
    
    estimatesTable$position <- 2
    estimatesTable$dependOn(.dataDependenciesBinomialLS)
    
    estimateText <- .estimateTextLS(options[["pointEstimate"]])
    
    estimatesTable$addColumnInfo(name = "hypothesis",   title = gettext("Model"),                       type = "string")
    estimatesTable$addColumnInfo(name = "prior",        title = gettextf("Prior (%s)", "\u03B8"),       type = "string")
    estimatesTable$addColumnInfo(name = "priorEst",     title = gettextf("Prior %s", estimateText),     type = "number")
    estimatesTable$addColumnInfo(name = "posterior",    title = gettextf("Posterior (%s)", "\u03B8"),   type = "string")
    estimatesTable$addColumnInfo(name = "posteriorEst", title = gettextf("Posterior %s", estimateText), type = "number")
    
    estimatesContainer[["estimatesTable"]] <- estimatesTable
    
    if (ready["data"] && !ready["priors"])
      return()
    else if (!ready["data"]){
      
      if ((options[["dataType"]] == "dataVariable" && options[["selectedVariable"]]  != "") ||
          (options[["dataType"]] == "dataSequence" && options[["dataSequenceInput"]] != ""))
        estimatesTable$addFootnote(gettext("Please specify successes and failures."))
      
      return()
      
    } else if (ready["priors"]){
      
      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){
        # add mock data to use only priors
        tempData <- list(
          "nSuccesses" = 0,
          "nFailures"  = 0
        )
        tempResults <- .estimateBinomialLS(tempData, options[["priors"]][[i]])
        
        tempRow <- list(
          prior        = tempResults[["distribution"]],
          priorEst     = tempResults[[options[["pointEstimate"]]]],
          hypothesis   = options[["priors"]][[i]][["name"]], 
          posterior    = "",
          posteriorEst = "")
        
        
        if (all(ready)){
          # and when real data are supplied as well, add posterior information
          tempResults <- .estimateBinomialLS(data, options[["priors"]][[i]])
          
          tempRow["posterior"]    <- tempResults[["distribution"]]
          tempRow["posteriorEst"] <- tempResults[[options[["pointEstimate"]]]]
          
        }
        
        estimatesTable$addRows(tempRow)
      }
      
      # add footnote clarifying what dataset was used
      estimatesTable$addFootnote(gettextf(
        "These results are based on %i %s and %i %s.",
        data[["nSuccesses"]], ifelse(data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]],  ifelse(data[["nFailures"]]  == 1, gettext("failure"), gettext("failures"))
      ))
      
    }
  }
  
  return()
}
.estimatesSequentialBinomialLS     <- function(jaspResults, data, ready, options){
  
  containerIterativeUpdating <- .containerSequentialUpdatingLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterativeUpdating[["estimatesSequentialTable"]])){
    
    estimatesSequentialTable <- createJaspTable()
    
    estimatesSequentialTable$position <- 2
    estimatesSequentialTable$dependOn(.dataDependenciesBinomialLS)
    
    estimatesSequentialTable$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    containerIterativeUpdating[["estimatesSequentialTable"]] <- estimatesSequentialTable
    
    
    estimatesSequentialTable$setExpectedSize(ifelse(ready["data"], length(data[["y"]]) + 1, 1))
    if (ready["priors"]){
      for(i in 1:length(options[["priors"]])){
        estimatesSequentialTable$addColumnInfo(
          name  = options[["priors"]][[i]]$name,  
          title = options[["priors"]][[i]]$name,
          type = "string")
      }
    }
    
    
    if (!all(ready))
      return()
    else {
      # add priors to the first row
      tempRow <- NULL
      tempRow[["iteration"]] <- 0
      for(h in 1:length(options[["priors"]])){
        tempData    <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
        tempResults <- .estimateBinomialLS(tempData, options[["priors"]][[h]])
        tempRow[[options[["priors"]][[h]]$name]] <- tempResults$distribution
      }
      estimatesSequentialTable$addRows(tempRow)
      
      # then update the posteriors as the data go in
      if (length(data[["y"]]) > 0){
        for(i in 1:length(data[["y"]])){
          tempRow <- NULL
          tempRow[["iteration"]] <- i
          for(h in 1:length(options[["priors"]])){
            tempData    <- list(
              nSuccesses = sum(data[["y"]][1:i] == 1),
              nFailures  = sum(data[["y"]][1:i] == 0)
            )
            tempResults <- .estimateBinomialLS(tempData, options[["priors"]][[h]])
            tempRow[[options[["priors"]][[h]]$name]] <- tempResults$distribution
          }
          estimatesSequentialTable$addRows(tempRow)
        }
      }
    }
  }
  
  return()
}
.plotsSimpleBinomialLS             <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlotsLS(jaspResults, options, "binEst", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsSimple <- createJaspPlot(
      width  = if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying") 700 else 530,
      height = 400)
    
    plotsSimple$position <- 2
    plotsSimple$dependOn(c(
      .dataDependenciesBinomialLS,
      ifelse(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying", "colorPalette", "")))
    
    containerPlots[[paste0("plots",type)]] <- plotsSimple
    
    if (!all(ready))return()
    
    allLines  <- c()
    allArrows <- c()
    legend    <- NULL
    for(i in 1:length(options[["priors"]])){
      
      if (options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP$g <- options[["priors"]][[i]]$name
        
        allArrows   <- c(allArrows, list(dfArrowPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      } else if (options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$g <- options[["priors"]][[i]]$name
        
        allLines    <- c(allLines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      }
    }
    
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying")
      p <- .plotOverlyingLS(allLines, allArrows, xName = xName, palette = options[["colorPalette"]])
    else
      p <- .plotStackedLS(allLines, allArrows, legend, xName = xName)
    
    plotsSimple$plotObject <- p
  }
  
  return()
}
.plotsIndividualBinomialLS         <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlotsLS(jaspResults, options, "binEst", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsIndividual <- createJaspContainer()
    
    plotsIndividual$position <- 2
    plotsIndividual$dependOn(c(.dataDependenciesBinomialLS,
                               ifelse(type == "Prior", "plotsPriorIndividualEstimate",     "plotsPosteriorIndividualEstimate"),
                               ifelse(type == "Prior", "plotsPriorIndividualEstimateType", "plotsPosteriorIndividualEstimateType"),
                               ifelse(type == "Prior", "plotsPriorIndividualCI",   "plotsPosteriorIndividualCI"),
                               ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType"),
                               ifelse(type == "Prior", "plotsPriorCoverage",       "plotsPosteriorCoverage"),
                               ifelse(type == "Prior", "plotsPriorLower",          "plotsPosteriorLower"),
                               ifelse(type == "Prior", "plotsPriorUpper",          "plotsPosteriorUpper")))
    
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
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIndividual[[options[["priors"]][[i]]$name]] <- tempPlot
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL
        
        if (options[[ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]){
          
          if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(
              tempData,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )
            
          } else if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(
              tempData,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )
            
          } else if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(
              tempData, options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
              options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]],
              type = "parameter"
            )  
            
          } else if (options[["plotsPosteriorIndividualType"]] == "support"){
            
            dfCI <- .dataSupportBinomialLS(
              tempData,
              options[["priors"]][[i]],
              options[["plotsPosteriorBF"]]
            )  
            
          }
        }
        
        
        if (options[["priors"]][[i]]$type == "spike")
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
        else if (options[["priors"]][[i]]$type == "beta"){
          
          dfLinesPP  <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
          dfLinesPP  <- dfLinesPP[dfLinesPP$g == type,]
          
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
        
        if (options[[ifelse(type == "Prior", "plotsPriorIndividualEstimate", "plotsPosteriorIndividualEstimate")]]){
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["priors"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse(type == "Prior", "plotsPriorIndividualEstimateType", "plotsPosteriorIndividualEstimateType")]])
        } else
          dfPointEstimate <- NULL
        
        
        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfPointEstimate, dfCI, dfCILinesPP, NULL, c(0,1), xName, nRound = 3)
        tempPlot$plotObject <- p
      }
    }
    
  }
  
  return()
}
.plotsBothBinomialLS               <- function(jaspResults, data, ready, options){
  
  containerBoth <- .containerPlotsBothLS(jaspResults, options, "binEst")
  
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
        
        if (options[["priors"]][[i]]$type == "spike"){
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
        } else if (options[["priors"]][[i]]$type == "beta"){
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
.plotsIterativeOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterative[["plotsIterative"]])){
    
    plotsIterative <- createJaspPlot(width = 700, height = 400)
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.dataDependenciesBinomialLS, "plotsIterativeEstimateType",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "plotsIterativeIndividualType", "plotsIterativeBF",
                              "colorPalette"))
    containerIterative[["plotsIterative"]] <- plotsIterative
    
    if (!all(ready))
      return()
    
    plotDataLines <- list()
    plotDataCI    <- list()
    
    # then update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){
      
      tempLines   <- NULL
      tempCI      <- NULL
      # for dealing with possible bimodal distributions from HPD
      CIunimodal  <- TRUE
      tempCI1     <- NULL
      tempCI2     <- NULL
      
      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data[["y"]]) == 0)
        iterSeq <- c(0, 0.1)
      else
        iterSeq <- 0:length(data[["y"]])
      
      for(i in iterSeq){
        
        tempData    <- list(
          nSuccesses = sum(data[["y"]][0:i] == 1),
          nFailures  = sum(data[["y"]][0:i] == 0)
        )
        
        tempResults    <- .estimateBinomialLS(tempData, options[["priors"]][[h]])
        tempLines      <- rbind(tempLines, data.frame(
          y    = as.numeric(tempResults[[options[["plotsIterativeEstimateType"]]]]),
          x    = i,
          name = options[["priors"]][[h]]$name
        ))
        
        if (options[["plotsIterativeIndividualCI"]]){
          
          if (options[["plotsIterativeIndividualType"]] == "central"){
            tempCIPP <- .dataCentralBinomialLS(tempData, options[["priors"]][[h]],
                                                options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "HPD"){
            
            tempCIPP <- .dataHPDBinomialLS(tempData, options[["priors"]][[h]],
                                            options[["plotsIterativeCoverage"]], type = "parameter")
            if (nrow(tempCIPP) == 2)CIunimodal <- FALSE
            
          } else if (options[["plotsIterativeIndividualType"]] == "support"){
            
            tempCIPP <- .dataSupportBinomialLS(tempData, options[["priors"]][[h]],
                                                options[["plotsIterativeBF"]])
            if (nrow(tempCIPP) == 0)tempCIPP <- NULL
            
          }
          
          if (nrow(tempCIPP) == 1 && CIunimodal){
            
            tempCI <- rbind(tempCI, data.frame(
              y1   = tempCIPP$xStart,
              y2   = tempCIPP$xEnd,
              x    = i,
              name = options[["priors"]][[h]]$name
            ))
            
          } else if (nrow(tempCIPP) == 1 && !CIunimodal){
            
            tempCI <- rbind(
              tempCI,
              data.frame(
                y1   = (tempCIPP$xStart + tempCIPP$xEnd)/2,
                y2   = (tempCIPP$xStart + tempCIPP$xEnd)/2,
                x    = i,
                name = tempCI1$name
              ),
              data.frame(
                y1   = c(tempCI1$y2, tempCI1$y1),
                y2   = c(tempCI2$y1, tempCI2$y2),
                x    = rep(tempCI1$x, 2),
                name = rep(tempCI1$name, 2)
              ),
              data.frame(
                y1   = tempCIPP$xStart,
                y2   = tempCIPP$xEnd,
                x    = i,
                name = options[["priors"]][[h]]$name
              )
            )
            CIunimodal <- TRUE
            
          } else if (nrow(tempCIPP) == 2){
            
            tempCI1 <- rbind(
              tempCI1,
              data.frame(
                y1   = tempCIPP$xStart[1],
                y2   = tempCIPP$xEnd[1],
                x    = i,
                name = options[["priors"]][[h]]$name
              ))
            
            tempCI2 <- rbind(
              tempCI2,
              data.frame(
                y1   = tempCIPP$xStart[2],
                y2   = tempCIPP$xEnd[2],
                x    = i,
                name = options[["priors"]][[h]]$name
              ))
            
          } else if (nrow(tempCIPP) > 2)
            JASP:::.quitAnalysis(gettext("More than bimodal CIs are not implemented in the Sequential analysis plot."))
        }
        
      }
      
      plotDataLines <- c(plotDataLines, list(tempLines))
      
      # deal with a possibility of two disjoined CIs
      if (options[["plotsIterativeIndividualCI"]]){
        if (CIunimodal){
          # deal with possible non-existing support intervals
          if (all(is.na(tempCI[,c("y1", "y2")])))
            plotDataCI    <- c(plotDataCI, list(NULL))          
          else
            plotDataCI    <- c(plotDataCI, list(tempCI))
        } else
          plotDataCI    <- c(plotDataCI, list(tempCI1), list(tempCI2))
      }
      
    }
    
    yName  <- bquote(.(gettext("Population proportion"))~~theta)
    xName  <- gettext("Observation")
    
    p <- .plotIterativeLS(plotDataLines, plotDataCI, xName = xName, yName = yName, palette = options[["colorPalette"]])
    
    plotsIterative$plotObject <- p
    
  }
  
  return()
}
.plotsIterativeStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "binEst") 
  
  if (is.null(containerIterative[["plotsIterative"]])){
    plotsIterative <- createJaspContainer()
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(.dataDependenciesBinomialLS)
    
    containerIterative[["plotsIterative"]] <- plotsIterative
    
    
    if (all(!ready) || (ready["data"] && !ready["priors"])){
      
      plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready["data"] && ready["priors"]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIterative[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                          width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      #options[["priors"]][[i]]$name
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIterative[[options[["priors"]][[i]]$name]] <- tempPlot
        
        allLines  <- c()
        allArrows <- c()
        legend    <- NULL
        
        # too many iterations crashes JASP
        if (length(data[["y"]]) > 10)
          iterSequence <- round(seq(0, length(data[["y"]]), length.out = 10))
        else
          iterSequence <- 0:length(data[["y"]])
        
        iterSequence <- rev(iterSequence)
        
        for(iteration in iterSequence){
          
          if (options[["priors"]][[i]]$type == "spike"){
            
            dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)
            
            allArrows  <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          } else if (options[["priors"]][[i]]$type == "beta"){
            
            tempData <- list(
              "nSuccesses" = sum(data[["y"]][0:iteration] == 1),
              "nFailures"  = sum(data[["y"]][0:iteration] == 0)
            )
            
            dfLinesPP   <- .dataLinesBinomialLS(tempData, options[["priors"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)
            
            allLines    <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          }
          
        }
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName)
      }
    } 
  }
  
  return()
}
.plotsIterativeIntervalOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])){
    
    plotsIterativeInterval <- createJaspPlot(width = 700, height = 400)
    
    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval
    
    if (!all(ready))
      return()
    
    
    plotDataLines <- list()
    
    # update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){
      
      tempLines   <- NULL
      
      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data[["y"]]) == 0)
        iterSeq <- c(0, 0.1)
      else
        iterSeq <- 0:length(data[["y"]])
      
      
      for(i in iterSeq){
        
        tempData    <- list(
          nSuccesses = sum(data[["y"]][0:i] == 1),
          nFailures  = sum(data[["y"]][0:i] == 0)
        )
        
        tempResults    <- .dataCustomBinomialLS(tempData, options[["priors"]][[h]],
                                                 lCI = options[["plotsIterativeIntervalLower"]],
                                                 uCI = options[["plotsIterativeIntervalUpper"]],
                                                 type = c("parameter"))
        
        tempLines      <- rbind(tempLines, data.frame(
          y    = tempResults$coverage,
          x    = i,
          name = options[["priors"]][[h]]$name
        ))
        
      }
      
      plotDataLines <- c(plotDataLines, list(tempLines))
      
    }
    
    yName  <- bquote("P("~{.(options[["plotsIterativeIntervalLower"]])<=theta}<=.(options[["plotsIterativeIntervalUpper"]])~")")
    xName  <- gettext("Observation")
    
    p <- .plotIterativeLS(plotDataLines, allCI = NULL, xName = xName, yName = yName, palette = options[["colorPalette"]])
    
    plotsIterativeInterval$plotObject <- p
  }
  
  return()
}
.plotsIterativeIntervalStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])){
    
    plotsIterativeInterval <- createJaspContainer()
    
    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    
    
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval
    
    
    if (all(!ready) || (ready["data"] && !ready["priors"])){
      
      plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready["data"] && ready["priors"]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                  width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      #options[["priors"]][[i]]$name
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- tempPlot
        
        allLines  <- c()
        allArrows <- c()
        legend     <- NULL
        
        # too many iterations crashes JASP
        if (length(data[["y"]]) > 10)
          iterSequence <- round(seq(0, length(data[["y"]]), length.out = 10))
        else
          iterSequence <- 0:length(data[["y"]])
        
        iterSequence <- rev(iterSequence)
        
        for(iteration in iterSequence){
          
          if (options[["priors"]][[i]]$type == "spike"){
            
            dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)
            
            allArrows   <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          } else if (options[["priors"]][[i]]$type == "beta"){
            
            tempData <- list(
              "nSuccesses" = sum(data[["y"]][0:iteration] == 1),
              "nFailures"  = sum(data[["y"]][0:iteration] == 0)
            )
            
            dfLinesPP   <- .dataLinesBinomialLS(tempData, options[["priors"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)
            
            allLines    <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          }
          
        }
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName,
                                               lCI = options[["plotsIterativeIntervalLower"]], uCI = options[["plotsIterativeIntervalUpper"]])
      }
    }
  }
  
  return()
}
.tableIterativeBinomialLS          <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterative[["tableIterative"]])){
    tableIterative <- createJaspTable()
    
    tableIterative$position <- 3
    tableIterative$dependOn(c(.dataDependenciesBinomialLS, "plotsIterativeEstimateType",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "plotsIterativeIndividualType", "plotsIterativeBF",
                              "colorPalette", "plotsIterativeUpdatingTable"))
    containerIterative[["tableIterative"]] <- tableIterative
    
    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["priors"]){
      if (options[["plotsIterativeIndividualCI"]]){
        if (options[["plotsIterativeIndividualType"]] == "central")
          CItitle <- gettextf("%i %% CI", options[["plotsIterativeCoverage"]]*100)
        else if (options[["plotsIterativeIndividualType"]] == "HPD")
          CItitle <- gettextf("%i %% HPD", options[["plotsIterativeCoverage"]]*100)
        else if (options[["plotsIterativeIndividualType"]] == "support")
          CItitle <- gettextf("SI (BF=%s)", options[["plotsIterativeBF"]])
        
        for(i in 1:length(options[["priors"]])){
          tableIterative$addColumnInfo(
            name      = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title     = .estimateTextLS(options[["plotsIterativeEstimateType"]]),
            overtitle = options[["priors"]][[i]]$name,
            type      = "number")
          tableIterative$addColumnInfo(
            name      = paste(options[["priors"]][[i]]$name,"CI", sep = "_"),
            title     = CItitle,
            overtitle = options[["priors"]][[i]]$name,
            type      = "string")
        }
      } else {
        for(i in 1:length(options[["priors"]])){
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),  
            title = options[["priors"]][[i]]$name,
            type = "number")
        }
      }
    }
    
    if (!all(ready))
      return()
    
    iterSeq <- 0:length(data[["y"]])
    
    for(i in iterSeq){
      
      tempRow     <- list() 
      tempRow[["iteration"]] <- i
      
      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )
      
      for(h in 1:length(options[["priors"]])){
        
        tempResults <- .estimateBinomialLS(tempData, options[["priors"]][[h]])
        tempRow[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- tempResults[[options[["plotsIterativeEstimateType"]]]]
        
        if (options[["plotsIterativeIndividualCI"]]){
          
          if (options[["plotsIterativeIndividualType"]] == "central"){
            tempCIPP <- .dataCentralBinomialLS(tempData, options[["priors"]][[h]],
                                                options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "HPD"){
            tempCIPP <- .dataHPDBinomialLS(tempData, options[["priors"]][[h]],
                                            options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "support"){
            tempCIPP <- .dataSupportBinomialLS(tempData, options[["priors"]][[h]],
                                                options[["plotsIterativeBF"]])
          }
          
          if (all(is.na(tempCIPP[1:2]))){
            tempInt <- "\u2205"
          } else {
            tempInt <- sapply(1:nrow(tempCIPP), function(i)paste(c(
              "[",format(round(tempCIPP$xStart[i], 3), nsmall = 3),", ",format(round(tempCIPP$xEnd[i], 3), nsmall = 3),"]"
            ), collapse = ""))
            tempInt <- paste(tempInt, collapse = " and " )
            
            tempRow[[paste(options[["priors"]][[h]]$name,"CI", sep = "_")]] <- tempInt
          }
          
        }
        
      }
      
      tableIterative$addRows(tempRow)
      
    } 
  }
  
  return()
}
.tableIterativeIntervalBinomialLS  <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")
  
  if (is.null(containerIterativeInterval[["tableIterativeInterval"]])){
    
    tableIterativeInterval <- createJaspTable()
    
    tableIterativeInterval$position <- 3
    tableIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "plotsIterativeIntervalUpdatingTable"))
    containerIterativeInterval[["tableIterativeInterval"]] <- tableIterativeInterval
    
    tableIterativeInterval$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["priors"]){
      for(i in 1:length(options[["priors"]])){
        tableIterativeInterval$addColumnInfo(
          name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),  
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }
    
    if (!all(ready))
      return()
    
    iterSeq <- 0:length(data[["y"]])
    
    for(i in iterSeq){
      
      tempRow     <- list() 
      tempRow[["iteration"]] <- i
      
      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )
      
      for(h in 1:length(options[["priors"]])){
        
        tempResults    <- .dataCustomBinomialLS(tempData, options[["priors"]][[h]],
                                                 lCI = options[["plotsIterativeIntervalLower"]], uCI = options[["plotsIterativeIntervalUpper"]],
                                                 type = c("parameter"))
        tempRow[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- tempResults$coverage
      }
      
      tableIterativeInterval$addRows(tempRow)
    }  
  }
  
  return()
}
.tablePredictionsBinomialLS        <- function(jaspResults, data, ready, options){
  
  containerPredictions <- .containerPredictionsLS(jaspResults, options, "binEst")
  
  if (is.null(containerPredictions[["predictionsTable"]])){
    
    predictionsTable <- createJaspTable()
    
    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.dataDependenciesBinomialLS, "predictionN"))
    
    estimateText <- .estimateTextLS(options[["predictionTableEstimate"]])
    
    predictionsTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),                        type = "string")
    predictionsTable$addColumnInfo(name = "posterior",     title = gettextf("Posterior (%s)", "\u03B8"),    type = "string")
    predictionsTable$addColumnInfo(name = "posteriorEst",  title = gettextf("Posterior %s", estimateText),  type = "number")
    predictionsTable$addColumnInfo(name = "predictive",    title = gettext("Prediction (Successes)"),       type = "string")
    predictionsTable$addColumnInfo(name = "predictiveEst", title = gettextf("Prediction %s", estimateText), type = "number")
    predictionsTable$addColumnInfo(name = "predictiveSD",  title = gettextf("Prediction std. deviation"),   type = "number")
    
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
      
      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){
        
        tempResults    <- .estimateBinomialLS(data, options[["priors"]][[i]])
        tempPrediction <- .predictBinomialLS(data, options[["priors"]][[i]], options)
        
        tempRow <- list(
          hypothesis     = options[["priors"]][[i]][["name"]],
          posterior      = tempResults[["distribution"]],
          posteriorEst   = tempResults[[options[["predictionTableEstimate"]]]],
          predictive     = tempPrediction[["distribution"]],
          predictiveEst  = tempPrediction[[options[["predictionTableEstimate"]]]],
          predictiveSD   = tempPrediction[["SD"]]
        )
        
        predictionsTable$addRows(tempRow)
      }
      
      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %s %s is based on %s %s and %s %s.",
        options[["predictionN"]], ifelse(options[["predictionN"]] == 1, gettext("observation"), gettext("observations")),
        data[["nSuccesses"]], ifelse(data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]],  ifelse(data[["nFailures"]] == 1,  gettext("failure"), gettext("failures"))
      ))
    }
  }
  
  return()  
}
.plotsPredictionsIndividualBinomialLS      <- function(jaspResults, data, ready, options){
  
  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")
  
  if (is.null(containerPredictionPlots[["plotsPredictions"]])){
    
    plotsPredictions <- createJaspContainer()
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesBinomialLS, "predictionN",
                                "plotsPredictionCI", "plotsPredictionType",
                                "plotsPredictionEstimate", "plotsPredictionEstimateType",
                                "plotsPredictionCoverage", "plotsPredictionLower", "plotsPredictionUpper",
                                "predictionPlotProp"))
    
    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions
    
    
    if (!ready["priors"]){
      
      plotsPredictions[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else {
      
      for(i in 1:length(options[["priors"]])){
        
        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsPredictions[[options[["priors"]][[i]]$name]] <- tempPlot
        
        if (options[["predictionPlotProp"]]){
          xName  <- gettext("Predicted sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/options[["predictionN"]],1 + .5/options[["predictionN"]])
        } else {
          xName  <- gettext("Predicted number of successes")
          yName  <- gettext("Probability")
          xRange <- c(0, options[["predictionN"]])
        }
        
        dfCI   <- NULL
        dfHist <- NULL
        
        if (options[["plotsPredictionCI"]]){
          
          if (options[["plotsPredictionType"]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(data, options[["priors"]][[i]], options[["plotsPredictionCoverage"]],
                                           n = options[["predictionN"]],type = "prediction")
            
          } else if (options[["plotsPredictionType"]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(data, options[["priors"]][[i]], options[["plotsPredictionCoverage"]],
                                       n = options[["predictionN"]], type = "prediction")
            
          } else if (options[["plotsPredictionType"]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(data, options[["priors"]][[i]],
                                          options[["plotsPredictionLower"]], options[["plotsPredictionUpper"]],
                                          n = options[["predictionN"]], type = "prediction")
            
            if (options[["plotsPredictionUpper"]] > options[["predictionN"]]){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(
                gettext("The upper CI limit is higher than the number of future observations. Please, change the value of the upper CI limit in the settings panel."))
              
              return()
            }
            if (options[["plotsPredictionLower"]] > options[["predictionN"]]){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the number of future observations. Please, change the value of the lower CI limit in the settings panel."))
              
              return()
            }
            if (options[["plotsPredictionLower"]] > options[["plotsPredictionUpper"]]){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the upper CI limit. Please, change the value of the CI limits in the settings panel."))
              
              return()
            }
          }
        }
        
        dfHist  <- .dataHistBinomialLS(data, options[["priors"]][[i]], options[["predictionN"]])
        
        if (options[["predictionPlotProp"]]){
          dfHist$x <- dfHist$x/options[["predictionN"]]
          if (options[["plotsPredictionCI"]]){
            dfCI$xStart <- dfCI$xStart/options[["predictionN"]]
            dfCI$xEnd   <- dfCI$xEnd  /options[["predictionN"]]
          }
          nRound <- 3
        } else
          nRound <- 0
        
        if (options[["plotsPredictionEstimate"]]){
          dfPointEstimate <- .estimateDataPointBinomial(data, options[["priors"]][[i]], N = options[["predictionN"]], 
                                                        type = "prediction", estimate = options[["plotsPredictionEstimateType"]],
                                                        prop = options[["predictionPlotProp"]])
        } else
          dfPointEstimate <- NULL
        
        p <- .plotPredictionLS(dfHist, dfPointEstimate, dfCI, xRange, xName, yName, nRound = nRound,
                               proportions = options[["predictionPlotProp"]], predictionN = options[["predictionN"]])
        tempPlot$plotObject <- p
      }
      
    }
  }
  
  return()
}
.plotsPredictionsBinomialLS        <- function(jaspResults, data, ready, options){
  
  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")
  
  if (is.null(containerPredictionPlots[["plotsPredictions"]])){
    
    plotsPredictions <- createJaspPlot(
      width  = if (options[["predictionPlotType"]] == "overlying") 700 else 530,
      height = 400)
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesBinomialLS, "predictionN",
                                "colorPalette", "predictionPlotProp"))
    
    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions
    
    
    if (!ready["priors"])
      return()
    else {
      
      if (options[["predictionPlotProp"]]){
        xName  <- gettext("Predicted sample proportions")
        yName  <- gettext("Density")
        xRange <- c(-.5/options[["predictionN"]],1+.5/options[["predictionN"]])
      } else {
        xName  <- gettext("Predicted number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, options[["predictionN"]]+.5)
      }
      
      allLines  <- c()
      legend     <- NULL
      
      for(i in 1:length(options[["priors"]])){
        
        dfHist   <- .dataHistBinomialLS2(data, options[["priors"]][[i]], options[["predictionN"]])
        dfHist$g <- options[["priors"]][[i]]$name
        
        if (options[["predictionPlotProp"]]){
          dfHist$x <- dfHist$x/options[["predictionN"]]
        }
        
        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        allLines<- c(allLines, list(dfHist))
      }
      
      if (options[["predictionPlotType"]] == "overlying"){
        p <- .plotOverlyingLS(allLines, NULL, xName = xName, yName = yName, xRange = xRange, discrete = TRUE,
                              palette = options[["colorPalette"]], proportions = options[["predictionPlotProp"]])
      } else {
        p <- .plotStackedLS(allLines, NULL, legend, xName = xName, xRange = xRange,
                            discrete = TRUE, proportions = options[["predictionPlotProp"]])
      }
      
      plotsPredictions$plotObject <- p
    }
  }
  
  return()  
}
.tablePosteriorPredictions         <- function(jaspResults, data, ready, options){
  
  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")
  
  if (is.null(containerPredictionPlots[["tablePredictions"]])){
    
    tablePredictions <- createJaspTable()
    
    tablePredictions$position <- 3
    tablePredictions$dependOn(c(.dataDependenciesBinomialLS, "predictionN", "predictionPlotProp", "predictionPlotTable"))
    containerPredictionPlots[["tablePredictions"]] <- tablePredictions
    

    if (options[["predictionPlotProp"]]){
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Proportion of Successes"), type = "number")
      tablePredictions$addColumns(c(0:options[["predictionN"]])/options[["predictionN"]])
    } else {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Successes"), type = "integer")
      tablePredictions$addColumns(0:options[["predictionN"]])
    }
    
    
    if (ready["priors"]){
      for(i in seq_along(options[["priors"]])){
        tablePredictions$addColumnInfo(name = paste0("hyp_", i), title = options[["priors"]][[i]]$name, type = "number")
      }
    } else
      return()
    
    
    if (!ready["data"]){
      
      if ((options[["dataType"]] == "dataVariable" && options[["selectedVariable"]]     != "") ||
          (options[["dataType"]] == "dataSequence" && options[["dataSequenceInput"]]    != ""))
        tablePredictions$addFootnote(gettext("Please specify successes and failures."))
      
      return()
    }
    
    
    tempPred    <- NULL
    
    for(i in 1:length(options[["priors"]])) {
      tempResults <- .dataHistBinomialLS2(data, options[["priors"]][[i]], options[["predictionN"]])
      tablePredictions$addColumns(tempResults[1:length(tempResults) %% 2 == 0,"y"])
    }

  }
  return()
}