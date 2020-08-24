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
  if (options[["introText"]]).introductoryTextLS(jaspResults, options, "bin_est")
  
  # evaluate the expressions in priors
  if (ready[2])options[["priors"]] <- .evaluate_priors(options[["priors"]])
  
  # load, check, transform and process data
  if (ready[1])data <- .readDataBinomialLS(dataset, options)
  
  # data summary table ifrequested (but not ifthe data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "bin_est")
  
  
  ### inference 
  # estimated parameter values
  .estimatesBinomialLS(jaspResults, data, ready, options)
  
  # prior
  if (options[["plotsPrior"]]){
    if (options[["plotsPriorType"]] != "individual").plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Prior")
    if (options[["plotsPriorType"]] == "individual").plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # posterior
  if (options[["plotsPosterior"]]){
    if (options[["plotsPosteriorType"]] != "individual").plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["plotsPosteriorType"]] == "individual").plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if (options[["plotsBoth"]]).plotsBothBinomialLS(jaspResults, data, ready, options)
  
  ### sequential analysis
  # point estimate
  if (options[["plotsIterative"]]){
    if (options[["plotsIterativeType"]] == "overlying").plotsIterativeOverlyingBinomialLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeType"]] == "stacked").plotsIterativeStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # point estimate table
  if (options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]]).tableIterativeBinomialLS(jaspResults, data, ready, options)
  
  # interval
  if (options[["plotsIterativeInterval"]]){
    if (options[["plotsIterativeIntervalType"]] == "overlying").plotsIterativeIntervalOverlyingBinomialLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeIntervalType"]] == "stacked").plotsIterativeIntervalStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # interval estimate table
  if (options[["plotsIterativeInterval"]] && options[["plotsIterativeIntervalUpdatingTable"]]).tableIterativeIntervalBinomialLS(jaspResults, data, ready, options)
  
  # posterior updating table
  if (options[["doIterative"]] && options[["dataType"]] != "dataCounts").estimatesSequentialBinomialLS(jaspResults, data, ready, options)
  
  
  ### prediction
  if (options[["predictionTable"]]).tablePredictionsBinomialLS(jaspResults, data, ready, options)
  
  # plot
  if (options[["plotsPredictions"]]){
    if (options[["predictionPlotType"]] != "individual").plotsPredictionsBinomialLS(jaspResults, data, ready, options)
    if (options[["predictionPlotType"]] == "individual").plotsPredictionsIndividualBinomialLS(jaspResults, data, ready, options)
  }
  
  return()
}

# main functions
.estimatesBinomialLS               <- function(jaspResults, data, ready, options){
  
  estimatesContainer <- .estimatesContainerLS(jaspResults, options, "bin_est")
  
  if (is.null(estimatesContainer[['estimatesTable']])){
    
    estimatesTable <- createJaspTable(title = gettextf("Estimation Summary"))
    
    estimatesTable$position <- 2
    estimatesTable$dependOn(.BinomialLS_data_dependencies)
    
    estimateText <- .estimateTextLS(options[["pointEstimate"]])
    
    estimatesTable$addColumnInfo(name = "hypothesis",   title = gettext("Model"),                       type = "string")
    estimatesTable$addColumnInfo(name = "prior",        title = gettextf("Prior (%s)", "\u03B8"),       type = "string")
    estimatesTable$addColumnInfo(name = "priorEst",     title = gettextf("Prior %s", estimateText),     type = "number")
    estimatesTable$addColumnInfo(name = "posterior",    title = gettextf("Posterior (%s)", "\u03B8"),   type = "string")
    estimatesTable$addColumnInfo(name = "posteriorEst", title = gettextf("Posterior %s", estimateText), type = "number")
    
    estimatesContainer[["estimatesTable"]] <- estimatesTable
    
    if (ready[1] && !ready[2])
      return()
    else if (!ready[1]){
      
      estimatesTable$setError(gettext("Please specify successes and failures."))
      return()
      
    } else if (ready[2]){
      
      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){
        # add mock data to use only priors
        temp_data <- list(
          "nSuccesses" = 0,
          "nFailures"  = 0
        )
        temp_results <- .estimateBinomialLS(temp_data, options[["priors"]][[i]])
        
        temp_row <- list(
          prior        = temp_results[["distribution"]],
          priorEst     = temp_results[[options[["pointEstimate"]]]],
          hypothesis   = options[["priors"]][[i]][["name"]], 
          posterior    = "",
          posteriorEst = "")
        
        
        if (all(ready)){
          # and when real data are supplied as well, add posterior information
          temp_results <- .estimateBinomialLS(data, options[["priors"]][[i]])
          
          temp_row["posterior"]    <- temp_results[["distribution"]]
          temp_row["posteriorEst"] <- temp_results[[options[["pointEstimate"]]]]
          
        }
        
        estimatesTable$addRows(temp_row)
      }
      
      # add footnote clarifying what dataset was used
      estimatesTable$addFootnote(gettextf(
        "These results are based on %i %s and %i %s.",
        data$nSuccesses, ifelse(data$nSuccesses == 1, gettext("success"), gettext("successes")),
        data$nFailures,  ifelse(data$nFailures  == 1, gettext("failure"), gettext("failures"))
      ))
      
    }
  }
  
  return()
}
.estimatesSequentialBinomialLS     <- function(jaspResults, data, ready, options){
  
  containerIterativeUpdating <- .containerSequentialUpdatingLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterativeUpdating[["estimatesSequentialTable"]])){
    
    estimatesSequentialTable <- createJaspTable()
    
    estimatesSequentialTable$position <- 2
    estimatesSequentialTable$dependOn(.BinomialLS_data_dependencies)
    
    estimatesSequentialTable$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    containerIterativeUpdating[["estimatesSequentialTable"]] <- estimatesSequentialTable
    
    
    estimatesSequentialTable$setExpectedSize(ifelse(ready[1], length(data$y) + 1, 1))
    if (ready[2]){
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
      temp_row <- NULL
      temp_row[["iteration"]] <- 0
      for(h in 1:length(options[["priors"]])){
        temp_data    <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
        temp_results <- .estimateBinomialLS(temp_data, options[["priors"]][[h]])
        temp_row[[options[["priors"]][[h]]$name]] <- temp_results$distribution
      }
      estimatesSequentialTable$addRows(temp_row)
      
      # then update the posteriors as the data go in
      if (length(data$y) > 0){
        for(i in 1:length(data$y)){
          temp_row <- NULL
          temp_row[["iteration"]] <- i
          for(h in 1:length(options[["priors"]])){
            temp_data    <- list(
              nSuccesses = sum(data$y[1:i] == 1),
              nFailures  = sum(data$y[1:i] == 0)
            )
            temp_results <- .estimateBinomialLS(temp_data, options[["priors"]][[h]])
            temp_row[[options[["priors"]][[h]]$name]] <- temp_results$distribution
          }
          estimatesSequentialTable$addRows(temp_row)
        }
      }
    }
  }
  
  return()
}
.plotsSimpleBinomialLS             <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlotsLS(jaspResults, options, "bin_est", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsSimple <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsSimple$position <- 2
    plotsSimple$dependOn(c(.BinomialLS_data_dependencies,
                           ifelse(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying",
                                  "colorPalette", "")))
    
    containerPlots[[paste0("plots",type)]] <- plotsSimple
    
    if (!all(ready))return()
    
    all_lines  <- c()
    all_arrows <- c()
    legend     <- NULL
    for(i in 1:length(options[["priors"]])){
      
      if (options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP$g <- options[["priors"]][[i]]$name
        
        all_arrows  <- c(all_arrows, list(dfArrowPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      } else if (options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$g <- options[["priors"]][[i]]$name
        
        all_lines   <- c(all_lines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      }
    }
    
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if (options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying")
      p <- .plotOverlyingLS(all_lines, all_arrows, xName = xName, palette = options[["colorPalette"]])
    else
      p <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
    
    plotsSimple$plotObject <- p
  }
  
  return()
}
.plotsIndividualBinomialLS         <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlotsLS(jaspResults, options, "bin_est", type)
  
  if (is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsIndividual <- createJaspContainer()
    
    plotsIndividual$position <- 2
    plotsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                               ifelse(type == "Prior", "plotsPriorIndividualEstimate",     "plotsPosteriorIndividualEstimate"),
                               ifelse(type == "Prior", "plotsPriorIndividualEstimateType", "plotsPosteriorIndividualEstimateType"),
                               ifelse(type == "Prior", "plotsPriorIndividualCI",   "plotsPosteriorIndividualCI"),
                               ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType"),
                               ifelse(type == "Prior", "plotsPriorCoverage",       "plotsPosteriorCoverage"),
                               ifelse(type == "Prior", "plotsPriorLower",          "plotsPosteriorLower"),
                               ifelse(type == "Prior", "plotsPriorUpper",          "plotsPosteriorUpper")))
    
    containerPlots[[paste0("plots",type)]] <- plotsIndividual
    
    
    if (all(!ready) || (ready[1] && !ready[2])){
      
      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      if (type == "Prior"){
        temp_data <- list(
          nSuccesses = 0,
          nFailures = 0
        )
      } else
        temp_data <- data
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIndividual[[options[["priors"]][[i]]$name]] <- temp_plot
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL
        
        if (options[[ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]){
          
          if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(
              temp_data,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )
            
          } else if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(
              temp_data,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )
            
          } else if (options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(
              temp_data, options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
              options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]],
              type = "parameter"
            )  
            
          } else if (options[["plotsPosteriorIndividualType"]] == "support"){
            
            dfCI <- .dataSupportBinomialLS(
              temp_data,
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
              temp_CILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$x_start[r] & dfLinesPP$x <= dfCI$x_end[r],]
              temp_CILinesPP$g <- paste(c(as.character(dfCI$g), r), collapse = "")
              temp_CILinesPP   <- rbind.data.frame(
                data.frame(x = dfCI$x_start[r], y = 0, g = temp_CILinesPP$g[1]),
                temp_CILinesPP,
                data.frame(x = dfCI$x_end[r], y = 0, g = temp_CILinesPP$g[1])
              )
              dfCILinesPP <- rbind.data.frame(dfCILinesPP, temp_CILinesPP)
            }
          }
          
        }
        
        if (options[[ifelse(type == "Prior", "plotsPriorIndividualEstimate", "plotsPosteriorIndividualEstimate")]]){
          dfPointEstimate <- .dataPointEstimateBinomial(temp_data, options[["priors"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse(type == "Prior", "plotsPriorIndividualEstimateType", "plotsPosteriorIndividualEstimateType")]])
        } else
          dfPointEstimate <- NULL
        
        
        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfPointEstimate, dfCI, dfCILinesPP, NULL, c(0,1), xName, nRound = 3)
        temp_plot$plotObject <- p
      }
    }
    
  }
  
  return()
}
.plotsBothBinomialLS               <- function(jaspResults, data, ready, options){
  
  containerBoth <- .containerPlotsBothLS(jaspResults, options, "bin_est")
  
  if (is.null(containerBoth[["plotsBoth"]])){
    
    plotsBoth <- createJaspContainer()
    
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.BinomialLS_data_dependencies, "plotsBothSampleProportion"))
    
    containerBoth[["plotsBoth"]] <- plotsBoth
    
    
    if (all(!ready) || (ready[1] && !ready[2])){
      
      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsBoth[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsBoth[[options[["priors"]][[i]]$name]] <- temp_plot
        
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
        temp_plot$plotObject <- p
      }
    }
  }
  
  return()
}
.plotsIterativeOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterative[["plotsIterative"]])){
    
    plotsIterative <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeEstimateType",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "plotsIterativeIndividualType", "plotsIterativeBF",
                              "colorPalette"))
    containerIterative[["plotsIterative"]] <- plotsIterative
    
    if (!all(ready))
      return()
    
    plot_data_lines <- list()
    plot_data_CI    <- list()
    
    # then update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){
      
      temp_lines   <- NULL
      temp_CI      <- NULL
      # for dealing with possible bimodal distributions from HPD
      CI_unimodal  <- TRUE
      temp_CI1     <- NULL
      temp_CI2     <- NULL
      
      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data$y) == 0)
        iter_seq <- c(0, 0.1)
      else
        iter_seq <- 0:length(data$y)
      
      for(i in iter_seq){
        
        temp_data    <- list(
          nSuccesses = sum(data$y[0:i] == 1),
          nFailures  = sum(data$y[0:i] == 0)
        )
        
        temp_results    <- .estimateBinomialLS(temp_data, options[["priors"]][[h]])
        temp_lines      <- rbind(temp_lines, data.frame(
          y    = as.numeric(temp_results[[options[["plotsIterativeEstimateType"]]]]),
          x    = i,
          name = options[["priors"]][[h]]$name
        ))
        
        if (options[["plotsIterativeIndividualCI"]]){
          
          if (options[["plotsIterativeIndividualType"]] == "central"){
            temp_CIPP <- .dataCentralBinomialLS(temp_data, options[["priors"]][[h]],
                                                options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "HPD"){
            
            temp_CIPP <- .dataHPDBinomialLS(temp_data, options[["priors"]][[h]],
                                            options[["plotsIterativeCoverage"]], type = "parameter")
            if (nrow(temp_CIPP) == 2)CI_unimodal <- FALSE
            
          } else if (options[["plotsIterativeIndividualType"]] == "support"){
            
            temp_CIPP <- .dataSupportBinomialLS(temp_data, options[["priors"]][[h]],
                                                options[["plotsIterativeBF"]])
            if (nrow(temp_CIPP) == 0)temp_CIPP <- NULL
            
          }
          
          if (nrow(temp_CIPP) == 1 && CI_unimodal){
            
            temp_CI <- rbind(temp_CI, data.frame(
              y1   = temp_CIPP$x_start,
              y2   = temp_CIPP$x_end,
              x    = i,
              name = options[["priors"]][[h]]$name
            ))
            
          } else if (nrow(temp_CIPP) == 1 && !CI_unimodal){
            
            temp_CI <- rbind(
              temp_CI,
              data.frame(
                y1   = (temp_CIPP$x_start + temp_CIPP$x_end)/2,
                y2   = (temp_CIPP$x_start + temp_CIPP$x_end)/2,
                x    = i,
                name = temp_CI1$name
              ),
              data.frame(
                y1   = c(temp_CI1$y2, temp_CI1$y1),
                y2   = c(temp_CI2$y1, temp_CI2$y2),
                x    = rep(temp_CI1$x, 2),
                name = rep(temp_CI1$name, 2)
              ),
              data.frame(
                y1   = temp_CIPP$x_start,
                y2   = temp_CIPP$x_end,
                x    = i,
                name = options[["priors"]][[h]]$name
              )
            )
            CI_unimodal <- TRUE
            
          } else if (nrow(temp_CIPP) == 2){
            
            temp_CI1 <- rbind(
              temp_CI1,
              data.frame(
                y1   = temp_CIPP$x_start[1],
                y2   = temp_CIPP$x_end[1],
                x    = i,
                name = options[["priors"]][[h]]$name
              ))
            
            temp_CI2 <- rbind(
              temp_CI2,
              data.frame(
                y1   = temp_CIPP$x_start[2],
                y2   = temp_CIPP$x_end[2],
                x    = i,
                name = options[["priors"]][[h]]$name
              ))
            
          } else if (nrow(temp_CIPP) > 2)
            JASP:::.quitAnalysis(gettext("More than bimodal CIs are not implemented in the Sequential analysis plot."))
        }
        
      }
      
      plot_data_lines <- c(plot_data_lines, list(temp_lines))
      
      # deal with a possibility of two disjoined CIs
      if (options[["plotsIterativeIndividualCI"]]){
        if (CI_unimodal){
          # deal with possible non-existing support intervals
          if (all(is.na(temp_CI[,c("y1", "y2")])))
            plot_data_CI    <- c(plot_data_CI, list(NULL))          
          else
            plot_data_CI    <- c(plot_data_CI, list(temp_CI))
        } else
          plot_data_CI    <- c(plot_data_CI, list(temp_CI1), list(temp_CI2))
      }
      
    }
    
    yName  <- bquote(.(gettext("Population proportion"))~~theta)
    xName  <- gettext("Observation")
    
    p <- .plotIterativeLS(plot_data_lines, plot_data_CI, xName = xName, yName = yName, palette = options[["colorPalette"]])
    
    plotsIterative$plotObject <- p
    
  }
  
  return()
}
.plotsIterativeStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "bin_est") 
  
  if (is.null(containerIterative[["plotsIterative"]])){
    plotsIterative <- createJaspContainer()
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(.BinomialLS_data_dependencies)
    
    containerIterative[["plotsIterative"]] <- plotsIterative
    
    
    if (all(!ready) || (ready[1] && !ready[2])){
      
      plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIterative[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                          width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      #options[["priors"]][[i]]$name
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIterative[[options[["priors"]][[i]]$name]] <- temp_plot
        
        all_lines  <- c()
        all_arrows <- c()
        legend     <- NULL
        
        # too many iterations crashes JASP
        if (length(data$y) > 10)
          iter_sequence <- round(seq(0, length(data$y), length.out = 10))
        else
          iter_sequence <- 0:length(data$y)
        
        iter_sequence <- rev(iter_sequence)
        
        for(iteration in iter_sequence){
          
          if (options[["priors"]][[i]]$type == "spike"){
            
            dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)
            
            all_arrows  <- c(all_arrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          } else if (options[["priors"]][[i]]$type == "beta"){
            
            temp_data <- list(
              "nSuccesses" = sum(data$y[0:iteration] == 1),
              "nFailures"  = sum(data$y[0:iteration] == 0)
            )
            
            dfLinesPP   <- .dataLinesBinomialLS(temp_data, options[["priors"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)
            
            all_lines   <- c(all_lines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          }
          
        }
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
      }
    } 
  }
  
  return()
}
.plotsIterativeIntervalOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])){
    
    plotsIterativeInterval <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.BinomialLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval
    
    if (!all(ready))
      return()
    
    
    plot_data_lines <- list()
    
    # update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){
      
      temp_lines   <- NULL
      
      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data$y) == 0)
        iter_seq <- c(0, 0.1)
      else
        iter_seq <- 0:length(data$y)
      
      
      for(i in iter_seq){
        
        temp_data    <- list(
          nSuccesses = sum(data$y[0:i] == 1),
          nFailures  = sum(data$y[0:i] == 0)
        )
        
        temp_results    <- .dataCustomBinomialLS(temp_data, options[["priors"]][[h]],
                                                 lCI = options[["plotsIterativeIntervalLower"]],
                                                 uCI = options[["plotsIterativeIntervalUpper"]],
                                                 type = c("parameter"))
        
        temp_lines      <- rbind(temp_lines, data.frame(
          y    = temp_results$coverage,
          x    = i,
          name = options[["priors"]][[h]]$name
        ))
        
      }
      
      plot_data_lines <- c(plot_data_lines, list(temp_lines))
      
    }
    
    yName  <- bquote("P("~{.(options[["plotsIterativeIntervalLower"]])<=theta}<=.(options[["plotsIterativeIntervalUpper"]])~")")
    xName  <- gettext("Observation")
    
    p <- .plotIterativeLS(plot_data_lines, all_CI = NULL, xName = xName, yName = yName, palette = options[["colorPalette"]])
    
    plotsIterativeInterval$plotObject <- p
  }
  
  return()
}
.plotsIterativeIntervalStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])){
    
    plotsIterativeInterval <- createJaspContainer()
    
    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.BinomialLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    
    
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval
    
    
    if (all(!ready) || (ready[1] && !ready[2])){
      
      plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else if (!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                  width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    } else {
      
      #options[["priors"]][[i]]$name
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- temp_plot
        
        all_lines  <- c()
        all_arrows <- c()
        legend     <- NULL
        
        # too many iterations crashes JASP
        if (length(data$y) > 10)
          iter_sequence <- round(seq(0, length(data$y), length.out = 10))
        else
          iter_sequence <- 0:length(data$y)
        
        iter_sequence <- rev(iter_sequence)
        
        for(iteration in iter_sequence){
          
          if (options[["priors"]][[i]]$type == "spike"){
            
            dfArrowPP   <- .dataArrowBinomialLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)
            
            all_arrows  <- c(all_arrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          } else if (options[["priors"]][[i]]$type == "beta"){
            
            temp_data <- list(
              "nSuccesses" = sum(data$y[0:iteration] == 1),
              "nFailures"  = sum(data$y[0:iteration] == 0)
            )
            
            dfLinesPP   <- .dataLinesBinomialLS(temp_data, options[["priors"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)
            
            all_lines   <- c(all_lines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))
            
          }
          
        }
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName,
                                               lCI = options[["plotsIterativeIntervalLower"]], uCI = options[["plotsIterativeIntervalUpper"]])
      }
    }
  }
  
  return()
}
.tableIterativeBinomialLS          <- function(jaspResults, data, ready, options){
  
  containerIterative <- .containerSequentialPointLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterative[["tableIterative"]])){
    tableIterative <- createJaspTable()
    
    tableIterative$position <- 3
    tableIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeEstimateType",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "plotsIterativeIndividualType", "plotsIterativeBF",
                              "colorPalette", "plotsIterativeUpdatingTable"))
    containerIterative[["tableIterative"]] <- tableIterative
    
    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready[2]){
      if (options[["plotsIterativeIndividualCI"]]){
        if (options[["plotsIterativeIndividualType"]] == "central")
          CI_title <- gettextf("%i %% CI", options[["plotsIterativeCoverage"]]*100)
        else if (options[["plotsIterativeIndividualType"]] == "HPD")
          CI_title <- gettextf("%i %% HPD", options[["plotsIterativeCoverage"]]*100)
        else if (options[["plotsIterativeIndividualType"]] == "support")
          CI_title <- gettextf("SI (BF=%s)", options[["plotsIterativeBF"]])
        
        for(i in 1:length(options[["priors"]])){
          tableIterative$addColumnInfo(
            name      = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title     = .estimateTextLS(options[["plotsIterativeEstimateType"]]),
            overtitle = options[["priors"]][[i]]$name,
            type      = "number")
          tableIterative$addColumnInfo(
            name      = paste(options[["priors"]][[i]]$name,"CI", sep = "_"),
            title     = CI_title,
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
    
    iter_seq <- 0:length(data$y)
    
    for(i in iter_seq){
      
      temp_row     <- list() 
      temp_row[["iteration"]] <- i
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      
      for(h in 1:length(options[["priors"]])){
        
        temp_results <- .estimateBinomialLS(temp_data, options[["priors"]][[h]])
        temp_row[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- temp_results[[options[["plotsIterativeEstimateType"]]]]
        
        if (options[["plotsIterativeIndividualCI"]]){
          
          if (options[["plotsIterativeIndividualType"]] == "central"){
            temp_CIPP <- .dataCentralBinomialLS(temp_data, options[["priors"]][[h]],
                                                options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "HPD"){
            temp_CIPP <- .dataHPDBinomialLS(temp_data, options[["priors"]][[h]],
                                            options[["plotsIterativeCoverage"]], type = "parameter")
          } else if (options[["plotsIterativeIndividualType"]] == "support"){
            temp_CIPP <- .dataSupportBinomialLS(temp_data, options[["priors"]][[h]],
                                                options[["plotsIterativeBF"]])
          }
          
          if (all(is.na(temp_CIPP[1:2]))){
            temp_int <- "∅"
          } else {
            temp_int <- sapply(1:nrow(temp_CIPP), function(i)paste(c(
              "[",format(round(temp_CIPP$x_start[i], 3), nsmall = 3),", ",format(round(temp_CIPP$x_end[i], 3), nsmall = 3),"]"
            ), collapse = ""))
            temp_int <- paste(temp_int, collapse = " and " )
            
            temp_row[[paste(options[["priors"]][[h]]$name,"CI", sep = "_")]] <- temp_int
          }
          
        }
        
      }
      
      tableIterative$addRows(temp_row)
      
    } 
  }
  
  return()
}
.tableIterativeIntervalBinomialLS  <- function(jaspResults, data, ready, options){
  
  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "bin_est")
  
  if (is.null(containerIterativeInterval[["tableIterativeInterval"]])){
    
    tableIterativeInterval <- createJaspTable()
    
    tableIterativeInterval$position <- 3
    tableIterativeInterval$dependOn(c(.BinomialLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "plotsIterativeIntervalUpdatingTable"))
    containerIterativeInterval[["tableIterativeInterval"]] <- tableIterativeInterval
    
    tableIterativeInterval$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready[2]){
      for(i in 1:length(options[["priors"]])){
        tableIterativeInterval$addColumnInfo(
          name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),  
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }
    
    if (!all(ready))
      return()
    
    iter_seq <- 0:length(data$y)
    
    for(i in iter_seq){
      
      temp_row     <- list() 
      temp_row[["iteration"]] <- i
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      
      for(h in 1:length(options[["priors"]])){
        
        temp_results    <- .dataCustomBinomialLS(temp_data, options[["priors"]][[h]],
                                                 lCI = options[["plotsIterativeIntervalLower"]], uCI = options[["plotsIterativeIntervalUpper"]],
                                                 type = c("parameter"))
        temp_row[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- temp_results$coverage
      }
      
      tableIterativeInterval$addRows(temp_row)
    }  
  }
  
  return()
}
.tablePredictionsBinomialLS        <- function(jaspResults, data, ready, options){
  
  containerPredictions <- .containerPredictionsLS(jaspResults, options, "bin_est")
  
  if (is.null(containerPredictions[["predictionsTable"]])){
    
    predictionsTable <- createJaspTable()
    
    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.BinomialLS_data_dependencies, "predictionN"))
    
    estimateText <- .estimateTextLS(options[["predictionTableEstimate"]])
    
    predictionsTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),                        type = "string")
    predictionsTable$addColumnInfo(name = "posterior",     title = gettextf("Posterior (%s)", "\u03B8"),    type = "string")
    predictionsTable$addColumnInfo(name = "posteriorEst",  title = gettextf("Posterior %s", estimateText),  type = "number")
    predictionsTable$addColumnInfo(name = "predictive",    title = gettext("Prediction (Successes)"),       type = "string")
    predictionsTable$addColumnInfo(name = "predictiveEst", title = gettextf("Prediction %s", estimateText), type = "number")
    
    predictionsTable$setExpectedSize(length(options[["priors"]]))
    
    containerPredictions[["predictionsTable"]] <- predictionsTable
    
    if (!ready[2])
      return()
    else {
      
      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){
        
        temp_results    <- .estimateBinomialLS(data, options[["priors"]][[i]])
        temp_prediction <- .predictBinomialLS(data, options[["priors"]][[i]], options)
        
        temp_row <- list(
          hypothesis     = options[["priors"]][[i]][["name"]],
          posterior      = temp_results[["distribution"]],
          posteriorEst   = temp_results[[options[["predictionTableEstimate"]]]],
          predictive     = temp_prediction[["distribution"]],
          predictiveEst  = temp_prediction[[options[["predictionTableEstimate"]]]]
        )
        
        predictionsTable$addRows(temp_row)
      }
      
      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %s %s is based on %s %s and %s %s.",
        options[["predictionN"]], ifelse(options[["predictionN"]] == 1, gettext("observation"), gettext("observations")),
        data$nSuccesses, ifelse(data$nSuccesses == 1, gettext("success"), gettext("successes")),
        data$nFailures, ifelse(data$nFailures == 1, gettext("failure"), gettext("failures"))
      ))
    }
  }
  
  return()  
}
.plotsPredictionsIndividualBinomialLS      <- function(jaspResults, data, ready, options){
  
  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "bin_est")
  
  if (is.null(containerPredictionPlots[["plotsPredictions"]])){
    
    plotsPredictions <- createJaspContainer()
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.BinomialLS_data_dependencies, "predictionN",
                                "plotsPredictionCI", "plotsPredictionType",
                                "plotsPredictionEstimate", "plotsPredictionEstimateType",
                                "plotsPredictionCoverage", "plotsPredictionLower", "plotsPredictionUpper",
                                "predictionPlotProp"))
    
    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions
    
    
    if (!ready[2]){
      
      plotsPredictions[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    } else {
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsPredictions[[options[["priors"]][[i]]$name]] <- temp_plot
        
        if (options[["predictionPlotProp"]]){
          xName  <- gettext("Sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/options[["predictionN"]],1 + .5/options[["predictionN"]])
        } else {
          xName  <- gettext("Number of successes")
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
            dfCI$x_start <- dfCI$x_start/options[["predictionN"]]
            dfCI$x_end   <- dfCI$x_end  /options[["predictionN"]]
          }
          nRound <- 3
        } else
          nRound <- 0
        
        if (options[["plotsPredictionEstimate"]]){
          dfPointEstimate <- .dataPointEstimateBinomial(data, options[["priors"]][[i]], N = options[["predictionN"]], 
                                                        type = "prediction", estimate = options[["plotsPredictionEstimateType"]],
                                                        prop = options[["predictionPlotProp"]])
        } else
          dfPointEstimate <- NULL
        
        p <- .plotPredictionLS(dfHist, dfPointEstimate, dfCI, xRange, xName, yName, nRound = nRound,
                               proportions = options[["predictionPlotProp"]], predictionN = options[["predictionN"]])
        temp_plot$plotObject <- p
      }
      
    }
  }
  
  return()
}
.plotsPredictionsBinomialLS        <- function(jaspResults, data, ready, options){
  
  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "bin_est")
  
  if (is.null(containerPredictionPlots[["plotsPredictions"]])){
    
    plotsPredictions <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.BinomialLS_data_dependencies, "predictionN",
                                "colorPalettePrediction", "predictionPlotProp"))
    
    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions
    
    
    if (!ready[2])
      return()
    else {
      
      if (options[["predictionPlotProp"]]){
        xName  <- gettext("Sample proportions")
        yName  <- gettext("Density")
        xRange <- c(-.5/options[["predictionN"]],1+.5/options[["predictionN"]])
      } else {
        xName  <- gettext("Number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, options[["predictionN"]]+.5)
      }
      
      all_lines  <- c()
      legend     <- NULL
      
      for(i in 1:length(options[["priors"]])){
        
        dfHist   <- .dataHistBinomialLS2(data, options[["priors"]][[i]], options[["predictionN"]])
        dfHist$g <- options[["priors"]][[i]]$name
        
        if (options[["predictionPlotProp"]]){
          dfHist$x <- dfHist$x/options[["predictionN"]]
        }
        
        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        all_lines<- c(all_lines, list(dfHist))
      }
      
      if (options[["predictionPlotType"]] == "overlying"){
        p <- .plotOverlyingLS(all_lines, NULL, xName = xName, yName = yName, xRange = xRange, discrete = TRUE,
                              palette = options[["colorPalettePrediction"]], proportions = options[["predictionPlotProp"]])
      } else {
        p <- .plotStackedLS(all_lines, NULL, legend, xName = xName, xRange = xRange,
                            discrete = TRUE, proportions = options[["predictionPlotProp"]])
      }
      
      plotsPredictions$plotObject <- p
    }
  }
  
  return()  
}
