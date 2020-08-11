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
saveOptions(options)
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)
  
  # introductory text
  if(options[["introText"]]).introductoryTextLS(jaspResults, options, "bin_test")
  
  # evaluate the expressions in priors
  if(ready[2])options[["priors"]] <- .evaluate_priors(options[["priors"]])
  # scale the prior probabilities
  if(ready[2])options[["priors"]] <- .scale_priors(options[["priors"]])
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataBinomialLS(dataset, options)
  
  # data summary table if requested (but not if the data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "bin_test")
  
  ### inference
  # summary table
  .testsBinomialLS(jaspResults, data, ready, options)

  # prior parameter
  if(options[["plotsPrior"]]){
    if(options[["plotsPriorType"]] != "conditional").plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if(options[["plotsPriorType"]] == "conditional").plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }

  # prior predictive
  if(options[["plotsPredictions"]]){
    if(options[["plotsPredictionType"]] != "conditional").plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if(options[["plotsPredictionType"]] == "conditional").plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }

  # predictive accuracy
  if(options[["plotsPredictiveAccuracy"]]).plotsPredAccuracyBinomial2LS(jaspResults, data, ready, options)
  
  # posterior parameter
  if(options[["plotsPosterior"]]){
    if(options[["plotsPosteriorType"]] != "conditional").plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if(options[["plotsPosteriorType"]] == "conditional").plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if(options[["plotsBoth"]]){
    if(options[["plotsBothType"]] != "conditional").plotsBothBinomialLS2(jaspResults, data, ready, options)
    if(options[["plotsBothType"]] == "conditional").plotsBothIndividualBinomial2LS(jaspResults, data, ready, options)
  }
  

  ### sequential analysis
  if(options[["plotsIterative"]]).plotsIterativeOverlyingBinomial2LS(jaspResults, data, ready, options)
  if(options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]]).tableIterativeBinomial2LS(jaspResults, data, ready, options)
  
  
  ### posterior predictive
  if(options[["predictionTable"]]).tablepredictionsBinomialLS(jaspResults, data, ready, options)
  if(options[["plotsPredictionsPost"]]){
    if(options[["plotsPredictionPostType"]] != "conditional").plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if(options[["plotsPredictionPostType"]] == "conditional").plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  return()
}

.testsBinomialLS              <- function(jaspResults, data, ready, options){
  
  if(is.null(jaspResults[["testsContainer"]])){
    testsContainer <- createJaspContainer("Hypotheses")
    testsContainer$position <- 2
    jaspResults[["testsContainer"]] <- testsContainer 
  }else{
    testsContainer <- jaspResults[["testsContainer"]]
  }
  
  
  if(options[["introText"]] && is.null(testsContainer[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("tests", options, "bin_test")
    
    testsContainer[['introText']] <- introText    
  }
  
  
  if(is.null(testsContainer[['testsTable']])){
    
    testsTable <- createJaspTable(title = gettext("Testing Summary"))
    
    testsTable$position <- 2
    testsTable$dependOn(c(.BinomialLS_data_dependencies, "bfType", "bfTypevsName", "bayesFactorType"))
    
    bfType_name <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )
    
    testsTable$addColumnInfo(name = "hypothesis",   title = gettext("Hypothesis"),          type = "string")
    testsTable$addColumnInfo(name = "prior",        title = gettext("P(H)"),                type = "number")
    testsTable$addColumnInfo(name = "log_lik",      title = gettext("log(likelihood)"),     type = "number")
    testsTable$addColumnInfo(name = "posterior",    title = gettext("P(H|data)"),           type = "number")
    testsTable$addColumnInfo(name = "bf",           title = bfType_name,                    type = "number")
    
    testsTable$setExpectedSize(length(options[["priors"]]))
    
    testsContainer[["testsTable"]] <- testsTable
    
    if(ready[1] && !ready[2]){
      
      return()
      
    }else if(!ready[1]){
      
      testsTable$setError(gettext("Please specify successes and failures."))
      
    }else if(ready[2]){
      
      temp_results <- .testBinomialLS(data, options[["priors"]])
      
      for(i in 1:length(options[["priors"]])){
        
        temp_row <- list(
          hypothesis  = options[["priors"]][[i]]$name,
          prior       = temp_results$prior[i],
          log_lik     = temp_results$log_lik[i], 
          posterior   = temp_results$posterior[i])
        
        if(options[["bfType"]] == "inclusion"){
          temp_bf <- (temp_results$posterior[i] / (1-temp_results$posterior[i])) / (temp_results$prior[i] / (1-temp_results$prior[i]))
        }else if(options[["bfType"]] == "best"){
          temp_bf <- exp(temp_results$log_lik[i]) / exp(temp_results$log_lik[which.max(temp_results$log_lik)])
        }else if(options[["bfType"]] == "vs"){
          temp_bf <- exp(temp_results$log_lik[i]) / exp(temp_results$log_lik[sapply(options[["priors"]][[i]], function(p)p$n) == options[["bfTypevsName"]]])
        }
        
        if(options[["bayesFactorType"]] == "BF10"){
          temp_row$bf <- temp_bf
        }else if(options[["bayesFactorType"]] == "BF01"){
          temp_row$bf <- 1/temp_bf          
        }else if(options[["bayesFactorType"]] == "LogBF10"){
          temp_row$bf <- log(temp_bf)
        }
        
        testsTable$addRows(temp_row)
      }
      
      # add footnote clarifying what dataset was used
      testsTable$addFootnote(gettextf(
        "These results are based on %i %s and %i %s.",
        data$nSuccesses, ifelse(data$nSuccesses == 1, gettext("success"), gettext("successes")),
        data$nFailures,  ifelse(data$nFailures  == 1, gettext("failure"), gettext("failures"))
      ))
      
    }
    
  }
  
  return()
}
.plotsSimpleBinomial2LS       <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlots2LS(jaspResults, options, "bin_test", type)
  
  if(is.null(containerPlots[[paste0("plots",type)]])){
  
    plotsSimple <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsSimple$position <- 2
    plotsSimple$dependOn(c(.BinomialLS_data_dependencies,
                           ifelse(type == "Prior", "plotsPriorJointType",        "plotsPosteriorJointType"),
                           ifelse(type == "Prior", "plotsPriorMarginalCI",       "plotsPosteriorMarginalCI"),
                           ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage"),
                           ifelse(type == "Prior", "plotsPriorMarginalLower",    "plotsPosteriorMarginalLower"),
                           ifelse(type == "Prior", "plotsPriorMarginalUpper",    "plotsPosteriorMarginalUpper"),
                           if(type == "Posterior") "plotsPosteriorObserved",
                           "colorPalette", "scaleSpikes"))
    
    
    containerPlots[[paste0("plots",type)]] <- plotsSimple
    
    if (!all(ready))return()
    
    all_lines    <- c()
    all_arrows   <- c()
    legend       <- NULL
    temp_results <- .testBinomialLS(data, options[["priors"]])
    
    if(any(is.nan(temp_results$posterior))){
      plotsSimple$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
      return()
    }
    
    for(i in 1:length(options[["priors"]])){
      
      if(options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPP       <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP$y_end <- exp(log(dfArrowPP$y_end)+log(temp_results[i, tolower(type)]))
        dfArrowPP$g     <- options[["priors"]][[i]]$name
        
        all_arrows      <- c(all_arrows, list(dfArrowPP))
        legend          <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      }else if(options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$y <- exp(log(dfLinesPP$y)+log(temp_results[i, tolower(type)]))
        dfLinesPP$g <- options[["priors"]][[i]]$name
        
        all_lines   <- c(all_lines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))
        
      }
    }
    
    if(type == "Posterior" && options[["plotsPosteriorObserved"]]){
      dfPoints <- data.frame(
        x = data$nSuccesses/(data$nSuccesses + data$nFailures),
        y = 0,
        g = "Observed"
      )
    }else{
      dfPoints <- NULL
    }
    
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "joint"){
      
      if(options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "overlying"){
        p <- .plotOverlyingLS(all_lines, all_arrows, dfPoints, xName = xName, palette = options[["colorPalette"]])  
      }else if(options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "stacked"){
        p <- .plotStackedLS(all_lines, all_arrows, legend, dfPoints, xName = xName)
      }
      
      
    }else if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "marginal"){
      
      all_lines_new <- c()
      
      if(length(all_lines) > 0){
        
        for(i in 1:length(all_lines)){
          
          if(i == 1){
            all_lines_new[[1]] <- all_lines[[i]]
          }else{
            all_lines_new[[1]]$y <- all_lines_new[[1]]$y + all_lines[[i]]$y
          }
          
        }
        all_lines_new[[1]]$g <- "__marginal"
      }
      
      if(length(all_arrows) > 0){
        for(i in 1:length(all_arrows)){
          all_arrows[[i]]$g <- "__marginal"
        }
      }
      
      if(options[[ifelse(type == "Prior", "plotsPriorMarginalCI", "plotsPosteriorMarginalCI")]]){
        
        all_spikes   <- list()
        if(type == "Prior"){
          for(i in 1:length(options[["priors"]])){
            if(options[["priors"]][[i]]$type == "spike"){
              all_spikes <- c(
                all_spikes, 
                list(data.frame(y = options[["priors"]][[i]]$PH, x = options[["priors"]][[i]]$parPoint, g = "__marginal"))
              )
            }
          }
        }else{
          temp_results <- .testBinomialLS(data, options[["priors"]])
          for(i in 1:length(options[["priors"]])){
            if(options[["priors"]][[i]]$type == "spike"){
              all_spikes <- c(
                all_spikes, 
                list(data.frame(y = temp_results$posterior[i], x = options[["priors"]][[i]]$parPoint, g = "__marginal"))
              )
            }
          }
          
        }
        
        if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "central"){
          
          dfCI <- .marginalCentralBinomialLS(all_lines_new[[1]], all_spikes, 
                                             options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "HPD"){
          
          dfCI <- .marginalHPDBinomialLS(all_lines_new[[1]], all_spikes,
                                         options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])    
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "custom"){
          
          dfCI <- .marginalCustomBinomialLS(all_lines_new[[1]], all_spikes,
                                            lCI = options[[ifelse(type == "Prior", "plotsPriorMarginalLower", "plotsPosteriorMarginalLower")]],
                                            uCI = options[[ifelse(type == "Prior", "plotsPriorMarginalUpper", "plotsPosteriorMarginalUpper")]])
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "support"){
          
          dfCI <- .marginalSupportBinomialLS(data, options[["priors"]], all_lines_new[[1]], all_spikes, options[["plotsPosteriorMarginalBF"]])
          
        }
        
      }else{
        dfCI <- NULL
      }
      
      p <- .plotOverlyingLS(all_lines_new, all_arrows, dfPoints, CI = dfCI, xName = xName, no_legend = T)
      
    }
    
    plotsSimple$plotObject <- p
  }
  
  return()
}
.plotsIndividualBinomial2LS   <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPlots2LS(jaspResults, options, "bin_test", type)
  
  if(is.null(containerPlots[[paste0("plots",type)]])){
    
    plotsIndividual <- createJaspContainer()
    
    plotsIndividual$position <- 2
    plotsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                               ifelse(type == "Prior", "plotsPriorTypeCI",       "plotsPosteriorTypeCI"),
                               ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                               ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                               ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper"),
                               if(type == "Posterior") "plotsPosteriorObserved",
                               "scaleSpikes"))
    
    containerPlots[[paste0("plots",type)]] <- plotsIndividual
    
    
    if(all(!ready) || (ready[1] && !ready[2])){
      
      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    }else if(!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    }else{
      
      
      if(type == "Prior"){
        temp_data <- list(
          nSuccesses = 0,
          nFailures = 0
        )
      }else{
        temp_data <- data
      }
      
      temp_results <- .testBinomialLS(data, options[["priors"]])
      
      if(type == "Posterior" && options[["plotsPosteriorObserved"]]){
        dfPoints <- data.frame(
          x = data$nSuccesses/(data$nSuccesses + data$nFailures),
          y = 0,
          g = "Observed"
        )
      }else{
        dfPoints <- NULL
      }
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsIndividual[[options[["priors"]][[i]]$name]] <- temp_plot
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL
        
        if(options[[ifelse(type == "Prior", "plotsPriorCI", "plotsPosteriorCI")]]){
          
          if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(temp_data, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
            
          }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(temp_data, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
            
          }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(temp_data, options[["priors"]][[i]], options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
                                          options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]], type = "parameter")  
            
          }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "support"){
            
            dfCI <- .dataSupportBinomialLS(temp_data, options[["priors"]][[i]], options[["plotsPosteriorBF"]])  
            
          }
        }
        
        
        if(options[["priors"]][[i]]$type == "spike"){
          
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
          
        }else if(options[["priors"]][[i]]$type == "beta"){
          
          dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
          dfLinesPP$y <- dfLinesPP$y
          
          if(!is.null(dfCI)){
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
        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfCI, dfCILinesPP, dfPoints, c(0,1), xName, nRound = 3)
        temp_plot$plotObject <- p
      }
      
    }
  }
  
  return()
}
.plotsPredictionsBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "bin_test", type)
  
  if(is.null(containerPlots[[paste0("plotsPredictions",type)]])){

    plotsPredictions <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.BinomialLS_data_dependencies,
                                ifelse(type == "Prior", "plotsPredictionPostMarginalTypeCI", "plotsPredictionPostMarginalTypeCI"),
                                ifelse(type == "Prior", "plotsPredictionMarginalCoverage",   "plotsPredictionPostMarginalCoverage"),
                                ifelse(type == "Prior", "plotsPredictionMarginalLower",      "plotsPredictionPostMarginalLower"),
                                ifelse(type == "Prior", "plotsPredictionMarginalUpper",      "plotsPredictionPostMarginalUpper"),
                                ifelse(type == "Prior", "plotsPredictionsObserved",          "predictionPostPlotProp"),
                                ifelse(type == "Prior", "colorPalette",                      "colorPalettePrediction")
    ))
    
    containerPlots[[paste0("plotsPredictions",type)]] <- plotsPredictions
    
    
    if(!all(ready) || (data$nSuccesses == 0 && data$nFailures == 0)){
      return()
    }else{
      
      if(type == "Prior"){
        predictionN  <- data$nSuccesses + data$nFailures
        temp_results <- .testBinomialLS(data, options[["priors"]])
        temp_data    <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      }else if(type == "Posterior"){
        predictionN  <- options[["predictionN"]]
        temp_results <- .testBinomialLS(data, options[["priors"]])
        temp_data    <- data
        
        if(any(is.nan(temp_results$posterior))){
          plotsPredictions$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
          return()
        }
      }
      
      if(type == "Posterior" && options[["predictionPostPlotProp"]]){
        xName  <- gettext("Sample proportions")
        yName  <- gettext("Density")
        xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        proportions <- options[["predictionPostPlotProp"]]
        nRound <- 3
      }else{
        xName  <- gettext("Number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, predictionN + .5)
        nRound <- 0
        proportions <- FALSE
      }
      
      
      all_lines  <- c()
      legend     <- NULL
      
      for(i in 1:length(options[["priors"]])){
        
        dfHist   <- .dataHistBinomialLS2(temp_data, options[["priors"]][[i]], predictionN)
        dfHist$g <- options[["priors"]][[i]]$name
        dfHist$y <- dfHist$y*temp_results[i,ifelse(type == "Prior","prior","posterior")]
        
        if(type == "Posterior" && options[["predictionPostPlotProp"]]){
          dfHist$x <- dfHist$x/predictionN
        }
        
        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        all_lines<- c(all_lines, list(dfHist))
      }
      
      if(type == "Prior"){
        if(options[["plotsPredictionsObserved"]]){
          dfPoint <- data.frame(x = data$nSuccesses, y = 0)
        }else{
          dfPoint <- NULL
        }
      }else{
        dfPoint <- NULL
      }
      
      if(options[[ifelse(type == "Prior","plotsPredictionType", "plotsPredictionPostType")]] == "joint"){
        
        if(options[[ifelse(type == "Prior", "plotsPredictionJointType", "plotsPredictionPostJointType")]] == "overlying"){
          p <- .plotOverlyingLS(all_lines, NULL, dfPoints = dfPoint, xName = xName, yName = yName, xRange = xRange,
                                palette = options[[ifelse(type == "Prior", "colorPalette","colorPalettePrediction")]], nRound = nRound,
                                discrete = TRUE, proportions = proportions)
        }else if(options[[ifelse(type == "Prior", "plotsPredictionJointType", "plotsPredictionPostJointType")]] == "stacked"){
          p <- .plotStackedLS(all_lines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange,
                              proportions = proportions, discrete = TRUE)
        }
        
        
        
      }else if(options[[ifelse(type == "Prior","plotsPredictionType", "plotsPredictionPostType")]] == "marginal"){
        
        if(length(all_lines) > 0){
          
          for(i in 1:length(all_lines)){
            
            if(i == 1){
              all_lines_new <- all_lines[[i]]
            }else{
              all_lines_new$y <- all_lines_new$y + all_lines[[i]]$y
            }
            
          }
          all_lines_new$g <- "__marginal"
        }
        
        all_lines_new   <- all_lines_new[seq(1,nrow(all_lines_new),2),]
        if(type == "Posterior" && options[["predictionPostPlotProp"]]){
          all_lines_new$x <- all_lines_new$x + .5/predictionN
        }else{
          all_lines_new$x <- all_lines_new$x + .5
        }
        
        if(type == "Prior"){
          if(options[["plotsPredictionsObserved"]]){
            xBlacked <- data$nSuccesses
          }else{
            xBlacked <- NULL
          }
        }else{
          xBlacked <- NULL
        }
        
        
        if(options[[ifelse(type == "Prior", "plotsPredictionMarginalCI", "plotsPredictionPostMarginalCI")]]){
          
          if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "central"){
            
            dfCI <- .marginalCentralBinomialLS(all_lines_new, NULL, options[["plotsPredictionMarginalCoverage"]], 0, predictionN, TRUE)        
            
          }else if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "HPD"){
            
            dfCI <- .marginalHPDBinomialLS(all_lines_new, list(),
                                           options[[ifelse(type == "Prior", "plotsPredictionMarginalCoverage", "plotsPredictionPostMarginalCoverage")]],
                                           0, predictionN, TRUE)    
            
          }else if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "custom"){
            
            dfCI <- .marginalCustomBinomialLS(all_lines_new, list(),
                                              lCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]],
                                              uCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]],
                                              TRUE)
            
            if(options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]
               > predictionN){
              
              plotsPredictions$setError("The upper CI limit is higher than the number of future 
                                       observations. Please change the value of the upper CI limit 
                                       in the settings panel.")
              
              return()
            }
            if(options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]]
               > predictionN){
              
              plotsPredictions$setError("The lower CI limit is higher than the number of future 
                                       observations. Please change the value of the lower CI limit 
                                       in the settings panel.")
              
              return()
            }
            if(options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]] > 
               options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]){
              
              plotsPredictions$setError("The lower CI limit is higher than the upper CI limit.
                                       Please change the value of the CI limits 
                                       in the settings panel.")
              
              return()
            }
            
          }
        }else{
          dfCI <- NULL
        }
        
        if(type == "Posterior" && options[["predictionPostPlotProp"]]){
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        }else{
          xRange <- c(0, predictionN)
        }
        
        
        p <- .plotPredictionLS(all_lines_new, dfCI, xRange = xRange, xName = xName, yName = yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)
        
      }else{
        p <- .plotStackedLS(all_lines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange, proportions = proportions)
      }
      
      plotsPredictions$plotObject <- p
    }  
  }

  return()
}
.plotsPredictionsIndividualBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "bin_test", type)
  
  if(is.null(containerPlots[[paste0("plotsPredictions",type)]])){
   
    plotsPredictionsIndividual <- createJaspContainer()
    
    plotsPredictionsIndividual$position <- 2
    plotsPredictionsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                                          ifelse(type == "Prior", "plotsPredictionCI",      "plotsPredictionPostCI"),
                                          ifelse(type == "Prior", "plotsPredictionTypeCI",  "plotsPredictionPostTypeCI"),
                                          ifelse(type == "Prior", "plotsPredictionCoverage","plotsPredictionPostCoverage"),
                                          ifelse(type == "Prior", "plotsPredictionLower",   "plotsPredictionPostLower"),
                                          ifelse(type == "Prior", "plotsPredictionUpper",   "plotsPredictionPostUpper"),
                                          ifelse(type == "Prior", "colorPalette",           "colorPalettePrediction"),
                                          ifelse(type == "Prior", "",                       "predictionPostPlotProp")
    ))
    
    
    containerPlots[[paste0("plotsPredictions",type)]] <- plotsPredictionsIndividual
    
    
    if(all(!ready) || (ready[1] && !ready[2])){
      
      plotsPredictionsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    }else if((!ready[1] && ready[2]) || (data$nSuccesses == 0 & data$nFailures == 0)){
      
      for(i in 1:length(options[["priors"]])){
        plotsPredictionsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                      width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    }else{
      
      if(type == "Prior"){
        predictionN  <- data$nSuccesses + data$nFailures
        temp_results <- .testBinomialLS(data, options[["priors"]])
        temp_data    <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      }else if(type == "Posterior"){
        predictionN  <- options[["predictionN"]]
        temp_results <- .testBinomialLS(data, options[["priors"]])
        temp_data    <- data
      }
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsPredictionsIndividual[[options[["priors"]][[i]]$name]] <- temp_plot
        
        if(type == "Posterior" && options[["predictionPostPlotProp"]]){
          xName  <- gettext("Sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
          proportions <- options[["predictionPostPlotProp"]]
        }else{
          xName  <- gettext("Number of successes")
          yName  <- gettext("Probability")
          xRange <- c(0, predictionN)
          proportions <- FALSE
        }
        
        
        dfCI   <- NULL
        dfHist <- NULL
        
        if(options[[ifelse(type == "Prior","plotsPredictionCI","plotsPredictionPostCI")]]){
          
          if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "central"){
            
            dfCI <- .dataCentralBinomialLS(data, options[["priors"]][[i]],
                                           options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                                           n = predictionN,type = "prediction")
            
          }else if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "HPD"){
            
            dfCI <- .dataHPDBinomialLS(data, options[["priors"]][[i]],
                                       options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                                       n = predictionN, type = "prediction")
            
          }else if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "custom"){
            
            dfCI <- .dataCustomBinomialLS(data, options[["priors"]][[i]],
                                          options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]],
                                          options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]],
                                          n = predictionN, type = "prediction")
            
            if(options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]] > predictionN){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The upper CI limit is higher than the number of future observations. Please, change the value of the upper CI limit in the settings panel."))
              
              return()
            }
            if(options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]]  > predictionN){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the number of future observations. Please, change the value of the lower CI limit in the settings panel."))
              
              return()
            }
            if(options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]] 
               > options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]]){
              
              plotsPredictionsIndividual[[options[["priors"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the upper CI limit. Please, change the value of the CI limits in the settings panel."))
              
              return()
            }
            
          }
        }
        
        dfHist  <- .dataHistBinomialLS(temp_data, options[["priors"]][[i]], predictionN)
        
        if(type == "Prior"){
          if(options[["plotsPredictionsObserved"]]){
            xBlacked <- data$nSuccesses
          }else{
            xBlacked <- NULL
          }
        }else{
          xBlacked <- NULL
        }
        
        if(type == "Posterior" && options[["predictionPostPlotProp"]]){
          dfHist$x <- dfHist$x/predictionN
          if(options[["plotsPredictionPostCI"]]){
            dfCI$x_start <- dfCI$x_start/predictionN
            dfCI$x_end   <- dfCI$x_end  /predictionN
          }
          nRound <- 3
        }else{
          nRound <- 0
        }
        
        p <- .plotPredictionLS(dfHist, dfCI, xRange, xName, yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)
        temp_plot$plotObject <- p
      }
    }
  }
  
  return()
}
.plotsPredAccuracyBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerPredictiveAccuracy <- .containerPredictiveAccuracyLS(jaspResults, options, "bin_test")
  
  if(is.null(containerPredictiveAccuracy[["plotsPredAccuracy"]])){
  
    plotsPredAccuracy <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsPredAccuracy$position <- 2
    plotsPredAccuracy$dependOn(c(.BinomialLS_data_dependencies, "colorPalette"))
    
    containerPredictiveAccuracy[["plotsPredAccuracy"]] <- plotsPredAccuracy
    
    
    if(!all(ready) || (data$nSuccesses == 0 && data$nFailures == 0)){
      return()
    }else{
      
      predictionN  <- data$nSuccesses + data$nFailures
      temp_results <- .testBinomialLS(data, options[["priors"]])
      
      dfHist_all   <- NULL
      xRange       <- c(0, predictionN)
      xName        <- gettext("Hypothesis")
      yName        <- gettext("Probability")
      
      
      if(options[["plotsPredictiveAccuracyType"]] == "conditional"){
        temp_y <- exp(temp_results[,"log_lik"])
      }else if(options[["plotsPredictiveAccuracyType"]] == "joint"){
        temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
      }else if(options[["plotsPredictiveAccuracyType"]] == "marginal"){
        temp_y <- temp_results[,"posterior"]
      }
      
      dfHist_all <- data.frame(
        "x" = 1:length(options[["priors"]]),
        "y" = temp_y,
        "g" = sapply(options[["priors"]],function(x)x$name))
      
      if(any(is.nan(dfHist_all$y))){
        plotsPredAccuracy$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
        return()
      }
      
      p <- .plotAccuracyLS(dfHist_all, xName = xName, yName = yName)
      plotsPredAccuracy$plotObject <- p
      
    }
  }
  
  return()
}
.plotsIterativeOverlyingBinomial2LS <- function(jaspResults, data, ready, options){
  
  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "bin_test")
  
  if(is.null(containerSequentialTests[["plotsIterative"]])){
  
    plotsIterative <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
    
    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.BinomialLS_data_dependencies, "colorPalette",
                              "bfTypeSequential", "bayesFactorTypeSequential", "bfTypevsNameSequential"))
    containerSequentialTests[["plotsIterative"]] <- plotsIterative
    
    if(length(data$y) == 0){
      return()
    }
    if(!all(ready)){
      return()
    }
    if(options[["plotsIterativeType"]] == "BF"){
      if(options[["bfTypeSequential"]] == "vs" &&  options[["bfTypevsNameSequential"]] == ""){
        tableIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if(length(options[["priors"]]) < 2){
        plotsIterative$setError("At least 2 hypotheses need to be specified.")
        return()
      }
      if(options[["bfTypeSequential"]] == "best"){
        theBest <- which.max(.testBinomialLS(data, options[["priors"]])$log_lik)
      }
    }
    
    results <- NULL
    if(length(data$y) == 1){
      iter_seq <- c(1, 1.1)
    }else{
      iter_seq <- 1:length(data$y)
    }
    
    for(i in iter_seq){
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      
      temp_results <- .testBinomialLS(temp_data, options[["priors"]])

      if(options[["plotsIterativeType"]] == "conditional"){
        yName  <- "Conditional probability"
        temp_y <- exp(temp_results[,"log_lik"])
      }else if(options[["plotsIterativeType"]] == "joint"){
        yName  <- "Joint probability"
        temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
      }else if(options[["plotsIterativeType"]] == "marginal"){
        yName  <- "Marginal probability"
        temp_y <- temp_results[,"posterior"]
      }else if(options[["plotsIterativeType"]] == "BF"){
        
        if(options[["bfTypeSequential"]] == "inclusion"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            (temp_results$posterior[h] / (1-temp_results$posterior[h])) / (temp_results$prior[h] / (1-temp_results$prior[h]))
            )
        }else if(options[["bfTypeSequential"]] == "best"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            exp(temp_results$log_lik[h]) / exp(temp_results$log_lik[theBest])
          )
        }else if(options[["bfTypeSequential"]] == "vs"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            exp(temp_results$log_lik[h]) / exp(temp_results$log_lik[sapply(options[["priors"]][[h]], function(p)p$n) == options[["bfTypevsNameSequential"]]])
          )
        }

        if(options[["bayesFactorTypeSequential"]] == "BF10"){
          temp_y <- temp_bf
        }else if(options[["bayesFactorTypeSequential"]] == "BF01"){
          temp_y <- 1/temp_bf          
        }else if(options[["bayesFactorTypeSequential"]] == "LogBF10"){
          temp_y <- log(temp_bf)
        }
        
      }
      
      results <- rbind.data.frame(results, temp_y)
      
    }
    
    yName <- switch(
      options[["bayesFactorTypeSequential"]],
      "BF10"    = gettextf("BF%s","\u2081\u2080"),
      "BF01"    = gettextf("BF%s","\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )
    
    plot_data_lines <- list()
    for(h in 1:length(options[["priors"]])){
      if(options[["plotsIterativeType"]] == "BF" && options[["bfTypeSequential"]] == "vs"){
        if(options[["bfTypevsNameSequential"]] == options[["priors"]][[h]]$name)next
      }
      
      temp_lines   <- NULL
      temp_lines   <- rbind(temp_lines, data.frame(
        x    = iter_seq,
        y    = results[,h],
        name = options[["priors"]][[h]]$name
      ))
      plot_data_lines <- c(plot_data_lines, list(temp_lines))
      
    }
    
    xName  <- gettext("Observation")
    
    if(options[["plotsIterativeType"]] == "BF"){
      BF_log <- options[["bayesFactorTypeSequential"]] == "LogBF10"
    }else{
      BF_log <- NULL
    }

    p <- .plotIterativeLS(plot_data_lines, NULL, xName = xName, yName = yName, x_start = 1, palette = options[["colorPalette"]], BF_log = BF_log)
    
    plotsIterative$plotObject <- p
  }
  
  return()
}
.tableIterativeBinomial2LS <- function(jaspResults, data, ready, options){

  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "bin_test")
  
  if(is.null(containerSequentialTests[["tableIterative"]])){
 
    tableIterative <- createJaspTable()
    
    tableIterative$position <- 3
    tableIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeUpdatingTable"))
    containerSequentialTests[["tableIterative"]] <- tableIterative
    
    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observations"), type = "integer")
    if(ready[2]){
      for(i in 1:length(options[["priors"]])){
        tableIterative$addColumnInfo(
          name  = options[["priors"]][[i]]$name,  
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }
    
    
    if(!all(ready)){
      return()
    }
    if(options[["plotsIterativeType"]] == "BF"){
      if(options[["bfTypeSequential"]] == "vs" &&  options[["bfTypevsNameSequential"]] == ""){
        tableIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if(length(options[["priors"]]) < 2){
        tableIterative$setError(gettext("At least 2 hypotheses need to be specified."))
        return()
      }
      if(options[["bfTypeSequential"]] == "best"){
        theBest <- which.max(.testBinomialLS(data, options[["priors"]])$log_lik)
      }
    }
    
    
    results <- NULL
    
    if(length(data$y) > 1){
      iter_seq <- 1:length(data$y)
    }else{
      iter_seq <- 1
    }
    
    for(i in iter_seq){
      
      temp_row     <- list() 
      temp_row[["iteration"]] <- i
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      temp_results <- .testBinomialLS(temp_data, options[["priors"]])
      
      if(options[["plotsIterativeType"]] == "conditional"){
        temp_y <- exp(temp_results[,"log_lik"])
      }else if(options[["plotsIterativeType"]] == "joint"){
        temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
      }else if(options[["plotsIterativeType"]] == "marginal"){
        temp_y <- temp_results[,"posterior"]
      }else if(options[["plotsIterativeType"]] == "BF"){
        
        if(options[["bfTypeSequential"]] == "inclusion"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            (temp_results$posterior[h] / (1-temp_results$posterior[h])) / (temp_results$prior[h] / (1-temp_results$prior[h]))
          )
        }else if(options[["bfTypeSequential"]] == "best"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            exp(temp_results$log_lik[h]) / exp(temp_results$log_lik[theBest])
          )
        }else if(options[["bfTypeSequential"]] == "vs"){
          temp_bf <- sapply(1:nrow(temp_results), function(h)
            exp(temp_results$log_lik[h]) / exp(temp_results$log_lik[sapply(options[["priors"]][[h]], function(p)p$n) == options[["bfTypevsNameSequential"]]])
          )
        }
        
        temp_y <- switch(
          options[["bayesFactorTypeSequential"]],
          "BF10"    = temp_bf,
          "BF01"    = 1/temp_bf,
          "LogBF10" = log(temp_bf)
        )
        
      }
      
      for(h in 1:length(options[["priors"]])){
        
        temp_row[[options[["priors"]][[h]]$name]] <- temp_y[h]
        
      }
      
      tableIterative$addRows(temp_row)
    }
  }
 
  return()
}
.plotsBothBinomialLS2      <- function(jaspResults, data, ready, options){
  
  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "bin_test")
  
  if(is.null(containerBoth[["plotsBoth"]])){
    
    plotsBoth <- createJaspContainer()
    
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.BinomialLS_data_dependencies, "plotsBothSampleProportion"))
    
    containerBoth[["plotsBoth"]] <- plotsBoth
    

    if (!all(ready))return()
    
    all_lines    <- c()
    all_arrows   <- c()
    legend       <- NULL
    temp_results <- .testBinomialLS(data, options[["priors"]])
    
    if(any(is.nan(temp_results$posterior))){
      plotsBoth_error <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
      plotsBoth[["plotsBoth_error"]] <- plotsBoth_error
      plotsBoth_error$setError(gettext("The plot could not be created because the posterior model probabilities are not defined."))
      return()
    }
    
    for(i in 1:length(options[["priors"]])){
      
      if(options[["priors"]][[i]]$type == "spike"){
        
        dfArrowPP_prior       <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP_posterior   <- .dataArrowBinomialLS(options[["priors"]][[i]])
        dfArrowPP_prior$g     <- "Prior"
        dfArrowPP_posterior$g <- "Posterior"
        dfArrowPP_prior$y_end     <- exp(log(dfArrowPP_prior$y_end)     + log(temp_results[i, "prior"]))
        dfArrowPP_posterior$y_end <- exp(log(dfArrowPP_posterior$y_end) + log(temp_results[i, "posterior"]))
        
        all_arrows      <- c(all_arrows, list(rbind(dfArrowPP_posterior, dfArrowPP_prior)))
        
      }else if(options[["priors"]][[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
        dfLinesPP$y[dfLinesPP$g == "prior"]     <- exp(log(dfLinesPP$y[dfLinesPP$g == "prior"])+log(temp_results[i, "prior"]))
        dfLinesPP$y[dfLinesPP$g == "posterior"] <- exp(log(dfLinesPP$y[dfLinesPP$g == "posterior"])+log(temp_results[i, "posterior"]))
        
        all_lines   <- c(all_lines, list(dfLinesPP))
      }
    }
    
    if(options[["plotsBothSampleProportion"]]){
      dfPointsPP <- .dataProportionBinomialLS(data)
      if(is.nan(dfPointsPP$x))dfPointsPP <- NULL
    }else{
      dfPointsPP <- NULL
    }
    xName  <- bquote(.(gettext("Population proportion"))~theta)
    
    if(options[["plotsBothType"]] == "joint"){
      
      spikes_i <- 1
      betas_i  <- 1
      
      for(i in 1:length(options[["priors"]])){
        temp_plotsBoth <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        plotsBoth[[paste0("plotsBoth_",i)]] <- temp_plotsBoth
        
        if(options[["priors"]][[i]]$type == "spike"){
          temp_p   <- .plotPriorPosteriorLS(NULL, all_arrows[spikes_i], dfPoints = dfPointsPP, xName = xName)
          spikes_i <- spikes_i + 1
        }else if(options[["priors"]][[i]]$type == "beta"){
          temp_p   <- .plotPriorPosteriorLS(all_lines[betas_i], NULL, dfPoints = dfPointsPP, xName = xName)
          betas_i  <- betas_i + 1        
        }
        
        temp_plotsBoth$plotObject <- temp_p
      }
      
      
    }else if(options[["plotsBothType"]] == "marginal"){
      
      plotsBoth_plot <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
      plotsBoth[["plotsBoth_plot"]] <- plotsBoth_plot
      
      all_lines_new <- c()
      
      if(length(all_lines) > 0){
        
        for(i in 1:length(all_lines)){
          
          if(i == 1){
            all_lines_new[[1]] <- all_lines[[i]]
          }else{
            all_lines_new[[1]]$y <- all_lines_new[[1]]$y + all_lines[[i]]$y
          }
          
        }
      }
      
      p <- .plotPriorPosteriorLS(all_lines_new, all_arrows, dfPoints = dfPointsPP, xName = xName)
      plotsBoth_plot$plotObject <- p
      
    }
  }
 
  return()
}
.plotsBothIndividualBinomial2LS <- function(jaspResults, data, ready, options){

  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "bin_test")
  
  if(is.null(containerBoth[["plotsBoth"]])){
  
    plotsBoth <- createJaspContainer()
    
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.BinomialLS_data_dependencies, "plotsBothSampleProportion"))
    
    containerBoth[["plotsBoth"]] <- plotsBoth
    
    
    if(all(!ready) || (ready[1] && !ready[2])){
      
      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()
      
    }else if(!ready[1] && ready[2]){
      
      for(i in 1:length(options[["priors"]])){
        plotsBoth[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()
      
    }else{
      
      for(i in 1:length(options[["priors"]])){
        
        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        
        plotsBoth[[options[["priors"]][[i]]$name]] <- temp_plot
        
        dfArrowPP <- NULL
        dfLinesPP <- NULL
        
        xName  <- bquote(.(gettext("Population proportion"))~theta)
        
        if(options[["priors"]][[i]]$type == "spike"){
          dfArrowPP  <- .dataArrowBinomialLS(options[["priors"]][[i]])
        }else if(options[["priors"]][[i]]$type == "beta"){
          dfLinesPP  <- .dataLinesBinomialLS(data, options[["priors"]][[i]])
          
          if(all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])){
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- "Prior = Posterior"
          }
          
        }
        
        if(options[["plotsBothSampleProportion"]]){
          dfPointsPP <- .dataProportionBinomialLS(data)
          if(is.nan(dfPointsPP$x))dfPointsPP <- NULL
        }else{
          dfPointsPP <- NULL 
        }
        
        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName)
        temp_plot$plotObject <- p
      }
    }
  }
  
  return()
}
