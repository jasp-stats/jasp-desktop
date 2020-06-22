reliabilityBayesian <- function(jaspResults, dataset, options) {
  

  
	dataset <- .reliabilityReadData(dataset, options)

	.reliabilityCheckErrors(dataset, options)
  
	model <- .BayesianReliabilityMainResults(jaspResults, dataset, options)

	.BayesianReliabilityScaleTable(         jaspResults, model, options)
	.BayesianReliabilityItemTable(          jaspResults, model, options)
	.BayesianReliabilityProbTable(          jaspResults, model, options)
	.BayesianReliabilityPosteriorPlot(      jaspResults, model, options)
	.BayesianReliabilityPosteriorPredictive(jaspResults, model, options)
	.BayesianReliabilityIfItemPlot(         jaspResults, model, options)
	.BayesianReliabilityTracePlot(          jaspResults, model, options)
	  

	return()

}

# read data, check errors----
.BayesianReliabilityDerivedOptions <- function(options) {

  # order of appearance in Bayesrel
  derivedOptions <- list(
    selectedEstimators  = unlist(options[c("mcDonaldScale", "alphaScale", "guttman2Scale", "guttman6Scale", "glbScale",
                                           "averageInterItemCor", "meanScale", "sdScale")]),
    selectedEstimatorsPlots  = unlist(options[c("mcDonaldScale", "alphaScale", "guttman2Scale", "guttman6Scale", 
                                                "glbScale")]),
    itemDroppedSelected = unlist(options[c("mcDonaldItem", "alphaItem", "guttman2Item", "guttman6Item","glbItem",
                                           "itemRestCor", "meanItem", "sdItem")]),
    itemDroppedSelectedItem = unlist(options[c("mcDonaldItem", "alphaItem", "guttman2Item", "guttman6Item", 
                                               "glbItem")]),

    namesEstimators     = list(
      tables = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                 "Greatest Lower Bound", "Average interitem correlation", "mean", "sd"),
      tables_item = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                      "Greatest Lower Bound", "Item-rest correlation", "mean", "sd"),
      coefficients = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                       "Greatest Lower Bound", "Item-rest correlation"),
      plots = list(expression("McDonald's"~omega), expression("Cronbach\'s"~alpha), expression("Guttman's"~lambda[2]), 
                   expression("Guttman's"~lambda[6]), "Greatest Lower Bound")
    )

  )

  # order to show in JASP : check that again when more or different estimators are included
  derivedOptions[["order"]] <- c(5, 1, 2, 3, 4, 6, 7, 8)

  return(derivedOptions)
}

.reliabilityReadData <- function(dataset, options) {

  variables <- unlist(options[["variables"]])
	if (is.null(dataset)) {
		dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = NULL, exclude.na.listwise = NULL)
	}
  return(dataset)
}

.reliabilityCheckErrors <- function(dataset, options) {

  .hasErrors(dataset = dataset, perform = "run",
             type = c("infinity", "variance", "observations", "varCovData"),
             observations.amount = " < 3",
             custom = .checkEigen,
             exitAnalysisIfErrors = TRUE)
  
}
# check for negative eigenvalues, positive semidefiniteness
.checkEigen <- function() {
  if (any(eigen(cov(x))$values < 0)) {
    return(gettext("Data covariance matrix is not positive semidefinite"))
  }
}

.reliabilityCheckLoadings <- function(dataset, variables) {
  if (ncol(dataset > 2)) {
    prin <- psych::principal(dataset)
    idx <- prin[["loadings"]] < 0
    sidx <- sum(idx)
    if (sidx == 0) {
      return("")
    } else {
      footnote <- .footnoteNegativeCorrelation(variables, idx)
      return(footnote)
    }
  } else {
    return("")  
  }
}


# estimate reliability ----
.BayesianReliabilityMainResults <- function(jaspResults, dataset, options) {
  if (!options[["mcDonaldScale"]] && !options[["alphaScale"]] && !options[["guttman2Scale"]]
      && !options[["guttman6Scale"]] && !options[["glbScale"]] && !options[["averageInterItemCor"]]
      && !options[["meanScale"]] && !options[["sdScale"]]) {
    variables <- options[["variables"]]
    if (length(options[["reverseScaledItems"]]) > 0L) {
      dataset <- .reverseScoreItems(dataset, options)
    }
    model <- list()
    model[["footnote"]] <- .reliabilityCheckLoadings(dataset, variables)
    return(model)
  }
  
  model <- jaspResults[["modelObj"]]$object
  relyFit <- model[["relyFit"]]
  if (is.null(model)) {
    model <- list()
    variables <- options[["variables"]]

    if (length(variables) > 2L) {

      if (length(options[["reverseScaledItems"]]) > 0L) {
        dataset <- .reverseScoreItems(dataset, options)
      }
      
      missing <- "none" 
      options[["missings"]] <- "everything"
      if (any(is.na(dataset))) {
        if (options[["missingValues"]] == "excludeCasesPairwise") {
          missing <- "pairwise"
          options[["missings"]] <- "pairwise.complete.obs"
        } else if (options[["missingValues"]] == "excludeCasesListwise") {
          pos <- which(is.na(dataset), arr.ind = T)[, 1]
          dataset <- dataset[-pos, ] 
          missing <- "listwise"
          options[["missings"]] <- "complete.obs"
          }
      }
      model[["footnote"]] <- .reliabilityCheckLoadings(dataset, variables)
      
      chains <- options[["noChains"]]
      samples <- options[["noSamples"]]
      p <- ncol(dataset)
      startProgressbar((chains*samples)*7 # cov sampling + every coefficient (also avg_cor)
                       + (chains*samples)*6*p) # every coefficient for if item samples (also item_rest_cor)
      
      if (options[["setSeed"]]) {
        set.seed(options[["seedValue"]])
      }
      relyFit <- try(Bayesrel::strel(data = dataset, estimates=c("alpha", "lambda2", "lambda6", "glb", "omega"), 
                                     n.iter = options[["noSamples"]], n.burnin = options[["noBurnin"]], 
                                     n.chains = options[["noChains"]], thin = options[["noThin"]],
                                     freq = F, item.dropped = TRUE, missing = missing, callback = progressbarTick))
      
      if (any(is.na(dataset))) {
        if (!is.null(relyFit[["miss_pairwise"]])) {
          model[["footnote"]] <- gettextf("%s Using pairwise complete cases. ", model[["footnote"]])
        } else {
          model[["footnote"]] <- gettextf("%s Using %1.f complete cases. ", model[["footnote"]], nrow(dataset))
        }
      }

      
      # add the scale info
      corsamp <- apply(relyFit$Bayes$covsamp, c(1, 2), .cov2cor.callback, progressbarTick)
      relyFit$Bayes$samp$avg_cor <- coda::mcmc(apply(corsamp, c(2, 3), function(x) mean(x[x!=1])))
      relyFit$Bayes$est$avg_cor <- mean(relyFit$Bayes$samp$avg_cor)
      
      # get rid of multiple chains, first save the chains:
      relyFit$Bayes$chains <- relyFit$Bayes$samp
      relyFit$Bayes$samp <- lapply(relyFit$Bayes$chains, .chainSmoker)

      # mean and sd
      relyFit$Bayes$samp$mean <- NA_real_
      relyFit$Bayes$est$mean <- mean(rowMeans(dataset, na.rm = T))
      relyFit$Bayes$samp$sd <- NA_real_
      relyFit$Bayes$est$sd <- sd(colMeans(dataset, na.rm = T))
      
      # get rid of multiple chains, first save the chains:
      relyFit$Bayes$ifitem$chains <- relyFit$Bayes$ifitem$samp
      relyFit$Bayes$ifitem$samp <- lapply(relyFit$Bayes$ifitem$chains, .chainSmoker)

      # now the item statistics
      relyFit$Bayes$ifitem$samp$ircor <- .reliabilityItemRestCor(dataset, options[["noSamples"]], options[["noBurnin"]], 
                                                                 options[["noThin"]], options[["noChains"]], missing, 
                                                                 callback = progressbarTick)
      relyFit$Bayes$ifitem$est$ircor <- apply(relyFit$Bayes$ifitem$samp$ircor, 2, mean)
      
      # mean and sd
      relyFit$Bayes$ifitem$est$mean <- colMeans(dataset, na.rm = T)
      relyFit$Bayes$ifitem$est$sd <- apply(dataset, 2, sd, na.rm = T)
      relyFit$Bayes$ifitem$samp$mean <- (matrix(NA_real_, ncol(dataset), 2))
      relyFit$Bayes$ifitem$samp$sd <- (matrix(NA_real_, ncol(dataset), 2))
      
      ops <- .BayesianReliabilityDerivedOptions(options)
      order <- ops[["order"]]
      relyFit[["Bayes"]][["samp"]] <- relyFit[["Bayes"]][["samp"]][order]
      relyFit[["Bayes"]][["chains"]] <- relyFit[["Bayes"]][["chains"]][order]
      relyFit[["Bayes"]][["est"]] <- relyFit[["Bayes"]][["est"]][order]
      
      relyFit[["Bayes"]][["ifitem"]][["samp"]] <- relyFit[["Bayes"]][["ifitem"]][["samp"]][order]
      relyFit[["Bayes"]][["ifitem"]][["est"]] <- relyFit[["Bayes"]][["ifitem"]][["est"]][order]
      

      # Consider stripping some of the contents of relyFit to reduce memory load
      if (inherits(relyFit, "try-error")) {

        model[["error"]] <- paste("The analysis crashed with the following error message:\n", relyFit)

      } else {
        
        model[["dataset"]] <- dataset

        model[["relyFit"]] <- relyFit
        
        model[["options"]] <- options

        stateObj <- createJaspState(model)
        stateObj$dependOn(options = c("variables", "reverseScaledItems", "noSamples", "noBurnin", "noChains", "noThin",
                                      "missingValues", "setSeed", "seedValue"))
        jaspResults[["modelObj"]] <- stateObj
      }
    }
  }

	if (is.null(model[["error"]])) {
	  criState <- jaspResults[["criObj"]]$object
	  if (is.null(criState) && !is.null(relyFit)) {

	    scaleCri <- .BayesianReliabilityCalcCri(relyFit[["Bayes"]][["samp"]],             
	                                            options[["credibleIntervalValueScale"]])
	    itemCri  <- .BayesianReliabilityCalcCri(relyFit[["Bayes"]][["ifitem"]][["samp"]], 
	                                            options[["credibleIntervalValueItem"]])
      
	    
	    criState <- list(scaleCri = scaleCri, itemCri  = itemCri)
	    jaspCriState <- createJaspState(criState)
	    jaspCriState$dependOn(options = c("credibleIntervalValueScale", "credibleIntervalValueItem"), 
	                                      optionsFromObject = jaspResults[["modelObj"]])
	    jaspResults[["criObj"]] <- jaspCriState

	  }
	  model[["cri"]] <- criState
	}

  model[["derivedOptions"]] <- .BayesianReliabilityDerivedOptions(options)
  model[["itemsDropped"]] <- .unv(colnames(dataset))
  
	return(model)
}

.BayesianReliabilityCalcCri <- function(samps, criValue) {

  cri <- vector("list", length(samps))
  names(cri) <- names(samps)
  
  for (nm in names(samps)) {
    if (any(is.na(samps[[nm]]))) {
      cri[[nm]] <- c(NA_real_, NA_real_)
    } else {
      cri[[nm]] <- coda::HPDinterval(coda::mcmc(samps[[nm]]), prob = criValue)
    }
  }
  return(cri)
}





# ----------------------------- tables ------------------------------------
.BayesianReliabilityScaleTable <- function(jaspResults, model, options) {
  if (!is.null(jaspResults[["scaleTable"]])) {
    return()
  }
  
  scaleTable <- createJaspTable(gettext("Bayesian Scale Reliability Statistics"))
  scaleTable$dependOn(options = c("variables", "mcDonaldScale", "alphaScale", "guttman2Scale",  "guttman6Scale",
                                  "glbScale", "reverseScaledItems", "credibleIntervalValueScale", "noSamples", "noBurnin",
                                  "noChains", "noThin", "setSeed", "seedValue",
                                  "averageInterItemCor", "meanScale", "sdScale", "missingValues", "rHat"))
  
  scaleTable$addColumnInfo(name = "estimate", title = "Estimate", type = "string")
  intervalLow <- gettextf("%s%% CI",
                          format(100*options[["credibleIntervalValueScale"]], digits = 3, drop0trailing = TRUE))
  intervalUp <- gettextf("%s%% CI",
                         format(100*options[["credibleIntervalValueScale"]], digits = 3, drop0trailing = TRUE))
  intervalLow <- gettextf("%s lower bound", intervalLow)
  intervalUp <- gettextf("%s upper bound", intervalUp)
  
  if (options[["rHat"]]) {
    allData <- data.frame(estimate = c("Posterior mean", intervalLow, intervalUp, "R-hat"))
  } else {
    allData <- data.frame(estimate = c("Posterior mean", intervalLow, intervalUp))
  }
  
  if ((!options[["mcDonaldScale"]] && !options[["alphaScale"]] && !options[["guttman2Scale"]] 
       && !options[["guttman6Scale"]] && !options[["glbScale"]] && !options[["averageInterItemCor"]]
       && !options[["meanScale"]] && !options[["sdScale"]])) {

    scaleTable$setData(allData)
    nvar <- length(options[["variables"]])
    if (nvar > 0L && nvar < 3L)
      scaleTable$addFootnote(gettextf("Please enter at least 3 variables to do an analysis. %s", model[["footnote"]]))
    else
      scaleTable$addFootnote(gettext(model[["footnote"]]))
    jaspResults[["scaleTable"]] <- scaleTable
    scaleTable$position <- 1
    return()
  }
  
  
  relyFit <- model[["relyFit"]]
  derivedOptions <- model[["derivedOptions"]]
  opts     <- derivedOptions[["namesEstimators"]][["tables"]]
  order    <- derivedOptions[["order"]]
  selected <- derivedOptions[["selectedEstimators"]]
  idxSelected <- which(selected)
  
  if (!is.null(relyFit)) {

    for (i in idxSelected) {
      scaleTable$addColumnInfo(name = paste0("est", i), title = opts[i], type = "number")
      if (options[["rHat"]]) {
        if (opts[i] == "mean" || opts[i] == "sd") {
          rhat <- NA_real_
        } else {
          tmp <- lapply(as.data.frame(t(relyFit[["Bayes"]][["chains"]][[i]])), coda::mcmc)
          rhat <- coda::gelman.diag(coda::as.mcmc.list(tmp))[["psrf"]][, 1]
        }
        newData <- data.frame(est = c(unlist(relyFit$Bayes$est[[i]], use.names = F), 
                                      unlist(model[["cri"]][["scaleCri"]][[i]], use.names = F), 
                                      rhat))
      } else {
        newData <- data.frame(est = c(unlist(relyFit$Bayes$est[[i]], use.names = F), 
                                      unlist(model[["cri"]][["scaleCri"]][[i]], use.names = F))) 
      }
      colnames(newData) <- paste0(colnames(newData), i)
      allData <- cbind(allData, newData)
    }
    scaleTable$setData(allData)
    
    if (!is.null(model[["footnote"]]))
      scaleTable$addFootnote(gettext(model[["footnote"]]))
    
  } else if (sum(selected) > 0L) {
    
    for (i in idxSelected) {
      scaleTable$addColumnInfo(name = paste0("est", i), title = opts[i], type = "number")
    }
    
    nvar <- length(options[["variables"]])
    if (nvar > 0L && nvar < 3L)
      scaleTable$addFootnote(gettext("Please enter at least 3 variables to do an analysis."))
  }
  if (!is.null(model[["error"]]))
    scaleTable$setError(model[["error"]])
  
  if (!is.null(model[["footnote"]]))
    scaleTable$addFootnote(gettext(model[["footnote"]]))
  
  jaspResults[["scaleTable"]] <- scaleTable
  scaleTable$position = 1
  return()
}


.BayesianReliabilityItemTable <- function(jaspResults, model, options) {
  
	if (!is.null(jaspResults[["itemTable"]]) || !any(model[["derivedOptions"]][["itemDroppedSelected"]])) {
		return()
	}

  derivedOptions <- model[["derivedOptions"]]

  # fixes issue that unchecking the scale coefficient box, does not uncheck the item-dropped coefficient box:
  for (i in 1:5) {
      if (!derivedOptions[["selectedEstimators"]][i]) {
        derivedOptions[["itemDroppedSelected"]][i] <- derivedOptions[["selectedEstimators"]][i]
      }
  }
  
  itemDroppedSelected <- derivedOptions[["itemDroppedSelected"]]
  overTitles <- format(derivedOptions[["namesEstimators"]][["tables_item"]], digits = 3, drop0trailing = T)
  overTitles <- gettextf("%s (if item dropped)", overTitles)
  
  cred <- format(100*options[["credibleIntervalValueItem"]], digits = 3, drop0trailing = TRUE)
  itemTable <- createJaspTable(gettext("Bayesian Individual Item Reliability Statistics"))
  itemTable$dependOn(options = c("variables",
                                 "mcDonaldScale", "alphaScale", "guttman2Scale",  "guttman6Scale", "glbScale", 
                                 "averageInterItemCor", "meanScale", "sdScale",
                                 "mcDonaldItem",  "alphaItem",  "guttman2Item",  "guttman6Item", "glbItem",
                                 "reverseScaledItems", "credibleIntervalValueItem", 
                                 "itemRestCor", "meanItem", "sdItem", "missingValues", "setSeed", "seedValue"))
  
  itemTable$addColumnInfo(name = "variable", title = "Item", type = "string")

  idxSelected <- which(itemDroppedSelected)
  estimators <- derivedOptions[["namesEstimators"]][["tables_item"]]
  coefficients <- derivedOptions[["namesEstimators"]][["coefficients"]]
  
  for (i in idxSelected) {
    if (estimators[i] %in% coefficients) {
      if (estimators[i] == "Item-rest correlation") { # no item deleted for item rest cor
        itemTable$addColumnInfo(name = paste0("postMean", i), title = "Posterior Mean", type = "number", 
                                overtitle = gettext("Item-rest correlation"))
        itemTable$addColumnInfo(name = paste0("lower", i), title = paste0("Lower ", cred, "%"), type = "number", 
                                overtitle = gettext("Item-rest correlation"))
        itemTable$addColumnInfo(name = paste0("upper", i), title = paste0("Upper ", cred, "%"), type = "number", 
                                overtitle = gettext("Item-rest correlation"))
      } else {
        itemTable$addColumnInfo(name = paste0("postMean", i), title = "Posterior Mean", type = "number", 
                                overtitle = overTitles[i])
        itemTable$addColumnInfo(name = paste0("lower", i), title = paste0("Lower ", cred, "%"), type = "number", 
                                overtitle = overTitles[i])
        itemTable$addColumnInfo(name = paste0("upper", i), title = paste0("Upper ", cred, "%"), type = "number", 
                                overtitle = overTitles[i])
      }
    } else {
      itemTable$addColumnInfo(name = paste0("postMean", i), title = estimators[i], type = "number")
    }

  }

  relyFit <- model[["relyFit"]]
  if (!is.null(relyFit)) {
    cris <- model[["cri"]][["itemCri"]]
    tb <- data.frame(variable = model[["itemsDropped"]])
    for (i in idxSelected) {
      if (i %in% c(1:6)) { # check this when more estimators are included !!!!!!!!!!!!!!!!!!!!!
        newtb <- cbind(postMean = relyFit$Bayes$ifitem$est[[i]], cris[[i]])
      } else {
        newtb <- cbind(postMean = relyFit$Bayes$ifitem$est[[i]])
      }
      colnames(newtb) <- paste0(colnames(newtb), i)
      tb <- cbind(tb, newtb)
    }
		itemTable$setData(tb)

  } else if (length(model[["itemsDropped"]]) > 0) {
    itemTable[["variable"]] <- model[["itemsDropped"]]
  }

  jaspResults[["itemTable"]] <- itemTable
  
  itemTable$position = 2
	return()
}


.BayesianReliabilityProbTable <- function(jaspResults, model, options) {

  if (!is.null(jaspResults[["probTable"]]) || !options[["probTable"]]) {
		return()
  }

  probTable <- createJaspTable(
    gettextf("Probability that Reliability Statistic is Larger than %.2f and Smaller than %.2f", 
            options[["probTableValueLow"]], options[["probTableValueHigh"]]))
  probTable$dependOn(options = c("variables", "mcDonaldScale", "alphaScale", "guttman2Scale", "guttman6Scale",
                                 "glbScale", "reverseScaledItems", "probTableValueLow", "probTable",
                                 "probTableValueHigh", "missingValues", "setSeed", "seedValue"))
  overTitle <- format("Probability",
                      digits = 3, drop0trailing = T)
  probTable$addColumnInfo(name = "statistic", title = "Statistic",   type = "string")
	probTable$addColumnInfo(name = "prior",     title = "Prior", type = "number", overtitle = overTitle )
	probTable$addColumnInfo(name = "posterior", title = "Posterior", type = "number", overtitle = overTitle )
	

  relyFit <- model[["relyFit"]]
	derivedOptions <- model[["derivedOptions"]]
	order    <- derivedOptions[["order"]]
	order <- order[1:(length(order)-3)] # dont need avgCor, mean and sd 
	opts     <- derivedOptions[["namesEstimators"]][["tables"]]
	selected <- derivedOptions[["selectedEstimatorsPlots"]]
	idxSelected  <- which(selected)



	if (!is.null(relyFit)) {
	  n.item <- dim(relyFit$Bayes$covsamp)[3]
	  prior <- Bayesrel:::priors[[as.character(n.item)]] 
	  prior <- prior[order]
	  end <- length(prior[[1]][["x"]])
	  poslow <- end - sum(prior[[1]][["x"]] > options[["probTableValueLow"]]) 
	  poshigh <- end - sum(prior[[1]][["x"]] > options[["probTableValueHigh"]]) 
	  # since the priors are only available in density form, the prior probability for the estimator being larger than
	  # a cutoff is given by caculating the relative probability of the density from the cutoff to 1.
	  # maybe check this with someone though
	  
    probsPost <- numeric(sum(selected))
    probsPrior <- numeric(sum(selected))
    z <- 1
    for (i in idxSelected) {
      probsPost[z] <- mean(relyFit[["Bayes"]][["samp"]][[i]] > options[["probTableValueLow"]]) -
                      mean(relyFit[["Bayes"]][["samp"]][[i]] > options[["probTableValueHigh"]])
      probsPrior[z] <- sum(prior[[i]][["y"]][poslow:end]) / sum(prior[[i]][["y"]]) - 
                       sum(prior[[i]][["y"]][poshigh:end]) / sum(prior[[i]][["y"]])
      z <- z+1 # very bulky thing to do here
    }
    df <- data.frame(statistic = opts[idxSelected], prior = probsPrior, posterior = probsPost)
    probTable$setData(df)
  } else if (sum(selected) > 0) {
    probTable[["statistic"]] <- opts[idxSelected]
  }

  jaspResults[["probTable"]] <- probTable
  probTable$position = 3
  
  return()
}


# -------------------------------------------- plots ---------------------------------
.BayesianReliabilityPosteriorPlot <- function(jaspResults, model, options) {

	if (!options[["plotPosterior"]])
		return()

  plotContainer <- jaspResults[["plotContainer"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(gettext("Posterior Plots"))
    plotContainer$dependOn(options = c("variables", "reverseScaledItems", "plotPosterior", "shadePlots",
                                       "probTable", "probTableValueLow", "probTableValueHigh", "fixXRange", 
                                       "dispPrior", "noSamples", "noBurnin", "noChains", "noThin",
                                       "credibleIntervalValueScale","alphaScale", "guttman2Scale", "guttman6Scale", 
                                       "glbScale", "mcDonaldScale", "missingValues", "setSeed", "seedValue"))
    jaspResults[["plotContainer"]] <- plotContainer
  }

	derivedOptions <- model[["derivedOptions"]]
	order     <- derivedOptions[["order"]]
	order <- order[1:(length(order)-3)] # dont need avgCor, mean and sd 
	indices   <- which(derivedOptions[["selectedEstimatorsPlots"]])
	nmsLabs   <- derivedOptions[["namesEstimators"]][["plots"]]
	nmsObjs   <- derivedOptions[["namesEstimators"]][["tables"]]

	relyFit  <- model[["relyFit"]]
	scaleCri <- model[["cri"]][["scaleCri"]]


	if (options[["shadePlots"]] && options[["probTable"]]) {
	  shadePlots <- c(options[["probTableValueLow"]], options[["probTableValueHigh"]])
	} else {
	  shadePlots <- NULL
	}


	if (!is.null(relyFit)) {
	  n.item <- dim(relyFit$Bayes$covsamp)[3]
	  prior <- Bayesrel:::priors[[as.character(n.item)]][order] ##### change this when more estimators are included!!!
	  
	  for (i in indices) {
	    if (is.null(plotContainer[[nmsObjs[i]]])) {

	      p <- .BayesianReliabilityMakeSinglePosteriorPlot(relyFit, scaleCri, i, nmsLabs[[i]], options[["fixXRange"]],
	                                                       shadePlots, options[["dispPrior"]], prior)
	      plotObj <- createJaspPlot(plot = p, title = nmsObjs[i])
	      plotObj$dependOn(options = names(indices[i]))
	      plotObj$position <- i
	      plotContainer[[nmsObjs[i]]] <- plotObj

	    } 
	  }
	} else if (length(indices) > 0) {
	  for (i in indices) {
	    plotObj <- createJaspPlot(title = nmsObjs[i])
	    plotObj$dependOn(options = names(indices[i]))
	    plotObj$position <- i
	    plotContainer[[nmsObjs[i]]] <- plotObj
	  }
	} else {
	  plotContainer[["Posterior Plots"]] <- createJaspPlot()
	}
  plotContainer$position <- 5 
	return()
}


.BayesianReliabilityMakeSinglePosteriorPlot <- function(relyFit, scaleCri, i, nms, fixXRange, 
                                                        shade = NULL, priorTrue, priorSample) {

  # TODO: consider precomputing all densities (maybe with kernsmooth?) and reducing memory that way
  pr <- priorSample[[i]]
  if (fixXRange) {
    d <- stats::density(relyFit$Bayes$samp[[i]], from = 0, to = 1, n = 2^10)
  } else {
    d <- stats::density(relyFit$Bayes$samp[[i]], n = 2^10)
  }
	datDens <- data.frame(x = d$x, y = d$y)
	datPrior <- data.frame(x = pr$x, y = pr$y)
	


	# xBreaks <- JASPgraphs::getPrettyAxisBreaks(datDens$x)
	xBreaks <- pretty(datDens$x, n = 4)
	# max height posterior is at 90% of plot area; remainder is for credible interval
	ymax <- max(d$y) / .9
	yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, ymax))
	ymax <- max(yBreaks)
	datCri <- data.frame(xmin = scaleCri[[i]][1L], xmax = scaleCri[[i]][2L], y = .925 * ymax)
	height <- (ymax - .925 * ymax) / 2
	if (fixXRange) {
	  datTxt <- data.frame(x = c(datCri$xmin -.08, datCri$xmax + .08),
	                       y = 0.985 * ymax,
	                       label = sapply(c(datCri$xmin, datCri$xmax), format, digits = 3, scientific = -1),
	                       stringsAsFactors = FALSE)
	} else {
	  datTxt <- data.frame(x = c(datCri$xmin, datCri$xmax),
	                       y = 0.985 * ymax,
	                       label = sapply(c(datCri$xmin, datCri$xmax), format, digits = 3, scientific = -1),
	                       stringsAsFactors = FALSE)
	}


	if (datCri$xmin[1L] < 0) {
	  datCri$xmin[1L] <- 0
	  datTxt$x[1L] <- 0
	  datTxt$label[1L] <- "< 0"
	}

	# if the bounds are less than 0.05 away from 0 or 1, expand the axis by 0.1 so the credible interval text does not
	# get chopped off.
	xExpand <- .1 * ((c(0, 1) - datTxt$x) <= 0.05)

	g <- ggplot2::ggplot(data = datDens, mapping = ggplot2::aes(x = x, y = y)) +
		ggplot2::geom_line(size = .85) +
		ggplot2::geom_errorbarh(data = datCri, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
		                        height = height, inherit.aes = FALSE) +
	ggplot2::geom_text(data = datTxt, mapping = ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE, 
	                   size = 5) +
	ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
	ggplot2::scale_x_continuous(name = nms, breaks = (xBreaks), expand = xExpand)

	
	if (!is.null(shade)) {
	  datFilter <- datDens[datDens[["x"]] >= shade[1] & datDens[["x"]] <= shade[2], ]
	  g <- g + ggplot2::geom_ribbon(data = datFilter, mapping = ggplot2::aes(ymin = 0, ymax = y), 
	                                fill = "grey", alpha = 0.95) +
	           ggplot2::geom_line(size = .85)
	}
	
	if (priorTrue) {
	  g <- g + ggplot2::geom_line(data = datPrior, mapping = ggplot2::aes(x = x, y = y),
	                              linetype = "dashed", size = .85) +
	           ggplot2::scale_x_continuous(name = nms, breaks = xBreaks, expand = xExpand,
	                                       limits = c(min(xBreaks), max(xBreaks)))
	  
	}

	


	return(JASPgraphs::themeJasp(g))

}


.BayesianReliabilityIfItemPlot <- function(jaspResults, model, options) {
  
  if (!options[["plotItem"]])
    return()
  
  plotContainerItem <- jaspResults[["plotContainerItem"]]
  if (is.null(plotContainerItem)) {
    plotContainerItem <- createJaspContainer(gettext("If Item Dropped Posterior Plots"))
    plotContainerItem$dependOn(options = c("variables", "plotItem", "noSamples", "noBurnin", "noChains", "noThin",
                                           "credibleIntervalValueItem", "orderType", "orderItem",
                                           "reverseScaledItems", "missingValues", "setSeed", "seedValue"))
    jaspResults[["plotContainerItem"]] <- plotContainerItem
  } 
  
  derivedOptions <- model[["derivedOptions"]]
  order     <- derivedOptions[["order"]]
  order <- order[1:(length(order)-3)] # dont need itemrestCor, mean and sd 
  indices   <- which(derivedOptions[["itemDroppedSelectedItem"]])
  nmsLabs   <- derivedOptions[["namesEstimators"]][["plots"]]
  nmsObjs   <- derivedOptions[["namesEstimators"]][["tables_item"]]
  
  relyFit  <- model[["relyFit"]]
  
  if (options[["orderItem"]]) {
      ordering <- options[["orderType"]]
  } else {ordering <- NULL}
  if (!is.null(relyFit)) {
    for (i in indices) {
      if (is.null(plotContainerItem[[nmsObjs[i]]])) {
        p <- .BayesianReliabilityMakeIfItemPlot(relyFit, i, nmsLabs[[i]], options[["credibleIntervalValueItem"]], 
                                                ordering = ordering, options[["variables"]])
        plotObjItem <- createJaspPlot(plot = p, title = nmsObjs[i])
        plotObjItem$dependOn(options = names(indices[i]))
        plotObjItem$position <- i
        plotContainerItem[[nmsObjs[i]]] <- plotObjItem
        
      } 
    }
  } else if (length(indices) > 0) {
    for (i in indices) {
      plotObjItem <- createJaspPlot(title = nmsObjs[i])
      plotObjItem$dependOn(options = names(indices[i]))
      plotObjItem$position <- i
      plotContainerItem[[nmsObjs[i]]] <- plotObjItem
    }
  } else {
    plotContainerItem[["If Item Dropped Posterior Plots"]] <- createJaspPlot()
  }
  
  plotContainerItem$position <- 6 
  return()
}

.BayesianReliabilityMakeIfItemPlot <- function(relyFit, j, nms, int, ordering, variables) {
  n_row <- length(unlist(relyFit$Bayes$ifitem$est[1]))
  lower <- (1-int)/2
  upper <- int + (1-int)/2

  dat <- data.frame(as.matrix(unlist(relyFit$Bayes$samp[[j]])), row.names =  NULL)
  names(dat) <- "value"
  dat$colos <- "1"
  dat$var <- "original"
  
  dat_del <- t(as.matrix(as.data.frame(relyFit$Bayes$ifitem$samp[[j]])))
  names <- (variables)

  for (i in n_row:1){
    tmp <- as.data.frame(dat_del[i, ])
    colnames(tmp) <- "value"
    tmp$var <- names[i]
    tmp$colos <- "2"
    dat <- rbind(dat, tmp)
  }
  dat$var <- factor(dat$var, levels = unique(dat$var))
  
  if (!is.null(ordering)) {
    if (ordering == "orderItemMean") {
      est <- as.data.frame(relyFit$Bayes$ifitem$est[[j]])
      est[n_row + 1, ] <- 1
      colnames(est) <- "value"
      est$name <- c(names, "original")
      dists <- abs(relyFit$Bayes$est[[j]] - relyFit$Bayes$ifitem$est[[j]]) 
      dists[length(dists)+1] <- 0
      est <- est[order(dists, decreasing = F), ]
      dat$var <- factor(dat$var, levels = c(est$name))

    } else if (ordering == "orderItemKL") {
      est <- as.data.frame(relyFit$Bayes$ifitem$est[[j]])
      est[n_row + 1, ] <- 1
      colnames(est) <- "value"
      est$name <- c(names, "original")
      samps <- relyFit$Bayes$ifitem$samp[[j]]
      og_samp <- relyFit$Bayes$samp[[j]]
      dists <- apply(samps, 2, .KLD.statistic, y = og_samp) # kl divergence
      dists[length(dists)+1] <- 0
      est <- est[order(dists), ]
      dat$var <- factor(dat$var, levels = c(est$name))
      
    } else if (ordering == "orderItemKS") {
      est <- as.data.frame(relyFit$Bayes$ifitem$est[[j]])
      est[n_row + 1, ] <- 1
      colnames(est) <- "value"
      est$name <- c(names, "original")
      samps <- relyFit$Bayes$ifitem$samp[[j]]
      og_samp <- relyFit$Bayes$samp[[j]]
      dists <- apply(samps, 2, .ks.test.statistic, y = og_samp) # ks distance
      dists[length(dists)+1] <- 0
      est <- est[order(dists), ]
      dat$var <- factor(dat$var, levels = c(est$name))
    }
  }

  g <- ggplot2::ggplot(dat, ggplot2::aes(x = value, y = var, fill = colos)) +
    ggridges::stat_density_ridges(quantile_lines = T, quantiles = c(lower, 0.5, upper),
                                  alpha = .85, show.legend = F, scale = 1) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   strip.text = ggplot2::element_text(colour = "black")) +
    ggplot2::ylab(gettext("Item Dropped")) +
    ggplot2::xlab(nms) +
    ggplot2::scale_fill_grey() +
    ggplot2::scale_y_discrete(expand = ggplot2::expand_scale(mult = c(0.1, 0.25)))  

  
  
  return(JASPgraphs::themeJasp(g))
  
}

.BayesianReliabilityPosteriorPredictive <- function(jaspResults, model, options) {
  
  if (!options[["dispPPC"]] || !options[["mcDonaldScale"]])
    return()
  
  relyFit <- model[["relyFit"]]
  dataset <- model[["dataset"]]
  
  if (is.null(relyFit)) {
    g <- NULL
  } else {
    ll <- relyFit[["Bayes"]][["loadings"]]
    rr <- relyFit[["Bayes"]][["resid_var"]]
    cimpl <- ll %*% t(ll) + diag(rr)
    cobs <- cov(dataset, use = model[["options"]][["missings"]])
    k <- ncol(cobs)
    eframe <- data.frame(number = seq(1, k), eigen_value = eigen(cobs)$values)
    ee_impl <- matrix(0, 1e3, k)
    for (i in 1:1e3) {
      dtmp <- MASS::mvrnorm(nrow(dataset), rep(0, k), cimpl)
      ee_impl[i, ] <- eigen(cov(dtmp))$values
    }
    eframe$eigen_sim_low <- apply(ee_impl, 2, quantile, prob = .025)
    eframe$eigen_sim_up<- apply(ee_impl, 2, quantile, prob = .975)
    leg_pos <- (max(eframe$eigen_value) + min(eframe$eigen_value)) * .75
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, max(eframe$eigen_sim_up)))
    
    
    g <- ggplot2::ggplot(eframe, mapping = ggplot2::aes(x = number, y = eigen_value)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = eigen_sim_low, ymax = eigen_sim_up), fill = "grey80") +
      ggplot2::geom_line(ggplot2::aes(x = number, y = eigen_sim_low), colour = "grey", linetype = 2) +
      ggplot2::geom_line(ggplot2::aes(x = number, y = eigen_sim_up), colour = "grey", linetype = 2) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_y_continuous(name = gettext("Eigenvalue"), breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::xlab(gettext("Factors"))
    
    g <- JASPgraphs::themeJasp(g)
  }
  plot <- createJaspPlot(plot = g, title = "Posterior Predictive Check Omega", width = 400)
  plot$dependOn(options = c("variables", "reverseScaledItems", "noSamples", "noBurnin", "noChains", "noThin",
                            "credibleIntervalValueScale", "dispPPC", "mcDonaldScale", "missingValues",
                            "setSeed", "seedValue"))
  jaspResults[["OmegaPosteriorPredictive"]] <- plot
  plot$position <- 7
}

  
.BayesianReliabilityTracePlot <- function(jaspResults, model, options) {
  
  if (!options[["tracePlot"]])
    return()
  
  plotContainerTP <- jaspResults[["plotContainerTP"]]
  if (is.null(plotContainerTP)) {
    plotContainerTP <- createJaspContainer(gettext("Convergence Traceplot"))
    plotContainerTP$dependOn(options = c("variables", "tracePlot", "noSamples", "noBurnin", "noChains", "noThin",
                                         "missingValues", "reverseScaledItems", "setSeed", "seedValue"))
    jaspResults[["plotContainerTP"]] <- plotContainerTP
    
  } 
  
  derivedOptions <- model[["derivedOptions"]]
  order     <- derivedOptions[["order"]]
  order <- order[1:(length(order)-3)] # dont need avgCor, mean and sd 
  indices   <- which(derivedOptions[["selectedEstimatorsPlots"]])
  nmsLabs   <- derivedOptions[["namesEstimators"]][["plots"]]
  nmsObjs   <- derivedOptions[["namesEstimators"]][["tables"]]
  
  relyFit  <- model[["relyFit"]]
  
  xlim <- (options[["noSamples"]] - options[["noBurnin"]]) / options[["noThin"]]
  if (!is.null(relyFit)) {
    for (i in indices) {
      if (is.null(plotContainerTP[[nmsObjs[i]]])) {
        
        p <- .BayesianReliabilityMakeTracePlot(relyFit, i, nmsLabs[[i]])
        plotObjTP <- createJaspPlot(plot = p, title = nmsObjs[i], width = 400)
        plotObjTP$dependOn(options = names(indices[i]))
        plotObjTP$position <- i
        plotContainerTP[[nmsObjs[i]]] <- plotObjTP
        
      } 
    }
  } else if (length(indices) > 0) {
    for (i in indices) {
      plotObjTP <- createJaspPlot(title = nmsObjs[i])
      plotObjTP$dependOn(options = names(indices[i]))
      plotObjTP$position <- i
      plotContainerTP[[nmsObjs[i]]] <- plotObjTP
    }
  } else {
    plotContainerTP[["If Item Dropped Posterior Plots"]] <- createJaspPlot()
  }
  
  plotContainerTP$position <- 8
  
  return()
}


.BayesianReliabilityMakeTracePlot <- function(relyFit, i, nms) {
  
  dd <- relyFit$Bayes$chains[[i]]
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, length(dd[1, ])+50))

  dv <- cbind(dd[1, ], 1, seq(1, ncol(dd))) 
  for (j in 2:nrow(dd)) {
    dv <- rbind(dv, cbind(dd[j, ], j, seq(1, ncol(dd))))
  }
  dat <- data.frame(dv)
  colnames(dat) <- c("Value", "chain", "Iterations")
  dat$chain <- as.factor(dat$chain)
  
  g <- ggplot2::ggplot(dat, ggplot2::aes(x = Iterations, y = Value, colour = chain)) +
    ggplot2::geom_line(size = .3) +
    ggplot2::ylab(nms) +
    ggplot2::scale_x_continuous(name = gettext("Iterations"), breaks = xBreaks)


  return(JASPgraphs::themeJasp(g))
  
}

.numformat <- function(val) { sub("^(-?)0.", "\\1.", gettextf("%.2f", val)) }



# ----- some other functions -----------------
.reliabilityItemRestCor <- function(dataset, n.iter, n.burnin, thin, n.chains, missing, callback) {

  help_dat <- array(0, c(ncol(dataset), nrow(dataset), 2))
  
  for (i in 1:ncol(dataset)) {
    help_dat[i, , ] <- cbind(dataset[, i], rowMeans(dataset[, -i], na.rm = T))
  }
  
  ircor_samp <- apply(help_dat, 1, .WishartCorTransform, n.iter = n.iter, n.burnin = n.burnin, thin = thin,
                      n.chains = n.chains, missing = missing, callback = callback)
  return(ircor_samp)
}



.WishartCorTransform <- function(x, n.iter, n.burnin, thin, n.chains, missing, callback) {
  pairwise <- FALSE
  if (missing == "pairwise") {pairwise <- TRUE}
  tmp_cov <- Bayesrel:::covSamp(x, n.iter, n.burnin, thin, n.chains, pairwise, callback)$cov_mat
  tmp_cor <- apply(tmp_cov, c(1, 2), cov2cor)
  out <- apply(tmp_cor, c(2, 3), function(x) mean(x[x!=1]))
  callback()
  return(out)
}


# calculate the kolomogorov smirnov distances between some samples and the original sample
.ks.test.statistic <- function(x, y) {
  t <- stats::ks.test(x, y)
  t$statistic
}

# calculate the kublack leibler distance between two samples
.KLD.statistic <- function(x, y) {
  t <- LaplacesDemon::KLD(x, y)
  t$sum.KLD.py.px
}

.chainSmoker <- function(A) {
  d <- dim(A)
  if (length(d) == 2) {
    Av <- as.vector(A)
  } else {
    Av <- apply(A, seq(3, length(d), 1), as.vector)
  }
  return(coda::mcmc(Av))
}

.cov2cor.callback <- function(C, callback) {
  callback()
  return(cov2cor(C))
}

.reverseScoreItems <- function(dataset, options) {
  dataset <- as.matrix(dataset) # fails for string factors!
  cols <- match(unlist(options[["reverseScaledItems"]]), .unv(colnames(dataset)))
  total <- apply(as.matrix(dataset[, cols]), 2, min) + apply(as.matrix(dataset[, cols]), 2, max)
  dataset[ ,cols] <- matrix(rep(total, nrow(dataset)), nrow(dataset), length(cols), byrow=T) - dataset[ ,cols]
  return(dataset)
}

.footnoteNegativeCorrelation <- function(variables, idx) {
  footnote <- sprintf(ngettext(length(variables[idx]),
                               "The following item correlated negatively with the scale: %s. ",
                               "The following items correlated negatively with the scale: %s. "),
                      paste(variables[idx], collapse = ", "))
}
  
