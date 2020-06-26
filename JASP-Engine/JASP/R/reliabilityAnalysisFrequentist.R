reliabilityFrequentist <- function(jaspResults, dataset, options) {


  
  dataset <- .reliabilityReadData(dataset, options)
  .reliabilityCheckErrors(dataset, options)
  
  model <- .frequentistReliabilityMainResults(jaspResults, dataset, options)
  
  .frequentistReliabilityScaleTable(         jaspResults, model, options)
  .frequentistReliabilityItemTable(          jaspResults, model, options)
  .freqentistReliabilitySingleFactorFitTable(jaspResults, model, options)
  return()
  
}

# read data, check errors----
.frequentistReliabilityDerivedOptions <- function(options) {
  
  # order of appearance in Bayesrel
  derivedOptions <- list(
    selectedEstimatorsF  = unlist(options[c("mcDonaldScale","alphaScale", "guttman2Scale", "guttman6Scale", 
                                            "glbScale", "averageInterItemCor", "meanScale", "sdScale")]),
    itemDroppedSelectedF = unlist(options[c("mcDonaldItem", "alphaItem", "guttman2Item", "guttman6Item",
                                            "glbItem", "itemRestCor", "meanItem", "sdItem")]),
    namesEstimators     = list(
      tables = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                 "Greatest Lower Bound", "Average interitem correlation", "mean", "sd"),
      tables_item = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                      "Greatest Lower Bound", "Item-rest correlation", "mean", "sd"),
      coefficients = c("McDonald's \u03C9", "Cronbach's \u03B1", "Guttman's \u03BB2", "Guttman's \u03BB6", 
                       "Greatest Lower Bound"),
      plots = list(expression("McDonald's"~omega), expression("Cronbach\'s"~alpha), expression("Guttman's"~lambda[2]), 
                   expression("Guttman's"~lambda[6]), "Greatest Lower Bound")
    )
  )

  # order to show in JASP
  derivedOptions[["order"]] <- c(5, 1, 2, 3, 4, 6, 7, 8)
  
  return(derivedOptions)
}



# estimate reliability ----
# maybe in the future it would be easier to have one function for every estimator...
.frequentistReliabilityMainResults <- function(jaspResults, dataset, options) {
  if (!options[["mcDonaldScale"]] && !options[["alphaScale"]] && !options[["guttman2Scale"]]
      && !options[["guttman6Scale"]] && !options[["glbScale"]] && !options[["averageInterItemCor"]]
      && !options[["meanScale"]] && !options[["sdScale"]] 
      && !options[["itemRestCor"]] && !options[["meanItem"]] && !options[["sdItem"]]) {
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
    
    samples <- options[["noSamples"]]
    p <- ncol(dataset)
    
    if (length(variables) > 2L) {
      
      if (length(options[["reverseScaledItems"]]) > 0L) {
        dataset <- .reverseScoreItems(dataset, options)
      }

      # observations for alpha interval need to be speccified: 
      model[["obs"]] <- nrow(dataset)
      
      model[["footnote"]] <- .reliabilityCheckLoadings(dataset, variables)
      if (any(is.na(dataset))) {
        if (options[["missingValues"]] == "excludeCasesPairwise") {
          missing <- "pairwise"
          use.cases <- "pairwise.complete.obs"
          model[["footnote"]] <- gettextf("%s Of the observations, pairwise complete cases were used. ", model[["footnote"]])
        } else {
          pos <- which(is.na(dataset), arr.ind = T)[, 1]
          dataset <- dataset[-pos, ] 
          use.cases <- "complete.obs"
          model[["footnote"]] <- gettextf("%s Of the observations, %1.f complete cases were used. ", model[["footnote"]], nrow(dataset))
        }
      } else {
        use.cases <- "everything"
      }

      if (options[["alphaInterval"]] == "alphaAnalytic") {
        alphaAna <- TRUE
        alphaSteps <- 0
      } else {
        alphaAna <- FALSE
        alphaSteps <- samples
        if (options[["alphaMethod"]] == "alphaStand") {
          alphaSteps <- alphaSteps + samples
        }
      }
      
      if (options[["omegaEst"]] == "pfa") {
        omegaSteps <- samples
        omegaAna <- FALSE
      } else {
        if (options[["omegaInterval"]] == "omegaAnalytic") {
          omegaAna <- TRUE
          omegaSteps <- 0
        } else {
          omegaAna <- FALSE
          omegaSteps <- 0
          # omegaSteps <- samples  # not working because we cannot tick in the bootstrapLavaan function
        }
      }
      
      if (options[["bootType"]] == "bootNonpara") {
        para <- FALSE
      } else {
        para <- TRUE
      }
      
      if (options[["intervalOn"]]) { # with confidence interval: 
        
        startProgressbar(samples * 5 # cov_mat bootstrapping and coefficients (also avg_cor) without alpha and omega
                         + alphaSteps
                         + omegaSteps) # dont need ifitem steps since that is very fast
        if (options[["setSeed"]]) {
          set.seed(options[["seedValue"]])
        }

        if (options[["alphaMethod"]] == "alphaStand") {
          model[["dat_cov"]] <- Bayesrel:::make_symmetric(cov2cor(cov(dataset, use = use.cases)))
          relyFit <- try(Bayesrel::strel(data = dataset, estimates=c("lambda2", "lambda6", "glb", "omega"), 
                                         Bayes = FALSE, n.boot = options[["noSamples"]],
                                         item.dropped = TRUE, omega.freq.method = options[["omegaEst"]], 
                                         omega.int.analytic = omegaAna,
                                         para.boot = para,
                                         missing = missing, callback = progressbarTick))
          
          relyFit$freq$est$freq_alpha <- Bayesrel:::applyalpha(model[["dat_cov"]])
          p <- ncol(dataset)
          Ctmp <- array(0, c(p, p - 1, p - 1))
          for (i in 1:p){
            Ctmp[i, , ] <- model[["dat_cov"]][-i, -i]
          }
          relyFit$freq$ifitem$alpha <- apply(Ctmp, 1, Bayesrel:::applyalpha)
          
          if (!alphaAna) { # when standardized alpha, but bootstrapped alpha interval:
            cors <- array(0, c(options[["noSamples"]], p, p))
            for (i in 1:options[["noSamples"]]) {
              cors[i, , ] <- .cov2cor.callback(relyFit$freq$covsamp[i, , ], progressbarTick)
            }
            relyFit$freq$boot$alpha <- apply(cors, 1, Bayesrel:::applyalpha)
            if (omegaAna && is.null(relyFit[["freq"]][["omega.error"]])) {
              relyFit[["freq"]][["boot"]] <- relyFit[["freq"]][["boot"]][c(4, 1, 2, 3)]
            } else {
              relyFit[["freq"]][["boot"]] <- relyFit[["freq"]][["boot"]][c(5, 1, 2, 3, 4)]
            }
          }
          
          relyFit[["freq"]][["est"]] <- relyFit[["freq"]][["est"]][c(5, 1, 2, 3, 4)]
          relyFit[["freq"]][["ifitem"]] <- relyFit[["freq"]][["ifitem"]][c(5, 1, 2, 3, 4)]
          
        } else { # alpha unstandardized
          model[["dat_cov"]] <- Bayesrel:::make_symmetric(cov(dataset, use = use.cases))
          relyFit <- try(Bayesrel::strel(data = dataset, estimates=c("alpha", "lambda2", "lambda6", "glb", "omega"), 
                                         Bayes = FALSE, n.boot = options[["noSamples"]],
                                         item.dropped = TRUE, omega.freq.method = options[["omegaEst"]], 
                                         alpha.int.analytic = alphaAna, 
                                         omega.int.analytic = omegaAna,
                                         para.boot = para,
                                         missing = missing, callback = progressbarTick))
        }

        # first the scale statistics
        cordat <- cor(dataset, use = use.cases)
        relyFit$freq$est$avg_cor <- mean(cordat[lower.tri(cordat)])
        relyFit$freq$est$mean <- mean(rowMeans(dataset, na.rm = T))
        relyFit$freq$est$sd <- sd(colMeans(dataset, na.rm = T))
        
        corsamp <- apply(relyFit$freq$covsamp, c(1), .cov2cor.callback, progressbarTick)
        relyFit$freq$boot$avg_cor <- apply(corsamp, 2, function(x) mean(x[x!=1]))
        relyFit$freq$boot$mean <- c(NA_real_, NA_real_)
        relyFit$freq$boot$sd <- c(NA_real_, NA_real_)
        
        
        # now the item statistics
        relyFit$freq$ifitem$ircor <- NULL
        
        for (i in 1:ncol(dataset)) {
          relyFit$freq$ifitem$ircor[i] <- cor(dataset[, i], rowMeans(dataset[, -i], na.rm = T), use = use.cases)
        }
        relyFit$freq$ifitem$mean <- colMeans(dataset, na.rm = T)
        relyFit$freq$ifitem$sd <- apply(dataset, 2, sd, na.rm = T)  
        
        ops <- .BayesianReliabilityDerivedOptions(options)
        order <- ops[["order"]]
        relyFit[["freq"]][["est"]] <- relyFit[["freq"]][["est"]][order]
        relyFit[["freq"]][["ifitem"]] <- relyFit[["freq"]][["ifitem"]][order]
        
        # ------------------------ only point estimates, no intervals: ---------------------------
      } else { 
        relyFit <- list()
        cv <- Bayesrel:::make_symmetric(cov(dataset, use = use.cases))
        cordat <- cor(dataset, use = use.cases)
        p <- ncol(dataset)
        Cvtmp <- array(0, c(p, p - 1, p - 1))
        Dtmp <- array(0, c(p, nrow(dataset), p - 1))
        for (i in 1:p){
          Cvtmp[i, , ] <- cv[-i, -i]
          Dtmp[i, , ] <- as.matrix(dataset[, -i])
        }
        
        if (options[["omegaEst"]] == "pfa") {
          omega.est <- Bayesrel:::applyomega_pfa(cv)
          omega.item <- apply(Cvtmp, 1, Bayesrel:::applyomega_pfa)
        } else {
          if (use.cases == "pairwise.complete.obs") {
            omega <- Bayesrel:::omegaFreqData(dataset, interval=.95, omega.int.analytic = T, pairwise = T)
            omega.est <- omega$omega
            relyFit$freq$omega.fit <- omega$indices
            omega.item <- apply(Dtmp, 1, Bayesrel:::applyomega_cfa_data, interval = .95, pairwise = T)
            if (any(is.na(omega))) {
              omega.est <- Bayesrel:::applyomega_pfa(cv)
              relyFit$freq$omega.error <- TRUE
              omega.item <- apply(Cvtmp, 1, Bayesrel:::applyomega_pfa)
            }
          } else {
            omega <- Bayesrel:::omegaFreqData(dataset, interval=.95, omega.int.analytic = T, pairwise = F)
            omega.est <- omega$omega
            relyFit$freq$omega.fit <- omega$indices
            omega.item <- apply(Dtmp, 1, Bayesrel:::applyomega_cfa_data, interval = .95, pairwise = F)
            if (any(is.na(omega))) {
              omega.est <- Bayesrel:::applyomega_pfa(cv)
              relyFit$freq$omega.error <- TRUE
              omega.item <- apply(Cvtmp, 1, Bayesrel:::applyomega_pfa)
            }
            if (any(is.na(omega.item))) {
              relyFit$freq$omega.item.error <- TRUE
              omega.item <- apply(Cvtmp, 1, Bayesrel:::applyomega_pfa)
            }
          }
        }
        if (options[["alphaMethod"]] == "alphaStand") {
          ca <- Bayesrel:::make_symmetric(cov2cor(cv))
          alpha <- Bayesrel:::applyalpha(ca)
          Crtmp <- array(0, c(p, p - 1, p - 1))
          for (i in 1:p){
            Crtmp[i, , ] <- ca[-i, -i]
          }
          alpha.item <- apply(Crtmp, 1, Bayesrel:::applyalpha)
        } else {
          alpha <- Bayesrel:::applyalpha(cv)
          alpha.item <- apply(Cvtmp, 1, Bayesrel:::applyalpha)
          
        }
        relyFit$freq$est <- list(freq_omega = omega.est, freq_alpha = alpha, 
                                 freq_lambda2 = Bayesrel:::applylambda2(cv), freq_lambda6 = Bayesrel:::applylambda6(cv),
                                 freq_glb = Bayesrel:::glbOnArray(cv), avg_cor = mean(cordat[lower.tri(cordat)]), 
                                 mean = mean(rowMeans(dataset, na.rm = T)), sd = sd(colMeans(dataset, na.rm = T)))

        relyFit$freq$ifitem <- list(omega = omega.item, alpha = alpha.item, 
                                          lambda2 = apply(Cvtmp, 1, Bayesrel:::applylambda2), 
                                          lambda6 = apply(Cvtmp, 1, Bayesrel:::applylambda6), 
                                          glb = apply(Cvtmp, 1, Bayesrel:::glbOnArray))
        relyFit$freq$ifitem$ircor <- NULL
        for (i in 1:ncol(dataset)) {
          relyFit$freq$ifitem$ircor[i] <- cor(dataset[, i], rowMeans(dataset[, -i], na.rm = T), use = use.cases)
        }
        relyFit$freq$ifitem$mean <- colMeans(dataset, na.rm = T)
        relyFit$freq$ifitem$sd <- apply(dataset, 2, sd, na.rm = T)  
      }

      
      
      # Consider stripping some of the contents of relyFit to reduce memory load
      if (inherits(relyFit, "try-error")) {
        
        model[["error"]] <- paste(gettext("The analysis crashed with the following error message:\n", relyFit))
        
      } else {
        
        model[["dataset"]] <- dataset
        
        model[["relyFit"]] <- relyFit
        
        stateObj <- createJaspState(model)
        stateObj$dependOn(options = c("variables", "reverseScaledItems", "noSamples", "missingValues", "omegaEst", 
                                        "alphaMethod", "alphaInterval", "omegaInterval", "bootType", 
                                        "setSeed", "seedValue", "intervalOn"))

        jaspResults[["modelObj"]] <- stateObj
        
      }
    }
  } 
  
  if (is.null(model[["error"]])) {
    if (options[["intervalOn"]]) {
      cfiState <- jaspResults[["cfiObj"]]$object
      if (is.null(cfiState) && !is.null(relyFit)) {
        
        scaleCfi <- .frequentistReliabilityCalcCfi(relyFit[["freq"]][["boot"]],             
                                                   options[["confidenceIntervalValue"]])
        
        # alpha int is analytical, not from the boot sample, so:
        if (options[["alphaInterval"]] == "alphaAnalytic") {
          
          alphaCfi <- Bayesrel:::ciAlpha(1 - options[["confidenceIntervalValue"]], model[["obs"]], model[["dat_cov"]])
          names(alphaCfi) <- c("lower", "upper")
          scaleCfi$alpha <- alphaCfi
        }
        
        
        # omega cfa analytic interval:
        if (is.null(relyFit[["freq"]][["omega.pfa"]]) && (options[["omegaInterval"]] == "omegaAnalytic")) { 
          fit <- relyFit[["freq"]][["fit.object"]]
          params <- lavaan::parameterestimates(fit, level = options[["confidenceIntervalValue"]])
          om_low <- params$ci.lower[params$lhs=="omega"]
          om_up <- params$ci.upper[params$lhs=="omega"]
          omegaCfi <- c(om_low, om_up)
          names(omegaCfi) <- c("lower", "upper")
          scaleCfi$omega <- omegaCfi
          if (options[["alphaInterval"]] == "alphaAnalytic") {
            scaleCfi <- scaleCfi[c(8, 7, 1, 2, 3, 4, 5, 6)] # check this when more estimators come in
          } else {
            scaleCfi <- scaleCfi[c(8, 1, 2, 3, 4, 5, 6, 7)] # check this when more estimators come in
          }
        } else {
          if (options[["alphaInterval"]] == "alphaAnalytic") {
            scaleCfi <- scaleCfi[c(4, 8, 1, 2, 3, 5, 6, 7)] # check this when more estimators come in
          } else {
            scaleCfi <- scaleCfi[c(5, 1, 2, 3, 4, 6, 7, 8)] # check this when more estimators come in
          }
        }
        
        cfiState <- list(scaleCfi = scaleCfi)
        jaspCfiState <- createJaspState(cfiState)
        jaspCfiState$dependOn(options = "confidenceIntervalValue", optionsFromObject = jaspResults[["modelObj"]])
        jaspResults[["cfiObj"]] <- jaspCfiState
        
      }
      model[["cfi"]] <- cfiState
      progressbarTick()
    }
  }
  
  model[["derivedOptions"]] <- .frequentistReliabilityDerivedOptions(options)
  model[["itemsDropped"]] <- .unv(colnames(dataset))
  
  # when variables are deleted again, a model footnote is expected, but none produce, hence: 
  if (is.null(model[["footnote"]])) model[["footnote"]] <- ""
  
  return(model)
}

.frequentistReliabilityCalcCfi <- function(boot, cfiValue) {
  
  cfi <- vector("list", length(boot))
  names(cfi) <- names(boot)
  
  for (nm in names(boot)) {
    if (any(is.na(boot[[nm]]))) {
      cfi[[nm]] <- c(NA_real_, NA_real_)
    } else {
      cfi[[nm]] <- quantile(boot[[nm]], prob = c((1-cfiValue)/2, 1-(1-cfiValue)/2))
    }
    names(cfi[[nm]]) <- c("lower", "upper")
  }
  return(cfi)
}




# tables ----


.frequentistReliabilityScaleTable <- function(jaspResults, model, options) {
  if (!is.null(jaspResults[["scaleTableF"]])) {
    return()
  }
  scaleTableF <- createJaspTable(gettext("Frequentist Scale Reliability Statistics"))
  scaleTableF$dependOn(options = c("variables", "mcDonaldScale", "alphaScale", "guttman2Scale", "guttman6Scale",
                                   "glbScale", "reverseScaledItems", "confidenceIntervalValue", "noSamples", 
                                   "averageInterItemCor", "meanScale", "sdScale", "missingValues", "omegaEst", 
                                   "alphaMethod", "alphaInterval", "omegaInterval", "bootType", 
                                   "setSeed", "seedValue", "intervalOn"))
  scaleTableF$addColumnInfo(name = "estimate", title = "Estimate", type = "string")
  

  
  if (options[["intervalOn"]]) {
    intervalLow <- gettextf("%s%% CI",
                            format(100*options[["confidenceIntervalValue"]], digits = 3, drop0trailing = TRUE))
    intervalUp <- gettextf("%s%% CI",
                           format(100*options[["confidenceIntervalValue"]], digits = 3, drop0trailing = TRUE))
    intervalLow <- gettextf("%s lower bound", intervalLow)
    intervalUp <- gettextf("%s upper bound", intervalUp)
    allData <- data.frame(estimate = c(gettext("Point estimate"), intervalLow, intervalUp))
  } else {
    allData <- data.frame(estimate = c(gettext("Point estimate")))
  }
  
# if no coefficients selected:
  if ((!options[["mcDonaldScale"]] && !options[["alphaScale"]] && !options[["guttman2Scale"]] 
       && !options[["guttman6Scale"]] && !options[["glbScale"]] && !options[["averageInterItemCor"]]
       && !options[["meanScale"]] && !options[["sdScale"]])) {
    
    scaleTableF$setData(allData)
    nvar <- length(options[["variables"]])
    if (nvar > 0L && nvar < 3L)
      scaleTableF$addFootnote(gettextf("Please enter at least 3 variables to do an analysis. %s", model[["footnote"]]))
    else
      scaleTableF$addFootnote(gettext(model[["footnote"]]))
    jaspResults[["scaleTableF"]] <- scaleTableF
    scaleTableF$position <- 1
    return()
  }


  relyFit <- model[["relyFit"]]
  derivedOptions <- model[["derivedOptions"]]
  opts     <- derivedOptions[["namesEstimators"]][["tables"]]
  order    <- derivedOptions[["order"]]
  selected <- derivedOptions[["selectedEstimatorsF"]]
  idxSelected <- which(selected)
  
  if (options[["mcDonaldScale"]] && !is.null(relyFit[["freq"]][["omega.error"]])) {
    model[["footnote"]] <- gettextf("%sMcDonald's \u03C9 estimation method switched to PFA because the CFA
                                          did not find a solution. ", model[["footnote"]])
  }

  if (!is.null(relyFit)) {
    if (options[["intervalOn"]]) {
      for (i in idxSelected) {
        scaleTableF$addColumnInfo(name = paste0("est", i), title = opts[i], type = "number")
        newData <- data.frame(est = c(unlist(relyFit$freq$est[[i]], use.names = F), 
                                      unlist(model[["cfi"]][["scaleCfi"]][[i]], use.names = F)))
        colnames(newData) <- paste0(colnames(newData), i)
        allData <- cbind(allData, newData)
      }
    } else {
      for (i in idxSelected) {
        scaleTableF$addColumnInfo(name = paste0("est", i), title = opts[i], type = "number")
        newData <- data.frame(est = c(unlist(relyFit$freq$est[[i]], use.names = F)))
        colnames(newData) <- paste0(colnames(newData), i)
        allData <- cbind(allData, newData)
      }
    }
    
    scaleTableF$setData(allData)
    
    if (!is.null(model[["footnote"]]))
      scaleTableF$addFootnote(gettext(model[["footnote"]]))
  } else if (sum(selected) > 0L) {
    
    for (i in idxSelected) {
      scaleTableF$addColumnInfo(name = paste0("est", i), title = opts[i], type = "number")
    }
    nvar <- length(options[["variables"]])
    if (nvar > 0L && nvar < 3L){
      scaleTableF$addFootnote(gettext("Please enter at least 3 variables to do an analysis."))
    }
    
  }
  if (!is.null(model[["error"]]))
    scaleTableF$setError(model[["error"]])
  
  if (!is.null(model[["footnote"]]))
    scaleTableF$addFootnote(gettext(model[["footnote"]]))
  
  jaspResults[["scaleTableF"]] <- scaleTableF
  scaleTableF$position <- 1
  
  return()
}


.frequentistReliabilityItemTable <- function(jaspResults, model, options) {

  if (!is.null(jaspResults[["itemTableF"]]) || !any(model[["derivedOptions"]][["itemDroppedSelectedF"]])) {
    return()
  }
  
  derivedOptions <- model[["derivedOptions"]]
  # fixes issue that unchecking the scale coefficient box, does not uncheck the item-dropped coefficient box:
  for (i in 1:5) { 
    if (!derivedOptions[["selectedEstimatorsF"]][i]) {
      derivedOptions[["itemDroppedSelectedF"]][i] <- derivedOptions[["selectedEstimatorsF"]][i]
    }
  }
  itemDroppedSelectedF <- derivedOptions[["itemDroppedSelectedF"]]
  # order <- derivedOptions[["order_item"]]
  estimators <- derivedOptions[["namesEstimators"]][["tables_item"]]
  overTitle <- gettext("If item dropped")
  
  itemTableF <- createJaspTable(gettext("Frequentist Individual Item Reliability Statistics"))
  itemTableF$dependOn(options = c("variables",
                                  "mcDonaldScale", "alphaScale", "guttman2Scale", "guttman6Scale", "glbScale", 
                                  "averageInterItemCor", "meanScale", "sdScale",
                                  "mcDonaldItem",  "alphaItem",  "guttman2Item", "guttman6Item", "glbItem",
                                  "reverseScaledItems", "meanItem", "sdItem", "itemRestCor", "missingValues", 
                                  "omegaEst", "alphaMethod", "setSeed", "seedValue"))
  itemTableF$addColumnInfo(name = "variable", title = "Item", type = "string")
  
  idxSelectedF <- which(itemDroppedSelectedF)
  coefficients <- derivedOptions[["namesEstimators"]][["coefficients"]]
  for (i in idxSelectedF) {
    if (estimators[i] %in% coefficients) {
      itemTableF$addColumnInfo(name = paste0("pointEst", i), title = estimators[i], type = "number", 
                               overtitle = overTitle)
    } else {
      itemTableF$addColumnInfo(name = paste0("pointEst", i), title = estimators[i], type = "number")
    }
  }

  relyFit <- model[["relyFit"]]

  if (!is.null(relyFit)) {
    if (options[["mcDonaldScale"]] && !is.null(relyFit[["freq"]][["omega.item.error"]])) {
      itemTableF$addFootnote(gettext("McDonald's \u03C9 estimation method for item-dropped statistics
                                    switched to PFA because the CFA did not find a solution."))
    }
    
    tb <- data.frame(variable = model[["itemsDropped"]])
    for (i in idxSelectedF) {
      newtb <- cbind(pointEst = relyFit$freq$ifitem[[i]])
      colnames(newtb) <- paste0(colnames(newtb), i)
      tb <- cbind(tb, newtb)
      
    }
    itemTableF$setData(tb)
    
    if (!is.null(unlist(options[["reverseScaledItems"]]))) {
      itemTableF$addFootnote(sprintf(ngettext(length(options[["reverseScaledItems"]]),
                                             "The following item was reverse scaled: %s. ",
                                             "The following items were reverse scaled: %s. "),
                                    paste(options[["reverseScaledItems"]], collapse = ", ")))
    }
    
  } else if (length(model[["itemsDropped"]]) > 0) {
    itemTableF[["variables"]] <- model[["itemsDropped"]]
    
    if (!is.null(unlist(options[["reverseScaledItems"]]))) {
      itemTableF$addFootnote(sprintf(ngettext(length(options[["reverseScaledItems"]]),
                                             "The following item was reverse scaled: %s. ",
                                             "The following items were reverse scaled: %s. "),
                                    paste(options[["reverseScaledItems"]], collapse = ", ")))
    }
  }
  
  jaspResults[["itemTableF"]] <- itemTableF
  itemTableF$position <- 2
  
  return()
}

# once the package is updated check this again and apply:
.freqentistReliabilitySingleFactorFitTable <- function(jaspResults, model, options) {

  if (!options[["fitMeasures"]] || !options[["mcDonaldScale"]])
    return()
  if (!is.null(jaspResults[["fitTable"]]) || !options[["fitMeasures"]]) {
    return()
  }

  fitTable <- createJaspTable(gettextf("Fit Measures of Single Factor Model Fit"))
  fitTable$dependOn(options = c("variables", "mcDonaldScale", "reverseScaledItems", "fitMeasures", "missingValues", 
                                "omegaEst", "setSeed", "seedValue"))
  fitTable$addColumnInfo(name = "measure", title = "Fit Measure",   type = "string")
  fitTable$addColumnInfo(name = "value",  title = "Value", type = "number")

  relyFit <- model[["relyFit"]]
  derivedOptions <- model[["derivedOptions"]]
  opts <- names(relyFit$freq$omega_fit)

  if (!is.null(relyFit)) {
    if (is.null(opts)) {
      allData <- data.frame(
        measure = NA_real_,
        value = NA_real_
      )
      if (!is.null(relyFit[["freq"]][["omega.error"]])) {
          fitTable$addFootnote(gettext("Fit measures cannot be displayed because the McDonald's \u03C9 
                                               estimation method switched 
                                               to PFA as the CFA did not find a solution."))
      }
    } else {
      opts <- c("Chi-Square", "df", "p.value", "RMSEA", "Lower CI RMSEA", "Upper CI RMSEA", "SRMR")
      allData <- data.frame(
        measure = opts,
        value = as.vector(unlist(relyFit$freq$omega_fit, use.names = FALSE))
      )
    }

    fitTable$setData(allData)
  }
  if (!is.null(model[["error"]]))
    fitTable$setError(model[["error"]])


  jaspResults[["fitTable"]] <- fitTable
  fitTable$position <- 3
  
}



# get bootstrapped sample for omega with cfa
.applyomega_cfa_cov <- function(cov, n){
  data <- MASS::mvrnorm(n, numeric(ncol(cov)), cov)
  out <- Bayesrel:::omegaFreqData(data, pairwise = F)
  om <- out$omega
  return(om)
}


