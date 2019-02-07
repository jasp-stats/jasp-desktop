#
# Copyright (C) 2018 University of Amsterdam
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

RegressionLinearBayesian <- function(jaspResults, dataset = NULL, options, ...) {
  
  jaspResults$title <- "Bayesian Linear Regression"
  
  status <- .basregSetStatus(options)
  
  dataset <- .basregReadData(dataset, options, status)
  
  .basregCheckErrors(dataset, options, status)
  
  basregResults <- .basregComputeModel(dataset, jaspResults, options, status)
  status <- basregResults$status
  basregModel <- basregResults$model
  
  .basregTableModelComparison(basregModel, jaspResults, options, status)
  .basregTableDescriptives(dataset, jaspResults, options, status)
  
  posteriorSummary <- .basregComputePosteriorSummary(basregModel, dataset, jaspResults, options, status)
  
  .basregTablePosteriorSummary(posteriorSummary, basregModel, jaspResults, options, status)
  .basregPlotPosteriorSummary(posteriorSummary, jaspResults, options, status)
  .basregplotsPosteriorDistribution(posteriorSummary, basregModel, jaspResults, options, status)
  
  .basregPlotResidualsVsFitted(basregModel, jaspResults, options, status)
  .basregPlotModelProbabilities(basregModel, jaspResults, options, status)
  .basregPlotModelComplexity(basregModel, jaspResults, options, status)
  .basregPlotInclusionProbabilities(basregModel, jaspResults, options, status)
  .basregPlotQQ(basregModel, jaspResults, options, status)
  
  # this function still needs to be rewritten to ggplot (start in .plotImage.basReg):
  .basregPlotPosteriorLogOdds(basregModel, jaspResults, options, status)
  
  return()
}

.basregSetStatus <- function(options) {
  status <- list(
    ready = TRUE,
    error = FALSE,
    errorMessage <- ""
  )
  
  if (length(options$modelTerms) == 0 || options$dependent == "") {
    status$ready <- FALSE
  }
  
  return(status)
}

.basregReadData <- function(dataset, options, status) {
  if (!status$ready) return()
  
  vars <- c(options$dependent, unlist(options$covariates))
  if (options$wlsWeights != "") {
    vars <- c(vars, options$wlsWeights)
  }
  
  dataset <- .readDataSetToEnd(columns = vars, exclude.na.listwise = vars)
  
  return(dataset)
}

.basregCheckErrors <- function(dataset, options, status) {
  if (!status$ready) return()
  
  customChecks <- list( #TODO: add these to hasErrors officially
    function() {
      nuisanceTerms <- sapply(options$modelTerms, function(term) term$isNuisance)
      if (sum(nuisanceTerms) == length(options$modelTerms)) {
        return("All effects are specified as nuisance")
      }
    },
    
    function() {
      maxModelComponents <- max(sapply(options$modelTerms, function(term) length(term$components)))
      if (maxModelComponents < 2) {
        return()
      }
      for (term in options$modelTerms) {
        if (length(term$components) < 2) {
          next
        }
        required <- 2^length(term$components) - 1
        included <- 0
        for (term2 in options$modelTerms) {
          if (all(term2$components %in% term$components)) {
            included <- included + 1
          }
        }
        if (sum(included) != required) {
          return("Main effects and lower-order interactions must be included whenever the corresponding higher-order interaction is included")
        }
      }
    },
    
    function() {
      for (term in options$modelTerms) {
        if (term$isNuisance == FALSE || length(term$components) < 2) {
          next
        }
        for (term2 in options$modelTerms) {
          if (term2$isNuisance == TRUE) {
            next
          }
          if (all(term2$components %in% term$components)) {
            return("Main effects and lower-order interactions must be specified as nuisance whenever the corresponding higher-order interaction is specified as nuisance")
          }
        }
      }
    },
    
    function() {
      if (options$wlsWeights != "") {
        weightsVar <- options$wlsWeights
        min.weight <- min(dataset[[ .v(weightsVar) ]])
        if (min.weight <= 0) {
          return("There are nonpositive weights")
        }
      }
    })
  
  .hasErrors(dataset = dataset, perform = perform,
             type=c("infinity", "observations", "variance"), custom = customChecks,
             infinity.target = c(options$covariates, options$dependent, options$wlsWeight),
             observations.target = options$dependent, observations.amount = paste("<", length(options$modelTerms) + 1),
             variance.target = c(options$covariates, options$dependent),
             exitAnalysisIfErrors = TRUE)
  
  return()
}

.basregComputeModel <- function(dataset, jaspResults, options, status) {
  if (!is.null(jaspResults[["stateModel"]]) && !is.null(jaspResults[["stateModel"]]$object))
    return(jaspResults[["stateModel"]]$object)
  
  jaspResults[["stateModel"]] <- createJaspState()
  jaspResults[["stateModel"]]$dependOnOptions(c(
    "dependent", "covariates", "wlsWeights", "modelTerms",
    "priorRegressionCoefficients", "alpha", "rScale",
    "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
    "samplingMethod", "iterationsMCMC", "numberOfModels")
  )
  
  if (!status$ready) 
    return(list(model = NULL, status = status))
  
  nPreds <- length(options$modelTerms)
  isNuisance <- rep(FALSE, nPreds)
  formula <- c(options$dependent, "~")
  for (i in 1:length(options$modelTerms)) {
    term <- options$modelTerms[[i]]
    termName <- paste(term$component, collapse=":")
    names(isNuisance)[i] <- termName
    sep <- ifelse(i == 1, "", "+")
    formula <- c(formula, sep, termName)
    if (term$isNuisance) {
      isNuisance[i] <- TRUE
    }
  }
  names(isNuisance) <- .vf(names(isNuisance))
  formula <- as.formula(.vf(paste(formula, collapse="")))
  hasInteraction <- any(attr(stats::terms.formula(formula), "order") > 1)
  
  # set initprobs (BAS' method for nuisance terms)
  initProbs <- rep(0.5, nPreds + 1) # the + 1 is the intercept
  index <- c(1, which(isNuisance) + 1)
  initProbs[index] <- 1
  
  # get the weights
  wlsWeights <- NULL
  if (options$wlsWeights != "") {
    weightsVar <- options$wlsWeights
    wlsWeights <- dataset[[ .v(weightsVar) ]]
  }
  
  # select the type of model prior
  footnoteInteraction <- NULL
  pInteraction <- 0.5 # probability of model inclusion conditional on inclusion parents
  if (options$modelPrior == "beta.binomial") {
    modelPrior <- BAS::beta.binomial(as.numeric(options$betaBinomialParamA), as.numeric(options$betaBinomialParamB))
    if (hasInteraction) {
      footnoteInteraction <- paste("Prior model probabilities for models with interaction effects",
                                   "are obtained from a Bernoulli (p = 0.5) prior.",
                                   "We advice using a different model prior, or excluding interaction effects.")
    }
  } else if (options$modelPrior == "uniform") {
    modelPrior <- BAS::uniform()
  } else if (options$modelPrior == "Bernoulli") {
    modelPrior <- BAS::Bernoulli(options$bernoulliParam)
    pInteraction <- options$bernoulliParam
  }
  
  # number of models
  n.models <- NULL
  if (options$samplingMethod == "BAS" && options$numberOfModels > 0) {
    n.models <- options$numberOfModels
  }
  
  # iterations for MCMC
  MCMC.iterations <- NULL
  if (options$samplingMethod == "MCMC" && options$iterationsMCMC > 0) {
    MCMC.iterations <- options$iterationsMCMC
  }
  
  # parameter for hyper-g's or jzs (all use same alpha param in bas.lm)
  alpha <- switch(
    options$priorRegressionCoefficients,
    hyper_g = options$alpha,
    hyper_g_laplace = options$alpha,
    hyper_g_n = options$alpha,
    JZS = options$rScale^2,
    NULL
  )
  
  # Bayesian Adaptive Sampling
  bas_lm <- try(BAS::bas.lm(
    formula = formula,
    data = dataset,
    prior = options$priorRegressionCoefficients,
    alpha = alpha,
    modelprior = modelPrior,
    n.models = n.models,
    method = options$samplingMethod,
    MCMC.iterations = MCMC.iterations,
    initprobs = initProbs,
    weights = wlsWeights,
    renormalize = TRUE
  ))
  
  if (isTryError(bas_lm)) {
    status$error <- TRUE
    status$errorMessage <- .extractErrorMessage(bas_lm)
  } else {
    if (bas_lm$n.models > 1 && nPreds > 1) # can crash without this check
      bas_lm <- BAS::force.heredity.bas(bas_lm)
    bas_lm[["interaction"]] <- list(
      hasInteraction = hasInteraction,
      footnote = footnoteInteraction,
      pInteraction = pInteraction
    )
    
    bas_lm[["nuisanceTerms"]] <- isNuisance
    names(bas_lm[["nuisanceTerms"]]) <- .unvf(names(bas_lm[["nuisanceTerms"]]))
    # fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
    bas_lm[["priorprobs"]] <- bas_lm[["priorprobs"]] / sum(bas_lm[["priorprobs"]])
    bas_lm[["priorprobsPredictor"]] <- .basregComputePriorMarginalInclusionProbs(bas_lm)
    bas_lm[["formula"]] <- formula
    bas_lm[["weights"]] <- wlsWeights
    bas_lm[["BFinclusion"]] <- .basregComputeInclusionBF(bas_lm)
    bas_lm[["namesx"]][-1] <- .unvf(bas_lm[["namesx"]][-1])
  }
  
  result <- list(model = bas_lm, status = status)
  
  jaspResults[["stateModel"]]$object <- result
  
  return(result)
}

.basregTableModelComparison <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["modelComparisonTable"]]))
    return()
  
  modelComparison <- createJaspTable(title = "Model Comparison")
  jaspResults[["modelComparisonTable"]] <- modelComparison
  
  modelComparison$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  modelComparison$dependOnOptions(c("bayesFactorType", "bayesFactorOrder", "shownModels", "numShownModels"))
  
  
  # modelComparison$addCitation <- list(
  #   "Clyde, M. A. (2017). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.4.7)[Computer software].", #FIXME: fix version
  #   "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101."
  # ) #TODO: add this again
  
  if (options$bayesFactorType == "BF10") {
    bf.title <- "BF<sub>10</sub>"
  } else if (options$bayesFactorType == "BF01") {
    bf.title <- "BF<sub>01</sub>"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(BF<sub>10</sub>)"
  }
  
  modelComparison$addColumnInfo(name = "Models", type = "string")
  modelComparison$addColumnInfo(name = "priorProbModel", type = "number", format = "sf:4;dp:3", title="P(M)")
  modelComparison$addColumnInfo(name = "postProbModel", type = "number", format = "sf:4;dp:3", title = "P(M|data)")
  modelComparison$addColumnInfo(name = "BFM", type = "number", format = "sf:4;dp:3", title = "BF<sub>M</sub>")
  modelComparison$addColumnInfo(name = "BF", type = "number", format = "sf:4;dp:3", title = paste(bf.title, sep = ""))
  modelComparison$addColumnInfo(name = "R2", type = "number", format = "dp:3", title = "R\u00B2")
  
  if (!status$ready) {
    modelComparison$setExpectedRows(1)
    return()
  } else if (status$error) {
    modelComparison$error <- "badData"
    modelComparison$errorMessage <- status$errorMessage
    return()
  }
  
  nRows <- NULL
  if (options$shownModels == "limited") {
    nRows <- options$numShownModels
  }
  
  if (is.null(nRows) || nRows > length(basregModel$which)) {
    nRows <- length(basregModel$which)
  }
  
  modelComparison$setExpectedRows(nRows)
  
  footnote <- basregModel[["interaction"]][["footnote"]]
  if (!is.null(footnote)) {
    modelComparison$addFootnote(message = footnote, symbol = "<em>Warning.</em>")
  }
  
  if (sum(basregModel$nuisanceTerms) > 0) {
    footnote <- paste("All models include ", paste(names(which(basregModel$nuisanceTerms)), collapse = ", "), ".", sep = "")
    modelComparison$addFootnote(message = footnote, symbol = "<em>Note.</em>")
  }
  
  .basregFillTableModelComparison(basregModel, nRows, jaspResults, options, status)
  
  return()
}

.basregFillTableModelComparison <- function(basregModel, nRows, jaspResults, options, status) {
  
  # ordered indices based on posterior probabilities of the models
  models.ordered <- order(basregModel$postprobs, decreasing = TRUE)
  if (options$bayesFactorOrder == "nullModelTop") {
    bestModelIndices <- models.ordered[1:nRows]
    if (1 %in% bestModelIndices) { # null model has index 1
      index <- which(bestModelIndices == 1)
      models.ordered <- c(1, bestModelIndices[-index]) # change position of null model
    } else {
      nRows <- nRows + 1 # show null model + best n
      models.ordered <- c(1, bestModelIndices)
    }
  } else { # best model top
    models.ordered <- models.ordered[1:nRows]
  }
  models <- basregModel$which[models.ordered]
  
  # null model name
  nuisanceTerms <- basregModel$nuisanceTerms
  null.model <- "Null model"
  if (sum(nuisanceTerms) > 0) {
    null.model <- paste("Null model (incl. ", paste(names(which(nuisanceTerms)), collapse = ", "), ")", sep = "")
  }
  
  # generate all model names
  allModelsVisited <- any(lengths(models) == 0) # analysis will chrash if TRUE
  model.names <- vector("character", length(models))
  
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (length(model) == 1) { # only has intercept
      model.names[i] <- null.model
      next
    }
    model <- model[-1]  # pop the intercept term (not in the nuisance vector)
    nuisanceInModel <- sum(nuisanceTerms[model])
    if (nuisanceInModel == length(model)) { # found the null model
      model.names[i] <- null.model
    } else {
      nonNuisance <- which(!nuisanceTerms[model])
      model.names[i] <- paste(names(nonNuisance), collapse = " + ")
    }
  }
  
  # get the Bayes factors for the models
  if (options$bayesFactorType == "BF10") {
    models.bf <- exp(basregModel$logmarg[models.ordered] -
                       basregModel$logmarg[models.ordered][1])
  } else if (options$bayesFactorType == "BF01") {
    models.bf <- exp(basregModel$logmarg[models.ordered][1] -
                       basregModel$logmarg[models.ordered])
  } else { # logBF10
    models.bf <- basregModel$logmarg[models.ordered] -
      basregModel$logmarg[models.ordered][1]
  }
  
  # calculate the BFM for the models
  nModels <- basregModel$n.models
  postProbs <- basregModel$postprobs
  priorProbs <- basregModel$priorprobs
  index <- is.finite(postProbs)
  BFM <- numeric(length(postProbs))
  BFM[index] <- (postProbs[index] / (1 - postProbs[index])) / (priorProbs[index] / (1 - priorProbs[index]))
  BFM[!index] <- NA
  BFM <- BFM[models.ordered]
  
  for (i in 1:nRows) {
    jaspResults[["modelComparisonTable"]]$addRows(list(
      Models = .clean(model.names[i]),
      BF = .clean(models.bf[i]),
      BFM = .clean(BFM[i]),
      postProbModel = .clean(basregModel$postprobs[[models.ordered[i]]]),
      R2 = .clean(basregModel$R2[[models.ordered[i]]]),
      priorProbModel = .clean(basregModel$priorprobs[[models.ordered[i]]])
    ))
  }
  
  return()
}

.basregTableDescriptives <- function(dataset, jaspResults, options, status) {
  if (!is.null(jaspResults[["descriptivesTable"]]) ||
      !options$descriptives)
    return()
  
  descriptives <- createJaspTable(title = "Descriptives")
  jaspResults[["descriptivesTable"]] <- descriptives
  
  descriptives$dependOnOptions(c("dependent", "covariates", "descriptives"))
  
  descriptives$addColumnInfo(name="v", title="", type="string")
  descriptives$addColumnInfo(name="N", title="N", type="integer")
  descriptives$addColumnInfo(name="mean", title="Mean", type="number", format="sf:4;dp:3")
  descriptives$addColumnInfo(name="sd", title="SD", type="number", format="sf:4;dp:3")
  
  if (!status$ready) {
    descriptives$setExpectedRows(1)
    return()
  }
  
  nRows <- length(c(unlist(options$covariates), options$dependent))
  descriptives$setExpectedRows(nRows)
  
  .basregFillTableDescriptives(dataset, jaspResults, options)
  
  return()
}

.basregFillTableDescriptives <- function(dataset, jaspResults, options) {
  
  variables <- c(options$dependent, unlist(options$covariates))
  for (variable in variables) {
    data <- na.omit(dataset[[ .v(variable) ]])
    n <- .clean(length(data))
    mean <- .clean(mean(data))
    sd <- .clean(sd(data))
    
    jaspResults[["descriptivesTable"]]$addRows(list(v = variable, N = n, mean = mean, sd = sd))
  }
  
  return()
}

.basregComputePosteriorSummary <- function(basregModel, dataset, jaspResults, options, status) {
  if (!is.null(jaspResults[["statePosteriorSummary"]]) && !is.null(jaspResults[["statePosteriorSummary"]]$object))
    return(jaspResults[["statePosteriorSummary"]]$object)
  
  jaspResults[["statePosteriorSummary"]] <- createJaspState()
  jaspResults[["statePosteriorSummary"]]$dependOnOptions(c(
    "summaryType", "posteriorSummaryPlotCredibleIntervalValue", "nSimForCRI",
    "dependent", "covariates", "wlsWeights", "modelTerms",
    "priorRegressionCoefficients", "alpha", "rScale",
    "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
    "samplingMethod", "iterationsMCMC", "numberOfModels"
  ))
  
  if (!status$ready || status$error ||
      (!options$postSummaryTable && !options$postSummaryPlot && !options$plotCoefficientsPosterior))
    return()
  
  estimator <- switch(
    options$summaryType,
    best="HPM",
    median="MPM",
    "BMA"
  )
  
  # required for the marginal posterior plots
  # done here such that the information in the plots and tables always matches
  # if a user selects the same options. (The method uses approximations and otherwise decimals are off)
  footnote <- NULL
  coefBMA <- .basregOverwritecoefBas(basregModel, estimator = "BMA", dataset = dataset, weights = basregModel[["weights"]])
  conf95BMA <- try(stats::confint(coefBMA, level = 0.95, nsim = options$nSimForCRI))
  if (isTryError(conf95BMA)) {
    conf95BMA <- cbind(NA, NA, coefBMA$postmean)
    rownames(conf95BMA) <- coefBMA$namesx
    colnames(conf95BMA) <- c("2.5%", "97.5%", "beta")
    conf95BMA[is.nan(conf95BMA)] <- NA
    footnote <- "Parameters estimates and/or credible intervals could not be calculated."
  }
  
  # check if results of table and plots should match
  criVal <- options[["posteriorSummaryPlotCredibleIntervalValue"]]
  if (estimator == "BMA" && isTRUE(all.equal(criVal, 0.95))) { # what we show under Marginal Posterior distributions
    coef <- coefBMA
    conf95 <- conf95BMA
  } else {
    coef <- .basregOverwritecoefBas(basregModel, estimator = estimator, dataset = dataset, weights = basregModel[["weights"]])
    conf95 <- stats::confint(coef, level = criVal, nsim = options$nSimForCRI)
  }
  
  probne0 <- coef[["probne0"]]
  coefficients <- basregModel[["namesx"]]
  if (estimator == "HPM") {
    loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
  } else if (estimator == "MPM") {
    loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
    probne0 <- basregModel[["probne0"]]
  } else {
    loopIdx <- seq_along(coefficients)
  }
  
  posteriorSummary <- list(coef = coef, loopIdx = loopIdx, coefficients = coefficients, probne0 = probne0,
                           conf95 = conf95, coefBMA = coefBMA, conf95BMA = conf95BMA, footnote = footnote)
  
  jaspResults[["statePosteriorSummary"]]$object <- posteriorSummary
  
  return(posteriorSummary)
}

.basregOverwritecoefBas <- function (basregModel, n.models, estimator = "BMA",
                                     dataset, weights = NULL) {
  
  # this function is an adaptation of BAS:::coef.bas
  # additional arguments:
  #
  # dataset
  # weights
  #
  # in addition, the formula object should be stored in the bas object.
  #
  # the original function evaluates things via eval(calls) constructions
  # JASP does not guarantree that this lookup structure works
  # so we need to modify this function.
  # this is only the case for the median model!
  
  # if there are future updates to the BAS package, this function can probably be removed
  # the code below is a small test for when an error happens.
  
  # data(UScrime, package = "MASS")
  # UScrime <- UScrime[, 1:5]
  # form <- M ~ So + Ed + Po1 + Po2
  # crime.bic =  BAS::bas.lm(
  #   formula = M ~ So + Ed + Po1 + Po2, # <-- toggle this one (works)
  #  # formula = form,                  # <-- and this one    (errors)
  #  data = UScrime,
  #  prior = "JZS",
  #  initprobs = c(1, 0.5, 0.5, 0.5, 0.5),
  #  renormalize = TRUE)
  # BAS:::coef.bas(crime.bic, estimator = "MPM") # <-- this function call will error
  
  # additionaly, the code previously failed (in JASP) for the correlation dataset (Big 5)
  # and selecting estimator = "MPM" (median model)
  # if neither of these errors occur in a future version then the original function can
  # probably be used again
  
  if (estimator == "MPM") {
    nvar = basregModel$n.vars - 1
    bestmodel <- (0:nvar)[basregModel$probne0 > 0.5]
    best = 1
    models <- rep(0, nvar + 1)
    models[bestmodel + 1] <- 1
    if (sum(models) > 1) {
      # this if statement is ugly but crucial
      if (is.null(weights)) {
        basregModel <- BAS::bas.lm(formula = basregModel$formula, data = dataset,
                                   weights = NULL,
                                   n.models = 1,
                                   alpha = basregModel$g, initprobs = basregModel$probne0,
                                   prior = basregModel$prior, modelprior = basregModel$modelprior,
                                   update = NULL, bestmodel = models, prob.local = 0)
        
      } else {
        basregModel <- BAS::bas.lm(formula = basregModel$formula, data = dataset,
                                   weights = weights,
                                   n.models = 1,
                                   alpha = basregModel$g, initprobs = basregModel$probne0,
                                   prior = basregModel$prior, modelprior = basregModel$modelprior,
                                   update = NULL, bestmodel = models, prob.local = 0)
      }
    }
  }
  postprobs = basregModel$postprobs
  if (estimator == "MPM" | estimator == "HPM")
    n.models = 1
  if (missing(n.models))
    n.models = length(postprobs)
  topm = order(-postprobs)[1:n.models]
  postprobs = postprobs[topm]/sum(postprobs[topm])
  shrinkage = basregModel$shrinkage[topm]
  conditionalmeans = BAS:::list2matrix.bas(basregModel, "mle")[topm,
                                                               , drop = F]
  conditionalmeans[, -1] = sweep(conditionalmeans[, -1, drop = F],
                                 1, shrinkage, FUN = "*")
  postmean = as.vector(postprobs %*% conditionalmeans)
  conditionalsd = BAS:::list2matrix.bas(basregModel, "mle.se")[topm,
                                                               , drop = F]
  if (!(basregModel$prior == "AIC" || basregModel$prior == "BIC")) {
    conditionalsd[, -1] = sweep(conditionalsd[, -1, drop = F],
                                1, sqrt(shrinkage), FUN = "*")
  }
  postsd = sqrt(postprobs %*% conditionalsd^2 + postprobs %*%
                  ((sweep(conditionalmeans, 2, postmean, FUN = "-"))^2))
  postsd = as.vector(postsd)
  if (is.null(basregModel$df[topm])) {
    df = rep(basregModel$n, length(postprobs))
    if (basregModel$prior == "BIC" | basregModel$prior == "AIC") {
      df = df - basregModel$size
    }
    else {
      df = df - 1
    }
  }
  else df = basregModel$df[topm]
  out = list(postmean = postmean, postsd = postsd, probne0 = basregModel$probne0,
             conditionalmeans = conditionalmeans, conditionalsd = conditionalsd,
             namesx = basregModel$namesx, postprobs = postprobs, n.vars = basregModel$n.vars,
             n.models = n.models, df = df, estimator = estimator)
  class(out) = "coef.bas"
  return(out)
}

.basregComputePriorMarginalInclusionProbs <- function(basregModel) {
  # Calculate the prior inclusions probabilities for each predictor
  #
  # Args:
  #   model: bas object (including nuisanceTerms entry)
  #
  # Return:
  #   vector of inclusion probabilities (including intercept)
  
  allModels <- basregModel$which
  modelProbs <- basregModel$priorprobs
  nPreds <- length(basregModel$probne0)
  
  # model prior has been modified, recalculate the prior inclusion probs
  nModels <- length(allModels)
  priorProbs <- numeric(nPreds)
  
  for (i in 1:nModels) {
    
    idx <- allModels[[i]] + 1 # +1 to change 0 for intercept into a 1 so it can be used as an index
    priorProbs[idx] = priorProbs[idx] + modelProbs[i]
    
  }
  
  return(priorProbs)
}

.basregComputeInclusionBF <- function(basregModel, posteriorSummary) {
  
  nModels <- basregModel[["n.models"]]
  nPred <- length(basregModel[["probne0"]])
  
  # should this work on a log scale??
  # first row is numerator of the odds; second row is denominator
  priorOdds <- matrix(0, 2, nPred)
  posteriorOdds <- matrix(0, 2, nPred)
  for (i in seq_len(nModels)) {
    
    idxN <- basregModel[["which"]][[i]] + 1
    idxD <- (1:nPred)[-idxN]
    
    # increment numerators
    priorOdds[1, idxN] <- priorOdds[1, idxN] + basregModel[["priorprobs"]][i]
    posteriorOdds[1, idxN] <- posteriorOdds[1, idxN] + basregModel[["postprobs"]][i]
    
    # increment denominators
    priorOdds[2, idxD] <- priorOdds[2, idxD] + basregModel[["priorprobs"]][i]
    posteriorOdds[2, idxD] <- posteriorOdds[2, idxD] + basregModel[["postprobs"]][i]
    
  }
  
  priOdds <- priorOdds[1, ] / priorOdds[2, ]
  posOdds <- posteriorOdds[1, ] / posteriorOdds[2, ]
  BFinclusion <- posOdds / priOdds
  
  # nuisance terms and intercept are always included
  BFinclusion[-1][basregModel[["nuisanceTerms"]]] <- 1 # nuisance terms
  BFinclusion[1] <- 1 # intercept
  return(BFinclusion)
  
}

.basregTablePosteriorSummary <- function(posteriorSummary, basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["posteriorSummaryTable"]]) ||
      !options$postSummaryTable)
    return()
  
  posteriorSummaryTable <- createJaspTable(title = "Posterior Summaries of Coefficients")
  jaspResults[["posteriorSummaryTable"]] <- posteriorSummaryTable
  
  posteriorSummaryTable$copyDependenciesFromJaspObject(jaspResults[["statePosteriorSummary"]])
  posteriorSummaryTable$dependOnOptions("postSummaryTable")
  
  overtitle <- sprintf("%s%% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
  posteriorSummaryTable$addColumnInfo(name="coefficient", title="Coefficient", type="string")
  posteriorSummaryTable$addColumnInfo(name="mean", title="Mean", type="number", format="sf:4;dp:3")
  posteriorSummaryTable$addColumnInfo(name="sd", title="SD", type="number", format="sf:4;dp:3")
  posteriorSummaryTable$addColumnInfo(name="pInclprior", title ="P(incl)", type="number", format="sf:4;dp:3")
  posteriorSummaryTable$addColumnInfo(name="pIncl", title ="P(incl|data)", type="number", format="sf:4;dp:3")
  posteriorSummaryTable$addColumnInfo(name="BFincl", title ="BF<sub>inclusion</sub>", type="number", format="sf:4;dp:3")
  posteriorSummaryTable$addColumnInfo(name="lowerCri", title = "Lower", type="number", format="sf:4;dp:3", overtitle = overtitle)
  posteriorSummaryTable$addColumnInfo(name="upperCri", title = "Upper", type="number", format="sf:4;dp:3", overtitle = overtitle)
  
  if (!status$ready) {
    posteriorSummaryTable$setExpectedRows(1)
    return()
  } else if (status$error) {
    posteriorSummaryTable$error <- "badData"
    return()
  }
  
  nRows <- length(unlist(options$covariates)) + 1 # 1 extra for intercept
  posteriorSummaryTable$setExpectedRows(nRows)
  
  footnote <- posteriorSummary[["footnotes"]]
  if (!is.null(footnote)) {
    posteriorSummaryTable$addFootnote(footnote, symbol="<em>Warning.</em>")
  }
  
  .basregFillTablePosteriorSummary(posteriorSummary, basregModel, jaspResults, options)
  
  return()
}

.basregFillTablePosteriorSummary <- function(posteriorSummary, basregModel, jaspResults, options) {
  
  BFinclusion <- basregModel[["BFinclusion"]]
  priorProbs <- basregModel[["priorprobsPredictor"]]
  nModels <- basregModel[["n.models"]]
  coef <- posteriorSummary[["coef"]]
  coefficients <- posteriorSummary[["coefficients"]]
  probne0 <- posteriorSummary[["probne0"]]
  loopIdx <- posteriorSummary[["loopIdx"]]
  confInt <- posteriorSummary[["conf95"]]
  
  topm <- order(-basregModel$postprobs)[1:nModels]
  mostComplex <- which.max(lengths(basregModel$which)[topm])
  
  for (i in loopIdx) {
    coefficient <- .clean(coefficients[i])
    pIncl <- .clean(probne0[i])
    pInclprior <- .clean(priorProbs[i])
    BFincl <- .clean(BFinclusion[i])
    
    if (options$summaryType == "complex") {
      mean <- .clean(unname(coef$conditionalmeans[mostComplex, i]))
      sd <- .clean(unname(coef$conditionalsd[mostComplex, i]))
    } else {
      mean <- .clean(coef$postmean[i])
      sd <- .clean(coef$postsd[i])
    }
    lowerCri <- .clean(confInt[i, 1])
    upperCri <- .clean(confInt[i, 2])
    
    jaspResults[["posteriorSummaryTable"]]$addRows(
      list(coefficient = coefficient, mean = mean, sd = sd, pIncl = pIncl,
           pInclprior = pInclprior, BFincl = BFincl, lowerCri = lowerCri, upperCri = upperCri
      )
    )
  }
  
  return()
}

.basregPlotPosteriorSummary <- function(posteriorSummary, jaspResults, options, status) {
  if (!is.null(jaspResults[["posteriorSummaryPlot"]]) ||
      !options$postSummaryPlot)
    return()
  
  title <- sprintf("Posterior Coefficients with %s%% Credible Interval", 
                   format(100*options$posteriorSummaryPlotCredibleIntervalValue, digits = 3))
  posteriorSummaryPlot <- createJaspPlot(title = title, width = 530, height = 400)
  jaspResults[["posteriorSummaryPlot"]] <- posteriorSummaryPlot
  
  posteriorSummaryPlot$copyDependenciesFromJaspObject(jaspResults[["statePosteriorSummary"]])
  posteriorSummaryPlot$dependOnOptions(c("postSummaryPlot", "omitIntercept"))
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    posteriorSummaryPlot$error <- "badData"
    return()
  }
  
  .basregFillPlotPosteriorSummary(posteriorSummary, jaspResults, options)
  
  return()
}

.basregFillPlotPosteriorSummary <- function(posteriorSummary, jaspResults, options) {
  
  coef <- posteriorSummary[["coef"]]
  confInt <- posteriorSummary[["conf95"]]
  loopIdx <- posteriorSummary[["loopIdx"]]
  coefficients <- posteriorSummary[["coefficients"]]
  
  # exlude intercept if it's not the only predictor?
  if (options[["omitIntercept"]] && length(loopIdx) > 1)
    loopIdx <- loopIdx[-1, drop = FALSE]
  
  confInt <- confInt[loopIdx, , drop = FALSE] # only plot parameters present in table
  df <- data.frame(
    x = factor(coefficients[loopIdx], levels = coefficients[loopIdx]),
    y = confInt[, 3],
    lower = confInt[, 1],
    upper = confInt[, 2]
  )
  
  p <- try({
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(c(confInt)), eps.correct = 2)
    g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = lower, ymax = upper)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_errorbar(, width = 0.2) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = expression(beta), breaks = yBreaks, limits = range(yBreaks))
    JASPgraphs::themeJasp(g) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(angle = 0, vjust = .5, size = 20)
      )
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["posteriorSummaryPlot"]]$error <- "badData" #FIXME: this should go
    jaspResults[["posteriorSummaryPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["posteriorSummaryPlot"]]$plotObject <- p
  }
  
  return()
}

.basregplotsPosteriorDistribution <- function(posteriorSummary, basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["posteriorDistributionPlots"]]) || # TODO: check if some variable plots can be re-used or not
      !options$plotCoefficientsPosterior) #TODO: rename this option
    return()

  posteriorDistributionPlots <- createJaspContainer("Distribution plots") #TODO: check if this name is ok
  jaspResults[["posteriorDistributionPlots"]] <- posteriorDistributionPlots
  
  posteriorDistributionPlots$copyDependenciesFromJaspObject(jaspResults[["statePosteriorSummary"]]) #TODO: check this: was probably wrong in release
  posteriorDistributionPlots$dependOnOptions("plotCoefficientsPosterior") 
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    posteriorDistributionPlots$error <- "badData"
    return()
  }
  
  plotNames <- basregModel$namesx
  isNuisance <- basregModel$nuisanceTerms
  for (plotName in plotNames) {
    
    if (plotName != "Intercept" && isNuisance[which(names(isNuisance) == plotName)])
      next
    
    posteriorDistributionPlots[[plotName]] <- createJaspPlot(title = plotName, width = 530, height = 400)
    index <- which(plotNames == plotName)
    .basregFillplotPosteriorDistribution(plotName, index, posteriorSummary, basregModel, jaspResults, options)
    
  }
  
  return()
}

.basregFillplotPosteriorDistribution <- function(name, index, posteriorSummary, basregModel, jaspResults, options) {
  
  # these first lines are there to create compatibility with the BAS plotting code we copied
  x <- posteriorSummary[["coefBMA"]]
  conf95 <- posteriorSummary[["conf95BMA"]]
  subset <- list(index)
  e <- 1e-04
  
  # based on BAS:::plot.coef.bas.
  p <- try(silent = FALSE, expr = {
    # start of copied code
    df = x$df
    i <- index
    
    sel = x$conditionalmeans[, i] != 0
    prob0 = 1 - x$probne0[i]
    mixprobs = x$postprobs[sel]/(1 - prob0)
    means = x$conditionalmeans[sel, i, drop = TRUE]
    sds = x$conditionalsd[sel, i, drop = TRUE]
    name = x$namesx[i]
    df.sel = df[sel]
    
    df <- df.sel # modified from original
    
    nsteps = 500
    if (prob0 == 1 | length(means) == 0) {
      xlower = -0
      xupper = 0
      xmax = 1
    } else {
      qmin = min(qnorm(e/2, means, sds))
      qmax = max(qnorm(1 - e/2, means, sds))
      if (i > 1) {
        xlower = min(qmin, 0)
        xupper = max(0, qmax)
      } else {
        xlower <- qmin
        xupper <- qmax
      }
    }
    
    xx = seq(xlower, xupper, length.out = nsteps)
    yy = rep(0, times = length(xx))
    maxyy = 1
    if (prob0 < 1 & length(sds) > 0) {
      yy = mixprobs %*% apply(matrix(xx, ncol = 1), 1,
                              FUN = function(x, d, m, s) {
                                dt(x = (x - m)/s, df = d)/s
                              }, d = df, m = means, s = sds)
      maxyy = max(yy)
    }
    ymax = max(prob0, 1 - prob0)
    # end of copied code
    
    dens <- (1 - prob0) * yy/maxyy
    dfLines <- data.frame(
      x = c(0, 0, xx),
      y = c(0, prob0, dens),
      g = factor(rep(1:2, c(2, length(xx))))
    )
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(xlower, xupper))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, 1.15*max(dfLines$y)))
    
    # figure out whether to draw text left or right of 0
    step <- (xupper + abs(xlower)) / (nsteps - 1)  # stepsize of grid
    idx0 <- round(abs(xlower / step))              # idx of x closest to 0
    idxMax <- which.max(dens)                      # idx of maximum of density
    maxX <- xx[idxMax]                             # x value at maximum of density
    maxHeight <- dens[idxMax]                        # y value at maximum of density
    if (prob0 > maxHeight) { # if text drawn above posterior no action is required
      
      xText <- 0.05 * xBreaks[length(xBreaks)]
      hjust = "left"
      # text below maxheight
      
    } else {
      
      # text is drawn right if:
      # - density is below textheight
      # - peak of density is left of textheight
      
      # text drawn at similar height as posterior
      if (maxX < 0 && dens[idx0] < prob0) {
        # peak is left of text; density is below text height
        xText <- 0.05 * xBreaks[length(xBreaks)]
        hjust = "left"
        
      } else {
        
        xText <- -abs(0.05 * xBreaks[1])
        hjust = "right"
        
      }
      
    }
    dfText <- data.frame(
      x = xText,
      y = prob0,
      label = format(prob0, digits = 3, scientific = -2)
    )
    
    # obtain credible interval given that predictor is in model
    cri <- conf95[i, 1:2]
    # find closest x-locations on grid to credible interval
    idxCri <- c(
      which.min(abs(xx - cri[1])),
      which.min(abs(xx - cri[2]))
    )
    dfCri <- data.frame(
      xmin = xx[idxCri[1]],
      xmax = xx[idxCri[2]],
      y = 0.9 * yBreaks[length(yBreaks)]
    )
    hBarHeight <- 0.05 * yBreaks[length(yBreaks)]
    dfCriText <- data.frame(
      x = xx[idxCri],
      y = 0.975 * yBreaks[length(yBreaks)],
      label = format(cri, digits = 3, scientific = -2)
    )
    
    g <- ggplot2::ggplot(data = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g, color = g)) +
      ggplot2::geom_line(size = 1.25, show.legend = FALSE) +
      ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = name, breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_color_manual(values = c("gray", "black"))
    if (prob0 > 0.01)
      g <- g + ggplot2::geom_text(data = dfText, mapping = ggplot2::aes(x = x, y = y, label = label),
                                  size = 6, hjust = hjust, inherit.aes = FALSE)
    g <- g + ggplot2::geom_errorbarh(data = dfCri, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
                                     height = hBarHeight, inherit.aes = FALSE) +
      ggplot2::geom_text(data = dfCriText, mapping = ggplot2::aes(x = x, y = y, label = label), size = 6,
                         hjust = c("right", "left"), inherit.aes = FALSE)
    
    JASPgraphs::themeJasp(g)
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["posteriorDistributionPlots"]][[name]]$error <- "badData" #FIXME: this should go
    jaspResults[["posteriorDistributionPlots"]][[name]]$errorMessage <- errorMessage
  } else {
    jaspResults[["posteriorDistributionPlots"]][[name]]$plotObject <- p
  }
  
  return()
}

.basregPlotResidualsVsFitted <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["ResidualsVsFittedPlot"]]) ||
      !options$plotResidualsVsFitted)
    return()
  
  ResidualsVsFittedPlot <- createJaspPlot(title = "Residuals vs Fitted", width = 530, height = 400)
  jaspResults[["ResidualsVsFittedPlot"]] <- ResidualsVsFittedPlot
  
  ResidualsVsFittedPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  ResidualsVsFittedPlot$dependOnOptions("plotResidualsVsFitted")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    ResidualsVsFittedPlot$error <- "badData"
    return()
  }

  .basregFillPlotResidualsVsFitted(basregModel, jaspResults, options)

  return()
}

.basregFillPlotResidualsVsFitted <- function(basregModel, jaspResults, options) {
  
  p <- try({
    
    x <- fitted(basregModel, estimator = "BMA")
    y <- basregModel$Y - x
    dfPoints <- data.frame(
      x = x,
      y = y
    )
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
    g <- JASPgraphs::drawAxis()
    g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
    g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85)
    g <- JASPgraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7) +
      ggplot2::ylab("Residuals") +
      ggplot2::scale_x_continuous(name = "Predictions under BMA", breaks = xBreaks, limits = range(xBreaks))
    JASPgraphs::themeJasp(g)
    
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["ResidualsVsFittedPlot"]]$error <- "badData" #FIXME: this should go
    jaspResults[["ResidualsVsFittedPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["ResidualsVsFittedPlot"]]$plotObject <- p
  }
  
  return()
}

.basregPlotModelProbabilities <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["modelProbabilitiesPlot"]]) ||
      !options$plotModelProbabilities)
    return()
  
  modelProbabilitiesPlot <- createJaspPlot(title = "Model Probabilities", width = 530, height = 400)
  jaspResults[["modelProbabilitiesPlot"]] <- modelProbabilitiesPlot
  
  modelProbabilitiesPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  modelProbabilitiesPlot$dependOnOptions("plotModelProbabilities")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    modelProbabilitiesPlot$error <- "badData"
    return()
  }
  
  .basregFillPlotModelProbabilities(basregModel, jaspResults, options)
  
  return()
}

.basregFillPlotModelProbabilities <- function(basregModel, jaspResults, options) {
  
  p <- try({
    
    cum.prob = cumsum(basregModel$postprobs)
    m.index = 1:basregModel$n.models
    
    dfPoints <- data.frame(
      x = m.index,
      y = cum.prob
    )
    
    xBreaks <- round(seq(1, basregModel$n.models, length.out = min(5, basregModel$n.models)))
    g <- JASPgraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
    g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = "Cumulative Probability", limits = 0:1) +
      ggplot2::scale_x_continuous(name = "Model Search Order", breaks = xBreaks)
    JASPgraphs::themeJasp(g)
    
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["modelProbabilitiesPlot"]]$error <- "badData" #FIXME: this should go
    jaspResults[["modelProbabilitiesPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["modelProbabilitiesPlot"]]$plotObject <- p
  }
  
  return()
}

.basregPlotModelComplexity <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["modelComplexityPlot"]]) ||
      !options$plotModelComplexity)
    return()
  
  modelComplexityPlot <- createJaspPlot(title = "Log(P(data|M)) vs. model size", width = 530, height = 400)
  jaspResults[["modelComplexityPlot"]] <- modelComplexityPlot
  
  modelComplexityPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  modelComplexityPlot$dependOnOptions("plotModelComplexity")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    modelComplexityPlot$error <- "badData"
    return()
  }
  
  .basregFillPlotModelComplexity(basregModel, jaspResults, options)
  
  return()
}

.basregFillPlotModelComplexity <- function(basregModel, jaspResults, options) {
  
  p <- try({
    
    logmarg = basregModel$logmarg
    dim = basregModel$size
    
    dfPoints <- data.frame(
      x = dim,
      y = logmarg
    )
    
    # gonna assume here that dim (the number of parameters) is always an integer
    xBreaks <- unique(round(pretty(dim)))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(logmarg), eps.correct = 2)
    g <- JASPgraphs::drawPoints(dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = "Log(P(data|M))", breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = "Model Dimension", breaks = xBreaks)
    JASPgraphs::themeJasp(g)
    
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["modelComplexityPlot"]]$error <- "badData" #FIXME: this should go
    jaspResults[["modelComplexityPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["modelComplexityPlot"]]$plotObject <- p
  }
  
  return()
}

.basregPlotInclusionProbabilities <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["inclusionProbabilitiesPlot"]]) ||
      !options$plotInclusionProbabilities)
    return()
  
  inclusionProbabilitiesPlot <- createJaspPlot(title = "Inclusion Probabilities", width = 700, height = 400)
  jaspResults[["inclusionProbabilitiesPlot"]] <- inclusionProbabilitiesPlot
  
  inclusionProbabilitiesPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  inclusionProbabilitiesPlot$dependOnOptions("plotInclusionProbabilities")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    inclusionProbabilitiesPlot$error <- "badData"
    return()
  }
  
  .basregFillPlotInclusionProbabilities(basregModel, jaspResults, options)
  
  return()
}

.basregFillPlotInclusionProbabilities <- function(basregModel, jaspResults, options) {
  
  p <- try({
    
    probne0 = basregModel$probne0[-1]
    variables = basregModel$namesx[-1] # 1:basregModel$n.vars
    priorProb <- basregModel$priorprobsPredictor[1:basregModel$n.vars][-1]
    
    # reorder from low to high
    o <- order(probne0, decreasing = FALSE)
    probne0 <- probne0[o]
    variables <- variables[o]
    priorProb <- priorProb[o]
    
    width <- .8 # width of the bars
    dfBar <- data.frame(
      x = factor(variables, levels = variables),
      y = probne0
    )
    dfLine <- data.frame(
      x = rep(1:(basregModel$n.vars-1), each = 2) + c(-width/2, width/2),
      y = rep(priorProb, each = 2),
      g = rep(factor(variables), each = 2),
      g0 = factor(1)
    )
    base <- .1
    yLimits <- c(0, base * ceiling(max(c(priorProb, probne0)) / base))
    yBreaks <- seq(yLimits[1], yLimits[2], length.out = 5)
    
    g <- JASPgraphs::drawBars(dat = dfBar, width = width)
    g <- JASPgraphs::drawLines(g, dat = dfLine,
                               mapping = ggplot2::aes(x = x, y = y, group = g, linetype = g0), show.legend = TRUE) +
      ggplot2::scale_y_continuous("Marginal Inclusion Probability", breaks = yBreaks, limits = yLimits) +
      ggplot2::xlab("") +
      ggplot2::scale_linetype_manual(name = "", values = 2, labels = "Prior\nInclusion\nProbabilities")
    
    JASPgraphs::themeJasp(g, horizontal = TRUE, legend.position = "right") +
      ggplot2::theme(
        legend.title = ggplot2::element_text(size = .8*JASPgraphs::graphOptions("fontsize"))
      )
    
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["inclusionProbabilitiesPlot"]]$error <- "badData" #FIXME: this should go
    jaspResults[["inclusionProbabilitiesPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["inclusionProbabilitiesPlot"]]$plotObject <- p
  }
  
  return()
}

.basregPlotQQ <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["qqPlot"]]) ||
      !options$plotQQplot)
    return()
  
  qqPlot <- createJaspPlot(title = "Q-Q Plot", width = 700, height = 400)
  jaspResults[["qqPlot"]] <- qqPlot
  
  qqPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]]) #TODO: check if qqplot really needs all these dependencies
  qqPlot$dependOnOptions("plotQQplot")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    qqPlot$error <- "badData"
    return()
  }

  .basregFillPlotQQ(basregModel, jaspResults, options)

  return()
}

.basregFillPlotQQ <- function(basregModel, jaspResults, options) {
  
  p <- try({
    x <- fitted(basregModel, estimator = "BMA")
    y <- basregModel$Y - x
    JASPgraphs::plotQQnorm(y)
  })
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    jaspResults[["qqPlot"]]$error <- "badData"
    jaspResults[["qqPlot"]]$errorMessage <- errorMessage
  } else {
    jaspResults[["qqPlot"]]$plotObject <- p
  }
  
  return()
}

.basregPlotPosteriorLogOdds <- function(basregModel, jaspResults, options, status) {
  if (!is.null(jaspResults[["logPosteriorOddsPlot"]]) ||
      !options$plotLogPosteriorOdds)
    return()
  
  postLogOddsPlot <- createJaspPlot(title = "Posterior Log Odds", width = 530, height = 400) # TODO: these should now display empty axes
  jaspResults[["logPosteriorOddsPlot"]] <- postLogOddsPlot
  
  postLogOddsPlot$copyDependenciesFromJaspObject(jaspResults[["stateModel"]])
  postLogOddsPlot$dependOnOptions("plotLogPosteriorOdds")
  
  if (!status$ready) {
    return()
  } else if (status$error) {
    postLogOddsPlot$error <- "badData"
  } else if (options$samplingMethod == "MCMC") {
    postLogOddsPlot$error <- "badData"
    postLogOddsPlot$errorMessage <- "Cannot display Posterior Log Odds when sampling method is MCMC."
  } else {
    .basregFillPlotPosteriorLogOdds(basregModel, jaspResults)
  }

  return()
}


.basregFillPlotPosteriorLogOdds <- function(basregModel, jaspResults) {
  
  p <- 
    function() { #TODO: figure out what happens if p fails
      BAS:::image.bas(basregModel, rotate = FALSE)
    }

  jaspResults[["logPosteriorOddsPlot"]]$plotObject <- p
  
  return()
}

# to be adapted and spliced into the logodds plot:
.plotImage.basReg <- function(x, top.models = 20, intensity = TRUE, prob = TRUE,
                              log = TRUE, rotate = TRUE, color = "rainbow", subset = NULL,
                              offset = 0.75, digits = 3, vlas = 2, plas = 0, rlas = 0,
                              ...) {
  # code from BAS:::image.bas
  postprob = x$postprobs
  top.models = min(top.models, x$n.models)
  best = order(-x$postprobs)[1:top.models]
  postprob = postprob[best]/sum(postprob[best])
  which.mat <- BAS:::list2matrix.which(x, best)
  nvar <- ncol(which.mat)
  if (is.null(subset))
    subset = 1:nvar
  which.mat = which.mat[, subset, drop = FALSE]
  nvar = ncol(which.mat)
  namesx = x$namesx[subset]
  scale = postprob
  prob.lab = "Posterior Probability"
  if (log) {
    scale = log(postprob) - min(log(postprob))
    prob.lab = "Log Posterior Odds"
  }
  if (intensity)
    which.mat = sweep(which.mat, 1, scale + offset, "*")
  if (rotate)
    scale = rev(scale)
  if (prob)
    m.scale = cumsum(c(0, scale))
  else m.scale = seq(0, top.models)
  mat = (m.scale[-1] + m.scale[-(top.models + 1)])/2
  colors = switch(color,
                  rainbow = c("black", rainbow(top.models +1, start = 0.75, end = 0.05)),
                  blackandwhite = gray(seq(0, 1, length = top.models)))
  
  # end of code from BAS:::image.bas
  
  w <- diff(mat)
  w <- c(w[1], w)
  
  dfHeat <- data.frame(
    x = rep(rev(mat), ncol(which.mat)),
    y = rep(1:nvar, each = nrow(which.mat)),
    z = c(which.mat[, nvar:1]),
    zCat = 1* (abs(c(which.mat[, nvar:1])) > .Machine$double.eps),
    w = rev(w)
  )
  dfHeat$x <- dfHeat$x - dfHeat$w / 2
  # above line is required since width expands half of widht left and half of width right
  # check code below to verify
  # cbind(dfHeat$x - dfHeat$w / 2, dfHeat$x + dfHeat$w / 2)
  nr <- nrow(dfHeat)
  dfLines <- data.frame(
    x = rep(c(mat - w, mat[length(mat)]), each = 2),
    y = rep(c(.5, nvar+.5), length(mat)+1),
    g = factor(rep(1:(length(mat)+1), each = 2))
  )
  
  discrete <- TRUE
  if (discrete) {
    show.legend <- FALSE
    colors[colors != "black"] <- "green"
    colors[colors == "black"] <- "white"
    mapping = ggplot2::aes(x = x, y = y, fill = zCat, width = w)
  } else {
    show.legend <- TRUE
    mapping = ggplot2::aes(x = x, y = y, fill = z, width = w)
  }
  
  g <- JASPgraphs::drawHeatmap(dat = dfHeat, show.legend = show.legend, fillColor = colors,
                               mapping = mapping,
                               geom = "tile")
  g
  xBreaks <- mat - w/2
  g <- JASPgraphs::drawAxis(graph = g, xName = prob.lab, xBreaks = xBreaks, xLabels = round(scale, digits = digits),
                            yName = "", yBreaks = 1:nvar, yLabels = namesx, xLimits = NULL, yLimits = NULL,
                            secondaryXaxis = list(~.,name = "Model Rank", breaks = xBreaks, labels = top.models:1),
                            xTrans = scales::reverse_trans())
  g <- JASPgraphs::drawLines(g, dat = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g),
                             color = "gray50", alpha = .7, size = 2)
  g <- JASPgraphs::themeJasp(graph = g, legend.position = "right", axisTickLength = 0,
                             bty = "o")
  
  # this plot needs some additional treatment
  unit <- JASPgraphs::graphOptions("axisTickLengthUnit")
  fillLg <- ggplot2::guide_colorbar(title = "", default.unit = unit,
                                    barheight = 5, barwidth = 1)
  g <- g + ggplot2::theme(
    axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(0, 0, .5, 0, unit)),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(.5, 0, 0, 0, unit))
  ) + ggplot2::guides(fill = fillLg)
  
  return(g)
  
}
