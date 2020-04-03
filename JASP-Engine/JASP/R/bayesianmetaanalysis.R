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

# Main function ----

BayesianMetaAnalysis <- function(jaspResults, dataset, options) {
  
  # Ready: variables needed for the analysis (confidence interval missing)
  ready <- options[["effectSize"]] != "" && (options[["standardError"]] != "" || (all(unlist(options$confidenceInterval) != "")  && !is.null(unlist(options[["confidenceInterval"]])))) 
  
  # Dependencies: basically everything
  # dependencies <- .bmaDependencies
  
  # Dataset with effectSize, standardError, and studyLabels
  # If data is null stuff is missing
  dataset <- .bmaReadData(jaspResults, options)
  
  # Table: Posterior Model Estimates
  .bmaMainTable(jaspResults, dataset, options, ready, .bmaDependencies)
  
  # Table: Model Probabilities
  if(options$postTable){
    .bmaPostModelTable(jaspResults, dataset, options, ready, .bmaDependencies)
  }
  
  # Table: Effect Sizes per Study
  if(options$esTable){
    .bmaEffectSizeTable(jaspResults, dataset, options, ready, .bmaDependencies)
  }
  
  # Plot: Prior(s); only when checked  
  if(options$plotPrior){
    .bmaPriorPlot(jaspResults, dataset, options, ready)
  }
  
  # Plot: Prior(s) and Posterior(s); only when checked 
  if(options$plotPosterior){  
    .bmaPriorAndPosteriorPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }
  
  # Plot: Forest plot; only when checked
  if(options$checkForestPlot || options$plotCumForest){  
    .bmaForestPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }
  
  # Plot: Cumulative forest plot and sequential; only when checked
  if(options$plotSequential || options$plotSeqPM){
    .bmaSequentialPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }
}

.bmaDependencies <- c("effectSize", "standardError", "confidenceInterval", "modelSpecification",
                      "allPos", "allNeg", "priorH0FE", "priorH1FE", "priorH0RE", "priorH1RE",
                      "priorES", "informativeCauchyLocation", "informativeCauchyScale",
                      "checkLowerPrior", "checkUpperPrior", "lowerTrunc", "upperTrunc",
                      "informativeNormalMean", "informativeNormalStd",
                      "informativeTLocation", "informativeTScale", "informativeTDf",
                      "priorSE", "inverseGammaShape", "inverseGammaScale",
                      "informativehalfTScale", "informativehalfTDf",
                      "BFComputation", "iterBridge", "iterMCMC", "chainsMCMC")

# Get dataset
.bmaReadData <- function(jaspResults, options){
  varES <- options[["effectSize"]]
  varSE <- options[["standardError"]]
  CI <- unlist(options$confidenceInterval)
  lower <- CI[[1]]
  upper <- CI[[2]]
  study <- options[["studyLabels"]]
  if(varES == "") varES <- NULL
  if(varSE == "") varSE <- NULL
  if(CI[[1]] == ""  || CI[[2]] == "" || is.null(CI)) {
    lower <- NULL
    upper <- NULL
  }
  if(study == "") study <- NULL
  variables.to.read <- c(varES, varSE, lower, upper, study)
  dataset <- .readDataSetToEnd(columns.as.numeric = variables.to.read, 
                               exclude.na.listwise = variables.to.read)
  return(dataset)
}

# Save priors for later use (without data)
.bmaPriors <- function(jaspResults, options) {
  if (!is.null(jaspResults[["bmaPriors"]])) return(jaspResults[["bmaPriors"]]$object)
  
  # Effect size prior parameters
  # Lower and upper limits without truncation
  lowerES <- -Inf
  upperES <- Inf
  
  # prior distribution
  if(options$priorES == "cauchy"){
    familyES <- "t"
    paramES <- c(options$informativeCauchyLocation, 
                 options$informativeCauchyScale, 1)
  } else if(options$priorES == "normal"){
    familyES <- "norm"
    paramES <- c(options$informativeNormalMean, 
                 options$informativeNormalStd)
  } else if(options$priorES == "t"){
    familyES <- "t"
    paramES <- c(options$informativeTLocation, 
                 options$informativeTScale, 
                 options$informativeTDf)
  }
  
  # If truncated is checked
  if(options$checkLowerPrior){
    lowerES <- options$lowerTrunc
  }
  if(options$checkUpperPrior){
    upperES <- options$upperTrunc
  }
  
  if (lowerES >= upperES)
    .quitAnalysis(gettext("The prior lower bound is not smaller than the upper bound.")) 
  
  # Heterogeneity prior parameters
  # Inverse gamma prior
  if(options$priorSE == "inverseGamma"){
    familySE <- "invgamma"
    paramSE <- c(options$inverseGammaShape, 
                 options$inverseGammaScale)
  }
  # Half t prior
  if(options$priorSE == "halfT"){
    familySE <- "t"
    paramSE <- c(0, # location is always zero
                 options$informativehalfTScale, 
                 options$informativehalfTDf)
  }
  
  # Make priors (probability density functions)
  d <- metaBMA::prior(familyES, paramES, lowerES, upperES)
  tau <- metaBMA::prior(familySE, paramSE, 0)
  
  if(options[["modelSpecification"]] == "CRE" && options$direction == "allPos"){
    x <- seq(-1, -1e-05, 0.001)
    if(any(d(x) > 0))
      .quitAnalysis(gettext("Your prior contains negative values."))
  } 
  if(options[["modelSpecification"]] == "CRE" && options$direction == "allNeg"){
    x <- seq(1e-05, 1, 0.001)
    if(any(d(x) > 0))
      .quitAnalysis(gettext("Your prior contains positive values."))
  } 
  
  
  # Save priors
  jaspResults[["bmaPriors"]] <- createJaspState(
    object=list(d = d, tau = tau),
    dependencies=c("priorES", 
                   "cauchy", "informativeCauchyLocation", "informativeCauchyScale",
                   "checkLowerPrior", "lowerTrunc", "checkUpperPrior", "upperTrunc",
                   "normal", "informativeNormalMean", "informativeNormalStd",
                   "t", "informativeTLocation", "informativeTScale","informativeTDf",
                   "priorSE", "inverseGamma", "inverseGammaShape", "inverseGammaScale",
                   "halfT", "informativehalfTScale", "informativehalfTDf"))
  
  return(jaspResults[["bmaPriors"]]$object)
}

#For state
.bmaResultsState <- function(jaspResults, dataset, options, .bmaDependencies) {
  
  if(!is.null(jaspResults[["bmaResults"]])) return(jaspResults[["bmaResults"]]$object)
  
  results <- .bmaResults(jaspResults, dataset, options)
  
  # The results object is too large for .jasp files. Break it up and reassemble only the required components.
  bmaResults <- list()
  
  # Averaged model
  bma                       <- list()
  bma[["estimates"]]        <- results$estimates
  if(options[["modelSpecification"]] != "CRE")
    anchorPoint             <- results$estimates["averaged", 1]
  if(options[["modelSpecification"]] == "CRE")
    anchorPoint             <- results$estimates["ordered", 1]
  bma[["xPost"]]            <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  bma[["yPost"]]            <- results$posterior_d(bma[["xPost"]])
  bma[["yPrior"]]           <- results$meta$fixed$prior_d(bma[["xPost"]])
  bma[["dfPointsY"]]        <- data.frame(prior = results$meta$fixed$prior_d(0),
                                          posterior = results$posterior_d(0))
  bmaResults[["bma"]]       <- bma
  
  # Prior and posterior models
  models                    <- list()
  models[["prior"]]         <- results$prior_models
  models[["posterior"]]     <- results$posterior_models
  bmaResults[["models"]]    <- models
  
  # Bayes factors
  bf <- list()
  bf[["BF"]]                <- results$BF
  bf[["inclusionBF"]]       <- results$inclusion$incl.BF
  bf[["fixedBF"]]           <- results$meta$fixed$BF
  bf[["randomBF"]]          <- results$meta$random$BF
  bmaResults[["bf"]]        <- bf
  
  # Fixed effects model
  fixed <- list()
  fixed[["estimates"]]      <- results$meta$fixed$estimates
  ## Prior and posterior - effect size
  anchorPoint               <- results$meta$fixed$estimates["d", 1]
  fixed[["xPost"]]          <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  fixed[["yPost"]]          <- results$meta$fixed$posterior_d(fixed[["xPost"]])
  fixed[["yPrior"]]         <- results$meta$fixed$prior_d(fixed[["xPost"]])
  fixed[["dfPointsY"]]      <- data.frame(prior = results$meta$fixed$prior_d(0),
                                          posterior = results$meta$fixed$posterior_d(0))
  
  bmaResults[["fixed"]]     <- fixed
  
  # Random effects model
  random <- list()
  random[["estimates"]]     <- results$meta$random$estimates 
  random[["summary"]]       <- rstan::summary(results$meta$random$stanfit_dstudy)$summary
  ## Prior and posterior - effect size
  anchorPoint               <- random[["estimates"]]["d", 1]
  random[["xPost"]]         <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  random[["yPost"]]         <- results$meta$random$posterior_d(random[["xPost"]])
  random[["yPrior"]]        <- results$meta$random$prior_d(random[["xPost"]])
  ## Prior and posterior - heterogeneity
  anchorPoint               <- random[["estimates"]][2, "mean"]
  random[["xPostTau"]]      <- seq(-0.05, anchorPoint + 4, .001)
  random[["yPostTau"]]      <- results$meta$random$posterior_tau(random[["xPostTau"]])
  random[["yPriorTau"]]     <- results$meta$random$prior_tau(random[["xPostTau"]])
  random[["dfPointsY"]]     <- data.frame(prior = results$meta$random$prior_d(0),
                                          posterior = results$meta$random$posterior_d(0))
  
  bmaResults[["random"]]    <- random
  
  # Ordered effects model
  if(options[["modelSpecification"]] == "CRE"){
    
    ordered                 <- list()
    ordered[["estimates"]]  <- results$meta$ordered$estimates
    ordered[["summary"]]    <- rstan::summary(results$meta$ordered$stanfit_dstudy)$summary
    ## Prior and posterior - effect size
    anchorPoint             <- results$meta$ordered$estimates[2, "mean"]
    if(options$direction == "allPos") xSeq <- seq(-0.05, anchorPoint + 4, .001)
    if(options$direction == "allNeg") xSeq <- seq(anchorPoint - 4, 0.05, .001)
    ordered[["xPost"]]   <- xSeq
    ordered[["yPost"]]   <- results$meta$ordered$posterior_d(ordered[["xPost"]])
    ordered[["yPrior"]]  <- results$meta$ordered$prior_d(ordered[["xPost"]])
    
    ## Prior and posterior - heterogeneity
    anchorPoint             <- results$meta$ordered$estimates[2, "mean"]
    ordered[["xPostTau"]]   <- seq(-0.05, anchorPoint + 4, .001)
    ordered[["yPostTau"]]   <- results$meta$ordered$posterior_tau(ordered[["xPostTau"]])
    ordered[["yPriorTau"]]  <- results$meta$ordered$prior_tau(ordered[["xPostTau"]])
    ordered[["dfPointsY"]]  <- data.frame(prior = results$meta$ordered$prior_d(0),
                                          posterior = results$meta$ordered$posterior_d(0))
    
    bmaResults[["ordered"]] <- ordered
  }
  
  # Save trimmed down list in state and return
  jaspResults[["bmaResults"]] <- createJaspState(object=bmaResults, dependencies=.bmaDependencies)
  return(jaspResults[["bmaResults"]]$object)
}

# Save the Bayesian meta-analysis
.bmaResults <- function(jaspResults, dataset, options) {
  
  
  varES <- options[["effectSize"]]
  
  # Get necessary variables
  y <- dataset[, .v(options[["effectSize"]])]
  
  if(options[["modelSpecification"]] == "CRE" && options[["direction"]] == "allPos"){
    
    negativeValues <- function(){
      if(all(dataset[, .v(options[["effectSize"]])] < 0))
        return(gettextf("No positive numbers found in %s", options[["effectSize"]]))
    }
    
    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = negativeValues)
    
  } else if(options[["modelSpecification"]] == "CRE" && options[["direction"]] == "allNeg"){
    
    positiveValues <- function(){
      if(all(dataset[, .v(options[["effectSize"]])] > 0))
        return(gettextf("No negative numbers found in %s", options[["effectSize"]]))
    }
    
    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = positiveValues)   
  }
  
  if(all(unlist(options[["confidenceInterval"]]) != "") && !is.null(unlist(options[["confidenceInterval"]]))){
    lower <- dataset[, .v(options$confidenceInterval[[1]][[1]])]
    upper <- dataset[, .v(options$confidenceInterval[[1]][[2]])]
    
    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = function() {
                 if (!all(lower < upper))
                   return(gettextf("The 95%% CI Lower Bound must be smaller than the Upper Bound."))
               })
    
    SE <- (upper - lower)/2/qnorm(0.975)
  }
  if(options$standardError != ""){
    SE <- dataset[, .v(options[["standardError"]])]
    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               type = "negativeValues",
               negativeValues.target = options$standardError)
  }
  
  
  # Advanced: estimation settings
  iter <- options[["iterMCMC"]]
  chains <- options[["chainsMCMC"]]
  
  # Advanced: bayes factor computation
  if(options$BFComputation == "integration"){
    logml <- "integrate"
    logml_iter <- 5000
  } else if(options$BFComputation == "bridgeSampling"){
    logml <- "stan"
    logml_iter <- options[["iterBridge"]]
  }
  
  # Prior model probabilities
  prior <- c(options[["priorH0FE"]], options[["priorH1FE"]], 
             options[["priorH0RE"]], options[["priorH1RE"]])
  
  if(all(prior == 0) && options[["modelSpecification"]] != "CRE") 
    .quitAnalysis(gettext("You cannot set all the prior model probabilties to zero."))
  
  # Get priors from jasp state
  .bmaPriors(jaspResults, options)
  
  d   <- jaspResults[["bmaPriors"]]$object[["d"]]
  tau <- jaspResults[["bmaPriors"]]$object[["tau"]]
  
  
  # Bayesian meta analysis
  if(options$modelSpecification != "CRE"){
    p <- try({
      # Bayesian model averaging (includes fixed and random effects)
      results <- metaBMA::meta_bma(y     = y, 
                                   SE    = SE, 
                                   prior = prior, 
                                   d     = d, 
                                   tau   = tau,
                                   logml   = logml,
                                   logml_iter = logml_iter,
                                   iter     = iter,
                                   chains = chains)
    })
  } else {
    p <- try({
      # Ordered effects
      results <- metaBMA::meta_ordered(y = y, 
                                       SE = SE, 
                                       d = d, 
                                       tau = tau,
                                       # logml = logml,
                                       # logml_iter = logml_iter,
                                       iter = 10000 # because of an issue with stored variables, it is not yet possible to make it reactive.
                                       # chains = chains
      )
    })
  }
  
  if(isTryError(p)){
    .quitAnalysis(gettextf("The model could not be fit. Please check the following: Do you have at least n=2 studies? If the prior is truncated, is it consistent with the data (when most effect sizes are negative, the analysis may not work when the prior is constrained to be postive)?")) 
  }
  
  return(results)
}

.bmaGetModelName <- function(options) {
  if(options[["modelSpecification"]] == "CRE") return("ordered")
  if(options[["modelSpecification"]] == "BMA") return("averaged")
  if(options[["modelSpecification"]] == "RE")  return("random")
  return("fixed")
}

.bmaCalculateBFHeterogeneity <- function(prior_models, posterior_models){
  postOdds <- (posterior_models["random_H0"] + posterior_models["random_H1"]) / (posterior_models["fixed_H0"] + posterior_models["fixed_H1"])
  priorOdds <- (prior_models[3] + prior_models[4]) / (prior_models[1] + prior_models[2])
  BFheterogeneity <- postOdds/priorOdds
  return(BFheterogeneity)
}

.bmaSequentialResults <- function(jaspResults, dataset, options, .bmaDependencies) {
  
  if(!is.null(jaspResults[["bmaSeqResults"]])) return(jaspResults[["bmaSeqResults"]]$object)
  
  startProgressbar(nrow(dataset)-1)
  
  seqResults <- list(mean=numeric(), lowerMain=numeric(), upperMain=numeric(), 
                     BFs=numeric(1), posterior_models=list(), BFsHeterogeneity = numeric(1)) 
  
  d                       <- .bmaPriors(jaspResults, options)[["d"]]
  # Fix voor truncated priors
  priorSamples            <- sample(seq(-10, 10, by = 0.001), size = 2e5, replace = TRUE, prob = d(seq(-10, 10, by = 0.001)))
  seqResults$mean[1]      <- mean(priorSamples)
  seqResults$lowerMain[1] <- quantile(priorSamples, probs = 0.025)
  seqResults$upperMain[1] <- quantile(priorSamples, probs = 0.975)
  
  modelName <- .bmaGetModelName(options)
  
  for(i in 2:nrow(dataset)){
    bmaResults <- .bmaResults(jaspResults, dataset[1:i, ], options)
    
    seqResults$mean[i]      <- bmaResults$estimates[modelName, "mean"]
    seqResults$lowerMain[i] <- bmaResults$estimates[modelName, "2.5%"]
    seqResults$upperMain[i] <- bmaResults$estimates[modelName, "97.5%"]
    
    if(options[["modelSpecification"]] == "BMA"){
      seqResults$BFs[i] <- bmaResults$inclusion$incl.BF
      seqResults$BFsHeterogeneity[[i]] <- .bmaCalculateBFHeterogeneity(bmaResults$prior_models, bmaResults$posterior_models)
    }
    if(options[["modelSpecification"]] == "FE")  seqResults$BFs[i] <- bmaResults$BF["fixed_H1", "fixed_H0"]
    if(options[["modelSpecification"]] == "RE"){
      seqResults$BFs[i] <- bmaResults$BF["random_H1", "random_H0"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults$BF["random_H1", "fixed_H1"]
    }
    if(options[["modelSpecification"]] == "CRE"){
      seqResults$BFs[i] <- bmaResults$BF["ordered", "null"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults$BF["ordered", "fixed"]
    }
    
    seqResults$posterior_models[[i]] <- bmaResults$posterior_models
    
    
    progressbarTick()
  }
  
  jaspResults[["bmaSeqResults"]] <- createJaspState(object=seqResults, dependencies=.bmaDependencies)
  
  return(jaspResults[["bmaSeqResults"]]$object)
}

# Table: Posterior Model Estimates
.bmaMainTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["bmaTable"]])) return()
  bmaTable <- createJaspTable(title = gettext("Posterior Estimates per Model"))
  bmaTable$position <- 1
  
  # Add standard depencies
  bmaTable$dependOn(options = c(.bmaDependencies, "bayesFactorType"))
  
  if (options$bayesFactorType == "BF10")
    bfTitle <- gettextf("BF%s%s", "\u2081", "\u2080")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettextf("BF%s%s", "\u2080", "\u2081")
  else
    bfTitle <- gettextf("Log(BF%s%s)", "\u2081", "\u2080")
  
  # Add columns
  bmaTable$addColumnInfo(name = "model", title = "", type = "string", combine = TRUE)
  bmaTable$addColumnInfo(name = "parameter", title = "", type = "string")
  bmaTable$addColumnInfo(name = "ES", title = gettext("Mean"), type = "number")
  bmaTable$addColumnInfo(name = "SD", title = gettext("SD"), type = "number")
  bmaTable$addColumnInfo(name = "lb", title = gettext("Lower"), type = "number",
                         overtitle = gettextf("95%% Credible Interval"))
  bmaTable$addColumnInfo(name = "ub", title = gettext("Upper"), type = "number",
                         overtitle = gettextf("95%% Credible Interval"))   
  bmaTable$addColumnInfo(name = "BF", title = bfTitle, type = "number")
  
  # Row names (tried to get modelRE idented, but failed)
  modelBMA <- gettext("Averaged")
  modelFE  <- gettext("Fixed effects")   
  modelRE  <- gettext("Random effects")
  modelCRE <- gettext("Ordered effects")
  
  tau <- "\u03C4"
  mu <- "\u03BC"
  
  if(options[["modelSpecification"]] == "FE"){
    model <- modelFE
    parameter <- mu
    group <- T
    bmaTable$setExpectedSize(1)
  }
  if(options[["modelSpecification"]] == "RE"){
    model <- c(modelRE, modelRE)
    parameter <- c(mu, tau)
    group <- c(T, F)
    bmaTable$setExpectedSize(2)
  }
  if(options[["modelSpecification"]] == "BMA"){
    model <- c(modelFE, modelRE, modelRE, modelBMA, modelBMA)
    parameter <- c(mu, mu, tau, mu, tau)
    group <- c(T, T, F, T, F)
    bmaTable$setExpectedSize(5)
  }
  if(options[["modelSpecification"]] == "CRE"){
    model <- c(modelFE, modelCRE, modelCRE, modelRE, modelRE)
    parameter <- c(mu, mu, tau, mu, tau)
    group <- c(T, T, F, T, F)
    bmaTable$setExpectedSize(5)
  }
  
  if(options$modelSpecification != "FE"){
    bmaTable$addFootnote(gettextf("%s and %s are the group-level effect size and standard deviation, respectively.", "\u03BC", "\u03C4"))
  } else {
    bmaTable$addFootnote(gettextf("%s is the group-level effect size.", "\u03BC"))
  }
  
  jaspResults[["bmaTable"]] <- bmaTable
  
  # Check if ready
  if(!ready){
    rows <- data.frame(model = model, 
                       parameter = parameter,
                       ES = ".", 
                       SD = ".", 
                       lb = ".", 
                       ub = ".", 
                       BF = ".",
                       .isNewGroup = group)
    row.names(rows) <- paste0("row", 1:length(model))
    bmaTable$addRows(rows)
    return()
  }
  
  # Get analysis results
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  
  # Get results per column (different per model)
  if(options[["modelSpecification"]] == "BMA"){
    meanES <- c(bmaResults[["bma"]]$estimates["fixed", "mean"], 
                bmaResults[["bma"]]$estimates["random", "mean"],
                bmaResults[["random"]]$estimates["tau", "mean"],
                bmaResults[["bma"]]$estimates["averaged", "mean"],
                NA)
    meanSD <- c(bmaResults[["bma"]]$estimates["fixed", "sd"], 
                bmaResults[["bma"]]$estimates["random", "sd"], 
                bmaResults[["random"]]$estimates["tau", "sd"],
                bmaResults[["bma"]]$estimates["averaged", "sd"],
                NA)
    lower <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"], 
               bmaResults[["bma"]]$estimates["random", "2.5%"], 
               bmaResults[["random"]]$estimates["tau", "2.5%"],
               bmaResults[["bma"]]$estimates["averaged", "2.5%"],
               NA)
    upper <- c(bmaResults[["bma"]]$estimates["fixed", "97.5%"], 
               bmaResults[["bma"]]$estimates["random", "97.5%"], 
               bmaResults[["random"]]$estimates["tau", "97.5%"],
               bmaResults[["bma"]]$estimates["averaged", "97.5%"],
               NA)
    BF <- c(bmaResults[["bf"]]$BF["fixed_H1", "fixed_H0"], 
            bmaResults[["bf"]]$BF["random_H1", "random_H0"], 
            bmaResults[["bf"]]$BF["random_H1", "fixed_H1"],
            bmaResults[["bf"]]$inclusionBF,
            .bmaCalculateBFHeterogeneity(prior_models = bmaResults[["models"]]$prior, 
                                         posterior_models = bmaResults[["models"]]$posterior))
  }
  else if(options[["modelSpecification"]] == "RE"){
    meanES <- bmaResults[["random"]]$estimates[, "mean"]
    meanSD <- bmaResults[["random"]]$estimates[, "sd"]
    lower <- bmaResults[["random"]]$estimates[, "2.5%"]
    upper <- bmaResults[["random"]]$estimates[, "97.5%"]
    BF <- c(bmaResults[["bf"]]$BF["random_H1", "random_H0"], 
            bmaResults[["bf"]]$BF["random_H1", "fixed_H1"])
  }
  else if(options[["modelSpecification"]] == "FE"){
    meanES <- bmaResults[["fixed"]]$estimates[, "mean"]
    meanSD <- bmaResults[["fixed"]]$estimates[, "sd"]
    lower <- bmaResults[["fixed"]]$estimates[, "2.5%"]
    upper <- bmaResults[["fixed"]]$estimates[, "97.5%"]
    BF <- bmaResults[["bf"]]$BF["fixed_H1", "fixed_H0"]
  }
  else if(options[["modelSpecification"]] == "CRE"){
    meanES <- c(bmaResults[["bma"]]$estimates["fixed", "mean"],
                bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "mean"],
                bmaResults[["random"]]$estimates[, "mean"])
    meanSD <- c(bmaResults[["bma"]]$estimates["fixed", "sd"],
                bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "sd"],
                bmaResults[["random"]]$estimates[, "sd"])
    lower <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"],
               bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "2.5%"],
               bmaResults[["random"]]$estimates[, "2.5%"])
    upper <- c(bmaResults[["bma"]]$estimates["fixed", "97.5%"],
               bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "97.5%"],
               bmaResults[["random"]]$estimates[, "97.5%"])
    BF <- c(bmaResults[["bf"]]$BF["fixed", "null"],
            bmaResults[["bf"]]$BF["ordered", "null"],
            bmaResults[["bf"]]$BF["ordered", "fixed"],
            bmaResults[["bf"]]$BF["random", "null"],
            bmaResults[["bf"]]$BF["random", "fixed"])
  }
  
  
  footnoteRandomBFtau <- gettextf("Bayes factor of the random effects H%1$s over the fixed effects H%1$s.", "\u2081")
  
  footnoteAverage <- gettextf("Posterior estimates are based on the models that assume an effect to be present. The Bayes factor is based on all four models: fixed effects H%2$s & random effects H%2$s over the fixed effects H%1$s & random effects H%1$s.", "\u2080", "\u2081")
  footnoteAverageBFtau <- gettextf("Model averaged posterior estimates for %3$s are not yet available, but will be added in the future. The Bayes factor is based on all four models: random effects H%1$s & H%2$s over the fixed effects H%1$s & H%2$s.", "\u2080", "\u2081", "\u03C4")
  footnoteOrderedBFtau <- gettextf("Bayes factor of the (unconstrained/constrained) random effects H%1$s over the fixed effects H%1$s.", "\u2081")
  
  if(options[["modelSpecification"]] == "CRE") 
    creBF <- bmaResults[["bf"]]$BF["ordered", "random"]
  
  if(options[["bayesFactorType"]] == "BF01"){
    BF <- 1/BF
    footnoteRandomBFtau <- gettextf("Bayes factor of the fixed effects H%1$s over the random effects H%1$s.", "\u2081")
    footnoteAverage <- gettextf("Model averaged posterior estimates are based on the models that assume an effect to be present. The Bayes factor is based on all four models: fixed effects H%1$s & random effects H%1$s over the fixed effects H%2$s & random effects H%2$s.", "\u2080", "\u2081")
    footnoteAverageBFtau <- gettextf("Model averaged posterior estimates for %3$s are not yet available, but will be added in the future. The Bayes factor is based on all four models: fixed effects H%1$s & H%2$s over the random effects H%1$s & H%2$s.", "\u2080", "\u2081", "\u03C4")
    footnoteOrderedBFtau <- gettextf("Bayes factor of the fixed effects H%1$s over the (unconstrained/constrained) random effects H%1$s.", "\u2081")
    
    if(options[["modelSpecification"]] == "CRE"){
      creBF <- 1/bmaResults[["bf"]]$BF["ordered", "random"]
    }
  }
  if(options[["bayesFactorType"]] == "LogBF10"){
    BF <- log(BF)
    if(options[["modelSpecification"]] == "CRE"){
      creBF <- log(bmaResults[["bf"]]$BF["ordered", "random"])
    }
  }
  # Add results to table
  rows <- data.frame(model = model, 
                     parameter = parameter,
                     ES = meanES, 
                     SD = meanSD, 
                     lb = lower, 
                     ub = upper, 
                     BF = BF,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(model))
  
  bmaTable$addRows(rows)
  
  if(options$modelSpecification == "RE") bmaTable$addFootnote(footnoteRandomBFtau, colNames = "BF", rowNames = "row1")
  
  if(options$modelSpecification == "BMA") {
    bmaTable$addFootnote(footnoteAverage,
                         colNames = "parameter", rowNames="row3") 
    bmaTable$addFootnote(footnoteRandomBFtau,
                         colNames = "BF", rowNames = "row2")
    bmaTable$addFootnote(footnoteAverageBFtau,
                         colNames = "parameter", rowNames = "row4")
  }
  
  if(options$modelSpecification == "CRE"){
    if(options[["bayesFactorType"]] == "BF01" || options[["bayesFactorType"]] == "LogBF10"){
      footnoteCREbf <- gettextf("Bayes factor of the ordered effects H%1$s over the fixed effects H%1$s. The Bayes factor for the ordered effects H%1$s versus the unconstrained (random) effects H%1$s model is %2$.3f.", "\u2081", creBF)
    } else if(options[["bayesFactorType"]] == "BF10"){
      footnoteCREbf <-gettextf("Bayes factor of the fixed effects H%1$s over the ordered effects H%1$s. The Bayes factor for the unconstrained (random) effects H%1$s versus the ordered effects H%1$s model is %2$.3f.", "\u2081", creBF)
    }
    
    
    bmaTable$addFootnote(footnoteCREbf,
                         colNames = "BF", rowNames="row1") 
    bmaTable$addFootnote(footnoteOrderedBFtau, colNames = "BF", rowNames = c("row2", "row4"))
  }
}

# Table: Model Probabilities
.bmaPostModelTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["postTable"]])) return()
  postTable <- createJaspTable(title = gettext("Model Probabilities"))
  postTable$dependOn(c(.bmaDependencies, "postTable"))
  postTable$position <- 2
  
  # Add columns
  postTable$addColumnInfo(name = "model", title = "", type = "string")
  postTable$addColumnInfo(name = "priorProb",   title = gettext("Prior"),   type = "number")
  postTable$addColumnInfo(name = "postProb",   title = gettext("Posterior"),   type = "number")
  
  # Add table to output
  jaspResults[["postTable"]] <- postTable
  
  modelFixedH0 <- gettextf("Fixed H%s", "\u2080")
  modelFixedH1 <- gettextf("Fixed H%s", "\u2081")
  modelRandomH0 <- gettextf("Random H%s", "\u2080")
  modelRandomH1 <- gettextf("Random H%s", "\u2081")
  modelOrderedH1 <- gettextf("Ordered H%s", "\u2081")
  
  if(options$modelSpecification == "BMA"){
    model <- c(modelFixedH0, modelFixedH1, modelRandomH0, "Random H\u2081")
  }
  if(options$modelSpecification == "FE"){
    model <- c(modelFixedH0, modelFixedH1)
  }
  if(options$modelSpecification == "RE"){
    model <- c(modelRandomH0, "Random H\u2081")
  }
  if(options$modelSpecification == "CRE"){
    model <- c(modelFixedH0, modelFixedH1, modelOrderedH1, modelRandomH1)
  }
  
  # Check if ready
  if(!ready){
    row <- data.frame(model = model, priorProb = ".", postProb = ".")
    postTable$addRows(row)
    return()
  }
  
  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  
  # Get results per column (different per model)
  if(options$modelSpecification == "BMA"){
    postProb <- bmaResults[["models"]]$posterior
    priorProb <- bmaResults[["models"]]$prior
  }
  if(options$modelSpecification == "FE"){
    postProb <- bmaResults[["models"]]$posterior[c("fixed_H0", "fixed_H1")]
    priorProb <- bmaResults[["models"]]$prior[1:2]
  }
  if(options$modelSpecification == "RE"){
    postProb <- bmaResults[["models"]]$posterior[c("random_H0", "random_H1")]
    priorProb <- bmaResults[["models"]]$prior[3:4]
  }
  if(options$modelSpecification == "CRE"){
    postProb <- bmaResults[["models"]]$posterior
    priorProb <- bmaResults[["models"]]$prior
  }
  
  # Fill table
  row <- data.frame(model = model, priorProb =  priorProb, postProb = postProb)
  postTable$addRows(row)
}

# Table: Effect Sizes per Study
.bmaEffectSizeTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["esTable"]])) return()
  esTable <- createJaspTable(title = gettext("Effect Sizes per Study"))
  esTable$dependOn(c(.bmaDependencies, "esTable", "studyLabels"))
  esTable$position <- 3
  
  # Add standard columns
  esTable$addColumnInfo(name = "study", title = "", type = "string")
  esTable$addColumnInfo(name = "observedES", title = gettext("Observed"), type = "number")
  
  # Add conditional columns
  if(options$modelSpecification != "FE"){
    esTable$addColumnInfo(name = "estimatedES", title = gettext("Mean"), type = "number",
                          overtitle = gettext("Estimated"))
    esTable$addColumnInfo(name = "estimatedLower", title = gettext("Lower"), type = "number",
                          overtitle = gettext("Estimated"))
    esTable$addColumnInfo(name = "estimatedUpper", title = gettext("Upper"), type = "number",
                          overtitle = gettext("Estimated"))
  }
  
  # Only show conditional columns for right analysis
  esTable$showSpecifiedColumnsOnly <- TRUE
  
  # Add table to output
  jaspResults[["esTable"]] <- esTable
  
  # Check if ready
  if(!ready){
    if(options[["studyLabels"]] != ""){
      studyLabels <- dataset[, .v(options[["studyLabels"]])]
      row <- data.frame(study = studyLabels, 
                        observedES = ".", 
                        estimatedES = ".", 
                        estimatedLower = ".", 
                        estimatedUpper = ".")
      esTable$addRows(row)
    }
    return()
  }    
  
  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  
  # Get effect size variable
  varES <- dataset[, .v(options[["effectSize"]])]
  
  # Create empty vectors
  estimatedES <- rep(NA, length(varES))
  estimatedLower <- rep(NA, length(varES))
  estimatedUpper <- rep(NA, length(varES))
  
  # Fill vectors with estimation variables if not FE
  if(options$modelSpecification != "FE"){
    estimatedES    <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "mean"]
    estimatedLower <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "2.5%"]
    estimatedUpper <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "97.5%"]
  }
  
  if(options$modelSpecification == "CRE"){
    estimatedES <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "mean"]
    estimatedLower <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "2.5%"]
    estimatedUpper <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "97.5%"] 
  }
  
  # Add studylabels when given, otherwise use "Study n"
  if(options[["studyLabels"]] != ""){
    studyLabels <- dataset[, .v(options[["studyLabels"]])]
  } else {
    studyLabels <- paste(gettext("Study"), 1:length(varES))
  }
  
  # Add results to table
  row <- data.frame(study = studyLabels, 
                    observedES = varES, 
                    estimatedES = estimatedES, 
                    estimatedLower = estimatedLower, 
                    estimatedUpper = estimatedUpper)
  esTable$addRows(row)
  
  if(options$modelSpecification != "FE"){
    esTable$addFootnote(gettextf("Posterior mean and 95%% credible interval estimates from the random effects model."),
                        colNames = c("estimatedES", "estimatedLower", "estimatedUpper"))
  } else if(options$modelSpecification == "CRE"){
    esTable$addFootnote(gettextf("Posterior mean and 95%% credible interval estimates from the constrained random effects model."),
                        colNames = c("estimatedES", "estimatedLower", "estimatedUpper"))
  }
}

# Plot: prior(s)  
.bmaPriorPlot <- function(jaspResults, dataset, options, ready) {
  priorContainer <- createJaspContainer(title = gettext("Prior"))
  priorContainer$dependOn("plotPrior")
  jaspResults[["priorContainer"]] <- priorContainer
  jaspResults[["priorContainer"]]$position <- 4
  
  # Create empty plot
  priorPlot <- createJaspPlot(plot = NULL, title = gettext("Effect Size"), width = 450, height = 350)
  priorPlot$position <- 1
  
  # Custom dependencies (only dependent on prior settings)
  priorPlot$dependOn(c("priorES", 
                       "cauchy", "informativeCauchyLocation", "informativeCauchyScale",
                       "checkLowerPrior", "lowerTrunc", "checkUpperPrior", "upperTrunc",
                       "normal", "informativeNormalMean", "informativeNormalStd",
                       "t", "informativeTLocation", "informativeTScale","informativeTDf"
  ))
  
  # Fill plot with effect size prior
  .bmaFillPriorPlot(priorPlot, jaspResults, dataset, options, type = "ES")
  priorContainer[["ES"]] <- priorPlot
  
  # Make plot hetergeneity prior
  if(options[["modelSpecification"]] != "FE"){
    priorPlotSE <- createJaspPlot(plot = NULL, title = gettext("Heterogeneity"), width = 350, height = 350)
    priorPlotSE$dependOn(c("priorSE", "inverseGamma", "inverseGammaShape", "inverseGammaScale",
                           "halfT", "informativehalfTScale", "informativehalfTDf"))
    priorPlotSE$position <- 2
    .bmaFillPriorPlot(priorPlotSE, jaspResults, dataset, options, type = "SE")
    priorContainer[["SE"]] <- priorPlotSE
  }
}

# Fill prior plot
.bmaFillPriorPlot <- function(priorPlot, jaspResults, dataset, options, type){
  # Get priors from jasp state
  if (is.null(jaspResults[["bmaPriors"]]))
    .bmaPriors(jaspResults, options)
  priors <- jaspResults[["bmaPriors"]]$object
  
  # Get parameters and x limits
  if(type == "ES"){
    prior <- priors$d
    mean <- attr(prior, "param")[1]
    s <- attr(prior, "param")[2]
    xlimLeft <- mean - (s * 5)
    xlab <- bquote(paste(.(gettext("Effect size")), ~mu))
  } else if(type == "SE"){
    prior <- priors$tau
    mean <- attr(prior, "param")[1]
    s <- attr(prior, "param")[2]
    xlimLeft <- 0
    xlab <- bquote(paste(.(gettext("Heterogeneity")), ~tau))
  }
  
  if(options$modelSpecification == "CRE" && options$direction == "allPos"){
    xlimLeft <- 0
  } else if(options$modelSpecification == "CRE" && options$direction == "allNeg"){
    xlimRight <- 0
  }
  
  xlimRight <- mean + (s * 5)  
  xlimLeft <- xlimLeft - 0.05
  xlimRight <- xlimRight + 0.05
  
  # Create dataframe for ggplot
  x <- c(xlimLeft, xlimRight)
  df <- data.frame(x = x)
  
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(xlimLeft, xlimRight, 0.5))
  
  # Plot density function
  plot <- ggplot2::ggplot(df, ggplot2::aes(x)) +
    ggplot2::stat_function(fun = prior, n = 1000, size = 1) +
    ggplot2::labs(x = xlab, y = gettext("Density")) +
    ggplot2::xlim(xlimLeft, xlimRight) +
    ggplot2::scale_x_continuous(breaks = xBreaks)
  plot <- JASPgraphs::themeJasp(plot)
  priorPlot$plotObject <- plot
  return()
}

# Plot: Prior and Posterior
.bmaPriorAndPosteriorPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  postContainer <- createJaspContainer(title = gettext("Prior and Posteriors"))
  postContainer$dependOn(c(.bmaDependencies, "plotPosterior", "shade", "addInfo", "addLines"))
  jaspResults[["postContainer"]] <- postContainer
  jaspResults[["postContainer"]]$position <- 5
  
  # Create empty plot
  postPlotES <- createJaspPlot(plot = NULL, title = gettext("Effect size"), width = 500, height = 350)
  postPlotES$position <- 1
  
  
  
  # Check if ready
  if(!ready){
    return()
  }
  
  # Fill posterior plot effect size
  .bmaFillPostPlot(postPlotES, jaspResults, dataset, options, type = "ES")
  postContainer[["ES"]] <- postPlotES
  
  # Make posterior plot heterogeneity
  if(options$modelSpecification != "FE"){
    postPlotSE <- createJaspPlot(plot = NULL, title = gettext("Heterogeneity"), width = 500, height = 350)
    postPlotSE$position <- 2
    postContainer[["SE"]] <- postPlotSE
    .bmaFillPostPlot(postPlotSE, jaspResults, dataset, options, type = "SE")
  }
}

# Fill prior and posterior plot
.bmaFillPostPlot <- function(postPlot, jaspResults, dataset, options, type){
  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  
  # Get prior and posterior functions, and 95% CI intervals
  alpha <- 0.2
  postName <- "Posterior"
  valuesCol <- c("black", "black")
  valuesLine <- c("solid", "dotted")
  
  if(type == "ES"){
    xlab <- bquote(paste(.(gettext("Effect size")), ~mu))
    xlim <- c(-4, 4)
    if(options[["modelSpecification"]] == "BMA"){
      int <- c(bmaResults[["bma"]]$estimates["averaged", "2.5%"], bmaResults[["bma"]]$estimates["averaged", "97.5%"])
      postName <- "Averaged"
      if(options[["addLines"]]){
        labelsModel <- c(bquote(.(gettext("Fixed H"))[1]),
                         bquote(.(gettext("Random H"))[1]),
                         bquote(.(gettext("Averaged H"))[1]), 
                         bquote(.(gettext("Prior H"))[1]))
      } else {
        labelsModel <- c(bquote(.(gettext("Averaged H"))[1]), bquote(.(gettext("Prior H"))[1]))
      }
      yPrior <- bmaResults[["bma"]]$yPrior
      xPost <- bmaResults[["bma"]]$xPost
      yPost <- bmaResults[["bma"]]$yPost
      dfPointsY <- bmaResults[["bma"]]$dfPointsY
    } else if(options[["modelSpecification"]] == "RE"){
      int <- c(bmaResults[["bma"]]$estimates["random", "2.5%"], bmaResults[["bma"]]$estimates["random", "97.5%"])
      postName <- "Random"
      labelsModel <- c(bquote(.(gettext("Random H"))[0]), bquote(.(gettext("Prior H"))[1]))
      yPrior <- bmaResults[["random"]]$yPrior
      xPost <- bmaResults[["random"]]$xPost
      yPost <- bmaResults[["random"]]$yPost
      dfPointsY <- bmaResults[["random"]]$dfPointsY
    } else if(options[["modelSpecification"]] == "FE"){
      int <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"], bmaResults[["bma"]]$estimates["fixed", "97.5%"])
      postName <- "Fixed"
      labelsModel <- c(bquote(.(gettext("Fixed H"))[1]), bquote(.(gettext("Prior H"))[1]))
      yPrior <- bmaResults[["fixed"]]$yPrior
      xPost <- bmaResults[["fixed"]]$xPost
      yPost <- bmaResults[["fixed"]]$yPost
      dfPointsY <- bmaResults[["fixed"]]$dfPointsY
    } else if(options[["modelSpecification"]] == "CRE"){
      int <- c(bmaResults[["bma"]]$estimates["ordered", "2.5%"], bmaResults[["bma"]]$estimates["ordered", "97.5%"])
      postName <- "Ordered"
      if(options[["addLines"]]){
        labelsModel <- c(bquote(.(gettext("Fixed H"))[1]), 
                         bquote(.(gettext("Ordered H"))[1]), 
                         bquote(.(gettext("Random H"))[1]), 
                         bquote(.(gettext("Prior H"))[1])
        )
      } else {
        labelsModel <- c(bquote(.(gettext("Ordered H"))[1]), 
                         bquote(.(gettext("Prior H"))[1]))
      }
      yPrior <- bmaResults[["ordered"]]$yPrior
      xPost <- bmaResults[["ordered"]]$xPost
      yPost <- bmaResults[["ordered"]]$yPost
      dfPointsY <- bmaResults[["ordered"]]$dfPointsY
    }
    # Heterogeneity priors
  } else if(type == "SE"){
    if(options[["modelSpecification"]] == "BMA" || options[["modelSpecification"]] == "RE"){
      int <- c(bmaResults[["random"]]$estimates["tau", "2.5%"], bmaResults[["random"]]$estimates["tau", "97.5%"])
      postName <- "Random"
      yPrior <- bmaResults[["random"]]$yPriorTau
      xPost <- bmaResults[["random"]]$xPostTau
      yPost <- bmaResults[["random"]]$yPostTau
      dfPointsY <- data.frame(prior = yPrior[which(xPost == 0)], posterior = yPost[which(xPost == 0)])
    } else if (options[["modelSpecification"]] == "CRE"){
      int <- c(bmaResults[["ordered"]]$estimates["tau", "2.5%"], bmaResults[["ordered"]]$estimates["tau", "97.5%"])
      postName <- "Ordered"
      yPrior <- bmaResults[["ordered"]]$yPriorTau
      xPost <- bmaResults[["ordered"]]$xPostTau
      yPost <- bmaResults[["ordered"]]$yPostTau
      dfPointsY <- data.frame(prior = yPrior[which(xPost == 0)], posterior = yPost[which(xPost == 0)])
    }
    
    if(options[["modelSpecification"]] == "BMA") valuesCol <- c("#009E73", "black")
    
    
    xlab <- bquote(paste(.(gettext("Heterogeneity")), ~tau))
    xlim <- c(0, 3)
    alpha <- 0.3
    
    if(options[["modelSpecification"]] == "BMA") labelsModel <- c(bquote(.(gettext("Random H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["modelSpecification"]] == "CRE") labelsModel <- c(bquote(.(gettext("Ordered H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["modelSpecification"]] == "FE") labelsModel <- c(bquote(.(gettext("Fixed H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["modelSpecification"]] == "RE") labelsModel <- c(bquote(.(gettext("Random H"))[1]), bquote(.(gettext("Prior H"))[1]))
  }
  
  index <- which(yPost > 0.0001)
  xPost <- xPost[index]
  yPost <- yPost[index]
  yPrior <- yPrior[index]

  df <- data.frame(x = c(xPost, xPost), y = c(yPrior, yPost), g = rep(c("Prior", postName), each = length(xPost)))
  
  if(options$addLines && (options$modelSpecification == "BMA" || options$modelSpecification == "CRE")){
    if(type == "ES"){
      yPostES <- c(bmaResults[["fixed"]]$yPost, bmaResults[["random"]]$yPost)
      xPostES <- c(bmaResults[["fixed"]]$xPost, bmaResults[["random"]]$xPost)
      gPostES <- c(rep("Fixed", length(bmaResults[["fixed"]]$xPost)), rep("Random", length(bmaResults[["random"]]$xPost)))
      dfPost <- data.frame(x = xPostES,  y = yPostES, g = gPostES)
      if(options[["modelSpecification"]] == "BMA"){
        valuesCol <- c("#fcae91ff", "#009E73", "black", "black")
      } else if(options[["modelSpecification"]] == "CRE"){
        valuesCol <- c("#fcae91ff", "black", "#009E73", "black")
      }
      valuesLine <- c("solid", "solid", "solid", "dotted")
    } else if(type == "SE"){
      yPostSE <- bmaResults[["random"]]$yPostTau
      xPostSE <- bmaResults[["random"]]$xPostTau
      gPostSE <- rep("Random", length(bmaResults[["random"]]$xPostTau))
      dfPost <- data.frame(x = xPostSE,  y = yPostSE, g = gPostSE)
      if(options[["modelSpecification"]] == "CRE"){
        valuesCol <- c("black", "#009E73", "black")
        valuesLine <- c("solid", "solid", "dotted")
        labelsModel <- c(bquote(.(gettext("Ordered H"))[1]), 
                         bquote(.(gettext("Random H"))[1]), 
                         bquote(.(gettext("Prior H"))[1]))
      }
    }
    df <- rbind(df, dfPost)
  }
  
  if(!options$addLines || options$modelSpecification == "RE" || options$modelSpecification == "FE"){
    df$g <- factor(df$g, levels = c(postName, "Prior"))
  } else if(options$addLines){
    if(type == "ES"){
      if(options$modelSpecification == "BMA") df$g <- factor(df$g, levels = c("Fixed", "Random", "Averaged", "Prior"))
      if(options$modelSpecification == "CRE") df$g <- factor(df$g, levels = c("Fixed", "Ordered", "Random", "Prior"))
    } else if(type == "SE"){
      if(options$modelSpecification == "BMA") df$g <- factor(df$g, levels = c("Random", "Prior"))
      if(options$modelSpecification == "CRE") df$g <- factor(df$g, levels = c("Ordered", "Random", "Prior"))
    }
  }
  
  if(type == "ES"){
    if(options$modelSpecification == "FE") BF <- bmaResults[["bf"]]$fixedBF["fixed_H1", "fixed_H0"]
    if(options$modelSpecification == "RE") BF <- bmaResults[["bf"]]$randomBF["random_H1", "random_H0"]
    if(options$modelSpecification == "BMA") BF <- bmaResults[["bf"]]$inclusionBF
    if(options$modelSpecification == "CRE") BF <- bmaResults[["bf"]]$BF["ordered", "null"]
    
    if(options$modelSpecification == "FE") CRI <- bmaResults[["bma"]]$estimates["fixed", c("2.5%", "97.5%")]
    if(options$modelSpecification == "RE") CRI <- bmaResults[["bma"]]$estimates["random", c("2.5%", "97.5%")]
    if(options$modelSpecification == "BMA") CRI <- bmaResults[["bma"]]$estimates["averaged", c("2.5%", "97.5%")]
    if(options$modelSpecification == "CRE") CRI <- bmaResults[["ordered"]]$estimates["average_effect", c("2.5%", "97.5%")]
    
    if(options$modelSpecification == "FE") med <- bmaResults[["bma"]]$estimates["fixed", "mean"]
    if(options$modelSpecification == "RE") med <- bmaResults[["bma"]]$estimates["random", "mean"]
    if(options$modelSpecification == "BMA") med <- bmaResults[["bma"]]$estimates["averaged", "mean"]
    if(options$modelSpecification == "CRE") med <- bmaResults[["ordered"]]$estimates["average_effect", "mean"]
    
  } else if (type == "SE"){
    if(options$modelSpecification == "RE") BF <- bmaResults[["bf"]]$BF["random_H1", "fixed_H1"]
    if(options$modelSpecification == "BMA") BF <- bmaResults[["bf"]]$BF["random_H1", "fixed_H1"]
    if(options$modelSpecification == "CRE") BF <- bmaResults[["bf"]]$BF["ordered", "fixed"]
    
    if(options$modelSpecification == "RE") CRI <- bmaResults[["random"]]$estimates["tau", c("2.5%", "97.5%")]
    if(options$modelSpecification == "BMA") CRI <- bmaResults[["random"]]$estimates["tau", c("2.5%", "97.5%")]
    if(options$modelSpecification == "CRE") CRI <- bmaResults[["ordered"]]$estimates["tau", c("2.5%", "97.5%")]
    
    
    if(options$modelSpecification == "RE" || options$modelSpecification == "BMA")  med <- bmaResults[["random"]]$estimates["tau", "mean"]
    if(options$modelSpecification == "CRE") med <- bmaResults[["ordered"]]$estimates["tau", "mean"]
  }
  
  
  
  if(!options[["addInfo"]]){
    BF <- NULL
    CRI <- NULL
    bfType <- NULL
    med <- NULL
  } else {
    if(options[["bayesFactorType"]] == "BF01") {
      BF    <- 1/BF
      bfType <- "BF01"
    } else if(options[["bayesFactorType"]] == "LogBF10") {
      BF <- log(BF)
      bfType <- "LogBF10"
    } else {
      bfType <- "BF10" 
    }
  }
  
  if(options[["addInfo"]]){
    BF <- round(BF, 3)
    CRI <- round(CRI, 3)
    med <- round(med, 3)    
  }
  
  
  pizzaTxt <- c("data | f H1", "data | r H1")
  bfSubscripts <-  c("BF[italic(rf)]", "BF[italic(fr)]")
  
  if(options$modelSpecification == "CRE"){
    pizzaTxt <- c("data | f H1", "data | o H1")
    bfSubscripts <-  c("BF[italic(of)]", "BF[italic(fo)]")
  }
  
  xr   <- range(df$x)
  idx  <- which.max(df$y)
  xmax <- df$x[idx]
  if (xmax > mean(xr)) {
    legend.position = c(0.2, 0.875)
  } else {
    legend.position = c(0.80, 0.875)
  }
  
  if(type == "ES"){
    plot <- JASPgraphs::PlotPriorAndPosterior(dfLines = df,
                                              lineColors = valuesCol, 
                                              BF = BF,
                                              CRI = CRI,
                                              bfType = bfType,
                                              xName = xlab,
                                              median = med,
                                              medianTxt = "Mean:")
  } else if(type == "SE"){
    plot <- JASPgraphs::PlotPriorAndPosterior(dfLines = df,
                                              lineColors = valuesCol,
                                              BF = BF,
                                              CRI = CRI,
                                              bfType = bfType,
                                              xName = xlab,
                                              bfSubscripts = bfSubscripts,
                                              pizzaTxt = pizzaTxt,
                                              median = med,
                                              medianTxt = "Mean:")
  }
  
  .extraPost <- function(plot, int, xPost, yPost){
    
    
    if(options[["shade"]]){
      shadeData <- data.frame(x = xPost[xPost < max(int) & xPost > min(int)], y = yPost[xPost < max(int) & xPost > min(int)])
      plot <- plot + ggplot2::geom_area(data = shadeData, mapping = ggplot2::aes(x = x, y = y), fill = "grey", group = 1, linetype = 1, color = NA, alpha = 0.5)
    }
    
    if(options[["addLines"]] && options[["modelSpecification"]] == "BMA"){
      plot <- plot + ggplot2::scale_linetype_manual(values = valuesLine)
    }  
    
    plot <- plot +
      ggplot2::scale_linetype_manual("", values = valuesLine, labels = labelsModel) +
      ggplot2::scale_color_manual("", values = valuesCol, labels = labelsModel) +
      ggplot2::theme(legend.text.align = 0,
                     legend.position = legend.position)
    return(plot)
  }
  
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, xPost))

  if(options[["addInfo"]]){
    plot$subplots$mainGraph <- plot$subplots$mainGraph + ggplot2::scale_x_continuous(name = xlab, breaks = xBreaks, limits = c(min(xPost), max(xPost)))
    plot$subplots$mainGraph <- .extraPost(plot$subplots$mainGraph, int, xPost, yPost)
  } else {
    plot <- plot + ggplot2::scale_x_continuous(name = xlab, breaks = xBreaks, limits = c(min(xPost), max(xPost)))
    plot <- .extraPost(plot, int, xPost, yPost)
  }
  
  postPlot$plotObject <- plot
  return()
}

# Plot: Forest plot
.bmaForestPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  forestContainer <- createJaspContainer(title = gettext("Forest Plot"))
  forestContainer$dependOn(c(.bmaDependencies, "studyLabels"))
  jaspResults[["forestContainer"]] <- forestContainer
  jaspResults[["forestContainer"]]$position <- 6
  
  # Get studylabels
  if(options[["studyLabels"]] != ""){
    studyLabels <- as.character(dataset[, .v(options[["studyLabels"]])])
  } else {
    studyLabels <- paste(gettext("Study"), 1:nrow(dataset))
  }
  
  # Scale the height and width of the plot
  height <- nrow(dataset) * 50
  width <- 500 + (nchar(max(studyLabels)) * 5)
  
  # title of plot based on observed/estimated 
  if(options$forestPlot == "plotForestBoth"){
    title <- gettext("Observed and estimated study effects")
    height <- nrow(dataset) * 50 * 1.5
  } else  if(options$forestPlot == "plotForestEstimated"){
    title <- gettext("Estimated study effects")
  } else  if(options$forestPlot == "plotForestObserved" || options$modelSpecification == "FE"){
    title <- gettext("Observed study effects")
  }
  
  # Check if ready    
  if(!ready){
    return()
  } 
  
  # Create empty plot
  if(options$checkForestPlot){
    forestPlot <- createJaspPlot(plot = NULL, title = title, height = height, width = width)
    # Fill plot
    forestPlot$dependOn(c("plotForestObserved", "plotForestEstimated", "plotForestBoth", 
                          "checkForestPlot", "ascendingForest", "labelForest",
                          "orderForest"))
    forestPlot$position <- 1
    .bmaFillForestPlot(forestPlot, jaspResults, dataset, options, studyLabels)
    # Add plot to container
    forestContainer[["forestPlot"]] <- forestPlot
  }
  
  if(options$plotCumForest){
    cumForestPlot <- createJaspPlot(plot = NULL, title = gettext("Cumulative forest plot"), height = height, width = width)
    cumForestPlot$dependOn("plotCumForest")
    cumForestPlot$position <- 2
    .bmaFillCumForest(cumForestPlot, jaspResults, dataset, options, studyLabels, .bmaDependencies)
    forestContainer[["cumForestPlot"]] <- cumForestPlot
  }
}

.bmaFillForestPlot <- function(forestPlot, jaspResults, dataset, options, studyLabels){
  # Get analysis results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  
  # Create effect size and standard error variable and make dataframe
  varES <- dataset[, .v(options[["effectSize"]])]
  
  if(all(unlist(options[["confidenceInterval"]]) != "") && !is.null(unlist(options[["confidenceInterval"]]))){
    lower <- dataset[, .v(options[["confidenceInterval"]][[1]][[1]])]
    upper <- dataset[, .v(options[["confidenceInterval"]][[1]][[2]])]
    varSE <- (upper - lower)/2/qnorm(0.975)
  }
  if(options[["standardError"]] != ""){
    varSE <- dataset[, .v(options[["standardError"]])]
  }
  
  # Assign weights for the observed point sizes
  weight <- 1/varSE^2
  weight_scaled <- ((4 - 1)*(weight - min(weight))) / (max(weight) - min(weight)) + 2
  
  # Assign weights for the estimated point sizes
  # Should be different for ordered analysis
  if(options[["modelSpecification"]] == "CRE"){
    se_estimated <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "se_mean"]
  } else {
    se_estimated <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "se_mean"]
  }
  
  weight_estimated <- 1 / se_estimated^2
  weight_estimated_scaled <- ((4 - 1) * (weight_estimated - min(weight_estimated))) / (
    max(weight_estimated) - min(weight_estimated)) + 2
  
  # Create text object for next to the observed points
  ci <- .95
  lower <- varES - qnorm((ci+1)/2) * varSE
  upper <- varES + qnorm((ci+1)/2) * varSE
  
  text_observed <- paste(sprintf('%.2f', varES),
                         " [",
                         sprintf('%.2f', lower),
                         ", ",
                         sprintf('%.2f', upper),
                         "]",
                         sep = "")
  
  # Get estimated points and CI's
  if(options$modelSpecification == "BMA" || options$modelSpecification == "RE" || options$modelSpecification == "CRE"){
    mean_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "mean"]
    lower_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "2.5%"]
    upper_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "97.5%"]
  } 
  # The estimates for the ordered analysis are not always saved
  if(options$modelSpecification == "CRE"){
    mean_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "mean"]
    lower_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "2.5%"]
    upper_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "97.5%"]
  }
  
  # Create text object for estimated points
  if(options$modelSpecification != "FE"){
    text_estimated <- paste(sprintf('%.2f', mean_estimates),
                            " [",
                            sprintf('%.2f', lower_estimates),
                            ", ",
                            sprintf('%.2f', upper_estimates),
                            "]",
                            sep = "")
  }
  
  # Make index for model diamond
  modelIndex <- .bmaGetModelName(options)
  
  
  yDiamond <- -0.5
  
  if (options$modelSpecification == "BMA" || options$modelSpecification == "CRE") {
    if (options$forestPlot == "plotForestBoth")
      yDiamond <- c(-0.5, -1.1, -1.7)
    else
      yDiamond <- c(-0.5, -1.5, -2.5)
  }
  
  # Create diamond for averaged or ordered model
  meanMain <- bmaResults[["bma"]]$estimates[modelIndex, "mean"]
  lowerMain <- bmaResults[["bma"]]$estimates[modelIndex, "2.5%"]
  upperMain <- bmaResults[["bma"]]$estimates[modelIndex, "97.5%"]
  if(modelIndex == "ordered"){
    yMain <- yDiamond[2]
  } else if(options$modelSpecification == "BMA"){
    yMain <- yDiamond[3]
  } else yMain <- -0.5
  
  d <- data.frame(x = c(lowerMain, meanMain,
                        upperMain, meanMain),
                  y = c(yMain, yMain + 0.25, 
                        yMain, yMain - 0.25))
  
  # Text object for next to model diamond
  textMain <- paste0(sprintf('%.2f', meanMain), " [",
                     sprintf('%.2f', lowerMain), ", ",
                     sprintf('%.2f', upperMain), "]")
  
  
  # Create diamond for fixed model
  meanFixed <- bmaResults[["bma"]]$estimates["fixed", "mean"]
  lowerFixed <- bmaResults[["bma"]]$estimates["fixed", "2.5%"]
  upperFixed <- bmaResults[["bma"]]$estimates["fixed", "97.5%"]
  yFixed <- yDiamond[1]
  
  d.fixed <- data.frame(x = c(lowerFixed, meanFixed,
                              upperFixed, meanFixed),
                        y = c(yFixed, yFixed + 0.25, 
                              yFixed, yFixed - 0.25))
  
  text_fixed <- paste0(sprintf('%.2f', meanFixed), " [",
                       sprintf('%.2f', lowerFixed), ", ",
                       sprintf('%.2f', upperFixed), "]")
  
  # Create diamond for random model
  meanRandom <- bmaResults[["bma"]]$estimates["random", "mean"]
  lowerRandom <- bmaResults[["bma"]]$estimates["random", "2.5%"]
  upperRandom <- bmaResults[["bma"]]$estimates["random", "97.5%"]
  if(options$modelSpecification == "RE"){
    yRandom <- -0.5
  } else if(options$modelSpecification == "BMA"){
    yRandom <- yDiamond[2]
  } else if(options$modelSpecification == "CRE"){
    yRandom <- yDiamond[3]
  } else yRandom <- 0
  
  d.random <- data.frame(x = c(lowerRandom, meanRandom,
                               upperRandom, meanRandom),
                         y = c(yRandom, yRandom + 0.25, 
                               yRandom, yRandom - 0.25))
  
  text_random <- paste0(sprintf('%.2f', meanRandom), " [",
                        sprintf('%.2f', lowerRandom), ", ",
                        sprintf('%.2f', upperRandom), "]")
  
  # Get y coordinates, labels, and text for diamonds
  if(options$modelSpecification == "BMA"){
    model <- c(gettext("Fixed effects"), gettext("Random effects"), gettext("Averaged"))
    textDiamond <- c(text_fixed, text_random, textMain)
  } else if(options$modelSpecification == "RE"){
    model <- gettext("Random effects")
    textDiamond <- text_random
  } else if(options$modelSpecification == "FE"){
    model <- gettext("Fixed effects")
    textDiamond <- text_fixed
  } else if(options$modelSpecification == "CRE"){
    model <- c(gettext("Fixed effects"), gettext("Ordered effects"), gettext("Random effects"))
    textDiamond <- c(text_fixed, textMain, text_random)
  }
  
  # Shape if only observed points
  shape <- 15
  
  df <- data.frame(effectSize = varES, y = length(varES):1,
                   studyLabels = studyLabels,
                   weight_scaled = weight_scaled, 
                   lower = lower, upper = upper,
                   text = text_observed)
  
  # Change objects if only estimated points
  if(options$forestPlot == "plotForestEstimated"){
    df <- data.frame(effectSize = mean_estimates, y = length(varES):1, 
                     studyLabels = studyLabels,
                     weight_scaled = weight_estimated_scaled,
                     lower = lower_estimates, upper = upper_estimates,
                     text = text_estimated)
    shape <- 19
  }
  
  # Get y values for the estimated points 
  yEst <- rev(seq(.6, length(varES) - .4, 1))
  
  ranked <- rank(df$effectSize, ties.method="first")
  if(options$orderForest == "ascendingForest"){
    ord <- (length(varES) + 1) - ranked
    df$y <- ord
    yEst <- yEst[ranked]
  } 
  
  if(options$orderForest == "descendingForest"){
    ord <- ranked
    df$y <- ord
    yEst <- yEst[(length(varES) + 1) - ranked]
  } 
  
  # Create plot
  plot <-  ggplot2::ggplot(df, ggplot2::aes(x = effectSize, y = y)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = df$lower, xmax = df$upper), height = .2) +
    ggplot2::geom_point(shape = shape, size = df$weight_scaled) +
    ggplot2::scale_y_continuous(breaks = c(df$y, yDiamond), labels = c(as.character(df$studyLabels), model),
                                sec.axis = ggplot2::sec_axis(~ ., breaks = c(df$y, yDiamond), labels = c(as.character(df$text), textDiamond)), expand = c(0, 0.5)) +
    ggplot2::xlab(bquote(paste(.(gettext("Effect size")), ~mu)))
  
  if(options$forestPlot == "plotForestBoth"){
    dfBoth <- data.frame(effectSize = c(varES, mean_estimates),
                         y = c(df$y, yEst),
                         studyLabels = c(studyLabels, studyLabels),
                         weight_scaled = c(weight_scaled, weight_estimated_scaled), 
                         lower = c(lower, lower_estimates), upper = c(upper, upper_estimates),
                         text = c(text_observed, text_estimated),
                         g = rep(c("Observed", "Estimated"), each = length(varES)))
    
    plot <-  ggplot2::ggplot(dfBoth, ggplot2::aes(x = effectSize, y = y)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
      ggplot2::geom_point(ggplot2::aes(shape = as.factor(dfBoth$g), colour = as.factor(dfBoth$g)), size = dfBoth$weight_scaled) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = dfBoth$lower, xmax = dfBoth$upper, colour = as.factor(dfBoth$g)), height = .1, show.legend = FALSE) +
      ggplot2::scale_y_continuous(breaks = c(df$y, yDiamond), labels = c(as.character(df$studyLabels), model),
                                  sec.axis = ggplot2::sec_axis(~ ., breaks = c(df$y, yEst, yDiamond), labels = c(text_observed, text_estimated, textDiamond)), expand = c(0, 0.5)) +
      ggplot2::scale_color_manual("", values = c("slategrey", "black"), labels = c(gettext("Estimated"), gettext("Observed"))) +
      ggplot2::scale_shape_manual("", values = c(16, 15)) +
      ggplot2::guides(shape = ggplot2::guide_legend(reverse=TRUE, override.aes = list(size=3)), colour = ggplot2::guide_legend(reverse=TRUE)) +
      ggplot2::theme(axis.text.y.right = ggplot2::element_text(colour = c(rep(c("black", "slategrey"), each = nrow(df)), rep("black", 3)))) +
      ggplot2::xlab(bquote(paste(.(gettext("Effect size")), ~mu)))
  }
  
  plot <- JASPgraphs::themeJasp(plot, yAxis = FALSE)
  
  # Add other theme elements (no y axis and aligning y axis labels)
  plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                axis.line.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_text(hjust = 0),
                                axis.text.y.right = ggplot2::element_text(hjust = 1))
  
  if(options$forestPlot == "plotForestBoth"){
    plot <- plot + ggplot2::theme(
      legend.position = c(1, 1),
      legend.justification=c(0, 0),
      plot.margin = ggplot2::unit(c(5, 1, 0.5, 0.5), "lines"),
      legend.title = ggplot2::element_blank()
    )
  }
  # Add the model diamond
  plot <- plot + ggplot2::geom_polygon(data = d, ggplot2::aes(x = x, y = y))
  
  # Add the diamonds of the other models for BMA or ordered analysis
  if(options$modelSpecification == "BMA" || options$modelSpecification == "CRE"){
    plot <- plot + ggplot2::geom_polygon(data = d.fixed, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(data = d.random, ggplot2::aes(x = x, y = y))
  }
  
  forestPlot$plotObject <- plot
  return()
}

.bmaFillCumForest <- function(cumForestPlot, jaspResults, dataset, options, studyLabels, .bmaDependencies){
  
  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)
  
  meanMain   <- rowResults$mean
  lowerMain  <- rowResults$lowerMain
  upperMain  <- rowResults$upperMain
  
  text <- paste(sprintf('%.2f', meanMain),
                " [",
                sprintf('%.2f', lowerMain),
                ", ",
                sprintf('%.2f', upperMain),
                "]",
                sep = "")
  
  studyLabels[2] <- paste(studyLabels[1], "\n &", studyLabels[2])
  studyLabels    <- paste("+", studyLabels)
  studyLabels[1] <- gettext("Prior")
  
  df <- data.frame(effectSize = meanMain, studyLabels = studyLabels, y = length(meanMain):1)
  
  plot <-  ggplot2::ggplot(df, ggplot2::aes(x = effectSize, y = y))+
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted")+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lowerMain, xmax = upperMain), height = .2) +
    ggplot2::geom_point(shape = 16, size = 4) +
    ggplot2::xlab(bquote(paste(.(gettext("Overall effect size")), ~mu))) + 
    ggplot2::scale_y_continuous(breaks = df$y, labels = studyLabels, expand = c(0, 0.5),
                                sec.axis = ggplot2::sec_axis(~ ., breaks = df$y, labels = text))
  
  plot <- JASPgraphs::themeJasp(plot, yAxis = FALSE)
  
  # Add other theme elements (no y axis and aligning y axis labels)
  plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                axis.line.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_text(hjust = 0),
                                axis.text.y.right = ggplot2::element_text(hjust = 1))
  
  cumForestPlot$plotObject <- plot
  return() 
}

.bmaSequentialPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  # Create empty plot
  seqContainer <- createJaspContainer(title = gettext("Sequential Analysis"))
  seqContainer$dependOn(.bmaDependencies)
  jaspResults[["seqContainer"]] <- seqContainer
  jaspResults[["seqContainer"]]$position <- 6
  
  # Check if ready
  if(!ready){
    return()
  }
  
  # Fill posterior plot effect size
  if(options$plotSequential){
    seqPlotES <- createJaspPlot(plot = NULL, title = gettext("Bayes factors effect size"), height = 400, width = 580)
    seqPlotES$dependOn(c("plotSequential", "BF")) 
    seqPlotES$position <- 1
    seqContainer[["seqPlotES"]] <- seqPlotES
    .bmaFillSeqPlot(seqPlotES, jaspResults, dataset, options, .bmaDependencies, type = "ES")
    if(!options$modelSpecification == "FE"){
      seqPlotSE <- createJaspPlot(plot = NULL, title = gettext("Bayes factors heterogeneity"), height = 400, width = 580)
      seqPlotSE$dependOn(c("plotSequential", "BF")) 
      seqPlotSE$position <- 2
      seqContainer[["seqPlotSE"]] <- seqPlotSE
      .bmaFillSeqPlot(seqPlotSE, jaspResults, dataset, options, .bmaDependencies, type = "SE")
    }
  }
  
  if(options$plotSeqPM){
    seqPMPlot <- createJaspPlot(plot = NULL, title = gettext("Posterior model probabilities"), height = 400, width = 580)
    seqPMPlot$dependOn("plotSeqPM")
    seqPMPlot$position <- 3
    .bmaFillSeqPM(seqPMPlot, jaspResults, dataset, options, .bmaDependencies)
    seqContainer[["seqPMPlot"]] <- seqPMPlot
  }
  
}

.bmaFillSeqPlot <- function(seqPlot, jaspResults, dataset, options, .bmaDependencies, type){
  
  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)
  if(type == "ES"){
    BFs <- rowResults$BFs
  } else if(type == "SE"){
    BFs <- rowResults$BFsHeterogeneity
    yName <- "BF[italic(rf)]"
  }
  BFs[1] <- 1  
  
  if(options$bayesFactorType == "BF01") {
    BFs    <- 1/BFs
    bfType <- "BF01"
    yName <- "BF[italic(fr)]"
  } else if(options$bayesFactorType == "LogBF10") {
    bfType <- "LogBF10"
  } else {
    bfType <- "BF10" 
    
    
  }
  
  pizzaTxt <- c("data | f H1", "data | r H1")
  bfSubscripts <-  c("BF[italic(rf)]", "BF[italic(fr)]")
  
  if(options$modelSpecification == "CRE"){
    pizzaTxt <- c("data | f H1", "data | o H1")
    bfSubscripts <-  c("BF[italic(of)]", "BF[italic(fo)]")
    if(type == "SE") yName <- "BF[italic(of)]"
    if(type == "SE" && options$bayesFactorType == "BF01") yName <- "BF[italic(fo)]"
  }
  
  if(any(is.infinite(BFs))){
    seqPlot$setError(gettext("Plotting failed: The Bayes factors contain infinity."))
    return()
  }
  
  df <- data.frame(x = 1:nrow(dataset), y = log(BFs))
  
  if(type == "ES"){
    
    plot <- JASPgraphs::PlotRobustnessSequential(dfLines = df,
                                                 xName = "Studies",
                                                 BF = BFs[nrow(dataset)],
                                                 bfType = bfType,
                                                 hasRightAxis = TRUE)
  } else if(type == "SE"){
    plot <- JASPgraphs::PlotRobustnessSequential(dfLines = df,
                                                 xName = "Studies",
                                                 BF = BFs[nrow(dataset)],
                                                 bfType = bfType,
                                                 bfSubscripts = bfSubscripts,
                                                 pizzaTxt = pizzaTxt,
                                                 hasRightAxis = TRUE,
                                                 yName = yName,
                                                 evidenceTxt  = bquote(paste(.(gettext("Evidence for r H"))[1], ":")),
                                                 arrowLabel  = c(bquote(.(gettext("Evidence~'for f'~H"))[1]),
                                                                 bquote(.(gettext("Evidence~'for r'~H"))[1]))
    )
  }
  
  
  seqPlot$plotObject <- plot
  return()
}

.bmaFillSeqPM <- function(seqPMPlot, jaspResults, dataset, options, .bmaDependencies){
  n     <- nrow(dataset)
  x     <- 0:n
  x     <- x[-2]
  dfPMP <- data.frame(prob = 0, g = rep(c("FE0", "FE1", "RE0", "RE1"), each = n))
  bmaResults     <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  pM    <- bmaResults[["models"]]$prior
  
  dfPMP[c(1, 1 + n, 1 + 2*n, 1 + 3*n), 1] <- pM
  
  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)
  
  for(i in 2:nrow(dataset)){
    posterior_models <- rowResults$posterior_models[[i]]
    dfPMP[c(i, i + n, i + 2*n, i + 3*n), 1] <- posterior_models
  }
  
  if(options[["modelSpecification"]] == "BMA" || options[["modelSpecification"]] == "CRE"){
    
    labels <- c(bquote(.(gettext("Fixed H"))[0]),bquote(.(gettext("Fixed H"))[1]),
                bquote(.(gettext("Random H"))[0]), bquote(.(gettext("Random H"))[1]))
    colorValues <- c("#fcae91ff", "#fcae91ff", "#009E73", "#009E73")
    linetypeValues <- rep("solid", 4)
    pointValues <- c(21, 19, 21, 19)
    lineValues <- c("dotted", "solid", "dotted", "solid")
    
  } else if(options[["modelSpecification"]] == "FE"){
    labels <- c(bquote(.(gettext("Fixed H"))[0]), bquote(.(gettext("Fixed H"))[1]))
    colorValues <- c("#fcae91ff", "#fcae91ff")
    linetypeValues <- rep("solid", 2)
    pointValues <- c(21, 19)
    lineValues <- c("dotted", "solid")
    dfPMP <- subset(dfPMP, dfPMP$g == "FE0" | dfPMP$g == "FE1")
    
  } else if(options[["modelSpecification"]] == "RE"){
    
    labels <- c(bquote(.(gettext("Random H"))[0]), bquote(.(gettext("Random H"))[1]))
    colorValues <- c("#009E73", "#009E73")
    linetypeValues <- rep("solid", 2)
    pointValues <- c(21, 19)
    lineValues <- c("dotted", "solid")
    dfPMP <- subset(dfPMP, dfPMP$g == "RE0" | dfPMP$g == "RE1")
    
  }
  
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(x)
  
  
  gridLines <- ggplot2::geom_segment(
    data        = data.frame(x = xBreaks[1L], y = c(0, 0.25, 0.5, 0.75, 1), xend = xBreaks[length(xBreaks)]),
    mapping     = ggplot2::aes(x = x, y = y, xend = xend, yend = y),
    inherit.aes = FALSE,
    colour      = rep("gray", 5),
    linetype    = rep("dashed", 5),
    size        = 0.85)
  
  df <- data.frame(x = x, y = dfPMP$prob, g = dfPMP$g)
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = g, linetype = g)) +
    gridLines + 
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_y_continuous(limits = c(0,1.05), breaks = c(0, .25, .5, .75, 1)) +
    ggplot2::scale_x_continuous(breaks = xBreaks) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme(legend.spacing.x = ggplot2::unit(0.35, 'cm')) +
    ggplot2::labs(x = gettext("Studies"), y = gettext("Posterior model \n probability")) + 
    ggplot2::scale_colour_manual(name = "",
                                 labels = labels,
                                 values = colorValues) +
    ggplot2::scale_linetype_manual(name = "",
                                   labels = labels,
                                   values = linetypeValues)
  
  
  
  if(nrow(dataset) < 40) {
    plot <- plot + 
      ggplot2::geom_point(ggplot2::aes(shape = dfPMP$g), size = 3, fill = "white") +
      ggplot2::scale_shape_manual(name = "", values = pointValues, labels = labels)
  } else {
    plot <- plot + 
      ggplot2::scale_linetype_manual(name = "", values = lineValues, labels = labels)
  }
  
  plot <- JASPgraphs::themeJasp(plot, legend.position = "top")
  
  
  seqPMPlot$plotObject <- plot
  return()                              
}

