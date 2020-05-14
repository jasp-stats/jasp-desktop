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

RegressionLinearBayesian <- function(jaspResults, dataset = NULL, options) {
  ready <- length(options$modelTerms) > 0 && options$dependent != ""
  
  if (ready) {
    dataset <- .basregReadData(dataset, options)
    .basregCheckErrors(dataset, options)
  }
  
  basregContainer <- .basregGetModelContainer(jaspResults, position = 1)
  basregModel     <- .basregGetModel(basregContainer, dataset, options, ready)

  if (is.null(basregContainer[["modelComparisonTable"]]))
    .basregTableModelComparison(basregContainer, basregModel, options, position = 11)
  
  if (options$postSummaryTable || options$postSummaryPlot)
    postSumContainer <- .basregGetPosteriorSummaryContainer(basregContainer, position = 12)
    
  if (options$postSummaryTable || options$postSummaryPlot || options$plotCoefficientsPosterior)
    postSumModel <- .basregGetPosteriorSummary(basregContainer, basregModel, dataset, options, ready)
  
  if (options$postSummaryTable && is.null(basregContainer[["postSumContainer"]][["postSumTable"]]))
    .basregTablePosteriorSummary(postSumContainer, postSumModel, basregModel, options, position = 121)
  
  if (options$postSummaryPlot && is.null(basregContainer[["postSumContainer"]][["postSumPlot"]]))
    .basregPlotPosteriorSummary(postSumContainer, postSumModel, options, position = 122)
  
  if (options$plotLogPosteriorOdds && is.null(basregContainer[["logPosteriorOddsPlot"]]))
    .basregPlotPosteriorLogOdds(basregContainer, basregModel, options, position = 13)
    
  if (options$plotResidualsVsFitted && is.null(basregContainer[["ResidualsVsFittedPlot"]]))
    .basregPlotResidualsVsFitted(basregContainer, basregModel, position = 14)
  
  if (options$plotModelProbabilities && is.null(basregContainer[["modelProbabilitiesPlot"]]))
    .basregPlotModelProbabilities(basregContainer, basregModel, position = 15)
  
  if (options$plotModelComplexity && is.null(basregContainer[["modelComplexityPlot"]]))
    .basregPlotModelComplexity(basregContainer, basregModel, position = 16)
    
  if (options$plotInclusionProbabilities && is.null(basregContainer[["inclusionProbabilitiesPlot"]]))
    .basregPlotInclusionProbabilities(basregContainer, basregModel, position = 17)
  
  if (options$plotQQplot && is.null(basregContainer[["qqPlot"]]))
    .basregPlotQQ(basregContainer, basregModel, position = 18)
  
  if (options$plotCoefficientsPosterior && is.null(basregContainer[["postDistContainer"]]))
    .basregPlotsPosteriorDistribution(basregContainer, postSumModel, basregModel, options, position = 19)
  
  if (options$descriptives && is.null(jaspResults[["descriptivesTable"]]))
    .basregTableDescriptives(jaspResults, dataset, options, ready, position = 2)
}

.basregReadData <- function(dataset, options) {
  if (!is.null(dataset)) 
    return(dataset)
  
  vars <- c(options$dependent, unlist(options$covariates))
  if (options$wlsWeights != "") {
    vars <- c(vars, options$wlsWeights)
  }
  
  dataset <- .readDataSetToEnd(columns = vars, exclude.na.listwise = vars)
  
  return(dataset)
}

.basregCheckErrors <- function(dataset, options) {
  customChecks <- list( #TODO: add these to hasErrors officially
    function() {
      nuisanceTerms <- sapply(options$modelTerms, function(term) term$isNuisance)
      if (sum(nuisanceTerms) == length(options$modelTerms)) {
        return(gettext("All effects are specified as nuisance"))
      }
    },
    function() {
      if (options$wlsWeights != "") {
        weightsVar <- options$wlsWeights
        min.weight <- min(dataset[[ .v(weightsVar) ]])
        if (min.weight <= 0) {
          return(gettext("There are nonpositive weights"))
        }
      }
    })
  
  .hasErrors(dataset = dataset,
             type = c("infinity", "observations", "variance", "modelInteractions"), custom = customChecks,
             infinity.target = c(options$covariates, options$dependent, options$wlsWeight),
             observations.target = options$dependent, observations.amount = paste("<", length(options$modelTerms) + 1),
             variance.target = c(options$covariates, options$dependent),
             modelInteractions.modelTerms = options$modelTerms,
             exitAnalysisIfErrors = TRUE)
}

.basregGetModelContainer <- function(jaspResults, position) {
  if (is.null(jaspResults[["basreg"]])) {
    basregContainer <- createJaspContainer()
    basregContainer$position <- position
    basregContainer$dependOn(c(
      "dependent", "covariates", "wlsWeights", "modelTerms",
      "priorRegressionCoefficients", "alpha", "rScale",
      "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
      "wilsonParamLambda", "castilloParamU",
      "samplingMethod", "iterationsMCMC", "numberOfModels", "seed", "setSeed"
    ))
    jaspResults[["basreg"]] <- basregContainer
  }
  return(jaspResults[["basreg"]])
}

.basregGetPosteriorSummaryContainer <- function(basregContainer, position) {
  if (is.null(basregContainer[["postSumContainer"]])) {
    postSumContainer <- createJaspContainer(gettext("Posterior Summary"))
    postSumContainer$position <- position
    postSumContainer$dependOn(c(
      "summaryType", "posteriorSummaryPlotCredibleIntervalValue", "nSimForCRI", "seed", "setSeed"
    ))
    basregContainer[["postSumContainer"]] <- postSumContainer
  }
  return(basregContainer[["postSumContainer"]])
}

.basregTableModelComparison <- function(basregContainer, basregModel, options, position) {
  if(options[['dependent']] == "")
    modelComparisonTable <- createJaspTable(gettext("Model Comparison"))
  else 
    modelComparisonTable <- createJaspTable(gettextf("Model Comparison - %s", options[['dependent']]))
  
  modelComparisonTable$position <- position
  modelComparisonTable$dependOn(c(
    "bayesFactorType", "bayesFactorOrder", "shownModels", "numShownModels"
  ))
  
  modelComparisonTable$addCitation(c(
    "Clyde, M. A. (2018). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software].",
    "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101.",
    "Consonni, G., Fouskakis, D., Liseo, B., & Ntzoufras, I. (2018). Prior Distributions for Objective Bayesian Analysis. Bayesian Analysis, 13, 627-679.",
    "Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g Priors for Bayesian Variable Selection. Journal of the American Statistical Association, 103, 410-423."
  ))
  
  if (options$modelPrior == "Wilson") {
    modelComparisonTable$addCitation(
      "Wilson, M. A., Iversen, E. S., Clyde, M. A., Schmidler, S. C., & Schildkraut, J. M. (2010). Bayesian model search and multilevel inference for SNP association studies. The annals of applied statistics, 4(3), 1342."
    )
    modelComparisonTable$addFootnote(gettextf(
"The Wilson model prior corresponds to a beta binomial prior with %1$s = 1 and %2$s = p * %3$s and \
corresponds to an approximate penalization equal to log(%3$s + 1) in log-odds scale for each additional \
covariate added to the model (Consonni et al., 2018; Wilson et al., 2010).", "\u03B1", "\u03B2", "\u03BB"))
  } else if (options$modelPrior == "Castillo") {
    modelComparisonTable$addCitation(
      "Castillo, I., Schmidt-Hieber, J., & Van der Vaart, A. (2015). Bayesian linear regression with sparse priors. The Annals of Statistics, 43(5), 1986-2018."
    )
    modelComparisonTable$addFootnote(gettextf(
      "The Castillo model prior corresponds to a beta binomial prior with %1$s = 1 and %2$s = p^u and is suitable \
for sparse regression when there are more covariates than observations (Castillo et al., 2015).", "\u03B1", "\u03B2"))
  }

  if (options$bayesFactorType == "BF10")      bfTitle <- gettext("BF<sub>10</sub>")
  else if (options$bayesFactorType == "BF01") bfTitle <- gettext("BF<sub>01</sub>")
  else                                        bfTitle <- gettext("Log(BF<sub>10</sub>)")
  
  modelComparisonTable$addColumnInfo(name = "Models",         type = "string", title = gettext("Models"))
  modelComparisonTable$addColumnInfo(name = "priorProbModel", type = "number", title = gettext("P(M)"))
  modelComparisonTable$addColumnInfo(name = "postProbModel",  type = "number", title = gettext("P(M|data)"))
  modelComparisonTable$addColumnInfo(name = "BFM",            type = "number", title = gettext("BF<sub>M</sub>"))
  modelComparisonTable$addColumnInfo(name = "BF",             type = "number", title = bfTitle)
  modelComparisonTable$addColumnInfo(name = "R2",             type = "number", title = gettext("R\u00B2"), format = "dp:3")
  
  if (!is.null(basregModel)) {
    generalNote <- NULL
    if (sum(basregModel$nuisanceTerms) > 0)
      generalNote <- gettextf("All models include %s.", paste(names(which(basregModel$nuisanceTerms)), collapse = ", "))
    
    if (options$shownModels == "limited" && options$numShownModels < length(basregModel$which)) {
      if (is.null(generalNote)) {
        s1 <- ""
      } else {
        s1 <- paste0(generalNote, " ") # hopefully all languages want a " " after a full stop.
      }
      generalNote <- gettextf("%sTable displays only a subset of models; to see all models, select \"No\" under \"Limit No. Models Shown\".", s1)
    }
    
    if (!is.null(generalNote))
      modelComparisonTable$addFootnote(message = generalNote)
    
    .basregFillTableModelComparison(modelComparisonTable, basregModel, options)
  }
  
  basregContainer[["modelComparisonTable"]] <- modelComparisonTable
}

.basregFillTableModelComparison <- function(modelComparisonTable, basregModel, options) {
  nModels <- length(basregModel$which)
  if (options$shownModels == "limited" && options$numShownModels < nModels)
    nModels <- options$numShownModels
  
  allModelIndices <- .basregGetModelOrder(basregModel, options$bayesFactorOrder)
  modelIndices <- allModelIndices[1:nModels]
  
  models <- basregModel$which[modelIndices]
  modelNames <- .basregGetModelNames(basregModel)[modelIndices]
  
  # get the Bayes factors for the models
  logMarg <- basregModel$logmarg[modelIndices]
  if (options$bayesFactorType == "BF10")
    bayesFactors <- exp(logMarg - logMarg[1])
  else if (options$bayesFactorType == "BF01")
    bayesFactors <- exp(logMarg[1] - logMarg)
  else # logBF10
    bayesFactors <- logMarg - logMarg[1]
  
  # calculate the BFM for the models
  postProbs <- basregModel$postprobs[modelIndices]
  priorProbs <- basregModel$priorprobs[modelIndices]
  valid <- is.finite(postProbs)
  BFM <- numeric(length(postProbs))
  BFM[valid] <- (postProbs[valid] / (1 - postProbs[valid])) / (priorProbs[valid] / (1 - priorProbs[valid]))
  BFM[!valid] <- NA

  modelComparisonTable$setData(list(
    Models = modelNames,
    BF = bayesFactors,
    BFM = BFM,
    postProbModel = postProbs,
    R2 = basregModel$R2[modelIndices],
    priorProbModel = priorProbs
  ))
}

.basregGetModelOrder <- function(basregModel, order) {  
  # ordered indices based on posterior probabilities of the models
  sortedModels <- order(basregModel$postprobs, decreasing = TRUE)
  
  if (order == "nullModelTop") {
    indexNullModel <- which(sortedModels == 1)
    sortedModels <- c(1, sortedModels[-indexNullModel])
  }
  
  return(sortedModels)
}

.basregGetModelNames <- function(basregModel) {
  # null model name
  nuisanceTerms <- basregModel$nuisanceTerms
  nullModel <- gettext("Null model")
  if (sum(nuisanceTerms) > 0) {
    nullModel <- gettextf("Null model (incl. %s)", paste(names(which(nuisanceTerms)), collapse = ", "))
  }
  
  models <- basregModel$which
  allModelsVisited <- any(lengths(models) == 0) # analysis will crash if TRUE
  modelNames <- character(length(models))
  
  # generate all model names
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (length(model) == 1) { # only has intercept
      modelNames[i] <- nullModel
      next
    }
    model <- model[-1]  # pop the intercept term (not in the nuisance vector)
    nuisanceInModel <- sum(nuisanceTerms[model])
    if (nuisanceInModel == length(model)) { # found the null model
      modelNames[i] <- nullModel
    } else {
      nonNuisance <- which(!nuisanceTerms[model])
      modelNames[i] <- paste(names(nonNuisance), collapse = " + ")
    }
  }
  
  return(modelNames)
}

.basregTablePosteriorSummary <- function(postSumContainer, postSumModel, basregModel, options, position) {
  postSumTable <- createJaspTable(title = gettext("Posterior Summaries of Coefficients"))
  postSumTable$position <- position
  postSumTable$dependOn(c("postSummaryTable", "effectsType", "bayesFactorType"))
  
  bfTitle <- gettext("BF<sub>inclusion</sub>")
  if (options$bayesFactorType == "LogBF10")
    bfTitle <- gettext("Log(BF<sub>inclusion</sub>)")
  
  overtitle <- gettextf("%s%% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
  postSumTable$addColumnInfo(name = "coefficient", title = gettext("Coefficient"),   type = "string")
  postSumTable$addColumnInfo(name = "mean",        title = gettext("Mean"),          type = "number")
  postSumTable$addColumnInfo(name = "sd",          title = gettext("SD"),            type = "number")
  postSumTable$addColumnInfo(name = "pInclprior",  title = gettext("P(incl)"),       type = "number")
  postSumTable$addColumnInfo(name = "pIncl",       title = gettext("P(incl|data)"),  type = "number")
  postSumTable$addColumnInfo(name = "BFincl",      title = bfTitle,                  type = "number")
  postSumTable$addColumnInfo(name = "lowerCri",    title = gettext("Lower"),         type = "number", overtitle = overtitle)
  postSumTable$addColumnInfo(name = "upperCri",    title = gettext("Upper"),         type = "number", overtitle = overtitle)
  
  if (!is.null(basregModel) && !is.null(postSumModel)) {
    footnote <- postSumModel[["footnotes"]]
    if (!is.null(footnote))
      postSumTable$addFootnote(footnote, symbol = gettext("<em>Warning.</em>"))
    
    .basregFillTablePosteriorSummary(postSumTable, postSumModel, basregModel, options)
  }
  
  postSumContainer[["postSumTable"]] <- postSumTable
}

.basregFillTablePosteriorSummary <- function(postSumTable, postSumModel, basregModel, options) {
  if (options[["effectsType"]] == "allModels") {

    probne0 <- basregModel[["probne0"]]
    priorProbs <- basregModel[["priorprobsPredictor"]]
    BFinclusion <- basregModel[["BFinclusion"]]

  } else {

    priorModelProbs <- basregModel$priorprobs
    postModelProbs  <- basregModel$postprobs
    terms <- attr(basregModel$terms, "factors")[-1, , drop = FALSE]
    rownames(terms) <- .unvf(rownames(terms))
    colnames(terms) <- .unvf(colnames(terms))
    inclMat <- BAS:::list2matrix.which(basregModel)[, -1, drop = FALSE]
    terms <- rbind(terms, matrix(FALSE, nrow = ncol(terms) - nrow(terms), ncol = ncol(terms)))
    diag(terms) <- FALSE
    storage.mode(terms) <- "logical"
    storage.mode(inclMat) <- "logical"
    rownames(terms) <- colnames(terms)
    effectNames <- colnames(terms)

    tmp <- .BANOVAcomputMatchedInclusion(
      effectNames, inclMat, terms, priorModelProbs, postModelProbs
    )
    probne0     <- c(1 ,tmp[["postInclProb"]])
    priorProbs  <- c(1, tmp[["priorInclProb"]])
    BFinclusion <- c(1, tmp[["bfIncl"]])

  }
  
  # show BFinclusion for nuisance predictors as 1, rather than NaN
  priorInclIs1 <- is.nan(BFinclusion) | abs(1 - priorProbs) <= sqrt(.Machine$double.eps)
  BFinclusion[priorInclIs1] <- 1
  
  if (options$bayesFactorType == "LogBF10")
    BFinclusion <- log(BFinclusion)
  
  nModels <- basregModel[["n.models"]]
  coef <- postSumModel[["coef"]]
  coefficients <- postSumModel[["coefficients"]]
  loopIdx <- postSumModel[["loopIdx"]]
  confInt <- postSumModel[["conf95"]]
  
  topm <- order(-basregModel$postprobs)[1:nModels]
  mostComplex <- which.max(lengths(basregModel$which)[topm])
  
  for (i in loopIdx) {
    coefficient <- coefficients[i]
    pIncl <- probne0[i]
    pInclprior <- priorProbs[i]
    BFincl <- BFinclusion[i]
    
    if (options$summaryType == "complex") {
      mean <- unname(coef$conditionalmeans[mostComplex, i])
      sd <- unname(coef$conditionalsd[mostComplex, i])
    } else {
      mean <- coef$postmean[i]
      sd <- coef$postsd[i]
    }
    lowerCri <- confInt[i, 1]
    upperCri <- confInt[i, 2]
    
    postSumTable$addRows(
      list(coefficient = coefficient, mean = mean, sd = sd, pIncl = pIncl,
           pInclprior = pInclprior, BFincl = BFincl, lowerCri = lowerCri, upperCri = upperCri
      )
    )
  }
}

.basregPlotPosteriorSummary <- function(postSumContainer, postSumModel, options, position) {
  title <- gettextf("Posterior Coefficients with %s%% Credible Interval",
                   format(100 * options$posteriorSummaryPlotCredibleIntervalValue, digits = 3))
  postSumPlot <- createJaspPlot(title = title, width = 530, height = 400)
  postSumPlot$position <- position
  postSumPlot$dependOn(c("postSummaryPlot", "omitIntercept"))

  postSumContainer[["postSumPlot"]] <- postSumPlot
  
  if (!is.null(postSumModel))
    .basregFillPlotPosteriorSummary(postSumPlot, postSumModel, options)
}

.basregFillPlotPosteriorSummary <- function(postSumPlot, postSumModel, options) {
  coef <- postSumModel[["coef"]]
  confInt <- postSumModel[["conf95"]]
  loopIdx <- postSumModel[["loopIdx"]]
  coefficients <- postSumModel[["coefficients"]]
  coefficients <- .basregReplaceInteractionUnicodeSymbol(coefficients)
  
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
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    postSumPlot$setError(errorMessage)
  } else {
    postSumPlot$plotObject <- p
  }
}

.basregPlotPosteriorLogOdds <- function(basregContainer, basregModel, options, position) {
  postLogOddsPlot <- createJaspPlot(title = gettext("Posterior Log Odds"), width = 530, height = 400)
  postLogOddsPlot$position <- position
  postLogOddsPlot$dependOn("plotLogPosteriorOdds")
  
  basregContainer[["logPosteriorOddsPlot"]] <- postLogOddsPlot

  if (options$samplingMethod == "MCMC") {
    postLogOddsPlot$setError(gettext("Cannot display Posterior Log Odds when sampling method is MCMC."))
    return(postLogOddsPlot)
  }
  
  if (!is.null(basregModel))
    .basregFillPlotPosteriorLogOdds(postLogOddsPlot, basregModel)
}

.basregFillPlotPosteriorLogOdds <- function(postLogOddsPlot, basregModel) {
  basregModel$namesx <- .basregReplaceInteractionUnicodeSymbol(basregModel$namesx)
  postLogOddsPlot$plotObject <- function() { 
    BAS:::image.bas(basregModel, rotate = FALSE) 
  }
}

# to be adapted and spliced into the logodds plot:
# .plotImage.basreg <- function(x, top.models = 20, intensity = TRUE, prob = TRUE,
#                               log = TRUE, rotate = TRUE, color = "rainbow", subset = NULL,
#                               offset = 0.75, digits = 3, vlas = 2, plas = 0, rlas = 0,
#                               ...) {
#   # code from BAS:::image.bas
#   postprob = x$postprobs
#   top.models = min(top.models, x$n.models)
#   best = order(-x$postprobs)[1:top.models]
#   postprob = postprob[best]/sum(postprob[best])
#   which.mat <- BAS:::list2matrix.which(x, best)
#   nvar <- ncol(which.mat)
#   if (is.null(subset))
#     subset = 1:nvar
#   which.mat = which.mat[, subset, drop = FALSE]
#   nvar = ncol(which.mat)
#   namesx = x$namesx[subset]
#   scale = postprob
#   prob.lab = "Posterior Probability"
#   if (log) {
#     scale = log(postprob) - min(log(postprob))
#     prob.lab = "Log Posterior Odds"
#   }
#   if (intensity)
#     which.mat = sweep(which.mat, 1, scale + offset, "*")
#   if (rotate)
#     scale = rev(scale)
#   if (prob)
#     m.scale = cumsum(c(0, scale))
#   else m.scale = seq(0, top.models)
#   mat = (m.scale[-1] + m.scale[-(top.models + 1)])/2
#   colors = switch(color,
#                   rainbow = c("black", rainbow(top.models +1, start = 0.75, end = 0.05)),
#                   blackandwhite = gray(seq(0, 1, length = top.models)))
  
#   # end of code from BAS:::image.bas
  
#   w <- diff(mat)
#   w <- c(w[1], w)
  
#   dfHeat <- data.frame(
#     x = rep(rev(mat), ncol(which.mat)),
#     y = rep(1:nvar, each = nrow(which.mat)),
#     z = c(which.mat[, nvar:1]),
#     zCat = 1* (abs(c(which.mat[, nvar:1])) > .Machine$double.eps),
#     w = rev(w)
#   )
#   dfHeat$x <- dfHeat$x - dfHeat$w / 2
#   # above line is required since width expands half of widht left and half of width right
#   # check code below to verify
#   # cbind(dfHeat$x - dfHeat$w / 2, dfHeat$x + dfHeat$w / 2)
#   nr <- nrow(dfHeat)
#   dfLines <- data.frame(
#     x = rep(c(mat - w, mat[length(mat)]), each = 2),
#     y = rep(c(.5, nvar+.5), length(mat)+1),
#     g = factor(rep(1:(length(mat)+1), each = 2))
#   )
  
#   discrete <- TRUE
#   if (discrete) {
#     show.legend <- FALSE
#     colors[colors != "black"] <- "green"
#     colors[colors == "black"] <- "white"
#     mapping = ggplot2::aes(x = x, y = y, fill = zCat, width = w)
#   } else {
#     show.legend <- TRUE
#     mapping = ggplot2::aes(x = x, y = y, fill = z, width = w)
#   }
  
#   g <- JASPgraphs::drawHeatmap(dat = dfHeat, show.legend = show.legend, fillColor = colors,
#                                mapping = mapping,
#                                geom = "tile")
#   g
#   xBreaks <- mat - w/2
#   g <- JASPgraphs::drawAxis(graph = g, xName = prob.lab, xBreaks = xBreaks, xLabels = round(scale, digits = digits),
#                             yName = "", yBreaks = 1:nvar, yLabels = namesx, xLimits = NULL, yLimits = NULL,
#                             secondaryXaxis = list(~.,name = "Model Rank", breaks = xBreaks, labels = top.models:1),
#                             xTrans = scales::reverse_trans())
#   g <- JASPgraphs::drawLines(g, dat = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g),
#                              color = "gray50", alpha = .7, size = 2)
#   g <- JASPgraphs::themeJasp(graph = g, legend.position = "right", axisTickLength = 0,
#                              bty = "o")
  
#   # this plot needs some additional treatment
#   unit <- JASPgraphs::graphOptions("axisTickLengthUnit")
#   fillLg <- ggplot2::guide_colorbar(title = "", default.unit = unit,
#                                     barheight = 5, barwidth = 1)
#   g <- g + ggplot2::theme(
#     axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(0, 0, .5, 0, unit)),
#     axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(.5, 0, 0, 0, unit))
#   ) + ggplot2::guides(fill = fillLg)
  
#   return(g)
# }

.basregPlotResidualsVsFitted <- function(basregContainer, basregModel, position) {
  residualsVsFittedPlot <- createJaspPlot(title = gettext("Residuals vs Fitted"), width = 530, height = 400)
  residualsVsFittedPlot$position <- position
  residualsVsFittedPlot$dependOn("plotResidualsVsFitted")
  
  basregContainer[["ResidualsVsFittedPlot"]] <- residualsVsFittedPlot
  
  if (!is.null(basregModel))
    .basregFillPlotResidualsVsFitted(residualsVsFittedPlot, basregModel)
  
}

.basregFillPlotResidualsVsFitted <- function(residualsVsFittedPlot, basregModel) {
  x <- fitted(basregModel, estimator = "BMA")
  y <- basregModel$Y - x
  dfPoints <- data.frame(
    x = x,
    y = y
  )
  
  p <- try({
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
    g <- JASPgraphs::drawAxis()
    g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
    g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85)
    g <- JASPgraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7) +
      ggplot2::ylab("Residuals") +
      ggplot2::scale_x_continuous(name = gettext("Predictions under BMA"), breaks = xBreaks, limits = range(xBreaks))
    JASPgraphs::themeJasp(g)
  })

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    residualsVsFittedPlot$setError(errorMessage)
  } else {
    residualsVsFittedPlot$plotObject <- p
  }
}

.basregPlotModelProbabilities <- function(basregContainer, basregModel, position) {
  modelProbabilitiesPlot <- createJaspPlot(title = gettext("Model Probabilities"), width = 530, height = 400)
  modelProbabilitiesPlot$position <- position
  modelProbabilitiesPlot$dependOn("plotModelProbabilities")

  basregContainer[["modelProbabilitiesPlot"]] <- modelProbabilitiesPlot
  
  if (!is.null(basregModel))
    .basregFillPlotModelProbabilities(modelProbabilitiesPlot, basregModel)
}

.basregFillPlotModelProbabilities <- function(modelProbabilitiesPlot, basregModel) {
  cum.prob = cumsum(basregModel$postprobs)
  m.index = 1:basregModel$n.models
  
  dfPoints <- data.frame(
    x = m.index,
    y = cum.prob
  )
  
  p <- try({
    xBreaks <- round(seq(1, basregModel$n.models, length.out = min(5, basregModel$n.models)))
    g <- JASPgraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
    g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = gettext("Cumulative Probability"), limits = 0:1) +
      ggplot2::scale_x_continuous(name = gettext("Model Search Order"), breaks = xBreaks)
    JASPgraphs::themeJasp(g)
  })
  
  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    modelProbabilitiesPlot$setError(errorMessage)
  } else {
    modelProbabilitiesPlot$plotObject <- p
  }
}

.basregPlotModelComplexity <- function(basregContainer, basregModel, position) {
  modelComplexityPlot <- createJaspPlot(title = gettext("Log(P(data|M)) vs. Model Size"), width = 530, height = 400)
  modelComplexityPlot$position <- position
  modelComplexityPlot$dependOn("plotModelComplexity")

  basregContainer[["modelComplexityPlot"]] <- modelComplexityPlot
  
  if (!is.null(basregModel))
    .basregFillPlotModelComplexity(modelComplexityPlot, basregModel)
}

.basregFillPlotModelComplexity <- function(modelComplexityPlot, basregModel) {
  logmarg = basregModel$logmarg
  dim = basregModel$size
  
  dfPoints <- data.frame(
    x = dim,
    y = logmarg
  )
  
  p <- try({
    # gonna assume here that dim (the number of parameters) is always an integer
    xBreaks <- unique(round(pretty(dim)))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(logmarg), eps.correct = 2)
    g <- JASPgraphs::drawPoints(dat = dfPoints, size = 4) +
      ggplot2::scale_y_continuous(name = gettext("Log(P(data|M))"),  breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = gettext("Model Dimension"), breaks = xBreaks)
    JASPgraphs::themeJasp(g)
  })
  
  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    modelComplexityPlot$setError(errorMessage)
  } else {
    modelComplexityPlot$plotObject <- p
  }
}

.basregPlotInclusionProbabilities <- function(basregContainer, basregModel, position) {
  inclusionProbabilitiesPlot <- createJaspPlot(title = gettext("Inclusion Probabilities"), width = 700, height = 400)
  inclusionProbabilitiesPlot$position <- position
  inclusionProbabilitiesPlot$dependOn("plotInclusionProbabilities")

  basregContainer[["inclusionProbabilitiesPlot"]] <- inclusionProbabilitiesPlot
  
  if (!is.null(basregModel))
    .basregFillPlotInclusionProbabilities(inclusionProbabilitiesPlot, basregModel)
}

.basregFillPlotInclusionProbabilities <- function(inclusionProbabilitiesPlot, basregModel) {
  probne0 <- basregModel$probne0[-1]
  variables <- basregModel$namesx[-1] # 1:basregModel$n.vars
  variables <- .basregReplaceInteractionUnicodeSymbol(variables)
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
  
  p <- try({
    yLimits <- c(0, base * ceiling(max(c(priorProb, probne0)) / base))
    yBreaks <- seq(yLimits[1], yLimits[2], length.out = 5)
    
    g <- ggplot2::ggplot(data = dfBar, mapping = ggplot2::aes(x = x, y = y)) + 
      ggplot2::geom_bar(width = width, stat = "identity", fill = "gray80", show.legend = FALSE)
    g <- JASPgraphs::drawLines(g, dat = dfLine,
                               mapping = ggplot2::aes(x = x, y = y, group = g, linetype = g0), show.legend = TRUE) +
      ggplot2::scale_y_continuous(gettext("Marginal Inclusion Probability"), breaks = yBreaks, limits = yLimits) +
      ggplot2::xlab("") +
      ggplot2::scale_linetype_manual(name = "", values = 2, labels = gettext("Prior\nInclusion\nProbabilities"))
    
    JASPgraphs::themeJasp(g, horizontal = TRUE, legend.position = "right") +
      ggplot2::theme(
        legend.title = ggplot2::element_text(size = .8*JASPgraphs::graphOptions("fontsize"))
      )
  })
  
  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    inclusionProbabilitiesPlot$setError(errorMessage)
  } else {
    inclusionProbabilitiesPlot$plotObject <- p
  }
}

.basregPlotQQ <- function(basregContainer, basregModel, position) {
  qqPlot <- createJaspPlot(title = gettext("Q-Q Plot"), width = 700, height = 400)
  qqPlot$position <- position
  qqPlot$dependOn("plotQQplot")

  basregContainer[["qqPlot"]] <- qqPlot

  if (!is.null(basregModel))
    .basregFillPlotQQ(qqPlot, basregModel)
}

.basregFillPlotQQ <- function(qqPlot, basregModel) {
  p <- try({
    x <- fitted(basregModel, estimator = "BMA")
    y <- basregModel$Y - x
    JASPgraphs::plotQQnorm(y)
  })
  
  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    qqPlot$setError(errorMessage)
  } else {
    qqPlot$plotObject <- p
  }
}

.basregPlotsPosteriorDistribution <- function(basregContainer, postSumModel, basregModel, options, position) {
  postDistContainer <- createJaspContainer(gettext("Marginal Posterior Distributions")) #TODO: check if this name is ok
  postDistContainer$position <- position
  postDistContainer$dependOn(c(
    "plotCoefficientsPosterior", "summaryType", 
    "posteriorSummaryPlotCredibleIntervalValue", "nSimForCRI", "seed", "setSeed"
  )) #TODO: check if dependencies are correct for this item: was probably wrong in release

  .basregInsertPosteriorDistributionPlots("placeholders", postDistContainer, plotNames, options, basregModel)
    
  if (!is.null(basregModel) && !is.null(postSumModel))
      .basregInsertPosteriorDistributionPlots("fill", postDistContainer, plotNames, options, basregModel, postSumModel)
  
  basregContainer[["postDistContainer"]] <- postDistContainer
}

.basregInsertPosteriorDistributionPlots <- function(type, postDistContainer, plotNames, options, basregModel = NULL, postSumModel = NULL) {
  if (is.null(basregModel)) {
    plotNames <- "Intercept"
    isNuisance <- setNames(FALSE, "Intercept")
  } else {
    plotNames <- .basregReplaceInteractionUnicodeSymbol(basregModel$namesx)
    isNuisance <- basregModel[["nuisanceTerms"]]
    names(isNuisance) <- .basregReplaceInteractionUnicodeSymbol(names(isNuisance))
  }
  
  for (plotName in plotNames) {
    if (plotName != "Intercept" && isNuisance[which(names(isNuisance) == plotName)])
      next
      
    if (type == "placeholders")
      postDistContainer[[plotName]] <- createJaspPlot(title = plotName, width = 530, height = 400)
    else
      .basregFillPlotPosteriorDistribution(postDistContainer[[plotName]], which(plotNames == plotName), postSumModel)
      
  }
}

.basregFillPlotPosteriorDistribution <- function(posteriorDistributionPlot, index, postSumModel) {
  # these first lines are there to create compatibility with the BAS plotting code we copied
  x      <- postSumModel[["coefBMA"]]
  conf95 <- postSumModel[["conf95BMA"]]
  subset <- list(index)
  e      <- 1e-04
  
  # based on BAS:::plot.coef.bas.
  # start of copied code
  df <- x$df
  i  <- index
  
  sel      <- x$conditionalmeans[, i] != 0
  prob0    <- 1 - x$probne0[i]
  mixprobs <- x$postprobs[sel]/(1 - prob0)
  means    <- x$conditionalmeans[sel, i, drop = TRUE]
  sds      <- x$conditionalsd[sel, i, drop = TRUE]
  name     <- x$namesx[i]
  name     <- .basregReplaceInteractionUnicodeSymbol(name)
  df.sel   <- df[sel]
  
  df <- df.sel # modified from original
  
  p <- try(silent = FALSE, expr = {
    nsteps <- 500
    if (prob0 == 1 | length(means) == 0) {
      xlower <- -0
      xupper <- 0
      xmax   <- 1
    } else {
      qmin <- min(qnorm(e/2, means, sds))
      qmax <- max(qnorm(1 - e/2, means, sds))
      if (i > 1) {
        xlower <- min(qmin, 0)
        xupper <- max(0, qmax)
      } else {
        xlower <- qmin
        xupper <- qmax
      }
    }
    
    xx    <- seq(xlower, xupper, length.out = nsteps)
    yy    <- rep(0, times = length(xx))
    maxyy <- 1
    if (prob0 < 1 & length(sds) > 0) {
      yy <- mixprobs %*% apply(matrix(xx, ncol = 1), 1,
                              FUN = function(x, d, m, s) {
                                dt(x = (x - m)/s, df = d)/s
                              }, d = df, m = means, s = sds)
      maxyy <- max(yy)
    }
    ymax <- max(prob0, 1 - prob0)
    # end of copied code
    
    dens    <- (1 - prob0) * yy/maxyy
    dfLines <- data.frame(
      x = c(0, 0, xx),
      y = c(0, prob0, dens),
      g = factor(rep(1:2, c(2, length(xx))))
    )
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(xlower, xupper))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, 1.15*max(dfLines$y)))
    
    # figure out whether to draw text left or right of 0
    step      <- (xupper + abs(xlower)) / (nsteps - 1)  # stepsize of grid
    idx0      <- round(abs(xlower / step))              # idx of x closest to 0
    idxMax    <- which.max(dens)                      # idx of maximum of density
    maxX      <- xx[idxMax]                             # x value at maximum of density
    maxHeight <- dens[idxMax]                        # y value at maximum of density
    if (prob0 > maxHeight) { # if text drawn above posterior no action is required
      
      xText <- 0.05 * xBreaks[length(xBreaks)]
      hjust <- "left"
      # text below maxheight
      
    } else {
      
      # text is drawn right if:
      # - density is below textheight
      # - peak of density is left of textheight
      
      # text drawn at similar height as posterior
      if (maxX < 0 && dens[idx0] < prob0) {
        # peak is left of text; density is below text height
        xText <- 0.05 * xBreaks[length(xBreaks)]
        hjust <- "left"
        
      } else {
        
        xText <- -abs(0.05 * xBreaks[1])
        hjust <- "right"
        
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
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    posteriorDistributionPlot$setError(errorMessage)
  } else {
    posteriorDistributionPlot$plotObject <- p
  }
}

.basregTableDescriptives <- function(jaspResults, dataset, options, ready, position) {
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$position <- position
  descriptivesTable$dependOn(c("descriptives", "dependent", "covariates"))
  
  descriptivesTable$addColumnInfo(name = "v",     title = "",              type = "string")
  descriptivesTable$addColumnInfo(name = "N",     title = gettext("N"),    type = "integer")
  descriptivesTable$addColumnInfo(name = "mean",  title = gettext("Mean"), type = "number")
  descriptivesTable$addColumnInfo(name = "sd",    title = gettext("SD"),   type = "number")
  
  if (ready)
    .basregFillTableDescriptives(descriptivesTable, dataset, options)
  
  jaspResults[["descriptivesTable"]] <- descriptivesTable
}

.basregFillTableDescriptives <- function(descriptivesTable, dataset, options) {
  variables <- c(options$dependent, unlist(options$covariates))
  for (variable in variables) {
    data <- na.omit(dataset[[ .v(variable) ]])
    n <- length(data)
    mean <- mean(data)
    sd <- sd(data)
    
    descriptivesTable$addRows(list(v = variable, N = n, mean = mean, sd = sd))
  }
}

.basregGetModel <- function(basregContainer, dataset, options, ready) {
  if (!ready || basregContainer$getError())
    return()
    
  if (!is.null(basregContainer[["basregModel"]]))
    return(basregContainer[["basregModel"]]$object)
  
  formula <- .basregCreateFormula(options$dependent, options$modelTerms)
  isNuisance <- .basregCreateNuisanceLookupVector(options$modelTerms)
  nPreds <- length(options$modelTerms)
  
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
  if (options$modelPrior == "beta.binomial")
    modelPrior <- BAS::beta.binomial(as.numeric(options$betaBinomialParamA), as.numeric(options$betaBinomialParamB))
  else if (options$modelPrior == "uniform")
    modelPrior <- BAS::uniform()
  else if (options$modelPrior == "Bernoulli")
    modelPrior <- BAS::Bernoulli(options$bernoulliParam)
  else if (options$modelPrior == "Wilson")
    modelPrior <- BAS::beta.binomial(1.0, as.numeric(nPreds * options$wilsonParamLambda))
  else if (options$modelPrior == "Castillo")
    modelPrior <- BAS::beta.binomial(1.0, as.numeric(nPreds ^ options$castilloParamU))
  
  # number of models
  n.models <- NULL
  if (options$samplingMethod == "BAS" && options$numberOfModels > 0)
    n.models <- options$numberOfModels
  
  # iterations for MCMC
  MCMC.iterations <- NULL
  if (options$samplingMethod == "MCMC" && options$iterationsMCMC > 0)
    MCMC.iterations <- options$iterationsMCMC
  
  # parameter for hyper-g's or jzs (all use same alpha param in bas.lm)
  alpha <- switch(
    options$priorRegressionCoefficients,
    "hyper-g" = options$alpha,
    "hyper-g-laplace" = options$alpha,
    "hyper-g-n" = options$alpha,
    "JZS" = options$rScale^2,
    NULL
  )
  
  # Bayesian Adaptive Sampling
  .setSeedJASP(options)
  bas_lm <- try(BAS::bas.lm(
    formula         = formula,
    data            = dataset,
    prior           = options$priorRegressionCoefficients,
    alpha           = alpha,
    modelprior      = modelPrior,
    n.models        = n.models,
    method          = options$samplingMethod,
    MCMC.iterations = MCMC.iterations,
    initprobs       = initProbs,
    weights         = wlsWeights,
    renormalize     = TRUE
  ))
  
  if (isTryError(bas_lm)) {
    errorMsg <- .extractErrorMessage(bas_lm)
    basregContainer$setError(errorMsg)
    return()
  }
  
  if (bas_lm$n.models > 1 && nPreds > 1) # can crash without this check
    bas_lm <- BAS::force.heredity.bas(bas_lm)
  
  # fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
  bas_lm[["priorprobs"]] <- bas_lm[["priorprobs"]] / sum(bas_lm[["priorprobs"]])
  bas_lm[["priorprobsPredictor"]] <- .basregComputePriorMarginalInclusionProbs(bas_lm)
  bas_lm[["weights"]] <- wlsWeights
  bas_lm[["BFinclusion"]] <- .basregComputeInclusionBF(bas_lm)
  bas_lm[["namesx"]][-1] <- .unvf(bas_lm[["namesx"]][-1])
  bas_lm[["nuisanceTerms"]] <- setNames(isNuisance, .unvf(names(isNuisance)))

  basregContainer[["basregModel"]] <- createJaspState(bas_lm)
  
  return(bas_lm)
}

.basregCreateFormula <- function(dependent, modelTerms) {
  formula <- c(dependent, "~")
  for (i in 1:length(modelTerms)) {
    term <- modelTerms[[i]]
    termName <- paste(term$component, collapse = ":")
    formula <- c(formula, ifelse(i == 1, "", "+"), termName)
  }
  formula <- as.formula(.vf(paste(formula, collapse = "")), env = parent.frame(1)) # bas.lm searches for objects defined in .basregGetModel in the formula env..
  return(formula)
}

.basregCreateNuisanceLookupVector <- function(modelTerms) {
  isNuisance <- rep(FALSE, length(modelTerms))
  for (i in 1:length(modelTerms)) {
    term <- modelTerms[[i]]
    termName <- paste(term$component, collapse = ":")
    names(isNuisance)[i] <- .vf(termName)
    if (term$isNuisance)
      isNuisance[i] <- TRUE
  }
  return(isNuisance)
}

.basregGetPosteriorSummary <- function(basregContainer, basregModel, dataset, options, ready) {
  if (!ready || basregContainer$getError())
    return()
  
  if (!is.null(basregContainer[["postSumModel"]]))
    return(basregContainer[["postSumModel"]]$object)
  
  # required for the marginal posterior plots
  # done here such that the information in the plots and tables always matches
  # if a user selects the same options. (The method uses approximations and otherwise decimals are off)
  footnote <- NULL
  
  .setSeedJASP(options)
  coefBMA <- .basregOverwritecoefBas(basregModel, estimator = "BMA", dataset = dataset, options = options, weights = basregModel[["weights"]])
  conf95BMA <- try(stats::confint(coefBMA, level = 0.95, nsim = options$nSimForCRI))
  if (isTryError(conf95BMA)) {
    conf95BMA <- cbind(NA, NA, coefBMA$postmean)
    rownames(conf95BMA) <- coefBMA$namesx
    colnames(conf95BMA) <- c("2.5%", "97.5%", "beta")
    conf95BMA[is.nan(conf95BMA)] <- NA
    footnote <- gettext("Parameters estimates and/or credible intervals could not be calculated.")
  }
  
  # check if results of table and plots should match
  estimator <- switch(options$summaryType, best = "HPM", median = "MPM", "BMA")
  criVal <- options[["posteriorSummaryPlotCredibleIntervalValue"]]
  if (estimator == "BMA" && isTRUE(all.equal(criVal, 0.95))) { # what we show under Marginal Posterior distributions
    coef <- coefBMA
    conf95 <- conf95BMA
  } else {
    .setSeedJASP(options)
    coef <- .basregOverwritecoefBas(basregModel, estimator = estimator, dataset = dataset, options = options, weights = basregModel[["weights"]])
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
  
  postSumModel <- list(coef = coef, loopIdx = loopIdx, coefficients = coefficients, probne0 = probne0,
                           conf95 = conf95, coefBMA = coefBMA, conf95BMA = conf95BMA, footnote = footnote)
  
  basregContainer[["postSumModel"]] <- createJaspState(postSumModel)
  basregContainer[["postSumModel"]]$dependOn(c("summaryType", "posteriorSummaryPlotCredibleIntervalValue", "nSimForCRI",
                                               "seed", "setSeed"))
  
  return(postSumModel)
}

.basregOverwritecoefBas <- function (basregModel, n.models, estimator = "BMA", dataset, options, weights = NULL) {
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
    formula <- .basregCreateFormula(options$dependent, options$modelTerms)
    nvar = basregModel$n.vars - 1
    bestmodel <- (0:nvar)[basregModel$probne0 > 0.5]
    best = 1
    models <- rep(0, nvar + 1)
    models[bestmodel + 1] <- 1
    if (sum(models) > 1) {
        basregModel <- BAS::bas.lm(formula = formula, data = dataset,
                                   weights = weights,
                                   n.models = 1,
                                   alpha = basregModel$g, initprobs = basregModel$probne0,
                                   prior = basregModel$prior, modelprior = basregModel$modelprior,
                                   update = NULL, bestmodel = models, prob.local = 0)
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

.basregComputeInclusionBF <- function(basregModel) {
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

.basregReplaceInteractionUnicodeSymbol <- function(name) {
  # ggplot can't show the interaction symbol
  gsub("\u2009\u273b\u2009", " x ", name, fixed = TRUE)
}
