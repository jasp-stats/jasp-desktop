#
# Copyright (C) 2013-2018 University of Amsterdam
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

AnovaBayesian <- function(dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), ...) {
##PREAMBLE
	if (is.null(base::options()$BFMaxModels))
		base::options(BFMaxModels = 50000)
	if (is.null(base::options()$BFpretestIterations))
		base::options(BFpretestIterations = 100)
	if (is.null(base::options()$BFapproxOptimizer))
		base::options(BFapproxOptimizer = "optim")
	if (is.null(base::options()$BFapproxLimits))
		base::options(BFapproxLimits = c(-15, 15))
	if (is.null(base::options()$BFprogress))
		base::options(BFprogress = interactive())
	if (is.null(base::options()$BFfactorsMax))
		base::options(BFfactorsMax = 5)

	env <- environment()

	.callbackBFpackage <- function(...) {
		response <- .callbackBayesianLinearModels()
		if(response$status == "ok")
			return(as.integer(0))
		return(as.integer(1))
	}

	.callbackBayesianLinearModels <- function (results = NULL, progress = NULL) {
		response <- callback(results, progress)
		if (response$status == "changed") {

			change <- .diff(env$options, response$options)

			env$options <- response$options

			if (change$modelTerms ||
				change$dependent ||
				change$fixedFactors ||
				change$randomFactors ||
				change$priorFixedEffects ||
				change$priorRandomEffects ||
				change$sampleMode ||
				change$fixedSamplesNumber)
				return(response)
			response$status <- "ok"
		}
		return(response)
	}

	state <- .retrieveState()
	if (! is.null(state)) {
		change <- .diff(options, state$options)
		if (! base::identical(change, FALSE) && (change$modelTerms ||
				change$dependent ||
				change$fixedFactors ||
				change$randomFactors ||
				change$priorFixedEffects ||
				change$priorRandomEffects ||
				change$sampleMode ||
				change$fixedSamplesNumber)) {
			state <- NULL
		} else {
			perform <- "run"
		}
	}

## META
	results <- list()
	meta <- list()
	meta[[1]] <- list(name = "title", type = "title")
	meta[[2]] <- list(name = "model comparison", type = "table")
	meta[[3]] <- list(name = "effects", type = "table")
	meta[[4]] <- list(name = "estimates", type = "table")
	meta[[5]] <- list(name = "posthoc", type = "collection", meta = "table")
	meta[[6]] <- list(name = "Q-Q plot", type = "image")
	meta[[7]] <- list(name = "Model Averaged Posteriors", type = "collection", meta = "image")

	wantsTwoPlots <- options$plotSeparatePlots
	if (wantsTwoPlots == "") {
		meta[[8]] <- list(
			name = "descriptivesObj", type = "object",
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "image"))
			)
	} else {
		meta[[8]] <- list(
			name = "descriptivesObj", type = "object",
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "collection", meta = "image"))
			)
	}
  meta[[9]] <- list(
    name = "Single Model Inference", type = "object",
    meta = list(
      list(name = "Analysis of effects",      type = "table"), 
      list(name = "Posterior Distributions",  type = "collection", meta = "image"),
      list(name = "Q-Q plot",                 type = "image")
    )
  )

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian ANOVA"

## DATA
	dataset <- .readBayesianLinearModelData(dataset, options, perform)

	if (is.null(state)) {
##STATUS (INITIAL)
		status <- .setBayesianLinearModelStatus(dataset, options, perform)

## MODEL
		model.object <- .theBayesianLinearModels(dataset, options, perform, status, .callbackBayesianLinearModels, .callbackBFpackage, results, analysisType = "ANOVA")

		if (is.null(model.object))
			return()

		model <- model.object$model
		status <- model.object$status
	} else {
		model <- state$model
		status <- state$status
	}

## Posterior Table
	model.comparison <- .theBayesianLinearModelsComparison(model, options, perform, status, populate = FALSE)
	results[["model comparison"]] <- model.comparison$modelTable

	if (is.null(state))
		model <- model.comparison$model

	# calculate posteriors
	model$posteriors <- .AnovaBayesianSamplePosteriors(model, options, perform)

## Effects Table
	results[["effects"]] <- .theBayesianLinearModelsEffects(model, options, perform, status, populate = FALSE)

## Posterior Estimates
	results[["estimates"]] <- .theBayesianLinearModelEstimates(model, options, perform, status)

## Post Hoc Table
	results[["posthoc"]] <- .anovaNullControlPostHocTable(dataset, options, perform, status, analysisType = "ANOVA")

	save(model = model, options = options, perform = perform, file = "C:/Users/donvd/Desktop/jasp.rda")
## Q-Q plot
	results[["Q-Q plot"]] <- .AnovaBayesianQQplot(model, perform, options[["qqPlot"]])

## Plots of model averaged posteriors
	results[["Model Averaged Posteriors"]] <- .AnovaBayesianWrapperPosteriorPlot(
	  model = model, perform = perform, optsCheck = options[["modelAveragedPosteriors"]]
	)
	
## Single model effects
  results[["Single Model Inference"]] <- .AnovaBayesianSMI(model, options, perform)

## Descriptives Table
	descriptivesTable <- .anovaDescriptivesTable(dataset, options, perform, status, stateDescriptivesTable = NULL)[["result"]]

## Descriptives Plot
	options$plotErrorBars <- options$plotCredibleInterval
	options$errorBarType <- "confidenceInterval"
	options$confidenceIntervalInterval <- options$plotCredibleIntervalInterval
	plotOptionsChanged <- isTRUE( identical(wantsTwoPlots, options$plotSeparatePlots) == FALSE )
	descriptivesPlot <- .anovaDescriptivesPlot(dataset, options, perform, status, stateDescriptivesPlot = NULL)[["result"]]

	if (length(descriptivesPlot) == 1) {
		results[["descriptivesObj"]] <- list(
			title = "Descriptives", descriptivesTable = descriptivesTable,
			descriptivesPlot = descriptivesPlot[[1]]
			)

		if (plotOptionsChanged)
			results[[".meta"]][[5]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "image")

	} else {
		results[["descriptivesObj"]] <- list(
			title = "Descriptives", descriptivesTable = descriptivesTable,
			descriptivesPlot = list(collection = descriptivesPlot, title = "Descriptives Plots")
			)

		if (plotOptionsChanged)
			results[[".meta"]][[5]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "collection", meta = "image")

	}

	keepDescriptivesPlot <- unlist(lapply(descriptivesPlot, `[[`, "data"))
	keepSMI <- c(
	  unlist(lapply(results[["Single Model Inference"]][["Posterior Distributions"]], `[[`, "data")),
	  results[["Single Model Inference"]][["Q-Q plot"]]$data
	)
	keep <- c(results[["Q-Q plot"]]$data, keepDescriptivesPlot, keepSMI)

	new.state <- list(options = options, model = model, status = status, keep = keep)

	if (perform == "run" || ! status$ready || ! is.null(state)) {
		return(list(results = results, status = "complete", state = new.state, keep = keepDescriptivesPlot))
	} else {
		return(list(results = results, status = "inited", keep = keepDescriptivesPlot))
	}
}

.AnovaBayesianQQplot <- function(model, perform = "run", modelIndex = "modelAveraged", optsCheck = FALSE) {

  # meta wrapper for Q-Q plots
  if (!optsCheck)
    return()

  output <- list(
    title  = "Q-Q Plot",
		width  = 400,
		height = 400,
		custom = list(
		  width  = "plotWidthDescriptivesPlotLegend",
		  height = "plotHeightDescriptivesPlotLegend"
		)
  )

  if (perform == "run" && length(model$posteriors) >= 1L) {

    p <- .AnovaBayesianPlotPosteriors(model, modelIndex = modelIndex, parameterIndex = "residual")

    content <- .writeImage(
      width  = output$width,
      height = output$height,
      plot   = p$plotList$residual,
      obj    = TRUE
    )

  	output[["data"]]        <- content[["png"]]
  	output[["obj"]]         <- content[["obj"]]
  	output[["convertible"]] <- TRUE
  	output[["status"]]      <- "complete"

  }
  return(output)
}

.AnovaBayesianSamplePosteriors <- function(model, options, perform) {

  # TODO: does not updates when model different models are added to the NULL-model
  # sample from all models considered in
  if (perform != "run" || !is.null(model$posteriors) || !any(unlist(options[c(
        "qqPlot", "modelAveragedPosteriors", "singleModelPosteriors", "singleModelqqPlot"
  )]))) {
    # if it already exists, reuse it. else if no options needs it, return whatever there currently is.
    return(model$posteriors)
  }

  # TODO: progress bar?
  nmodels <- length(model$models)
  samplesLst <- vector("list", nmodels)
  for (i in seq_len(nmodels)) {
    samplesLst[[i]] <- BayesFactor::posterior(model$models[[i]][[3L]], iterations = 1e3)
  }
  return(samplesLst)

}

.AnovaBayesianPlotQQnorm <- function(mean, lower95 = NULL, upper95 = NULL, abline = TRUE, ablineColor = "red") {

  # TODO: can be reused, move to JASPgraphs

  n <- length(mean)
  hasErrorbars <- !is.null(lower95) && !is.null(upper95)

  df <- data.frame(
    y = mean,
    x = stats::qnorm(stats::ppoints(n))[order(order(mean))]
  )
  if (hasErrorbars) {
    df$ymin <- lower95
    df$ymax <- upper95
  }

  # determine axes breaks
  xBreaks <- pretty(df$x)
  yBreaks <- pretty(c(df$y, df$ymin, df$ymax))

  # from stats::qqline
  xvals <- stats::quantile(df$y, c(0.25, 0.75), names = FALSE)
  yvals <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(xvals) / diff(yvals)
  int <- xvals[1L] - slope * yvals[1L]

  # initial guess for line range
  xvals <- range(xBreaks)
  yvals <- int + slope * xvals

  # TODO: if any y-values exceed the axes boundaries, recalculate line segment
  # if (yvals[1L] < yBreaks[1]) {
  # }
  # if (yvals[2L] > yBreaks[2L]) {
  # }

  # construct plot
  aes <- ggplot2::aes
  dfLine <- data.frame(x = xvals, y = yvals)
  g <- ggplot2::ggplot(data = df, aes(x = x, y = y))

  if (abline)
    g <- g + ggplot2::geom_line(mapping = aes(x = x, y = y), data = dfLine, inherit.aes = FALSE, color = ablineColor)

  if (hasErrorbars)
    g <- g + ggplot2::geom_errorbar(aes(ymin = ymin, ymax = ymax))

  g <- g +
    JASPgraphs::geom_point() +
    JASPgraphs::scale_x_continuous(name = "Theoretical quantiles", breaks = xBreaks) +
    JASPgraphs::scale_y_continuous(name = "Observed quantiles",    breaks = yBreaks)

  return(JASPgraphs::themeJasp(g))

}

.AnovaBayesianWrapperPosteriorPlot <- function(model, perform = "run", modelIndex = "modelAveraged", optsCheck) {

  # meta wrapper for model averaged posterior plots, single model posterior plots, and Q-Q plots

  nmodels <- length(model$posteriors)
  if (!optsCheck || perform != "run" || nmodels < 1L)
    return()

  singlePlot <- list(
    title  = "Posterior Distribution",
    width  = 400,
    height = 400,
    custom = list(
      width  = "plotWidthDescriptivesPlotLegend",
      height = "plotHeightDescriptivesPlotLegend"
    )
  )

  allParamNames <- colnames(model$posteriors[[nmodels]])
  idx <- which(allParamNames == "sig2")
  allParamNames <- allParamNames[-c(1, idx[length(idx)]:length(allParamNames))]
  
  output <- list(title = "Posterior Distributiosn")
  for (name in allParamNames) {

    p <- .AnovaBayesianPlotPosteriors(model, modelIndex = modelIndex, parameterIndex = name)

    content <- .writeImage(
      width  = singlePlot$width,
      height = singlePlot$height,
      plot   = p$plotList$residual,
      obj    = TRUE
    )

    title <- .unv(strsplit(name, "-")[[1]][1])
    
    thisPlot                  <- singlePlot
    thisPlot[["title"]]       <- title
    thisPlot[["data"]]        <- content[["png"]]
    thisPlot[["obj"]]         <- content[["obj"]]
    thisPlot[["convertible"]] <- TRUE
    thisPlot[["status"]]      <- "complete"

    output[["collection"]][[name]] <- thisPlot

  }

  return(output)

}

.AnovaBayesianPlotPosteriors <- function(model, modelIndex, parameterIndex) {

  # general plot function for both model averaged and not model averaged posteriors and Q-Q plots

  posteriors <- model$posteriors
  nmodels <- length(posteriors)
  if (is.numeric(modelIndex)) {
    if (modelIndex < 0 || modelIndex > nmodels)
      stop("Incorrect model index!")
  } else {
    modelIndex <- match.arg(modelIndex, "modelAveraged")
  }

  cnms <- colnames(posteriors[[nmodels]]) # last model contains all possible names
  parameterIndex <- match.arg(parameterIndex, c(cnms, "residual"), several.ok = TRUE)

  residSumStats <- predsSumStats <- NULL
  nplots <- length(parameterIndex)
  plotList <- vector("list", nplots)
  names(plotList) <- parameterIndex
  for (p in parameterIndex) {
    if (p == "residual") {

      if (modelIndex == "modelAveraged") {

        nsamples <- nrow(model$posteriors[[1L]])
        postProbs <- sapply(model$models, `[[`, "postProb")
        postProbs <- c(1 - sum(postProbs), postProbs) # attach P(M|data) for null model
        weights <- rep(postProbs, each = nsamples)

        iv <- all.vars(as.formula(posteriors[[1]]@model@identifier$formula))[1L]
        resids <- .AnovaBayesianResidualsNullModel(nsamples, posteriors[[1L]]@data[, iv])

        # browser()
        for (i in seq_len(nmodels)) {
          resids <- cbind(resids, .AnovaBayesianGetResidPredict(posteriors[[i]])$resid)
        }
        # compute weighted mean for each row
        means <- tcrossprod(weights / nsamples, resids)

        # compute weighted quantiles for each row
        quants <- apply(resids, 1L, Hmisc::wtd.quantile, weights = weights, probs = c(0.025, 0.975))

        residSumStats <- rbind(
          means,
          quants
        )
        rownames(residSumStats) <- c("mean", "cri.2.5%", "cri.97.5%")

      } else {

        tmp <- .AnovaBayesianGetResidPredict(posteriors[[modelIndex]])

        # temporary function to summarize by mean, sd, median, and cri
        foo <- function(x) {
          tmp <- quantile(x, probs = c(0.025, 0.5, 0.975))
          c(mean   = mean(x),
            sd     = sd(x),
            median = tmp[2L],
            cri    = tmp[-2L])
        }
        residSumStats <- apply(tmp$resid, 1, foo)
        predsSumStats <- apply(tmp$preds, 1, foo)

      }

      plotList[[p]] <- .AnovaBayesianPlotQQnorm(residSumStats["mean", ], residSumStats["cri.2.5%", ], residSumStats["cri.97.5%", ])

    } else {

      if (modelIndex == "modelAveraged") {

        # get model weights
        postProbs <- sapply(model$models, `[[`, "postProb")
	      postProbs <- c(1 - sum(postProbs), postProbs) # attach P(M|data) for null model

        # get all samples
        allSamps <- sapply(posteriors, function(x, p) if (p %in% colnames(x)) x[, p], p)

        # remove NULL indices
        idxNonNull <- lengths(allSamps) > 0
	      weights <- postProbs[idxNonNull]
        allSamps <- allSamps[idxNonNull]

        # renormalize the weights so output is a proper density function (TODO: do we want this, or BAS style?)
        weights <- weights / sum(weights)

        # create x-grid for samples
        rr <- range(sapply(allSamps, range))

        # compute density estimates
        dd <- lapply(allSamps, density, from = rr[1], to = rr[2], n = 2^10)

        # compute weighted density
        ys <- numeric(2^10)
        for (i in seq_along(dd)) {
          ys <- ys + weights[i] * dd[[i]]$y
        }

      } else {
        # wrap it in a list for compatability with model averaged result
        dd <- list(density(posteriors[[modelIndex]][, p], n = 2^10))
        ys <- dd[[1]]$y
      }

      # make prior posterior plot
      df <- data.frame(x = dd[[1]]$x, y = ys)
      
      xName <- strsplit(p, "-")[[1]]
      xName <- paste0(.unv(xName[1]), ": ", xName[2])
      
      # plotList[[p]] <- JASPgraphs::PlotPriorAndPosterior(dfLines = df, xName = xName)
      plotList[[p]] <- JASPgraphs::themeJasp(ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_line() + ggplot2::xlab(xName) + ggplot2::ylab("Density"))
    }
  }

  output <- list(
    plotList = plotList,
    residuals = list(
      residSumStats
    ),
    predictions = list(
      predsSumStats
    )
  )

  return(output)
}

.AnovaBayesianGetResidPredict <- function(posterior) {

  # input : posterior object from Bayesfactor package
  # output: list with residuals and predictions

  # NOTE: the matrix multiplication in this function allocates an array of nobs * nsamples, which can be enormous.
  # if is more memory efficient to use for loops (but slower in R) to calculate predictions for each observation
  # and posterior sample. This could be done in the future if performance is an issue. However, it is not
  # straightforward to compute e.g., weighted quantiles without allocating all samples.

  # here we need the data in a one-hot encoded way
  f <- as.formula(posterior@model@identifier$formula) # describes what data we need

  # first one is dependent variable, rest are independent variables
  dvs <- all.vars(f)[1L]
  ivs <- all.vars(f)[-1L]

  # idx contains variables that need to be one-hot encoded
  dat <- posterior@data
  isFactor <- sapply(dat, class) == "factor"
  idx <- isFactor & names(isFactor) %in% c(dvs, ivs)

  # do one-hot encoding (thank base R for calling a function `contrast` and giving it the argument `contrast` ...)
  datOneHot <- model.matrix(f, data = dat, contrasts.arg = lapply(dat[idx], contrasts, contrasts = FALSE))

  idx <- which(colnames(posterior) == "sig2")
  idx <- idx[length(idx)]: ncol(posterior)
  preds <- tcrossprod(datOneHot, posterior[, -idx])

  # calculate residuals (correctly recycles dat[[dvs]])
  resid <- dat[[dvs]] - preds

  return(list(resid = resid, preds = preds))
}

.AnovaBayesianResidualsNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0)

  # compute all pairwise differences
  resids <- outer(dependent, samples, `-`)

  return(resids)

}

# Single Model Inference (SMI)
.AnovaBayesianSMI <- function(model, options, perform) {
  
  nmodels <- length(model$models)
  if (perform != "run" || nmodels == 0 || length(options$singleModelTerms) == 0)
    return()
  
  modelIndex <- .AnovaBayesianSMIgetModelIndex(model, options)
  
  output <- list(
    title = "Single Model Inference",
    "Analysis of effects"     = .AnovaBayesianSMIEffects(
      model = model, options = options, modelIndex = modelIndex
    ),
    "Posterior Distributions" = .AnovaBayesianWrapperPosteriorPlot(
      model = model, modelIndex = modelIndex, optsCheck = options[["singleModelPosteriors"]]#, 
      # title = "Posteriors of Single Model"
    ), 
    "Q-Q plot"                = .AnovaBayesianQQplot(
      model = model, modelIndex = modelIndex, optsCheck = options[["singleModelqqPlot"]]
    )
  )
  return(output)
}

.AnovaBayesianSMIEffects <- function(model, options, modelIndex) {
  
  if (!options[["singleModelEffects"]])
    return()
  
}

.AnovaBayesianSMIgetModelIndex <- function(model, options) {
  
  targetFormula <- .theBayesianLinearModelsGetModelFormula(options$dependent, options$modelTerms)$model.formula
  smiTerms <- colnames(attr(terms(formula(targetFormula)), "factors"))
  # smiTerms <- sapply(options$singleModelTerms, `[[`, "components")
  tmpFun <- function(x, smiTerms) {
    # get the names of variables used in a specific BayesFactor object
    nms <- colnames(attr(terms(as.formula((x$bf@numerator[[1]]@identifier$formula))), "factors"))
    # check if the lengths are equal and only then if the contents match
    return(length(nms) == length(smiTerms) && all(smiTerms %in% nms)) 
  }
  for (i in seq_along(model$models)) {
    check <- tmpFun(model$models[[i]], smiTerms)
    if (check) # exit loop on match
      break
  }
  if (!check)
    .quitAnalysis("Internal error in single model inference.")
  
  return(i)
}
