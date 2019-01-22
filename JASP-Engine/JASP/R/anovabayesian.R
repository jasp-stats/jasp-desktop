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

	.theBayesianLinearModelsInitBayesFactor()
	env <- environment()

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
	meta[[1]] <- list(name = "title",                     type = "title")
	meta[[2]] <- list(name = "model comparison",          type = "table")
	meta[[3]] <- list(name = "effects",                   type = "table")
	meta[[4]] <- list(name = "estimates",                 type = "table")
	meta[[5]] <- list(name = "posthoc",                   type = "collection", meta = "table")
	meta[[6]] <- list(name = "Q-Q plot",                  type = "image")
	meta[[7]] <- list(name = "Model Averaged Posteriors", type = "collection", meta = "image")
	meta[[8]] <- list(name = "rsquared plot",             type = "image")

	wantsTwoPlots <- options$plotSeparatePlots
	if (wantsTwoPlots == "") {
		meta[[9]] <- list(
			name = "descriptivesObj", type = "object",
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "image"))
			)
	} else {
		meta[[9]] <- list(
			name = "descriptivesObj", type = "object",
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "collection", meta = "image"))
			)
	}
	smiTitle <- .AnovaBayesianGetSMItitle(options)
  meta[[10]] <- list(
    name = "Single Model Inference", type = "object",
    meta = list(
      list(name = "Analysis of effects",      type = "table"), 
      list(name = "Posterior Distributions",  type = "collection", meta = "image"),
      list(name = "Q-Q plot",                 type = "image"),
      list(name = "rsquared plot",            type = "image")
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
	model$modelIndex <- .AnovaBayesianSMIgetModelIndex(model, options, perform)
	
	# calculate ggplots 
	model$BMASMIPlot <- .AnovaBayesianComputePlots(model, options, perform)
	save(model = model, options = options, perform = perform, file = "C:/Users/donvd/Desktop/jasp.rda")
	# load("C:/Users/donvd/Desktop/jasp.rda")
## Effects Table
	results[["effects"]] <- .theBayesianLinearModelsEffects(model, options, perform, status, populate = FALSE)

## Posterior Estimates
	results[["estimates"]] <- .theBayesianLinearModelEstimates(model, options, perform, status)

## Post Hoc Table
	results[["posthoc"]] <- .anovaNullControlPostHocTable(dataset, options, perform, status, analysisType = "ANOVA")

## Q-Q plot
	results[["Q-Q plot"]] <- .AnovaBayesianQQplot(model, perform, optsCheck = options[["qqPlot"]])
	
## R-squared plot
	results[["rsquared plot"]] <- .AnovaBayesianRsqPlot(model, perform, optsCheck = options[["rsqPlot"]])

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
			results[[".meta"]][[10]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "image")

	} else {
		results[["descriptivesObj"]] <- list(
			title = "Descriptives", descriptivesTable = descriptivesTable,
			descriptivesPlot = list(collection = descriptivesPlot, title = "Descriptives Plots")
			)

		if (plotOptionsChanged)
			results[[".meta"]][[10]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "collection", meta = "image")

	}
	keep <- c(
	  unlist(lapply(descriptivesPlot, `[[`, "data")),
	  unlist(lapply(results[["Single Model Inference"]][["Posterior Distributions"]][["collection"]], `[[`, "data")),
	  unlist(lapply(results[["Model Averaged Posteriors"]][["collection"]], `[[`, "data")),
	  results[["Single Model Inference"]][["Q-Q plot"]]$data,
	  results[["Single Model Inference"]][["rsquared plot"]]$data,
	  results[["Q-Q plot"]]$data,
	  results[["rsquared plot"]]$data
	)
	new.state <- list(options = options, model = model, status = status, keep = keep)

	if (perform == "run" || ! status$ready || ! is.null(state)) {
		return(list(results = results, status = "complete", state = new.state, keep = keep))
	} else {
		return(list(results = results, status = "inited", keep = keep))
	}
}

# functions that calculate things ----
.AnovaBayesianSamplePosteriors <- function(model, options, perform) {

  # TODO: does not updates when model different models are added to the NULL-model
  # sample from all models considered in
  if (perform != "run" || !is.null(model$posteriors) || !any(unlist(options[c(
        "qqPlot", "modelAveragedPosteriors", "singleModelPosteriors", "singleModelqqPlot",
        "rsqPlot", "singleModelrsqPlot"
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

.AnovaBayesianGetSMIResidRsq <- function(posterior) {

  # input : posterior object from Bayesfactor package
  # output: list with residuals, predictions, and r-squared

  # NOTE: the matrix multiplication in this function allocates an array of nobs * nsamples, which can be enormous.
  # if is more memory efficient to use for loops (but slower in R) to calculate predictions for each observation
  # and posterior sample. This could be done in the future if performance is an issue. However, this likely cannot be
  # done efficiently in R.

  # here we need the data in a one-hot encoded way
  f <- as.formula(posterior@model@identifier$formula) # describes what data we need

  # first one is dependent variable, rest are independent variables
  dvs <- all.vars(f)[1L]
  ivs <- all.vars(f)[-1L]

  # idx contains variables that need to be one-hot encoded
  dat <- posterior@data
  isFactor <- sapply(dat, class) == "factor"
  idx <- isFactor & names(isFactor) %in% c(dvs, ivs)

  # do one-hot encoding (thank base R for calling a function `contrast` and giving it the argument `contrast`...)
  datOneHot <- model.matrix(f, data = dat, contrasts.arg = lapply(dat[idx], contrasts, contrasts = FALSE))

  idx <- which(colnames(posterior) == "sig2")
  idx <- idx[length(idx)]: ncol(posterior)
  preds <- tcrossprod(datOneHot, posterior[, -idx])

  # calculate residuals (correctly recycles dat[[dvs]])
  resid <- dat[[dvs]] - preds
  
  rsq <- .AnovaBayesianComputeRsq(dat[[dvs]], preds)
  
  return(list(resid = resid, preds = preds, rsq = rsq))
}

.AnovaBayesianComputeRsq <- function(obs, preds) {
  
  # NOTE: R^2 != cor(obs, predict) because the predictions from the posterior samples are not
  # OLS estimates. In addition, we should avoid terms that scale in N.
  
  if (is.null(dim(preds)))
    preds <- matrix(preds, ncol = 1)

  mu <- mean(obs)
  n <- length(obs)
  sstot <- var(obs) * (n - 1)
  ssreg <- colSums((preds - mu)^2)
  return(ssreg / sstot)
  
}

.AnovaBayesianResidualsNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n      <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  # uninformative priors
  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0)

  # compute all pairwise differences (residuals)
  preds <- tcrossprod(rep(1, n), samples)
  
  resids <- dependent - preds
  
  rsq <- .AnovaBayesianComputeRsq(dependent, preds)

  return(list(resids = resids, rsq = rsq))

}

.AnovaBayesianComputePlots <- function(model, options, perform) {
  
  # we precompute the residual and rsq plot, because they require the same computations
  nmodels <- length(model$models)
  if (perform != "run" || nmodels == 0L)
    return()
  
  plotList <- list()
  if (options$qqPlot || options$rsqPlot)
    plotList$BMA <- .AnovaBayesianPlotPosteriors(model, "modelAveraged", "residual")$plotList

  if (options$singleModelqqPlot ||options$singleModelrsqPlot) {
    plotList$SMI <- .AnovaBayesianPlotPosteriors(model, model$modelIndex, "residual")$plotList
  }

  return(plotList)
}

# meta wrapper functions ----
.AnovaBayesianQQplot <- function(model, perform = "run", modelIndex = "modelAveraged", optsCheck = FALSE) {

  # meta wrapper for Q-Q plots
  if (!optsCheck)
    return()

  title <- .AnovaBayesianPrePendBMA("Q-Q Plot", modelIndex)
  output <- list(
    title  = title,
		width  = 400,
		height = 400,
		custom = list(
		  width  = "plotWidthDescriptivesPlotLegend",
		  height = "plotHeightDescriptivesPlotLegend"
		)
  )

  if (perform == "run" && length(model$posteriors) >= 1L) {

    # p <- .AnovaBayesianPlotPosteriors(model, modelIndex = modelIndex, parameterIndex = "residual")
    idx <- if (modelIndex == "modelAveraged") "BMA" else "SMI"
    content <- .writeImage(
      width  = output$width,
      height = output$height,
      plot   = model$BMASMIPlot[[idx]][["residual"]],
      obj    = TRUE
    )

  	output[["data"]]        <- content[["png"]]
  	output[["obj"]]         <- content[["obj"]]
  	output[["convertible"]] <- TRUE
  	output[["status"]]      <- "complete"

  }
  return(output)
}

.AnovaBayesianRsqPlot <- function(model, perform = "run", modelIndex = "modelAveraged", optsCheck = FALSE) {
 
  # meta wrapper for r-squared plots
  if (!optsCheck)
    return()

  title <- .AnovaBayesianPrePendBMA("Posterior R\u00B2", modelIndex)
  output <- list(
    title  = title,
		width  = 400,
		height = 400,
		custom = list(
		  width  = "plotWidthDescriptivesPlotLegend",
		  height = "plotHeightDescriptivesPlotLegend"
		)
  )

  if (perform == "run" && length(model$posteriors) >= 1L) {

    # p <- model$BMA$plotlist[["rsquared"]]
    idx <- if (modelIndex == "modelAveraged") "BMA" else "SMI"
    content <- .writeImage(
      width  = output$width,
      height = output$height,
      plot   = model$BMASMIPlot[[idx]][["rsquared"]],
      obj    = TRUE
    )

  	output[["data"]]        <- content[["png"]]
  	output[["obj"]]         <- content[["obj"]]
  	output[["convertible"]] <- TRUE
  	output[["status"]]      <- "complete"

  }
  return(output) 
}

.AnovaBayesianWrapperPosteriorPlot <- function(model, perform = "run", modelIndex = "modelAveraged", optsCheck) {

  # meta wrapper for model averaged posterior plots, single model posterior plots, and Q-Q plots
  nmodels <- length(model$posteriors)
  if (!optsCheck || perform != "run" || nmodels < 1L)
    return()

  title <- .AnovaBayesianPrePendBMA("Posterior Distributions", modelIndex)
  singlePlot <- list(
    title  = title,
    width  = 400,
    height = 400,
    custom = list(
      width  = "plotWidthDescriptivesPlotLegend",
      height = "plotHeightDescriptivesPlotLegend"
    )
  )
  
  allParamNames <- colnames(model$posteriors[[nmodels]])
  idx <- which(allParamNames == "sig2") # first trivial parameter
  allParamNames <- allParamNames[-c(1L, idx[length(idx)]:length(allParamNames))]

  output <- list(title = "Posterior Distributions")
  base64map <- model$base64map
  for (name in allParamNames) {

    p <- .AnovaBayesianPlotPosteriors(model, modelIndex = modelIndex, parameterIndex = name)

    content <- .writeImage(
      width  = singlePlot$width,
      height = singlePlot$height,
      plot   = p$plotList[[1L]],
      obj    = TRUE
    )

    title <- stringr::str_replace_all(string = name, model$base64map)
    
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

# plot functions ----
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
  parameterIndex <- match.arg(parameterIndex, c(cnms, "residual", "rsquared"), several.ok = TRUE)
  if ("rsquared" %in% parameterIndex)
    parameterIndex <- unique(c(parameterIndex, "residual"))

  residSumStats <- predsSumStats <- NULL
  nplots <- length(parameterIndex)
  plotList <- vector("list", nplots)
  names(plotList) <- parameterIndex
  for (p in parameterIndex) {
    if (p == "residual" || p == "rsquared") {

      if (modelIndex == "modelAveraged") {
        
        nsamples <- nrow(model$posteriors[[1L]])
        
        # get model weights
        postProbs <- .AnovaBayesianGetPostProbs(model)
        weights <- rep(postProbs, each = nsamples)

        iv <- all.vars(as.formula(posteriors[[1]]@model@identifier$formula))[1L]
        tmp    <- .AnovaBayesianResidualsNullModel(nsamples, posteriors[[1L]]@data[, iv])
        resids <- tmp$resid
        
        rsq <- vector("list", nmodels)
        rsq[[1]]    <- tmp$rsq

        for (i in seq_len(nmodels)) {
          tmp <- .AnovaBayesianGetSMIResidRsq(posteriors[[i]])
          resids <- cbind(resids, tmp$resid)
          rsq[[i+1L]] <- tmp$rsq
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

        # compute BMA density
        dd <- .AnovaBayesianGetBMAdensity(rsq, postProbs, 0:1)
        
        # compute weighted CRI
        rsqCri <- Hmisc::wtd.quantile(
          x       = unlist(rsq), 
          weights = weights, 
          probs   = c(0.025, 0.975)
        )

      } else {

        tmp <- .AnovaBayesianGetSMIResidRsq(posteriors[[modelIndex]])

        # temporary function to summarize by mean, sd, median, and cri
        foo <- function(x) {
          tmp <- quantile(x, probs = c(0.025, 0.5, 0.975))
          c(mean   = mean(x),
            sd     = sd(x),
            median = tmp[2L],
            cri    = tmp[-2L])
        }
        residSumStats <- apply(tmp$resid, 1, foo)
        
        dd <- density(tmp$rsq, n = 2^10, from = 0, to = 1)
        rsqCri <- quantile(tmp$rsq, c(0.025, 0.975))
      }

      plotList[["residual"]] <- .AnovaBayesianPlotQQnorm(residSumStats["mean", ], residSumStats["cri.2.5%", ], residSumStats["cri.97.5%", ])
      
      # it is extremely expensive to recompute the posterior r squared and relatively cheap to just make the plot here
      # when everything has been computed already, so we make the ggplot anyway.

      # make prior posterior plot
      df <- data.frame(x = dd$x, y = dd$y)
      xName <- expression(R^2)

      plotList[["rsquared"]] <- JASPgraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = rsqCri, 
                                                                  drawCRItxt = FALSE)

    } else {

      if (modelIndex == "modelAveraged") {

        # get model weights - omit first model weight (the null-model)
        postProbs <- .AnovaBayesianGetPostProbs(model)[-1L]

        # get all posterior samples
        allSamps <- lapply(posteriors, function(x, p) if (p %in% colnames(x)) x[, p], p)
        
        # compute BMA density
        dd <- .AnovaBayesianGetBMAdensity(allSamps, postProbs)
  
        # compute weighted CRI
        densCRI <- Hmisc::wtd.quantile(
          x       = unlist(allSamps), 
          weights = rep(postProbs, lengths(allSamps)), 
          probs   = c(0.025, 0.975)
        )
        
      } else {
        dd <- density(posteriors[[modelIndex]][, p], n = 2^10)
        densCRI <- quantile(posteriors[[modelIndex]][, p], probs = c(0.025, 0.975))
      }

      # make prior posterior plot
      df <- data.frame(x = dd$x, y = dd$y)
      
      xName <- stringr::str_replace_all(string = p, model$base64map)

      plotList[[p]] <- JASPgraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = densCRI, 
                                                         drawCRItxt = FALSE)

    }
  }

  output <- list(
    plotList = plotList,
    residuals = list(
      residSumStats
    )
  )

  return(output)
}

# Single Model Inference (SMI) ----
.AnovaBayesianSMI <- function(model, options, perform) {
  
  nmodels <- length(model$models)
  if (perform != "run" || nmodels == 0 || length(options$singleModelTerms) == 0)
    return()
  
  modelIndex <- model$modelIndex
  if (modelIndex == 0L)
    return()
  
  output <- list(
    title = "Single Model Inference",
    "Analysis of effects"     = .AnovaBayesianSMIEffects(
      model = model, options = options, modelIndex = modelIndex
    ),
    "Posterior Distributions" = .AnovaBayesianWrapperPosteriorPlot(
      model = model, modelIndex = modelIndex, optsCheck = options[["singleModelPosteriors"]]
    ), 
    "Q-Q plot"                = .AnovaBayesianQQplot(
      model = model, modelIndex = modelIndex, optsCheck = options[["singleModelqqPlot"]]
    ),
    "rsquared plot"           = .AnovaBayesianRsqPlot(
      model = model, modelIndex = modelIndex, optsCheck = options[["singleModelrsqPlot"]]
    )
  )
  return(output)
}

.AnovaBayesianSMIEffects <- function(model, options, modelIndex) {
  
  if (!options[["singleModelEffects"]])
    return()
  
}

.AnovaBayesianSMIgetModelIndex <- function(model, options, perform) {
  
  if (perform != "run")
    return()
  nmodels <- length(model$models)
  if (nmodels == 0L)
    return(0L)
  targetFormula <- .theBayesianLinearModelsGetModelFormula(options$dependent, options$singleModelTerms)$model.formula
  smiTerms <- colnames(attr(terms(formula(targetFormula)), "factors"))
  tmpFun <- function(x, smiTerms) {
    # get the names of variables used in a specific BayesFactor object
    nms <- colnames(attr(terms(as.formula((x$bf@numerator[[1L]]@identifier$formula))), "factors"))
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

.AnovaBayesianGetSMItitle <- function(options) {
  "Single Model Inference"
}

# helper functions ----
.AnovaBayesianPrePendBMA <- function(x, modelIndex) {
  if (modelIndex == "modelAveraged")
    x <- paste("Model Averaged", x)
  return(x)
}

.AnovaBayesianGetPostProbs <- function(model) {
  # get model weights
  postProbs <- sapply(model$models, `[[`, "postProb")
  # attach P(M|data) for null model
  postProbs <- c(1 - sum(postProbs), postProbs)
  return(postProbs)
}

.AnovaBayesianGetBMAdensity <- function(samples, weights, fromTo = NULL, n = 2^10) {
  
  # @param samples, list of samples
  # @param weigths, vector of numeric weights
  # @param fromTo vector of length 2 specifying lower and upper bound (optional).
  # @return a list with $x the x-coordinates and $y the y-coordinates.
  
  if (length(samples) != length(weights))
    stop("length of samples must be equal to length of weights!")
  
  # remove NULL indices
  idxNonNull <- lengths(samples) > 0
  weights <- weights[idxNonNull]
  samples <- samples[idxNonNull]
  
  # renormalize the weights so output is a proper density function (TODO: do we want this, or BAS style?)
  weights <- weights / sum(weights)

  # create x-grid for density
  if (is.null(fromTo))
    fromTo <- range(sapply(samples, range))
  xs <- seq(fromTo[1L], fromTo[2L], length.out = 2^10)
  
  # compute weighted density
  ys <- numeric(n)
  for (i in seq_along(samples)) {
    ys <- ys + weights[i] * density(samples[[i]], from = fromTo[1L], to = fromTo[2L], n = n)$y
  }
  
  return(list(x = xs, y = ys))
}
