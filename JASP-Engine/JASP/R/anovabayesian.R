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
	
	wantsTwoPlots <- options$plotSeparatePlots
	if (wantsTwoPlots == "") {
		meta[[7]] <- list(
			name = "descriptivesObj", type = "object", 
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "image"))
			)
	} else {
		meta[[7]] <- list(
			name = "descriptivesObj", type = "object", 
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "collection", meta = "image"))
			)	
	}
	
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

## Effects Table
	results[["effects"]] <- .theBayesianLinearModelsEffects(model, options, perform, status, populate = FALSE)

## Posterior Estimates
	results[["estimates"]] <- .theBayesianLinearModelEstimates(model, options, perform, status)

## Post Hoc Table
	results[["posthoc"]] <- .anovaNullControlPostHocTable(dataset, options, perform, status, analysisType = "ANOVA")
	
	results[["Q-Q plot"]] <- .AnovaBayesianQQplot(model, options, perform)

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

	keepDescriptivesPlot <- lapply(descriptivesPlot, function(x) x$data)
	
	new.state <- list(options = options, model = model, status = status, keep = keepDescriptivesPlot)
	
	if (perform == "run" || ! status$ready || ! is.null(state)) {
		return(list(results = results, status = "complete", state = new.state, keep = keepDescriptivesPlot))
	} else {
		return(list(results = results, status = "inited", keep = keepDescriptivesPlot))
	}
}

.AnovaBayesianQQplot <- function(model, options, perform = "run") {
  if (!options[["qqPlot"]])
    return()
  
  output <- list(
    title  = "Q-Q Plot",
		width  = options[["plotWidthDescriptivesPlotLegend"]],
		height = options[["plotHeightDescriptivesPlotLegend"]],
		custom = list(
		  width  = "plotWidthDescriptivesPlotLegend", 
		  height = "plotHeightDescriptivesPlotLegend"
		)
  )
  
  nmodels <- length(model$models)
  print("nmodels = ")
  print(nmodels)

  if (perform == "run" && nmodels >= 1L) {
    
    # TODO: put this in the state/ results object
    samplesLst <- vector("list", nmodels)
    for (i in seq_len(nmodels)) {
      samplesLst[[i]] <- BayesFactor::posterior(model$models[[i]][[3L]], iterations = 1e3)
    }
    print(str(samplesLst))

    p <- .AnovaBayesianGetMarginalPosteriors(samplesLst, modelIndex = 1, parameterIndex = "residual")
  
    content <- .writeImage(
      width  = options[["plotWidthDescriptivesPlotLegend"]],
      height = options[["plotHeightDescriptivesPlotLegend"]],
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

.AnovaBayesianGetMarginalPosteriors <- function(samples, modelIndex, parameterIndex) {
  
  samps <- as.matrix(samples[[modelIndex]])
  samps <- samps[, -(ncol(samps):(ncol(samps) - 1L))]
  
  cnms <- colnames(samps)
  match.arg(parameterIndex, c(cnms, "residual"), several.ok = TRUE)
  
  summStats <- NULL
  nplots <- length(parameterIndex)
  plotList <- vector("list", nplots)
  names(plotList) <- parameterIndex
  for (p in parameterIndex) {
    if (p == "residual") {
      
      # here we need the data in a one-hot encoded way
      f <- as.formula(samples[[modelIndex]]@model@identifier$formula) # describes what data we need
      
      # first one is dependent variable, rest are independent variables
      dvs <- all.vars(f)[1L]
      ivs <- all.vars(f)[-1L] 
      
      # idx contains variables that need to be one-hot encoded
      dat <- samples[[modelIndex]]@data
      isFactor <- sapply(dat, class) == "factor"
      idx <- isFactor & names(isFactor) %in% c(dvs, ivs)
      
      # do one-hot encoding (thank base R for calling a function `contrast` and giving it the argument `contrast` ...)
      datOneHot <- model.matrix(f, data = dat, contrasts.arg = lapply(dat[idx], contrasts, contrasts = FALSE))
      
      # NOTE: the matrix multiplication below allocates an array of nobs * nsamples, which can be enourmous.
      # if is more memory efficient to use for loops (but slower in R) 
      # calculate predictions for each observation and posterior sample
      preds <- datOneHot %*% t(samps)
      
      # calculate residuals (correctly recycles dat[[dvs]])
      resid <- dat[[dvs]] - preds
      
      # temporary function to summarize by mean, sd, median, and cri
      foo <- function(x) {
        tmp <- quantile(x, probs = c(0.025, 0.5, 0.975))
        c(mean   = mean(x), 
          sd     = sd(x), 
          median = tmp[2L], 
          cri    = tmp[-2L])
      }
      residSumStats <- apply(resid, 1, foo)
      predsSumStats <- apply(preds, 1, foo)
      
      plotList[[p]] <- .AnovaBayesianPlotQQnorm(
        mean    = residSumStats["mean", ], 
        lower95 = residSumStats["cri.2.5%", ], 
        upper95 = residSumStats["cri.97.5%", ]
      )
      
      # obtain density estimates?    
      # summDens <- apply(resids, 1, density, n = 2^10)

    } else {
      dens <- density(samps[, p])
      df <- data.frame(x = dens$x, y = dens$y)
      plotList[[p]] <- JASPgraphs::PlotPriorAndPosterior(dfLines = df)
    }
  }
  
  output <- list(
    plotList = plotList,
    residuals = list(
      summStats
    ),
    predictions = list(
      predsSumStats
    )
  )
  
  return(output)
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

