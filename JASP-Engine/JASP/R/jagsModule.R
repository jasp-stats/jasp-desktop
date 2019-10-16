#
# Copyright (C) 2013-2019 University of Amsterdam
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

JAGS <- function(jaspResults, dataset, options, state = NULL) {

  # check model
  options <- .JAGSInitOptions(jaspResults, options)
  dataset <- .JAGSReadData   (jaspResults, options)

  # run model or update model
	mcmcResult <- .JAGSrunMCMC(jaspResults, dataset, options)

	# create output
	.JAGSoutputTable(jaspResults, options, mcmcResult)
	.JAGSmcmcPlots  (jaspResults, options, mcmcResult)

  return()

}

.JAGSrunMCMC <- function(jaspResults, dataset, options) {

	if (!is.null(jaspResults[["stateMCMC"]])) {
	  obj <- jaspResults[["stateMCMC"]]$object

	  # if parametersShown changed, update objects.
	  # else if parametersMonitored changed, check if we need to sample again.

	  return(obj)

	}
	if (!options[["goodModel"]] || jaspResults[["mainContainer"]]$getError())
	  return(NULL)

	model <- options[["model"]]

	if (options[["hasData"]]) {
		pattern <- colnames(dataset)
		names(pattern) <- paste0("\\b", .unv(pattern), "\\b")

		modelb64 <- stringr::str_replace_all(
			string  = model,
			pattern = pattern
		)
	} else {
		modelb64 <- model
		pattern <- NULL
	}

	# TODO: uncomment these before merge in JASP!
	# location <- .fromRCPP(".requestTempFileNameNative", ".txt")
	# modelFile <- file.path(location$root, location$relativePath)
	modelFile <- tempfile(pattern = "jagsModel", fileext = ".txt")
	print(modelFile)
	fileConn <- file(modelFile)
	writeLines(modelb64, fileConn)
	close(fileConn)

	noSamples        <- options[["noSamples"]]
	noBurnin         <- options[["noBurnin"]]
	noThinning       <- options[["noThinning"]]
	noChains         <- options[["noChains"]]
	deviance         <- options[["monitorDIC"]]
	parametersToSave <- options[["parametersToSave"]]

	datList <- as.list(dataset)
	if (options[["hasData"]]) {

		deviance <- TRUE

		# convention: deviance is first parameter!
		if (!("deviance" %in% parametersToSave))
			parametersToSave <- c("deviance", parametersToSave)

		rjags::load.module("dic", quiet = TRUE)
	} else {
		deviance <- FALSE
	}

	# Evaluate user R code, terminate early if the code doesn't work
	inits    <- .JAGSreadRcode(jaspResults, options[["initialValues"]], type = "initial values", noChains = options[["noChains"]])
	if (jaspResults[["mainContainer"]]$getError()) return(NULL)
	if (all(lengths(inits) == 0L)) inits <- NULL
	userData <- .JAGSreadRcode(jaspResults, options[["userData"]], type = "data")
	if (jaspResults[["mainContainer"]]$getError()) return(NULL)
	if (all(lengths(userData) == 0L)) userData <- NULL

	if (any(names(userData) %in% names(datList))) {
	  commonNames <- intersect(names(userData), names(datList))
    jaspResults[["mainContainer"]]$setError(paste(
      "The following names appeared both in the data set and in the user specified data:\n",
      commonNames
    ))
    return(NULL)
	} else {
	  datList <- c(datList, userData)
	}

	# this code is similar to how R2jags does it, but with
	# a try around it.
	e <- try({

		# compile model
		model <- rjags::jags.model(
			file     = modelFile,
			n.chains = noChains,
			n.adapt  = 0L,
			data     = datList,
			inits    = inits#unname(lapply(inits, list))
		)

		# sample burnin
		rjags::adapt(
			object         = model,
			n.iter         = noBurnin,
			by             = 0L,
			progress.bar   = "none",
			end.adaptation = TRUE
		)

		# sample remainder
		samples <- rjags::coda.samples(
			model          = model,
			variable.names = parametersToSave,
			n.iter         = noSamples,
			thin           = noThinning,
			by             = 0L,
			progress.bar   = "none"
		)

		# rjags::dic.samples(model, noSamples)

		fit <- coda:::summary.mcmc.list(samples, quantiles = c(0.025, 0.5, 0.975))
		neff <- coda::effectiveSize(samples)

		# if we only one have one parameters, ensure objects are still matrices with rownames, etc.
		if (length(parametersToSave) == 1L) {
			fit$statistics <- matrix(fit$statistics, 1L, dimnames = list(parametersToSave, names(fit$statistics)))
			fit$quantiles  <- matrix(fit$quantiles,  1L, dimnames = list(parametersToSave, names(fit$quantiles)))
		}
		fit$summary <- cbind(fit$statistics, fit$quantiles, neff)

		# construct parameter list
		allParams <- rownames(fit[["statistics"]])
		params <- vector("list", length = length(parametersToSave))
		names(params) <- parametersToSave
		for (p in parametersToSave) {
		  idx <- allParams == p
		  if (!any(idx))
		    idx <- startsWith(allParams, paste0(p, "["))
		  if (any(idx))
		    params[[p]] <- allParams[idx]
		}
	})

	# if something went wrong, present useful error message
	if (JASP:::isTryError(e)) {
	  jaspResults[["mainContainer"]]$setError(.JAGSmodelError(e, pattern, model, options))
	  return(NULL)
	}

	out <- list(
		model              = model,
		BUGSoutput         = fit,
		parameters.to.save = parametersToSave,
		model.file         = modelFile,
		n.iter             = noSamples,
		DIC                = deviance,
		samples            = samples,
		hasUserData        = !is.null(userData),
		params             = params
	)

	tmp <- createJaspState(object = out)
	tmp$dependOn(c("model", "noSamples", "noBurnin", "noThinning", "noChains", "initialValues", "userData", "showResultsFor"))
	if (options[["showResultsFor"]] == "monitorAllParameters")
	  tmp$dependOn("parametersShown")
	else
	  tmp$dependOn("monitoredParametersList")
	jaspResults[["stateMCMC"]] <- tmp

	return(out)

}

.JAGSisEmptyModel <- function(model) {
  model <- gsub("\\s", "", model)
  return(model == "model{}" || model == "")
  # regex <- stringr::str_extract(model, "(?<=model\\{\n)(.*)(?=\n\\})")
  # bool1 <- !identical(trimws(model), "")
  # bool2 <- (is.na(regex) || trimws(regex) != "")
  # return(bool1 && bool2)
}

.JAGSInitOptions <- function(jaspResults, options) {

  if (is.null(jaspResults[["mainContainer"]])) {
    # setup outer container with all common dependencies
    mainContainer <- createJaspContainer(dependencies = c("model", "noSamples", "noBurnin", "noThinning", "noChains",
                                                          "parametersMonitored", "parametersShown", "initialValues", "userData"))
    mainContainer$addCitation(.JAGSCitations)
    jaspResults[["mainContainer"]] <- mainContainer
  }

  # checks and sets errors
  .JAGSCheckJAGSInstallation(jaspResults[["mainContainer"]])
  if (jaspResults[["mainContainer"]]$getError()) {
    options[["goodModel"]] <- FALSE
    return(options)
  }

  # we don't need most if this if QML could return more information..
  model <- trimws(options[["model"]])
  tmp <- .JAGSGetModelParamsAndData(model)
  possibleParams <- tmp$possibleParams
  options[["colNames"]]       <- tmp$colNames
  options[["possibleTypos"]]  <- tmp$possibleTypos
  options[["possibleParams"]] <- possibleParams
  options[["possibleData"]]   <- tmp$possibleData
  options[["hasData"]]        <- length(options[["possibleData"]]) > 0L

  options[["parametersToShow"]] <- options[["parametersShown"]]
  options[["parametersToSave"]] <- options[["parametersShown"]]

  if (is.list(options[["parametersToShow"]])) {
    options[["bayesplot"]] <- list(
      pars = unlist(options[["parametersToShow"]]),
      regex_pars = character()
    )
  } else {
    options[["bayesplot"]] <- list(
      pars = character(),
      regex_pars = options[["parametersToShow"]]
    )
  }

	if (options[["monitorDeviance"]])
		options[["parametersToSave"]] <- c("deviance", options[["parametersToSave"]])

  # user specified monitoring?
  manualMonitor   <- options[["showResultsFor"]] == "monitorSelectedParameters"
  nParamAvailable <- length(possibleParams)
  # sum because only parameters can be assigned only once
  if (manualMonitor) {
    nParamMonitored <- length(options[["monitoredParametersList"]])
    # nParamAvailable <- length(options[["monitoredParametersList"]]) + length(options[["parametersList"]])
  } else {
    nParamMonitored <- length(options[["parametersShown"]])
    # nParamAvailable <- length(possibleParams)length(options[["parametersShown"]]) + length(options[["monitoredParametersList2"]])
  }
  nParamShown <- length(options[["parametersShown"]])

  options[["goodModel"]] <- TRUE
  if (.JAGSisEmptyModel(model)) {
    options[["goodModel"]] <- FALSE
  } else if (nParamAvailable > 0L) {
    if (manualMonitor && nParamMonitored == 0L) {
      options[["goodModel"]] <- FALSE
      options[["monitorWarning"]] <- "Please specify which parameters to monitor!"
    } else if (( manualMonitor && nParamMonitored > 0L && nParamShown == 0L) ||
               (!manualMonitor && nParamShown == 0L)) {
      options[["goodModel"]] <- FALSE
      options[["monitorWarning"]] <- "Please specify which parameters to show output for!"
    }
  }

  options[["legendposition"]] <- if (options[["showLegend"]]) "right" else "none"

  return(options)
}

.JAGSReadData <- function(jaspResults, options) {

  if (jaspResults[["mainContainer"]]$getError() || !options[["goodModel"]] || !options[["hasData"]])
    return(NULL)

  varsToRead <- options[["possibleData"]]
  dataset <- .readDataSetToEnd(columns.as.numeric = varsToRead)
  return(dataset)
}

# Tables ----
.JAGSoutputTable <- function(jaspResults, options, mcmcResult) {

	tb <- createJaspTable("MCMC summary")
	tb$position <- 1L
	ovt  <- "95% Credible Interval"
	ovt2 <- "Rhat"
	tb$addColumnInfo(name = "parameter", title = "Parameter",            type = "string")
	tb$addColumnInfo(name = "Mean",      title = "mean",                 type = "number")
	tb$addColumnInfo(name = "SD",        title = "sd",                   type = "number")
	tb$addColumnInfo(name = "50%",       title = "median",               type = "number")
	tb$addColumnInfo(name = "2.5%",      title = "Lower",                type = "number", overtitle = ovt)
	tb$addColumnInfo(name = "97.5%",     title = "Upper",                type = "number", overtitle = ovt)
	tb$addColumnInfo(name = "rhatPoint", title = "Point est.",           type = "number", overtitle = ovt2)
	tb$addColumnInfo(name = "rhatCI",    title = "Upper CI",             type = "number", overtitle = ovt2)
	tb$addColumnInfo(name = "neff",      title = "Effecive Sample Size", type = "number")

	if (!is.null(mcmcResult) && !jaspResults[["mainContainer"]]$getError()) {

		if (!options[["hasData"]] && !mcmcResult[["hasUserData"]])
			tb$addFootnote(message = "No data was supplied, everything was sampled from the priors!", symbol = .JAGSWarningSymbol)

		parametersToShow <- options[["parametersToShow"]]
		sum <- mcmcResult[["BUGSoutput"]][["summary"]]
		nms <- rownames(sum)
		if (identical(parametersToShow, "$ALL")) {
		  idx <- rep(TRUE, length(nms))
		} else if (is.list(parametersToShow)) {
		  idx <- logical(length(nms))
		  origParametersToShow <- names(parametersToShow)
		  for (i in seq_along(parametersToShow)) {
		    if (grepl("[", origParametersToShow[i], fixed = TRUE)) {
		      # subset, match literal
		      idx <- idx | (nms %in% unlist(parametersToShow[[i]]))
		    } else {
		      # complete set, match start of parameter
		      idx <- idx | (stringr::str_detect(nms, paste0("\\b", parametersToShow[[i]], "\\b")))
		    }
		  }
		} else {
		  # parameters to show does not contain mu[], extract without [] and match literal
		  nms2 <- sapply(stringr::str_extract_all(nms, "\\w+"), `[[`, 1L)
		  idx <- nms2 %in% parametersToShow
		}


		tb[["parameter"]] <- nms[idx]
		for (name in c("Mean", "SD", "50%", "2.5%", "97.5%", "neff"))
			tb[[name]] <- sum[idx, name]

		noChains <- options[["noChains"]]
		if (noChains > 1) {

			rhat <- coda::gelman.diag(mcmcResult[["samples"]])

			tb[["rhatPoint"]] <- rhat[["psrf"]][idx, 1L]
			tb[["rhatCI"]]    <- rhat[["psrf"]][idx, 2L]
			if (!is.null(rhat[["mpsrf"]])) {
				tb$addFootnote(message = sprintf(
					"The multivariate potential scale reduction factor is estimated at %.3f.",
					rhat[["mpsrf"]]
				))
			}
		} else {
			tb$addFootnote(message = paste(
				"Rhat statistic cannot be computed for only one chain.",
				"It is strongly recommoned to run more than one chain to assess MCMC convergence!"
			))
		}
	}

	if (!is.null(options[["monitorWarning"]]))
	  tb$addFootnote(message = options[["monitorWarning"]], symbol = .JAGSWarningSymbol)

	jaspResults[["mainContainer"]][["mainTable"]] <- tb

	return()
}

# Plots ----
.JAGSmcmcPlots <- function(jaspResults, options, mcmcResult) {

  if (is.null(jaspResults[["mainContainer"]][["plotContainer"]])) {
    plotContainer <- createJaspContainer(dependencies = c("parametersShown", "colorScheme"))
    # jaspResults[["mainContainer"]][["plotContainer"]] <- plotContainer
  } else {
    plotContainer <- jaspResults[["mainContainer"]][["plotContainer"]]
  }

  params <- .JAGSGetParams(options, mcmcResult)
  containerObj <- .JAGSInitPlotsContainers(plotContainer, options, params)
  if (is.null(containerObj))
    return()

  # put the container in jaspResults only now so that all empty plots appear at once
  if (is.null(jaspResults[["mainContainer"]][["plotContainer"]]))
    jaspResults[["mainContainer"]][["plotContainer"]] <- plotContainer

  colorpalette <- options[["colorScheme"]]
  oldColorpalette <- JASPgraphs::getGraphOption("palette")
  on.exit(JASPgraphs::setGraphOption("palette", oldColorpalette))
  JASPgraphs::setGraphOption("palette", colorpalette)

  if (!(is.null(mcmcResult) || jaspResults[["mainContainer"]][["plotContainer"]]$getError()))
    .JAGSFillPlotContainers(containerObj, options, mcmcResult)

  .JAGSPlotBivariateScatter(plotContainer, options, mcmcResult)

}

.JAGSInitPlotsContainers <- function(plotContainer, options, params) {

  # for each plot function, create an empty plot container (or the empty plot object)
  output <- NULL
  if (options[["plotDensity"]]) {

    add <- list("function" = ".JAGSPlotDensity")
    if (is.null(plotContainer[["plotDensity"]])) {
      add[["container"]] <- createJaspContainer(title = "Marginal Density",  position = 1,
                                                dependencies = c("plotDensity", "aggregateChains"))
      plotContainer[["plotDensity"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotDensity"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotHistogram"]]) {

    add <- list("function" = ".JAGSPlotHistogram")
    if (is.null(plotContainer[["plotHistogram"]])) {
      add[["container"]] <- createJaspContainer(title = "Marginal Histogram",  position = 2,
                                                dependencies = c("plotHistogram", "aggregateChains"))
      plotContainer[["plotHistogram"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotHistogram"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotTrace"]]) {

    add <- list("function" = ".JAGSPlotTrace")
    if (is.null(plotContainer[["plotTrace"]])) {
      add[["container"]] <- createJaspContainer(title = "Trace Plots",  position = 3,
                                                dependencies = c("plotTrace"))
      plotContainer[["plotTrace"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotTrace"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotAutoCor"]]) {

    add <- list("function" = ".JAGSPlotAutoCor")
    if (is.null(plotContainer[["plotAutoCor"]])) {
      add[["container"]] <- createJaspContainer(title = "Autocorrelation Plots",  position = 4,
                                                dependencies = c("plotAutoCor", "noLags", "acfType"))
      plotContainer[["plotAutoCor"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotAutoCor"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotBivarHex"]] && is.null(plotContainer[["plotBivarHex"]])) {

    jaspPlot <- createJaspPlot(title  = "Bivariate Scatter Plot",  position = 5,
                               dependencies = c("plotBivarHex", "parametersShown", "bivariateScatterDiagType",
                                                "bivariateScatterOffDiagType"))
    plotContainer[["plotBivarHex"]] <- jaspPlot

  }

  for (i in seq_along(output))
    .JAGSInitContainerWithPlots(output[[i]][["container"]], params)

  return(output)
}

.JAGSInitContainerWithPlots <- function(jaspContainer, params) {

  if (is.null(params) || length(params) == 0L) {
    # if there are no parameters, we just show a single empty plot
    jaspContainer[["temp"]] <- createJaspPlot()

  } else {

    baseParams <- names(params)
    for (j in seq_along(params))
      for (param in params[[j]])
        if (is.null(jaspContainer[[param]])) {
          jaspPlot <- createJaspPlot(title = param)
          jaspPlot$dependOn(optionContainsValue = list(parametersShown = param))
          jaspContainer[[param]] <- jaspPlot
        }
  }
}

.JAGSFillPlotContainers <- function(containerObj, options, mcmcResult) {

  params  <- mcmcResult[["params"]]
  samples <- mcmcResult[["samples"]]

  baseParams <- names(params)
  for (i in seq_along(containerObj)) {
    jaspContainer <- containerObj[[i]][["container"]]
    plotFun       <- get(containerObj[[i]][["function"]], mode = "function")
    for (j in seq_along(params))
      for (param in params[[j]])
        if (!is.null(jaspContainer[[param]]) && is.null(jaspContainer[[param]]$plotObject)) {
          jaspContainer[[param]]$status     <- "running"
          jaspContainer[[param]]$plotObject <- plotFun(samples, param, options)
        }
  }
}

.JAGSPlotDensity <- function(samples, param, options, removeAxisLabels = FALSE) {

  npoints <- 2^10 # precision for density estimation
  if (options[["aggregateChains"]]) {
    df <- do.call(rbind.data.frame, lapply(seq_along(samples), function(i) {
      d <- density(samples[[i]][, param], n = npoints)[c("x", "y")]
      return(data.frame(x = d[["x"]], y = d[["y"]], g = factor(i)))
    }))
    mapping <- ggplot2::aes(x = x, y = y, color = g)
    colorScale <- JASPgraphs::scale_JASPcolor_discrete(name = "Chain")
  } else {
    n <- nrow(samples[[1L]])
    d <- density(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE))
    df <- data.frame(x = d[["x"]], y = d[["y"]])
    mapping <- ggplot2::aes(x = x, y = y)
    colorScale <- NULL
  }
  if (removeAxisLabels)
    labs <- ggplot2::labs(x = NULL, y = NULL)
  else
    labs <- ggplot2::labs(x = param, y = "Density")

  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(df, mapping) +
      ggplot2::geom_line(show.legend = options[["aggregateChains"]]) +
      labs +
      colorScale, legend.position = options[["legendposition"]]#c(.85, .85)
  )
  return(g)
}

.JAGSPlotHistogram <- function(samples, param, options, removeAxisLabels = FALSE) {

  # TODO: get parameter bounds and respect these, e.g., truncate [0, 1] (probably pretty hard though)
  npoints <- 2^10 # precision for density estimation
  if (options[["aggregateChains"]]) {
    df <- do.call(rbind.data.frame, lapply(seq_along(samples), function(i) {
      d <- hist(samples[[i]][, param], plot = FALSE)
      return(data.frame(x = d[["mids"]], y = d[["counts"]], g = factor(i)))
    }))
    mapping <- ggplot2::aes(x = x, y = y, color = g, fill = g)
    colorScale <- JASPgraphs::scale_JASPcolor_discrete(name = "Chain")
    fillScale  <- JASPgraphs::scale_JASPfill_discrete(name = "Chain")
  } else {
    n <- nrow(samples[[1L]])
    d <- hist(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE), plot = FALSE)
    df <- data.frame(x = d[["mids"]], y = d[["counts"]])
    mapping <- ggplot2::aes(x = x, y = y)
    fillScale <- colorScale <- NULL
  }

  if (removeAxisLabels)
    labs <- ggplot2::labs(x = NULL, y = NULL)
  else
    labs <- ggplot2::labs(x = param, y = "Counts")

  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(df, mapping) +
      ggplot2::geom_col(show.legend = options[["aggregateChains"]], position = ggplot2::position_dodge()) +
      labs + colorScale + fillScale,
    legend.position = options[["legendposition"]]# c(.85, .85)
  )
  return(g)
}

.JAGSPlotTrace <- function(samples, param, options) {

  n <- nrow(samples[[1L]])
  df <- data.frame(
    x = rep(seq_len(n), length(samples)),
    y = unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE),
    g = factor(rep(seq_along(samples), each = n))
  )

  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = g)) +
      ggplot2::geom_line(show.legend = FALSE) +
      ggplot2::labs(x = "Iteration", y = param) +
      JASPgraphs::scale_JASPcolor_discrete()
  )
  return(g)
}

.JAGSPlotAutoCor <- function(samples, param, options) {

  # TODO: x coordinates should be based on acf()#n.used!
  nchains  <- length(samples)
  nlags    <- options[["noLags"]] + 1L
  acfs <- numeric(nchains * nlags)
  for (i in seq_len(nchains))
    acfs[(1 + (i-1) * nlags):(i * nlags)] <- c(stats::acf(x = samples[[i]][, param], type = "correlation",
                                                          lag.max = nlags - 1L, plot = FALSE)$acf)

  df <- data.frame(
    x = seq(0:(nlags - 1L)),
    y = acfs,
    g = factor(rep(seq_len(nchains), each = nlags))
  )

  if (options[["acfType"]] == "acfLines") {
    geom <- ggplot2::geom_line()
  } else {
    geom <- ggplot2::geom_col(position = ggplot2::position_dodge())
  }
  colorScale <- JASPgraphs::scale_JASPcolor_discrete(name = "Chain")
  fillScale  <- JASPgraphs::scale_JASPfill_discrete(name = "Chain")

  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, color = g, group = g, fill = g)) +
      geom + colorScale + fillScale + ggplot2::labs(x = "Lag", y = "Autocorrelation"),
    legend.position = options[["legendposition"]]#c(.85, .85)
  )
  return(g)
}

.JAGSPlotBivariateScatter <- function(plotContainer, options, mcmcResult) {

  if (is.null(plotContainer[["plotBivarHex"]]) || !is.null(plotContainer[["plotBivarHex"]]$plotObject))
    return()

  jaspPlot <- plotContainer[["plotBivarHex"]]
  if (length(options[["parametersToShow"]]) >= 2L) {
    jaspPlot$width  <- sum(lengths(mcmcResult[["params"]])) * 320L
    jaspPlot$height <- sum(lengths(mcmcResult[["params"]])) * 320L
    jaspPlot$plotObject <- .JAGSPlotBivariateMatrix(options, mcmcResult)
  } else {
    jaspPlot$setError("At least two parameters need to be monitored and shown to make a bivariate scatter plot!")
  }
}

.JAGSPlotBivariateMatrix <- function(options, mcmcResult) {

  samples <- mcmcResult[["samples"]]
  allParams <- unlist(mcmcResult[["params"]])
  nParams <- length(allParams)
  plotMatrix <- matrix(list(), nParams, nParams, dimnames = list(allParams, allParams))

  # these should always be false for the matrix plot
  options[["aggregateChains"]] <- FALSE
  options[["showLegend"]]      <- FALSE

  for (j in seq_len(nParams)) {
    for (i in seq_len(nParams)) {
    # for (i in i:nParams) {
      if (i == j) {
        if (options[["bivariateScatterDiagType"]] == "dens") {
          plotMatrix[[i, j]] <- .JAGSPlotDensity(samples, allParams[[j]], options, removeAxisLabels = TRUE)
        } else {
          plotMatrix[[i, j]] <- .JAGSPlotHistogram(samples, allParams[j], options, removeAxisLabels = TRUE)
        }
      } else {#if (i > j) {
          plotMatrix[[i, j]] <- .JAGSPlotHexOrScatter(samples, allParams[i], allParams[j],
                                                      type = options[["bivariateScatterOffDiagType"]])
      # } else {
        # TODO: do we want to show anything else for i > j?
      }
    }
  }
  return(JASPgraphs::ggMatrixPlot(plotMatrix))
}

.JAGSPlotHexOrScatter <- function(samples, paramX, paramY, type, removeAxisLabels = TRUE) {

  n <- nrow(samples[[1L]])
  df <- data.frame(
    x = unlist(lapply(samples, `[`, i = 1:n, j = paramX), use.names = FALSE),
    y = unlist(lapply(samples, `[`, i = 1:n, j = paramY), use.names = FALSE)
  )

  if (type == "hex") {
    geom <- ggplot2::stat_bin_hex()
    mapping = ggplot2::aes(x = x, y = y, fill = ..density..)
    scaleFill <- JASPgraphs::scale_JASPfill_continuous()
    scaleCol  <- NULL
  } else {
    geom <- ggplot2::stat_density_2d(mapping = ggplot2::aes(fill = stat(level)), geom = "polygon")
    mapping <- ggplot2::aes(x = x, y = y)
    scaleFill <- JASPgraphs::scale_JASPfill_continuous()
    scaleCol  <- JASPgraphs::scale_JASPcolor_continuous()
  }
  labs <- NULL
  if (removeAxisLabels)
    labs <- ggplot2::labs(x = NULL, y = NULL)

  return(JASPgraphs::themeJasp(ggplot2::ggplot(data = df, mapping = mapping) + geom + labs + scaleFill + scaleCol))

}

# Errors ----
.extractJAGSErrorMessage <- function(error) {
  split <- base::strsplit(as.character(error), "\n")[[1]]
  return(trimws(paste(split[-1L], collapse = "\n")))
}

.JAGSmodelError <- function(error, pattern, model, options) {

  if (options[["hasData"]]) {
    revPattern <- names(pattern)
    names(revPattern) <- pattern

    # change base64 to normal variable names
    errorMessage <- stringr::str_replace_all(
      string  = .extractJAGSErrorMessage(error),
      pattern = revPattern
    )
  } else {
    errorMessage <- .extractJAGSErrorMessage(error)
  }

  if (!is.null(unlist(options[["possibleTypos"]]))) {

    toAdd <- NULL
    idx <- stringr::str_detect(errorMessage, paste0("\\b", options[["possibleParams"]], "\\b"))
    nmax <- max(nchar(options[["possibleParams"]][idx]))
    if (any(idx)) {

      toAdd <- paste0("Model", strrep(" ", nmax - 5L), " | Data\n")
      toAdd <- paste0(toAdd, strrep("-", nchar(toAdd)), "\n")

      for (i in which(idx)) {
        toAdd <- paste0(toAdd,
                        options[["possibleParams"]][[i]],
                        " | ",
                        paste(options[["possibleTypos"]][[i]], collapse = ", "),
                        "\n"
        )
      }
    }

    if (!is.null(toAdd))
      errorMessage <- paste0(errorMessage, "\n\nPossible typos detected:\n\n", toAdd)

  }

  # perhaps some helpfull checks...
  chars <- stringr::fixed(c("[", "]", "{", "}", "(", ")"))
  counts <- stringr::str_count(model, chars)
  toAdd <- paste(
    .JAGSmodelErrorString(counts[1:2], chars[1:2]),
    .JAGSmodelErrorString(counts[3:4], chars[3:4]),
    .JAGSmodelErrorString(counts[5:6], chars[5:6])
  )

  if (length(toAdd) > 0L)
    errorMessage <- paste0(errorMessage, "\n\nIn addition:\n", toAdd)

  # return error message
  return(errorMessage)
}

.JAGSmodelErrorString <- function(counts, chars) {
  if (counts[1L] == counts[2L]) return(NULL)

  if (counts[1L] < counts[2L]) {
    counts <- counts[2:1]
    chars <- chars[2:1]
  }
  return(sprintf(
    "The model contains more '%s' than '%s' (%d vs %d)",
    chars[1L], chars[2L], counts[1L], counts[2L]
  ))
}

.JAGSCheckJAGSInstallation <- function(jaspContainer) {
  # NOTE: this function does setError if loadNamespace("rjags") fails.
  # Thus, all other calls to rjags:: should be preceded by $getError().
  e <- try(loadNamespace("rjags"), silent = TRUE)
  if (isTryError(e)) {
    # Sys.getenv() returns "" if nothing was found
    jaspContainer$setError(paste0("There was a problem loading JAGS, JAGS_HOME is: ", Sys.getenv("JAGS_HOME"), ".\nPlease contact the JASP team for support.\nError was: ", e))
  } else if (isTRUE(rjags::jags.version() < "4.3.0")) {
    jaspContainer$setError(paste("Expected JAGS version 4.3.0 but found", as.character(rjags::jags.version())))
  }
  return(NULL)
}


# Helper functions ----
.JAGSGetParams <- function(options, mcmcResult) {

  if (!is.null(mcmcResult))
    return(mcmcResult[["params"]])

  params <- options[["bayesplot"]][["pars"]]
  if (is.null(params))
    params <- options[["bayesplot"]][["regex_pars"]]
  if (is.null(params))
    return(NULL)
  obj <- as.list(params)
  names(obj) <- params
  return(obj)

}

.JAGSGetModelParamsAndData <- function(model) {

  # TODO: this is already done in qml? pass it as argument?
  # we just need to know which parameters are column names in the data set.
  header <- .readDataSetHeader(all.columns = TRUE)
  cnms <- colnames(header)
  hasCnms <- !(is.null(cnms) || length(cnms) == 0L)
  if (hasCnms) {
    cnms <- .unv(cnms)
    colNames <- cnms
  }

  # otherwise empty column names break stuff down the road
  cnms <- cnms[cnms != ""]

  # get everything before a '~'
  possibleParams <- stringr::str_extract_all(model, ".*(?=\\s~)")[[1L]]
  # omit empty matches
  possibleParams <- possibleParams[possibleParams != ""]
  # remove superfluous whitespace (tabs)
  possibleParams <- stringr::str_trim(possibleParams)
  # change model{mu to mu
  if (grepl("\\{", possibleParams[1L]))
    possibleParams[1L] <- sub("model\\s*\\{\\s*", "", possibleParams[1L])
  # change mu[i] to mu
  possibleParams <- unique(sapply(stringr::str_extract_all(possibleParams, "\\w+"), `[[`, 1L))

  if (hasCnms) {

    # check for each element in possibleParams whether it matches any in cnms.
    # matches like contNormal[i] are matched, while contNormala[i] will not be matched.
    idx <- logical()
    for (param in possibleParams)
      idx[param] <- any(stringr::str_detect(param, paste0("\\b", cnms, "\\b")))

    possibleData   <- possibleParams[idx]
    possibleParams <- possibleParams[!idx]

    # note: the loop below can be sped up using the C implementation from stringdist::amatch
    r <- vector("list", length(possibleParams))
    names(r) <- possibleParams
    for (i in seq_along(cnms)) {

      ii <- agrep(cnms[i], possibleParams)
      for (j in ii)
        r[[j]] <- c(r[[j]], cnms[i])
    }
    possibleTypos <- r

  } else {
    possibleData <- NULL
  }
  return(list(possibleParams = possibleParams, possibleData = possibleData,
              possibleTypos = possibleTypos, colNames = colNames))
}

.JAGSisPureNumber <- function(x) suppressWarnings(!is.na(as.numeric(x)))

.JAGSreadRcode <- function(jaspResults, input, type = c("initial values", "data"), noChains = 1L) {

  type <- match.arg(type)
  paramNms <- unlist(input[[1L]][["values"]])
  rcodes   <- unlist(input[[2L]][["values"]])

  output <- vector("list", length = noChains)
  for (j in seq_len(noChains)) {
    oneOutput <- vector("list", length = length(paramNms))
    names(oneOutput) <- paramNms
    for (i in seq_along(oneOutput)) {
      string <- rcodes[i]
      if (is.null(string) || string == "" || string == "...") { # this shouldn't be possible, but if string = NULL, parse prompts for user input.
        next
        # jaspResults[["mainContainer"]]$setError("The R code for %s was NULL!", type)
        # return()
      }
      obj <- try(eval(parse(text = string)))
      if (JASP:::isTryError(obj)) {
        jaspResults[["mainContainer"]]$setError(sprintf("The R code for %s crashed with error:\n%s",
                                                       type, JASP:::.extractErrorMessage(obj)))
        return()
      } else if (!is.numeric(obj)) {
        jaspResults[["mainContainer"]]$setError("The result of %s R code should be numeric but it was of mode %s and class %s",
                                                type, mode(obj), paste(class(obj), collapse = ","))
        return()
      } else {
        oneOutput[[i]] <- obj
      }
    }
    if (any(lengths(oneOutput) > 0L))
      output[[j]] <- oneOutput
  }
  if (type == "data")
    output <- output[[1L]]
  return(output)
}

.JAGSWarningSymbol <- "&#9888;"

# References ----
# citation("rjags")
# citation("coda")
# citation("stringr")
.JAGSCitations <- c(
  "rjags"   = "Martyn Plummer (2018). rjags: Bayesian Graphical Models using MCMC. R package version 4-8. https://CRAN.R-project.org/package=rjags",
  "coda"    = "Martyn Plummer, Nicky Best, Kate Cowles and Karen Vines (2006). CODA: Convergence Diagnosis and Output Analysis for MCMC, R News, vol 6, 7-11",
  "stringr" = "Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr",
  "JAGS"    = "Plummer, Martyn. (2003). JAGS: A Program for Analysis of Bayesian Graphical Models using Gibbs Sampling. 3rd International Workshop on Distributed Statistical Computing (DSC 2003); Vienna, Austria. 124."
)

# useful!
# rjags::jags.version()

# TODO long term: ----
# - get parameter bounds and respect these in the plots and density estimation, e.g., truncate [0, 1] (probably pretty hard though).
# - pass more information from QML to R to cleanup the code?
#

# old code for .JAGSInitOptions ----
# old approach
# parameters to store samples from in JAGS
# if (identical(options[["parametersMonitored"]], "$ALL")) {
#   # special keyword - monitor all parameters
# 	options[["parametersToSave"]] <- possibleParams
# } else if (identical(options[["parametersMonitored"]], "")) {
#   options[["goodModel"]] <- FALSE
#   return(options)
# } else { # user specified - check for errors
#
#   # check if parameters to monitor are a subset of possibleParams
#   # change input string into sepate words
#   paramsToSave <- stringr::str_extract_all(options[["parametersMonitored"]], "\\w+")[[1]]
#   diff <- setdiff(paramsToSave, possibleParams) # any mismatches?
#   if (length(diff) > 0L) {
#     msg <- paste0(
#       "The following parameter(s) should be monitored but do not appear in the model!\n",
#       "This happened for:\n\n", paste0(diff, collapse = ", ")
#     )
#     jaspResults[["mainContainer"]]$setError(msg)
#     return(options)
#   } else {
#     # monitors user specified parameters
#     options[["parametersToSave"]] <- paramsToSave
#   }
# }
#
# parameters to actually display stuff for in JASP
# if (identical(options[["parametersShown"]], "$ALL")) {
# 	options[["parametersToShow"]] <- "$ALL"
# } else if (!identical(options[["parametersShown"]], "")) { # do some checks..
#
# 	paramsShown <- options[["parametersShown"]]
# 	# change mu[i] to mu
# 	paramsShownBase <- unlist(stringr::str_extract_all(paramsShown, "\\w+"))
#   paramsShownBase <- unique(paramsShownBase[!.JAGSisPureNumber(paramsShownBase)])
# 	# check if all parameters to show are actually monitored
# 	diff <- setdiff(paramsShownBase, options[["parametersToSave"]])
# 	if (length(diff) > 0) {
# 	  msg <- paste0(
# 	    "The following parameter(s) should be shown but are not monitored!\n",
# 	    "This happened for:\n\n", paste(diff, collapse = ", ")
# 	  )
# 	  jaspResults[["mainContainer"]]$setError(msg)
# 	  return(options)
# 	}
#
#
# 	if (all(!grepl("[", paramsShown, fixed = TRUE))) {
# 	  # no subsets! just save the bases
# 	  options[["parametersToShow"]] <- paramsShownBase
# 	} else {
# 	  # save subsets of parameters -- split into subset and non subsets
#
# 	  paramsShown <- unlist(stringr::str_split(paramsShown, " "))
# 		# everything between [...]
# 		paramsShownIdx <- stringr::str_extract_all(paramsShown, "(?<=\\[).+?(?=\\])")
# 		names(paramsShownIdx) <- paramsShown
# 		paramsShownNms <- paramsShownIdx
# 		for (i in seq_along(paramsShownIdx)) {
# 			s <- paramsShownIdx[[i]]
# 			if (length(s) > 0) {
#
# 				# some checks if the user supplied range is actually okay
#
# 				# negative indexing is not supported.
# 				if (grepl("-", s)) {
# 					# yell at user
# 					msg <- sprintf("Negative indexing is not allowed for parameters monitored!\n A problem occured with: %s",
# 												 paramsShown[i])
# 					jaspResults[["mainContainer"]]$setError(msg)
# 					return(options)
# 				}
#
# 			  # if (grepl(""))
#
# 				if (grepl(",", s)) {
# 					# split on , but not if it's inside parentheses ( ), to allow mu[c(1, 2, 3), 1:2]
# 					s <- stringr::str_split(s, "(?![^(]*\\)),")[[1]]
# 				}
#
#         # remove whitespace so we can pattern match easily
#         s <- gsub("[[:space:]]", "", s)
#
#         # match all allowed patterns
#         isIdx    <- suppressWarnings(!is.na(as.numeric(s)))         # sningle index
#         isColon  <- stringr::str_detect(s, "\\d:\\d")               # range  of indices
#         isVector <- stringr::str_detect(s, "c\\([\\d,]{1,}\\)")     # vector of indices
#         # isBind   <- stringr::str_detect(s, "cbind\\([\\d,]{1,}\\)") # different range of indices
#
#         # check if all s match at least one pattern
#         allMatch <- isIdx | isColon | isVector# | isBind
#         if (!all(allMatch)) {
#           msg <- paste0(
#             "Did not understand ", paramsShown[i], ". Input should be either:\n\n",
#             paste(
#               "a single index",
#               "a range of indices (1:2)",
#               "a vector of indices (c(1, 2, 3))",
#               sep = ", or\n"
#             )
#           )
# 					jaspResults[["mainContainer"]]$setError(msg)
# 					return(options)
#
#         }
#
#         # this could be done more nicely
#         allCombs <- try(do.call(expand.grid, lapply(s, function(x) eval(parse(text = x)))))
#         if (isTryError(allCombs)) {
#           msg <- paste0(
#             "Did not understand ", paramsShown[i], ". Input should be either:\n\n",
#             paste(
#               "a single index",
#               "a range of indices (1:2)",
#               "a vector of indices (c(1, 2, 3))",
#               sep = ", or\n"
#             )
#           )
# 					jaspResults[["mainContainer"]]$setError(msg)
# 					return(options)
#         }
#
#         names2lookup <- apply(allCombs, 1, function(x) {
#           paste0(paramsShownBase[i], "[", paste(x, collapse = ","), "]")
#         })
#
# 				paramsShownIdx[[i]] <- names2lookup
# 			} else {
# 				# sanity check -- even though there was no match between [...],
# 				# check that there is not a single '[' or ']'.
# 				str <- names(paramsShownIdx)[i]
# 				countLeft  <- stringr::str_count(str, stringr::fixed("["))
# 				countRight <- stringr::str_count(str, stringr::fixed("]"))
# 				if (countLeft != countRight) {
#
# 					msg <- sprintf("In parameters to show, %s has an additional '[' or ']' (%d vs %d)",
# 																str, countLeft, countRight)
# 					jaspResults[["mainContainer"]]$setError(msg)
# 					return(options)
#
# 				}
# 				paramsShownIdx[[i]] <- str
# 			}
# 		}
# 		options[["parametersToShow"]] <- paramsShownIdx
# 	}
# }


# Old plots ----
# .JAGSsetPlotTheme <- function(options) {
#   colorScheme <- switch(
#     options[["colorScheme"]],
#     "blue"    = "blue",
#     "gray"    = "gray",
#     "Viridis" = "viridis",
#     "brewer-Dark2"
#   )
#   bayesplot::color_scheme_set(colorScheme)
#   bayesplot::bayesplot_theme_set(new = JASPgraphs::themeJaspRaw())
#   return(NULL)
# }
#
# .JAGSplotMarginalDensity <- function(plotContainer, options, mcmcResult) {
#
# 	if (!options[["plotDensity"]] || !is.null(plotContainer[["plotMarginalDensity"]])) return()
#
# 	jaspPlot <- createJaspPlot(title  = "Marginal Density", position = 1,
# 	                           dependencies = c("plotDensity", "aggregateChains"))
# 	plotContainer[["plotMarginalDensity"]] <- jaspPlot
#   if (is.null(mcmcResult) || plotContainer$getError())
#     return()
#
# 	if (options[["aggregateChains"]]) {
# 	  plot <- bayesplot::mcmc_dens(mcmcResult[["samples"]],
# 	                               pars = options[["bayesplot"]][["pars"]],
# 	                               regex_pars = options[["bayesplot"]][["regex_pars"]]
# 	  )
# 	} else {
# 	  plot <- bayesplot::mcmc_dens_overlay(mcmcResult[["samples"]],
# 	                                       pars = options[["bayesplot"]][["pars"]],
# 	                                       regex_pars = options[["bayesplot"]][["regex_pars"]]
# 	  )
# 	}
# 	jaspPlot$plotObject <- plot + JASPgraphs::themeJaspRaw()
# 	return()
# }
#
# .JAGSplotMarginalHistogram <- function(plotContainer, options, mcmcResult) {
#
# 	if (!options[["plotHistogram"]] || !is.null(plotContainer[["plotMarginalHistogram"]])) return()
#
# 	jaspPlot <- createJaspPlot(title  = "Marginal Histogram",  position = 2,
# 	                           dependencies = c("plotHistogram", "aggregateChains"))
# 	plotContainer[["plotMarginalHistogram"]] <- jaspPlot
#   if (is.null(mcmcResult) || plotContainer$getError())
#     return()
#
# 	if (options[["aggregateChains"]]) {
# 	  plot <- bayesplot::mcmc_hist(mcmcResult[["samples"]],
# 	                               pars = options[["bayesplot"]][["pars"]],
# 	                               regex_pars = options[["bayesplot"]][["regex_pars"]]
# 	  )
# 	} else {
# 	  plot <- bayesplot::mcmc_hist_by_chain(mcmcResult[["samples"]],
# 	                                       pars = options[["bayesplot"]][["pars"]],
# 	                                       regex_pars = options[["bayesplot"]][["regex_pars"]]
# 	  )
# 	}
# 	jaspPlot$plotObject <- plot + JASPgraphs::themeJaspRaw()
# 	return()
# }
#
# .JAGSplotTrace <- function(plotContainer, options, mcmcResult) {
#
# 	if (!options[["plotTrace"]] || !is.null(plotContainer[["plotTrace"]])) return()
#
# 	jaspPlot <- createJaspPlot(title  = "Trace Plots",  position = 3,
# 	                           dependencies = c("plotTrace", "parametersShown"))
# 	plotContainer[["plotTrace"]] <- jaspPlot
#   if (is.null(mcmcResult) || plotContainer$getError())
#     return()
#
#   # plot$width <- 320 *
#   jaspPlot$plotObject <- bayesplot::mcmc_trace(mcmcResult[["samples"]],
#     pars = options[["bayesplot"]][["pars"]],
#     regex_pars = options[["bayesplot"]][["regex_pars"]]
#   ) + JASPgraphs::themeJaspRaw()
# 	return()
# }
#
# .JAGSplotAcf <- function(plotContainer, options, mcmcResult) {
#
# 	if (!options[["plotAutoCor"]] || !is.null(plotContainer[["plotAutoCor"]])) return()
#
# 	jaspPlot <- createJaspPlot(title = "Autocorrelation Plot",  position = 4,
# 	                           dependencies = c("plotAutoCor", "parametersShown", "noLags", "acfType"))
# 	plotContainer[["plotAutoCor"]] <- jaspPlot
#   if (is.null(mcmcResult) || plotContainer$getError())
#     return()
#
#   if (options[["acfType"]] == "acfLines") {
#     plotfun <- bayesplot::mcmc_acf
#   } else {
#     plotfun <- bayesplot::mcmc_acf_bar
#   }
#
#   jaspPlot$plotObject <- plotfun(
#     mcmcResult$samples,
#     pars = options[["bayesplot"]][["pars"]],
#     regex_pars = options[["bayesplot"]][["regex_pars"]],
#     lags = options[["noLags"]]
#   ) + JASPgraphs::themeJaspRaw()
# 	return()
# }
#

