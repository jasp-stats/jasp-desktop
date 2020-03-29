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
  .JAGSInit(jaspResults, options)
  dataset <- .JAGSReadData(jaspResults, options)

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

  if (!.JAGSgetGoodModel(jaspResults) || jaspResults[["mainContainer"]]$getError())
    return(NULL)

  model <- options[["model"]][["model"]]

  # TODO: uncomment these before merge in JASP!
  location <- .fromRCPP(".requestTempFileNameNative", ".txt")
  modelFile <- file.path(location$root, location$relativePath)
  # modelFile <- tempfile(pattern = "jagsModel", fileext = ".txt")
  print(modelFile)
  fileConn <- file(modelFile)
  writeLines(model, fileConn)
  close(fileConn)

  noSamples        <- options[["noSamples"]]
  noBurnin         <- options[["noBurnin"]]
  noThinning       <- options[["noThinning"]]
  noChains         <- options[["noChains"]]
  deviance         <- options[["monitorDIC"]]
  parametersToSave <- if (options[["showResultsFor"]] == "monitorAllParameters")
    options[["parametersShown"]]
  else
    options[["monitoredParametersList"]]

  datList <- as.list(dataset)

  error <- .JAGSloadModules(jaspResults)

  if (.JAGShasData(options)) {

    deviance <- TRUE
    # convention: deviance is first parameter!
    parametersToSave <- c("deviance", parametersToSave)

    #     if ("dic" %in% rjags::list.modules())
    # 		  rjags::load.module("dic", quiet = TRUE)
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
    jaspResults[["mainContainer"]]$setError(gettextf(
      "The following names appeared both in the data set and in the user specified data:\n%s",
      commonNames
    ))
    return(NULL)
  } else {
    datList <- c(datList, userData)
  }

  # set a seed (same procedure as R2jags)
  .setSeedJASP(options)
  RNGname <- "base::Wichmann-Hill"
  if (is.null(inits)) {
    inits <- vector("list", noChains)
    for (i in seq_len(noChains)) {
      inits[[i]]$.RNG.name <- RNGname
      inits[[i]]$.RNG.seed <- runif(1, 0, 2^31)
    }
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

    fit <- coda:::summary.mcmc.list(samples, quantiles = c(0.025, 0.5, 0.975))
    neff <- coda::effectiveSize(samples)

    # if we only one have one parameters, ensure objects are still matrices with rownames, etc.
    if (length(parametersToSave) == 1L && !is.matrix(fit$statistics)) {
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
  if (isTryError(e)) {
    jaspResults[["mainContainer"]]$setError(.JAGSmodelError(e, model, options))
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
  tmp$dependOn(c("model", "noSamples", "noBurnin", "noThinning", "noChains", "initialValues", "userData", "showResultsFor",
                 "setSeed", "seed"))
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
}

.JAGSInit <- function(jaspResults, options) {

  jaspResults$addCitation(.JAGSCitations)
  if (is.null(jaspResults[["mainContainer"]])) {
    # setup outer container with all common dependencies
    mainContainer <- createJaspContainer(dependencies = c("model", "noSamples", "noBurnin", "noThinning", "noChains",
                                                          "parametersMonitored", "parametersShown", "initialValues", "userData",
                                                          "setSeed", "seed", "showDeviance"))
    jaspResults[["mainContainer"]] <- mainContainer
  }

  # checks and sets errors
  .JAGSCheckJAGSInstallation(jaspResults[["mainContainer"]])
  if (jaspResults[["mainContainer"]]$getError()) {
    .JAGSsetGoodModel(jaspResults, FALSE)
    return()
  }

  # user specified monitoring?
  manualMonitor   <- options[["showResultsFor"]] == "monitorSelectedParameters"
  nParamAvailable <- length(options[["model"]][["parameters"]])

  # sum because only parameters can be assigned only once
  if (manualMonitor) {
    nParamMonitored <- length(options[["monitoredParametersList"]])
  } else {
    nParamMonitored <- length(options[["parametersShown"]])
  }
  nParamShown <- length(options[["parametersShown"]])

  monitorWarning <- NULL
  goodModel <- TRUE
  if (.JAGSisEmptyModel(trimws(options[["model"]][["model"]]))) {
    goodModel <- FALSE
  } else if (nParamAvailable > 0L) {
    if (manualMonitor && nParamMonitored == 0L) {
      goodModel <- FALSE
      monitorWarning <- gettext("Please specify which parameters to monitor!")
    } else if (( manualMonitor && nParamMonitored > 0L && nParamShown == 0L) ||
               (!manualMonitor && nParamShown == 0L)) {
      goodModel <- FALSE
      monitorWarning <- gettext("Please specify which parameters to show output for!")
    }
  }
  .JAGSsetGoodModel     (jaspResults, goodModel)
  .JAGSsetMonitorWarning(jaspResults, monitorWarning)

  return()
}

.JAGSReadData <- function(jaspResults, options) {

  if (jaspResults[["mainContainer"]]$getError() || !.JAGSgetGoodModel(jaspResults) || !.JAGShasData(options))
    return(NULL)

  varsToRead <- options[["model"]][["columns"]]
  dataset <- .readDataSetToEnd(columns.as.numeric = varsToRead)
  return(dataset)
}

# Tables ----
.JAGSoutputTable <- function(jaspResults, options, mcmcResult) {

  tb <- createJaspTable("MCMC Summary")
  tb$position <- 1L
  ovt0 <- gettext("Posterior")
  ovt1 <- gettextf("95%% Credible Interval")
  ovt2 <- gettext("Rhat")
  tb$addColumnInfo(name = "parameter", title = gettext("Parameter"),            type = "string")
  tb$addColumnInfo(name = "Mean",      title = gettext("Mean"),                 type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "50%",       title = gettext("Median"),               type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "SD",        title = gettext("SD"),                   type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "2.5%",      title = gettext("Lower"),                type = "number", overtitle = ovt1)
  tb$addColumnInfo(name = "97.5%",     title = gettext("Upper"),                type = "number", overtitle = ovt1)
  tb$addColumnInfo(name = "rhatPoint", title = gettext("Point est."),           type = "number", overtitle = ovt2)
  tb$addColumnInfo(name = "rhatCI",    title = gettext("Upper CI"),             type = "number", overtitle = ovt2)
  tb$addColumnInfo(name = "neff",      title = gettext("Effecive Sample Size"), type = "number")

  if (!is.null(mcmcResult) && !jaspResults[["mainContainer"]]$getError()) {

    if (!.JAGShasData(options) && !mcmcResult[["hasUserData"]]) {
      tb$addFootnote(message = gettext("No data was supplied, everything was sampled from the priors!"), symbol = .JAGSWarningSymbol)
      if (options[["showDeviance"]])
        tb$addFootnote(message = gettext("Deviance cannot be computed without data."), symbol = .JAGSWarningSymbol)
    }

    parametersToShow <- options[["parametersShown"]]
    if (mcmcResult[["DIC"]] && options[["showDeviance"]])
      parametersToShow <- c("deviance", parametersToShow)
    sum <- mcmcResult[["BUGSoutput"]][["summary"]]
    nms <- rownames(sum)
    nms2 <- sapply(stringr::str_extract_all(nms, "\\w+"), `[[`, 1L)
    idx <- nms2 %in% parametersToShow

    tbR <- as.data.frame(sum[idx, c("Mean", "SD", "50%", "2.5%", "97.5%", "neff"), drop = FALSE])
    tbR$parameter <- nms[idx]

    if (options[["noChains"]] > 1L) {

      rhat <- try(coda::gelman.diag(mcmcResult[["samples"]]))
      if (isTryError(rhat)) {
        tb$addFootnote(message = gettext("Failed to compute the Rhat statistic. This is expected if the model contains discrete parameters."), 
                       colNames = c("rhatPoint", "rhatCI"))
      } else {

        tbR[["rhatPoint"]] <- rhat[["psrf"]][idx, 1L]
        tbR[["rhatCI"]]    <- rhat[["psrf"]][idx, 2L]
        if (!is.null(rhat[["mpsrf"]])) {
          tb$addFootnote(message = gettextf(
            "The multivariate potential scale reduction factor is estimated at %.3f.",
            rhat[["mpsrf"]]
          ))
        }
      }
    } else {
      tb$addFootnote(message = gettext("Rhat statistic cannot be computed for only one chain. It is strongly recommoned to run more than one chain to assess MCMC convergence!"))
    }
    tb$setData(tbR)
  }

  if (!is.null(message <- .JAGSgetMonitorWarning(jaspResults)))
    tb$addFootnote(message = message, symbol = .JAGSWarningSymbol)

  jaspResults[["mainContainer"]][["mainTable"]] <- tb

  return()
}

# Plots ----
.JAGSmcmcPlots <- function(jaspResults, options, mcmcResult) {

  if (is.null(jaspResults[["mainContainer"]][["plotContainer"]])) {
    plotContainer <- createJaspContainer(dependencies = c("parametersShown", "colorScheme"))
  } else {
    plotContainer <- jaspResults[["mainContainer"]][["plotContainer"]]
  }

  params <- .JAGSGetParams(options, mcmcResult)
  containerObj <- .JAGSInitPlotsContainers(plotContainer, options, params)
  if (is.null(containerObj) && is.null(plotContainer[["plotBivarHex"]]))
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

  .JAGSPlotBivariateScatter(plotContainer, options, mcmcResult, params)

}

.JAGSInitPlotsContainers <- function(plotContainer, options, params) {

  # for each plot function, create an empty plot container (or the empty plot object)
  output <- NULL
  if (options[["plotDensity"]]) {

    add <- list("function" = ".JAGSPlotDensity")
    if (is.null(plotContainer[["plotDensity"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Marginal Density"),  position = 1,
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
      add[["container"]] <- createJaspContainer(title = gettext("Marginal Histogram"),  position = 2,
                                                dependencies = c("plotHistogram", "aggregateChains", "showLegend"))
      plotContainer[["plotHistogram"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotHistogram"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotTrace"]]) {

    add <- list("function" = ".JAGSPlotTrace")
    if (is.null(plotContainer[["plotTrace"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Trace Plots"),  position = 3,
                                                dependencies = c("plotTrace", "showLegend"))
      plotContainer[["plotTrace"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotTrace"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotAutoCor"]]) {

    add <- list("function" = ".JAGSPlotAutoCor")
    if (is.null(plotContainer[["plotAutoCor"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Autocorrelation Plots"),  position = 4,
                                                dependencies = c("plotAutoCor", "noLags", "acfType", "showLegend"))
      plotContainer[["plotAutoCor"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["plotAutoCor"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["plotBivarHex"]] && is.null(plotContainer[["plotBivarHex"]])) {

    jaspPlot <- createJaspPlot(title  = gettext("Bivariate Scatter Plot"),  position = 5,
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
    labs <- ggplot2::labs(x = param, y = gettext("Density"))

  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(df, mapping) +
      ggplot2::geom_line(show.legend = options[["aggregateChains"]]) +
      labs +
      colorScale, legend.position = if (options[["showLegend"]]) "right" else "none"
  )
  return(g)
}

.JAGSIsParameterDiscrete <- function(samples, param) {
  
  u <- c()
  for (i in seq_along(samples))
    u <- c(u, unique(samples[[i]][, param]))
  
  # if a parameter has 25 unique values or less, we assume the parameter is discrete
  return(length(u) > 25L)
}

.JAGSGetHistogramBreaks <- function(samples, param) {

  n <- nrow(samples[[1L]])
  u <- sort(unique(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE)))

  # if a parameter has 25 unique values or less, we assume the parameter is discrete
  isDiscrete <- length(u) <= 25L

  if (isDiscrete) {
    # this works because by default, hist makes the first category using e.g., [0, 1], but the next using (1, 2]. 
    # the resulting histogram may be heavily misleading. Hist only does this for particular frequencies, but unfortunately,
    # the default number of samples can lead to this when sampling from the prior predictive of binomial(theta) with theta ~ dbeta(1, 1).
    return(list(breaks = c(u[1L], 0.999 + u), unique = u))
  } else {
    return(list(breaks = "Sturges")) # the default of hist
  }
}

.JAGSPlotHistogram <- function(samples, param, options, removeAxisLabels = FALSE) {

  # TODO: get parameter bounds and respect these, e.g., truncate [0, 1] (probably pretty hard though)
  npoints <- 2^10 # precision for density estimation

  tmpBreaks  <- .JAGSGetHistogramBreaks(samples, param)
  breaksType <- tmpBreaks$breaks
  isDiscrete <- !is.character(breaksType)

  if (options[["aggregateChains"]]) {
    df <- do.call(rbind.data.frame, lapply(seq_along(samples), function(i) {
      d <- graphics::hist(samples[[i]][, param], breaks = breaksType, plot = FALSE)
      return(data.frame(x = if (isDiscrete) tmpBreaks$unique else d[["mids"]], y = d[["counts"]], g = factor(i)))
    }))
    mapping <- ggplot2::aes(x = x, y = y, color = g, fill = g)
    colorScale <- JASPgraphs::scale_JASPcolor_discrete(name = gettext("Chain"))
    fillScale  <- JASPgraphs::scale_JASPfill_discrete(name = gettext("Chain"))
  } else {
    n <- nrow(samples[[1L]])
    d <- graphics::hist(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE), breaks = breaksType, plot = FALSE)
    df <- data.frame(x = if (isDiscrete) tmpBreaks$unique else d[["mids"]], y = d[["counts"]])
    mapping <- ggplot2::aes(x = x, y = y)
    fillScale <- colorScale <- NULL
  }

  xAxis <- NULL
  # prevent non-discrete axis labels
  if (isDiscrete)
    xAxis <- ggplot2::scale_x_continuous(breaks = round(JASPgraphs::getPrettyAxisBreaks(tmpBreaks$unique)))

  if (removeAxisLabels)
    labs <- ggplot2::labs(x = NULL, y = NULL)
  else
    labs <- ggplot2::labs(x = param, y = gettext("Counts"))

  # TODO: (vandenman - 29/03) from ggplot2 version 3.3.0 onwards we need to uncomment the 'orientation = "x"'
  g <- JASPgraphs::themeJasp(
    ggplot2::ggplot(df, mapping) +
      ggplot2::geom_bar(show.legend = options[["aggregateChains"]], position = ggplot2::position_dodge(),
                        stat = "identity") + #, orientation = "x") +
      labs + colorScale + fillScale + xAxis,
    legend.position = if (options[["showLegend"]]) "right" else "none"
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

  g <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = g)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = gettext("Iteration"), y = param) +
    JASPgraphs::scale_JASPcolor_discrete(name = gettext("Chain")) +
    JASPgraphs::geom_rangeframe() +
    JASPgraphs::themeJaspRaw(legend.position = if (options[["showLegend"]]) "right" else "none") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 0, 0))
  return(g)
}

.JAGSPlotAutoCor <- function(samples, param, options) {

  # TODO: x coordinates should be based on acf()#n.used!
  nchains  <- length(samples)
  nlags    <- options[["noLags"]]
  nvals    <- nlags + 1L # we're getting one more value than nlags since the 0th lag counts.
  acfs <- numeric(nchains * nvals)
  for (i in seq_len(nchains))
    acfs[(1 + (i-1) * nvals):(i * nvals)] <- c(stats::acf(x = samples[[i]][, param], type = "correlation",
                                                          lag.max = nlags, plot = FALSE)$acf)

  df <- data.frame(
    x = 0:nlags,
    y = acfs,
    g = factor(rep(seq_len(nchains), each = nvals))
  )

  if (options[["acfType"]] == "acfLines") {
    geom <- ggplot2::geom_line()
  } else {
    geom <- ggplot2::geom_col(position = ggplot2::position_dodge())
  }
  colorScale <- JASPgraphs::scale_JASPcolor_discrete(name = gettext("Chain"))
  fillScale  <- JASPgraphs::scale_JASPfill_discrete(name = gettext("Chain"))

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, nlags))
  xBreaks <- xBreaks[xBreaks <= nlags]
  xLimits <- c(0L, nlags)

  g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, color = g, group = g, fill = g)) +
    geom + colorScale + fillScale +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1.05) +
    ggplot2::ylab(gettext("Autocorrelation")) +
    ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = xBreaks, limits = xLimits) +
    JASPgraphs::geom_rangeframe() +
    JASPgraphs::themeJaspRaw(legend.position = if (options[["showLegend"]]) "right" else "none")

  return(g)
}

.JAGSPlotBivariateScatter <- function(plotContainer, options, mcmcResult, params) {

  if (is.null(plotContainer[["plotBivarHex"]]) || !is.null(plotContainer[["plotBivarHex"]]$plotObject))
    return()

  jaspPlot <- plotContainer[["plotBivarHex"]]
  if (length(options[["parametersShown"]]) >= 2L) {
    jaspPlot$width  <- sum(lengths(mcmcResult[["params"]])) * 320L
    jaspPlot$height <- sum(lengths(mcmcResult[["params"]])) * 320L
    jaspPlot$plotObject <- .JAGSPlotBivariateMatrix(options, mcmcResult, unlist(params))
  } else if (length(options[["parametersShown"]]) == 1L) {
    # only show an error when some variables are selected to avoid error messages when users set the options
    jaspPlot$setError(gettext("At least two parameters need to be monitored and shown to make a bivariate scatter plot!"))
  }
}

.JAGSPlotBivariateMatrix <- function(options, mcmcResult, allParams) {

  samples <- mcmcResult[["samples"]]
  nParams <- length(allParams)
  plotMatrix <- matrix(list(), nParams, nParams, dimnames = list(allParams, allParams))

  # these should always be false for the matrix plot
  options[["aggregateChains"]] <- FALSE
  options[["showLegend"]]      <- FALSE

  for (j in seq_len(nParams)) {
    for (i in seq_len(nParams)) {

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

.JAGSmodelError <- function(error, model, options) {

  errorMessage <- .extractJAGSErrorMessage(error)

  # perhaps some helpfull checks...
  chars <- stringr::fixed(c("[", "]", "{", "}", "(", ")"))
  counts <- stringr::str_count(model, chars)
  toAdd <- paste(
    .JAGSmodelErrorString(counts[1:2], chars[1:2]),
    .JAGSmodelErrorString(counts[3:4], chars[3:4]),
    .JAGSmodelErrorString(counts[5:6], chars[5:6])
  )

  if (length(toAdd) > 0L)
    errorMessage <- gettextf("%1$s%2$sIn addition:%3$s%4$s", errorMessage, "\n\n", "\n", toAdd)

  # return error message
  return(errorMessage)
}

.JAGSmodelErrorString <- function(counts, chars) {
  if (counts[1L] == counts[2L]) return(NULL)

  if (counts[1L] < counts[2L]) {
    counts <- counts[2:1]
    chars <- chars[2:1]
  }
  return(gettextf(
    "The model contains more '%1$s' than '%2$s' (%3$d vs %4$d)",
    chars[1L], chars[2L], counts[1L], counts[2L]
  ))
}

.JAGSCheckJAGSInstallation <- function(jaspContainer) {
  # NOTE: this function does setError if loadNamespace("rjags") fails.
  # Thus, all other calls to rjags:: should be preceded by $getError().
  e <- try(loadNamespace("rjags"), silent = TRUE)
  if (isTryError(e)) {
    # Sys.getenv() returns "" if nothing was found
    jaspContainer$setError(gettextf("There was a problem loading JAGS, JAGS_HOME is: '%1$s'.\nPlease contact the JASP team for support.\nError was: %2$s.",
                                    Sys.getenv("JAGS_HOME"), e))
  } else if (isTRUE(rjags::jags.version() < "4.3.0")) {
    jaspContainer$setError(gettextf("Expected JAGS version 4.3.0 but found %s", as.character(rjags::jags.version())))
  }
  return(NULL)
}


# Helper functions ----
.JAGSGetParams <- function(options, mcmcResult) {

  if (!is.null(mcmcResult)) {
    params <- mcmcResult[["params"]]
    if (!options[["showDeviance"]]) {
      params <- params[names(params) != "deviance"]
    }
    return(params)
  }

  params <- unlist(options[["parametersShown"]])
  if (is.null(params))
    return(NULL)
  obj <- as.list(params)
  names(obj) <- params
  return(obj)

}

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
      }
      obj <- try(eval(parse(text = string)))
      if (isTryError(obj)) {
        jaspResults[["mainContainer"]]$setError(gettextf("The R code for %1$s crashed with error:\n%2$s",
                                                        type, .extractErrorMessage(obj)))
        return()
      } else if (!is.numeric(obj)) {
        jaspResults[["mainContainer"]]$setError(gettextf("The result of %1$s R code should be numeric but it was of mode %2$s and class %3$s",
                                                type, mode(obj), paste(class(obj), collapse = ",")))
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

.JAGSloadModules <- function(jaspResults) {
  # just like R2jags, we just always load the modules
  e <- try(
    for (m in c("glm", "dic")) # allow users to add custom modules?
      rjags::load.module(m)
  )
  if (isTryError(e))
    .JAGSsetModuleLoadingError(e)
}

# one line helpers ----
.JAGSWarningSymbol <- "&#9888;"

.JAGShasData <- function(options) length(options[["model"]][["columns"]]) > 0L

.JAGSsetState <- function(jaspResults, key, value, dependencies = NULL) jaspResults[[key]] <- createJaspState(value, dependencies = dependencies)
.JAGSgetState <- function(jaspResults, key) jaspResults[[key]]$object

.JAGSsetGoodModel <- function(jaspResults, value) .JAGSsetState(jaspResults, "__goodModel", value)
.JAGSgetGoodModel <- function(jaspResults) isTRUE(.JAGSgetState(jaspResults, "__goodModel"))

.JAGSsetMonitorWarning <- function(jaspResults, value) .JAGSsetState(jaspResults, "__monitorWarning", value)
.JAGSgetMonitorWarning <- function(jaspResults) .JAGSgetState(jaspResults, "__monitorWarning")

.JAGSsetModuleLoadingError <- function(jaspResults, value) .JAGSsetState(jaspResults, "__moduleLoading", value)
.JAGSgetModuleLoadingError <- function(jaspResults) .JAGSgetState(jaspResults, "__moduleLoading")

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


# TODO long term: ----
# - get parameter bounds and respect these in the plots and density estimation, e.g., truncate [0, 1] (probably pretty hard though).
# - allow custom jags modules?
