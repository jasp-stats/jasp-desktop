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

ClassicalMetaAnalysis <- function(dataset=NULL, options, perform="run", callback=function(...) 0, state=NULL, ..., DEBUG=0) {

  # Restore previous computation state and detect option changes
  run <- perform == "run"

  fitOpts <- c("dependent", "wlsWeights", "method", "studyLabels", "covariates", "test",
               "factors", "modelTerms", "includeConstant", "regressionCoefficientsConfidenceIntervalsInterval")
  stateKey <- list(
    rma.fit = fitOpts,
    dataset= fitOpts,
    coefficients = c(fitOpts, "regressionCoefficientsEstimates"),
    fitStats = c(fitOpts, "modelFit"),
    residPars = c(fitOpts, "residualsParameters"),
    coeffVcov = c(fitOpts, "regressionCoefficientsCovarianceMatrix"),
    ranktst = c(fitOpts, "rSquaredChange"),
    egger = c(fitOpts, "funnelPlotAsymmetryTest"),
    influ = c(fitOpts, "residualsCasewiseDiagnostics"),
    fsn.fit = c(fitOpts, "plotResidualsCovariates"),
    forestPlot = c(fitOpts, "forestPlot"),
    funnelPlot = c(fitOpts, "funnelPlot"),
    diagnosticsPlot = c(fitOpts, "plotResidualsDependent", "plotResidualsQQ"),
    profilePlot = c(fitOpts, "plotResidualsPredicted"),
    trimfillPlot = c(fitOpts, "trimfillPlot")
  )

  rma.fit <- state$rma.fit
  dataset <- state$dataset
  coefficients <- state$coefficients
  fitStats <- state$fitStats
  residPars <- state$residPars
  coeffVcov <- state$coeffVcov
  ranktst <- state$ranktst
  egger <- state$egger
  influ <- state$influ
  fsn.fit <- state$fsn.fit
  forestPlot <- state$forestPlot
  funnelPlot <- state$funnelPlot
  diagnosticsPlot <- state$diagnosticsPlot
  profilePlot <- state$profilePlot
  trimfillPlot <- state$trimfillPlot

  ### This file interfaces the metafor::rma function and associated diagnostic functions
  ### (but it restricts its multipurposeness to one central purpose: fitting a
  ### meta-regression to effect sizes and their standard errors and providing diagnostics.)

  ### options contains:
  ## essentials:
  #   effectsize:  string (maps to 'yi'), name of the variable that contains the effect sizes (ES)
  #   stderr:      string (maps to 'sei'), name of the variable containing the ES standard errors
  #   intercept:   logical (maps to 'intercept'), intercept in the model?
  #
  ## optional:
  #   covariates:  string array (maps to 'mods'), names of continuous predictor variables
  #   factors:     string array (maps to 'mods'), names of nominal/ordinal predictor variables
  ## plotting:
  #   studylabels:    string (maps to 'slab'), name of variable that contains label for a forrest plot
  #   forrestPlot:    logical, make this plot?
  #   funnelPlot:     logical,
  ## advanced analysis:
  #   method:         string, one of [`Fixed Effects`, `Maximum Likelihood`, `Restricted ML`, `DerSimonian-Laird`, `Hedges`, `Hunter-Schmidt`, `Sidik-Jonkman`, `Empirical Bayes`, `Paule-Mandel`] (see ?rma)
  #  -
  #   test:           string, one of ["z", "knha"]
  #   btt:            numeric, vector of indices specifying which coefficients to include in the omnibus test of moderators
  #  -
  #   ...:            numeric, -3 ≤ value ≤ 3, distribution used is dt((tStatistic - Tlocation) / Tscale, TDf)

  #********
  # debuging:
  #  options = list(effectsize = "yi", stderr = "sei", intercept=TRUE,
  #                 covariates = NULL, factors = NULL, studylabels = NULL,
  #                 forrestPlot = TRUE, funnelPlot = FALSE, method="FE")
  #  readJaspMsg <- function(connection=pipe('pbpaste')) jsonlite::fromJSON(paste(readLines(connection),collapse = "\n"))
  #  readOptions <- function(connection=pipe('pbpaste')) if (is.list) connection$options else readJaspMsg(connection)$options
  #  dd = function(options, ...){x=list(...); options[names(x)] = x; options}

  # eval(parse(text = gsub("\\s+\\#","",readLines(pipe('pbpaste')))))
  # dataset = metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=metafor::dat.bcg)
  # dataset = transform(dataset, sei = vi^0.5)
  #********

  #### Compute results of the analysis

  ## Get data. ####

  studyLabelName  <- options$studyLabels

  effsizeName <- unlist(options$dependent)
  stderrName  <- unlist(options$wlsWeights)

  covarNames <- if (length(options$covariates) > 0) unlist(options$covariates)
  factNames  <- if (length(options$factors) > 0) unlist(options$factors)

  list.variables    <- Filter(function(s) s != "", c(effsizeName, covarNames))
  numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
  factor.variables  <- Filter(function(s) s != "", c(factNames, studyLabelName))


  if (is.null(dataset)) {
    .readDS <- switch(perform, run = .readDataSetToEnd, .readDataSetHeader)
    dataset <- .readDS(
      columns.as.factor  = factor.variables,
      columns.as.numeric = numeric.variables,
      exclude.na.listwise = numeric.variables
    )

    .hasErrors(dataset, perform, type=c("infinity", "observations"),
               custom = error_if(any(dataset[[b64(stderrName)]] <= 0), "Non-positive standard errors encountered in variable '", stderrName,
                                 "'. Standard errors should be positive. Please check your input."),
               all.target=numeric.variables, observations.amount="< 2",
               exitAnalysisIfErrors=TRUE)
  }

  method <- switch(options$method, `Fixed Effects` = "FE", `Maximum Likelihood` = "ML",
                  `Restricted ML` = "REML", `DerSimonian-Laird` = "DL", `Hedges` = "HE",
                  `Hunter-Schmidt` = "HS", `Sidik-Jonkman` = "SJ", `Empirical Bayes` = "EB",
                  `Paule-Mandel` = "PM", "REML")

  can.run <- all(c(effsizeName, stderrName) != "")

  ## Run the analysis. ####
  if (is.null(rma.fit)) {
    rma.fit <- structure(list('b' = numeric(),'se' = numeric(),'ci.lb' = numeric(), 'ci.ub' = numeric(),
                    'zval' = numeric(),'pval' = numeric()), class = c("dummy", "rma"))

    if (run && can.run) {

      # infer model formula
      .vmodelTerms <- b64(options$modelTerms)

      formula.rhs <- as.formula(as.modelTerms(.vmodelTerms))
      if (is.null(formula.rhs))
        formula.rhs <- ~1

      if (!options$includeConstant)
        formula.rhs <- update(formula.rhs, ~ . + 0)

      if (identical(formula.rhs, ~ 1 - 1))
        .quitAnalysis("The model should contain at least one predictor or an intercept.")

      # analysis
      rma.fit <- tryCatch( # rma generates informative error messages; use them!
        metafor::rma(
          yi = get(b64(effsizeName)), sei = get(b64(stderrName)), data = dataset,
          method=method, mods = formula.rhs, test = options$test,
          slab = if(options$studyLabel != "") paste0(get(b64(options$studyLabels))),
          level = options$regressionCoefficientsConfidenceIntervalsInterval + 1e-9, # add tiny amount because 1 is treated by rma() as 100% whereas values > 1 as percentages
          control = list(maxiter = 500)
        ), error = function(e) .quitAnalysis(e$message))

      rma.fit <- d64(rma.fit, values = all.vars(formula.rhs))

    }
  }

  if (DEBUG == 1) return(rma.fit)

  #### Output: Initialize meta and results objects ####

  meta <- list()
  results <- list()
  results[["title"]] <- analysisTitle(rma.fit)

  ### Prepare Q-tests table output ####
  meta[[length(meta)+1]] <- list(name = "qtests", type = "table")
  qtesttable <- qTestsTable(rma.fit)

  # footnotes
  qfootnotes <- .newFootnotes()
  .addFootnote(qfootnotes, symbol = "<em>Note.</em>", text = "<em>p</em>-values are approximate.")
  qtesttable[["footnotes"]] <- as.list(qfootnotes)

  # citation
  qtesttable[["citation"]] <- list(
    "Hedges, L. V., & Olkin, I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press."
  )
  results[["qtests"]] <- qtesttable

  ### Prepare Coefficients Table Output ####
  if (options$regressionCoefficientsEstimates) {

    meta[[length(meta) + 1]] <- list(name = "table", type = "table")
    cols <- c("Estimate", "Standard Error", "z", "p", "Lower Bound", "Upper Bound")

    if (is.null(coefficients) && run && can.run)
      coefficients <- .clean(coef(summary(rma.fit)))

    table <- list(title = "Coefficients")
    if (!is.null(coefficients)) {
      table$x <- coefficients
      colnames(table$x) <- cols
      if (! options$regressionCoefficientsConfidenceIntervals) {
        table$x <- table$x[1:4] # remove confidence interval bounds
        colnames(table$x) <- cols[1:4]
      }
    } else {
      table$x <- cols
      if (! options$regressionCoefficientsConfidenceIntervals)
        table$x <- cols[1:4]
    }

    coeftable <- do.call(as.jaspTable, table) # FIXME: we want an overtitle above upper/lower

    if (DEBUG == 2) return(table)

    # Add footnotes to the analysis result
    footnotes <- .newFootnotes()
    footnote.text <- switch(options$test, z = "Wald test.", "Wald tests.")
    .addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote.text)
    coeftable[["footnotes"]] <- as.list(footnotes)

    # Add citation reference list
    coeftable[["citation"]] <- list(
      "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of
      Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/"
    )

    results[["table"]] <- coeftable
  }

  ### Prepare Model fit table output ####

  if (options$modelFit) {

    meta[[length(meta)+1]] <- list(name = "modelFit", type = "table")
    title <- "Fit measures"
    cols <- options$method
    rows <- c("Log-likelihood", "Deviance", "AIC", "BIC", "AICc")

    if (is.null(fitStats) && run && can.run)
      fitStats <- try(metafor:::fitstats(rma.fit))

    table <- list(title = title)
    if (! is.null(fitStats)) {
      table$x <- fitStats
      rownames(table$x) <- rows
    } else {
      table$x <- cols
      table$y <- rows
    }

    #if (inherits(df, "try-error"))  df = structure(list(rep(".",3)), .Names = options$method, row.names = c("logLik","AIC","BIC"), class="data.frame")
    fittable <- do.call(as.jaspTable, table)
    results[["modelFit"]] <- fittable

  }


  ### Prepare Residuals Parameters ####

  if (options$residualsParameters && method != "FE") {

    meta[[length(meta) + 1]] <- list(name = "residPars", type = "table")
    options(jasp_number_format = "sf:5;dp:4")
    title <- "Residual Heterogeneity Estimates"
    cols <- c("Estimate", "Lower Bound", "Upper Bound")
    rows <- c("<em>&tau;&sup2;</em>", "<em>&tau;</em>", "<em>I&sup2;</em> (%)", "<em>H&sup2;</em>")

    if (is.null(residPars) && run && can.run)
      residPars <- confint(rma.fit, digits = 12, level = options$regressionCoefficientsConfidenceIntervalsInterval)$random

    table <- list(title = title)
    if (! is.null(residPars)) {
      table$x <- residPars
      colnames(table$x) <- cols
      rownames(table$x) <- rows
      if (! options$regressionCoefficientsConfidenceIntervals) {
        table$x <- table$x[, 1, drop=FALSE] # remove confidence interval bounds
        colnames(table$x) <- cols[1]
      }
    } else {
      table$x <- cols
      table$y <- rows
      if (! options$regressionCoefficientsConfidenceIntervals) {
        table$x <- cols[1]
      }
    }

    residParsTable <- do.call(as.jaspTable, table) # FIXME: we want an overtitle above upper/lower bounds
    results[["residPars"]] <- residParsTable

  }


  ### Prepare Coefficient Covariance Matrix ####

  if (options$regressionCoefficientsCovarianceMatrix) {

    meta[[length(meta) + 1]] <- list(name = "vcov", type = "table")

    if (is.null(coeffVcov) && run && can.run)
      coeffVcov <- vcov(rma.fit)

    options(jasp_number_format = "sf:5;dp:4")
    table <- list(title = "Parameter Covariances", x = "...", y = "...")
    if (!is.null(coeffVcov)) table$x = coeffVcov

    vcovTable <- do.call(as.jaspTable, table)
    results[["vcov"]] <- vcovTable

  }


  ### Prepare Rank test for Funnel plot asymmetry ####

  if (options$rSquaredChange) {

    meta[[length(meta) + 1]] <- list(name = "ranktest", type = "table")
    options(jasp_number_format = "sf:5;dp:4")
    title <- "Rank correlation test for Funnel plot asymmetry"
    cols <- c("Kendall's &tau;", "p")
    rows <- "Rank test"

    if (is.null(ranktst) && run && can.run)
      ranktst <- unlist(metafor::ranktest(rma.fit))

    table <- list(title = title)
    if (! is.null(ranktst)) {
      table$x <- as.data.frame(t(ranktst[1:2]))
      rownames(table$x) <- rows
      colnames(table$x) <- cols
    } else {
      table$x <- cols
      table$y <- rows
    }

    rankTestTable <- do.call(as.jaspTable, table)
    results[["ranktest"]] <- rankTestTable

  }


  ### Prepare Egger's test for Funnel plot asymmetry ('test for publication bias') ####

  if (options$funnelPlotAsymmetryTest) {

    meta[[length(meta) + 1]] <- list(name = "egger", type = "table")
    options(jasp_number_format = "sf:5;dp:4")
    title <- "Regression test for Funnel plot asymmetry (\"Egger's test\")"
    cols <- if (options$test == "knha") c("t", "p") else c("z", "p")

    if (is.null(egger) && run && can.run)
      egger <- metafor::regtest(rma.fit)

    table <- list(title = title)
    if (! is.null(egger)) {
      table$x <- metafor::coef.summary.rma(summary(egger$fit))[egger$predictor, c(paste0(cols[1], "val"), "pval")]
      colnames(table$x) <- cols
    } else {
      table$x <- cols
    }

    eggerTable <- do.call(as.jaspTable, table)
    eggerTable[["citation"]] <- list("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. <em>Journal of Statistical Software</em>, <b>36</b>(3), 1–48.")
    results[["egger"]] <- eggerTable

  }


  ### Prepare Casewise diagnostics table ####

  if (options$residualsCasewiseDiagnostics) {

    meta[[length(meta) + 1]] <- list(name = "influence", type = "table")
    options(jasp_number_format = "sf:5;dp:4")

    if (is.null(influ) && run && can.run)
      influ <- influence(rma.fit)

    table <- list(title = "Influence Measures")
    cols <- c("Std. Residual", "DFFITS", "Cook's Distance", "Cov. Ratio",
              "&tau;&sup2;<sub>(-i)</sub>", "Q<sub>E(-i)</sub>", "Hat", "Weight")
    if (!is.null(influ)) {
      table$x <- influ$inf
      colnames(table$x) <- cols
      influential <- rownames(influ$inf)[which(influ$is.infl)]
      postfix <- ifelse(!is.na(influ$is.infl) & influ$is.infl, "*","")
      rownames(table$x) <- paste0(rownames(table$x), postfix)
    } else {
      table$x <- cols
      influential <- c()
    }

    influTable <- do.call(as.jaspTable, table)

    # Markup influential cases in a footnote
    ninfl <- length(influential)

    if (ninfl > 0) {
      infl.footn <- .newFootnotes()
      .addFootnote(infl.footn, symbol = "<em>Note.</em>",
                   text = "Cases marked with * are influential.")
      influTable[["footnotes"]] <- as.list(infl.footn)
    }

    results[["influence"]] <- influTable
  }


  ### Prepare Fail-safe N diagnostics table ####

  if (options$plotResidualsCovariates & run & can.run) {

    meta[[length(meta) + 1]] <- list(name = "failsafen", type = "table")

    fsn.fit <-
      metafor::fsn(yi = get(b64(effsizeName)), sei = get(b64(stderrName)), data = dataset)
    fsn.fit <- d64(fsn.fit)

    failsnTable <- do.call(data.frame, fsn.fit[c('fsnum','alpha','pval')])
    colnames(failsnTable) = c('Fail-safe N', 'Target Significance', 'Observed Significance')
    rownames(failsnTable) = fsn.fit[['type']]

    options(jasp_number_format = "sf:5;dp:4")
    failsnTable <- as.jaspTable(.clean(failsnTable), title = "File Drawer Analysis")
    results[["failsafen"]] <- failsnTable

  }

  ### Prepare Plot Output ####

  meta[[length(meta) + 1]] <-
    list(name = "plots", type = "object",
         meta = list(
            list(name = "forrestPlot", type = "image"),
            list(name = "funnelPlot", type = "image"),
            list(name = "diagnosticPlot", type = "image"),
            list(name = "profilePlot", type = "image"),
            list(name = "trimfillPlot", type = "image")
        )
    )
  plots <- list(title = "Plot")

  # plot forest plot
  if (options$forestPlot && (! is.null(forestPlot) || run && can.run)) {

    if (is.null(forestPlot)) {
      img.height <- max(520, nobs(rma.fit) * 20) # very heuristic! needs improvement...
      forest.img <- .writeImage(width = 520, height = img.height, function() cmaForest(rma.fit, cex.lab=1.2, las=1, efac=15 / nobs(rma.fit)))
      forestPlot <- list(title="Forest plot", width = 500, height = img.height, convertible = TRUE,
                         obj = forest.img[["obj"]], data = forest.img[["png"]],
                         status = "complete")
    }
    plots[["forrestPlot"]]  <- forestPlot

  }

  # plot funnel plot
  if (options$funnelPlot && (! is.null(funnelPlot) || run && can.run)) {

    if (is.null(funnelPlot)) {
      funnel.img <- .writeImage(width = 520, height = 520, function() metafor::funnel(rma.fit, las=1))
      funnelPlot <- list(title="Funnel plot", width = 500, height = 500, convertible = TRUE,
                         obj = funnel.img[["obj"]], data = funnel.img[["png"]],
                         status = "complete")
    }
    plots[["funnelPlot"]]  <- funnelPlot

  }

  # plot 1: residuals and dependent diagnostic plot
  if (options$plotResidualsDependent && (! is.null(diagnosticsPlot) || run && can.run)) {

    if (is.null(diagnosticsPlot)) {
      diagnostics.img <- .writeImage(width = 820, height = 820, function() plot(rma.fit, qqplot = options$plotResidualsQQ, las=1))
      diagnosticsPlot <- list(title = "Diagnostic plots", width = 820, height = 820, convertible = TRUE,
                              obj = diagnostics.img[["obj"]], data = diagnostics.img[["png"]],
                              status = "complete")
    }
    plots[["diagnosticPlot"]]  <- diagnosticsPlot

  }

  # profile plot: diagnostic plot for tau parameter
  if (options$plotResidualsPredicted && (! is.null(profilePlot) || run && can.run)) {

    if (is.null(profilePlot)) {
      profile.img <- .writeImage(width = 520, height = 520, function() profile(rma.fit, cex.lab=1.2, las=1))
      profilePlot <- list(title = "Log-likelihood profile for &tau;&sup2;", width = 520, height = 520, convertible = TRUE,
                          obj = profile.img[["obj"]], data = profile.img[["png"]],
                          status = "complete")
    }
    plots[["profilePlot"]]  <- profilePlot

  }


  # profile plot: diagnostic plot for tau parameter
  if (options$trimfillPlot && (! is.null(trimfillPlot) || run && can.run)) {

    if (is.null(trimfillPlot)) {
      trimfill.fit <- metafor::trimfill(update(rma.fit, mods = ~1))
      trimfill.img <- .writeImage(width = 820, height = 820, function() plot(trimfill.fit, qqplot=T, cex.lab=1.2, las=1))
      trimfillPlot <- list(title = "Trim-fill Analysis", width = 820, height = 820, convertible = TRUE,
                           obj = trimfill.img[["obj"]], data = trimfill.img[["png"]],
                           status = "complete")
    }
    plots[["trimfillPlot"]]  <- trimfillPlot

  }

  ### Create results object to be returned ####
  results[[".meta"]] <- meta
  results[["plots"]] <- plots

  ### Finish off: collect objects to keep; update status and state ####

  keep <- NULL
  for (plot in plots) {
    if (! is.null(names(plot)) && "data" %in% names(plot)) {
      keep <- c(keep, plot$data)
    }
  }

  if (run) {
    status <- "complete"
    state <- list(
      options = options,
      rma.fit = rma.fit,
      dataset = dataset,
      coefficients = coefficients,
      fitStats = fitStats,
      residPars = residPars,
      coeffVcov = coeffVcov,
      ranktst = ranktst,
      egger = egger,
      influ = influ,
      fsn.fit = fsn.fit,
      forestPlot = forestPlot,
      funnelPlot = funnelPlot,
      diagnosticsPlot = diagnosticsPlot,
      profilePlot = profilePlot,
      trimfillPlot = trimfillPlot
    )
    attr(state, "key") <- stateKey
  } else {
    status <- "inited"
  }

  return(list(results = results, status = status,
              state = state, keep = keep)
  )
}



analysisTitle <- function(object) {
  title <- capture.output(print(object, showfit = F, signif.legend = F))[2]
  title <- gsub("numeric\\(0\\)", "", title)
  title <- gsub("tau\\^2", '&tau;<sup style=font-size:small>2</sup>', title)
  if (is.na(title)) "Meta-analysis" else title
}

qTestsTable <- function(object) {
  # Return the ANOVA table with Q-tests for model significance and residual heterogeneity
  #print.output <- capture.output(print(object))
  table <- list()

  # Define table schema
  fields <- list(
    list(name = "name", type = "string", title = " "),
    list(name = "qstat", type = "number", format = "sf:4;dp:3", title = "Q"),
    list(name = "df", type = "integer", title = "df"),
    list(name = "pval", type = "number", format = "dp:3;p:.001", title = "p")
  )

  # Empty table.
  table[["schema"]] <- list(fields = fields)
  table[["title"]] <- "Fixed and Random Effects"
  table[["data"]] <- list(
    list(name="Omnibus test of Model Coefficients", qstat=".", df=".", pval = "."),
    list(name="Test of Residual Heterogeneity", qstat=".", df=".", pval = ".")
  )

  # Return empty table if no fit has been done
  if (is.null(object$QE)) return(table)

  # Fill table
  qstat  <- unlist(object[c('QE','QM')])
  df <- c(object$k - object$p, object$m)
  pval <- unlist(object[c('QEp','QMp')])
  name <- c("Test of Residual Heterogeneity","Omnibus test of Model Coefficients")
  qtable <- data.frame(name, qstat, df, pval)
  table[["data"]] <- lapply(2:1, function(i) as.list(qtable[i,]))
  table
}

rmaDiagnosticPlot <- function(object, width = 500, height = 500) {
  plot(object)
}

rmaForestPlot <- function(object, width = 500, height = 500) {
  cmaForest(object)
}
cmaForest <- function(x, ...) UseMethod("cmaForest")
cmaForest.dummy <- function(x, ...) {} # do nothing
cmaForest.rma <- function(x, ...) metafor::forest(x, ...)

print.dummy <- function(...) {
}

plot.dummy <- function(object, ...) {
  plot(1:10, col="transparent", axes=FALSE, xlab="", ylab="")
}

forest.dummy <- function(object, ...) {
  # do nothing
}

b64.rma <- function(object, ...) {
  ## Translate names in object x to base64

  # which elements in object have a names or dimnames attribute?
  idx <- which(sapply(object, function(x) !is.null(names(x)) || !is.null(dimnames(x))))
  if (is.null(list(...)$values)) {

    # infer the values to be translated from the object call
    formula <- eval(object$call$mods)
    if (!inherits(formula, "formula"))
      stop("Currently cannot infer 'values' when 'mods' is not a formula. Please provide 'values'.")
    values <- all.vars(formula)

    # translate object
    object$call$mods <- formula
    object[idx] <- b64(object[idx], values = values, ...)

  }
  else {

    # translate object
    object[idx] <- b64(object[idx], ...)
  }
  object
}
d64.rma <- function(object, ...) {
  ## Untranslate names in object x from base64

  # which elements in object have a names or dimnames attribute?
  idx <- which(sapply(object, function(x) !is.null(names(x)) || !is.null(dimnames(x))))

  if (is.null(list(...)$values)) {

    # infer the values to be untranslated from the object call
    formula <- try(eval(object$call$mods))
    if (!inherits(formula, "formula"))
      stop("Currently cannot infer 'values' when 'mods' is not a formula. Please provide 'values'.")
    values <- all.vars(formula)

    # untranslate object
    object$call$mods <- formula
    object[idx] <- d64(object[idx], values = values, ...)
  }
  else {

    # untranslate object
    object[idx] <- d64(object[idx], ...)
  }
  object
}
d64.fsn <- function(object, ...) {
  object # nothing to untranslate
}


### to JASP table conversion
## default formatting options
options(jasp_number_format = "sf:4;dp:3")
as.jaspTable <- function(x, ...) UseMethod("as_jaspTable")
as_jaspTable.data.frame <- function(x, title = "", ...) {
  jaspTable <- list(data = NULL, schema = NULL, title = title)

  # Extract table schema
  fields <- lapply(c("name", names(x)), function(name) {
    if (is.null(x[[name]]))
      return(list(name = "name", type = "string", format = "", title = ""))
    y <- na.omit(x[[name]])

    type <- ""
    tryCatch(if (all(is.numeric(y))) {
      if (any(abs(y - floor(y)) > 0L))
        type <- "number"
      else
        type <- "integer"
    } else {
      type <- "string"
    }, error = function(e) .quitAnalysis(paste(capture.output({print(x); print(name); print(y)}), collapse = "\n")))

    format <- ""
    if (type == "number") {
      if (! is.null(options(paste0("jasp_",type,"_format"))[[1]])) {
      format <- options(paste0("jasp_",type,"_format"))[[1]]
      } else {
        format <- "sf:4;dp:3"
      }
      if (name == "p" | name == "pval")
        format <- "dp:3;p:.001"
    }

    list(name = name, type = type, format = format, title = name)
  })
  jaspTable[["schema"]] <- list(fields = fields)

  # Populate the table
  jaspTable[["data"]] <- list(
    lapply(names(x), function(...) ".")
  )
  if (nrow(x) > 0) {
    Table <- .clean(x)
    Table <- cbind(name = rownames(Table), Table);

    jasp.table <- lapply(1:nrow(Table), function(i) as.list(Table[i,])) # apply converts all to string
    jaspTable[["data"]]  <- structure(jasp.table, .Names=NULL)
  }
  jaspTable
}

as_jaspTable.character <- function(x, y = NULL, ...) {
  table <- as.data.frame(matrix(".", ncol = length(x), nrow = length(y)))

  colnames(table) <- x
  if (! is.null(y))
    rownames(table) <- y

  as.jaspTable(table, ...)
}

as_jaspTable.matrix <- function(x, ...) {
  # use data.frame method
  jaspTable <- as.jaspTable(as.data.frame(x), ...)

  # recompute the "data" entry (necessary to work properly with repeated row names)
  if (nrow(x) > 0) {
    Table <- .clean(as.data.frame(x))
    Table <- cbind(name = rownames(x), Table);

    jasp.table <- lapply(1:nrow(Table), function(i) as.list(Table[i,])) # apply() converts all to string, so use lapply()
    jaspTable[["data"]]  <- structure(jasp.table, .Names=NULL)
  }
  jaspTable
}

vcov.dummy <- function(x, ...) {
  matrix(0,0,0)
}

error_if <- function(conditions, ...) {
  function() {
    paste(ifelse(conditions, do.call(paste0, list(...)), ""), collapse = "\n\n")
  }
}

# if(interactive()) {
#   # For debug purposes these are reset because the ones defined in 'common.R' only work properly
#   # from within JASP. Make sure to source('common.R') first!
#   #.beginSaveImage <- function(...) {}
#   #.endSaveImage <- function(...){}
#   .requestTempFileNameNative <- function(ext) tempfile(fileext = ext)
# }
