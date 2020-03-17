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

ClassicalMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- options$dependent != "" && options$wlsWeights != ""
  if(ready) {
    dataset <- .metaAnalysisReadData(dataset, options)
    .metaAnalysisCheckErrors(dataset, options)
  }
  
  # Output tables
  .metaAnalysisFixRandTable(jaspResults, dataset, options, ready)
  .metaAnalysisCoeffTable(jaspResults, dataset, options, ready)
  .metaAnalysisFitMeasuresTable(jaspResults, dataset, options, ready)
  .metaAnalysisResidualTable(jaspResults, dataset, options, ready)
  .metaAnalysisCovMatTable(jaspResults, dataset, options, ready)
  .metaAnalysisRankTestTable(jaspResults, dataset, options, ready)
  .metaAnalysisRegTestTable(jaspResults, dataset, options, ready)
  .metaAnalysisCasewiseTable(jaspResults, dataset, options, ready)
  .metaAnalysisFailSafeTable(jaspResults, dataset, options, ready)

  # Output plots
  .metaAnalysisForestPlot(jaspResults, dataset, options, ready)
  .metaAnalysisFunnelPlot(jaspResults, dataset, options, ready)
  .metaAnalysisDiagnosticPlot(jaspResults, dataset, options, ready)
  .metaAnalysisProfilePlot(jaspResults, dataset, options, ready)
  .metaAnalysisTrimFillPlot(jaspResults, dataset, options, ready)
  return()
}

## This file interfaces the metafor::rma function and associated diagnostic functions
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
#   ...:            numeric, -3 ??? value ??? 3, distribution used is dt((tStatistic - Tlocation) / Tscale, TDf)

.metaAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    effsizeName <- unlist(options$dependent)
    stderrName  <- unlist(options$wlsWeights)
    covarNames  <- if (length(options$covariates) > 0) unlist(options$covariates)
    factNames   <- if (length(options$factors) > 0) unlist(options$factors)

    numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
    factor.variables  <- Filter(function(s) s != "", c(factNames, options$studyLabels))
    return(.readDataSetToEnd(columns.as.factor   = factor.variables,
                             columns.as.numeric  = numeric.variables,
                             exclude.na.listwise = numeric.variables))
  }
}

.metaAnalysisCheckErrors <- function(dataset, options){
  effsizeName <- unlist(options$dependent)
  stderrName  <- unlist(options$wlsWeights)
  covarNames  <- if (length(options$covariates) > 0) unlist(options$covariates)
  numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
  .hasErrors(dataset              = dataset,
             type                 = c("infinity", "observations", "variance"),
             all.target           = numeric.variables,
             observations.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)
  .hasErrors(dataset              = dataset,
             type                 = c("modelInteractions"),
             modelInteractions.modelTerms = options$modelTerms,
             exitAnalysisIfErrors = TRUE)
  .hasErrors(dataset              = dataset,
             type                 = c("negativeValues"),
             negativeValues.target= options$wlsWeights,
             exitAnalysisIfErrors = TRUE)
}

.metaAnalysisComputeModel <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["Model"]]))
    return(jaspResults[["Model"]]$object)
  rma.fit <- structure(list('b'     = numeric(),
                            'se'    = numeric(),
                            'ci.lb' = numeric(),
                            'ci.ub' = numeric(),
                            'zval'  = numeric(),
                            'pval'  = numeric()),
                       class = c("dummy", "rma"))
  if (ready) {
    # analysis
    rma.fit <- tryCatch( # rma generates informative error messages; use them!
      metafor::rma(
        yi      = get(.v(options$dependent)),
        sei     = get(.v(options$wlsWeights)),
        data    = dataset,
        method  = .metaAnalysisGetMethod(options),
        mods    = .metaAnalysisFormula(options),
        test    = options$test,
        slab    = if(options$studyLabels != "") paste0(get(.v(options$studyLabels))),
        # add tiny amount because 1 is treated by rma() as 100% whereas values > 1 as percentages
        level   = options$regressionCoefficientsConfidenceIntervalsInterval + 1e-9,
        control = list(maxiter = 500)),
      error = function(e) .quitAnalysis(gettextf("The metafor package crashed with the following error: %s", e$message)))

    rma.fit <- .unv(rma.fit)
  }

  # Save results to state
  jaspResults[["Model"]] <- createJaspState(rma.fit)
  jaspResults[["Model"]]$dependOn(c("modelTerms", "dependent", "wlsWeights", "test", "studyLabels",
                                    "regressionCoefficientsConfidenceIntervalsInterval", "method"))
  return(rma.fit)
}

#Tables
.metaAnalysisFixRandTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["fixRandTable"]])) return()

  mainTable  <- createJaspTable(gettext("Fixed and Random Effects"))
  mainTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels", "method"))
  mainTable$position <- 1
  mainTable$addCitation("Hedges, L. V., & Olkin, I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press.")

  mainTable$addColumnInfo(name = "name",  type = "string",  title = "")
  mainTable$addColumnInfo(name = "qstat", type = "number",  title = gettext("Q"))
  mainTable$addColumnInfo(name = "df",    type = "integer", title = gettext("df"))
  mainTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  mainTable$addFootnote(gettext("<em>p</em>-values are approximate."))

  jaspResults[["fixRandTable"]] <- mainTable

  res <- try(.metaAnalysisFixRandFill(jaspResults, dataset, options, ready))

  .metaAnalysisSetError(res, mainTable)
}

.metaAnalysisCoeffTable <- function(jaspResults, dataset, options, ready) {
  if (!options$regressionCoefficientsEstimates || !is.null(jaspResults[["coeffTable"]]))
    return()

  coeffTable <- createJaspTable(gettext("Coefficients"))
  coeffTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels", "method", "includeConstant",
                        "regressionCoefficientsConfidenceIntervals", "regressionCoefficientsEstimates"))
  coeffTable$position <- 2
  coeffTable$showSpecifiedColumnsOnly <- TRUE
  coeffTable$addCitation("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/")

  coeffTable$addColumnInfo(name = "name",  type = "string", title = "")
  coeffTable$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  coeffTable$addColumnInfo(name = "se",    type = "number", title = gettext("Standard Error"))
  coeffTable$addColumnInfo(name = "zval",  type = "number", title = gettext("z"))
  coeffTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  .metaAnalysisConfidenceInterval(options, coeffTable)

  coeffTable$addFootnote(switch(options$test, z = gettext("Wald test."), gettext("Wald tests.")))

  jaspResults[["coeffTable"]] <- coeffTable
  if(!ready)
    return()

  res <- try(.metaAnalysisCoeffFill(jaspResults, dataset, options))

  .metaAnalysisSetError(res, coeffTable)
}

.metaAnalysisFitMeasuresTable <- function(jaspResults, dataset, options, ready) {
  if (!options$modelFit || !is.null(jaspResults[["fitMeasuresTable"]]))
    return()

  fitMeasuresTable <- createJaspTable(gettext("Fit measures"))
  fitMeasuresTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors",
                              "studyLabels", "modelFit", "method"))
  fitMeasuresTable$position <- 3

  method <- .metaAnalysisGetTranslatedMethod(options)

  fitMeasuresTable$addColumnInfo(name = "name",   type = "string", title = "")
  fitMeasuresTable$addColumnInfo(name = "method", type = "number", title = method)

  jaspResults[["fitMeasuresTable"]] <- fitMeasuresTable

  res <- try(.metaAnalysisFitMeasuresFill(jaspResults, dataset, options, ready))

  .metaAnalysisSetError(res, fitMeasuresTable)
}

.metaAnalysisResidualTable <- function(jaspResults, dataset, options, ready) {
  method <- .metaAnalysisGetMethod(options)

  if (!options$residualsParameters || method == "FE" || !is.null(jaspResults[["residualTable"]]))
    return()

  residualTable <- createJaspTable(gettext("Residual Heterogeneity Estimates"))
  residualTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                           "regressionCoefficientsConfidenceIntervals", "residualsParameters", "method"))
  residualTable$position <- 4
  residualTable$showSpecifiedColumnsOnly <- TRUE

  residualTable$addColumnInfo(name = "name",  type = "string",  title = "")
  residualTable$addColumnInfo(name = "est",   type = "number",  title = gettext("Estimate"))
  .metaAnalysisConfidenceInterval(options, residualTable)

  jaspResults[["residualTable"]] <- residualTable

  res <- try(.metaAnalysisResidualFill(jaspResults, dataset, options, ready))

  .metaAnalysisSetError(res, residualTable)
}

.metaAnalysisCovMatTable <- function(jaspResults, dataset, options, ready) {
  if (!options$regressionCoefficientsCovarianceMatrix || !is.null(jaspResults[["covMatTable"]]))
    return()

  covMatTable <- createJaspTable(gettext("Parameter Covariances"))
  covMatTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                         "regressionCoefficientsCovarianceMatrix", "method"))
  covMatTable$position <- 5
  covMatTable$showSpecifiedColumnsOnly <- TRUE

  covMatTable$addColumnInfo(name = "name",  type = "string",  title = "")
  if(!ready) {
    coeffVcov <- NULL
    covMatTable$addColumnInfo(name = "intercept", type = "number", title = "...")
  } else {
    rma.fit   <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
    coeffVcov <- try(vcov(rma.fit))
    colnames(coeffVcov) <- .metaAnalysisMakePrettyCoeffNames(colnames(coeffVcov), dataset)
    for (i in seq_along(colnames(coeffVcov)))
      covMatTable$addColumnInfo(name = colnames(coeffVcov)[i], type = "number")
  }

  jaspResults[["covMatTable"]] <- covMatTable

  res <- try(.metaAnalysisCovMatFill(jaspResults, dataset, options, ready, coeffVcov))

  .metaAnalysisSetError(res, covMatTable)
}

.metaAnalysisRankTestTable <- function(jaspResults, dataset, options, ready) {
  if (!options$rSquaredChange || !is.null(jaspResults[["rankTestTable"]]))
    return()

  rankTestTable <- createJaspTable(gettext("Rank correlation test for Funnel plot asymmetry"))
  rankTestTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                           "rSquaredChange", "method"))
  rankTestTable$position <- 6
  rankTestTable$showSpecifiedColumnsOnly <- TRUE


  rankTestTable$addColumnInfo(name = "name",    type = "string", title = "")
  rankTestTable$addColumnInfo(name = "kendall", type = "number", title = gettextf("Kendall's %s", "\u3C4"))
  rankTestTable$addColumnInfo(name = "pval",    type = "pvalue", title = gettext("p"))

  jaspResults[["rankTestTable"]] <- rankTestTable

  res <- try(.metaAnalysisRankTestFill(jaspResults, dataset, options, ready))

  .metaAnalysisSetError(res, rankTestTable)
}

.metaAnalysisRegTestTable <- function(jaspResults, dataset, options, ready) {
  if (!options$funnelPlotAsymmetryTest || !is.null(jaspResults[["regTestTable"]]))
    return()
  regTestTable <- createJaspTable(gettext("Regression test for Funnel plot asymmetry (\"Egger's test\")"))
  regTestTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                          "funnelPlotAsymmetryTest", "test", "method"))
  regTestTable$position <- 6
  regTestTable$showSpecifiedColumnsOnly <- TRUE
  regTestTable$addCitation("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. <em>Journal of Statistical Software</em>, <b>36</b>(3), 1-48.")

  regTestTable$addColumnInfo(name = "name",    type = "string", title = "")
  if (options$test == "knha")
    title <- gettext("t")
  else
    title <- gettext("z")
  regTestTable$addColumnInfo(name = "test", type = "number", title = title)
  regTestTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  jaspResults[["regTestTable"]] <- regTestTable

  if(!ready)
    return()

  res <- try(.metaAnalysisRegTestFill(jaspResults, dataset, options))

  .metaAnalysisSetError(res, regTestTable)
}

.metaAnalysisCasewiseTable <- function(jaspResults, dataset, options, ready) {
  if (!options$residualsCasewiseDiagnostics || !is.null(jaspResults[["casewiseTable"]]))
    return()

  casewiseTable <- createJaspTable(gettext("Influence Measures"))
  casewiseTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                           "residualsCasewiseDiagnostics", "method"))
  casewiseTable$position <- 6
  casewiseTable$showSpecifiedColumnsOnly <- TRUE

  casewiseTable$addColumnInfo(name = "name",   type = "string",  title = "")
  casewiseTable$addColumnInfo(name = "sdRes",  type = "number",  title = gettext("Std. Residual"))
  casewiseTable$addColumnInfo(name = "dfFits", type = "number",  title = gettext("DFFITS"))
  casewiseTable$addColumnInfo(name = "cook",   type = "number",  title = gettext("Cook's Distance"))
  casewiseTable$addColumnInfo(name = "cov",    type = "number",  title = gettext("Cov. Ratio"))
  casewiseTable$addColumnInfo(name = "tau2",   type = "number",  title = gettextf("%s%s<sub>(-i)</sub>", "\u3C4", "\u00B2"))
  casewiseTable$addColumnInfo(name = "QE",     type = "number",  title = gettext("Q<sub>E(-i)</sub>"))
  casewiseTable$addColumnInfo(name = "hat",    type = "number",  title = gettext("Hat"))
  casewiseTable$addColumnInfo(name = "weight", type = "number",  title = gettext("Weight"))

  jaspResults[["casewiseTable"]] <- casewiseTable

  if(!ready)
    return()

  res <- try(.metaAnalysisCasewiseFill(jaspResults, dataset, options))

  .metaAnalysisSetError(res, casewiseTable)
}

.metaAnalysisFailSafeTable <- function(jaspResults, dataset, options, ready) {
  if (!options$plotResidualsCovariates || !is.null(jaspResults[["failSafeTable"]]) || !ready)
    return()

  failSafeTable <- createJaspTable(gettext("File Drawer Analysis"))
  failSafeTable$dependOn(c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                           "plotResidualsCovariates", "method"))
  failSafeTable$position <- 6
  failSafeTable$showSpecifiedColumnsOnly <- TRUE

  failSafeTable$addColumnInfo(name = "name",  type = "string", title = "")
  failSafeTable$addColumnInfo(name = "fsnum", type = "number", title = gettext("Fail-safe N"))
  failSafeTable$addColumnInfo(name = "alpha", type = "number", title = gettext("Target Significance"))
  failSafeTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("Observed Significance"))

  jaspResults[["failSafeTable"]] <- failSafeTable

  res <- try(.metaAnalysisFailSafeFill(jaspResults, dataset, options))

  .metaAnalysisSetError(res, failSafeTable)
}

#Table filling
.metaAnalysisFixRandFill <- function(jaspResults, dataset, options, ready) {
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)

  row <- list(name = gettext("Omnibus test of Model Coefficients"),
              qstat = ".", df = ".", pval = ".")
  if(ready) {
    row$qstat <- rma.fit$QM
    row$df    <- rma.fit$m
    row$pval  <- rma.fit$QMp
  }
  jaspResults[["fixRandTable"]]$addRows(row)
  row <- list(name = gettext("Test of Residual Heterogeneity"),
              qstat = ".", df = ".", pval = ".")
  if(ready) {
    row$qstat <- rma.fit$QE
    row$df    <- rma.fit$k - rma.fit$p
    row$pval  <- rma.fit$QEp
  }
  jaspResults[["fixRandTable"]]$addRows(row)
}

.metaAnalysisCoeffFill <- function(jaspResults, dataset, options) {
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready = TRUE)
  coeff   <- coef(summary(rma.fit))
  cov     <-.metaAnalysisMakePrettyCoeffNames(rownames(coeff), dataset)
  if (options$includeConstant) {
    start <- 1
  } else {
    if (length(cov) == 1)
      return()
    else
      start <- 2
  }
  for (i in start:length(cov)) {
    jaspResults[["coeffTable"]]$addRows(list(
      name  = cov[[i]],
      est   = coeff[i,1],
      se    = coeff[i,2],
      zval  = coeff[i,3],
      pval  = coeff[i,4],
      lower = coeff[i,5],
      upper = coeff[i,6]
    ))
  }
}

.metaAnalysisFitMeasuresFill <- function(jaspResults, dataset, options, ready) {
  stats <- list(
    logLik   = ".",
    deviance = ".",
    AIC      = ".",
    BIC      = ".",
    AICc     = "."
  )
  if(ready) {
    # Compute/get model
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
    fitStats <- try(metafor:::fitstats(rma.fit))
    stats$logLik   <- fitStats[[1]]
    stats$deviance <- fitStats[[2]]
    stats$AIC      <- fitStats[[3]]
    stats$BIC      <- fitStats[[4]]
    stats$AICc     <- fitStats[[5]]
  }

  # Fill table
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = gettext("Log-likelihood"), method = stats$logLik))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = gettext("Deviance"),       method = stats$deviance))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = gettext("AIC"),            method = stats$AIC))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = gettext("BIC"),            method = stats$BIC))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = gettext("AICc"),           method = stats$AICc))
}

.metaAnalysisResidualFill <- function(jaspResults, dataset, options, ready) {
  est <- ci.lower <- ci.upper <- list(
    tau2 = ".",
    tau  = ".",
    I2   = ".",
    H2   = "."
  )

  if (ready) {
    # Compute/get model
    rma.fit   <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
    confInt   <- options$regressionCoefficientsConfidenceIntervalsInterval
    residPars <- try(confint(rma.fit, digits = 12, level = confInt)$random)

    est$tau2 <- residPars[1,1]
    est$tau  <- residPars[2,1]
    est$I2   <- residPars[3,1]
    est$H2   <- residPars[4,1]

    ci.lower$tau2 <- residPars[1,2]
    ci.lower$tau  <- residPars[2,2]
    ci.lower$I2   <- residPars[3,2]
    ci.lower$H2   <- residPars[4,2]

    ci.upper$tau2 <- residPars[1,3]
    ci.upper$tau  <- residPars[2,3]
    ci.upper$I2   <- residPars[3,3]
    ci.upper$H2   <- residPars[4,3]
  }

  ##TODO: need name column entries in <em></em>
  # Fill table
  jaspResults[["residualTable"]]$addRows(list(
    list(name = "\u3C4\u00B2", est = est$tau2,
         lower = ci.lower$tau2, upper = ci.upper$tau2),
    list(name = "\u3C4", est = est$tau,
         lower = ci.lower$tau, upper = ci.upper$tau),
    list(name = gettextf("I%s (%%)", "\u00B2"), est = est$I2,
         lower = ci.lower$I2, upper = ci.upper$I2),
    list(name = gettextf("H%s", "\u00B2"), est = est$H2,
         lower = ci.lower$H2, upper = ci.upper$H2)
  ))
}

.metaAnalysisCovMatFill <- function(jaspResults, dataset, options, ready, coeffVcov) {
  if(ready) {
    cov <- colnames(coeffVcov)
    for(i in 1:length(cov)) {
      row <- list(name = cov[[i]])
      for(j in 1:length(cov))
        row[[paste(cov[[j]])]]  <- coeffVcov[i,j]
      jaspResults[["covMatTable"]]$addRows(row)
    }
  } else
    jaspResults[["covMatTable"]]$addRows(list(name = "...", intercept = "."))
}

.metaAnalysisRankTestFill <- function(jaspResults, dataset, options, ready) {
  results <- list(name = gettext("Rank test"), kendall = ".", pval = ".")
  if(ready) {
    # Compute/get model
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
    ranktst <- unlist(metafor::ranktest(rma.fit))
    results$kendall <- ranktst[[1]]
    results$pval    <- ranktst[[2]]
  }
  jaspResults[["rankTestTable"]]$addRows(results)
}

.metaAnalysisRegTestFill <- function(jaspResults, dataset, options) {
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready = TRUE)
  egger   <- metafor::regtest(rma.fit)
  jaspResults[["regTestTable"]]$setData(list(name = egger$predictor, test = egger$zval, pval = egger$pval))
}

.metaAnalysisCasewiseFill <- function(jaspResults, dataset, options) {
  rma.fit       <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready = TRUE)
  influ <- influence(rma.fit)
  influenceVals <- influ$inf
  isInfluential <- influ$is.infl
  
  if (sum(isInfluential) > 0)
    jaspResults[["casewiseTable"]]$addFootnote(gettextf("Cases marked with %s are influential.", "\u002A"))

  for (i in 1:length(influenceVals$rstudent)) {
    name <- influenceVals$slab[i]
    if (!is.na(isInfluential[i]) && isInfluential[i])
      name <- paste0(name, "\u002A")

    jaspResults[["casewiseTable"]]$addRows(list(
      name   = name,
      sdRes  = influenceVals$rstudent[i],
      dfFits = influenceVals$dffits[i],
      cook   = influenceVals$cook.d[i],
      cov    = influenceVals$cov.r[i],
      tau2   = influenceVals$tau2.del[i],
      QE     = influenceVals$QE.del[i],
      hat    = influenceVals$hat[i],
      weight = influenceVals$weight[i]
    ))
  }
}

.metaAnalysisFailSafeFill <- function(jaspResults, dataset, options) {
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
  fsn.fit <- metafor::fsn(yi   = get(.v(options$dependent)),
                          sei  = get(.v(options$wlsWeights)),
                          data = dataset)
  fsn.fit <- .unv(fsn.fit)
  jaspResults[["failSafeTable"]]$addRows(list("name" = fsn.fit$type,
                                              "fsnum" = fsn.fit$fsnum,
                                              "alpha" = fsn.fit$alpha,
                                              "pval"  = fsn.fit$pval))
}

# Plots
.metaAnalysisPlotsContainer <- function(jaspResults, options, ready)  {
  if(!ready) return()
  if(!options$forestPlot && !options$funnelPlot && !options$plotResidualsDependent &&
     !options$plotResidualsPredicted && !options$trimFillPlot)
    return()
  if (is.null(jaspResults[["plots"]])) {
    container <- createJaspContainer(gettext("Plot"))
    container$dependOn(c("dependent", "wlsWeights", "method",
                         "studyLabels", "covariates", "modelTerms"))
    jaspResults[["plots"]] <- container
  }
}

.metaAnalysisForestPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$forestPlot)
    return()
  .metaAnalysisPlotsContainer(jaspResults, options, ready)
  container <- jaspResults[["plots"]]
  # Compute/get model
  rma.fit    <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
  img.height <- 400
  if(!is.null(rma.fit))
    img.height <- max(520, nobs(rma.fit) * 20)
  forestPlot   <- createJaspPlot(title = gettext("Forest plot"), width = 520, height = img.height)
  forestPlot$position <- 1
  forestPlot$dependOn(c("forestPlot"))
  container[["forest"]] <- forestPlot
  if(ready){
    p <- try(.metaAnalysisForestPlotFill(rma.fit))
    if(isTryError(p))
      forestPlot$setError(.extractErrorMessage(p))
    else
      forestPlot$plotObject <- p
  }
  return()
}

.metaAnalysisFunnelPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$funnelPlot)
    return()
  .metaAnalysisPlotsContainer(jaspResults, options, ready)
  container <- jaspResults[["plots"]]
  # Compute/get model
  rma.fit    <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)

  funnelPlot   <- createJaspPlot(title = gettext("Funnel Plot"), width = 520, height = 520)
  funnelPlot$position <- 2
  funnelPlot$dependOn(c("funnelPlot"))
  container[["funnel"]] <- funnelPlot

  if(ready){
    p <- try(.metaAnalysisFunnelPlotFill(rma.fit))
    if(isTryError(p))
      funnelPlot$setError(.extractErrorMessage(p))
    else
      funnelPlot$plotObject <- p
  }
  return()
}

.metaAnalysisProfilePlot <- function(jaspResults, dataset, options, ready) {
  if(!options$plotResidualsPredicted)
    return()
  .metaAnalysisPlotsContainer(jaspResults, options, ready)
  container <- jaspResults[["plots"]]
  # Compute/get model
  rma.fit    <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)

  profilePlot   <- createJaspPlot(title = gettextf("Log-likelihood for %s%s", "\u3C4", "\u00B2"), width = 520, height = 520)
  profilePlot$position <- 4
  profilePlot$dependOn(c("plotResidualsPredicted"))
  container[["profile"]] <- profilePlot
  if(ready){
    p <- try(.metaAnalysisProfilePlotFill(rma.fit))
    if(isTryError(p))
      profilePlot$setError(.extractErrorMessage(p))
    else
      profilePlot$plotObject <- p
  }
  return()
}

.metaAnalysisTrimFillPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$trimFillPlot)
    return()
  .metaAnalysisPlotsContainer(jaspResults, options, ready)
  container <- jaspResults[["plots"]]
  # Compute/get model
  trimFillPlot <- createJaspPlot(title = gettext("Trim-fill Analysis"), width = 820, height = 820)
  trimFillPlot$position <- 5
  trimFillPlot$dependOn(c("trimFillPlot"))
  container[["trimFill"]] <- trimFillPlot

  if (ready) {
    rma.fit      <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
    trimfill.fit <- metafor::trimfill(update(rma.fit, mods = ~1))
    
    p <- try(.metaAnalysisTrimFillPlotFill(trimfill.fit))
    if (isTryError(p))
      trimFillPlot$setError(.extractErrorMessage(p))
    else
      trimFillPlot$plotObject <- p
  }
  return()
}

.metaAnalysisDiagnosticPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$plotResidualsDependent)
    return()
  .metaAnalysisPlotsContainer(jaspResults, options, ready)
  container <- jaspResults[["plots"]]
  # Compute/get model
  rma.fit   <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready)
  diagnosticPlot <- createJaspPlot(title = gettext("Diagnostic Plots"), width = 820, height = 820)
  diagnosticPlot$position <- 3
  diagnosticPlot$dependOn(c("plotResidualsDependent", "plotResidualsQQ"))
  container[["diagnosticPlot"]] <- diagnosticPlot
  if(ready){
    p <- try(.metaAnalysisDiagnosticPlotFill(container, rma.fit,
                                             qqplot = options$plotResidualsQQ,
                                             radial = rma.fit$int.only))
    if(isTryError(p))
      diagnosticPlot$setError(.extractErrorMessage(p))
    else
      diagnosticPlot$plotObject <- p
  }
  return()
}

#Plot filling
.metaAnalysisTrimFillPlotFill <- function(trimfill.fit){
  plotMat <- matrix(list(), 2, 2)
  plotMat[[1,1]] <- .metaAnalysisForestPlotFill(trimfill.fit)
  plotMat[[1,2]] <- .metaAnalysisFunnelPlotFill(trimfill.fit)
  plotMat[[2,1]] <- .metaAnalysisRadialPlotFill(trimfill.fit)
  plotMat[[2,2]] <- .metaAnalysisQQPlotFill(trimfill.fit)
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, scaleXYlabels = NULL)
  return(p)
}

.metaAnalysisDiagnosticPlotFill <- function(container, rma.fit, qqplot, radial = TRUE) {
  plotMat <- matrix(list(), 2, 2)

  if(!is.null(container[["forest"]]))
    plotMat[[1,1]] <- container[["forest"]][["plotObject"]]
  else
    plotMat[[1,1]] <- .metaAnalysisForestPlotFill(rma.fit)

  if(!is.null(container[["funnel"]]))
    plotMat[[1,2]] <- container[["funnel"]][["plotObject"]]
  else
    plotMat[[1,2]] <- .metaAnalysisFunnelPlotFill(rma.fit)

  if(radial)
    plotMat[[2,1]] <- .metaAnalysisRadialPlotFill(rma.fit)
  else
    plotMat[[2,1]] <- .metaAnalysisFittedVsStandardPlotFill(rma.fit)

  if(qqplot)
    plotMat[[2,2]] <- .metaAnalysisQQPlotFill(rma.fit)
  else
    plotMat[[2,2]] <- .metaAnalysisStandResidPlotFill(rma.fit)

  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, scaleXYlabels = NULL)
  return(p)
}

.metaAnalysisForestPlotFill <- function(rma.fit){
  ci.lb    <- rma.fit$yi - qnorm(rma.fit$level/2, lower.tail = FALSE) * sqrt(rma.fit$vi)
  ci.ub    <- rma.fit$yi + qnorm(rma.fit$level/2, lower.tail = FALSE) * sqrt(rma.fit$vi)
  xlims    <- c(-1, rma.fit$k+1)
  ylims    <- c(min(ci.lb), max(ci.ub))
  ci.int   <- sprintf("%.2f [%.2f, %.2f]", rma.fit$yi, ci.lb, ci.ub)
  b.pred   <- predict(rma.fit)
  b.ci.lb  <- round(b.pred$ci.lb, 2)
  b.ci.ub  <- round(b.pred$ci.ub, 2)
  b.ci.int <- sprintf("%.2f [%.2f, %.2f]", b.pred$pred, b.ci.lb, b.ci.ub)

  cols <- c("black", "grey")

  k     <- rma.fit$k
  wi    <- weights(rma.fit)
  psize <- wi/sum(wi, na.rm = TRUE)
  rng   <- max(psize, na.rm = TRUE) - min(psize, na.rm = TRUE)
  if (rng <= .Machine$double.eps^0.5)
    psize <- rep(1, k)
  else
    psize <- 0.5 * (psize - min(psize, na.rm = TRUE))/rng
  if (all(is.na(psize)))
    psize <- rep(1, k)
  if(rma.fit$slab.null)
    slabs <- gettextf("Study %s", rma.fit$ids[rma.fit$not.na])
  else
    slabs <- rma.fit$slab[rma.fit$not.na]
  studyNos <- rma.fit$ids[rma.fit$not.na]
  studies.not.na <- sum(rma.fit$not.na)
  rma.data <-  data.frame(StudyNo = studies.not.na + 1 - 1:studies.not.na,
                          labs    = slabs,
                          ES      = rma.fit$yi,
                          ci.int  = ci.int,
                          ci.lb   = ci.lb,
                          ci.ub   = ci.ub,
                          shape   = rep(15, sum(rma.fit$not.na)),
                          size    = psize)

  #grey polygons when not intercept only
  if(!rma.fit$int.only){
    pred   <- fitted(rma.fit)
    height <- (xlims[2] - xlims[1])/50
    alim   <- range(k + 1 - rma.fit$ids[rma.fit$not.na])
    add.data <- data.frame()
    for(study in 1:studies.not.na) {
      if (is.na(pred[study]))
        next
      rownum <- studies.not.na + 1 - study
      row1 <- data.frame(ES      = c(pred[study], b.ci.ub[study], pred[study]),
                         StudyNo = c(rownum - height, rownum, rownum + height),
                         group   = as.character(rep(study,3)))
      row2 <- data.frame(ES      = c(pred[study], b.ci.lb[study], pred[study]),
                        StudyNo  = c(rownum - height, rownum, rownum + height),
                        group    = as.character(rep(study+0.5,3)))
      add.data <- rbind(add.data, row1, row2)
    }
  }

  if(rma.fit$int.only){
    mName <- ifelse(rma.fit$method == "FE", gettext("FE Model"), gettext("RE Model"))
    mData <- data.frame(StudyNo = -1,
                        labs    = mName,
                        ES      = b.pred$pred,
                        ci.int  = b.ci.int,
                        ci.lb   = b.ci.lb,
                        ci.ub   = b.ci.ub,
                        shape   = 18,
                        size    = 1)
    dat <- rbind(rma.data, mData)
  }
  else
    dat <- rma.data



  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = StudyNo, y = ES))
  if(!rma.fit$int.only)
    p <- p + ggplot2::geom_polygon(data = add.data, fill = "grey75", ggplot2::aes(group = group))

  p <- p + ggplot2::geom_point(data = dat, ggplot2::aes(size = size, shape = factor(shape)), colour = cols[1]) +
    ggplot2::geom_errorbar(ggplot2::aes(x = StudyNo, ymax = ci.ub, ymin = ci.lb),
                           width = 0.5, colour = cols[1]) +
    ggplot2::scale_shape_manual(values = c(15, 18))

  p <- p +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0),   lty = "dotted", size = 0.5, colour = cols[1]) +
    ggplot2::annotate("segment", x = k + .95, xend = k + .95, y = 10 * ylims[1], yend = 10 * ylims[2])
  if(rma.fit$int.only)
    p <- p + ggplot2::annotate("segment", x = 0, xend = 0, y = 10 * ylims[1], yend = 10 * ylims[2])

  # clip = "off" allows us to draw outside of the margins using the annotate("segment", ...) above.
  p <- p + ggplot2::coord_flip(ylim = ylims, clip = "on") +
    ggplot2::xlab(NULL) + ggplot2::ylab(gettext("Observed Outcome")) +
    ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(ylims),
                                expand = ggplot2::expand_scale(mult = c(0.3,0.3), add = 0))

  p <- p + ggplot2::scale_x_continuous(breaks   = dat$StudyNo,
                                       limits   = xlims,
                                       labels   = dat$labs,
                                       sec.axis = ggplot2::dup_axis(trans = ~., labels = dat$ci.int),
                                       expand   = ggplot2::expand_scale(mult = c(0.1,0), add = 0))

  fontsize <- 0.85 * JASPgraphs::getGraphOption("fontsize")
  p <- p + JASPgraphs::geom_rangeframe(sides = "b") + JASPgraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks.y      = ggplot2::element_blank(),
                   axis.text.y.left  = ggplot2::element_text(hjust = 0, size = fontsize),
                   axis.text.y.right = ggplot2::element_text(hjust = 1, size = fontsize),
                   plot.margin       = ggplot2::margin(5))

  return(p)
}

.metaAnalysisProfilePlotFill <- function(rma.fit){
  x <- rma.fit
  vc.ci <- try(suppressWarnings(confint(x)), silent = TRUE)
  if (isTryError(vc.ci)) {
    vc.lb <- NA
    vc.ub <- NA
  }
  else {
    vc.lb <- min(x$tau2, vc.ci$random[1, 2])
    vc.ub <- max(0.1, x$tau2, vc.ci$random[1, 3])
  }
  if (is.na(vc.lb) || is.na(vc.ub)) {
    vc.lb <- max(0, x$tau2 - 1.96 * x$se.tau2)
    vc.ub <- max(0.1, x$tau2 + 1.96 * x$se.tau2)
  }
  if (is.na(vc.lb) || is.na(vc.ub)) {
    vc.lb <- max(0, x$tau2/4)
    vc.ub <- max(0.1, x$tau2 * 4)
  }
  if (is.na(vc.lb) || is.na(vc.ub))
    stop("Cannot set 'xlim' automatically. Please set this argument manually.")
  xlim <- c(vc.lb, vc.ub)
  vcs  <- seq(xlim[1], xlim[2], length = 20)
  lls  <- rep(NA_real_, length(vcs))
  for (i in seq_along(vcs)) {
    res <- try(suppressWarnings(metafor::rma.uni(x$yi, x$vi, weights = x$weights,
                                                 mods = x$X, intercept = FALSE, method = x$method,
                                                 weighted = x$weighted, test = x$test, level = x$level,
                                                 control = x$control, tau2 = vcs[i])), silent = TRUE)
    if (isTryError(res))
      next
    lls[i] <- c(logLik(res))
  }

  xlab         <- bquote(paste(tau^2, .(gettext(" Value"))))
  title        <- bquote(paste(.(gettext("Profile Plot for ")), tau^2))
  profile.data <- data.frame(tau2 = vcs, ll = lls)

  ylim <- c(min(lls, logLik(x)[1]), max(lls, logLik(x)[1] + 0.001*abs(logLik(x)[1])))

  p <- ggplot2::ggplot(data = profile.data, ggplot2::aes(x = tau2, y = ll)) +
    ggplot2::geom_point(data = profile.data, shape = 19, colour = "black") +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = logLik(x), linetype = "dotted", colour = "black") +
    ggplot2::geom_vline(xintercept = x$tau2,    linetype = "dotted", colour = "black") +
    ggplot2::xlab(xlab) + ggplot2::ylab(gettext("Restricted Log-Likelihood")) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(xlim),
                                limits = xlim) +
    ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(ylim),
                                limits = ylim)
  p <- p + ggplot2::theme(axis.line.x = ggplot2::element_line(),
                          axis.line.y = ggplot2::element_line())
  p <- JASPgraphs::themeJasp(p, legend.position = "none")
  return(p)
}

.metaAnalysisFunnelPlotFill <- function(rma.fit){
  x        <- rma.fit
  level    <- x$level
  ci.res   <- 1000
  k        <- x$k

  if (!inherits(x, "rma.uni.trimfill")) {
    col.vec <- FALSE
    col <- rep("black", x$k.all)
    if (!is.null(x$subset))
      col <- col[x$subset]
    bg.vec <- FALSE
    bg <- rep("white", x$k.all)
    if (!is.null(x$subset))
      bg <- bg[x$subset]
  }
  else {
    col <- c("black", "black")
    if (length(col) == 1L)
      col <- c(col, "black")
    col.vec <- FALSE
    bg <- c("white", "white")
    if (length(bg) == 1L)
      bg <- c(bg, "white")
    bg.vec <- FALSE
  }
  if (x$int.only) {
    refline <- c(x$beta)
    yi    <- x$yi
    vi    <- x$vi
    ni    <- x$ni
    sei   <- sqrt(vi)
    slab  <- x$slab[x$not.na]
    xlab  <- x$measure
  }
  else {
    refline <- 0
    res     <- rstandard(x)
    not.na  <- x$not.na
    #not.na  <- !is.na(res$resid)
    yi      <- res$resid[not.na]
    sei     <- res$se[not.na]
    ni      <- x$ni.f[not.na]
    vi      <- sei^2
    slab    <- x$slab[not.na]
    xlab    <- gettext("Residual Value")
  }

  ylim <- c(0, max(sei[!is.na(sei)]))

  level <- ifelse(level == 0, 1,
                  ifelse(level >= 1, (100 - level)/100,
                         ifelse(level > 0.5, 1 - level, level)))
  level.min <- min(level)
  lvals     <- length(level)

  x.lb.bot <- refline - qnorm(level.min/2, lower.tail = FALSE) * ylim[2]
  x.ub.bot <- refline + qnorm(level.min/2, lower.tail = FALSE) * ylim[2]

  xlim    <- c(min(x.lb.bot, min(yi[!is.na(yi)])), max(x.ub.bot, max(yi[!is.na(yi)])))
  rxlim   <- xlim[2] - xlim[1]
  xlim[2] <- xlim[1] - (rxlim * 0.1)
  xlim[1] <- xlim[2] + (rxlim * 0.1)

  new_ylim <- c()
  rylim <- ylim[1] - ylim[2]
  new_ylim[1] <- ylim[1]
  #new_ylim[1] <- ylim[1] + (rylim * 0.1)
  new_ylim[2] <- ylim[2]
  new_ylim[2] <- max(0, ylim[2] - (rylim * 0.1))

  yi.vals <- seq(from = new_ylim[2], to = new_ylim[1], length.out = ci.res)

  xaxis.vals <- yi
  yaxis.vals <- sei

  ci.left  <- refline - qnorm(level[1]/2, lower.tail = FALSE) * yi.vals
  ci.right <- refline + qnorm(level[1]/2, lower.tail = FALSE) * yi.vals

  xend <- max(abs(c(ci.left, ci.right, xlim)))
  xlims <- c(-1*xend, xend)

  if(inherits(x, "rma.uni.trimfill")) {
    fillcol <- ifelse(rma.fit$fill, "white", "black")
    shape   <- ifelse(rma.fit$fill, 1, 19)
  } else {
    fillcol <- rep("black", sum(rma.fit$not.na))
    shape   <- 19
  }

  funnel.data   <- data.frame(x = xaxis.vals, y = yaxis.vals, slab = slab, fill = fillcol)
  triangle.data <- data.frame(x = c(ci.left, ci.right[ci.res:1]),
                              y = c(yi.vals, yi.vals[ci.res:1]))
  hlines <- JASPgraphs::getPrettyAxisBreaks(ylim)
  #hlines <- seq(from = ylim[1], to = ylim[2], length.out = 5)
  p <- ggplot2::ggplot(data = funnel.data, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::geom_hline(yintercept = hlines, linetype  = "solid", colour = "white")
  p <- p + ggplot2::geom_polygon(data = triangle.data, fill = "white")
  p <- p + ggplot2::geom_point(data = funnel.data, shape = shape, colour = "black", fill = fillcol)
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = refline, y = new_ylim[2], xend = refline, yend = ylim[1]),
                                 linetype  = "solid", colour = "black")
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = min(ci.left), y = new_ylim[2], xend = refline, yend = new_ylim[1]),
                                 linetype  = "dotted", colour = "black") +
    ggplot2::geom_segment(ggplot2::aes(x = refline, y = new_ylim[1], xend = max(ci.right), yend = new_ylim[2]),
                          linetype  = "dotted", colour = "black")
  p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(gettext("Standard Error"))
  #p <- p + ggplot2::ylim(new_ylim[1], new_ylim[2])
  p <- JASPgraphs::themeJasp(p)
  p <- p + ggplot2::theme(axis.line.x.bottom= ggplot2::element_line(),
                          axis.line.x.top   = ggplot2::element_blank(),
                          axis.line.y       = ggplot2::element_line(),
                          panel.background  = ggplot2::element_rect(fill = "lightgrey"),
                          panel.grid.major  = ggplot2::element_blank(),
                          panel.grid.minor  = ggplot2::element_blank(),
                          legend.position   = "none")
  p <- p + ggplot2::scale_y_reverse(limits = c(new_ylim[2], new_ylim[1]),
                                    breaks = hlines,
                                    labels = round(hlines, 3),
                                    expand = ggplot2::expand_scale(mult = c(0,0.05), add = 0)) +
    ggplot2::scale_x_continuous(limits = xlims,
                                breaks = JASPgraphs::getPrettyAxisBreaks(xlims))
  return(p)
}

.metaAnalysisRadialPlotFill <- function(rma.fit){
  x <- rma.fit
  #new.radial(x)
  if (!inherits(x, "rma"))
    stop("Argument 'x' must be an object of class \"rma\".")
  if (inherits(x, "robust.rma"))
    stop("Function not applicable to objects of class \"robust.rma\".")
  if (inherits(x, "rma.ls"))
    stop("Function not applicable to objects of class \"rma.ls\".")
  level <- x$level
  yi    <- x$yi
  vi    <- x$vi
  beta  <- x$beta
  ci.lb <- x$ci.lb
  ci.ub <- x$ci.ub
  tau2  <- 1/mean(1/x$tau2)
  atyis <- range(yi)
  level <- ifelse(level == 0, 1,
                  ifelse(level >= 1, (100 - level)/100,
                         ifelse(level > 0.5, 1 - level, level)))
  zcrit <- qnorm(level/2, lower.tail = FALSE)
  zi    <- yi/sqrt(vi + tau2)
  xi    <- 1/sqrt(vi + tau2)
  if (any(is.infinite(c(xi, zi))))
    stop(mstyle$stop("Setting 'xlim' and 'zlim' automatically not possible (must set axis limits manually)."))
  xlims    <- c(0, (1.3 * max(xi)))
  ci.xpos  <- xlims[2] + 0.12 * (xlims[2] - xlims[1])
  ya.xpos  <- xlims[2] + 0.14 * (xlims[2] - xlims[1])
  xaxismax <- xlims[2]
  zlims <- c(min(-5, 1.1 * min(zi), 1.1 * ci.lb * ci.xpos,
                 1.1 * min(atyis) * ya.xpos,  1.1 * min(yi) * ya.xpos,
                 -1.1 * zcrit + xaxismax * beta),
             max(5, 1.1 * max(zi), 1.1 * ci.ub * ci.xpos,
                 1.1 * max(atyis) * ya.xpos, 1.1 * max(yi) * ya.xpos,
                 1.1 * zcrit + xaxismax * beta))

  asp.rat <- (zlims[2] - zlims[1])/(xlims[2] - xlims[1])

  if(x$method == "FE") {
    xlabExpression <- bquote(x[i] == frac(1, sqrt(v[i])))
    ylabExpression <- bquote(z[i] == frac(y[i], sqrt(v[i])))
  } else {
    xlabExpression <- bquote(x[i] == frac(1, sqrt(v[i]+tau^2)))
    ylabExpression <- bquote(z[i] == frac(y[i], sqrt(v[i]+tau^2)))
  }

  .arc.line  <- function(zlims, yi, ya.xpos, asp.rat, length){
    atyis <- seq(min(yi), max(yi), length = length)
    len   <- ya.xpos
    xis   <- sqrt(len^2/(1 + (atyis/asp.rat)^2))
    zis   <- xis * atyis
    valid <- zis > zlims[1] & zis < zlims[2]
    xisv  <- xis[valid]
    zisv  <- zis[valid]
    dat   <- data.frame()
    for(j in 2:length(atyis)) {
      row <- data.frame(
        x    = xisv[j-1],
        y    = zisv[j-1],
        xend = xisv[j],
        yend = zisv[j]
      )
      dat <- rbind(dat, row)
    }
    return(ggplot2::geom_segment(data = dat,
                                 ggplot2::aes(x = x, y = y,
                                              xend = xend, yend = yend)))
  }
  .arc.ticks <- function(xlims, yi, ya.xpos, asp.rat){
    atyis <- seq(min(yi), max(yi), length = 7)
    len.l <- ya.xpos
    len.u <- ya.xpos + 0.015 * (xlims[2] - xlims[1])
    xis.l <- sqrt(len.l^2/(1 + (atyis/asp.rat)^2))
    zis.l <- xis.l * atyis
    xis.u <- sqrt(len.u^2/(1 + (atyis/asp.rat)^2))
    zis.u <- xis.u * atyis
    valid <- zis.l > zlims[1] & zis.u > zlims[1] & zis.l < zlims[2] & zis.u < zlims[2]
    dat   <- data.frame(x = xis.l[valid], xend = xis.u[valid],
                        y = zis.l[valid], yend = zis.u[valid])
    return(ggplot2::geom_segment(data = dat,
                                   ggplot2::aes(x = x, y = y,
                                                xend = xend, yend = yend),
                                   colour = "black"))
  }
  .arc.text  <- function(xlims, yi, ya.xpos, asp.rat){
    atyis <- seq(min(yi), max(yi), length = 7)
    len   <- ya.xpos + 0.02 * (xlims[2] - xlims[1])
    xis   <- sqrt(len^2/(1 + (atyis/asp.rat)^2))
    zis   <- xis * atyis
    valid <- zis > zlims[1] & zis < zlims[2]
    dat   <- data.frame(x = xis[valid], y = zis[valid],
                        label = as.character(round(atyis[valid], 2)))
    return(ggplot2::geom_text(data = dat, ggplot2::aes(x = x, y = y, label = label),
                         nudge_x = 0.1 * (xlims[2] - xlims[1]), hjust = 0))
  }
  .arc.int   <- function(xlims, zlims, ci.xpos, ci.lb, beta, ci.ub, asp.rat){
    atyis <- c(ci.lb, beta, ci.ub)
    len.l <- ci.xpos - 0.007 * (xlims[2] - xlims[1])
    len.u <- ci.xpos + 0.007 * (xlims[2] - xlims[1])

    xis.l <- sqrt(len.l^2/(1 + (atyis/asp.rat)^2))
    zis.l <- xis.l * atyis
    xis.u <- sqrt(len.u^2/(1 + (atyis/asp.rat)^2))
    zis.u <- xis.u * atyis

    valid <- zis.l > zlims[1] & zis.u > zlims[1] & zis.l < zlims[2] & zis.u < zlims[2]
    dat <- data.frame(xl = xis.l[valid],
                      zl = zis.l[valid],
                      xu = xis.u[valid],
                      zu = (xis.u * atyis)[valid])
    connectingLine <- data.frame(xl = mean(c(dat[1,1], dat[1,3])),
                                 zl = mean(c(dat[1,2], dat[1,4])),
                                 xu = mean(c(dat[3,1], dat[3,3])),
                                 zu = mean(c(dat[3,2], dat[3,4])))
    dat <- rbind(dat, connectingLine)
    return(ggplot2::geom_segment(data = dat, ggplot2::aes(x = xl, xend = xu,
                                                         y = zl, yend = zu)))
  }
  arc.line   <- .arc.line(zlims, yi, ya.xpos, asp.rat, length = 100)
  arc.ticks  <- .arc.ticks(xlims, yi, ya.xpos, asp.rat)
  arc.text   <- .arc.text(xlims, yi, ya.xpos, asp.rat)
  arc.int    <- .arc.int(xlims, zlims, ci.xpos, ci.lb, beta, ci.ub, asp.rat)

  len   <- ya.xpos + 0.02 * (xlims[2] - xlims[1])
  atyis <- seq(min(yi), max(yi), length = 7)
  x.margin.right <- 1.2 * max(arc.text$data$x)#max(sqrt(len^2/(1 + (atyis)/asp.rat)^2))

  radial.data    <- data.frame(x = xi, y = zi, slab = x$slab[x$not.na])
  rectangle.data <- data.frame(x = c(0, xaxismax, xaxismax, 0),
                               y = c(zcrit, zcrit + xaxismax * beta,
                                    -zcrit + xaxismax * beta, -zcrit))
  p <- ggplot2::ggplot(data = radial.data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_polygon(data = rectangle.data, fill = "lightgrey") +
    ggplot2::geom_point(data = radial.data, shape = 19, colour = "black") +
    ggplot2::ggtitle(gettext("Radial Plot"))
  p <- p + arc.line + arc.ticks + arc.int + arc.text
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = xlims[1], y = max(zcrit), xend = xlims[2],
                                     yend = max(zcrit + xaxismax * beta)),
                                 linetype = "dotted", colour = "black")
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = xlims[1], y = min(-zcrit), xend = xlims[2],
                                     yend = min(-zcrit +xaxismax * beta)),
                                 linetype  = "dotted", colour = "black")
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = xlims[2], yend = xaxismax * beta),
                                 linetype  = "solid", colour = "black")

  valsForBreaks <- c(-zcrit, zcrit, min(-zcrit +xaxismax * beta), max(zcrit + xaxismax * beta))
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(valsForBreaks)
  # do it again to get something symmetric around 0
  temp <- max(abs(yBreaks))
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(-temp, temp))
  # add the data from the right axis to stop ggplot2 from deleting these values
  yLimits <- range(JASPgraphs::getPrettyAxisBreaks(c(
    yBreaks, valsForBreaks, arc.text$data$y, arc.line$data$y, arc.line$data$yend)
  ))

  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(-temp, temp))
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, radial.data$x))
  xLimits <- c(0, x.margin.right)
  p <- p + ggplot2::xlab(xlabExpression) + ggplot2::ylab(ylabExpression) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits)

  p <- p + JASPgraphs::geom_rangeframe() + JASPgraphs::themeJaspRaw()

  # we want to show the axis titles in the middle of the breaks, not the middle of the limits (the default)
  # so we adjust the vjust and hjust accordingly
  vjustY <- (mean(yBreaks) - yLimits[1L]) / (yLimits[2L] - yLimits[1L])
  hjustX <- (mean(xBreaks) - xLimits[1L]) / (xLimits[2L] - xLimits[1L])

  p <- p + ggplot2::theme(axis.line.x       = ggplot2::element_blank(),
                          axis.line.y       = ggplot2::element_blank(),
                          axis.title.y      = ggplot2::element_text(size = 12, angle = 0, vjust = vjustY),
                          axis.title.x      = ggplot2::element_text(size = 12, hjust = hjustX),
                          panel.background  = ggplot2::element_blank(),
                          panel.grid.major  = ggplot2::element_blank(),
                          panel.grid.minor  = ggplot2::element_blank(),
                          legend.position   = "none")
  return(p)
}

.metaAnalysisQQPlotFill <- function(rma.fit){
  x      <- rma.fit
  if (x$k == 1)
    stop(gettext("Stopped because k = 1."))

  res    <- rstandard(x)
  not.na <- !is.na(res$z)
  zi     <- res$z[not.na]
  slab   <- res$slab[not.na]
  ord    <- order(zi)
  slab   <- slab[ord]

  sav    <- qqnorm(zi, plot.it = FALSE)
  pos.x  <- sav$x[ord]
  pos.y  <- sav$y[ord]

  reps    <- 1000
  level   <- x$level
  dat     <- matrix(rnorm(x$k * reps), nrow = x$k, ncol = reps)
  H       <- hatvalues(x, type = "matrix")
  ImH     <- diag(x$k) - H
  ei      <- ImH %*% dat
  ei      <- apply(ei, 2, sort)
  lb      <- apply(ei, 1, quantile, (level/2))
  ub      <- apply(ei, 1, quantile, 1 - (level/2))
  temp.lb <- qqnorm(lb, plot.it = FALSE)
  temp.lb <- supsmu(temp.lb$x, temp.lb$y)
  temp.ub <- qqnorm(ub, plot.it = FALSE)
  temp.ub <- supsmu(temp.ub$x, temp.ub$y)

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(pos.x)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(pos.y, temp.lb$y, temp.ub$y))
  xLimits <- range(xBreaks)
  yLimits <- range(yBreaks)

  qq.data <- data.frame(x = pos.x, y = pos.y, ci.lb = temp.lb$y, ci.ub = temp.ub$y)
  p <- ggplot2::ggplot(data = qq.data) +
    ggplot2::ggtitle(gettext("Normal Q-Q Plot")) +
    ggplot2::geom_ribbon(data = qq.data, ggplot2::aes(ymin = ci.lb, ymax = ci.ub, x = x),
                         fill = "gray", alpha = 0.5, stat = "identity") +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = 19, colour = "black") +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::scale_x_continuous(name = gettext("Theoretical Quantiles"), limits = xLimits, breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettext("Sample Quantiles"),      limits = yLimits, breaks = yBreaks)
  p <- JASPgraphs::themeJasp(p)
  return(p)
}

.metaAnalysisStandResidPlotFill <- function(rma.fit) {
  res        <- rstandard(rma.fit)
  zi         <- res$z[!is.na(res$z)]
  title      <- gettext("Standardized Residuals")
  study      <- seq_along(zi)
  stand.data <- data.frame(study = factor(study, levels = study), resid = zi)
  hlines     <- qnorm(c(0.025, 0.5, 0.975))
  linetypes  <- c("dotted", "dashed", "dotted")
  ylims      <- range(c(zi, hlines))

  p <- ggplot2::ggplot(data = stand.data, ggplot2::aes(x = study, y = resid, group = 1)) +
    ggplot2::geom_point(shape = 19, colour = "black") + ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = hlines, linetype  = linetypes, colour = "black") +
    ggplot2::xlab(gettext("Study")) + ggplot2::ylab(" ") + ggplot2::ggtitle(title)
  p <- p + ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(ylims),
                                limits = ylims) +
    ggplot2::theme(axis.line.x         = ggplot2::element_line(),
                   axis.line.y         = ggplot2::element_line(),
                   axis.ticks.x.bottom = ggplot2::element_line())
  p <- JASPgraphs::themeJasp(p, legend.position = "none")

  return(p)
}

.metaAnalysisFittedVsStandardPlotFill <- function(rma.fit){
  title    <- gettext("Fitted vs. Standardized Residuals")
  fit.data <- data.frame(x = fitted(rma.fit), y = rstandard(rma.fit)$z)
  hlines   <- qnorm(c(0.025, 0.5, 0.975))
  lty      <- c("dotted", "dashed", "dotted")
  xlims    <- range(fitted(rma.fit))
  ylims    <- range(rstandard(rma.fit)$z)
  p <- ggplot2::ggplot(data = fit.data) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = hlines, linetype = lty, colour = "black") +
    ggplot2::xlab(gettext("Fitted Value")) + ggplot2::ylab(gettext("Standardized Residual")) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(limits = xlims,
                                breaks = JASPgraphs::getPrettyAxisBreaks(xlims)) +
    ggplot2::scale_y_continuous(limits = ylims,
                                breaks = JASPgraphs::getPrettyAxisBreaks(ylims))
  p <- p + ggplot2::theme(axis.line.x         = ggplot2::element_line(),
                          axis.line.y         = ggplot2::element_line(),
                          axis.ticks.x.bottom = ggplot2::element_line()
  )
  p <- JASPgraphs::themeJasp(p, legend.position = "none")
  return(p)
}

# Extra functions
.metaAnalysisFormula <- function(options){
  if (length(options$modelTerms) > 0)
    formula.rhs <- formula(as.modelTerms(.v(options$modelTerms)))
  else
    formula.rhs <- NULL

  if (is.null(formula.rhs))
    formula.rhs <- ~1
  if (!options$includeConstant)
    formula.rhs <- update(formula.rhs, ~ . + 0)

  if (identical(formula.rhs, ~ 1 - 1))
    .quitAnalysis(gettext("The model should contain at least one predictor or an intercept."))

  return(formula.rhs)
}

# we need both of these functions, because these values are shown as columns, but also passed along as arguments to the meta analysis pkg
.metaAnalysisGetTranslatedMethod <- function(options){
  switch(options$method,
         `Fixed Effects`      = gettext("FE"),
         `Maximum Likelihood` = gettext("ML"),
         `Restricted ML`      = gettext("REML"),
         `DerSimonian-Laird`  = gettext("DL"),
         `Hedges`             = gettext("HE"),
         `Hunter-Schmidt`     = gettext("HS"),
         `Sidik-Jonkman`      = gettext("SJ"),
         `Empirical Bayes`    = gettext("EB"),
         `Paule-Mandel`       = gettext("PM"),
         gettext("REML"))
}

.metaAnalysisGetMethod <- function(options){
  switch(options$method,
         `Fixed Effects`      = "FE",
         `Maximum Likelihood` = "ML",
         `Restricted ML`      = "REML",
         `DerSimonian-Laird`  = "DL",
         `Hedges`             = "HE",
         `Hunter-Schmidt`     = "HS",
         `Sidik-Jonkman`      = "SJ",
         `Empirical Bayes`    = "EB",
         `Paule-Mandel`       = "PM",
         "REML")
}

.metaAnalysisConfidenceInterval <- function(options, table) {
  if(options$regressionCoefficientsConfidenceIntervals) {
    ci <- gettextf("%g%% Confidence Interval", 100 * options$regressionCoefficientsConfidenceIntervalsInterval)
    table$addColumnInfo(name = "lower", type = "number", title = "Lower", overtitle = ci)
    table$addColumnInfo(name = "upper", type = "number", title = "Upper", overtitle = ci)
  }
}

.metaAnalysisSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}

# Replaces "intrcpt" with "intercept" and concated "factorNamelevelName" with "factorName (levelName)"
.metaAnalysisMakePrettyCoeffNames <- function(coeffNames, dataset, concatFactorNames = NULL) {
  newNames <- coeffNames
  
  if (is.null(concatFactorNames))
    concatFactorNames <- .metaAnalysisMapConcatFactorNames(dataset)
  
  for (i in seq_along(coeffNames)) {
    coeffName <- coeffNames[i]
    
    if (coeffName == "intrcpt") {
      newNames[i] <- "intercept"
    } else if (!is.null(concatFactorNames)) {
      coeffNameEnc <- .v(coeffName)
      if (grepl(":", coeffNameEnc, fixed = TRUE)) { # it's an interaction term
        terms <- unlist(strsplit(coeffNameEnc, ":", fixed = TRUE))
        replaced <- .metaAnalysisMakePrettyCoeffNames(.unv(terms), dataset, concatFactorNames)
        newNames[i] <- paste(replaced, collapse = "\u2009\u273b\u2009")
      } else { # it's a regular term
        match <- which(concatFactorNames == coeffName)
        if (length(match) == 1)
          newNames[i] <- names(concatFactorNames)[match]
      }
    }
  }
  
  return(newNames)
}

# Creates a named character vector with values = "factorNameLevelName" and names = "factorName (levelName)"
.metaAnalysisMapConcatFactorNames <- function(dataset) {
  factorCols <- unlist(lapply(dataset, is.factor))
  if (length(factorCols) == 0)
    return(NULL)
  
  dataset <- dataset[factorCols]
  
  mapping <- levelsPerFactor <- lapply(dataset, levels)
  names(levelsPerFactor) <- .unv(names(levelsPerFactor))
  names(mapping) <- NULL
  
  for (i in seq_along(levelsPerFactor)) {
    values <- paste0(names(levelsPerFactor)[i], levelsPerFactor[[i]])
    values <- setNames(values, paste0(names(levelsPerFactor)[i], " (", levelsPerFactor[[i]], ")"))
    mapping[[i]] <- values
  }
  
  return(unlist(mapping))
}
