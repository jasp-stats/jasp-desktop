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

ConfirmatoryFactorAnalysis <- function(jaspResults, dataset, options, ...) {
  jaspResults[["optionslist"]] <- createJaspHtml(paste(capture.output(str(options)), collapse = "\n"),
                                                 class = "jasp-code", position = 7, title = "Options")
}


ConfirmatoryFactorAnalysis <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Preprocess options
  options <- .cfaPreprocessOptions(options)

  # Read dataset
  dataset <- .cfaReadData(dataset, options)

  # Error checking
  errors <- .cfaCheckErrors(dataset, options)

  # Main table / model
  cfaResult <- .cfaComputeResults(jaspResults, dataset, options, errors)

  # Output tables
  .cfaContainerMain(   jaspResults, options, cfaResult) # Main table container
  .cfaTableMain(       jaspResults, options, cfaResult) # Main table with fit info
  .cfaTableFitMeasures(jaspResults, options, cfaResult) # Additional fit indices
  .cfaTableRsquared(   jaspResults, options, cfaResult) # R-squared of indicators
  .cfaTableParEst(     jaspResults, options, cfaResult) # Parameter estimates tables
  .cfaTableModIndices( jaspResults, options, cfaResult) # Modification Indices
  .cfaTableImpliedCov( jaspResults, options, cfaResult) # Implied Covariance matrix
  .cfaTableResCov(     jaspResults, options, cfaResult) # Residual Covariance Matrix

  # Output plots
  .cfaInitPlots( jaspResults, options, cfaResult)      # Create plots container
  .cfaPlotPath(  jaspResults, options, cfaResult)      # Path plot(s) showing model
  .cfaPlotMisfit(jaspResults, options, cfaResult)      # Plot residual correlations

  # Output model syntax
  .cfaSyntax(jaspResults, options, dataset, cfaResult) # Output model syntax to user

  return()
}

# Preprocessing functions ----
.cfaReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  vars <- unique(unlist(lapply(options$factors, function(x) x$indicators)))
  if (options$groupvar == "") {
    return(.readDataSetToEnd(columns = vars))
  } else {
    return(.readDataSetToEnd(columns = vars, columns.as.factor = options$groupvar))
  }
}

.cfaPreprocessOptions <- function(options) {
  # Remove empty factors
  facwoindicators <- sapply(options$factors, function(f) length(f$indicators)) == 0
  options$factors <- options$factors[!facwoindicators]

  # sofacwoindicators   <- sapply(options$secondOrder, function(f) length(f$indicators)) == 0
  # options$secondOrder <- options$secondOrder[!sofacwoindicators]
  if (length(options$secondOrder) > 0) {
    options$secondOrder <- list(list(indicators = options$secondOrder, name = "SecondOrder", title = "Second-Order"))
  }
  return(options)
}

.cfaCheckErrors <- function(dataset, options) {
  # TODO (vankesteren) content error checks, e.g., posdef covmat

  # Number of variables in the factors
  nvars <- lapply(options$factors, function(x) length(x$indicators))
  if (all(nvars == 0)) return("No variables")
  if (any(nvars == 1)) {
    JASP:::.quitAnalysis("The model could not be estimated. Ensure that factors have at least 2 observed variables.")
  }


  vars <- unique(unlist(lapply(options$factors, function(x) x$indicators)))

  if (options$groupvar == "") {

    JASP:::.hasErrors(dataset[, .v(vars)], perform = "run", type = 'varCovData', exitAnalysisIfErrors = TRUE,
               varCovData.corFun = stats::cov)

  } else {

    JASP:::.hasErrors(dataset, perform, type = "factorLevels", factorLevels.target = options$groupvar,
               factorLevels.amount = '< 2', exitAnalysisIfErrors = TRUE)

    for (group in levels(dataset[[.v(options$groupvar)]])) {

      idx <- dataset[[.v(options$groupvar)]] == group
      JASP:::.hasErrors(dataset[idx, .v(vars)], perform = "run", type = 'varCovData', exitAnalysisIfErrors = TRUE,
                 varCovData.corFun = stats::cov)

    }
  }

  return(NULL)
}

.translateFactorNames <- function(factor_name, options, back = FALSE) {
  fac_names    <- vapply(options$factors,     function(x) x$name, "name")
  sofac_names  <- vapply(options$secondOrder, function(x) x$name, "name")
  if (length(sofac_names) > 0) sofac_names <- paste0("so", sofac_names)
  fac_titles   <- vapply(options$factors,     function(x) x$title, "title")
  sofac_titles <- vapply(options$secondOrder, function(x) x$title, "title")

  fnames  <- c(fac_names, sofac_names)
  ftitles <- c(fac_titles, sofac_titles)

  if (back) {
    idx <- vapply(factor_name, function(n) which(ftitles == n), 0L, USE.NAMES = FALSE)
    return(fnames[idx])
  } else {
    idx <- vapply(factor_name, function(n) which(fnames == n), 0L, USE.NAMES = FALSE)
    return(ftitles[idx])
  }
}


# Results functions ----
.cfaComputeResults <- function(jaspResults, dataset, options, errors) {
  if (!is.null(errors) && errors == "No variables") return()

  if (!is.null(jaspResults[["stateCFAResult"]])) return(jaspResults[["stateCFAResult"]]$object)

  cfaResult <- list()

  cfaResult[["spec"]] <- .cfaCalcSpecs(dataset, options)


  # Recalculate the model
  mod <- .optionsToCFAMod(options, dataset, cfaResult)
  geq <- .CFAInvariance(options)
  if (options$groupvar == "") grp <- NULL else grp <- .v(options$groupvar)

  cfaResult[["lav"]] <- try(lavaan::lavaan(
    model           = mod,
    data            = dataset,
    group           = grp,
    group.equal     = geq,
    meanstructure   = options$includemeanstructure,
    se              = cfaResult[["spec"]]$se,
    std.lv          = options$identify == "factor",
    auto.fix.first  = options$identify == "marker",
    orthogonal      = options$uncorrelatedFactors,
    int.ov.free     = TRUE,
    int.lv.free     = FALSE,
    auto.fix.single = TRUE,
    auto.var        = TRUE,
    auto.cov.lv.x   = TRUE,
    auto.th         = TRUE,
    auto.delta      = TRUE,
    auto.cov.y      = TRUE,
    mimic           = options$mimic,
    estimator       = options$estimator
  ))


  # Quit analysis on error
  if (inherits(cfaResult[["lav"]], "try-error")) {
    JASP:::.quitAnalysis(paste("The model could not be estimated. Error message:\n\n",
                         attr(cfaResult[["lav"]], "condition")$message))
  }

  admissible <- .withWarnings(lavaan:::lav_object_post_check(cfaResult[["lav"]]))
  print(admissible)
  if (!admissible$value) {
    JASP:::.quitAnalysis(paste("The model is not admissible:", admissible$warnings[[1]]$message))
  }

  if (!cfaResult[["lav"]]@optim$converged) {
    JASP:::.quitAnalysis("The model could not be estimated due to nonconvergence.")
  }

  if (cfaResult[["lav"]]@test[[1]]$df < 0) {
    JASP:::.quitAnalysis("The model could not be estimated: No degrees of freedom left.")
  }


  # Bootstrapping with interruptible progress bar
  if (cfaResult[["spec"]]$bootstrap) {
    startProgressbar(options$bootstrapNumber)
    
    boot_1      <- lavaan::bootstrapLavaan(cfaResult[["lav"]], R = 1)
    bootres     <- matrix(0, options$bootstrapNumber, length(boot_1))
    bootres[1,] <- boot_1
    for (i in 2:options$bootstrapNumber) {
      bootres[i,] <- lavaan::bootstrapLavaan(cfaResult[["lav"]], 1)
      progressbarTick()
    }
    
    cfaResult[["lav"]]@boot       <- list(coef = bootres)
    cfaResult[["lav"]]@Options$se <- "bootstrap"
  }

  # Save cfaResult as state so it's available even when opts don't change
  jaspResults[["stateCFAResult"]] <- createJaspState(cfaResult)
  jaspResults[["stateCFAResult"]]$dependOn(c(
    "factors", "secondOrder", "rescov", "includemeanstructure", "identify",
    "uncorrelatedFactors", "mimic", "estimator", "se", "bootstrapNumber",
    "groupvar", "invariance"
  ))

  return(cfaResult)
}

.withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

.cfaCalcSpecs <- function(dataset, options) {
  spec <- list()
  spec$variables <- unlist(lapply(options$factors, function(x) x$indicators))
  spec$latents   <- vapply(options$factors,        function(x) x$name, "names")
  spec$soLatents <- vapply(options$secondOrder,    function(x) x$name, "names")
  if (length(spec$soLatents) > 0) spec$soLatents <- paste0("so", spec$soLatents)
  if (options$se == "bootstrap") {
    spec$se <- "standard"
    spec$bootstrap <- TRUE
  } else {
    spec$se <- options$se
    spec$bootstrap <- FALSE
  }
  return(spec)
}

.optionsToCFAMod <- function(options, dataset, cfaResult, base64 = TRUE) {
  gv <- .v(options$groupvar)
  if (!base64) .v <- identity

  vars    <- options$factors
  latents <- cfaResult[["spec"]]$latents
  labels  <- list()

  fo <- "# Factors"
  for (i in 1:length(vars)) {
    pre <- paste0("\n", latents[i], " =~ ")
    len <- length(vars[[i]]$indicators)
    labelledvars <- labels[[i]] <- character(len)
    for (j in 1:len) {
      labels[[i]][j]  <- paste0("lambda", i, j)
      labelledvars[j] <- paste0("lambda", i, j, "*", .v(vars[[i]]$indicators[j]))
    }
    fo <- paste0(fo, pre, paste0(labelledvars, collapse = " + "))
  }


  if (length(options$secondOrder) > 0) {
    facs    <- options$secondOrder
    soFacs  <- cfaResult[["spec"]]$soLatents
    lenvars <- length(vars)

    so <- "# Second-order factors"
    for (i in 1:length(facs)) {
      pre <- paste0("\n", soFacs[i], " =~ ")
      len <- length(facs[[i]]$indicators)
      labelledfacs <- labels[[i + lenvars]] <- character(len)
      for (j in 1:len) {
        labels[[i + lenvars]][j] <- paste0("gamma", i, j)
        labelledfacs[j] <- paste0("gamma", i, j, "*", facs[[i]]$indicators[j])
      }
      so <- paste0(so, pre, paste0(labelledfacs, collapse = " + "))
    }
  } else {
    so <- NULL
  }


  if (length(options$rescov) > 0) {
    rc <- "# Residual Correlations"
    for (rcv in options$rescov) {
      rc <- paste0(rc, "\n", .v(rcv[1]), " ~~ ", .v(rcv[2]))
    }
  } else {
    rc <- NULL
  }

  if (options$includemeanstructure && options$groupvar != "") {
    lm <- "# Latent means"
    lvs <- c(cfaResult[["spec"]]$latents, cfaResult[["spec"]]$soLatents)
    for (i in seq_along(lvs)) {
      lm <- paste0(lm, '\n', lvs[i], " ~ c(0,",
                  paste(rep(NA, length(unique(dataset[[gv]])) - 1), collapse = ","),
                  ")*1")
    }
  } else {
    lm <- NULL
  }

  if (options$identify == "effects") {
    ef <- "# Effects coding restrictions"
    for (i in 1:length(labels)) {
      restr <- paste0(labels[[i]][1], " == ",
                      paste(c(length(labels[[i]]), labels[[i]][-1]),
                            collapse = " - "))
      ef <- paste0(ef, "\n", restr)
    }
  } else {
    ef <- NULL
  }

  return(paste0(c(fo, so, rc, lm, ef), collapse = "\n\n"))
}

.CFAInvariance <- function(options) {
  if (options$invariance == "") return("")
  switch(options$invariance,
         "configural" = return(""),
         "metric"     = return("loadings"),
         "scalar"     = return(c("loadings", "intercepts")),
         "strict"     = return(c("loadings", "intercepts", "residuals", "residual.covariances"))
  )
}

# Output functions ----
.cfaContainerMain <- function(jaspResults, options, cfaResult) {
  # Create main container
  jaspResults[["maincontainer"]] <- createJaspContainer("Model fit", position = 1)
  jaspResults[["maincontainer"]]$dependOn(c(
    "factors", "secondOrder", "rescov", "includemeanstructure", "identify",
    "uncorrelatedFactors", "mimic", "estimator", "se", "bootstrapNumber",
    "groupvar", "invariance"
  ))
}

.cfaTableMain <- function(jaspResults, options, cfaResult) {
  # Main table
  # prepare table
  jaspResults[["maincontainer"]][["cfatab"]] <- maintab <- createJaspTable("Chi-square test")
  maintab$dependOn(optionsFromObject = jaspResults[["maincontainer"]])

  maintab$addColumnInfo(name = "mod",    title = "Model",        type = "string")
  maintab$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number", format = "dp:3")
  maintab$addColumnInfo(name = "df",     title = "df",           type = "integer")
  maintab$addColumnInfo(name = "pvalue", title = "p",            type = "number", format = "dp:3;p:.001")

  # add data
  if (is.null(cfaResult)) {
    maintab[["mod"]]    <- c("Baseline model", "Factor model")
    maintab[["chisq"]]  <- c(      "."       ,      "."      )
    maintab[["df"]]     <- c(      "."       ,      "."      )
    maintab[["pvalue"]] <- c(      "."       ,      "."      )
  } else {
    fm <- lavaan::fitMeasures(cfaResult[["lav"]])
    maintab[["mod"]]    <- c("Baseline model", "Factor model")
    maintab[["chisq"]]  <- fm[c("baseline.chisq", "chisq")]
    maintab[["df"]]     <- fm[c("baseline.df", "df")]
    maintab[["pvalue"]] <- c(NA, fm["pvalue"])
  }
}

.cfaTableRsquared <- function(jaspResults, options, cfaResult) {
  if (!options$rsquared || !is.null(jaspResults[["maincontainer"]][["rsquared"]])) return()
  
  jaspResults[["maincontainer"]][["rsquared"]] <- tabr2 <- createJaspTable("R-Squared")
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  tabr2$setExpectedSize(rows = 1, cols = 1)
  tabr2$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors",
                   "mimic", "estimator", "se", "bootstrapNumber", "groupvar", "invariance", "rsquared"))
  if (is.null(cfaResult)) return()
  
  r2res <- lavaan::inspect(cfaResult[["lav"]], "r2")
  
  if (options$groupvar != "") {
    # add columns with Rsq overtitle
    tabr2[["__var__"]] <- .unv(names(r2res[[1]]))
    lvls <- names(r2res)
    for (lvl in lvls) {
      tabr2$addColumnInfo(name = lvl, title = lvl, overtitle = "R\u00B2", type = "number", format = "sf:4;dp:3")
      tabr2[[lvl]] <- r2res[[lvl]]
    }
  } else {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
    tabr2[["__var__"]] <- .unv(names(r2res))
    tabr2[["rsq"]]     <- r2res
  }
}

.cfaTableFitMeasures <- function(jaspResults, options, cfaResult) {
  if (!options$additionalfits || !is.null(jaspResults[["maincontainer"]][["fits"]])) return()
  jaspResults[["maincontainer"]][["fits"]] <- fitms <- createJaspContainer("Additional fit measures")
  fitms$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors",
                   "mimic", "estimator", "se", "bootstrapNumber", "groupvar", "invariance", "additionalfits"))
  
  # Fit indices
  fitms[["indices"]] <- fitin <- createJaspTable("Fit indices")
  fitin$addColumnInfo(name = "index", title = "Index", type = "string")
  fitin$addColumnInfo(name = "value", title = "Value", type = "number", format = "sf:4;dp:3")
  fitin$setExpectedSize(rows = 1, cols = 2)
  
  # information criteria
  fitms[["incrits"]] <- fitic <- createJaspTable("Information criteria")
  fitic$addColumnInfo(name = "index", title = "",      type = "string")
  fitic$addColumnInfo(name = "value", title = "Value", type = "number", format = "sf:4;dp:3")
  fitic$setExpectedSize(rows = 1, cols = 2)
  
  # other fit measures
  fitms[["others"]] <- fitot <- createJaspTable("Other fit measures")
  fitot$addColumnInfo(name = "index", title = "Metric", type = "string")
  fitot$addColumnInfo(name = "value", title = "Value",  type = "number", format = "sf:4;dp:3")
  fitot$setExpectedSize(rows = 1, cols = 2)
  
  if (is.null(cfaResult)) return() 
  
  # actually compute the fit measures
  fm <- lavaan::fitmeasures(cfaResult[["lav"]])
  
  # Fit indices
  fitin[["index"]] <- c(
    "Comparative Fit Index (CFI)",
    "Tucker-Lewis Index (TLI)",
    "Bentler-Bonett Non-normed Fit Index (NNFI)",
    "Bentler-Bonett Normed Fit Index (NFI)",
    "Parsimony Normed Fit Index (PNFI)",
    "Bollen's Relative Fit Index (RFI)",
    "Bollen's Incremental Fit Index (IFI)",
    "Relative Noncentrality Index (RNI)"
  )
  fitin[["value"]] <- fm[c("cfi", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
  
  # information criteria
  fitic[["index"]] <- c(
    "Log-likelihood",
    "Number of free parameters", 
    "Akaike (AIC)",
    "Bayesian (BIC)", 
    "Sample-size adjusted Bayesian (SSABIC)"
  )
  fitic[["value"]] <- fm[c("logl", "npar", "aic", "bic", "bic2")]
  
  # other fitmeasures
  fitot[["index"]] <- c(
    "Root mean square error of approximation (RMSEA)", 
    "RMSEA 90% CI lower bound",
    "RMSEA 90% CI upper bound",
    "RMSEA p-value", 
    "Standardized root mean square residual (SRMR)", 
    "Hoelter's critical N (\u03B1 = .05)",
    "Hoelter's critical N (\u03B1 = .01)",
    "Goodness of fit index (GFI)", 
    "McDonald fit index (MFI)",
    "Expected cross validation index (ECVI)"
  )
  fitot[["value"]] <- fm[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", 
                           "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
  
  return()
}

.cfaTableParEst <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !is.null(jaspResults[["estimates"]])) return()

  jaspResults[["estimates"]] <- ests <- createJaspContainer("Parameter estimates", position = 2)
  ests$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors",
                  "mimic", "estimator", "se", "bootstrapNumber", "groupvar", "invariance", "std", "ciWidth"))


  pe <- lavaan::parameterEstimates(cfaResult[["lav"]], standardized = TRUE, remove.eq = FALSE, remove.system.eq = TRUE,
                                   remove.ineq = FALSE, remove.def = FALSE, add.attributes = TRUE, boot.ci.type = "perc",
                                   level = options$ciWidth)

  if (options$groupvar != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (i in 1:max(pe$group)) {
      pei <- pe[pe$group == i, ]
      ests[[groupLabs[i]]] <- createJaspContainer(groupLabs[i])
      ests[[groupLabs[i]]]$dependOn(optionsFromObject = ests)
      .cfaParEstToTablesHelper(pei, options, cfaResult[["spec"]], ests[[groupLabs[i]]])
    }
  } else {
    .cfaParEstToTablesHelper(pe, options, cfaResult[["spec"]], ests)
  }
}

.cfaParEstToTablesHelper <- function(pei, options, spec, jrobject) {
  pei <- as.data.frame(pei)
  facNames <- c(spec$latents, spec$soLatents)

  colSel <- c("lhs", "rhs", "label", "est", "se", "z", "pvalue", "ci.lower", "ci.upper")
  if (options$std != "none") colSel <- c(colSel, paste0("std.", options$std))

  # First-order factor loadings ----
  # Set up table
  jrobject[["fl1"]] <- fl1 <- createJaspTable(title = "Factor loadings")

  fl1$addColumnInfo(name = "lhs",   title = "Factor",    type = "string", combine = TRUE)
  fl1$addColumnInfo(name = "rhs",   title = "Indicator", type = "string")
  fl1$addColumnInfo(name = "label", title = "Symbol",    type = "string")

  fl1$addColumnInfo(name = "est",    title  = "Estimate",   type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "se",     title  = "Std. Error", type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "z",      title  = "z-value",    type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "pvalue", title  = "p",          type = "number", format = "dp:3;p:.001")

  fl1$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                    overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
  fl1$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                    overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

  if (options$std != "none")
    fl1$addColumnInfo(name = paste0("std.", options$std), title = paste0("Std. Est. (", options$std, ")"),
                      type = "number", format = "sf:4;dp:3")

  # add data
  fl1dat <- pei[pei$op == "=~" & !pei$rhs %in% facNames, colSel]
  fl1dat$label <- gsub("lambda", "\u03bb", fl1dat$label)
  fl1dat$lhs <- .translateFactorNames(fl1dat$lhs, options)
  fl1dat$rhs <- .unv(fl1dat$rhs)
  fl1$setData(fl1dat)
  fl1$dependOn(optionsFromObject = jrobject)

  # Second-order factor loadings ----
  if (length(options$secondOrder) > 0) {
    # Set up table
    jrobject[["fl2"]] <- fl2 <- createJaspTable(title = "Second-order factor loadings")

    fl2$addColumnInfo(name = "lhs",   title = "Factor",    type = "string", combine = TRUE)
    fl2$addColumnInfo(name = "rhs",   title = "Indicator", type = "string")
    fl2$addColumnInfo(name = "label", title = "Symbol",    type = "string")

    fl2$addColumnInfo(name = "est",    title  = "Estimate",   type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "se",     title  = "Std. Error", type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "z",      title  = "z-value",    type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "pvalue", title  = "p",          type = "number", format = "dp:3;p:.001")

    fl2$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                      overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    fl2$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                      overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

    if (options$std != "none")
      fl2$addColumnInfo(name = paste0("std.", options$std), title = paste0("Std. Est. (", options$std, ")"),
                        type = "number", format = "sf:4;dp:3")

    # add data
    fl2dat <- pei[pei$op == "=~" & pei$rhs %in% facNames, colSel]
    fl2dat$label <- gsub("gamma", "\u03b3", fl2dat$label)
    fl2dat$lhs   <- .translateFactorNames(fl2dat$lhs, options)
    fl2dat$rhs   <- .translateFactorNames(fl2dat$rhs, options)
    fl2$setData(fl2dat)
    fl2$dependOn(optionsFromObject = jrobject)
  }


  # Factor variances ----
  # Set up table
  jrobject[["fv"]] <- fv <- createJaspTable("Factor variances")

  fv$addColumnInfo(name = "lhs",    title = "Factor",     type = "string", combine = TRUE)
  fv$addColumnInfo(name = "est",    title = "Estimate",   type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "se",     title = "Std. Error", type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "z",      title = "z-value",    type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "pvalue", title = "p",          type = "number", format  = "dp:3;p:.001")

  fv$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                   overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
  fv$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                   overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

  if (options$std != "none")
    fv$addColumnInfo(name = paste0("std.", options$std), title = paste0("Std. Est. (", options$std, ")"),
                     type = "number", format = "sf:4;dp:3")

  # Add data
  fvdat     <- pei[pei$op == "~~" & pei$lhs %in% facNames & pei$lhs == pei$rhs, colSel[-c(2, 3)]]
  fvdat$lhs <- .translateFactorNames(fvdat$lhs, options)
  fv$setData(fvdat)
  fv$dependOn(optionsFromObject = jrobject)

  # Factor covariances ----
  hasMultipleFactorsAtTopLevel <-
    length(options$secondOrder) > 1 || (length(options$secondOrder) == 0 & length(options$factors) > 1)

  if (!options$uncorrelatedFactors & hasMultipleFactorsAtTopLevel) {
    jrobject[["fc"]] <- fc <- createJaspTable("Factor Covariances")

    fc$addColumnInfo(name = "lhs",    title  = "",          type = "string")
    fc$addColumnInfo(name = "op",     title  = "",          type = "string")
    fc$addColumnInfo(name = "rhs",    title  = "",          type = "string")
    fc$addColumnInfo(name = "est",    title = "Estimate",   type = "number", format  = "sf:4;dp:3")
    fc$addColumnInfo(name = "se",     title = "Std. Error", type = "number", format  = "sf:4;dp:3")
    fc$addColumnInfo(name = "z",      title = "z-value",    type = "number", format  = "sf:4;dp:3")
    fc$addColumnInfo(name = "pvalue", title = "p",          type = "number", format  = "dp:3;p:.001")

    fc$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                     overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    fc$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                     overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

    if (options$std != "none")
      fc$addColumnInfo(name   = paste0("std.", options$std),
                       title  = paste0("Std. Est. (", options$std, ")"),
                       type   = "number",
                       format = "sf:4;dp:3")

    fcdat <- pei[pei$op == "~~" & pei$lhs %in% facNames & pei$lhs != pei$rhs, colSel[-c(3)]]
    fcdat$lhs <- .translateFactorNames(fcdat$lhs, options)
    fcdat$rhs <- .translateFactorNames(fcdat$rhs, options)
    fcdat$op  <- rep("\u2194", nrow(fcdat))

    fc$setData(fcdat)
    fc$dependOn(optionsFromObject = jrobject)
  }

  # Residual variances ----
  # Set up table
  jrobject[["rv"]] <- rv <- createJaspTable("Residual variances")

  rv$addColumnInfo(name = "lhs",    title = "Indicator",  type = "string", combine = TRUE)
  rv$addColumnInfo(name = "est",    title = "Estimate",   type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "se",     title = "Std. Error", type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "z",      title = "z-value",    type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "pvalue", title = "p",          type = "number", format  = "dp:3;p:.001")

  rv$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                   overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
  rv$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                   overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

  if (options$std != "none")
    rv$addColumnInfo(name   = paste0("std.", options$std),
                     title  = paste0("Std. Est. (", options$std, ")"),
                     type   = "number",
                     format = "sf:4;dp:3")

  # add data
  rvdat <- pei[pei$op == "~~" & !pei$lhs %in% facNames & pei$lhs == pei$rhs, colSel[-c(2, 3)]]
  rvdat$lhs <- .unv(rvdat$lhs)
  rv$setData(rvdat)
  rv$dependOn(optionsFromObject = jrobject)

  # Residual covariances ----
  if (length(options$rescov) > 0) {
    rc <- pei[pei$op == "~~" & !pei$lhs %in% facNames & pei$lhs != pei$rhs, colSel[-3]]
    rescov <- createJaspTable("Residual covariances")
    rescov$dependOn(optionsFromObject = jrobject)

    rescov$addColumnInfo(name = "lhs",    title  = "",          type = "string")
    rescov$addColumnInfo(name = "op",     title  = "",          type = "string")
    rescov$addColumnInfo(name = "rhs",    title  = "",          type = "string")
    rescov$addColumnInfo(name = "est",    title = "Estimate",   type = "number", format  = "sf:4;dp:3")
    rescov$addColumnInfo(name = "se",     title = "Std. Error", type = "number", format  = "sf:4;dp:3")
    rescov$addColumnInfo(name = "z",      title = "z-value",    type = "number", format  = "sf:4;dp:3")
    rescov$addColumnInfo(name = "pvalue", title = "p",          type = "number", format  = "dp:3;p:.001")

    rescov$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    rescov$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

    rescov[["lhs"]]      <- .unv(rc$lhs)
    rescov[["op"]]       <- rep("\u2194", nrow(rc))
    rescov[["rhs"]]      <- .unv(rc$rhs)
    rescov[["est"]]      <- rc$est
    rescov[["se"]]       <- rc$se
    rescov[["z"]]        <- rc$z
    rescov[["pvalue"]]   <- rc$pvalue
    rescov[["ci.lower"]] <- rc$ci.lower
    rescov[["ci.upper"]] <- rc$ci.upper


    jrobject[["Residual Covariances"]] <- rescov
  }

  # Intercepts ----
  if (options$includemeanstructure) {

    if (options$groupvar != "") {
      jrobject[["Factor Intercepts"]] <- fi <- createJaspTable(title = "Factor Intercepts")

      fi$addColumnInfo(name = "lhs",    title = "Factor",     type = "string", combine = TRUE)
      fi$addColumnInfo(name = "est",    title = "Estimate",   type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "se",     title = "Std. Error", type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "z",      title = "z-value",    type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "pvalue", title = "p",          type = "number", format = "dp:3;p:.001")

      fi$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                        overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
      fi$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                        overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

      if (options$std != "none")
        fi$addColumnInfo(name = paste0("std.", options$std), title = paste0("Std. Est. (", options$std, ")"),
                          type = "number", format = "sf:4;dp:3")

      # add data
      fidat <- pei[pei$op == "~1" & pei$lhs %in% facNames, colSel[-c(2, 3)]]
      fidat$lhs <- .translateFactorNames(fidat$lhs, options)
      fi$setData(fidat)
      fi$dependOn(optionsFromObject = jrobject)
    }

    # Manifest variable intercepts
    jrobject[["Intercepts"]] <- vi <- createJaspTable(title = "Intercepts")

    vi$addColumnInfo(name = "lhs",    title  = "Indicator",  type = "string", combine = TRUE)
    vi$addColumnInfo(name = "est",    title  = "Estimate",   type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "se",     title  = "Std. Error", type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "z",      title  = "z-value",    type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "pvalue", title  = "p",          type = "number", format = "dp:3;p:.001")

    vi$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                     overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    vi$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                     overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))

    if (options$std != "none")
      vi$addColumnInfo(name = paste0("std.", options$std), title = paste0("Std. Est. (", options$std, ")"),
                        type = "number", format = "sf:4;dp:3")

    # add data
    vidat <- pei[pei$op == "~1" & !pei$lhs %in% facNames, colSel[-c(2, 3)]]
    vidat$lhs <- .unv(vidat$lhs)
    vi$setData(vidat)
    vi$dependOn(optionsFromObject = jrobject)
  }
}

.cfaTableModIndices <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$modIndices || !is.null(jaspResults[["modind"]])) return()

  mi <- lavaan::modindices(cfaResult[["lav"]])
  jaspResults[["modind"]] <- mic <- createJaspContainer("Modification Indices", position = 5)
  mic$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors",
                        "mimic", "estimator", "se", "bootstrapNumber", "groupvar", "invariance", "modIndices",
                        "miCutoff"))

  if (options$groupvar != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (i in 1:length(groupLabs)) {
      mic[[groupLabs[i]]] <- createJaspContainer(groupLabs[i])
      mic[[groupLabs[i]]]$dependOn(optionsFromObject = mic)
      .cfaMItoTablesHelper(mi[mi$group == i, ], options, mic[[groupLabs[i]]], cfaResult)
    }
  } else {
    .cfaMItoTablesHelper(mi, options, mic, cfaResult)
  }
}

.cfaMItoTablesHelper <- function(mii, options, jrobject, cfaResult) {

  # cross loadings (first order)
  foc <- mii[mii$op == "=~" & mii$lhs %in% cfaResult[["spec"]]$latents, c("lhs", "rhs", "mi", "epc")]
  foc <- foc[foc$mi > options$miCutoff, ]

  if (nrow(foc) > 0) {
    foc <- as.data.frame(foc)
    foc <- foc[order(foc$mi, decreasing = TRUE), ]
    jrobject[["Cross-Loadings"]] <- focro <- createJaspTable("Cross-loadings")
    focro$dependOn(optionsFromObject = jrobject)

    focro$addColumnInfo(name = "lhs", title = "",          type = "string")
    focro$addColumnInfo(name = "op",  title = "",          type = "string")
    focro$addColumnInfo(name = "rhs", title = "",          type = "string")
    focro$addColumnInfo(name = "mi",  title = "Mod. Ind.", type = "number", format = "sf:4;dp:3")
    focro$addColumnInfo(name = "epc", title  = "EPC",      type = "number", format = "sf:4;dp:3")

    focro[["lhs"]] <- .translateFactorNames(foc$lhs, options)
    focro[["op"]]  <- rep("\u2192", nrow(foc))
    focro[["rhs"]] <- .unv(foc$rhs)
    focro[["mi"]]  <- foc$mi
    focro[["epc"]] <- foc$epc
  }


  # cross loadings (second order)
  if (length(options$secondOrder) > 1) {
    soc <- mii[mii$op == "=~" & mii$lhs %in% options$soLatents & mii$rhs %in% cfaResult[["spec"]]$latents,
               c("lhs", "rhs", "mi", "epc")]
    soc <- soc[soc$mi > options$miCutoff, ]

    if (nrow(soc) > 0) {
      soc <- as.data.frame(soc)
      soc <- soc[order(soc$mi, decreasing = TRUE), ]
      jrobject[["Second-Order Cross-Loadings"]] <- socro <- createJaspTable("Second-order cross-loadings")
      socro$dependOn(optionsFromObject = jrobject)

      socro$addColumnInfo(name = "lhs", title  = "",          type = "string")
      socro$addColumnInfo(name = "op",  title  = "",          type = "string")
      socro$addColumnInfo(name = "rhs", title  = "",          type = "string")
      socro$addColumnInfo(name = "mi",  title  = "Mod. Ind.", type = "number", format = "sf:4;dp:3")
      socro$addColumnInfo(name = "epc", title  = "EPC",       type = "number", format = "sf:4;dp:3")


      socro[["lhs"]] <- soc$lhs
      socro[["op"]]  <- rep("\u2192", nrow(soc))
      socro[["rhs"]] <- soc$rhs
      socro[["mi"]]  <- soc$mi
      socro[["epc"]] <- soc$epc
    }
  }


  # residual covariances
  rec <- mii[mii$op == "~~" & !mii$lhs %in% c(cfaResult[["spec"]]$latents, cfaResult[["spec"]]$soLatents),
             c("lhs", "rhs", "mi", "epc")]
  rec <- rec[rec$mi > options$miCutoff, ]

  if (nrow(rec) > 0) {
    jrobject[["Residual Covariances"]] <- rescov <- createJaspTable("Residual covariances")
    rescov$dependOn(optionsFromObject = jrobject)

    rescov$addColumnInfo(name = "lhs", title  = "", type = "string")
    rescov$addColumnInfo(name = "op",  title  = "", type = "string")
    rescov$addColumnInfo(name = "rhs", title  = "", type = "string")
    rescov$addColumnInfo(name = "mi",  title  = "Mod. Ind.", type = "number", format = "sf:4;dp:3")
    rescov$addColumnInfo(name = "epc", title  = "EPC",       type = "number", format = "sf:4;dp:3")

    rec <- as.data.frame(rec)
    rec <- rec[order(rec$mi, decreasing = TRUE), ]
    rescov[["lhs"]] <- .unv(rec$lhs)
    rescov[["op"]]  <- rep("\u2194", nrow(rec))
    rescov[["rhs"]] <- .unv(rec$rhs)
    rescov[["mi"]]  <- rec$mi
    rescov[["epc"]] <- rec$epc
  }
}

.cfaTableImpliedCov <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$impliedCov || !is.null(jaspResults[["impcov"]])) return()

  fv <- lavaan::fitted.values(cfaResult[["lav"]])

  if (options$groupvar != "") {
    jaspResults[["impcov"]] <- icc <- createJaspContainer("Implied covariance matrices", position = 3)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (l in groupLabs) {
      ic <- fv[[l]]$cov
      ic[upper.tri(ic)] <- NA
      tab <- createJaspTable(l)
      for (i in 1:ncol(ic)) {
        nm <- colnames(ic)[i]
        tab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
      }
      tab$addRows(ic, rowNames = colnames(ic))
      icc[[l]] <- tab
    }
  } else {
    ic <- fv$cov
    ic[upper.tri(ic)] <- NA
    icc <- createJaspTable("Implied covariance matrix", position = 3)
    for (i in 1:ncol(ic)) {
      nm <- colnames(ic)[i]
      icc$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
    }
    icc$addRows(ic, rowNames = colnames(ic))
    jaspResults[["impcov"]] <- icc
  }

  icc$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify",
                        "uncorrelatedFactors", "mimic", "estimator", "se", "bootstrapNumber",
                        "groupvar", "invariance", "impliedCov"))
}

.cfaTableResCov <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$residCov || !is.null(jaspResults[["rescov"]])) return()
  rv <- lavaan::residuals(cfaResult[["lav"]])

  if (options$groupvar != "") {
    jaspResults[["rescov"]] <- rcc <- createJaspContainer("Residual covariance matrices", position = 4)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (l in groupLabs) {
      rc <- rv[[l]]$cov
      rc[upper.tri(rc)] <- NA
      tab <- createJaspTable(l)
      for (i in 1:ncol(rc)) {
        nm <- colnames(rc)[i]
        tab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3;p:.001")
      }
      tab$addRows(rc, rowNames = colnames(rc))
      rcc[[l]] <- tab
    }
  } else {
    rc <- rv$cov
    rc[upper.tri(rc)] <- NA
    rcc <- createJaspTable("Residual covariance matrix", position = 4)
    for (i in 1:ncol(rc)) {
      nm <- colnames(rc)[i]
      rcc$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3;p:.001")
    }
    rcc$addRows(rc, rowNames = colnames(rc))
    jaspResults[["rescov"]] <- rcc
  }

  rcc$dependOn(c("factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors",
                        "mimic", "estimator", "se", "bootstrapNumber", "groupvar", "invariance", "residCov"))
}

.cfaInitPlots <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !(options$pathplot || options$misfitplot) || !is.null(jaspResults[["plots"]])) return()

  jaspResults[["plots"]] <- createJaspContainer("Plots", position = 6)
  jaspResults[["plots"]]$dependOn(c(
    "factors", "secondOrder", "rescov", "includemeanstructure", "identify", "uncorrelatedFactors", "mimic",
    "estimator", "se", "bootstrapNumber", "groupvar", "invariance"
  ))
}

.cfaPlotPath <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$pathplot || !is.null(jaspResults[["plots"]][["pathplot"]])) return()

  png() # semplot opens a device even though we specify doNotPlot, so we hack.
  pathplot <- semPlot::semPaths(
    object         = .cfaLavToPlotObj(cfaResult[["lav"]], options),
    DoNotPlot      = TRUE,
    ask            = FALSE,
    layout         = "tree",
    intercepts     = options$plotmeans,
    whatLabels     = ifelse(options$plotpars, "par", "name"),
    mar            = ifelse(rep(is.null(options$secondOrder), 4), c(12, 3, 12, 3), c(6, 3, 6, 3)),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width   = 1.5,
    edge.label.cex = 0.9,
    lty            = 2,
    title          = FALSE
  )
  dev.off()

  # set height depending on whether there is a second-order factor
  plotwidth  <- 640
  plotheight <- 320
  if (length(cfaResult[["spec"]][["soLatents"]]) > 0) plotheight <- 500

  if (options$groupvar != "") {
    jaspResults[["plots"]][["pathplot"]] <- createJaspContainer("Model plots", position = 1)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (i in 1:length(groupLabs)) {
      jaspResults[["plots"]][["pathplot"]][[groupLabs[i]]] <-
        createJaspPlot(pathplot[[i]], title = groupLabs[i], height = plotheight, width = plotwidth)
      jaspResults[["plots"]][["pathplot"]][[groupLabs[i]]]$dependOn(c("pathplot", "plotmeans", "plotpars"))
    }
  } else {
    jaspResults[["plots"]][["pathplot"]] <- createJaspPlot(pathplot, title = "Model plot", height = plotheight,
                                                           width = plotwidth)
  }

  jaspResults[["plots"]][["pathplot"]]$dependOn(c("pathplot", "plotmeans", "plotpars"))
}

.cfaLavToPlotObj <- function(lavResult, options) {
  # Create semplot model and unv the names of the manifest variables and backtranslate latent variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]

  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- .unv(manifests)
  latents   <- semPlotMod@Vars$name[!semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[!semPlotMod@Vars$manifest] <- .translateFactorNames(latents, options)

  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest)) semPlotMod@Pars$lhs[lhsAreManifest] <- .unv(semPlotMod@Pars$lhs[lhsAreManifest])
  lhsAreLatent   <- semPlotMod@Pars$lhs %in% latents
  if (any(lhsAreLatent))
    semPlotMod@Pars$lhs[lhsAreLatent] <- .translateFactorNames(semPlotMod@Pars$lhs[lhsAreLatent], options)


  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest)) semPlotMod@Pars$rhs[rhsAreManifest] <- .unv(semPlotMod@Pars$rhs[rhsAreManifest])
  rhsAreLatent   <- semPlotMod@Pars$rhs %in% latents
  if (any(rhsAreLatent))
    semPlotMod@Pars$rhs[rhsAreLatent] <- .translateFactorNames(semPlotMod@Pars$rhs[rhsAreLatent], options)

  return(semPlotMod)
}

.cfaPlotMisfit <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$misfitplot || !is.null(jaspResults[["plots"]][["misfitplot"]])) return()
  rescor <- lavaan::residuals(cfaResult[["lav"]], type = "cor")
  wh <- 50 + 50 * length(cfaResult[["spec"]][["variables"]])

  if (options$groupvar != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    jaspResults[["plots"]][["misfitplot"]] <- createJaspContainer("Misfit plots", position = 2)
    for (i in 1:length(groupLabs)) {
      cc <- rescor[[i]]$cov
      cc[upper.tri(cc)] <- NA
      gg <- .resCorToMisFitPlot(cc)
      jaspResults[["plots"]][["misfitplot"]][[groupLabs[i]]] <-
        createJaspPlot(gg, title = groupLabs[i], width = wh, height = wh)
      jaspResults[["plots"]][["misfitplot"]][[groupLabs[i]]]$dependOn("misfitplot")
    }
  } else {
    cc <- rescor$cov
    cc[upper.tri(cc)] <- NA
    gg <- .resCorToMisFitPlot(cc)
    jaspResults[["plots"]][["misfitplot"]] <- createJaspPlot(gg, title = "Misfit plot", width = wh, height = wh)
  }

  jaspResults[["plots"]][["misfitplot"]]$dependOn("misfitplot")
}

.resCorToMisFitPlot <- function(rescor) {
  ggmisfit <- reshape2::melt(abs(t(rescor)))
  ggmisfit$labels <- substr(round(ggmisfit$value, 2), 2, 4)
  ggmisfit$labels[ggmisfit$labels == ""] <- "0"

  levels(ggmisfit$Var1) <- .unv(levels(ggmisfit$Var1))
  levels(ggmisfit$Var2) <- .unv(levels(ggmisfit$Var2))

  misfitplot <-
    ggplot2::ggplot(ggmisfit, ggplot2::aes(x = Var1, y = Var2, fill = value,
                                           label = labels)) +
    ggplot2::geom_tile(na.rm = TRUE) +
    ggplot2::geom_text(color = ifelse(ggmisfit$value > .5, "white", "black"),
                       na.rm = TRUE) +
    ggplot2::scale_y_discrete(limits = rev(levels(ggmisfit$Var1))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_continuous(low = "#FFFFFF", high = "#000000",
                                   na.value = "transparent",
                                   limits = c(0, 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 0)) +
    JASPgraphs::themeJaspRaw()

  return(misfitplot)
}

.cfaSyntax <- function(jaspResults, options, dataset, cfaResult) {
  if (is.null(cfaResult) || !options$showSyntax || !is.null(jaspResults[["syntax"]])) return()

  mod <- .optionsToCFAMod(options, dataset, cfaResult, FALSE)

  jaspResults[["syntax"]] <- createJaspHtml(mod, class = "jasp-code", position = 7, title = "Model syntax")
  jaspResults[["syntax"]]$dependOn(optionsFromObject = jaspResults[["maincontainer"]][["cfatab"]])
  jaspResults[["syntax"]]$dependOn("showSyntax")
}
