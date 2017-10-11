#
# Copyright (C) 2013-2017 University of Amsterdam
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
  
  fitOpts <- c("dependent", "wlsWeights", "method", "studyLabels", "covariates",
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
  #   method:         string, one of ["DL", "HE", "SJ", "ML", "REML", "EB", "HS", "GENQ"] (see ?rma)
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
  
  effsizeName     <- options$effectsize
  stderrName      <- options$stderr
  weightsName     <- options$wlsWeights
  studyLabelName  <- options$studyLabels
  
  effsizeName <- unlist(options$dependent)
  stderrName  <- unlist(options$wlsWeight)
  
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
      exclude.na.listwise = c()	# let metafor::rma deal with NA's
    )
  }
  
  can.run = all(c(effsizeName, stderrName) != "")
  
  ## Run the analysis. ####
  if (is.null(rma.fit)) {
    rma.fit <- structure(list('b' = numeric(),'se' = numeric(),'ci.lb' = numeric(),'ci.ub' = numeric(), 
                    'zval' = numeric(),'pval' = numeric()), class = c("dummy", "rma"))
    
    if (run && can.run) {
      #.vmodelTerms = b64(options$modelTerms) # map true names to base64
      .vmodelTerms = options$modelTerms
      dataset = d64(dataset)
      
      formula.rhs <- as.formula(as.modelTerms(.vmodelTerms))
      if (is.null(formula.rhs)) 
        formula.rhs = ~1
      if (!options$includeConstant) 
        formula.rhs = update(formula.rhs, ~ . + 0)
      
      #rma.fit <- metafor::rma(yi = get(.v(effsizeName)), sei = get(.v(stderrName)), data = dataset,

      rma.fit <- metafor::rma(
        yi = get(effsizeName), sei = get(stderrName), data = dataset,
        method=options$method, mods = formula.rhs, test = options$test,
        slab = if(options$studyLabel != "") get(options$studyLabels),
        level = options$regressionCoefficientsConfidenceIntervalsInterval
      )
      rma.fit <- d64(rma.fit)
      
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
    
    # this is old code to be replaced with as.jaspTable
    
    meta[[length(meta) + 1]] <- list(name = "table", type = "table")
    table <- list()
    table[["title"]] <- "Coefficients"
    
    # Define table schema
    
    fields <- list(
      list(name = "name", type = "string", title = " "),
      list(name = "estimate", type = "number", format = "sf:4;dp:3", title = "Estimate"),
      list(name = "se", type = "number", format = "sf:4;dp:3", title = "Standard Error"),
      list(name = "zval", type = "number", format = "sf:4;dp:3", title = "z"),
      list(name = "pval", type = "number", format = "dp:3;p:.001", title = "p"),
      list(name = "ci.lb", type = "number", format = "sf:4;dp:3", title = "Lower Bound", 
        overTitle=paste0(options$regressionCoefficientsConfidenceIntervalsInterval, "% Confidence interval")),
      list(name = "ci.ub", type = "number", format = "sf:4;dp:3", title = "Upper Bound", 
        overTitle=paste0(options$regressionCoefficientsConfidenceIntervalsInterval, "% Confidence interval"))
    )
    if (!options$regressionCoefficientsConfidenceIntervals)
      fields = fields[1:5] # remove confidence interval bounds
    table[["schema"]] <- list(fields = fields) 
    
    # Populate the table
    
    table[["data"]] <- list(
      list(name = "", estimate = ".", se = ".", zval = ".", pval = ".", ci.lb = ".", ci.ub = ".")
    )
    if (run && can.run || ! is.null(coefficients)) {
      
      if (is.null(coefficients)) {
        coefficients <- .clean(coef(summary(rma.fit)))
      }

      coeftable <- cbind(name = rownames(coefficients), coefficients); 
      if (!options$regressionCoefficientsConfidenceIntervals)
        coeftable = coeftable[1:5] # remove confidence interval bounds
      
      jasp.coeftable <- lapply(1:nrow(coeftable), function(i) as.list(coeftable[i,])) # apply converts all to string
      table[["data"]]  <- structure(jasp.coeftable, .Names=NULL)
    }
    if (DEBUG == 2) return(table)
    
    # Add footnotes to the analysis result
    
    footnotes <- .newFootnotes()
    footnote.text <- switch(options$test, z = "Wald test.", "Wald tests.")
    .addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote.text)
    table[["footnotes"]] <- as.list(footnotes)
    
    # Add citation reference list
    table[["citation"]] <- list(
      "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of 
     Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/"
    )
    
    results[["table"]] <- table
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
  
  if (options$residualsParameters) { 
    
    if (options$method == "FE") break # no confidence intervals for tau^2 FIXME: have to do this more gracefully
        
    meta[[length(meta) + 1]] <- list(name = "residPars", type = "table")
    options(jasp_number_format = "sf:5;dp:4")
    title <- "Residual Heterogeneity Estimates"
    cols <- c("Estimate", "Lower Bound", "Upper Bound")
    rows <- c("<em>&tau;&sup2;</em>", "<em>&tau;</em>", "<em>I&sup2;</em> (%)", "<em>H&sup2;</em>")
    
    if (is.null(residPars) && run && can.run)
      residPars = confint(rma.fit, digits = 12, level = options$regressionCoefficientsConfidenceIntervalsInterval)$random
    
    table <- list(title = title)
    if (! is.null(residPars)) {
      table$x <- residPars
      colnames(table$x) <- cols
      rownames(table$x) <- rows
    } else {
      table$x <- cols
      table$y <- rows
    }
    
    residParsTable = do.call(as.jaspTable, table) # FIXME: we want an overtitle above upper/lower bounds
    results[["residPars"]] <- residParsTable
    
  }
  
  
  ### Prepare Coefficient Covariance Matrix ####
  
  if (options$regressionCoefficientsCovarianceMatrix) { 
    
    meta[[length(meta) + 1]] <- list(name = "vcov", type = "table")
    options(jasp_number_format = "sf:5;dp:4")    
    title <- "Parameter Covariances"
    
    if (is.null(coeffVcov) && run && can.run)
      coeffVcov <- vcov(rma.fit)
    
    table <- list(title = title)
    if (! is.null(coeffVcov)) {
      table$x <- coeffVcov
    } else {
      table$x <- "..."
      table$y <- "..."
    }
    
    vcovTable = do.call(as.jaspTable, table)
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
    cols <- c("z", "p")
    
    if (is.null(egger) && run && can.run)
      egger <- metafor::regtest(rma.fit)
    
    table <- list(title = title)
    if (! is.null(egger)) {
      table$x <- coef(summary(egger$fit))[egger$predictor, c("zval", "pval")]
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
    title <- "Influence Measures"
    cols <- c("Std. Residual", "DFFITS", "Cook's Distance", "Cov. Ratio", 
              "&tau;&sup2;<sub>(-i)</sub>", "Q<sub>E(-i)</sub>", "Hat", "Weight", "Influential")
    
    if (is.null(influ) && run && can.run)
      influ = influence(rma.fit) 
    
    table <- list(title = title)
    if (! is.null(influ)) {
      table$x <- cbind(influ$inf, influential = ifelse(influ$is.infl, "*", "")) #FIXME maybe add the stars to the cases?
      colnames(table$x) <- cols
    } else {
      table$x <- cols
    }
    
    influTable = do.call(as.jaspTable, table)
    results[["influence"]] <- influTable
    
  }
  
  
  ### Prepare Fail-safe N diagnostics table ####
  
  if (options$plotResidualsCovariates) { 
    
    meta[[length(meta) + 1]] <- list(name = "failsafen", type = "table")
    options(jasp_number_format = "sf:5;dp:4")
    title <- "File Drawer Analysis"
    cols <- c("Fail-safe N", "&alpha;", "p")
    
    if (is.null(fsn.fit) && run && can.run)
      fsn.fit <- metafor::fsn(yi = get(effsizeName), sei = get(stderrName), 
        data = dataset, digits = 12)
    
    table <- list(title = title)
    if (! is.null(fsn.fit)) {
      table$x <- do.call(data.frame, fsn.fit[c('fsnum','alpha','pval')])
      colnames(table$x) <- cols
      rownames(table$x) <- fsn.fit[["type"]]
    } else {
      table$x <- cols
    }
    
    influTable = do.call(as.jaspTable, table)
    results[["failsafen"]] <- influTable
    
  }
  
  
  
  ### Prepare Plot Output ####

  meta[[length(meta) + 1]] <- list(name = "plots", type = "object",
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
      forest.img <- .writeImage(width = 520, height = 520, function() cmaForest(rma.fit, cex.lab=1.2, las=1))
      forestPlot <- list(title="Forest plot", width = 500, height = 500, convertable = TRUE, 
                    obj = forest.img[["obj"]], data = forest.img[["png"]], 
                    status = "complete")
    }
    plots[["forrestPlot"]]  <- forestPlot
    
  }

  # plot funnel plot
  if (options$funnelPlot && (! is.null(funnelPlot) || run && can.run)) {
    
    if (is.null(funnelPlot)) {
      funnel.img <- .writeImage(width = 520, height = 520, function() metafor::funnel(rma.fit, las=1))
      funnelPlot <- list(title="Funnel plot", width = 500, height = 500, convertable = TRUE, 
                    obj = funnel.img[["obj"]], data = funnel.img[["png"]], 
                    status = "complete")
    }
    plots[["funnelPlot"]]  <- funnelPlot
    
  }
  
  # plot 1: residuals and dependent diagnostic plot
  if (options$plotResidualsDependent && (! is.null(diagnosticsPlot) || run && can.run)) {
    
    if (is.null(diagnosticsPlot)) {
      diagnostics.img <- .writeImage(width = 820, height = 820, function() plot(rma.fit, qqplot = options$plotResidualsQQ, las=1))
      diagnosticsPlot <- list(title = "Diagnostic plots", width = 820, height = 820, convertable = TRUE, 
                    obj = diagnostics.img[["obj"]], data = diagnostics.img[["png"]],
                    status = "complete")
    }
    plots[["diagnosticPlot"]]  <- diagnosticsPlot
    
  }
  
  # profile plot: diagnostic plot for tau parameter
  if (options$plotResidualsPredicted && (! is.null(profilePlot) || run && can.run)) {
    
    if (is.null(profilePlot)) {
      profile.img <- .writeImage(width = 520, height = 520, function() profile(rma.fit, cex.lab=1.2, las=1))
      profilePlot <- list(title = "Log-likelihood profile for &tau;&sup2;", width = 520, height = 520, convertable = TRUE, 
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
      trimfillPlot <- list(title = "Trim-fill Analysis", width = 820, height = 820, convertable = TRUE, 
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


as.modelTerms <- function(object) structure(object, class = "modelTerms")

formula.modelTerms <- function(modelTerms, env = parent.frame()) {
  # Converts a modelTerms list into a one-side R formula
  #
  # Args:
  #   modelTerms:  A list of interaction terms, each term being a list of variable names involved in the interaction
  #   env:         An environement associated with the variables in the formula, see ?as.formula
  #
  # Value:
  #   A formula. See ?formula
  #
  terms = sapply(modelTerms, function(x) paste0(unlist(x), collapse = ":"))
  terms = terms[terms != ""]
  formula.rhs = paste(terms, collapse = " + ")
  if (formula.rhs != "") as.formula(paste(" ~ ", formula.rhs), env = env)
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
    list(name = "df", type = "integer", format = "sf:4;dp:3", title = "df"),
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
  #image.reference <- .beginSaveImage(width, height)
  plot(object)
  #.endSaveImage(image.reference)
}

rmaForestPlot <- function(object, width = 500, height = 500) {
  #image.reference <- .beginSaveImage(520,520)
  cmaForest(object)
  #.endSaveImage(image.reference)
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

b64.rma <- function(x, ...) {
  ## Translate names in object x to base64 
  idx = c('b', 'beta', 'call')
  x[idx] = b64(x[idx])
  x
}
d64.rma <- function(x, ...) {
  ## Untranslate names in object x from base64 
  idx = c('b', 'beta', 'call')
  x[idx] = d64(x[idx])
  x
}


### to JASP table conversion
## default formatting options
options(jasp_number_format = "sf:4;dp:3", jasp_string_format = NULL)
as.jaspTable <- function(x, ...) UseMethod("as_jaspTable")
as_jaspTable.data.frame <- function(x, title = "", ...) {
  jaspTable = list(data = NULL, schema = NULL, title = title)

  # Extract table schema
  fields = lapply(c("name", names(x)), function(name) {
    if(is.null(x[[name]])) 
      return(list(name = "name", type = "string", format = NULL, title = ""))
    y = x[[name]]
    type = if (!is.numeric(y)) "string" else "number" #FIXME: integers and p values are distinct
    list(name = name, type = type, format = options(paste0("jasp_",type,"_format"))[[1]], title = name)
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
  
  # recompute the data (necessary to work properly with repeated row names) 
  if (nrow(x) > 0) {
    Table <- .clean(as.data.frame(x))
    Table <- cbind(name = rownames(x), Table); 
    
    jasp.table <- lapply(1:nrow(Table), function(i) as.list(Table[i,])) # apply converts all to string
    jaspTable[["data"]]  <- structure(jasp.table, .Names=NULL)
  }
  jaspTable
}

vcov.dummy <- function(x, ...) {
  matrix(0,0,0)
}

# if(interactive()) {
#   # For debug purposes these are reset because the ones defined in 'common.R' only work properly 
#   # from within JASP. Make sure to source('common.R') first!
#   #.beginSaveImage <- function(...) {}
#   #.endSaveImage <- function(...){}
#   .requestTempFileNameNative <- function(ext) tempfile(fileext = ext)
# }