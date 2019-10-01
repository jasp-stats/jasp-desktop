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
  .metaAnalysisTrimFillPlot(jaspResults, dataset, options, ready)
  .metaAnalysisProfilePlot(jaspResults, dataset, options, ready)
  .metaAnalysisDiagnosticPlot(jaspResults, dataset, options, ready)
  
  return()
}

.metaAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) 
    return(dataset)
  else {
    studyLabelName  <- options$studyLabels
    effsizeName <- unlist(options$dependent)
    stderrName  <- unlist(options$wlsWeights)
    
    covarNames <- if (length(options$covariates) > 0) unlist(options$covariates)
    factNames  <- if (length(options$factors) > 0) unlist(options$factors)
    
    list.variables    <- Filter(function(s) s != "", c(effsizeName, covarNames))
    numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
    factor.variables  <- Filter(function(s) s != "", c(factNames, studyLabelName))
    return(.readDataSetToEnd(columns.as.factor  = factor.variables,
                      columns.as.numeric = numeric.variables,
                      exclude.na.listwise = numeric.variables))
  }
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

.metaAnalysisCheckErrors <- function(dataset, options){
  custom <- function() {
    if(any(dataset[[b64(options$wlsWeights)]] <= 0))
      return(paste0("Non-positive standard errors encountered in variable '", 
             options$wlsWeights,
             "'. Standard errors should be positive. Please check your input."))
  }
  effsizeName <- unlist(options$dependent)
  stderrName  <- unlist(options$wlsWeights)
  covarNames  <- if (length(options$covariates) > 0) unlist(options$covariates)
  numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
  .hasErrors(dataset              = dataset, 
             type                 = c("infinity", "observations"),
             custom               = custom,
             all.target           = numeric.variables, 
             observations.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)
}

.metaAnalysisComputeModel <- function(jaspResults, dataset, options, ready, method) {
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
    
    if(length(options$modelTerms) > 0) {
      # infer model formula
      .vmodelTerms <- b64(options$modelTerms)
      
      formula.rhs <- as.formula(as.modelTerms(.vmodelTerms))
    } else
      formula.rhs <- NULL
    
    if (is.null(formula.rhs))
      formula.rhs <- ~1
    if (!options$includeConstant)
      formula.rhs <- update(formula.rhs, ~ . + 0)
    
    if (identical(formula.rhs, ~ 1 - 1))
      .quitAnalysis("The model should contain at least one predictor or an intercept.")
    
    # analysis
    rma.fit <- tryCatch( # rma generates informative error messages; use them!
      metafor::rma(
        yi      = get(b64(options$dependent)), 
        sei     = get(b64(options$wlsWeights)), 
        data    = dataset,
        method  = method, 
        mods    = formula.rhs, 
        test    = options$test,
        slab    = if(options$studyLabels != "") paste0(get(b64(options$studyLabels))),
        # add tiny amount because 1 is treated by rma() as 100% whereas values > 1 as percentages
        level   = options$regressionCoefficientsConfidenceIntervalsInterval + 1e-9, 
        control = list(maxiter = 500)), 
      error = function(e) .quitAnalysis(e$message))
    
    rma.fit <- d64(rma.fit, values = all.vars(formula.rhs))
  }
  
  # Save results to state
  jaspResults[["Model"]] <- createJaspState(rma.fit)
  dependList <- c("modelTerms", "dependent", "wlsWeights", "test", "studyLabels",
                  "regressionCoefficientsConfidenceIntervalsInterval")
  jaspResults[["Model"]]$dependOn(dependList)
  return(rma.fit)
}

#Tables
.metaAnalysisFixRandTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["fixRandTable"]])) return()
  
  fixRandTable <- createJaspTable("Fixed and Random Effects")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels")
  fixRandTable$dependOn(dependList)
  fixRandTable$position <- 1
  msg <- ("Hedges, L. V., & Olkin, I. (1985). Statistical methods for meta-analysis. 
          San Diego, CA: Academic Press.")
  fixRandTable$addCitation(msg)

  fixRandTable$addColumnInfo(name = "name",  type = "string",  title = " ")
  fixRandTable$addColumnInfo(name = "qstat", type = "number",  title = "Q")
  fixRandTable$addColumnInfo(name = "df",    type = "integer", title = "df")
  fixRandTable$addColumnInfo(name = "pval",  type = "pvalue",  title = "p")
  
  message <- "<em>p</em>-values are approximate."
  fixRandTable$addFootnote(message)
  
  jaspResults[["fixRandTable"]] <- fixRandTable
  
  res <- try(.metaAnalysisFixRandFill(jaspResults, dataset, options, ready))
  
  .metaAnalysisSetError(res, fixRandTable)
}

.metaAnalysisCoeffTable <- function(jaspResults, dataset, options, ready) {
  if (!options$regressionCoefficientsEstimates || !is.null(jaspResults[["coeffTable"]])) 
    return()
  
  coeffTable <- createJaspTable("Coefficients")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "regressionCoefficientsConfidenceIntervals", "regressionCoefficientsEstimates")
  coeffTable$dependOn(dependList)
  coeffTable$position <- 2
  coeffTable$showSpecifiedColumnsOnly <- TRUE
  msg <- "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. 
         Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/"
  coeffTable$addCitation(msg)
  
  coeffTable$addColumnInfo(name = "name",  type = "string", title = " ")
  coeffTable$addColumnInfo(name = "est",   type = "number", title = "Estimate")
  coeffTable$addColumnInfo(name = "se",    type = "number", title = "Standard Error")
  coeffTable$addColumnInfo(name = "zval",  type = "number", title = "z")
  coeffTable$addColumnInfo(name = "pval",  type = "pvalue", title = "p")
  .metaAnalysisConfidenceInterval(options, coeffTable)
  
  message <- switch(options$test, z = "Wald test.", "Wald tests.")
  coeffTable$addFootnote(message)
  
  jaspResults[["coeffTable"]] <- coeffTable
  if(!ready)
    return()
  
  method <- .metaAnalysisGetMethod(options)
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  
  res <- try(.metaAnalysisCoeffFill(jaspResults, dataset, options, ready, rma.fit))
  
  .metaAnalysisSetError(res, coeffTable)
}

.metaAnalysisFitMeasuresTable <- function(jaspResults, dataset, options, ready) {
  
  if (!options$modelFit || !is.null(jaspResults[["fitMeasuresTable"]])) 
    return()
  
  fitMeasuresTable <- createJaspTable("Fit measures")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "modelFit")
  fitMeasuresTable$dependOn(dependList)
  fitMeasuresTable$position <- 3
  
  method <- options$method
  if(ready)
    method <- .metaAnalysisGetMethod(options)
  
  fitMeasuresTable$addColumnInfo(name = "name",   type = "string", title = " ")
  fitMeasuresTable$addColumnInfo(name = "method", type = "number", title = method)
  
  jaspResults[["fitMeasuresTable"]] <- fitMeasuresTable
  
  res <- try(.metaAnalysisFitMeasuresFill(jaspResults, dataset, options, ready))
  
  .metaAnalysisSetError(res, fitMeasuresTable)
}

.metaAnalysisResidualTable <- function(jaspResults, dataset, options, ready) {
  method <- .metaAnalysisGetMethod(options)
  
  if (!options$residualsParameters || method == "FE"|| 
      !is.null(jaspResults[["residualTable"]])) 
    return()
  
  residualTable <- createJaspTable("Residual Heterogeneity Estimates")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "regressionCoefficientsConfidenceIntervals", "residualsParameters")
  residualTable$dependOn(dependList)
  residualTable$position <- 4
  residualTable$showSpecifiedColumnsOnly <- TRUE
  
  
  residualTable$addColumnInfo(name = "name",  type = "string",  title = " ")
  residualTable$addColumnInfo(name = "est",   type = "number",  title = "Estimate")
  .metaAnalysisConfidenceInterval(options, residualTable)
  
  jaspResults[["residualTable"]] <- residualTable

  res <- try(.metaAnalysisResidualFill(jaspResults, dataset, options, ready))
  
  .metaAnalysisSetError(res, residualTable)
}

.metaAnalysisCovMatTable <- function(jaspResults, dataset, options, ready) {
  if (!options$regressionCoefficientsCovarianceMatrix || 
      !is.null(jaspResults[["covMatTable"]])) 
    return()
  
  covMatTable <- createJaspTable("Parameter Covariances")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "regressionCoefficientsCovarianceMatrix")
  covMatTable$dependOn(dependList)
  covMatTable$position <- 5
  covMatTable$showSpecifiedColumnsOnly <- TRUE
  
  
  covariates <- unlist(options$modelTerms)
  covariates <- c("intrcpt", covariates)
  
  covMatTable$addColumnInfo(name = "name",  type = "string",  title = " ")
  if(!ready)
    covMatTable$addColumnInfo(name = "intrcpt", type = "number", title = "...")
  else
    for(i in 1:length(covariates))
      covMatTable$addColumnInfo(name = covariates[[i]], type = "number")
  
  jaspResults[["covMatTable"]] <- covMatTable

  res <- try(.metaAnalysisCovMatFill(jaspResults, dataset, options, ready))
  
  .metaAnalysisSetError(res, covMatTable)
}

.metaAnalysisRankTestTable <- function(jaspResults, dataset, options, ready) {
  if (!options$rSquaredChange || !is.null(jaspResults[["rankTestTable"]])) 
    return()
  
  rankTestTable <- createJaspTable("Rank correlation test for Funnel plot asymmetry")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "rSquaredChange")
  rankTestTable$dependOn(dependList)
  rankTestTable$position <- 6
  rankTestTable$showSpecifiedColumnsOnly <- TRUE
  
  
  rankTestTable$addColumnInfo(name = "name",    type = "string", title = " ")
  rankTestTable$addColumnInfo(name = "kendall", type = "number", title = "Kendall's \u3C4")
  rankTestTable$addColumnInfo(name = "pval",    type = "pvalue", title = "p")
  
  jaspResults[["rankTestTable"]] <- rankTestTable
  
  res <- try(.metaAnalysisRankTestFill(jaspResults, dataset, options, ready))
  
  .metaAnalysisSetError(res, rankTestTable)
}

.metaAnalysisRegTestTable <- function(jaspResults, dataset, options, ready) {
  if (!options$funnelPlotAsymmetryTest || !is.null(jaspResults[["regTestTable"]])) 
    return()
  title <- "Regression test for Funnel plot asymmetry (\"Egger's test\")"
  regTestTable <- createJaspTable(title)
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "funnelPlotAsymmetryTest")
  regTestTable$dependOn(dependList)
  regTestTable$position <- 6
  regTestTable$showSpecifiedColumnsOnly <- TRUE
  
  regTestTable$addColumnInfo(name = "name",    type = "string", title = " ")
  if (options$test == "knha")
    regTestTable$addColumnInfo(name = "t", type = "number")
  else
    regTestTable$addColumnInfo(name = "z", type = "number")
  regTestTable$addColumnInfo(name = "pval",    type = "pvalue", title = "p")
  ##Should I add confidence intervals, and intercept values?
  
  jaspResults[["regTestTable"]] <- regTestTable
  
  if(!ready)
    return()
  
  res <- try(.metaAnalysisRegTestFill(jaspResults, dataset, options))
  
  .metaAnalysisSetError(res, regTestTable)
}

.metaAnalysisCasewiseTable <- function(jaspResults, dataset, options, ready) {
  if (!options$residualsCasewiseDiagnostics || !is.null(jaspResults[["casewiseTable"]])) 
    return()
  
  casewiseTable <- createJaspTable("Influence Measures")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "residualsCasewiseDiagnostics")
  casewiseTable$dependOn(dependList)
  casewiseTable$position <- 6
  casewiseTable$showSpecifiedColumnsOnly <- TRUE
  tau2title <- "\u3C4\u00B2<sub>(-i)</sub>" #"\u3C4\u00B2\u208D\u208B\u1D62\u208E"
  QEtitle   <- "Q<sub>E(-i)]</sub>"         #"Q[E]\u208D\u208B\u1D62\u208E"
  
  casewiseTable$addColumnInfo(name = "name",   type = "string",  title = " ")
  casewiseTable$addColumnInfo(name = "sdRes",  type = "number",  title = "Std. Residual")
  casewiseTable$addColumnInfo(name = "dfFits", type = "number",  title = "DFFITS")
  casewiseTable$addColumnInfo(name = "cook",   type = "number",  title = "Cook's Distance")
  casewiseTable$addColumnInfo(name = "cov",    type = "number",  title = "Cov. Ratio")
  casewiseTable$addColumnInfo(name = "tau2",   type = "number",  title = tau2title)
  casewiseTable$addColumnInfo(name = "QE",     type = "number",  title = QEtitle)
  casewiseTable$addColumnInfo(name = "hat",    type = "number",  title = "Hat")
  casewiseTable$addColumnInfo(name = "weight", type = "number",  title = "Weight")
  
  jaspResults[["casewiseTable"]] <- casewiseTable
  
  if(!ready)
    return()
  
  method <- .metaAnalysisGetMethod(options)
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  
  res <- try(.metaAnalysisCasewiseFill(jaspResults, dataset, options, rma.fit))
  
  .metaAnalysisSetError(res, casewiseTable)
  
  message <- "Cases marked with \u002A are influential."
  casewiseTable$addFootnote(message)
}

.metaAnalysisFailSafeTable <- function(jaspResults, dataset, options, ready) {
  if (!options$plotResidualsCovariates || !is.null(jaspResults[["failSafeTable"]]) || !ready) 
    return()
  
  failSafeTable <- createJaspTable("File Drawer Analysis")
  dependList <- c("modelTerms", "dependent", "wlsWeights", "factors", "studyLabels",
                  "plotResidualsCovariates")
  failSafeTable$dependOn(dependList)
  failSafeTable$position <- 6
  failSafeTable$showSpecifiedColumnsOnly <- TRUE
  
  failSafeTable$addColumnInfo(name = "name",  type = "string", title = " ")
  failSafeTable$addColumnInfo(name = "fsnum", type = "number", title = "Fail-safe N")
  failSafeTable$addColumnInfo(name = "alpha", type = "number", title = "Target Significance")
  failSafeTable$addColumnInfo(name = "pval",  type = "pvalue", title = "Observed Significance")
  
  jaspResults[["failSafeTable"]] <- failSafeTable
  
  res <- try(.metaAnalysisFailSafeFill(jaspResults, dataset, options))
  
  .metaAnalysisSetError(res, failSafeTable)
}

# Plots
.metaAnalysisPlotsContainer <- function(jaspResults, options, ready)  {
  if(!ready) return()
  #if(!options$forestplot && !options$funnelPlot && !options$plotResidualsDependent &&
  #   !options$plotResidualsPredicted && !options$trimfillPlot) 
  #  return()
  if (is.null(jaspResults[["plots"]])) {
    container <- createJaspContainer("Plot")
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
  method    <- .metaAnalysisGetMethod(options)
  # Compute/get model
  rma.fit    <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  img.height <- 400
  if(!is.null(rma.fit))
    img.height <- max(520, nobs(rma.fit) * 20)
  forestPlot   <- createJaspPlot(title = "Forest plot", width = 520, height = img.height)
  forestPlot$dependOn(c("forestPlot"))
  container[["forest"]] <- forestPlot
  browser()
  if(ready){
    p <- try(.metaAnalysisForestPlotFill(jaspResults, dataset, options, ready, rma.fit, img.height))
    #p <- .writeImage(width = 520, height = img.height, plot = forest.rma(rma.fit))
    if(isTryError(p))
      forestPlot$setError(.extractErrorMessage(p))
    else
      forestPlot$plotObject <- p
  }
  return()
}

#Plot filling
.metaAnalysisForestPlotFill <- function(jaspResults, dataset, options, ready, rma.fit, img.height){
  #p <- cmaForest(rma.fit, cex.lab = 1.2, las = 1, efac = 15 / nobs(rma.fit))
  ggplot2::theme_set(ggplot2::theme_bw(base_size=10))
  ci.lb    <- rma.fit$yi - qnorm(rma.fit$level/2, lower.tail = FALSE) * sqrt(rma.fit$vi)
  ci.lb    <- round(ci.lb, digits=2)
  ci.ub    <- rma.fit$yi + qnorm(rma.fit$level/2, lower.tail = FALSE) * sqrt(rma.fit$vi)
  ci.ub    <- round(ci.ub, digits=2)
  ci.int   <- paste0(round(rma.fit$yi, 2), "[", ci.lb, " ", ci.ub, "]")
  b.pred   <- predict(rma.fit)
  b.ci.lb  <- round(b.pred$ci.lb, 2)
  b.ci.ub  <- round(b.pred$ci.ub, 2)
  b.ci.int <- paste0(round(b.pred$pred, 2), "[", b.ci.lb, " ", b.ci.ub, "]")
  
  rma.data <-  data.frame(StudyNo = rma.fit$ids, 
                          ES      = rma.fit$yi,
                          SE      = sqrt(rma.fit$vi),
                          Type    = "Study", 
                          SecNms  = ci.int,
                          ci.lb   = ci.lb,
                          ci.ub   = ci.ub)
  #browser()
  
  StudyNames <- paste0("Study ", rma.data$StudyNo)
  
  REName <- ifelse((rma.fit$method == "FE"), "FE Model", "RE Model")
  #rma.data$Study2<-factor(rma.data$Study, levels=rev(levels(rma.data$Study)) )
  yticks <- pretty(c(min(ci.lb), max(ci.ub)))
  ymin   <- min(yticks)
  ymax   <- max(yticks)
  yLabs  <- JASPgraphs::axesLabeller(yticks)
  xticks <- rma.data$StudyNames
  xLabs  <- JASPgraphs::axesLabeller(xticks)
  
  REdata <- data.frame(StudyNo = -1, 
             ES      = b.pred$pred,
             SE      = rma.fit$se,
             Type    = "RE Model", 
             SecNms  = b.ci.int,
             ci.lb   = b.ci.lb,
             ci.ub   = b.ci.ub)
  #levels(rma.data$Study2)
  #p <- JASPgraphs::drawAxis(xName = "Study", yName = "Observed Outcome", 
  #                          xBreaks = xticks, yBreaks = yticks, 
  #                          yLabels = yLabs, xLabels = xLabs)
  p <- ggplot2::ggplot(data = rma.data, ggplot2::aes(x      = StudyNo, 
                                                     y      = ES,
                                                     colour = factor(Type)
                                                     )
                       )
  p <- p +  ggplot2::geom_point(data  = rma.data,
                                size  = rma.data$SE*8/max(rma.data$SE), 
                                shape = 15, colour = "black")
  p <- p +  ggplot2::geom_errorbar(data = rma.data, 
                           ggplot2::aes(x = StudyNo, ymax = ci.ub, ymin = ci.lb), 
                           width = 0.5, colour = "black")
  p <- p +  ggplot2::geom_point(data  = REdata,
                                size  = REdata$SE*8/max(REdata$SE), 
                                shape = 18, colour = "black")
  p <- p +  ggplot2::geom_errorbar(data = REdata, 
                                   ggplot2::aes(x = StudyNo, ymax = ci.ub, ymin = ci.lb), 
                                   width = 0.5, colour = "black")
  p <- p + ggplot2::coord_flip() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), lty=2, size=0.5, colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), lty=1, size=0.5, colour = "black") +
    ggplot2::scale_size_manual(values=c(0.5,1))
  #browser()
  p <- p + ggplot2::xlab("") +
    ggplot2::ylab("Observed Outcome") + 
    ggplot2::ylim(ymin,ymax) + 
    ggplot2::scale_x_discrete(limits =  StudyNames) +ggplot2::geom_label(REName)
  p <- p + ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(hjust = 0),
                          axis.text.x.top    = ggplot2::element_text(hjust = 1),
                          axis.text.y        = ggplot2::element_text(hjust = 0),
                          axis.line.x.top    = ggplot2::element_line(),
                          axis.line.x.bottom = ggplot2::element_line(size = 0.5),
                          axis.ticks.x       = ggplot2::element_line(size=0.8))
  p <- JASPgraphs::themeJasp(p, 
                             axisTickWidth  = 0,
                             xAxis = FALSE,
                             legend.position = "none")
  return(p)
}

.metaAnalysisFunnelPlot <- function(jaspResults, dataset, options, ready) {
  
}

.metaAnalysisTrimFillPlot <- function(jaspResults, dataset, options, ready) {
  
}

.metaAnalysisProfilePlot <- function(jaspResults, dataset, options, ready) {
  
}

.metaAnalysisDiagnosticPlot <- function(jaspResults, dataset, options, ready) {
  
}

#Table filling
.metaAnalysisFixRandFill <- function(jaspResults, dataset, options, ready) {
  method <- .metaAnalysisGetMethod(options)
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  
  row <- list(name = "Omnibus test of Model Coefficients", 
              qstat = ".", df = ".", pval = ".")
  if(ready) {
    row$qstat <- rma.fit$QM
    row$df    <- rma.fit$m
    row$pval  <- rma.fit$QMp
  }
  jaspResults[["fixRandTable"]]$addRows(row)
  row <- list(name = "Test of Residual Heterogeneity",     
              qstat = ".", df = ".", pval = ".")
  if(ready) {
    row$qstat <- rma.fit$QE
    row$df    <- rma.fit$k - rma.fit$p
    row$pval  <- rma.fit$QEp
  }
  jaspResults[["fixRandTable"]]$addRows(row)
}

.metaAnalysisCoeffFill <- function(jaspResults, dataset, options, ready, rma.fit) {
  
  results <- list()
  
  coeff <- coef(summary(rma.fit))
  cov <- unlist(options$modelTerms)
  cov <- c("intrcpt", cov)
  len.cov <- length(cov)
  for(i in 1:len.cov) {
    results <- list(
      name = cov[[i]],
      est  = coeff[i,1],
      se   = coeff[i,2],
      zval = coeff[i,3],
      pval = coeff[i,4] 
    )
    if(options$regressionCoefficientsConfidenceIntervals) {
      results$lower <- coeff[i,5]
      results$upper <- coeff[i,6]
    }
    jaspResults[["coeffTable"]]$addRows(results)
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
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
    fitStats <- try(metafor:::fitstats(rma.fit))
    stats$logLik   <- fitStats[[1]]
    stats$deviance <- fitStats[[2]]
    stats$AIC      <- fitStats[[3]]
    stats$BIC      <- fitStats[[4]]
    stats$AICc     <- fitStats[[5]]
  }
  
  # Fill table
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = "Log-likelihood", method = stats$logLik))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = "Deviance",       method = stats$deviance))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = "AIC",            method = stats$AIC))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = "BIC",            method = stats$BIC))
  jaspResults[["fitMeasuresTable"]]$addRows(list(name = "AICc",           method = stats$AICc))
}

.metaAnalysisResidualFill <- function(jaspResults, dataset, options, ready) {
  est <- ci.lower <- ci.upper <- list(
    tau2 = ".",
    tau  = ".",
    I2   = ".",
    H2   = "."
  )
  
  if(ready) {
    # Compute/get model
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
    confInt <- options$regressionCoefficientsConfidenceIntervalsInterval
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
  
  # Fill table
  jaspResults[["residualTable"]]$addRows(list(
    list(name = "\u3C4\u00B2", est = est$tau2, 
         lower = ci.lower$tau2, upper = ci.upper$tau2),
    list(name = "\u3C4", est = est$tau,
         lower = ci.lower$tau, upper = ci.upper$tau),
    list(name = "I\u00B2 (%)", est = est$I2,
         lower = ci.lower$I2, upper = ci.upper$I2),
    list(name = "H\u00B2", est = est$H2,
         lower = ci.lower$H2, upper = ci.upper$H2)
    ))
}

.metaAnalysisCovMatFill <- function(jaspResults, dataset, options, ready) {
  results <- list()
  cov <- unlist(options$modelTerms)
  cov <- c("intrcpt", cov)
  len.cov <- length(cov)
  if(ready) {
    # Compute/get model
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
    confInt <- options$regressionCoefficientsConfidenceIntervalsInterval
    coeffVcov <- try(vcov(rma.fit))
    for(i in 1:length(cov)) {
      row <- list(name = cov[[i]])
      for(j in 1:length(cov))
        row[[paste(cov[[j]])]]  <- coeffVcov[i,j]
      jaspResults[["covMatTable"]]$addRows(row)
    }
  } else 
    jaspResults[["covMatTable"]]$addRows(list(name = "...", intrcpt = ".")) 
}

.metaAnalysisRankTestFill <- function(jaspResults, dataset, options, ready) {
  results <- list(name = "Rank test", kendall = ".", pval = ".")
  
  if(ready) {
    # Compute/get model
    rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
    ranktst <- unlist(metafor::ranktest(rma.fit))
    results$kendall <- ranktst[[1]]
    results$pval    <- ranktst[[2]]
  }
  
  jaspResults[["rankTestTable"]]$addRows(results)
}

.metaAnalysisRegTestFill <- function(jaspResults, dataset, options) {
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  egger   <- metafor::regtest(rma.fit)
  coef    <- metafor::coef.summary.rma(summary(egger$fit))
  #if(options$includeConstant) {
  #  results <- list()
  #  results$name <- rownames(coef)[[1]]
  #  if (options$test == "knha")
  #    results$t <- coef[1,3]
  #  else
  #    results$z <- coef[1,3]
  #  results$pval <- coef[1,4]
  #  #results$lower <- coef[1,5]
  #  #results$upper <- coef[1,6]
  #  jaspResults[["regTestTable"]]$addRows(results)
  #}
  results <- list()
  results$name <- rownames(coef)[[2]]
  if (options$test == "knha")
    results$t <- coef[2,3]
  else
    results$z <- coef[2,3]
  results$pval <- coef[2,4]
  #results$lower <- coef[2,5]
  #results$upper <- coef[2,6]
  
  jaspResults[["regTestTable"]]$addRows(results)
}

.metaAnalysisCasewiseFill <- function(jaspResults, dataset, options, rma.fit) {
  influ <- influence(rma.fit)
  influ.inf <- influ$inf
  influ.is.infl <- influ$is.infl
  for(i in 1:length(influ.inf$rstudent)) {
    if(influ.is.infl[i])
      name <- paste0(i, "\u002A")
    else
      name <- paste0(i)
    
    jaspResults[["casewiseTable"]]$addRows(list(
      name   = name,
      sdRes  = influ.inf$rstudent[i],
      dfFits = influ.inf$dffits[i],
      cook   = influ.inf$cook.d[i],
      cov    = influ.inf$cov.r[i],
      tau2   = influ.inf$tau2.del[i],
      QE     = influ.inf$QE.del[i],
      hat    = influ.inf$hat[i],
      weight = influ.inf$weight[i]
    ))
  }
}

.metaAnalysisFailSafeFill <- function(jaspResults, dataset, options) {
  # Compute/get model
  rma.fit <- .metaAnalysisComputeModel(jaspResults, dataset, options, ready, method)
  fsn.fit <- metafor::fsn(yi   = get(b64(options$dependent)), 
                          sei  = get(b64(options$wlsWeights)), 
                          data = dataset)
  fsn.fit <- d64(fsn.fit)
  
  results <- list()
  results$name  <- fsn.fit$type
  results$fsnum <- fsn.fit$fsnum
  results$alpha <- fsn.fit$alpha
  results$pval  <- fsn.fit$pval
  
  jaspResults[["failSafeTable"]]$addRows(results)
}

# Extra functions
.metaAnalysisConfidenceInterval <- function(options, table) {
  if(options$regressionCoefficientsConfidenceIntervals) {
    ci <- paste0(100*options$regressionCoefficientsConfidenceIntervalsInterval, "% Confidence Interval")
    table$addColumnInfo(name = "lower", type = "number", title = "Lower", overtitle = ci)
    table$addColumnInfo(name = "upper", type = "number", title = "Upper", overtitle = ci)
  }
}

.metaAnalysisSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}