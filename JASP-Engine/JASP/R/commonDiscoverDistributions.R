#
# Copyright (C) 2013-2020 University of Amsterdam
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

### Summary stats for distributions module ----
.simulateData <- function(jaspResults, options, as = "scale"){
  if(is.null(jaspResults[['simdata']])){
    sample <- do.call(options[['rFun']], c(options[['pars']], n = options[['sampleSize']]))
    jaspResults[['simdata']] <- createJaspState(sample)
    jaspResults[['simdata']]$dependOn(c("newVariableName", "simulateNow"))
    
    if(as == "scale"){
      .setColumnDataAsScale  (options[["newVariableName"]], sample)
    } else if(as == "ordinal"){
      .setColumnDataAsOrdinal(options[["newVariableName"]], sample)
    } else{
      .setColumnDataAsNominal(options[["newVariableName"]], sample)
    }
    
  }
  
  return()
}

.ldCheckInteger <- function(variable, errors){
  is_integer <- all((variable %% 1) == 0)
  
  if(isFALSE(errors) && is_integer){
    errors <- FALSE
  } else if(isFALSE(errors) && !is_integer){
    errors <- list(integer = TRUE, message = gettext("The following problem(s) occurred while running the analysis:<ul><li>Variable has to be discrete (i.e., integer)</li></ul>"))
  } else if(!is_integer){
    errors[['integer']] <- TRUE
    errors[['message']] <- paste(errors[['message']], gettext("<ul><li>Variable has to be discrete (i.e., integer)</li></ul>"))
  } else{
    errors <- errors
  }
  
  return(errors)
}

.ldGetDataContainer <- function(jaspResults, options, errors = FALSE){
  if(!is.null(jaspResults[['dataContainer']])){
    dataContainer <- jaspResults[['dataContainer']]
  } else{
    dataContainer <- createJaspContainer(title = gettextf("Overview - %s", options[['variable']]))
    dataContainer$position <- 6
    dataContainer$dependOn(c("variable", "simulateNow"))
    
    jaspResults[['dataContainer']] <- dataContainer
  }
  
  if(!isFALSE(errors) && (!is.null(errors$infinity) || !is.null(errors$observations) || !is.null(errors$integer))){
    dataContainer$setError(errors$message)
  }
  
  if(!isFALSE(errors) && (!is.null(errors$factorLevels))){
    dataContainer$setError(errors$message)
  }
  
  return(dataContainer)
}

#### Descriptives ----
.ldSummaryContinuousTableMain <- function(dataContainer, variable, options, ready) {
  if(!options$summary) return()
  if(!is.null(dataContainer[['summary']])) return()
  if(!ready) return()
  
  summaryTable <- createJaspTable(title = gettext("Descriptives"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("summary"))
  
  summaryTable$addColumnInfo(name = "variable",   title = gettext("Variable"),       type = "string", combine = TRUE)
  summaryTable$addColumnInfo(name = "sampleSize", title = gettext("n"),              type = "integer")
  summaryTable$addColumnInfo(name = "mean",       title = gettext("Mean"),           type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "var",        title = gettext("Variance"),       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "sd",         title = gettext("Std. deviation"), type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "min",        title = gettext("Minimum"),        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile25", title = gettextf("25%% Quantile"),   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "median",     title = gettext("Median"),         type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile75", title = gettextf("75%% Quantile"),   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "max",        title = gettext("Maximum"),        type = "number", format = "sf:4")
  #summaryTable$addColumnInfo(name = "skew",       title = gettext("Skewness"),       type = "number", format = "sf:4")
  #summaryTable$addColumnInfo(name = "kurt",       title = gettext("Kurtosis"),       type = "number", format = "sf:4")
  
  dataContainer[['summary']] <- summaryTable
  
  .ldFillSummaryContinuousTableMain(summaryTable, variable, options)
  
  return()
}

.ldFillSummaryContinuousTableMain <- function(summaryTable, variable, options){
  
  summaryTable$addRows(list(variable   = options[['variable']],
                            sampleSize = sum(!is.na(variable)),
                            mean       = mean(variable, na.rm = TRUE),
                            var        = var(variable, na.rm = TRUE),
                            sd         = sd(variable, na.rm = TRUE),
                            min        = min(variable, na.rm = TRUE),
                            quantile25 = quantile(variable, 0.25, na.rm = TRUE),
                            median     = median(variable, na.rm = TRUE),
                            quantile75 = quantile(variable, 0.75, na.rm = TRUE),
                            max        = max(variable, na.rm = TRUE))
                       #skew       = .summarySkewness(variable),
                       #kurt       = .summaryKurtosis(variable))
  )
  
  return()
}

.ldSummaryFactorTableMain <- function(dataContainer, variable, options, ready) {
  if(!options$summary) return()
  if(!is.null(dataContainer[['summary']])) return()
  if(!ready) return()
  
  summaryTable <- createJaspTable(title = gettext("Descriptives"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("summary"))
  summaryTable$addCitation(.ldAllTextsList()$references$jasp)
  
  summaryTable$addColumnInfo(name = "level", title = "", type = "string")
  summaryTable$addColumnInfo(name = "freq",  title = gettext("n"), type = "integer")
  summaryTable$addColumnInfo(name = "rel.freq",  title = gettext("Rel. Frequency"), type = "number")
  
  summaryTable$setExpectedSize(rows = length(levels(variable)) + 1)
  
  dataContainer[['summary']] <- summaryTable
  
  .ldFillSummaryFactorTableMain(summaryTable, variable, options)
  
  return()
}

.ldFillSummaryFactorTableMain <- function(summaryTable, variable, options){
  for(l in levels(variable)){
    summaryTable$addRows(list(
      level = l,
      freq  = sum(variable == l),
      rel.freq = sum(variable == l)/length(variable)
    ))
  }
  
  summaryTable$addRows(list(
    level = gettext("Total"),
    freq = length(variable),
    rel.freq = "."
  ))
}
### Moments ----

.ldObservedMomentsTableMain <- function(dataContainer, variable, options, ready){
  if(!options$moments) return()
  if(!is.null(dataContainer[['moments']])) return()
  
  momentsTable <- createJaspTable(title = gettext("Observed Moments"))
  momentsTable$position <- 2
  momentsTable$dependOn(c("moments", "momentsUpTo"))
  
  momentsTable$addColumnInfo(name = "moment", title = gettext("Moment"), type = "integer")
  momentsTable$addColumnInfo(name = "raw",    title = gettext("Raw"),    type = "number")
  momentsTable$addColumnInfo(name = "central",title = gettext("Central"),type = "number")
  
  momentsTable$setExpectedSize(rows = options$momentsUpTo)
  
  dataContainer[['moments']] <- momentsTable
  
  if(!ready) return()
  
  .ldFillObservedMomentsTableMain(momentsTable, variable, options)
  return()
}

.ldFillObservedMomentsTableMain <- function(momentsTable, variable, options){
  res <- data.frame(moment = 1:options$momentsUpTo, raw = NA, central = NA)
  
  res$raw     <- .computeObservedMoments(variable, max.moment = options$momentsUpTo, about.mean = FALSE)
  res$central <- .computeObservedMoments(variable, max.moment = options$momentsUpTo, about.mean = TRUE)
  
  momentsTable$setData(res)
}
### Helper functions ----
# .summarySkewness <- function(x) {
#   
#   # Skewness function as in SPSS (for samlpes spaces):
#   # http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005
#   x <- na.omit(x)
#   n <- length(x)
#   m <- mean(x)
#   s <- sd(x)
#   z <- (x - m) / s  # z scores
#   a <- n / ((n - 1) * (n - 2))
#   
#   skewness <- sum(z^3) * a
#   
#   return(skewness)
# }
# 
# .summaryKurtosis <- function(x) {
#   
#   # Kurtosis function as in SPSS:
#   # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
#   # http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis
#   
#   x   <- na.omit(x)
#   n   <- length(x)
#   s4  <- sum((x - mean(x))^4)
#   s2  <- sum((x - mean(x))^2)
#   v   <- s2 / (n-1)
#   a   <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
#   b   <- s4 / (v^2)
#   c   <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
#   
#   kurtosis <- a * b + c
#   
#   return(kurtosis)
# }


.computeObservedMoments <- function(x, max.moment = 2, about.mean = FALSE){
  x <- na.omit(x)
  
  moments <- numeric(length = max.moment)
  moments[1] <- mean(x)
  
  if(max.moment < 2)
    return(moments)
  
  if(about.mean)
    x <- x-moments[1]
  
  
  for(i in 2:max.moment){
    moments[i] <- mean(x^i)
  }
  
  return(moments)
}

.ldGetFitContainer <- function(jaspResults, options, name, title, position, errors = FALSE){
  if(!is.null(jaspResults[[name]])){
    fitContainer <- jaspResults[[name]]
  } else{
    fitContainer <- createJaspContainer(title = gettext(title))
    fitContainer$position <- position
    
    fitContainer$dependOn(c("variable", "simulateNow"))
    
    jaspResults[[name]] <- fitContainer
  }
  
  if(!isFALSE(errors)){
    fitContainer$setError(gettext(errors$message))
  }
  
  return(fitContainer)
}

.ldEstimatesTable <- function(container, options, ci.possible, se.possible, method){
  if(!options$outputEstimates) return()
  if(!is.null(container[['estParametersTable']])) return()
  
  tab <- createJaspTable(title = gettext("Estimated Parameters"))
  tab$dependOn(c("outputEstimates", "outputSE", "ciInterval", "ciIntervalInterval", "parametrization", method, options$parValNames))
  tab$position <- 1
  tab$showSpecifiedColumnsOnly <- TRUE
  tab$setExpectedSize(rows = length(options$pars) - length(options$fix.pars))
  
  tab$addColumnInfo(name = "parName",  title = gettext("Parameter"), type = "string")
  tab$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  
  #"\u03BC\u0302"
  if(options$outputSE && se.possible){
    tab$addColumnInfo(name = "se", title = gettext("SE"), type = "number")
  } else if(options$outputSE) {
    tab$addFootnote(gettext("Standard errors are unavailable with this method"))
  }
  
  if(options$ciInterval && ci.possible){
    tab$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number",
                      overtitle = gettextf("%s%% CI", options[['ciIntervalInterval']]*100))
    tab$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number",
                      overtitle = gettextf("%s%% CI", options[['ciIntervalInterval']]*100))
  } else if(options$ciInterval){
    tab$addFootnote(gettext("Confidence intervals are unavailable with this method."))
  }
  
  if(method == "methodMLE"){
    tab$addCitation(.ldAllTextsList()$references$fitdistrplus)
    
    if(options$ciInterval || options$outputSE){
      tab$addCitation(.ldAllTextsList()$references$car)
    }
    
    if(options$ciInterval && !options$outputSE){
      tab$addFootnote(gettext("Confidence intervals were calculated using the delta method."))
    } else if(!options$ciInterval && options$outputSE){
      tab$addFootnote(gettext("Standard errors were calculated using the delta method."))
    } else if(options$ciInterval && options$outputSE){
      tab$addFootnote(gettext("Standard errors and confidence intervals were calculated using the delta method."))
    }
  }
  
  
  container[['estParametersTable']] <- tab
  
  return(tab)
}

### Fit distributions ----
### MLE stuff ----
.ldMLEResults <- function(mleContainer, variable, options, ready, distName){
  if(!ready) return()
  if(!is.null(mleContainer[['mleResults']])) return(mleContainer[['mleResults']]$object)
  
  starts <- options$pars
  if(!is.null(options$fix.pars)){
    starts[names(options$fix.pars)] <- NULL 
  }
  
  results <- list()
  results$fitdist <- try(fitdistrplus::fitdist(data = variable, distr = distName, method = "mle", 
                                               start = starts, fix.arg = options$fix.pars,
                                               keepdata = FALSE, optim.method = "L-BFGS-B",
                                               lower = options$lowerBound, upper = options$upperBound), silent = TRUE)
  
  if(isTryError(results)){
    results$fitdist <- try(MASS::fitdistr(x = variable, densfun = options$pdfFun, start = starts, 
                                          lower = options$lowerBound, upper = options$upperBound), silent = TRUE)
  }
  
  if(isTryError(results)){
    results$fitdist <- try(fitdistrplus::fitdist(data = variable, distr = distName, method = "mle", 
                                                 start = starts, fix.arg = options$fix.pars,
                                                 keepdata = FALSE), silent = TRUE)
  } else{
    results$fitdist$convergence <- 0
  }
  
  if(isTryError(results)){
    mleContainer$setError(.ldAllTextsList()$feedback$fitdistrError)
    return()
  } 
  
  if(is.na(results$fitdist$vcov)){
    mleContainer$setError(.ldAllTextsList()$feedback$vcovNA)
    #mleContainer[['estParametersTable']]$addFootnote(.ldAllTextsList()$feedback$vcovNA)
    results$fitdist$vcov <- diag(length(results$fitdist$estimate))
    
    include.se <- FALSE
  } else{
    include.se <- TRUE
  }
  
  results$structured <- .ldStructureResults(results$fitdist, options, include.se)
  
  mleContainer[['mleResults']] <- createJaspState(object = results, dependencies = c(options$parValNames, "ciIntervalInterval", "parametrization"))
  
  return(results)
}

.ldStructureResults <- function(fit, options, include.se){
  if(is.null(fit)) return()
  
  res <- sapply(options$transformations,
                function(tr) car::deltaMethod(fit$estimate, tr, fit$vcov, level = options$ciIntervalInterval))
  rownames(res) <- c("estimate", "se", "lower", "upper")
  res <- t(res)
  res <- cbind(par = rownames(res), res)
  res <- as.data.frame(res)
  
  if(!include.se){
    res$se <- res$lower <- res$upper <- NA
  }
  return(res)
}
#### Fit assessment ----
.ldFitStatisticsTable <- function(fitContainer, options, method){
  if(!is.null(fitContainer[['fitStatisticsTable']])) return()
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk", "chiSquare")
  optionsTests <- allTests %in% names(options)
  whichTests <- unlist(options[allTests[optionsTests]])
  
  if(all(!whichTests)) return()
  
  tab <- createJaspTable(title = gettext("Fit Statistics"))
  tab$position <- 1
  tab$dependOn(c(method, allTests[optionsTests]))
  tab$setExpectedSize(rows = sum(whichTests))
  
  
  tab$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  tab$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  tab$addColumnInfo(name = "p.value",   title = gettext("p"),         type = "pvalue")
  
  tab$addCitation(.ldAllTextsList()$references$goftest)
  
  fitContainer[['fitStatisticsTable']] <- tab
  
  return(tab)  
}

.ldFitStatisticsResults <- function(fitContainer, fit, variable, options, ready){
  if(!ready || fitContainer$getError()) return()
  if(is.null(fit)) return()
  if(!is.null(fitContainer[['fitStatisticsResults']])) return(fitContainer[['fitStatisticsResults']]$object)
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk", "chiSquare")
  tests <- allTests[allTests %in% names(options)]
  
  res <- data.frame(test = tests, statistic = numeric(length = length(tests)), p.value = numeric(length = length(tests)))
  
  pars <- c(as.list(fit$estimate), options$fix.pars)
  
  for(test in tests){
    arg <- switch (test,
                   "kolmogorovSmirnov" = c(list(x = variable, y = options$cdfFun), pars),
                   "shapiroWilk" = list(x = variable),
                   "chiSquare" = list(x = as.numeric(table(variable)), 
                                      p = do.call(options[['pdfFun']],
                                                  utils::modifyList(pars,
                                                                    list(x = as.numeric(names(table(variable))))
                                                  )
                                      ),
                                      rescale.p = TRUE),
                   c(list(x = variable, null = options$cdfFun), pars)
    )
    fun <- switch (test,
                   "kolmogorovSmirnov" = ks.test,
                   "cramerVonMisses"   = goftest::cvm.test,
                   "andersonDarling"   = goftest::ad.test,
                   "shapiroWilk"       = shapiro.test,
                   "chiSquare"         = chisq.test
    )
    compute <- do.call(fun, arg)
    res[res$test == test, "statistic"] <- as.numeric(compute$statistic)
    res[res$test == test, "p.value"]   <- as.numeric(compute$p.value)
  }
  
  fitContainer[['fitStatisticsResults']] <- createJaspState(object = res)
  
  return(res)
}

.ldFillFitStatisticsTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()
  
  
  allTests <- c("kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "shapiroWilk", "chiSquare")
  tests <- allTests[allTests %in% names(options)]
  testNames <- c(gettext("Kolmogorov-Smirnov"),
                 gettext("Cramér-von Mises"),
                 gettext("Anderson-Darling"),
                 gettext("Shapiro-Wilk"),
                 gettext("Chi-square"))[allTests %in% names(options)]
  
  whichTests <- unlist(options[tests])
  
  results$test <- testNames
  res <- results[whichTests,]
  
  table$setData(res)
  
  return()
}
### Fill plots----
.ldFitPlots <- function(fitContainer, estimates, options, variable, ready){
  estimates <- c(estimates, options$fix.pars)
  if(is.null(fitContainer[['estPDF']]) && isTRUE(options$estPDF)){
    pdfplot <- createJaspPlot(title = gettext("Histogram vs. Theoretical PDF"))
    pdfplot$dependOn(c("estPDF"))
    pdfplot$position <- 2
    fitContainer[['estPDF']] <- pdfplot
    
    if(ready && !fitContainer$getError())
      .ldFillEstPDFPlot(pdfplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['estPMF']]) && isTRUE(options$estPMF)){
    pmfplot <- createJaspPlot(title = gettext("Histogram vs. Theoretical PMF"))
    pmfplot$dependOn(c("estPMF"))
    pmfplot$position <- 2
    fitContainer[['estPMF']] <- pmfplot
    
    if(ready && !fitContainer$getError())
      .ldFillEstPMFPlot(pmfplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['qqplot']]) && options$qqplot){
    qqplot <- createJaspPlot(title = gettext("Q-Q plot"))
    qqplot$dependOn(c("qqplot"))
    qqplot$position <- 3
    fitContainer[['qqplot']] <- qqplot
    
    if(ready && !fitContainer$getError())
      .ldFillQQPlot(qqplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['estCDF']]) && options$estCDF){
    cdfplot <- createJaspPlot(title = gettext("Empirical vs. Theoretical CDF"))
    cdfplot$dependOn(c("estCDF"))
    cdfplot$position <- 4
    fitContainer[['estCDF']] <- cdfplot
    
    if(ready && !fitContainer$getError())
      .ldFillEstCDFPlot(cdfplot, estimates, options, variable)
  }
  
  if(is.null(fitContainer[['ppplot']]) && options$ppplot){
    ppplot <- createJaspPlot(title = gettext("P-P plot"))
    ppplot$dependOn(c("ppplot"))
    ppplot$position <-5
    fitContainer[['ppplot']] <- ppplot
    
    if(ready && !fitContainer$getError())
      .ldFillPPPlot(ppplot, estimates, options, variable)
  }
  
  return()
}

.ldFillQQPlot <- function(qqplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(sample = variable)) +
    ggplot2::stat_qq(distribution = options[['qFun']], dparams = estParameters, shape = 21, fill = "grey", size = 3) +
    ggplot2::stat_qq_line(distribution = options[['qFun']], dparams = estParameters) +
    ggplot2::xlab(gettext("Theoretical")) + ggplot2::ylab(gettext("Sample")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(c(0.1,0.15), 0))
  
  p <- JASPgraphs::themeJasp(p)
  
  qqplot$plotObject <- p
  
  return()
}

.ldFillEstPDFPlot <- function(pdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = "grey", col = "black") +
    ggplot2::stat_function(fun = options[['pdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range(variable),
                                breaks = pretty(range(variable))) +
    ggplot2::ylab(gettext("Density")) + ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  pdfplot$plotObject <- p
  
  return()
}

.ldFillEstPMFPlot <- function(pmfplot, estimates, options, variable){
  range <- range(variable)
  
  mids <- range[1]:range[2]
  counts <- sapply(mids, function(i) sum(variable == i))
  dat  <- data.frame(counts = counts, mids = mids, pmf = do.call(options$pdfFun, c(list(x = mids), estimates)))
  
  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = mids, y = counts/sum(counts))) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black") +
    JASPgraphs::geom_point(ggplot2::aes(x = mids, y = pmf)) +
    ggplot2::scale_x_continuous(limits = range + c(-0.5, 0.5), 
                                expand = c(0.1, 0.1),
                                breaks = dat$mids) + 
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(gettext("Probability Mass"))
  
  p <- JASPgraphs::themeJasp(p)
  
  pmfplot$plotObject <- p
  
  return()
}

.ldFillPPPlot <- function(ppplot, estParameters, options, variable){
  n <- length(variable)
  ObservedProp <- (1:n)/n - 0.5/n
  
  args <- as.list(estParameters)
  args[['q']] <- variable
  TheoreticalProp <- sort(do.call(options[['cdfFun']], args))
  
  p <- ggplot2::ggplot(data = NULL) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    JASPgraphs::geom_point(ggplot2::aes(x = TheoreticalProp, y = ObservedProp)) +
    ggplot2::xlab(gettext("Theoretical")) + ggplot2::ylab(gettext("Sample")) +
    ggplot2::scale_x_continuous(limits = 0:1, expand = ggplot2::expand_scale(mult = 0, add = c(0.05, 0.1))) + 
    ggplot2::scale_y_continuous(limits = 0:1, expand = ggplot2::expand_scale(mult = 0, add = c(0.05, 0.1)))
  
  p <- JASPgraphs::themeJasp(p)
  
  ppplot$plotObject <- p 
  
  return()
}

.ldFillEstCDFPlot <- function(cdfplot, estParameters, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_rug() +
    ggplot2::stat_function(fun = options[['cdfFun']], args = as.list(estParameters), size = 1.5) + 
    ggplot2::scale_x_continuous(limits = range(variable), breaks = pretty(range(variable))) +
    ggplot2::scale_y_continuous(limits = 0:1) + 
    ggplot2::ylab(substitute(p~(X <= x), list(p = gettext("Probability")))) +
    ggplot2::xlab(options[['variable']])
  
  p <- JASPgraphs::themeJasp(p)
  
  cdfplot$plotObject <- p
  
  return()
}

.ldGetPlotContainer <- function(jaspResults, options, name, title, position){
  if(!is.null(jaspResults[[name]])){
    plotsContainer <- jaspResults[[name]]
  } else{
    plotsContainer <- createJaspContainer(title = gettext(title))
    plotsContainer$position <- position
    
    if("parametrization" %in% names(options)){
      plotsContainer$dependOn(c(options$parValNames, "parametrization"))
    } else{
      plotsContainer$dependOn(c(options$parValNames))
    }
    
    jaspResults[[name]] <- plotsContainer
  }
  
  return(plotsContainer)
}

.ldFormulaPlot <- function(container, options, formulaText = NULL, depend = NULL){
  if(!options$formulas) return()
  if(!is.null(container[['formula']])) return()  
  
  formula <- createJaspHtml(title = gettext("Formula"), elementType = "h1")
  formula$position <- 3
  formula$dependOn(c("formulas", depend))
  
  if(is.function(formulaText)){
    formula[['text']] <- formulaText(options)
  } else if(is.character(formulaText)){
    formula[['text']] <- formulaText
  }
  
  container[['formula']] <- formula
}

### Plot distributions ----
### Plot PDF ----
.ldFillPDFContainer <- function(pdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotPDF) return()
  
  .ldExplanationPDF(pdfContainer, options, explanationText)
  .ldPlotPDF(pdfContainer, options)
  .ldFormulaPlot(pdfContainer, options, formulaText, "plotPDF")
  
  return()
}

.ldExplanationPDF <- function(pdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(pdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$pdf
  }
  
  explanation[['text']] <- explanationText
  pdfContainer[['explanation']] <- explanation
  
}

.ldPlotPDF <- function(pdfContainer, options){
  if(!is.null(pdfContainer[['pdfPlot']])) return()
  
  pdfPlot <- createJaspPlot(title = gettext("Density Plot"), width = 600, height = 320)
  pdfPlot$position <- 2 # after explanation, before formula
  pdfPlot$dependOn(c('plotPDF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  pdfContainer[['pdfPlot']] <- pdfPlot
  
  .ldFillPlotPDF(pdfPlot, options)
  
  return()
}

.ldFillPlotPDF <- function(pdfPlot, options){
  # basic density curve
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1.25)
  
  # highlight probability
  if(options$highlightProbability){
    # determine plotting region
    args <- options[['pars']]
    argsPDF <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['q']] <- c(-Inf, options[['highlightmax']])
    } else if(options[['highlightType']] == "upper"){
      args[['q']] <- c(options[['highlightmin']], Inf)
    }
    
    # calculate value under the curve
    cdfValue <- do.call(options[['cdfFun']], args)
    cdfValue <- cdfValue[2] - cdfValue[1]
    
    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    if(c(0, 1) %in% cdfValueRound){
      cdfValueRound <- round(cdfValue, 3)
    }
    
    # calculate position of the geom_text
    args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    argsPDF[['x']] <- seq(args[['q']][1], args[['q']][2], length.out = 20)
    x <- weighted.mean(argsPDF[['x']], do.call(options[['pdfFun']], argsPDF))
    argsPDF[['x']] <- x
    y <- do.call(options[['pdfFun']], argsPDF)/3
    
    plot <- plot + 
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], geom = "area", 
                             xlim = args[['q']], fill = "steelblue") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
  }
  
  # highlight density
  if(options$highlightDensity){
    # determine plotting region
    args <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['x']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['x']] <- options[['highlightmax']]
    } else if(options[['highlightType']] == "upper"){
      args[['x']] <- options[['highlightmin']]
    }
    
    
    pdfValue <- do.call(options[['pdfFun']], args)
    
    segment_data <- data.frame(x = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               xend = args[['x']], y = pdfValue)
    
    # plot density
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data, 
                            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = data.frame(x = options[['range_x']][1], y = pdfValue, label = round(pdfValue, 2)),
                         ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['x']], ymin = 0, ymax = pdfValue, linetype = 2) +
      JASPgraphs::geom_point(x = args[['x']], y = pdfValue, size = 5)
  }
  
  plot <- plot + ggplot2::ylab(gettext("Density")) + 
    ggplot2::scale_x_continuous(limits = options[['range_x']], breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']]))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  pdfPlot[['plotObject']] <- plot
}

### Plot CDF ----
.ldFillCDFContainer <- function(cdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotCDF) return()
  
  .ldExplanationCDF(cdfContainer, options, explanationText)
  .ldPlotCDF(cdfContainer, options)
  .ldFormulaPlot(cdfContainer, options, formulaText, "plotCDF")
}

.ldExplanationCDF <- function(cdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(cdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  cdfContainer[['explanation']] <- explanation
}

.ldPlotCDF <- function(cdfContainer, options){
  if(!is.null(cdfContainer[['cdfPlot']])) return()
  
  cdfPlot <- createJaspPlot(title = gettext("Cumulative Probability Plot"), width = 600, height = 320)
  cdfPlot$position <- 2 # after explanation, before formula
  cdfPlot$dependOn(c('plotCDF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  cdfContainer[['cdfPlot']] <- cdfPlot
  
  .ldFillPlotCDF(cdfPlot, options)
  
  return()
}

.ldFillPlotCDF <- function(cdfPlot, options){
  
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['cdfFun']], n = 101, args = options[['pars']], size = 1.25)
  
  
  args <- options[['pars']]
  if(options[['highlightType']] == "minmax"){
    args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
  } else if(options[['highlightType']] == "lower"){
    args[['q']] <- options[['highlightmax']]
  } else if(options[['highlightType']] == "upper"){
    args[['q']] <- options[['highlightmin']]
  }
  
  cdfValue <- do.call(options[['cdfFun']], args)
  point_data <- data.frame(x = args[['q']], y = cdfValue, col = as.factor(seq_along(args[['q']])))
  
  if(options$highlightProbability){ 
    # round value for plotting as text
    cdfValueRound <- round(cdfValue, 2)
    
    segment_data <- data.frame(xoffset = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               x = options[['range_x']][1], xend = args[['q']], y = cdfValue, label = cdfValueRound)
    
    
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xoffset, xend = xend, y = y, yend = y), linetype = 2) +
      ggplot2::geom_text(data = segment_data, ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['q']], ymin = 0, ymax = cdfValue, linetype = 2) + 
      JASPgraphs::geom_point(data = point_data, ggplot2::aes(x = x, y = y), size = 5)
  }
  
  if(options$highlightDensity){
    # determine plotting region
    pdfArgs <- args
    pdfArgs[['x']] <- pdfArgs[['q']]
    pdfArgs[['q']] <- NULL
    
    pdfValue <- do.call(options[['pdfFun']], pdfArgs)
    intercept <- cdfValue - args[['q']]*pdfValue
    slopeText <-  round(pdfValue, 2)
    
    line_data <- data.frame(slope = pdfValue, intercept = intercept, col = as.factor(1:length(pdfArgs[['x']])))
    
    plot <- plot + 
      ggplot2::geom_abline(data = line_data, ggplot2::aes(slope = slope, intercept = intercept, col = col), size = 1) +
      JASPgraphs::geom_point (data = point_data, ggplot2::aes(x = x, y = y, col = col), size = 5) + 
      JASPgraphs::scale_JASPcolor_discrete(name = gettext("Slope"), labels = as.character(slopeText))
  }
  
  
  plot <- plot + 
    ggplot2::ylab(substitute(p~(X <= x), list(p = gettext("Probability")))) +
    ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']])) +
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  plot <- JASPgraphs::themeJasp(plot, legend.position = c(0.85, 0.4))
  
  cdfPlot[['plotObject']] <- plot
}

### Plot QF ----
.ldFillQFContainer <- function(qfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotQF) return()
  
  .ldExplanationQF(qfContainer, options, explanationText)
  .ldPlotQF(qfContainer, options)
  .ldFormulaPlot(qfContainer, options, formulaText, "plotQF")
}

.ldExplanationQF <- function(qfContainer, options, explanationText){
  if(!options$explanatoryText) return()
  if(!is.null(qfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotQF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  qfContainer[['explanation']] <- explanation
}

.ldPlotQF <- function(qfContainer, options){
  if(!is.null(qfContainer[['qfPlot']])) return()
  
  qfPlot <- createJaspPlot(title = gettext("Quantile Plot"), width = 600, height = 320)
  qfPlot$position <- 2 # after explanation, before formula
  qfPlot$dependOn(c('plotQF', 'range'))
  qfContainer[['qfPlot']] <- qfPlot
  
  .ldFillPlotQF(qfPlot, options)
  
  return()
  
}

.ldFillPlotQF <- function(qfPlot, options){
  args <- options[['pars']]
  args[['q']] <- options[['range_x']]
  prange <- do.call(options[['cdfFun']], args)
  args[['q']] <- NULL
  
  plot <- ggplot2::ggplot(data = data.frame(x = prange), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['qFun']], n = 151, args = args, size = 1.25)  +
    ggplot2::ylab("x") + ggplot2::xlab(substitute(p~(X <= x), list(p = gettext("Probability")))) +
    ggplot2::scale_x_continuous(limits = 0:1) +
    ggplot2::scale_y_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']]))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  qfPlot[['plotObject']] <- plot
}

### Plot PMF ----
.ldFillPMFContainer <- function(pmfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotPMF) return()
  
  .ldExplanationPMF(pmfContainer, options, explanationText)
  .ldPlotPMF(pmfContainer, options)
  .ldFormulaPlot(pmfContainer, options, formulaText, "plotPMF")
  
  return()
}

.ldExplanationPMF <- function(pmfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(pmfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPMF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$pmf
  }
  
  explanation[['text']] <- explanationText
  pmfContainer[['explanation']] <- explanation
  
}

.ldPlotPMF <- function(pmfContainer, options){
  if(!is.null(pmfContainer[['pmfPlot']])) return()
  
  pmfPlot <- createJaspPlot(title = gettext("Probability Mass Plot"), width = 600, height = 320)
  pmfPlot$position <- 2 # after explanation, before formula
  pmfPlot$dependOn(c('plotPMF', 
                     'min_x', 'max_x',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max'))
  pmfContainer[['pmfPlot']] <- pmfPlot
  
  .ldFillPlotPMF(pmfPlot, options)
  
  return()
}

.ldFillPlotPMF <- function(pmfPlot, options){
  args <- options[['pars']]
  args[['x']] <- options[['range_x']][1]:options[['range_x']][2]
  
  # make a room next to the y-axis
  xlim <- options[['range_x']] + c(-0.1, 0.1) * diff(options[['range_x']]) + c(-0.8, 0.8)
  
  # if(diff(options[['range_x']]) <= 2){
  #   xlim[1] <- floor(xlim[1]) - 1
  #   xlim[2] <- ceiling(xlim[2]) + 1
  # }
  dat <- data.frame(x = args[['x']], y = do.call(options[['pdfFun']], args))
  
  # basic plot
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black", width = 0.8)
  
  # highlight probability
  if(options$highlightProbability){
    # determine plotting region
    args[['x']] <- options[['highlightmin']]:options[['highlightmax']]
    datHigh <- data.frame(x = args[['x']], pmf = do.call(options[['pdfFun']], args))
    
    # calculate value under the curve
    cdfValue <- sum(datHigh$pmf)
    
    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    if(c(0, 1) %in% cdfValueRound){
      cdfValueRound <- round(cdfValue, 3)
    }
    
    # calculate position of the geom_text
    datShown <- datHigh[datHigh$x %in% dat$x, ]
    if(ncol(datShown) > 0) {
      x <- datShown$x[which.max(datShown$pmf)]
      y <- datShown$pmf[which.max(datShown$pmf)] + 0.1 * max(dat$y)
    } else{ # the entire highlight region is outside of displayed range
      x <- NA
      y <- NA
      cdfValueRound <- NA
    }
    
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = x, y = pmf), 
                        data = datShown, width = 0.8,
                        fill = "steelblue", stat = "identity") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
  }
  
  # highlight density
  if(options$highlightDensity){
    # determine plotting region
    args <- options[['pars']]
    args[['x']] <- c(options[['highlightmin']], options[['highlightmax']])
    
    segment_data <- subset(dat, x %in% args[['x']])
    segment_data$xend <- segment_data$x
    segment_data$x    <- xlim[1] + 0.05 * diff(options[['range_x']])
    segment_data$xseg <- xlim[1] + 0.1 * diff(options[['range_x']])
    segment_data$label <- round(segment_data$y, 2)
    
    # make 10% margin to write values along axis
    #xlim <- xlim + c(-0.1, 0) * diff(options[['range_x']]) 
    # plot density
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = xend, y = y), stat = "identity",
                        data = segment_data, 
                        alpha = 0, colour = "black", size = 1.5, width = 0.8) +
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xseg, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = segment_data,
                         ggplot2::aes(x = x, y = y, label = label), size = 6)
  }
  
  breaks <- JASPgraphs::getPrettyAxisBreaks(options[['range_x']])
  # display only pretty integers
  breaks <- breaks[breaks %% 1 == 0]
  plot <- plot + 
    ggplot2::ylab(substitute(p~(X == x), list(p = gettext("Probability")))) + 
    ggplot2::scale_x_continuous(limits = xlim,
                                breaks = breaks,
                                labels = breaks,
                                expand = c(0, 0))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  pmfPlot[['plotObject']] <- plot
}

### Plot CMF ----
.ldFillCMFContainer <- function(cmfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotCMF) return()
  
  .ldExplanationCMF(cmfContainer, options, explanationText)
  .ldPlotCMF(cmfContainer, options)
  .ldFormulaPlot(cmfContainer, options, formulaText, "plotCMF")
}

.ldExplanationCMF <- function(cmfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(cmfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCMF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  cmfContainer[['explanation']] <- explanation
}

.ldPlotCMF <- function(cmfContainer, options){
  if(!is.null(cmfContainer[['cmfPlot']])) return()
  
  cmfPlot <- createJaspPlot(title = gettext("Cumulative Probability Plot"), width = 600, height = 320)
  cmfPlot$position <- 2 # after explanation, before formula
  cmfPlot$dependOn(c('plotCMF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  cmfContainer[['cmfPlot']] <- cmfPlot
  
  .ldFillPlotCMF(cmfPlot, options)
  
  return()
}

.ldFillPlotCMF <- function(cmfPlot, options){
  args <- options[['pars']]
  args[['q']] <- options[['range_x']][1]:options[['range_x']][2]
  
  dat <- data.frame(x = args[['q']], y = do.call(options[['cdfFun']], args))
  
  # make a room next to the y-axis
  xlim <- options[['range_x']] + c(-0.1, 0.1) * diff(options[['range_x']]) + c(-0.8, 0.8)
  
  # basic plot
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black", width = 0.8)
  
  
  # determine plotting region
  args <- options[['pars']]
  args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
  
  pdfArgs <- args
  pdfArgs[['x']] <- pdfArgs[['q']]
  pdfArgs[['q']] <- NULL
  
  pdfValue <- do.call(options[['pdfFun']], pdfArgs)
  cmfValue <- do.call(options[['cdfFun']], args)
  
  pdfValueRound <-  round(pdfValue, 2)
  cmfValueRound <- round(cmfValue, 2)
  
  # is there anything to highlight on the plot? (i.e., is the highlighted region inside the range of x)
  highlights <- any(args[['q']] %in% dat$x)
  
  if(options$highlightDensity && highlights){
    # redraw the bars with tip colored highlighting the increment at the selected x
    datDens <- dat
    datDens$col <- "grey"
    if(options$highlightmin %in% datDens$x) {
      datDens[dat$x == options$highlightmin, "y"] <- cmfValue[1] - pdfValue[1]
      datDens <- rbind(datDens, data.frame(x = options$highlightmin, y = pdfValue[1], col = "blue"))
    }
    if(options$highlightmax %in% datDens$x){
      datDens[datDens$x == options$highlightmax, "y"] <- cmfValue[2] - pdfValue[2]
      datDens <- rbind(datDens, data.frame(x = options$highlightmax, y = pdfValue[2], col = "blue"))
    }
    datDens$col <- factor(datDens$col, levels = c("blue","grey"))
    segment_data <- data.frame(xseg = args[['q']], 
                               xend = xlim[2] - 0.1*diff(options[['range_x']]),
                               x    = xlim[2] - 0.05*diff(options[['range_x']]),
                               ymin = cmfValue - pdfValue, ymax = cmfValue, ymid = cmfValue - 0.5*pdfValue,
                               labelDensity = pdfValueRound)
    
    plot <- plot +
      ggplot2::geom_bar(data = datDens, ggplot2::aes(x = x, y = y, fill = col), width = 0.8, stat = "identity", color = "black") +
      ggplot2::geom_segment(data = segment_data, ggplot2::aes(x = xseg, xend = xend, y = ymid, yend = ymid), linetype = 2) + 
      ggplot2::geom_text(data = segment_data, ggplot2::aes(x = x, y = ymid, label = labelDensity), size = 6) +
      ggplot2::scale_fill_manual(values = c("steelblue", "grey"), guide = FALSE)
  }
  
  if(options$highlightProbability && highlights){
    segment_data <- subset(dat, x %in% args[['q']])
    segment_data$xend <- segment_data$x
    
    segment_data$x    <- xlim[1] + 0.05 * diff(options[['range_x']])
    segment_data$xseg <- xlim[1] + 0.1 * diff(options[['range_x']])
    segment_data$label <- round(segment_data$y, 2)
    
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = xend, y = y), stat = "identity",
                        data = segment_data, 
                        alpha = 0, colour = "black", size = 1.5, width = 0.8) +
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xseg, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = segment_data,
                         ggplot2::aes(x = x, y = y, label = label), size = 6)
  }
  
  breaks <- JASPgraphs::getPrettyAxisBreaks(options[['range_x']])
  # display only pretty integers
  breaks <- breaks[breaks %% 1 == 0]
  plot <- plot + 
    ggplot2::ylab(substitute(p~(X <= x), list(p = gettext("Probability")))) + 
    ggplot2::scale_x_continuous(limits = xlim,
                                breaks = breaks,
                                labels = breaks,
                                expand = c(0, 0)) + 
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  cmfPlot[['plotObject']] <- plot
}

#### Plot empirical ----
.ldPlotHistogram <- function(dataContainer, variable, options, ready, as = "scale"){
  if(!options$histogram) return()
  if(!is.null(dataContainer[['histogram']])) return()
  
  title <- switch(as, scale = gettext("Histogram"), gettext("Bar plot"))
  histPlot <- createJaspPlot(title = title, width = 500, height = 320)
  
  if(as != "scale"){
    histPlot$dependOn(c("histogram"))
  } else{
    histPlot$dependOn(c("histogramBins", "histogram"))
  }
  histPlot$position <- 3
  
  dataContainer[['histogram']] <- histPlot
  
  if(!ready) return()
  
  .ldFillPlotHistogram(histPlot, options, variable, as)
  
}

.ldFillPlotHistogram <- function(histPlot, options, variable, as = "scale"){
  if(as == "scale"){
    range <- range(variable)
    histData <- hist(variable, 
                     breaks = seq(range[1], range[2], length.out = options[['histogramBins']]+1), 
                     plot = FALSE)
    dat <- data.frame(counts = histData$counts, density = histData$density, mids = histData$mids)
  } else if(as == "discrete"){
    range <- range(variable)
    mids <- range[1]:range[2]
    counts <- sapply(mids, function(i) sum(variable == i))
    dat  <- data.frame(counts = counts, mids = mids)
  } else if(as == "factor"){
    levs <- levels(variable)
    mids <- seq_along(levs)
    range <- range(mids)
    counts <- sapply(levs, function(i) sum(variable == i))
    dat <- data.frame(counts = counts, mids = mids, labs = levs)
  }
  
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = mids, y = counts/sum(counts))) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black") +
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(gettextf("Rel. Freq (%s in bin)", options[['variable']]))
  
  if(as == "scale"){
    plot <- plot + ggplot2::scale_x_continuous(limits = range, 
                                               expand = c(0.05, 0),
                                               breaks = JASPgraphs::getPrettyAxisBreaks(range))
  } else if (as == "discrete"){
    breaks <- pretty(range)
    breaks <- breaks[breaks %% 1 == 0]
    plot <- plot + ggplot2::scale_x_continuous(limits = range + c(-1, 1),
                                               breaks = breaks,
                                               labels = breaks,
                                               expand = c(0, 0))
  } else if (as == "factor"){
    plot <- plot + ggplot2::scale_x_continuous(limits = range + c(-1, 1),
                                               labels = dat$labs,
                                               breaks = dat$mids,
                                               expand = c(0, 0))
  }
  
  plot <- JASPgraphs::themeJasp(plot)
  histPlot[['plotObject']] <- plot
}

.ldPlotECDF <- function(dataContainer, variable, options, ready){
  if(!options[['ecdf']]) return()
  if(!is.null(dataContainer[['ecdf']])) return()
  
  ecdfPlot <- createJaspPlot(title = gettext("Empirical Cumulative Distribution"), width = 500, height = 320)
  
  ecdfPlot$dependOn(c("ecdf"))
  ecdfPlot$position <- 4
  
  dataContainer[['ecdf']] <- ecdfPlot
  
  if(!ready) return()
  
  .ldFillPlotECDF(ecdfPlot, options, variable)
}

.ldFillPlotECDF <- function(plot, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step", size = 1.5) +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range(variable)*1.1) +
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(substitute(f~(v == x), list(f = gettext("Freq"), v = options[['variable']])))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  
}

#### Texts ----
.ldParsSupportMoments <- function(pars, support, moments){
  formulas <- createJaspHtml(title = gettext("Parameters, Support, and Moments"))
  formulas$dependOn(c("parsSupportMoments", "parametrization"))
  formulas$position <- 2
  
  if(!is.na(moments)){
    text <- gettextf(
      "<b>Parameters</b>
      %s
      
      <b>Support</b>
      %s
      
      <b>Moments</b>
      E(X) = %s
      Var(X) = %s", paste(pars, collapse = " \n "), support, moments[[1]], moments[[2]])
  } else{
    text <- gettextf(
      "<b>Parameters</b>
      %s
      
      <b>Support</b>
      %s
      
      <b>Moments</b>
      not available", paste(pars, collapse = " \n "), support)
  }
  formulas$text <- text
  
  return(formulas)
}

.ldIntroText <- function(jaspResults, options, introText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(jaspResults[['introText']])) return()
  
  intro <- createJaspHtml()
  intro$dependOn(c("explanatoryText"))
  intro$position <- 1
  
  if(is.function(introText)){
    intro[['text']] <- gettext(introText())
  } else if(is.character(introText)){
    intro[['text']] <- gettextf(.ldAllTextsList(distributionName=introText)$explanations$intro,
                                introText, introText, introText, introText, introText)
  }
  
  jaspResults[['introText']] <- intro
  
  return()  
}

.ldAllTextsList <- function(distributionName="..."){
  list(
    explanations = list(
      pdf = gettext("The probability density function (PDF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the relative likelihood that a realization of the random variable X yields a value equal to x. 
    More formally, the PDF is defined as a derivative of a cumulative distribution function (CDF).
    
    The density plot displays the probability density function of a random variable.
    The <i>y</i>-axis displays the value of the density function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),
      
      pmf = gettext("The probability mass function (PMF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the probability that a realization of the random variable X yields a value equal to x.
    
    The probability mass plot displays the probability mass function of a random variable.
    The <i>y</i>-axis displays the value of the probability mass function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),
      
      cdf = gettext("The cumulative distribution function (CDF), usually denoted as F(x), is a function of a random variable X.
    The value of F(x) provides the probability that a realization of the random variable X yields a value that is equal to or smaller than x.
    
    The cumulative probability plot displays the cumulative distribution of a random variable.
    The <i>y</i>-axis displays the value of the cumulative distribution function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),
      
      qf  = gettext("The quantile function, usually denoted as Q(p), is the inverse of the cumulative distribution function.
    The function gives the quantile such that the probability of the random variable being less than or equal to that value equals the given probability p.   
    
    The quantile plot displays the quantile function.
    The <i>y</i>-axis displays the quantile of which the probability that the random variable is less or equal to that value is equal to p (displayed on the <i>x</i>-axis)."),

      intro = gettextf("<h3> Demonstration of the %1$s </h3>
This demonstration is divided into four parts.
The first part displays the %2$s, its probability density function, cumulative distribution function, and quantile function.
The second part allows you to generate data from the %3$s, compute descriptive statistics, and display descriptive plots.
The third part allows you to estimate the parameters of the %4$s.
The fourth part allows you to check the fit of the %5$s to the data.

<b>References</b>

Blitzstein, J. K., & Hwang, J. (2014). <i>Introduction to probability.</i> Chapman and Hall/CRC.

Leemis, L. M., & Pasupathy, R. (2019). The ties that bind. <i>Significance, 16</i>(4), 8–9.

For relationships with other distributions, visit www.math.wm.edu/~leemis/chart/UDR/UDR.html.

https://en.wikipedia.org/wiki/List_of_probability_distributions", distributionName, distributionName, distributionName, distributionName, distributionName)
    ),
    references   = list(
      jasp = "JASP Team (2020). JASP (Version 0.12) [Computer software].",
      goftest = "Julian Faraway, George Marsaglia, John Marsaglia and Adrian Baddeley (2017). goftest: Classical Goodness-of-Fit Tests for Univariate Distributions. R package version 1.1-1. https://CRAN.R-project.org/package=goftest",
      fitdistrplus = "Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. URL: http://www.jstatsoft.org/v64/i04/.",
      car = "John Fox and Sanford Weisberg (2011). An R Companion to Applied Regression, Second Edition. Thousand Oaks CA: Sage. URL: http://socserv.socsci.mcmaster.ca/jfox/Books/Companion."
    ),
    feedback = list(
      fitdistrError = gettext("Estimation failed: Optimization did not converge. <ul><li>Try adjusting parameter values, check outliers or feasibility of the distribution fitting the data.</li></ul>"),
      vcovNA = gettext("Estimation failed: Hessian matrix is not numerically computable. <ul><li>Check outliers or feasibility of the distribution fitting the data.</li></ul>")
    )
  )
}
