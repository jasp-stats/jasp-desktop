.simulateData <- function(jaspResults, options, as = "scale"){
  return()
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
  
  if(isFALSE(errors)){
    errors <- !is_integer
  } else if(!is_integer){
    errors[['integer']] <- TRUE
    errors[['message']] <- paste(errors[['message']], gettext("<ul><li>Variable has to be discrete (i.e., integer).</li></ul>"))
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
  
  summaryTable <- createJaspTable(title = gettext("Descriptives"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("summary"))
  
  summaryTable$addColumnInfo(name = "variable",   title = gettext("Variable"),       type = "string", combine = TRUE)
  summaryTable$addColumnInfo(name = "sampleSize", title = gettext("n"),              type = "integer")
  summaryTable$addColumnInfo(name = "mean",       title = gettext("Mean"),           type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "var",        title = gettext("Variance"),       type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "sd",         title = gettext("Std. deviation"), type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "min",        title = gettext("Minimum"),        type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile25", title = gettext("25% Quantile"),   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "median",     title = gettext("Median"),         type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "quantile75", title = gettext("75% Quantile"),   type = "number", format = "sf:4")
  summaryTable$addColumnInfo(name = "max",        title = gettext("Maximum"),        type = "number", format = "sf:4")
  #summaryTable$addColumnInfo(name = "skew",       title = gettext("Skewness"),       type = "number", format = "sf:4")
  #summaryTable$addColumnInfo(name = "kurt",       title = gettext("Kurtosis"),       type = "number", format = "sf:4")
  
  dataContainer[['summary']] <- summaryTable
  
  if(!ready) 
    return()
  
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
  
  summaryTable <- createJaspTable(title = gettext("Descriptives"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("summary"))
  summaryTable$addCitation(.ldAllTextsList()$references$jasp)
  
  summaryTable$addColumnInfo(name = "level", title = "", type = "string")
  summaryTable$addColumnInfo(name = "freq",  title = gettext("n"), type = "integer")
  summaryTable$addColumnInfo(name = "rel.freq",  title = gettext("Rel. Frequency"), type = "number")
  
  summaryTable$setExpectedSize(rows = length(levels(variable)) + 1)
  
  dataContainer[['summary']] <- summaryTable
  
  if(!ready) 
    return()
  
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
.summarySkewness <- function(x) {
  
  # Skewness function as in SPSS (for samlpes spaces):
  # http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  z <- (x - m) / s  # z scores
  a <- n / ((n - 1) * (n - 2))
  
  skewness <- sum(z^3) * a
  
  return(skewness)
}

.summaryKurtosis <- function(x) {
  
  # Kurtosis function as in SPSS:
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
  # http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis
  
  x   <- na.omit(x)
  n   <- length(x)
  s4  <- sum((x - mean(x))^4)
  s2  <- sum((x - mean(x))^2)
  v   <- s2 / (n-1)
  a   <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  b   <- s4 / (v^2)
  c   <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
  
  kurtosis <- a * b + c
  
  return(kurtosis)
}


.computeObservedMoments <- function(x, max.moment = 2, about.mean = FALSE){
  x <- na.omit(x)
  n <- length(x)
  moments <- numeric(length = max.moment)
  moments[1] <- mean(x)
  
  if(max.moment < 2)
    return(moments)
  
  if(about.mean)
    x <- x-moments[1]
  
  
  for(i in 2:max.moment){
    moments[i] <- sum(x^i) / n
  }
  
  return(moments)
}
