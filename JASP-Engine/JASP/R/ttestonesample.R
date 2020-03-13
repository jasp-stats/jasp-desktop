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

TTestOneSample <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$variables) > 0
  type  <- "one-sample"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestOneSampleMainTable(  jaspResults, dataset, options, ready, type)
  .ttestOneSampleNormalTable(jaspResults, dataset, options, ready, type)
  # Descriptives
  vars <- unique(unlist(options$variables))
  .ttestDescriptivesTable(        jaspResults, dataset, options, ready, vars)
  .ttestOneSampleDescriptivesPlot(jaspResults, dataset, options, ready)
  
  return()
}

.ttestOneSampleMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]])) 
    return()
  
  optionsList <- .ttestOptionsList(options, type)
  
  # Create table
  ttest <- createJaspTable(title = gettext("One Sample T-Test"))
  ttest$dependOn(c("effectSize", "effSizeConfidenceIntervalCheckbox", "variables",
                   "effSizeConfidenceIntervalPercent", "students", "mannWhitneyU",
                   "meanDifference", "meanDiffConfidenceIntervalCheckbox", "stddev",
                   "meanDiffConfidenceIntervalPercent", "hypothesis", 
                   "VovkSellkeMPR", "missingValues", "zTest", "testValue"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1
  
  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Wilcoxon signed-rank test."))
    testStat                <- "V"
    testStatName            <- gettext("V")
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat                <- "t"
    testStatName            <- gettext("t")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else if(optionsList$wantsZtest && optionsList$onlyTest){
    ttest$addFootnote(gettext("Z test."))
    testStat                <- "Z"
    testStatName            <- gettext("Z")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else {
    testStat                <- "Statistic"
    testStatName            <- gettext("Statistic")
    nameOfLocationParameter <- gettext("Location Parameter")
    nameOfEffectSize        <- gettext("Effect Size")
  }
  
  ttest$addColumnInfo(name = "v",     type = "string", title = "", combine = TRUE)
  
  ## if the user wants more than one test, add a column called "Test"
  if (sum(optionsList$allTests) > 1) 
    ttest$addColumnInfo(name = "test", type = "string", title = gettext("Test"))
  
  ttest$addColumnInfo(name = testStat, type = "number", title = testStatName)
  
  if (optionsList$wantsStudents)
    ttest$addColumnInfo(name = "df", type = "integer", title = gettext("df"))
  
  ttest$addColumnInfo(name = "p", type = "pvalue", title = gettext("p"))
  
  .ttestVovkSellke(ttest, options)
  
  if (optionsList$wantsStudents && optionsList$wantsZtest)
    testInNote <- gettext("Student t-test and Z-test")
  else if (optionsList$wantsStudents)
    testInNote <- gettext("Student t-test")
  else if (optionsList$wantsZtest)
    testInNote <- gettext("Z-test")
  
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "m", title = nameOfLocationParameter, type = "number")
    
    if (optionsList$wantsStudents || optionsList$wantsZtest || optionsList$wantsWilcox) {
      tzNote <- wNote <- NULL
      
      if (optionsList$wantsStudents || optionsList$wantsZtest)
        tzNote <- gettextf("For the %s, location parameter is given by mean difference <em>d</em>.", testInNote)
      
      if (optionsList$wantsWilcox)
        wNote <- gettext("For the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate.")
      
      ttest$addFootnote(paste(tzNote, wNote))
    }
  }
  
  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }
  
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    
    if (optionsList$wantsStudents || optionsList$wantsWilcox || optionsList$wantsZtest) {
      tNote <- wNote <- zNote <- NULL
      
      if (optionsList$wantsStudents)
        tNote <- gettext("For the Student t-test, effect size is given by Cohen's <em>d</em>.")
      
      if (optionsList$wantsWilcox)
        wNote <- gettext("For the Wilcoxon test, effect size is given by the matched rank biserial correlation.")
      
      if (optionsList$wantsZtest)
        zNote <- gettext("For the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation).")
      
      ttest$addFootnote(paste(tNote, wNote, zNote))
    }
  }
  
  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }
  
  ### check the directionality
  if (options$hypothesis == "greaterThanTestValue") {
    directionFootnote <- gettext("greater than")
    direction <- "greater"
  } else if (options$hypothesis == "lessThanTestValue") {
    directionFootnote <- gettext("less than")
    direction <- "less"
  } else {
    directionFootnote <- gettext("different from")
    direction <- "two.sided"
  }
  
  if ((options$testValue != 0 || options$hypothesis != "notEqualToTestValue") && (optionsList$wantsStudents || optionsList$wantsWilcox || optionsList$wantsZtest)) {
    tMessage <- wMessage <- NULL

    if (optionsList$wantsStudents || optionsList$wantsZtest)
      tMessage <- gettextf("For the %1$s, the alternative hypothesis specifies that the mean is %2$s %3$s.", testInNote, directionFootnote, options$testValue)
    
    if (optionsList$wantsWilcox)
      wMessage <- gettextf("For the Wilcoxon test, the alternative hypothesis specifies that the median is %1$s %2$s.", directionFootnote, options$testValue)
    
    ttest$addFootnote(paste(tMessage, wMessage))
  }
  
  jaspResults[["ttest"]] <- ttest
  
  if (ready) 
    .ttestOneSampleMainFill(ttest, dataset, options, testStat, optionsList)
}

.ttestOneSampleNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTests || !is.null(container[["ttestNormalTable"]])) 
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  ttestNormalTable$position <- 2
  
  ttestNormalTable$addColumnInfo(name = "v", title = "",  type = "string")
  ttestNormalTable$addColumnInfo(name = "W", title = gettext("W"), type = "number")
  ttestNormalTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  
  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  if (ready)
    .ttestOneSampleNormalFill(ttestNormalTable, dataset, options)
}

.ttestOneSampleMainFill <-function(table, dataset, options, testStat, optionsList) {
  variables <- options$variables
  for (variable in variables) {
    
    errors <- .hasErrors(dataset, 
                         message = 'short', 
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = '< 2')
    
    for (test in optionsList$whichTests) {

      row     <- list(v = variable, test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, variable, sep = "-")
      
      errorMessage <- NULL
      if (identical(errors, FALSE)) {
        rowResults <- try(.ttestOneSampleComputeMainTableRow(variable, dataset, test, testStat, optionsList, options))
        
          if (!isTryError(rowResults))
            row <- c(row, rowResults)
          else
            errorMessage <- .extractErrorMessage(rowResults)
          
      } else {
        errorMessage <- errors$message
      }
      
      if (!is.null(errorMessage)) {
        row[[testStat]] <- NaN
        table$addFootnote(errorMessage, colNames = testStat, rowNames = rowName)
      }
      
      table$addRows(row, rowNames = rowName)
    }
  }
}
  
.ttestOneSampleComputeMainTableRow <- function(variable, dataset, test, testStat, optionsList, options) {
  direction <- switch(options$hypothesis,
                      "notEqualToTestValue"  ="two.sided",
                      "greaterThanTestValue" ="greater",
                      "lessThanTestValue"    ="less")
  dat <- na.omit(dataset[[ .v(variable) ]])
  n   <- length(dat)
  if (test == "Wilcoxon") {
    r <- stats::wilcox.test(dat, alternative = direction, mu = options$testValue,
                            conf.level = optionsList$percentConfidenceMeanDiff, conf.int = TRUE)
    df   <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
    nd   <- sum(dat != 0)
    maxw <- (nd * (nd + 1)) / 2
    d    <- as.numeric((r$statistic / maxw) * 2 - 1)
    wSE  <- sqrt((nd * (nd + 1) * (2 * nd + 1)) / 6) /2
    mrSE <- sqrt(wSE^2  * 4 * (1 / maxw^2)) 
    # zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
    zmbiss <- atanh(d)
    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize)/2)*mrSE), 
                               tanh(zmbiss + qnorm((1+optionsList$percentConfidenceEffSize)/2)*mrSE)))
    else if (direction == "less") 
      confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(optionsList$percentConfidenceEffSize)*mrSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize))*mrSE), Inf))
  } else if (test == "Z"){
    r <- BSDA::z.test(dat, alternative = direction, mu = options$testValue, 
                      sigma.x = options$stddev, 
                      conf.level = optionsList$percentConfidenceMeanDiff)
    df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
    d  <- (mean(dat) - options$testValue) / options$stddev
    
    if(direction == "less")
      r$conf.int[1] <- -Inf
    else if(direction == "greater")
      r$conf.int[2] <- Inf
    
    confIntEffSize <- c(0,0)
    if(optionsList$wantsConfidenceEffSize)
      confIntEffSize <- r$conf.int/options$stddev
  } else {
    r <- stats::t.test(dat, alternative = direction, mu = options$testValue, 
                       conf.level = optionsList$percentConfidenceMeanDiff)
    df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
    d  <- (mean(dat) - options$testValue) / sd(dat)
    t  <- as.numeric(r$statistic)
    
    confIntEffSize <- c(0,0)
    
    if (optionsList$wantsConfidenceEffSize) {
      
      ciEffSize  <- options$effSizeConfidenceIntervalPercent
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
      
      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt(n), df = df, alpha.lower = alphaLevel,
                                                     alpha.upper = alphaLevel)[c(1, 3)]
      confIntEffSize <- unlist(confIntEffSize) / sqrt(n)
      
      if (direction == "greater")
        confIntEffSize[2] <- Inf
      else if (direction == "less")
        confIntEffSize[1] <- -Inf
      
      confIntEffSize <- sort(confIntEffSize)
    }
  }
  
  ## same for all tests
  p     <- as.numeric(r$p.value)
  stat  <- as.numeric(r$statistic)
  m     <- as.numeric(r$estimate - r$null.value)
  ciLow <- as.numeric(r$conf.int[1] - r$null.value)
  ciUp  <- as.numeric(r$conf.int[2] - r$null.value)
  ciLowEffSize <- as.numeric(confIntEffSize[1])
  ciUpEffSize  <- as.numeric(confIntEffSize[2])
  
  if (suppressWarnings(is.na(t)))  # do not throw warning when test stat is not 't' 
    stop("data are essentially constant")
  
  result <- list(df = df, p = p, m = m, d = d, 
              lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp, 
              lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize)
  result[[testStat]] <- stat
  
  if (options$VovkSellkeMPR)
    result[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
  
  return(result)
}

.ttestOneSampleNormalFill <- function(table, dataset, options) {
  variables <- options$variables
  for (variable in variables) {
    row <- list(v = variable)
    
    errors <- .hasErrors(dataset, 
                         message = 'short', 
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = c('< 3', '> 5000'))
    
    if (!identical(errors, FALSE)) {
      row[["W"]] <- NaN
      table$addFootnote(errors$message, colNames = "W", rowNames = variable)
    } else {
      data <- na.omit(dataset[[.v(variable)]])
      
      r <- stats::shapiro.test(data)
      row[["W"]] <- as.numeric(r$statistic)
      row[["p"]] <- r$p.value
    }
    
    table$addRows(row, rowNames = variable)
  }
}

.ttestOneSampleDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["plots"]] <- createJaspContainer(gettext("Descriptives Plots"))
  subcontainer <- container[["plots"]]
  container$position <- 5
  for(variable in options$variables) {
    if(!is.null(subcontainer[[variable]]))
      next
    descriptivesPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestOneSampleDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestOneSampleDescriptivesPlotFill <- function(dataset, options, variable) {
  errors <- .hasErrors(dataset, 
                       message = 'short', 
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = c('< 2'))
  if(!identical(errors, FALSE))
    stop(errors$message)
  
  
  base_breaks_y <- function(x, options) {
    
    values <- c(options$testValue, x[, "dependent"] - x[, "ci"],
                x[, "dependent"] + x[, "ci"])
    ci.pos <- c(min(values), max(values))
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(min(b),  options$testValue, max(b))))
  }
  
  dataSubset <- data.frame(dependent = dataset[[.v(variable)]],
                           groupingVariable = rep(variable, length(dataset[[.v(variable)]])))
  
  ci <- options$descriptivesPlotsConfidenceInterval
  summaryStat <- .summarySE(dataSubset, measurevar = "dependent",
                            groupvars = "groupingVariable",
                            conf.interval = ci, na.rm = TRUE, .drop = FALSE)
  testValue <- data.frame(testValue = options$testValue)
  pd <- ggplot2::position_dodge(0.2)
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,  ymax = ciUpper), 
                           colour = "black", width = 0.2, position = pd) + 
    ggplot2::geom_line(position = pd, size = 0.7) +#gives geom_path warning+ 
    ggplot2::geom_point(position = pd, size = 4)  + 
    ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept = testValue), linetype = "dashed") + 
    ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat, options) 
  p <- JASPgraphs::themeJasp(p) + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.ticks.x = ggplot2::element_blank())
  
  return(p)
}
