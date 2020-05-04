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

TTestIndependentSamples <- function(jaspResults, dataset = NULL, options, ...) {
  #at least one variable and one grouping variable
  ready <- length(options$variables) > 0 && options$groupingVariable != ""
  type  <- "independent"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestIndependentMainTable(  jaspResults, dataset, options, ready, type)
  .ttestIndependentNormalTable(jaspResults, dataset, options, ready, type)
  .ttestIndependentEqVarTable( jaspResults, dataset, options, ready, type)
  # Descriptives
  .ttestIndependentDescriptivesTable(jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesPlot( jaspResults, dataset, options, ready)
  
  return()
}

.ttestIndependentMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]])) 
    return()
  
  optionsList <- .ttestOptionsList(options, type)
  
  # Create table
  ttest <- createJaspTable(title = gettext("Independent Samples T-Test"))
  ttest$dependOn(c("effectSize", "effSizeConfidenceIntervalCheckbox", "variables",
                   "descriptivesEffectSizeConfidenceIntervalPercent", "students", "mannWhitneyU",
                   "meanDifference", "meanDiffConfidenceIntervalCheckbox", "stddev",
                   "meanDiffConfidenceIntervalPercent", "hypothesis", 
                   "VovkSellkeMPR", "missingValues", "groupingVariable", "effectSizesType", 
                   "welchs", "mannWhitneyU", "descriptivesMeanDiffConfidenceIntervalPercent"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1
  
  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Mann-Whitney U test."))
    testStat <- "W"
    testStatName <- gettext("W")
  } else if (optionsList$wantsWelchs && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Welch's t-test."))
    testStat <- "t"
    testStatName <- gettext("t")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat     <- "t"
    testStatName <- gettext("t")
  } else {
    testStat     <- "Statistic"
    testStatName <- gettext("Statistic")
  }
  
  dfType <- ifelse(optionsList$wantsWelchs, "number", "integer")
  
  ttest$addColumnInfo(name = "v", title = " ", type = "string", combine = TRUE)
  
  if (sum(optionsList$allTests) >= 2) 
    ttest$addColumnInfo(name = "test", type = "string",  title = gettext("Test"))
  
  ttest$addColumnInfo(name = testStat, type = "number",  title = testStatName)
  ttest$addColumnInfo(name = "df",     type = dfType,    title = gettext("df"))
  ttest$addColumnInfo(name = "p",      type = "pvalue",  title = gettext("p"))
  
  .ttestVovkSellke(ttest, options)
  
  if (options$effectSizesType == "cohensD")
    effSize <- "cohen"
  else if (options$effectSizesType == "glassD")
    effSize <- "glass"
  else if (options$effectSizesType == "hedgesG")
    effSize <- "hedges"
  
  nameOfEffectSizeParametric <- switch(effSize, 
                                       cohen  = gettext("Cohen's d"), 
                                       glass  = gettext("Glass' delta"),
                                       hedges = gettext("Hedges' g"))
  
  if (!optionsList$wantsWilcox) {
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- nameOfEffectSizeParametric
  } else if (optionsList$wantsWilcox && optionsList$onlyTest) {
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs)) {
    nameOfLocationParameter <-  gettext("Location Parameter")
    nameOfEffectSize        <-  gettext("Effect Size")
  }
  
  if (optionsList$wantsStudents && optionsList$wantsWelchs)
    testInNote <- gettext("Student t-test and Welch t-test")
  else if (optionsList$wantsStudents)
    testInNote <- gettext("Student t-test")
  else if (optionsList$wantsWelchs)
    testInNote <- gettext("Welch t-test")
  
  ## add mean difference and standard error difference
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
    
    if (!(optionsList$wantsWilcox && optionsList$onlyTest))  # Only add SE Difference if not only MannWhitney is requested
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")
    
    if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs))
      ttest$addFootnote(gettextf("For the %s, location parameter is given by mean difference. For the Mann-Whitney test, location parameter is given by the Hodges-Lehmann estimate.", testInNote))
  }
  
  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }
  
  ## add Cohen's d
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")

    if (optionsList$wantsWilcox) {
      wNote <- gettext("For the Mann-Whitney test, effect size is given by the rank biserial correlation.")
      
      twNote <- NULL
      if (optionsList$wantsStudents || optionsList$wantsWelchs) 
        twNote <- gettextf("For the %1$s, effect size is given by %2$s.", testInNote, nameOfEffectSizeParametric)
      
      ttest$addFootnote(paste(twNote, wNote))
    }
  }
  
  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }
  
  jaspResults[["ttest"]] <- ttest
  
  if (ready)
    .ttestIndependentMainFill(ttest, dataset, options, testStat, optionsList)
}

.ttestIndependentNormalTable <- function(jaspResults, dataset, options, ready, type) {
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
  
  ttestNormalTable$addColumnInfo(name = "dep", type = "string", title = "", combine = TRUE)
  ttestNormalTable$addColumnInfo(name = "lev", type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = gettext("W"))
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = gettext("p"))
  
  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  if (ready)
    .ttestIndependentNormalFill(ttestNormalTable, dataset, options)
}

.ttestIndependentEqVarTable <- function(jaspResults, dataset, options, ready, type){
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  
  if (!options$equalityOfVariancesTests || !is.null(container[["equalityVariance"]])) 
    return()
  
  # Create table
  equalityVariance <- createJaspTable(title = gettext("Test of Equality of Variances (Levene's)"))
  equalityVariance$showSpecifiedColumnsOnly <- TRUE
  equalityVariance$position <- 3
  equalityVariance$addColumnInfo(name = "variable", type = "string",  title = "")
  equalityVariance$addColumnInfo(name = "F",        type = "number",  title = gettext("F"))
  equalityVariance$addColumnInfo(name = "df",       type = "integer", title = gettext("df"))
  equalityVariance$addColumnInfo(name = "p",        type = "pvalue",  title = gettext("p"))
  
  container[["equalityVariance"]] <- equalityVariance
  
  if (ready)
    .ttestIndependentEqVarFill(equalityVariance, dataset, options)
}

.ttestIndependentMainFill <- function(table, dataset, options, testStat, optionsList) {
  
  if (options$effectSizesType == "cohensD")
    effSize <- "cohen"
  else if (options$effectSizesType == "glassD")
    effSize <- "glass"
  else if (options$effectSizesType == "hedgesG")
    effSize <- "hedges"

  levels <- levels(dataset[[ .v(options$groupingVariable) ]])
  
  if (options$hypothesis == "groupOneGreater" || options$hypothesis == "groupTwoGreater") {
    directionNote <- ifelse(options$hypothesis == "groupOneGreater", gettext("greater"), gettext("less"))
    table$addFootnote(gettextf("For all tests, the alternative hypothesis specifies that group %1$s is %2$s than group %3$s.",
                                                paste("<em>", levels[1], "</em>"), directionNote, paste("<em>", levels[2], "</em>")))
  }
  
  ## for each variable specified, run each test that the user wants
  for (variable in options$variables) {
    errors <- .hasErrors(dataset, 
                         message = 'short', 
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable, 
                         all.grouping = options$groupingVariable,
                         observations.amount = '< 2')
    
    for (test in optionsList$whichTests) {
      
      row     <- list(v = variable, test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, variable, sep = "-")
      
      errorMessage <- NULL
      if (identical(errors, FALSE)) {
        result <- try(ttestIndependentMainTableRow(variable, dataset, test, testStat, effSize, optionsList, options))
        
        if (!isTryError(result))
          row <- c(row, result[["row"]])
        else {
          errorMessage <- .extractErrorMessage(result)

          if (result[["leveneViolated"]])
            table$addFootnote(gettext("Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"), colNames = "p", rowNames = rowName)

        }
        
      } else {
        errorMessage <- errors$message
      }
      
      if (!is.null(errorMessage)) {
        row[[testStat]] <- NaN
        table$addFootnote(errorMessage, colNames = testStat, rowNames = rowName)
      }
      
      table$addRows(row, rowNames = rowName)
    }
    
    if (effSize == "glass") {
      ns  <- tapply(dataset[[.v(variable)]], dataset[[.v(options$groupingVariable)]], function(x) length(na.omit(x)))
      sdMessage <- gettextf("Glass' delta uses the standard deviation of group %1$s of variable %2$s.", names(ns[2]), options$groupingVariable)
      table$addFootnote(sdMessage)
    }
  }
}

ttestIndependentMainTableRow <- function(variable, dataset, test, testStat, effSize, optionsList, options) {
  ciEffSize  <- optionsList$percentConfidenceEffSize
  ciMeanDiff <- optionsList$percentConfidenceMeanDiff
  f <- as.formula(paste(.v(variable), "~",
                        .v(options$groupingVariable)))
  
  variableData <- dataset[[ .v(variable) ]]
  groupingData <- dataset[[ .v(options$groupingVariable) ]]
  
  sds <- tapply(variableData, groupingData, sd, na.rm = TRUE)
  ms  <- tapply(variableData, groupingData, mean, na.rm = TRUE)
  ns  <- tapply(variableData, groupingData, function(x) length(na.omit(x)))
  
  direction <- .ttestMainGetDirection(options$hypothesis)
  
  if (test == "Mann-Whitney") {
    r <- stats::wilcox.test(f, data = dataset,
                            alternative = direction,
                            conf.int = TRUE, conf.level = ciMeanDiff, paired = FALSE)
    df   <- ""
    sed  <- ""
    stat <- as.numeric(r$statistic)
    m    <- as.numeric(r$estimate)
    d    <- abs(as.numeric(1-(2*stat)/(ns[1]*ns[2]))) * sign(m)
    # rankBis <- 1 - (2*stat)/(ns[1]*ns[2])
    wSE <- sqrt((ns[1]*ns[2] * (ns[1]+ns[2] + 1))/12)
    rankBisSE <- sqrt(4 * 1/(ns[1]*ns[2])^2 * wSE^2)
    zRankBis  <- atanh(d)
    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize)/2)*rankBisSE), 
                               tanh(zRankBis + qnorm((1+ciEffSize)/2)*rankBisSE)))
    else if (direction == "less")
      confIntEffSize <- sort(c(-Inf, tanh(zRankBis + qnorm(ciEffSize)*rankBisSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize))*rankBisSE), Inf))
  } else {
    r <- stats::t.test(f, data = dataset, alternative = direction,
                       var.equal = test != "Welch", conf.level = ciMeanDiff, paired = FALSE)
    
    df   <- as.numeric(r$parameter)
    m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
    stat <- as.numeric(r$statistic)
    
    num <-  (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
    sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
    if (test == "Welch")  # Use different SE when using Welch T test!
      sdPooled <- sqrt(((sds[1]^2) + (sds[2]^2)) / 2)
    
    d <- "."
    if (optionsList$wantsEffect) {
      # Sources are https://en.wikipedia.org/wiki/Effect_size for now.
      if (options$effectSizesType == "cohensD")
        d <- as.numeric((ms[1] - ms[2]) / sdPooled)
      else if (options$effectSizesType == "glassD")
        d <- as.numeric((ms[1] - ms[2]) / sds[2])
      # Should give feedback on which data is considered 2.
      else if (options$effectSizesType == "hedgesG") {
        a <- sum(ns) - 2
        logCorrection <- lgamma(a / 2) - (log(sqrt(a / 2)) + lgamma((a - 1) / 2))
        d <- as.numeric((ms[1] - ms[2]) / sdPooled) * exp(logCorrection) # less biased / corrected version
      }
    }
    sed <- (as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])) / stat
    confIntEffSize <- c(0,0)
    
    if (optionsList$wantsConfidenceEffSize){
      # From MBESS package by Ken Kelley, v4.6
      dfEffSize  <-  ifelse(effSize == "glass", ns[2] - 1, df)
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt((prod(ns)) / (sum(ns))), 
                                                     df = dfEffSize, alpha.lower = alphaLevel, 
                                                     alpha.upper = alphaLevel)[c(1, 3)]
      confIntEffSize <- unlist(confIntEffSize) * sqrt((sum(ns)) / (prod(ns)))
      
      if (direction == "greater")
        confIntEffSize[2] <- Inf
      else if (direction == "less")
        confIntEffSize[1] <- -Inf
      
      confIntEffSize <- sort(confIntEffSize)
    }
  }
  ## if the user doesn't want a Welch's t-test or Mann-Whitney,
  ## give a footnote indicating if the equality of variance
  ## assumption is met; seems like in this setting there is no
  ## sampling plan, thus the p-value is not defined. haha!
  leveneViolated <- FALSE
  if (!optionsList$wantsWelchs && !optionsList$wantsWilcox && optionsList$wantsStudents) {
    levene <- car::leveneTest(variableData, groupingData, "mean")
    
    ## arbitrary cut-offs are arbitrary
    if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05)
      leveneViolated <- TRUE
  }
  
  ## same for all t-tests
  p     <- as.numeric(r$p.value)
  ciLow <- r$conf.int[1]
  ciUp  <- r$conf.int[2]
  lowerCIeffectSize <- as.numeric(confIntEffSize[1])
  upperCIeffectSize <- as.numeric(confIntEffSize[2])
  
  # this will be the results object
  row <- list(df = df, p = p, md = m, d = d, 
              lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
              lowerCIeffectSize = lowerCIeffectSize, upperCIeffectSize = upperCIeffectSize,
              sed = sed)
  
  row[[testStat]] <- stat
  
  if (options$VovkSellkeMPR)
    row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
  
  return(list(row = row, leveneViolated = leveneViolated))
}

.ttestIndependentEqVarFill <- function(table, dataset, options) {
  variables <- options$variables
  groups    <- options$groupingVariable
  
  levels <- levels(dataset[[ .v(groups) ]])
  
  for (variable in variables) {
    
    row <- list(variable = variable)
    
    errors <- .hasErrors(dataset, 
                        message = 'short', 
                        type = c('observations', 'variance', 'infinity'),
                        all.target = variable,
                        observations.amount = c('< 3'),
                        all.grouping = groups)
    
    errorMessage <- NULL
    if (identical(errors, FALSE)) {
      result <- try(.ttestIndependentEqVarRow(table, variable, groups, dataset))
      
      if (!isTryError(result))
        row <- c(row, result[["row"]])
      else
        errorMessage <- .extractErrorMessage(result)
      
    } else {
      errorMessage <- errors$message
    }
    
    if (!is.null(errorMessage)) {
      row[["F"]] <- NaN
      table$addFootnote(errors$message, colNames = "F", rowNames = variable)
    }
    
    if (!result[["LeveneComputed"]])
      table$addFootnote(gettext("F-statistic could not be calculated"), colNames = "F", rowNames = variable)
    
    table$addRows(row, rowNames = variable)
  }
}

.ttestIndependentEqVarRow <- function(table, variable, groups, dataset) {
  levene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(groups) ]], "mean")
  
  F  <- levene[1, "F value"]
  df <- levene[1, "Df"]
  p  <- levene[1, "Pr(>F)"]
  
  row <- list(F = F, df = df, p = p)
  
  LeveneComputed <- TRUE
  if (is.na(levene[1, "F value"]))
    LeveneComputed <- FALSE
  
  return(list(row = row, LeveneComputed = LeveneComputed))
}

.ttestIndependentNormalFill <- function(table, dataset, options) {
  ## for a independent t-test, we need to check both group vectors for normality
  variables <- options$variables
  factor    <- options$groupingVariable
  levels    <- levels(dataset[[.v(factor)]])
  
  for (variable in variables) {
    ## there will be two levels, otherwise .hasErrors will quit
    for (level in levels) {

      row     <- list(dep = variable, lev = level, .isNewGroup = (level == levels[1]))
      rowName <- paste(variable, level, sep = "-")
      
      errors <- .hasErrors(dataset, 
                           message = 'short', 
                           type = c('observations', 'variance', 'infinity'),
                           all.target = variable,
                           observations.amount = c('< 3', '> 5000'),
                           all.grouping = factor,
                           all.groupingLevel = level)
      
      if (!identical(errors, FALSE)) {
        row[["W"]] <- NaN
        table$addFootnote(errors$message, colNames = "W", rowNames = rowName)
      } else {
        ## get the dependent variable at a certain factor level
        data <- na.omit(dataset[[.v(variable)]][dataset[[.v(factor)]] == level])
        r <- stats::shapiro.test(data)
        row[["W"]] <- as.numeric(r$statistic)
        row[["p"]] <- r$p.value
      }

      table$addRows(row, rowNames = rowName)
    }
  }
}

.ttestIndependentDescriptivesTable <- function(jaspResults, dataset, options, ready) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]])) 
    return()
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = "Group Descriptives")
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  ttestDescriptivesTable$position <- 4
  ttestDescriptivesTable$addColumnInfo(name = "variable", type = "string",  title = "", combine = TRUE)
  ttestDescriptivesTable$addColumnInfo(name = "group",    type = "string",  title = gettext("Group"))
  ttestDescriptivesTable$addColumnInfo(name = "N",        type = "integer", title = gettext("N"))
  ttestDescriptivesTable$addColumnInfo(name = "mean",     type = "number",  title = gettext("Mean"))
  ttestDescriptivesTable$addColumnInfo(name = "sd",       type = "number",  title = gettext("SD"))
  ttestDescriptivesTable$addColumnInfo(name = "se",       type = "number",  title = gettext("SE"))
  
  container[["table"]] <- ttestDescriptivesTable
  
  if(ready)
    .ttestIndependentDescriptivesFill(ttestDescriptivesTable, dataset, options)
}

.ttestIndependentDescriptivesFill <- function(table, dataset, options) {
  variables <- options$variables
  groups <- options$groupingVariable
  levels <- base::levels(dataset[[ .v(groups) ]])
  groupingData <- dataset[[.v(groups)]]
  
  for (variable in variables) {
    
    for (level in levels) {
      
      row <- list(variable = variable, group = level, .isNewGroup = (level == levels[1]))
      
      variableData <- dataset[[.v(variable)]]
      groupData   <- variableData[groupingData == level]
      groupDataOm <- na.omit(groupData)
      
      if (class(groupDataOm) != "factor") {
        
        n    <- length(groupDataOm)
        mean <- mean(groupDataOm)
        std  <- sd(groupDataOm)
        sem  <- std / sqrt(n)
        
        row <- c(row, list(N = n, mean = mean, sd = std, se = sem))
        
      } else {
        n   <- length(groupDataOm)
        row <- c(row, list(n = n))
      }
      
      table$addRows(row)
    }
  }
}

.ttestIndependentDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["plots"]] <- createJaspContainer(gettext("Descriptives Plots"))
  subcontainer <- container[["plots"]]
  subcontainer$position <- 5
  for(variable in options$variables) {
    if(!is.null(subcontainer[[variable]]))
      next
    descriptivesPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestIndependentDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesPlotFill <- function(dataset, options, variable) {
  groups   <- options$groupingVariable
  
  errors <- .hasErrors(dataset, 
                       message = 'short', 
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = '< 2',
                       observations.grouping = groups)
  
  if(!identical(errors, FALSE))
    stop(errors$message)
  
  descriptivesPlotList <- list()
  
  base_breaks_x <- function(x) {
    b <- unique(as.numeric(x))
    d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1))
  }
  
  base_breaks_y <- function(x) {
    ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
  }
  
  dataset <- na.omit(dataset[, c(.v(groups), .v(variable))])
  ci <- options$descriptivesPlotsConfidenceInterval
  summaryStat <- .summarySE(as.data.frame(dataset), 
                            measurevar = .v(variable),
                            groupvars = .v(groups), 
                            conf.interval = ci, na.rm = TRUE, .drop = FALSE)
  
  colnames(summaryStat)[which(colnames(summaryStat) == .v(variable))] <- "dependent"
  colnames(summaryStat)[which(colnames(summaryStat) == .v(groups))]   <- "groupingVariable"
  
  pd <- ggplot2::position_dodge(0.2)
  
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
                                                 y = dependent, group = 1)) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), 
                           colour = "black", width = 0.2, position = pd) +
    ggplot2::geom_line(position = pd, size = 0.7) + 
    ggplot2::geom_point(position = pd, size = 4) + 
    ggplot2::ylab(unlist(variable)) + 
    ggplot2::xlab(options$groupingVariable) +
    base_breaks_y(summaryStat) + base_breaks_x(summaryStat$groupingVariable)
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}
