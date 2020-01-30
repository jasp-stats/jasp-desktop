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
  dependList <- c("effectSize", "effSizeConfidenceIntervalCheckbox", "variables",
                  "descriptivesEffectSizeConfidenceIntervalPercent", "students", "mannWhitneyU",
                  "meanDifference", "meanDiffConfidenceIntervalCheckbox", "stddev",
                  "meanDiffConfidenceIntervalPercent", "hypothesis", 
                  "VovkSellkeMPR", "missingValues")
  independentList <- c("groupingVariable", "effectSizesType", 
                       "welchs", "mannWhitneyU",
                       "descriptivesMeanDiffConfidenceIntervalPercent")
  ttest$dependOn(c(dependList, independentList))
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
    
  
  ttest$addColumnInfo(name = "v", title = " ", type = "string", combine = TRUE)
  if (sum(optionsList$allTests) >= 2) 
    ttest$addColumnInfo(name = "test", type = "string",  title = gettext("Test"))
  ttest$addColumnInfo(name = testStat, type = "number",  title = testStatName)
  ttest$addColumnInfo(name = "df",     type = "integer", title = gettext("df"))
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
  
  ## add mean difference and standard error difference
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
    if (!(optionsList$wantsWilcox && optionsList$onlyTest))  # Only add SE Difference if not only MannWhitney is requested
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")
  }
  
  if (optionsList$wantsDifference && optionsList$wantsWilcox && 
      optionsList$wantsStudents && optionsList$wantsWelchs) {
    message <- gettext("For the Student t-test and Welch t-test, 
              location parameter is given by mean difference; for the Mann-Whitney test, 
              location parameter is given by the Hodges-Lehmann estimate.")
    ttest$addFootnote(message)
  } else if (optionsList$wantsDifference && 
             optionsList$wantsWilcox && optionsList$wantsStudents) {
    message <- gettext("For the Student t-test, 
              location parameter is given by mean difference; for the Mann-Whitney test, 
              location parameter is given by Hodges-Lehmann estimate.")
    ttest$addFootnote(message)
  } else if (optionsList$wantsDifference &&
             optionsList$wantsWilcox && optionsList$wantsWelchs) {
    message <- gettext("For the Welch t-test, 
              location parameter is given by mean difference; for the Mann-Whitney test,
              location parameter is given by Hodges-Lehmann estimate.")
    ttest$addFootnote(message)
  }
  
  if (optionsList$wantsConfidenceMeanDiff) {
    interval <- 100 * optionsList$percentConfidenceMeanDiff
    title <- gettextf("%1$s%% CI for %2$s", interval, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                        title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                        title = gettext("Upper"), overtitle = title)
  }
  
  ## add Cohen's d
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    if (optionsList$wantsWilcox) {
      if (optionsList$wantsStudents || optionsList$wantsWelchs) 
        message <- paste0(gettext("For the Mann-Whitney test, effect size is given by the rank biserial correlation. 
                         For the other test(s), by "), nameOfEffectSizeParametric, ".")
      else
        message <- gettext("For the Mann-Whitney test, effect size is given by the rank biserial correlation.")
      ttest$addFootnote(message)
    }
  }
  
  if (optionsList$wantsConfidenceEffSize) {
    interval <- 100 * optionsList$percentConfidenceEffSize
    title <- gettextf("%1$s%% CI for %2$s", interval, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                        title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                        title = gettext("Upper"), overtitle = title)
  }
  
  jaspResults[["ttest"]] <- ttest
  res <- try(.ttestIndependentMainFill(jaspResults, dataset, options, ready, testStat, optionsList))
  .ttestSetError(res, ttest)
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
  
  res <- try(.ttestIndependentNormalFill(container, dataset, options, ready))
  .ttestSetError(res, ttestNormalTable)
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
  res <- try(.ttestIndependentEqVarFill(container, dataset, options, ready))
  .ttestSetError(res, equalityVariance)
}

.ttestIndependentMainFill <- function(jaspResults, dataset, options, ready, testStat, optionsList){
  direction <- .ttestMainGetDirection(options$hypothesis)
  
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
  
  variables  <- options$variables
  if (length(variables) == 0) 
    variables <- "."
  
  if(!ready) {
    for (variable in variables) 
      jaspResults[["ttest"]]$addRows(list(v = variable))
  } else {
    levels <- levels(dataset[[ .v(options$groupingVariable) ]])
    
    if (options$hypothesis == "groupOneGreater")
      message <- gettextf("For all tests, the alternative hypothesis specifies that group %1$s is greater than group %2$s.",
                          paste("<em>", levels[1], "</em>"), paste("<em>", levels[2], "</em>"))
    else if (options$hypothesis == "groupTwoGreater")
      message <- gettextf("For all tests, the alternative hypothesis specifies that group %1$s is less than group %2$s.",
                          paste("<em>", levels[1], "</em>"), paste("<em>", levels[2], "</em>"))
    else
      message <- gettextf("For all tests, the alternative hypothesis specifies that group %1$s is not equal to group %2$s.",
                          paste("<em>", levels[1], "</em>"), paste("<em>", levels[2], "</em>"))
    
    jaspResults[["ttest"]]$addFootnote(message)
    
    groupingData <- dataset[[ .v(options$groupingVariable) ]]
    ## for each variable specified, run each test that the user wants
    for (variable in options$variables) {
      errors <- .hasErrors(dataset, 
                           message = 'short', 
                           type = c('observations', 'variance', 'infinity'),
                           all.target = variable, 
                           all.grouping = options$groupingVariable,
                           observations.amount = '< 2')
      variableData <- dataset[[ .v(variable) ]]
      
      ## test is a number, indicating which tests should be run
      for (test in seq_len(length(optionsList$whichTests))) {
        
        ## don't run a test the user doesn't want
        currentTest <- optionsList$whichTests[[test]]
        if (!currentTest)
          next
        
        if (test == 1)
          whatTest <- "Student"
        else if (test == 2)
          whatTest <- "Welch"
        else
          whatTest <- "Mann-Whitney"
        
        if (!identical(errors, FALSE)) {
          jaspResults[["ttest"]]$addFootnote(errors$message, colNames = testStat, rowNames = variable)
          row <- list(v = variable, test = whatTest)
          row[[testStat]] <- NaN
          jaspResults[["ttest"]]$addRows(row, rowNames = variable)
          next
        }
        
        ## try to run the test, catching eventual errors
        row <- try(silent = FALSE, expr = {
          ciEffSize  <- optionsList$percentConfidenceEffSize
          ciMeanDiff <- optionsList$percentConfidenceMeanDiff
          f <- as.formula(paste(.v(variable), "~",
                                .v(options$groupingVariable)))
          
          y <- dataset[[ .v(variable) ]]
          groups <- dataset[[ .v(options$groupingVariable) ]]
          
          sds <- tapply(y, groups, sd, na.rm = TRUE)
          ms  <- tapply(y, groups, mean, na.rm = TRUE)
          ns  <- tapply(y, groups, function(x) length(na.omit(x)))
          
          
          if (test == 3) {
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
            whatTest <- ifelse(test == 2, "Welch", "Student")
            r <- stats::t.test(f, data = dataset, alternative = direction,
                               var.equal = test != 2, conf.level = ciMeanDiff, paired = FALSE)
            
            df   <- as.numeric(r$parameter)
            m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
            stat <- as.numeric(r$statistic)
            
            num <-  (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
            sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
            if (test == 2)  # Use different SE when using Welch T test!
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
          ## if the user doesn't want a Welch's t-test,
          ## give a footnote indicating if the equality of variance
          ## assumption is met; seems like in this setting there is no
          ## sampling plan, thus the p-value is not defined. haha!
          if (!optionsList$wantsWelchs && optionsList$wantsStudents) {
            levene <- car::leveneTest(variableData, groupingData, "mean")
            
            ## arbitrary cut-offs are arbitrary
            if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
              msg <- gettext("Levene's test is significant (p < .05), 
					  suggesting a violation of the equal variance assumption")
              jaspResults[["ttest"]]$addFootnote(msg)
            }
          }
          
          ## same for all t-tests
          p     <- as.numeric(r$p.value)
          ciLow <- r$conf.int[1]
          ciUp  <- r$conf.int[2]
          lowerCIeffectSize <- as.numeric(confIntEffSize[1])
          upperCIeffectSize <- as.numeric(confIntEffSize[2])
          # this will be the results object
          res <- list(v = variable, test = whatTest, df = df, p = p, md = m, d = d, 
                      lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
                      lowerCIeffectSize = lowerCIeffectSize, upperCIeffectSize = upperCIeffectSize,
                      sed = sed)
          res[[testStat]] <- stat
          if (options$VovkSellkeMPR)
            res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
          res
        })
        jaspResults[["ttest"]]$addRows(row)
      }
    }
    
    if (effSize == "glass") {
      sdMessage <- gettextf("Glass' delta uses the standard deviation of group %1$s of variable %2$s.", names(ns[2]), options$groupingVariable)
      jaspResults[["ttest"]]$addFootnote(sdMessage)
    }
  }
}

.ttestIndependentEqVarFill <- function(container, dataset, options, ready){
  
  data      <- list()
  variables <- options$variables
  groups    <- options$groupingVariable
  if (length(variables) == 0) variables <- "."
  
  for (variable in variables)
    data[[length(data) + 1]] <- list(variable = variable)
  
  if (groups != "") {
    
    levels <- levels(dataset[[ .v(groups) ]])
    
    for (variable in variables) {
      errors <- .hasErrors(dataset, 
                          message = 'short', 
                          type = c('observations', 'variance', 'infinity'),
                          all.target = variable,
                          observations.amount = c('< 3'),
                          all.grouping = groups)
      if (!identical(errors, FALSE)) {
        container[["equalityVariance"]]$addFootnote(errors$message, colNames = c("F", "p"), rowNames = variable)
        row <- list(variable = variable, F = NaN, df = "", p = NaN)
        container[["equalityVariance"]]$addRows(row, rowNames = variable)
        next
      }
      
      result <- try(silent = TRUE, expr = {
        
        levene <- car::leveneTest(dataset[[ .v(variable) ]],
                                  dataset[[ .v(groups) ]], "mean")
        
        F  <- levene[1, "F value"]
        df <- levene[1, "Df"]
        p  <- levene[1, "Pr(>F)"]
        
        row <- list(variable = variable, F = F, df = df, p = p)
        
        if (is.na(levene[1, "F value"])) {
          note <- gettext("F-statistic could not be calculated")
          container[["equalityVariance"]]$addFootnote(note)
        }
        row
      })
      
      if (isTryError(result))
        result <- list(variable = variable, F = "", df = "", p = "")
      
      container[["equalityVariance"]]$addRows(result)
    }
  }
}

.ttestIndependentNormalFill <- function(container, dataset, options, ready) {
  ## for a independent t-test, we need to check both group vectors for normality
  variables <- options$variables
  factor    <- options$groupingVariable
  levels    <- levels(dataset[[.v(factor)]])
  
  if (length(variables) == 0) 
    variables = "."
  if (length(levels) == 0) 
    levels = c(".", ".")
  
  for (variable in variables) {
    ## there will be two levels, otherwise .hasErrors will quit
    for (level in levels) {
      if (ready) {
        errors <- .hasErrors(dataset, 
                             message = 'short', 
                             type = c('observations', 'variance', 'infinity'),
                             all.target = variable,
                             observations.amount = c('< 3', '> 5000'),
                             all.grouping = factor,
                             all.groupingLevel = level)
        if (!identical(errors, FALSE)) {
          container[["ttestNormalTable"]]$addFootnote(errors$message, colNames = c("W", "p"), rowNames = paste0(variable, level))
          row <- list(dep = variable, lev = level, W = NaN, p = NaN)
          container[["ttestNormalTable"]]$addRows(row, rowNames = paste0(variable, level))
          next
        }
        
        ## get the dependent variable at a certain factor level
        data <- na.omit(dataset[[.v(variable)]][dataset[[.v(factor)]] == level])
        r    <- stats::shapiro.test(data)
        W    <- as.numeric(r$statistic)
        p    <- r$p.value
        
        result <- list(dep = variable, lev = level, W = W, p = p)
      } else 
        result <- list(dep = variable, lev = level, W = ".", p = ".")
      result[[".isNewGroup"]] <- level == levels[1]
      container[["ttestNormalTable"]]$addRows(result)
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
  
  if(!ready) 
    return()
  res <- try(.ttestIndependentDescriptivesFill(container, dataset, options))
  .ttestSetError(res, ttestDescriptivesTable)
}

.ttestIndependentDescriptivesFill <- function(container, dataset, options) {
data <- list()
  
  variables <- options$variables
  if (length(variables) == 0) variables <- "."
  
  groups <- options$groupingVariable
  
  ## if we actually have to do the test, and we have a grouping variable
  if (groups != "") {
    levels <- base::levels(dataset[[ .v(groups) ]])
    
    groupingData <- dataset[[.v(groups)]]
    
    ## do the whole loop as above again
    for (variable in variables) {
      
      for (i in 1:2) { #only 2 levels thanks to error check
        
        level <- levels[i]
        variableData <- dataset[[.v(variable)]]
        
        groupData   <- variableData[groupingData == level]
        groupDataOm <- na.omit(groupData)
        
        if (class(groupDataOm) != "factor") {
          
          n    <- length(groupDataOm)
          mean <- mean(groupDataOm)
          std  <- sd(groupDataOm)
          sem  <- std / sqrt(n)
          
          result <- list(variable = variable, group = level,
                         N = n, mean = mean, sd = std, se = sem)
          
        } else {
          n      <- length(groupDataOm)
          result <- list(variable = variable, group = "",
                         N = n, mean = "", sd = "", se = "")
        }
        if (i == 1)
          result[[".isNewGroup"]] <- TRUE
        
        data[[length(data) + 1]] <- result
      }
    }
  }
  container[["table"]]$addRows(data)
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
  
  dataset <- na.omit(dataset)
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
