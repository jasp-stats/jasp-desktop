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
  ttest <- createJaspTable(title = "One Sample T-Test")
  dependList <- c("effectSize", "effSizeConfidenceIntervalCheckbox", "variables",
                  "effSizeConfidenceIntervalPercent", "students", "mannWhitneyU",
                  "meanDifference", "meanDiffConfidenceIntervalCheckbox", "stddev",
                  "meanDiffConfidenceIntervalPercent", "hypothesis", 
                  "VovkSellkeMPR", "missingValues")
  oneSampleList   <- c("zTest", "testValue")
  ttest$dependOn(c(dependList, oneSampleList))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1
  
  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote("Wilcoxon signed-rank test.")
    testStat <- "V"
    nameOfLocationParameter <- "Hodges-Lehmann Estimate"
    nameOfEffectSize <- "Rank-Biserial Correlation"
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote("Student's t-test.")
    testStat <- "t"
    nameOfLocationParameter <- "Mean Difference"
    nameOfEffectSize <- "Cohen's d"
  } else if(optionsList$wantsZtest && optionsList$onlyTest){
    ttest$addFootnote("Z test.")
    testStat <- "Z"
    # potentially dangerous next line - removing df. Not posssible to remove by name
    #fields <- fields[-2] #Z test doesn't have degrees of freedoms
    nameOfLocationParameter <- "Mean Difference"
    nameOfEffectSize <- "Cohen's d"
  } else {
    testStat <- "Statistic"
    nameOfLocationParameter <-  "Location Parameter"
    nameOfEffectSize <-  "Effect Size"
  }
  
  ttest$addColumnInfo(name = "v",      type = "string", title = "")
  ## if the user wants more than one test, add a column called "Test"
  if (sum(optionsList$allTests) > 1) 
    ttest$addColumnInfo(name = "test", type = "string", title = "Test")
  ttest$addColumnInfo(name = testStat, type = "number")
  if(optionsList$wantsStudents)
    ttest$addColumnInfo(name = "df",     type = "integer")
  ttest$addColumnInfo(name = "p",      type = "pvalue")
  
  .ttestVovkSellke(ttest, options)
  
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "m", title = nameOfLocationParameter, type = "number")
    
    # preparing footnote - paste if selected
    textDifference <- ""
    if(optionsList$wantsStudents)
      textDifference <- "For the Student t-test, location parameter is given by mean difference <em>d</em>"
    if(optionsList$wantsWilcox){
      if(optionsList$wantsStudents)
        textDifference <- paste0(textDifference, "; for the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate")
      else
        textDifference <- "For the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate"
    }
    if(optionsList$wantsZtest){
      if(optionsList$onlyTest)
        textDifference <- "For the Z-test, location parameter is given by mean difference <em>d</em>"
      else
        textDifference <- paste0(textDifference,  "; for the Z-test, location parameter is given by mean difference <em>d</em>")
    }
    textDifference <- paste0(textDifference, ".")
    ttest$addFootnote(textDifference)
  }
  
  if (optionsList$wantsConfidenceMeanDiff) {
    interval <- 100 * optionsList$percentConfidenceMeanDiff
    title <- paste0(interval, "% CI for ", nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                        title = "Lower", overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                        title = "Upper", overtitle = title)
  }
  
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    
    # preparing footnote - paste if selected
    textEffect <- ""
    if(optionsList$wantsStudents)
      textEffect <- "For the Student t-test, effect size is given by Cohen's <em>d</em>"
    
    if(optionsList$wantsWilcox){
      if(optionsList$wantsStudents)
        textEffect <- paste0(textEffect, "; for the Wilcoxon test, effect size is given by the matched rank biserial correlation")
      else
        textEffect <- "For the Wilcoxon test, effect size is given by the matched rank biserial correlation"
    }
    
    if(optionsList$wantsZtest){
      if(optionsList$onlyTest)
        textEffect <- "For the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)"
      else
        textEffect <- paste0(textEffect, "; for the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)")
    }
    
    textEffect <- paste0(textEffect, ".")
    ttest$addFootnote(textEffect)
  }
  
  if (optionsList$wantsConfidenceEffSize) {
    interval <- 100 * optionsList$percentConfidenceEffSize
    title <- paste0(interval, "% CI for ", nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                        title = "Lower", overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                        title = "Upper", overtitle = title)
  }
  
  
  ### check the directionality
  if (options$hypothesis == "greaterThanTestValue") {
    direction <- "greater"
    note <- "For all tests, the alternative hypothesis specifies that the mean is greater than "
  } else if (options$hypothesis == "lessThanTestValue") {
    direction <- "less"
    note <- "For all tests, the alternative hypothesis specifies that the mean is less than "
  } else {
    direction <- "two.sided"
    note <- "For all tests, the alternative hypothesis specifies that the population mean is different from "
  }
  
  if (options$testValue != 0) {
    message <- paste0(note, options$testValue, ".")
    if (optionsList$wantsWilcox && !optionsList$wantsStudents)
      message <- gsub(pattern = "mean", replacement = "median", x = message)
    else if (optionsList$wantsWilcox && optionsList$wantsStudents){
      tMessage <- gsub(pattern = "For all tests", replacement = "For the Student t-tests", x = paste0(note, options$testValue))
      wilcoxMessage <- gsub(pattern = "mean", replacement = "median", x = paste0(note, options$testValue))
      wilcoxMessage <- gsub(pattern = "For all tests", replacement = "for the Wilcoxon test", x = wilcoxMessage)
      message <- paste0(tMessage, "; ", wilcoxMessage)
    }
    ttest$addFootnote(message)
  }
  
  jaspResults[["ttest"]] <- ttest
  res <- try(.ttestOneSampleMainFill(jaspResults, dataset, options, ready, testStat, optionsList))
  .ttestSetError(res, ttest)
}

.ttestOneSampleNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTests || !is.null(container[["ttestNormalTable"]])) 
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = "Test of Normality (Shapiro-Wilk)")
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  ttestNormalTable$position <- 2
  
  
  ttestNormalTable$addColumnInfo(name = "v", title = "", type = "string")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = "W")
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = "p")
  
  message <- "Significant results suggest a deviation from normality."
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  res <- try(.ttestOneSampleNormalFill(container, dataset, options, ready))
  .ttestSetError(res, ttestNormalTable)
}

.ttestOneSampleMainFill <-function(jaspResults, dataset, options, ready, testStat, optionsList) {
  direction <- switch(options$hypothesis,
                   "notEqualToTestValue"  ="two.sided",
                   "greaterThanTestValue" ="greater",
                   "lessThanTestValue"    ="less")
  
  if(!ready)
    return()
  variables <- options$variables
  if (length(variables) == 0)  
    variables = "."
  
  rowNo      <- 1
  ttest.rows <- list() # for each variable and each test, save stuff in there
  
  ## add a row for each variable, even before we are conducting tests
  for (variable in variables)
    ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
  
  for (variable in variables) {
    errors <- .hasErrors(dataset, 
                         message = 'short', 
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         observations.amount = '< 2')
    for (test in seq_len(length(optionsList$whichTests))) {
      currentTest <- optionsList$whichTests[[test]]
      
      ## don't run a test the user doesn't want
      if (!currentTest) 
        next
      if (!identical(errors, FALSE)){
        jaspResults[["ttest"]]$addFootnote(errors$message, colNames = c("p", "m", testStat), rowNames = variables)
        row <- list(v = variable, p = "NaN", m = "NaN", lowerCI = "", upperCI = "")
        row[[testStat]] <- NaN
        jaspResults[["ttest"]]$addRows(row, rowNames = variables)
        next
      }
      
      row <- try(silent = TRUE, expr = {
        
        dat <- na.omit(dataset[[ .v(variable) ]])
        n   <- length(dat)
        if (test == 2) {
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
        } else if(test == 3){
          r <- BSDA::z.test(dat, alternative = direction, mu = options$testValue, 
                            sigma.x = options$stddev, 
                            conf.level = optionsList$percentConfidenceMeanDiff)
          df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
          d  <- (mean(dat) - options$testValue) / options$stddev
          
          if(direction == "less")
            r$conf.int[1] <- -Inf
          else if(direction == "greater")
            r$conf.int[2] <- Inf
          
          Z <- as.numeric(r$statistic)
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
        
        res <- list(v = variable, df = df, p = p, m = m, d = d, 
                    lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp, 
                    lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize)
        res[[testStat]] <- stat
        if (options$VovkSellkeMPR)
          res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
        res
      })
      
      ## if we are not yet ready to perform, just create an empty table
      
      # if we have multiple tests, we want only variable name at the first row;
      # and having additional column indicating the name of the tests
      allTestNames <- c("Student", "Wilcoxon", "Z")
      testName <- allTestNames[test]
      row[["test"]] <- testName
      
      firstSelectedTest <- allTestNames[unlist(optionsList$whichTests)][1]
      #if(rowNo %% 2 == 0 || rowNo %% 3 == 0){
      if(testName != firstSelectedTest)
        row[["v"]] <- ""
      
      jaspResults[["ttest"]]$addRows(row)
      rowNo <- rowNo + 1
    }
  }
}

.ttestOneSampleNormalFill <- function(container, dataset, options, ready) {
  variables <- options$variables
  rowNo <- 1
  if (!ready) 
    variables = "."
  
  for (variable in variables) {
    
    if (ready) {
      errors <- .hasErrors(dataset, 
                           message = 'short', 
                           type = c('observations', 'variance', 'infinity'),
                           all.target = variable,
                           observations.amount = c('< 3', '> 5000'))
      if(!identical(errors, FALSE)) {
        container[["ttestNormalTable"]]$addFootnote(errors$message, colNames = c("W", "p"), rowNames = variable)
        container[["ttestNormalTable"]]$addRows(list(v = variable, W = "NaN", p = "NaN", 
                                                     .isNewGroup = rowNo==1), rowNames = variable)
        next
      }
      
      data <- na.omit(dataset[[.v(variable)]])
      
      r <- stats::shapiro.test(data)
      W <- as.numeric(r$statistic)
      p <- r$p.value
      
      row <- list(v = variable, W = W, p = p)
      
    } else 
      row <- list(v = variable, W = ".", p = ".")
    row[[".isNewGroup"]] <- rowNo == 1
    container[["ttestNormalTable"]]$addRows(row, rowNames = variable)
  }
}

.ttestOneSampleDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["plots"]] <- createJaspContainer("Descriptives Plots")
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
