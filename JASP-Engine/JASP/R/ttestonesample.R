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
  
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type = "one-sample")
    .ttestCheckErrors(        dataset, options, type = "one-sample")
  }
  # Output tables (each calls its own results function)
  .ttestOneMainTable(jaspResults, dataset, options, ready)
  .ttestNormalTable( jaspResults, dataset, options, ready, type = "one-sample")
  
  # Descriptives
  .ttestDescriptivesTable(    jaspResults, dataset, options, ready, type = "one-sample")
  .ttestOneDescriptivesPlot(  jaspResults, dataset, options, ready)
  
  return()
}

# Tables
.ttestOneMainTable <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["ttestPairMainTable"]])) return()
  
  # Create table
  ttest <- createJaspTable(title = "One Sample T-Test")
  dependList <- c("variables", 
                  "effectSize", "effSizeConfidenceIntervalCheckbox", 
                  "effSizeConfidenceIntervalPercent",
                  "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                  "meanDiffConfidenceIntervalPercent",
                  "students", "mannWhitneyU", "zTest",
                  "testValue", "hypothesis", "VovkSellkeMPR")
  ttest$dependOn(dependList)
  ttest$showSpecifiedColumnsOnly <- TRUE
  
  ## does the user want mean differences, effect sizes and confidence intervals?
  wantsEffect <- options$effectSize
  wantsDifference <- options$meanDifference
  wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
  wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
  percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
  wantsStudents <- options$students
  wantsWilcox <- options$mannWhitneyU
  wantsZtest <- options$zTest
  
  allTests <- c(wantsStudents, wantsWilcox, wantsZtest)
  onlyTest <- sum(allTests) == 1
  
  ttest$addColumnInfo(name = "v", type = "string", title = "")
  ttest$addColumnInfo(name = "df", type = "integer")
  ttest$addColumnInfo(name = "p", type = "pvalue")
  
  if (wantsWilcox && onlyTest) {
    ttest$addFootnote("Wilcoxon signed-rank test.")
    testStat <- "V"
    # potentially dangerous next line - removing df. Not posssible to remove by name
    #fields <- fields[-2] #Wilcoxon's test doesn't have degrees of freedoms
    nameOfLocationParameter <- "Hodges-Lehmann Estimate"
    nameOfEffectSize <- "Rank-Biserial Correlation"
  } else if (wantsStudents && onlyTest) {
    ttest$addFootnote("Student's t-test.")
    testStat <- "t"
    nameOfLocationParameter <- "Mean Difference"
    nameOfEffectSize <- "Cohen's d"
  } else if(wantsZtest && onlyTest){
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
  
  ttest$addColumnInfo(name = testStat, type = "number")
  
  ## if the user wants more than one tests, add a column called "Test"
  if (sum(allTests) > 1) 
    ttest$addColumnInfo(name = "test", type = "string", title = "Test")
  
  .ttestVovkSellke(ttest, options)
  
  if (wantsDifference) {
    ttest$addColumnInfo(name = "m", title = nameOfLocationParameter, type = "number")
    
    # preparing footnote - paste if selected
    textDifference <- ""
    if(wantsStudents)
      textDifference <- "For the Student t-test, location parameter is given by mean difference <em>d</em>"
    if(wantsWilcox){
      if(wantsStudents)
        textDifference <- paste0(textDifference, "; for the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate")
      else
        textDifference <- "For the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate"
    }
    if(wantsZtest){
      if(onlyTest)
        textDifference <- "For the Z-test, location parameter is given by mean difference <em>d</em>"
      else
        textDifference <- paste0(textDifference,  "; for the Z-test, location parameter is given by mean difference <em>d</em>")
    }
    textDifference <- paste0(textDifference, ".")
    ttest$addFootnote(textDifference, symbol = "<em>Note.</em>")
  }
  
  if (wantsConfidenceMeanDiff) {
    interval <- 100 * percentConfidenceMeanDiff
    title <- paste0(interval, "% CI for ", nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                                    title = "Lower", overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                                    title = "Upper", overtitle = title)
  }
  
  if (wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    
    # preparing footnote - paste if selected
    textEffect <- ""
    if(wantsStudents)
      textEffect <- "For the Student t-test, effect size is given by Cohen's <em>d</em>"
    
    if(wantsWilcox){
      if(wantsStudents)
        textEffect <- paste0(textEffect, "; for the Wilcoxon test, effect size is given by the matched rank biserial correlation")
      else
        textEffect <- "For the Wilcoxon test, effect size is given by the matched rank biserial correlation"
    }
    
    if(wantsZtest){
      if(onlyTest)
        textEffect <- "For the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)"
      else
        textEffect <- paste0(textEffect, "; for the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)")
    }
    
    textEffect <- paste0(textEffect, ".")
    ttest$addFootnote(textEffect, symbol = "<em>Note.</em>")
  }
  
  if (wantsConfidenceEffSize) {
    interval <- 100 * percentConfidenceEffSize
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
    if (wantsWilcox && !wantsStudents)
      message <- gsub(pattern = "mean", replacement = "median", x = message)
    else if (wantsWilcox && wantsStudents){
      tMessage <- gsub(pattern = "For all tests", replacement = "For the Student t-tests", x = paste0(note, options$testValue))
      wilcoxMessage <- gsub(pattern = "mean", replacement = "median", x = paste0(note, options$testValue))
      wilcoxMessage <- gsub(pattern = "For all tests", replacement = "for the Wilcoxon test", x = wilcoxMessage)
      message <- paste0(tMessage, "; ", wilcoxMessage)
    }
    ttest$addFootnote(message, symbol = "<em>Note.</em>")
  }
  
  jaspResults[["ttest"]] <- ttest
  if(!ready)
    ready()
  res <- try(.ttestOneMainFill(jaspResults, dataset, options, testStat))
  .ttestSetError(res, ttest)
}

# Plot
.ttestOneDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(variable in options$variables) {
    title <- variable
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[title]] <- descriptivesPlot
    
    if(ready){
      p <- try(.ttestOneDescriptivesPlotFill(jaspResults, dataset, options, ready, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestOneDescriptivesPlotFill <- function(jaspResults, dataset, options, ready, variable){
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
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) 
  p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,  ymax = ciUpper), colour = "black", width = 0.2, position = pd)
  p <- p + ggplot2::geom_line(position = pd, size = 0.7) #gives geom_path warning
  p <- p + ggplot2::geom_point(position = pd, size = 4) 
  p <- p + ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept = testValue), linetype = "dashed") 
  p <- p + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat, options) 
  p <- JASPgraphs::themeJasp(p) + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  
  return(p)
}

#Table fill functions
.ttestOneMainFill <- function(jaspResults, dataset, options, testStat) {
  ## does the user want mean differences, effect sizes and confidence intervals?
  wantsEffect <- options$effectSize
  wantsDifference <- options$meanDifference
  wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
  wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
  percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
  wantsStudents <- options$students
  wantsWilcox <- options$mannWhitneyU
  wantsZtest <- options$zTest
  
  allTests <- c(wantsStudents, wantsWilcox)
  onlyTest <- sum(allTests) == 1
  
  #########################
  ## check the directionality
  if (options$hypothesis == "greaterThanTestValue")
    direction <- "greater"
  else if (options$hypothesis == "lessThanTestValue") 
    direction <- "less"
  else 
    direction <- "two.sided"
  
  variables <- options$variables
  if (length(variables) == 0)  
    variables = "."
  
  rowNo <- 1
  ttest.rows <- list() # for each variable and each test, save stuff in there
  whichTests <- list("1" = wantsStudents, "2" = wantsWilcox, "3" = wantsZtest)
  
  ## add a row for each variable, even before we are conducting tests
  for (variable in variables)
    ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
  
  for (variable in variables) {
    for (test in seq_len(length(whichTests))) {
      
      currentTest <- whichTests[[test]]
      
      ## don't run a test the user doesn't want
      if (!currentTest) 
        next
      
      row <- try(silent = TRUE, expr = {
        
        dat <- na.omit(dataset[[ .v(variable) ]])
        n   <- length(dat)
        if (test == 2) {
          r <- stats::wilcox.test(dat, alternative = direction, mu = options$testValue,
                                  conf.level = percentConfidenceMeanDiff, conf.int = TRUE)
          df   <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
          nd   <- sum(dat != 0)
          maxw <- (nd * (nd + 1)) / 2
          d    <- as.numeric((r$statistic / maxw) * 2 - 1)
          wSE  <- sqrt((nd * (nd + 1) * (2 * nd + 1)) / 6) /2
          mrSE <- sqrt(wSE^2  * 4 * (1 / maxw^2)) 
          # zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
          zmbiss <- atanh(d)
          if(direction == "two.sided")
            confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize)/2)*mrSE), tanh(zmbiss + qnorm((1+percentConfidenceEffSize)/2)*mrSE)))
          else if (direction == "less") 
            confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(percentConfidenceEffSize)*mrSE)))
          else if (direction == "greater")
            confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize))*mrSE), Inf))
        } else if(test == 3){
          r <- BSDA::z.test(dat, alternative = direction, mu = options$testValue, 
                            sigma.x = options$stddev, conf.level = percentConfidenceMeanDiff)
          df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
          d  <- (mean(dat) - options$testValue) / options$stddev
          
          if(direction == "less")
            r$conf.int[1] <- -Inf
          else if(direction == "greater")
            r$conf.int[2] <- Inf
          
          Z <- as.numeric(r$statistic)
          confIntEffSize <- c(0,0)
          if(wantsConfidenceEffSize)
            confIntEffSize <- r$conf.int/options$stddev
        } else {
          
          r <- stats::t.test(dat, alternative = direction, mu = options$testValue, 
                             conf.level = percentConfidenceMeanDiff)
          df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
          d  <- (mean(dat) - options$testValue) / sd(dat)
          t  <- as.numeric(r$statistic)
          
          confIntEffSize <- c(0,0)
          
          if (wantsConfidenceEffSize) {
            
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
      
      firstSelectedTest <- allTestNames[unlist(whichTests)][1]
      #if(rowNo %% 2 == 0 || rowNo %% 3 == 0){
      if(testName != firstSelectedTest)
        row[["v"]] <- ""
      
      jaspResults[["ttest"]]$addRows(row)
      rowNo <- rowNo + 1
    }
  }
}
