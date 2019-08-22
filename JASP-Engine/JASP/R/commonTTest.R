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

#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
.ttestReadData <- function(dataset, options, type) {
  if (!is.null(dataset))
    return(dataset)
  else {
    groups  <- options$groupingVariable
    if (!is.null(groups) && groups == "") 
      groups <- NULL
    if(type %in% c("one-sample", "independent"))
      depvars <- unlist(options$variables)
    else if (type == 'paired') {
      depvars <- unlist(options$pairs)
      depvars <- depvars[depvars != ""]
    }
    exclude <- NULL
    if (options$missingValues == "excludeListwise")
      exclude <- depvars
    return(.readDataSetToEnd(columns.as.numeric  = depvars,
                             columns.as.factor   = groups,
                             exclude.na.listwise = exclude))
  }
}

.ttestCheckErrors <- function(dataset, options, type) {
  if(type == "paired") {
    for (pair in options$pairs) {
      if(pair[[1]] == "" || pair[[2]] == "")
        next
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      if(is.null(p1) || is.null(p2))
        return()
      datasetErrorCheck <- data.frame(dataset[[.v(p1)]] - dataset[[.v(p2)]])
      colnames(datasetErrorCheck) <- .v(paste0("Difference between ", p1, " and ", p2))
      .hasErrors(datasetErrorCheck, 
                 type = "variance", 
                 exitAnalysisIfErrors = TRUE)
      .hasErrors(dataset,
                 type = c('observations', 'variance', 'infinity'),
                 all.target = c(p1, p2), 
                 observations.amount  = c('< 3', '> 5000'), 
                 exitAnalysisIfErrors = TRUE)
    }
  }
  else if(type == "one-sample") {
    for (variable in options$variables)
      .hasErrors(dataset, 
                 type = c('observations', 'variance', 'infinity'),
                 all.target = variable,
                 observations.amount  = c('< 3', '> 5000'),
                 exitAnalysisIfErrors = TRUE)
  }
  else if(type == "independent") {
    if (length(options$variables) != 0 && options$groupingVariable != '') {
      .hasErrors(dataset,
                 type = 'factorLevels',
                 factorLevels.target  = options$groupingVariable, 
                 factorLevels.amount  = '!= 2',
                 exitAnalysisIfErrors = TRUE)
    }
    variables <- options$variables
    factor <- options$groupingVariable
    levels <- levels(dataset[[.v(factor)]])
    
    if (length(variables) == 0) variables = "."
    if (length(levels) == 0) levels = c(".", ".")
    
    for (variable in variables) {
      for (level in levels) {
        .hasErrors(dataset, 
                   type = c('observations', 'variance', 'infinity'),
                   all.target = variable,
                   observations.amount = c('< 3', '> 5000'),
                   all.grouping = factor,
                   all.groupingLevel = level,
                   exitAnalysisIfErrors = TRUE)
      }
    }
  }
}

.ttestMainTable <- function(jaspResults, dataset, options, ready, type) {
  if(type == "paired") {
    if (!is.null(jaspResults[["ttest"]])) 
      return()
    
    # Create table
    ttest <- createJaspTable(title = "Paired Samples T-Test")
    dependList <- c("pairs", 
                    "effectSize", "effSizeConfidenceIntervalCheckbox", 
                    "effSizeConfidenceIntervalPercent",
                    "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                    "meanDiffConfidenceIntervalPercent",
                    "students", "wilcoxonSignedRank",
                    "hypothesis", "VovkSellkeMPR")
    ttest$dependOn(dependList)
    ttest$showSpecifiedColumnsOnly <- TRUE
    
    ## what does the user want? what does s/he really want??
    wantsEffect <- options$effectSize
    wantsDifference <- options$meanDifference
    wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
    wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
    percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
    percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
    wantsStudents <- options$students
    wantsWilcox <- options$wilcoxonSignedRank
    
    allTests <- c(wantsStudents, wantsWilcox)
    onlyTest <- sum(allTests) == 1
    
    ttest$addColumnInfo(name = "v1", type = "string", title = "")
    #ttest$addColumnInfo(name = "sep",  type = "separator", title = "")
    ttest$addColumnInfo(name = "v2", type = "string", title = "")
    
    if (wantsWilcox && onlyTest) {
      ttest$addFootnote("Wilcoxon signed-rank test.")
      testStat <- "W"
      #fields <- fields[-4] #Wilcoxon's test doesn't have degrees of freedoms
      nameOfLocationParameter <- "Hodges-Lehmann Estimate"
      nameOfEffectSize <- "Rank-Biserial Correlation"
    } else if (wantsStudents && onlyTest) {
      ttest$addFootnote("Student's t-test.")
      testStat <- "t"
      nameOfLocationParameter <- "Mean Difference"
      nameOfEffectSize <- "Cohen's d"
    } else {
      testStat <- "Statistic"
      nameOfLocationParameter <-  "Location Parameter"
      nameOfEffectSize <-  "Effect Size"
    }
    
    
    ## if the user wants all tests, add a column called "Test"
    if (sum(allTests) == 2)
      ttest$addColumnInfo(name = "test", type = "string", title = "Test")
    ttest$addColumnInfo(name = testStat, type = "number")
    ttest$addColumnInfo(name = "df",  type = "integer")
    ttest$addColumnInfo(name = "p", type = "pvalue")
    .ttestVovkSellke(ttest, options)
    
    if (wantsDifference) {
      ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
      if(wantsStudents)
        ttest$addColumnInfo(name = "sed", title = "SE Difference", type = "number")
      if (wantsWilcox && wantsStudents) {
        message <- "For the Student t-test, location parameter is given by mean 
      difference <em>d</em>; for the Wilcoxon test, effect size is given by the 
      Hodges-Lehmann estimate."
        ttest$addFootnote(message, symbol = "<em>Note.</em>")
      } 
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
      if (wantsWilcox && wantsStudents) {
        message <- "For the Student t-test, 
      effect size is given by Cohen's <em>d</em>; for the Wilcoxon test, 
      effect size is given by the matched rank biserial correlation."
        ttest$addFootnote(message, symbol = "<em>Note.</em>")
      } 
    }
    
    if (wantsConfidenceEffSize) {
      interval <- 100 * percentConfidenceEffSize
      title <- paste0(interval, "% CI for ", nameOfEffectSize)
      ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                          title = "Lower", overtitle = title)
      ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                          title = "Upper", overtitle = title)
    }
    
    if (options$hypothesis == "groupOneGreater") {
      message   <- "All tests, hypothesis is measurement one greater than measurement two."
      ttest$addFootnote(message, symbol = "<em>Note.</em>")
    } else if (options$hypothesis == "groupTwoGreater") {
      message   <- "All tests, hypothesis is measurement one less than measurement two."
      ttest$addFootnote(message, symbol = "<em>Note.</em>")
    }
    jaspResults[["ttest"]] <- ttest
    
    res <- try(.ttestMainFill(jaspResults, dataset, options, ready, testStat, type))
    .ttestSetError(res, ttest)
  }
  else if(type == "one-sample") {
    if (!is.null(jaspResults[["ttest"]])) 
      return()
    
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
    
    ttest$addColumnInfo(name = "v",      type = "string", title = "")
    ttest$addColumnInfo(name = testStat, type = "number")
    ttest$addColumnInfo(name = "df",     type = "integer")
    ttest$addColumnInfo(name = "p",      type = "pvalue")
    
    ## if the user wants more than one test, add a column called "Test"
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
      return()
    res <- try(.ttestMainFill(jaspResults, dataset, options, testStat, type))
    .ttestSetError(res, ttest)
  }
  else if(type == "independent") {
    if (!is.null(jaspResults[["ttest"]])) 
      return()
    
    # Create table
    ttest <- createJaspTable(title = "Independent Samples T-Test")
    dependList <- c("variables", "groupingVariable",
                    "effectSize", "effSizeConfidenceIntervalCheckbox", 
                    "descriptivesEffectSizeConfidenceIntervalPercent", "effectSizesType",
                    "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                    "descriptivesMeanDiffConfidenceIntervalPercent",
                    "students", "welchs", "mannWhitneyU",
                    "hypothesis", "VovkSellkeMPR")
    ttest$dependOn(dependList)
    ttest$showSpecifiedColumnsOnly <- TRUE
    
    wantsEffect <- options$effectSize
    wantsDifference <- options$meanDifference
    wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
    wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
    percentConfidenceMeanDiff <- options$descriptivesMeanDiffConfidenceIntervalPercent
    percentConfidenceEffSize <- options$descriptivesEffectSizeConfidenceIntervalPercent
    ## can make any combination of the following tests:
    wantsWelchs <- options$welchs
    wantsStudents <- options$students
    wantsWilcox <- options$mannWhitneyU
    
    allTests <- c(wantsStudents, wantsWilcox)
    onlyTest <- sum(allTests) == 1
    
    if (wantsWilcox && onlyTest) {
      ttest$addFootnote("Mann-Whitney U test.")
      testStat <- "W"
      #fields <- fields[-3] # Wilcoxon's test doesn't have degrees of freedoms
    } else if (wantsWelchs && onlyTest) {
      ttest$addFootnote("Welch's t-test.")
      testStat <- "t"
    } else if (wantsStudents && onlyTest) {
      ttest$addFootnote("Student's t-test.")
      testStat <- "t"
    } else
      testStat <- "Statistic"
    
    ttest$addColumnInfo(name = "v", title = "title", type = "string", combine = TRUE)
    if(!onlyTest)
      ttest$addColumnInfo(name = "test", type = "string", title = "Test")
    ttest$addColumnInfo(name = testStat, type = "number")
    #type integer, not number, correct?
    ttest$addColumnInfo(name = "df",  type = "number")
    ttest$addColumnInfo(name = "p", type = "pvalue")
    .ttestVovkSellke(ttest, options)
    
    if (options$effectSizesType == "cohensD")
      effSize <- "cohen"
    else if (options$effectSizesType == "glassD")
      effSize <- "glass"
    else if (options$effectSizesType == "hedgesG")
      effSize <- "hedges"
    
    nameOfEffectSizeParametric <- switch(effSize, 
                                         cohen  = "Cohen's d", 
                                         glass  = "Glass' delta",
                                         hedges = "Hedges' g")
    
    if (!wantsWilcox) {
      nameOfLocationParameter <- "Mean Difference"
      nameOfEffectSize        <- nameOfEffectSizeParametric
    } else if (wantsWilcox && onlyTest) {
      nameOfLocationParameter <- "Hodges-Lehmann Estimate"
      nameOfEffectSize        <- "Rank-Biserial Correlation"
    } else if (wantsWilcox && (wantsStudents || wantsWelchs)) {
      nameOfLocationParameter <-  "Location Parameter"
      nameOfEffectSize        <-  "Effect Size"
    }
    
    ## add mean difference and standard error difference
    if (wantsDifference) {
      ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
      if (!(wantsWilcox && onlyTest))  # Only add SE Difference if not only MannWhitney is requested
        ttest$addColumnInfo(name = "sed", title = "SE Difference", type = "number")
    }
    
    if (wantsDifference && wantsWilcox && wantsStudents && wantsWelchs) {
      message <- "For the Student t-test and Welch t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test, 
                location parameter is given by the Hodges-Lehmann estimate."
      ttest$addFootnote(message)
    } else if (wantsDifference && wantsWilcox && wantsStudents) {
      message <- "For the Student t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test, 
                location parameter is given by Hodges-Lehmann estimate."
      ttest$addFootnote(message)
    } else if (wantsDifference && wantsWilcox && wantsWelchs) {
      message <- "For the Welch t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test,
                location parameter is given by Hodges-Lehmann estimate."
      ttest$addFootnote(message)
    }
    
    if (wantsConfidenceMeanDiff) {
      interval <- 100 * percentConfidenceMeanDiff
      title <- paste0(interval, "% CI for ", nameOfLocationParameter)
      ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                          title = "Lower", overtitle = title)
      ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                          title = "Upper", overtitle = title)
    }
    
    ## add Cohen's d
    if (wantsEffect) {
      ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
      if (wantsWilcox) {
        if (wantsStudents || wantsWelchs) 
          message <- paste0("For the Mann-Whitney test, effect size is given by the rank biserial correlation. 
                           For the other test(s), by ", nameOfEffectSizeParametric, ".")
        else
          message <- "For the Mann-Whitney test, effect size is given by the rank biserial correlation."
        ttest$addFootnote(message)
      }
    }
    
    if (wantsConfidenceEffSize) {
      interval <- 100 * percentConfidenceEffSize
      title <- paste0(interval, "% CI for ", nameOfEffectSize)
      ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                          title = "Lower", overtitle = title)
      ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                          title = "Upper", overtitle = title)
    }
    jaspResults[["ttest"]] <- ttest
    
    res <- try(.ttestMainFill(jaspResults, dataset, options, ready, testStat, type))
    .ttestSetError(res, ttest)
  }
}

.ttestMainFill <-function(jaspResults, dataset, options, ready, testStat, type) {
  if(type == "paired") {
    ## what does the user want? what does s/he really want??
    wantsEffect <- options$effectSize
    wantsDifference <- options$meanDifference
    wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
    wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
    percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
    percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
    wantsStudents <- options$students
    wantsWilcox <- options$wilcoxonSignedRank
    
    allTests <- c(wantsStudents, wantsWilcox)
    onlyTest <- sum(allTests) == 1
    
    #########################
    ## check the directionality
    if (options$hypothesis == "groupOneGreater")
      direction <- "greater"
    else if (options$hypothesis == "groupTwoGreater")
      direction <- "less"
    else
      direction <- "two.sided"
    
    rowNo <- 1
    ttest.rows <- list() # for each pair and each test, save stuff in there
    whichTests <- list("1" = wantsStudents, "2" = wantsWilcox)
    
    ## add a row for each variable, even before we are conducting tests
    for (pair in options$pairs)
      ttest.rows[[length(ttest.rows) + 1]] <- list(v1 = pair[[1]], sep = '-', v2 = pair[[2]])
    
    ## for each pair, run the checked tests and update the table
    for (pair in options$pairs) {
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      
      row <- list(v1 = "", sep = "", v2 = "")
      
      ## test is a number, indicating which tests should be run
      for (test in seq_len(length(whichTests))) {
        
        currentTest <- whichTests[[test]]
        
        ## don't run a test the user doesn't want
        if (!currentTest)
          next
        
        row.footnotes <- NULL
        
        if (p1 != "" && p2 != "") {
          
          c1 <- dataset[[ .v(p1) ]]
          c2 <- dataset[[ .v(p2) ]]
          df <- na.omit(data.frame(c1 = c1, c2 = c2))
          c1 <- df$c1
          c2 <- df$c2
          n  <- length(c1)
          
          result <- try(silent = FALSE, expr = {
            
            ## if Wilcox box is ticked, run a paired wilcoxon signed rank test
            if (test == 2) {
              res <- stats::wilcox.test(c1, c2, paired = TRUE,
                                        conf.level = percentConfidenceMeanDiff, conf.int = TRUE,
                                        alternative = direction)
              # only count the difference scores that are not 0.
              nd   <- sum(c1 - c2 != 0)
              maxw <- (nd * (nd + 1))/2
              d    <- as.numeric((res$statistic / maxw) * 2 - 1)
              wSE  <- sqrt((nd * (nd + 1) * (2 * nd + 1)) / 6) / 2
              mrSE <- sqrt(wSE^2  * 4 * (1 / maxw^2)) 
              # zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
              zmbiss <- atanh(d)
              if(direction == "two.sided")
                confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize)/2)*mrSE), tanh(zmbiss + qnorm((1+percentConfidenceEffSize)/2)*mrSE)))
              else if (direction == "less")
                confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(percentConfidenceEffSize)*mrSE)))
              else if (direction == "greater")
                confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize))*mrSE), Inf))
              ## else run a simple paired t-test
            } else {
              res <- stats::t.test(c1, c2, paired = TRUE, conf.level = percentConfidenceMeanDiff,
                                   alternative = direction)
              df  <- ifelse(is.null(res$parameter), "", as.numeric(res$parameter))
              d   <- mean(c1 - c2) / sd(c1 - c2)
              t   <- as.numeric(res$statistic)
              confIntEffSize <- c(0,0)
              
              if (wantsConfidenceEffSize) {
                
                ciEffSize  <- options$effSizeConfidenceIntervalPercent
                alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
                
                confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt(n), df = df, 
                                                               alpha.lower = alphaLevel,
                                                               alpha.upper = alphaLevel)[c(1, 3)]
                confIntEffSize <- unlist(confIntEffSize) / sqrt(n)
                
                if (direction == "greater")
                  confIntEffSize[2] <- Inf
                else if (direction == "less")
                  confIntEffSize[1] <- -Inf
                
                confIntEffSize <- sort(confIntEffSize)
              }
            }
            
            ## don't use a return statement in expressions
            ## ... it will take ages to debug :)
            res
          })
          
          ## same for all tests
          p    <- as.numeric(result$p.value)
          stat <- as.numeric(result$statistic)
          sed  <- sd(c1 - c2) / sqrt(length(c1))
          # num <- sqrt(sd(c1)^2 + sd(c2)^2 -  2 * cov(c1, c2))
          # d   <- mean(c1) - mean(c2) / num
          
          m <- as.numeric(result$estimate)
          ciLow <- ifelse(direction == "less", -Inf,
                          as.numeric(result$conf.int[1]))
          ciUp  <- ifelse(direction == "greater", Inf,
                          as.numeric(result$conf.int[2]))
          ciLowEffSize <- as.numeric(confIntEffSize[1])
          ciUpEffSize  <- as.numeric(confIntEffSize[2])
          
          ## paired t-test has it, wilcox doesn't!
          df  <- ifelse(is.null(result$parameter), "", as.numeric(result$parameter))
          sed <- ifelse(is.null(result$parameter), "", sed)
          
          # add things to the intermediate results object
          row <- list(df = df, p = p, md = m, d = d,
                      lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp, 
                      lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize,
                      sed = sed)
          
          if (options$VovkSellkeMPR)
            row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
          row[[testStat]] <- stat
        } else {
          row <- list(df = "", p = "", md = "", d = "", lowerCI = "", upperCI = "", sed = "")
          row[[testStat]] <- ""
        }
        
        ## if this is the first test / row for specific variables, add variable names
        ## since we have only two tests, the first test always will be an odd number
        isFirst <- (rowNo %% 2 == 1 && sum(allTests) == 2) || (sum(allTests) == 1)
        row[["v1"]]  <- ifelse(isFirst, p1, "")
        row[["sep"]] <- ifelse(isFirst, "-", "")
        row[["v2"]]  <- ifelse(isFirst, p2, "")
        
        
        if (!isFirst) {
          row[["test"]] <- "Wilcoxon"
          ttest.rows[[rowNo - 1]][["test"]] <- "Student"
        }
        
        ttest.rows[[rowNo]] <- row
        rowNo <- rowNo + 1
      }
    }
    jaspResults[["ttest"]]$addRows(ttest.rows)
  }
  else if(type == "one-sample") {
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
  else if(type == "independent") {
    wantsEffect <- options$effectSize
    wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
    percentConfidenceMeanDiff <- options$descriptivesMeanDiffConfidenceIntervalPercent
    percentConfidenceEffSize <- options$descriptivesEffectSizeConfidenceIntervalPercent
    ## can make any combination of the following tests:
    wantsWelchs <- options$welchs
    wantsStudents <- options$students
    wantsWilcox <- options$mannWhitneyU
    whichTests <- list("1" = wantsStudents, "2" = wantsWelchs, "3" = wantsWilcox)
    
    if (options$effectSizesType == "cohensD")
      effSize <- "cohen"
    else if (options$effectSizesType == "glassD")
      effSize <- "glass"
    else if (options$effectSizesType == "hedgesG")
      effSize <- "hedges"
    
    nameOfEffectSizeParametric <- switch(effSize, 
                                         cohen  = "Cohen's d", 
                                         glass  = "Glass' delta",
                                         hedges = "Hedges' g")
    
    variables  <- options$variables
    if (length(variables) == 0) 
      variables <- "."
    
    if(!ready) {
      for (variable in variables) 
        jaspResults[["ttest"]]$addRows(list(v = variable))
    } else {
      levels <- levels(dataset[[ .v(options$groupingVariable) ]])
      
      msgStart <- "For all tests, the alternative hypothesis specifies that group <em>"
      if (options$hypothesis == "groupOneGreater") {
        direction    <- "greater"
        msgDirection <- "</em> is greater than group <em>"
      } else if (options$hypothesis == "groupTwoGreater") {
        direction    <- "less"
        msgDirection <- "</em> is less than group <em>"
      } else {
        direction    <- "two.sided"
        msgDirection <- "</em> is not equal to group <em>"
      }
      message <- paste0(msgStart, levels[1], msgDirection, levels[2], "</em>.")
      jaspResults[["ttest"]]$addFootnote(message)
      
      #whichTests <- list("1" = wantsStudents, "2" = wantsWelchs, "3" = wantsWilcox)
      groupingData <- dataset[[ .v(options$groupingVariable) ]]
      ## for each variable specified, run each test that the user wants
      for (variable in options$variables) {
        variableData <- dataset[[ .v(variable) ]]
        
        ## test is a number, indicating which tests should be run
        for (test in seq_len(length(whichTests))) {
          
          currentTest <- whichTests[[test]]
          
          ## don't run a test the user doesn't want
          if (!currentTest)
            next
          
          ## try to run the test, catching eventual errors
          row <- try(silent = FALSE, expr = {
            ciEffSize  <- percentConfidenceEffSize
            ciMeanDiff <- percentConfidenceMeanDiff
            f <- as.formula(paste(.v(variable), "~",
                                  .v(options$groupingVariable)))
            
            y <- dataset[[ .v(variable) ]]
            groups <- dataset[[ .v(options$groupingVariable) ]]
            
            sds <- tapply(y, groups, sd, na.rm = TRUE)
            ms  <- tapply(y, groups, mean, na.rm = TRUE)
            ns  <- tapply(y, groups, function(x) length(na.omit(x)))
            
            
            if (test == 3) {
              whatTest <- "Mann-Whitney"
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
              if (wantsEffect) {
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
              
              if (wantsConfidenceEffSize){
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
            if (!wantsWelchs && wantsStudents) {
              levene <- car::leveneTest(variableData, groupingData, "mean")
              
              ## arbitrary cut-offs are arbitrary
              if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
                msg <- "Levene's test is significant (p < .05), 
						  suggesting a violation of the equal variance assumption"
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
        sdMessage <- paste0("Glass' delta uses the standard deviation of group ", names(ns[2]),
                            " of variable ", options$groupingVariable, ".")
        jaspResults[["ttest"]]$addFootnote(sdMessage)
      }
    }
  }
}

.ttestAssumptionCheckContainer <- function(jaspResults, options, type) {
  if(!options$normalityTests)
    return()
  if(type == "independent")
    if(!options$equalityOfVariancesTests)
      return()
  if (is.null(jaspResults[["AssumptionChecks"]])) {
    container <- createJaspContainer("Assumption Checks")
    dependList <- c("variables", "groupingVariable", "pairs",
                    "normalityTests", "equalityOfVariancesTests")
    container$dependOn(dependList)
    jaspResults[["AssumptionChecks"]] <- container
  }
}

.ttestNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTests || !is.null(container[["ttestNormalTable"]])) 
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = "Test of Normality (Shapiro-Wilk)")
  #dependList <- c()
  #ttestMainTable$dependOn(dependList)
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  
  if(type == "paired") {
    ttestNormalTable$addColumnInfo(name = "v1",  type = "string",    title = "")
    #ttestNormalTable$addColumnInfo(name = "sep", type = "separator", title = "")
    ttestNormalTable$addColumnInfo(name = "v2",  type = "string",    title = "")
  } else if(type == "independent") {
    ttestNormalTable$addColumnInfo(name = "dep", type = "string", title = "", combine = TRUE)
    ttestNormalTable$addColumnInfo(name = "lev", type = "string", title = "")
  } else if(type == "one-sample")
    ttestNormalTable$addColumnInfo(name = "v", title = "", type = "string")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = "W")
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = "p")
  
  message <- "Significant results suggest a deviation from normality."
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  res <- try(.ttestNormalFill(container, dataset, options, ready, type))
  .ttestSetError(res, ttestNormalTable)
}

.ttestNormalFill <- function(container, dataset, options, ready, type) {
  if(type == "paired") {
    pairs <- options$pairs
    rowNo <- 1
    if (!ready)
      pairs[[1]] <- list(".", ".")
    for (pair in pairs) {
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      
      if (ready && p1 != p2) {
        c1   <- dataset[[ .v(p1) ]]
        c2   <- dataset[[ .v(p2) ]]
        data <- na.omit(c1 - c2)
        
        r <- stats::shapiro.test(data)
        W <- as.numeric(r$statistic)
        p <- r$p.value
        row <- list(v1 = p1, sep = "-", v2 = p2, W = W,    p = p)
      } else 
        row <- list(v1 = p1, sep = "-", v2 = p2, W = ".",  p = ".")
      row[[".isNewGroup"]] <- rowNo == 1
      container[["ttestNormalTable"]]$addRows(row)
      rowNo <- rowNo + 1
    }
  } 
  else if(type == "independent") {
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
  else if(type == "one-sample") {
    variables <- options$variables
    rowNo <- 1
    if (!ready) 
      variables = "."
    for (variable in variables) {
      
      if (ready) {
        
        data <- na.omit(dataset[[.v(variable)]])
        
        r <- stats::shapiro.test(data)
        W <- as.numeric(r$statistic)
        p <- r$p.value
        
        row <- list(v = variable, W = W, p = p)
        
      } else 
        row <- list(v = variable, W = ".", p = ".")
      row[[".isNewGroup"]] <- rowNo == 1
      container[["ttestNormalTable"]]$addRows(row)
    }
  }
}

.ttestDescriptivesContainer <- function(jaspResults, options) {
  if(!options$descriptives && !options$descriptivesPlots) return()
  if (is.null(jaspResults[["ttestDescriptives"]])) {
    container <- createJaspContainer("Descriptives")
    container$dependOn(c("descriptives", "descriptivesPlots", 
                         "variables", "pairs", "groupingVariable"))
    jaspResults[["ttestDescriptives"]] <- container
  }
}

.ttestDescriptivesTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]])) 
    return()
  if(type == "independent")
    title <- "Group Descriptives"
  else
    title <- "Descriptives"
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = title)
  #dependList <- c()
  #ttestDescriptivesTable$dependOn(dependList)
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  if(type == "independent") {
    ttestDescriptivesTable$addColumnInfo(name = "variable", title = "", type = "string", combine = TRUE)
    ttestDescriptivesTable$addColumnInfo(name = "group", title = "Group", type = "string")
  } else 
    ttestDescriptivesTable$addColumnInfo(name = "v",    type = "string",  title = "")
  ttestDescriptivesTable$addColumnInfo(name = "N",    type = "integer", title = "N")
  ttestDescriptivesTable$addColumnInfo(name = "mean", type = "number",  title = "Mean")
  ttestDescriptivesTable$addColumnInfo(name = "sd",   type = "number",  title = "SD")
  ttestDescriptivesTable$addColumnInfo(name = "se",   type = "number",  title = "SE")

  container[["table"]] <- ttestDescriptivesTable

  if(!ready) 
    return()
  res <- try(.ttestDescriptivesFill(container, dataset, options, type))
  .ttestSetError(res, ttestDescriptivesTable)
}

.ttestDescriptivesFill <- function(container, dataset, options, type) {
  if(type %in% c("paired", "one-sample")) {
    if(type == "paired") {
      if(length(unlist(options$pairs)) == 0) return()
      descriptives.results <- list()
      desc.vars <- unique(unlist(options$pairs))
      desc.vars <- desc.vars[desc.vars != ""]
    } else if(type == "one-sample") {
      desc.vars <- unique(unlist(options$variables))
      desc.vars <- desc.vars[desc.vars != ""]
    }
    
    if(length(desc.vars) == 0)
      return()
    rowNo <- 1
    for (var in desc.vars) {
      row <- list(v = var)
      
      dat <- na.omit(dataset[[ .v(var) ]])
      n   <- as.numeric(length(dat))
      m   <- as.numeric(mean(dat, na.rm = TRUE))
      std <- as.numeric(sd(dat,   na.rm = TRUE))
      
      if (is.numeric(std))
        se <- as.numeric(std/sqrt(n))
      else
        se <- NaN
      
      row[["N"]]    <- n
      row[["mean"]] <- m
      row[["sd"]]   <- std
      row[["se"]]   <- se
      if(type == "one-sample")
        row[[".isNewGroup"]] <- rowNo == 1
      rowNo <- rowNo + 1
      container[["table"]]$addRows(row)
    }
  }
  else if(type == "independent") {
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
          if (i == 1) {
            result[[".isNewGroup"]] <- TRUE
          }
          
          data[[length(data) + 1]] <- result
        }
      }
    }
    container[["table"]]$addRows(data)
  }
}

.ttestVovkSellke <- function(table, options) {
  if (options$VovkSellkeMPR) {
    message <-"Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, 
    the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) 
    for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001)."
    table$addFootnote(message, symbol = "\u002A")
    table$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
  }
}

.ttestSetError <- function(res, table){
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}
