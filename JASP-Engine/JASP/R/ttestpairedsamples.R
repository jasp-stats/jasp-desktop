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

TTestPairedSamples <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$pairs) > 0  #at least one variable pair
  
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type = "paired")
    .ttestCheckErrors(        dataset, options, type = "paired")
  }
  # Output tables (each calls its own results function)
  .ttestPairMainTable(jaspResults, dataset, options, ready)
  .ttestNormalTable(  jaspResults, dataset, options, ready, type = "paired")
  
  # Descriptives
  .ttestDescriptivesTable(    jaspResults, dataset, options, ready, type = "paired")
  .ttestPairDescriptivesPlot( jaspResults, dataset, options, ready)
  
  return()
}

# Tables
.ttestPairMainTable <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["ttest"]])) return()
  
  # Create table
  ttest <- createJaspTable(title = "Paired Samples T-Test")
  dependList <- c("variables", 
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
  
  res <- try(.ttestPairMainFill(jaspResults, dataset, options, ready, testStat))
  .ttestSetError(res, ttest)
}

# Plot
.ttestPairDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(pair in options$pairs) {
    title <- paste(pair, collapse=" - ")
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(pairs = pair))
    subcontainer[[title]] <- descriptivesPlot
    
    if(ready){
      p <- try(.ttestPairDescriptivesPlotFill(jaspResults, dataset, options, ready, pair))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestPairDescriptivesPlotFill <- function(jaspResults, dataset, options, ready, pair){
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
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1), ggplot2::scale_y_continuous(breaks = c(min(b),
                                                                                                                                           max(b))))
  }
  
  c1 <- dataset[[ .v(pair[[1]]) ]]
  c2 <- dataset[[ .v(pair[[2]]) ]]
  ####
  data <- data.frame(id = rep(1:length(c1), 2), dependent = c(c1, c2),
                     groupingVariable = c(rep(paste("1.", pair[[1]], sep = ""), length(c1)),
                                          rep(paste("2.", pair[[2]], sep = ""), length(c2))))
  
  summaryStat <- .summarySEwithin(data, measurevar = "dependent", withinvars = "groupingVariable",
                                  idvar = "id", conf.interval = options$descriptivesPlotsConfidenceInterval,
                                  na.rm = TRUE, .drop = FALSE)
  
  pd <- ggplot2::position_dodge(0.2)
  
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
                                                 y = dependent, group = 1)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,
                                                                                                                  ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
    ggplot2::geom_line(position = pd, size = 0.7) + ggplot2::geom_point(position = pd,
                                                                        size = 4) + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat) +
    base_breaks_x(summaryStat$groupingVariable) + ggplot2::scale_x_discrete(labels = c(pair[[1]], pair[[2]]))
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}

#Table fill functions
.ttestPairMainFill <- function(jaspResults, dataset, options, ready, testStat) {
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
  for(lev in 1:length(ttest.rows))
    jaspResults[["ttest"]]$addRows(ttest.rows[[lev]])
}
