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
  type  <- "paired"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestPairedMainTable(  jaspResults, dataset, options, ready, type)
  .ttestPairedNormalTable(jaspResults, dataset, options, ready, type)
  # Descriptives
  vars <- unique(unlist(options$pairs))
  .ttestDescriptivesTable(     jaspResults, dataset, options, ready, vars)
  .ttestPairedDescriptivesPlot(jaspResults, dataset, options, ready)
  
  return()
}

.ttestPairedMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]])) 
    return()
  optionsList <- .ttestOptionsList(options, type)
  # Create table
  ttest <- createJaspTable(title = gettext("Paired Samples T-Test"))
  dependList <- c("effectSize", "effSizeConfidenceIntervalCheckbox", "variables",
                  "effSizeConfidenceIntervalPercent", "students", "mannWhitneyU",
                  "meanDifference", "meanDiffConfidenceIntervalCheckbox", "stddev",
                  "meanDiffConfidenceIntervalPercent", "hypothesis", 
                  "VovkSellkeMPR", "missingValues")
  pairList        <- c("pairs", "wilcoxonSignedRank")
  ttest$dependOn(c(dependList, pairList))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1
  
  ttest$addColumnInfo(name = "v1", type = "string", title = "")
  ttest$addColumnInfo(name = "sep",  type = "string", title = "")
  ttest$addColumnInfo(name = "v2", type = "string", title = "")
  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Wilcoxon signed-rank test."))
    testStat                <- "W"
    testStatName            <- gettext("W")
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat                <- "t"
    testStatName            <- gettext("t")
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- gettext("Cohen's d")
  } else {
    testStat                <- "Statistic"
    testStatName            <- gettext("Statistic")
    nameOfLocationParameter <- gettext("Location Parameter")
    nameOfEffectSize        <- gettext("Effect Size")
  }
  
  ## if the user wants all tests, add a column called "Test"
  if (sum(optionsList$allTests) == 2)
    ttest$addColumnInfo(name = "test", title = gettext("Test"), type = "string")
  ttest$addColumnInfo(name = testStat, title = testStatName,    type = "number")
  ttest$addColumnInfo(name = "df",     title = gettext("df"),   type = "integer")
  ttest$addColumnInfo(name = "p",      title = gettext("p"),    type = "pvalue")
  .ttestVovkSellke(ttest, options)
  
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
    if(optionsList$wantsStudents)
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")
    if (optionsList$wantsWilcox && optionsList$wantsStudents) {
      message <- gettext("For the Student t-test, location parameter is given by mean 
      difference <em>d</em>; for the Wilcoxon test, effect size is given by the 
      Hodges-Lehmann estimate.")
      ttest$addFootnote(message)
    } 
  }
  
  if (optionsList$wantsConfidenceMeanDiff) {
    interval <- 100 * optionsList$percentConfidenceMeanDiff
    title <- gettextf("%i%% CI for %s", interval, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                        title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                        title = gettext("Upper"), overtitle = title)
  }
  
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    if (optionsList$wantsWilcox && optionsList$wantsStudents) {
      message <- gettext("For the Student t-test, 
    effect size is given by Cohen's <em>d</em>; for the Wilcoxon test, 
    effect size is given by the matched rank biserial correlation.")
      ttest$addFootnote(message)
    } 
  }
  
  if (optionsList$wantsConfidenceEffSize) {
    interval <- 100 * optionsList$percentConfidenceEffSize
    title <- gettextf("%i%% CI for %s", interval, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                        title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                        title = gettext("Upper"), overtitle = title)
  }
  
  if (options$hypothesis == "groupOneGreater") {
    message   <- gettext("All tests, hypothesis is measurement one greater than measurement two.")
    ttest$addFootnote(message)
  } else if (options$hypothesis == "groupTwoGreater") {
    message   <- gettext("All tests, hypothesis is measurement one less than measurement two.")
    ttest$addFootnote(message)
  }
  
  jaspResults[["ttest"]] <- ttest
  
  if(!ready) return()
  res <- try(.ttestPairedMainFill(jaspResults, dataset, options, testStat, optionsList))
  .ttestSetError(res, ttest)
}

.ttestPairedNormalTable <- function(jaspResults, dataset, options, ready, type) {
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
  
  
  ttestNormalTable$addColumnInfo(name = "v1",  type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "sep", type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "v2",  type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = gettext("W"))
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = gettext("p"))
  
  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  if(!ready) return()
  
  res <- try(.ttestPairedNormalFill(container, dataset, options, ready))
  .ttestSetError(res, ttestNormalTable)
}

.ttestPairedMainFill <-function(jaspResults, dataset, options, testStat, optionsList) {
  direction <- .ttestMainGetDirection(options$hypothesis)
  
  rowNo      <- 1
  ttest.rows <- list() # for each pair and each test, save stuff in there
  whichTests <- list("1" = optionsList$wantsStudents, "2" = optionsList$wantsWilcox)
  
  ## add a row for each variable, even before we are conducting tests
  
  #for (pair in options$pairs)
  #  ttest.rows[[length(ttest.rows) + 1]] <- list(v1 = pair[[1]], sep = '-', v2 = pair[[2]])
  
  ## for each pair, run the checked tests and update the table
  for (pair in options$pairs) {
    p1 <- pair[[1]]
    p2 <- pair[[2]]
    
    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = c(p1, p2), 
                         observations.amount  = c('< 2'))
    
    ## test is a number, indicating which tests should be run
    for (test in seq_len(length(optionsList$whichTests))) {
      row <- list(v1 = p1, sep = "-", v2 = p2)
      currentTest <- optionsList$whichTests[[test]]
      ## don't run a test the user doesn't want
      if (!currentTest)
        next
      if (!identical(errors, FALSE) && length(pair[pair != ""]) == 2){
        jaspResults[["ttest"]]$addFootnote(errors$message, colNames = testStat, rowNames = paste(p1, p2, sep = "-"))
        isFirst <- (rowNo %% 2 == 1 && sum(optionsList$allTests) == 2) || (sum(optionsList$allTests) == 1)
        row <- list(v1  = ifelse(isFirst, p1, ""),
                    sep = ifelse(isFirst, "-", ""),
                    v2  = ifelse(isFirst, p2, ""))
        row[[testStat]] <- NaN
        jaspResults[["ttest"]]$addRows(row, rowNames = paste(p1, p2, sep = "-"))
        next
      }
      
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
                                      conf.level = optionsList$percentConfidenceMeanDiff, 
                                      conf.int = TRUE,
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
              confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize)/2)*mrSE), tanh(zmbiss + qnorm((1+optionsList$percentConfidenceEffSize)/2)*mrSE)))
            else if (direction == "less")
              confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(optionsList$percentConfidenceEffSize)*mrSE)))
            else if (direction == "greater")
              confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-optionsList$percentConfidenceEffSize))*mrSE), Inf))
            ## else run a simple paired t-test
          } else {
            res <- stats::t.test(c1, c2, paired = TRUE, conf.level = optionsList$percentConfidenceMeanDiff,
                                 alternative = direction)
            df  <- ifelse(is.null(res$parameter), "", as.numeric(res$parameter))
            d   <- mean(c1 - c2) / sd(c1 - c2)
            t   <- as.numeric(res$statistic)
            confIntEffSize <- c(0,0)
            
            if (optionsList$wantsConfidenceEffSize) {
              
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
      isFirst <- (rowNo %% 2 == 1 && sum(optionsList$allTests) %in% 1:2)
      row[["v1"]]  <- ifelse(isFirst, p1, "")
      row[["sep"]] <- ifelse(isFirst, "-", "")
      row[["v2"]]  <- ifelse(isFirst, p2, "")
      
      if (!isFirst)
        row[["test"]] <- "Wilcoxon"
        ##jaspResults[["ttest"]][["data"]][[rowNo - 1]][["test"]] <- "Student"
        #ttest.rows[[rowNo - 1]][["test"]] <- "Student"
      #}
      if(test == 1)
        row[["test"]] <- "Student"
      
      jaspResults[["ttest"]]$addRows(row)
      #ttest.rows[[rowNo]] <- row
      rowNo <- rowNo + 1
    }
  }
  
  #jaspResults[["ttest"]]$addRows(ttest.rows)
}

.ttestPairedNormalFill <- function(container, dataset, options, ready) {
  pairs <- options$pairs
  rowNo <- 1
  if (!ready)
    pairs[[1]] <- list(".", ".")
  for (pair in pairs) {
    if(length(pair) < 2) next
    p1 <- pair[[1]]
    p2 <- pair[[2]]
    
    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = c(p1, p2), 
                         observations.amount  = c('< 2', '>5000'))
    if(!identical(errors, FALSE)) {
      container[["ttestNormalTable"]]$addFootnote(errors$message, colNames = c("W", "p"), rowNames = paste(p1, p2, sep = "-"))
      container[["ttestNormalTable"]]$addRows(list(v1 = p1, sep = "-", v2 = p2, W = "NaN", p = "NaN"), rowNames = paste(p1, p2, sep = "-"))
      next
    }
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

.ttestDescriptivesTable <- function(jaspResults, dataset, options, ready, vars) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]])) 
    return()
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  ttestDescriptivesTable$position <- 4
  ttestDescriptivesTable$addColumnInfo(name = "v",    type = "string",  title = "")
  ttestDescriptivesTable$addColumnInfo(name = "N",    type = "integer", title = gettext("N"))
  ttestDescriptivesTable$addColumnInfo(name = "mean", type = "number",  title = gettext("Mean"))
  ttestDescriptivesTable$addColumnInfo(name = "sd",   type = "number",  title = gettext("SD"))
  ttestDescriptivesTable$addColumnInfo(name = "se",   type = "number",  title = gettext("SE"))
  
  container[["table"]] <- ttestDescriptivesTable
  
  if(!ready) 
    return()
  res <- try(.ttestDescriptivesFill(container, dataset, options, desc.vars = vars))
  .ttestSetError(res, ttestDescriptivesTable)
}

.ttestDescriptivesFill <- function(container, dataset, options, desc.vars) {
  desc.vars <- desc.vars[desc.vars != ""]
  
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
    rowNo <- rowNo + 1
    container[["table"]]$addRows(row)
  }
}

.ttestPairedDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["plots"]] <- createJaspContainer(gettext("Descriptives Plots"))
  subcontainer <- container[["plots"]]
  subcontainer$position <- 5
  for(pair in options$pairs) {
    title <- paste(pair, collapse = " - ")
    if(!is.null(subcontainer[[title]]))
      next
    descriptivesPlot <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(pairs = pair))
    subcontainer[[title]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestPairedDescriptivesPlotFill(dataset, options, pair))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  } 
  return()
}

.ttestPairedDescriptivesPlotFill <- function(dataset, options, pair) {
  errors <- .hasErrors(dataset, 
                       message = 'short', 
                       type = c('variance', 'infinity'),
                       all.target = pair)
  if(!identical(errors, FALSE))
    stop(errors$message)
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
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, size = 1), 
         ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
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
  
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
    ggplot2::geom_line(position = pd, size = 0.7) + 
    ggplot2::geom_point(position = pd, size = 4) + 
    ggplot2::ylab(NULL) + 
    ggplot2::xlab(NULL) + 
    base_breaks_y(summaryStat) +
    base_breaks_x(summaryStat$groupingVariable) + 
    ggplot2::scale_x_discrete(labels = c(pair[[1]], pair[[2]]))
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}


