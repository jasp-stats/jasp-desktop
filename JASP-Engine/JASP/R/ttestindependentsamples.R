#
# Copyright (C) 2018 University of Amsterdam
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

TTestIndependentSamples <- function(jaspResults, dataset, options, state) {
  
  # Set title
  jaspResults$title <- "Independent Samples T-Test"
  
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .ttisInitOptions(jaspResults, options)
  
  # Read dataset
  dataset <- .ttisReadData(options)
  
  # Error checking
  .ttisErrorHandling(dataset, options)
  
  # Compute results from which tables and plots can be created
  if (options$ready) {
    ttisResults <- .ttisComputeResults(jaspResults, dataset, options)
  }
  
  # Output containers, tables, and plots based on the results
  if (options$students == TRUE || options$welchs == FALSE) {
    .ttisTableParametric(jaspResults, dataset, options, ttisResults)
  }
  
  if (options$mannWhitneyU == TRUE) {
    .ttisTableNonParametric(jaspResults, options, ttisResults)
  }
  
  if (options$normalityTests == TRUE || options$equalityOfVariancesTests == TRUE) {
    .ttisContainerAssumptionChecks(jaspResults, options, ttisResults)
  }

  if (options$normalityTests == TRUE) {
    .ttisTableShapiro(jaspResults, options, ttisResults)
  }
    
  if (options$equalityOfVariancesTests == TRUE) {
    .ttisTableLevenes(jaspResults, options, ttisResults)
  }
  
  if (options$descriptives == TRUE || options$descriptivesPlots == TRUE) {
    .ttisContainerDescriptives(jaspResults, options, ttisResults)
  }
  
  if (options$descriptives == TRUE) {
    .ttisTableDescriptives(jaspResults, options, ttisResults)
  }
  
  if (options$descriptivesPlots) {
    .ttisContainerDescriptivesPlots(jaspResults, options, ttisResults)
    .ttisPlotDescriptives(jaspResults, options, ttisResults)
  }
  
	return()
}

.ttisInitOptions <- function(jaspResults, options) {
  
  # Update options
  if (options$hypothesis == "groupOneGreater") {
    options$hypothesisRec <- "greater"
  } else if (options$hypothesis == "groupTwoGreater") {
    options$hypothesisRec <- "less"
  } else {
    options$hypothesisRec <- "two.sided"
  }
  
  # Check if results can be computed
  options$ready <- (length(options$variables) > 0 && ! is.null(options$groupingVariable))
  
  return(options)
}

.ttisReadData <- function(options) {
  
  if (options$groupingVariable == "") {
    options$groupingVariable <- NULL
  }
  
  if (options$missingValues == "excludeListwise") {
    exclude <- options$variables
  } else {
    exclude <- NULL
  }
  
  dataset <- .readDataSetToEnd(columns.as.numeric = options$variables, columns.as.factor = options$groupingVariable,
                               exclude.na.listwise = exclude)
  
  return(dataset)
}

.ttisErrorHandling <- function(dataset, options) {
  
  # Check for errors
  if (options$ready) {
    
    # Error check 1: Number of levels of the grouping variable
    .hasErrors(dataset = dataset, perform = "run", type = 'factorLevels',
               factorLevels.target = options$groupingVariable, factorLevels.amount = '!= 2',
               exitAnalysisIfErrors = TRUE)
    
    # Error check 2: Weird data for grouping variable
    .hasErrors(dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$groupingVariable, observations.amount = c('< 3'),
               exitAnalysisIfErrors = TRUE)
    
    # Error check 3: Weird data for dependent variable in each level of the grouping variable
    .hasErrors(dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$variables, all.grouping = options$groupingVariable,
               observations.amount = c('< 3'),
               exitAnalysisIfErrors = TRUE)
  }
  
}

.ttisComputeResults <- function(jaspResults, dataset, options) {
  
  # Compute Results for Parametric Independent Samples T-Test Table
  # Student
  if (is.null(jaspResults[["stateTtisStudentResults"]])) {
    ttisStudentResults <- .ttisParametricComputeResults(dataset = dataset, options = options, test = "Student")
    #jaspResults[["stateTtisStudentResults"]] <- createJaspState(ttisStudentResults)
    #jaspResults[["stateTtisStudentResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                           "hypothesis", "students", "VovkSellkeMPR",
    #                                                           "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
    #                                                           "meanDiffConfidenceIntervalPercent",
    #                                                           "effectSize", "effectSizeConfidenceIntervalCheckbox",
    #                                                           "effectSizeConfidenceIntervalPercent", "effectSizeSD"))
  } else {
    ttisStudentResults <- jaspResults[["stateTtisStudentResults"]]$object
  }
  
  # Welch
  if (is.null(jaspResults[["stateTtisWelchResults"]])) {
    ttisWelchResults <- .ttisParametricComputeResults(dataset = dataset, options = options, test = "Welch")
    #jaspResults[["stateTtisWelchResults"]] <- createJaspState(ttisWelchResults)
    #jaspResults[["stateTtisWelchResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                           "hypothesis", "welchs", "VovkSellkeMPR",
    #                                                           "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
    #                                                           "meanDiffConfidenceIntervalPercent",
    #                                                           "effectSize", "effectSizeConfidenceIntervalCheckbox",
    #                                                           "effectSizeConfidenceIntervalPercent", "effectSizeSD"))
  } else {
    ttisWelchResults <- jaspResults[["stateTtisWelchResults"]]$object
  }
  
  # Compute Results for Non-Parametric Independent Samples T-Test Table
  if (is.null(jaspResults[["stateTtisNonParametricResults"]])) {
    ttisNonParametricResults <- .ttisNonParametricComputeResults(dataset = dataset, options = options)
    #jaspResults[["stateTtisNonParametricResults"]] <- createJaspState(ttisNonParametricResults)
    #jaspResults[["stateTtisNonParametricResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                                 "hypothesis", "mannWhitneyU", "VovkSellkeMPR",
    #                                                                 "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
    #                                                                 "meanDiffConfidenceIntervalPercent",
    #                                                                 "effectSize", "effectSizeConfidenceIntervalCheckbox",
    #                                                                 "effectSizeConfidenceIntervalPercent"))
  } else {
    ttisNonParametricResults <- jaspResults[["stateTtisNonParametricResults"]]$object
  }

  # Compute Results for Shapiro Wilk Table
  if (is.null(jaspResults[["stateTtisShapiroResults"]])) {
    ttisShapiroResults <- .ttisShapiroComputeResults(dataset = dataset, options = options)
    #jaspResults[["stateTtisShapiroResults"]] <- createJaspState(ttisShapiroResults)
    #jaspResults[["stateTtisShapiroResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                           "normalityTests"))
  } else {
    ttisShapiroResults <- jaspResults[["stateTtisShapiroResults"]]$object
  }

  # Compute Results for Levenes Table
  if (is.null(jaspResults[["stateTtisLevenesResults"]])) {
    ttisLevenesResults <- .ttisLevenesComputeResults(dataset = dataset, options = options)
    #jaspResults[["stateTtisLevenesResults"]] <- createJaspState(ttisLevenesResults)
    #jaspResults[["stateTtisLevenesResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                           "equalityOfVariancesTests"))
  } else {
    ttisLevenesResults <- jaspResults[["stateTtisLevenesResults"]]$object
  }
  
  # Compute Results for Descriptives Table
  if (is.null(jaspResults[["stateTtisDescriptivesResults"]])) {
    ttisDescriptivesResults <- .ttisDescriptivesComputeResults(dataset = dataset, options = options)
    #jaspResults[["stateTtisDescriptivesResults"]] <- createJaspState(ttisDescriptivesResults)
    #jaspResults[["stateTtisDescriptivesResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                                "descriptives"))
  } else {
    ttisDescriptivesResults <- jaspResults[["stateTtisDescriptivesResults"]]$object
  }
  
  # Compute Results for Descriptives Plots
  if (is.null(jaspResults[["stateTtisDescriptivesPlotsResults"]])) {
    ttisDescriptivesPlotsResults <- .ttisDescriptivesPlotsComputeResults(dataset = dataset, options = options)
    #jaspResults[["stateTtisDescriptivesPlotsResults"]] <- createJaspState(ttisDescriptivesPlotsResults)
    #jaspResults[["stateTtisDescriptivesPlotsResults"]]$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
    #                                                                "descriptivesPlots", 
    #                                                                "descriptivesPlotsConfidenceInterval"))
  } else {
    ttisDescriptivesPlotsResults <- jaspResults[["stateTtisDescriptivesPlotsResults"]]$object
  }
  
  return(list(ttisStudentResults = ttisStudentResults, ttisWelchResults = ttisWelchResults, 
              ttisNonParametricResults = ttisNonParametricResults, ttisShapiroResults = ttisShapiroResults, 
              ttisLevenesResults = ttisLevenesResults, ttisDescriptivesResults = ttisDescriptivesResults, 
              ttisDescriptivesPlotsResults = ttisDescriptivesPlotsResults))
}

.ttisParametricComputeResults <- function(dataset, options, test) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Get index
    index <- which(options$variables %in% variable)
    
    # Prepare for running the t-tests
    formula <- as.formula(paste(.v(variable), "~", .v(options$groupingVariable)))
    y <- dataset[[ .v(variable) ]]
    groups <- dataset[[ .v(options$groupingVariable) ]]
    
    sds <- tapply(y, groups, sd, na.rm = TRUE)
    means <- tapply(y, groups, mean, na.rm = TRUE)
    ns <- tapply(y, groups, function(x) length(na.omit(x)))
    
    # Compute results for everything except for effect sizes
    resultsTTest <- stats::t.test(formula = formula, data = dataset, alternative = options$hypothesisRec,
                                  var.equal = (test == "Student"), conf.level = options$meanDiffConfidenceIntervalPercent,
                                  paired = FALSE)
    
    stat            <- .clean(as.numeric(resultsTTest$statistic))
    df              <- .clean(as.numeric(resultsTTest$parameter))
    p               <- .clean(as.numeric(resultsTTest$p.value))
    meanDiff        <- .clean(as.numeric(resultsTTest$estimate[1]) - as.numeric(resultsTTest$estimate[2]))
    seDiff          <- .clean((as.numeric(resultsTTest$estimate[1]) - as.numeric(resultsTTest$estimate[2]))/stat)
    meanDiffLowerCI <- .clean(as.numeric(resultsTTest$conf.int[1]))
    meanDiffUpperCI <- .clean(as.numeric(resultsTTest$conf.int[2]))
    
    # Prepare for computing the effect sizes
    # Determine the sd depending on the kind of test and the effect size sd selected by the user
    if (test == "Student") {
      num <- (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
      sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
    } else if (test == "Welch") {
      if (options$effectSizeSD == "effectSizeSDPooled") {
        sdPooled <- sqrt(((sds[1]^2)+(sds[2]^2))/2)
      } else if (options$effectSizeSD == "effectSizeSDGroup1") {
        sdPooled <- sds[1]
      } else if (options$effectSizeSD == "effectSizeSDGroup2") {
        sdPooled <- sds[2]
      }
    }
    
    # Determine the proportion for the effect size ci depending on the kind of hypothesis selected by the user
    if (options$hypothesisRec != "two.sided") {
      ciEffSize <- 1-(1-options$effectSizeConfidenceIntervalPercent)*2
    } else {
      ciEffSize <- options$effectSizeConfidenceIntervalPercent
    }
    
    # Compute results for effect sizes
    effectsizes <- compute.es::mes2(m.1 = means[1], m.2 = means[2], s.pooled = sdPooled, n.1 = ns[1], n.2 = ns[2], 
                                    level = ciEffSize*100, dig = 15, verbose = FALSE)
    
    cohensd        <- .clean(as.numeric(effectsizes["d"]))
    cohensdLowerCI <- .clean(as.numeric(effectsizes["l.d"]))
    cohensdUpperCI <- .clean(as.numeric(effectsizes["u.d"]))
    hedgesg        <- .clean(as.numeric(effectsizes["g"]))
    hedgesgLowerCI <- .clean(as.numeric(effectsizes["l.g"]))
    hedgesgUpperCI <- .clean(as.numeric(effectsizes["u.g"]))
    
    if (options$hypothesisRec == "greater") {
      cohensdUpperCI <- .clean(Inf)
      hedgesgUpperCI <- .clean(Inf)
    } else if (options$hypothesisRec == "less") {
      cohensdLowerCI <- .clean(-Inf)
      hedgesgLowerCI <- .clean(-Inf)
    }
    
    # Add results for variable to results object
    results[["variable"]][[index]]        <- variable
    results[["test"]][[index]]            <- test
    results[["statistic"]][[index]]       <- stat
    results[["df"]][[index]]              <- df
    results[["p"]][[index]]               <- p
    results[["VovkSellkeMPR"]][[index]]   <- .VovkSellkeMPR(p)
    results[["meanDiff"]][[index]]        <- meanDiff
    results[["seDiff"]][[index]]          <- seDiff
    results[["meanDiffLowerCI"]][[index]] <- meanDiffLowerCI
    results[["meanDiffUpperCI"]][[index]] <- meanDiffUpperCI
    results[["cohensd"]][[index]]         <- cohensd
    results[["cohensdLowerCI"]][[index]]  <- cohensdLowerCI
    results[["cohensdUpperCI"]][[index]]  <- cohensdUpperCI
    results[["hedgesg"]][[index]]         <- hedgesg
    results[["hedgesgLowerCI"]][[index]]  <- hedgesgLowerCI
    results[["hedgesgUpperCI"]][[index]]  <- hedgesgUpperCI
  }
  
  return(results)
}

.ttisNonParametricComputeResults <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Get index
    index <- which(options$variables %in% variable)
    
    # Prepare for running the t-tests
    formula <- as.formula(paste(.v(variable), "~", .v(options$groupingVariable)))
    y <- dataset[[ .v(variable) ]]
    groups <- dataset[[ .v(options$groupingVariable) ]]
    
    sds <- tapply(y, groups, sd, na.rm = TRUE)
    means <- tapply(y, groups, mean, na.rm = TRUE)
    ns <- tapply(y, groups, function(x) length(na.omit(x)))
    
    # Compute results for everything except for effect sizes
    resultsWilcox <- stats::wilcox.test(formula = formula, data = dataset, alternative = options$hypothesisRec,
                                        conf.int = TRUE, conf.level = options$meanDiffConfidenceIntervalPercent,
                                        paired = FALSE)
    
    stat         <- .clean(as.numeric(resultsWilcox$statistic))
    p            <- .clean(as.numeric(resultsWilcox$p.value))
    hlEst        <- .clean(as.numeric(resultsWilcox$estimate))
    hlEstLowerCI <- .clean(as.numeric(resultsWilcox$conf.int[1]))
    hlEstUpperCI <- .clean(as.numeric(resultsWilcox$conf.int[2]))
    
    # Compute results for Rank Biserial Correlation
    rbc <- abs(.clean(as.numeric(1-(2*stat)/(ns[1]*ns[2])))) * sign(hlEst)
    wSE <- sqrt((ns[1]*ns[2] * (ns[1]+ns[2] + 1))/12)
    rankBisSE <- sqrt(4 * 1/(ns[1]*ns[2])^2 * wSE^2)
    zRankBis <- atanh(rbc)
    if (options$hypothesisRec == "two.sided") {
      rbcConfInt <- sort(c(tanh(zRankBis + qnorm((1-options$effectSizeConfidenceIntervalPercent)/2)*rankBisSE), tanh(zRankBis + qnorm((1+options$effectSizeConfidenceIntervalPercent)/2)*rankBisSE)))
    } else if (options$hypothesisRec == "less") {
      rbcConfInt <- sort(c(-Inf, tanh(zRankBis + qnorm(options$effectSizeConfidenceIntervalPercent)*rankBisSE)))
    } else if (options$hypothesisRec == "greater") {
      rbcConfInt <- sort(c(tanh(zRankBis + qnorm((1-options$effectSizeConfidenceIntervalPercent))*rankBisSE), Inf))
    }
    rbcLowerCI <- .clean(as.numeric(rbcConfInt[1]))
    rbcUpperCI <- .clean(as.numeric(rbcConfInt[2]))
    
    # Compute results for Cliff's Delta
    if (options$hypothesisRec == "two.sided") {
      ciEffSize <- options$effectSizeConfidenceIntervalPercent
    } else if (options$hypothesisRec == "less") {
      ciEffSize <- 1 - (1 - options$effectSizeConfidenceIntervalPercent) * 2
    } else if (options$hypothesisRec == "greater") {
      ciEffSize <- 1 - (1 - options$effectSizeConfidenceIntervalPercent) * 2
    }
    cliffsDeltaResults <- effsize::"cliff.delta"(formula = formula, data = dataset, conf.level = ciEffSize)
    cliffsDelta        <- .clean(as.numeric(cliffsDeltaResults[1]$estimate))
    cliffsDeltaLowerCI <- .clean(as.numeric(cliffsDeltaResults[2]$conf.int[1]))
    cliffsDeltaUpperCI <- .clean(as.numeric(cliffsDeltaResults[2]$conf.int[2]))
    
    if (options$hypothesisRec == "greater") {
      cliffsDeltaUpperCI <- .clean(Inf)
    } else if (options$hypothesisRec == "less") {
      cliffsDeltaLowerCI <- .clean(-Inf)
    }
    
    # Add results for variable to results object
    results[["variable"]][[index]]           <- variable
    results[["test"]][[index]]               <- "Mann-Whitney"
    results[["statistic"]][[index]]          <- stat
    results[["p"]][[index]]                  <- p
    results[["VovkSellkeMPR"]][[index]]      <- .VovkSellkeMPR(p)
    results[["hlEst"]][[index]]              <- hlEst
    results[["hlEstLowerCI"]][[index]]       <- hlEstLowerCI
    results[["hlEstUpperCI"]][[index]]       <- hlEstUpperCI
    results[["rbc"]][[index]]                <- rbc
    results[["rbcLowerCI"]][[index]]         <- rbcLowerCI
    results[["rbcUpperCI"]][[index]]         <- rbcUpperCI
    results[["cliffsDelta"]][[index]]        <- cliffsDelta
    results[["cliffsDeltaLowerCI"]][[index]] <- cliffsDeltaLowerCI
    results[["cliffsDeltaUpperCI"]][[index]] <- cliffsDeltaUpperCI
  }
  
  # Return results object
  return(results)
}

.ttisShapiroComputeResults <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  # Define index
  index <- 1
  
  for (variable in options$variables) {
    for (level in levels(dataset[[.v(options$groupingVariable)]])) {
      
      # Get the dependent variable at a certain factor level
      data <- na.omit(dataset[[.v(variable)]][dataset[[.v(options$groupingVariable)]] == level])
      
      # Compute results
      resultsShapiro <- stats::shapiro.test(data)
      W              <- .clean(as.numeric(resultsShapiro$statistic))
      p              <- .clean(as.numeric(resultsShapiro$p.value))
      
      # Add result for level of variable to results object
      results[["variable"]][[index]] <- variable
      results[["level"]][[index]]    <- level
      results[["W"]][[index]]        <- W
      results[["p"]][[index]]        <- p

      # Increase index
      index <- index + 1
    }
  }
  
  # Return results objects
  return(results)
}

.ttisLevenesComputeResults <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Get index
    index <- which(options$variables %in% variable)
    
    # Compute results
    resultsLevene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(options$groupingVariable) ]], "mean")
    F  <- .clean(as.numeric(resultsLevene[1, "F value"]))
    df <- .clean(as.numeric(resultsLevene[1, "Df"]))
    p  <- .clean(as.numeric(resultsLevene[1, "Pr(>F)"]))
    
    # Add results for variable to results object
    results[["variable"]][[index]] <- variable
    results[["F"]][[index]]        <- F
    results[["df"]][[index]]       <- df
    results[["p"]][[index]]        <- p
  }

  # Return results object
  return(results)
}

.ttisDescriptivesComputeResults <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  # Define index
  index <- 1
  
  for (variable in options$variables) {
    for (level in levels(dataset[[.v(options$groupingVariable)]])) {
      
      # Get the dependent variable at a certain factor level
      data <- na.omit(dataset[[.v(variable)]][dataset[[.v(options$groupingVariable)]] == level])
      
      # Compute results
      n <- .clean(length(data))
      mean <- .clean(mean(data))
      std <- .clean(sd(data))
      sem <- .clean(sd(data) / sqrt(length(data)))
      
      # Add results for level of variable to results object
      results[["variable"]][[index]] <- variable
      results[["group"]][[index]]    <- level
      results[["N"]][[index]]        <- n
      results[["mean"]][[index]]     <- mean
      results[["sd"]][[index]]       <- std
      results[["se"]][[index]]       <- sem
      
      # Increase index
      index <- index + 1
    }
  }
  
  # Return results object
  return(results)
}

.ttisDescriptivesPlotsComputeResults <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  # Define base breaks function for x
  base_breaks_x <- function(x) {
    b <- unique(as.numeric(x))
    d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), 
                               inherit.aes = FALSE, size = 1))
  }
  
  # Define base breaks function for y
  base_breaks_y <- function(x) {
    ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,xend = xend, yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
  }
  
  # Define plot position
  plotPosition <- ggplot2::position_dodge(0.2)
  
  for (variable in options$variables) {
    
    # Put together data frames for plot
    summaryStat <- .summarySE(as.data.frame(dataset), measurevar = .v(variable), groupvars = .v(options$groupingVariable),
                            conf.interval = options$descriptivesPlotsConfidenceInterval, na.rm = TRUE, .drop = FALSE)
    
    colnames(summaryStat)[which(colnames(summaryStat) == .v(variable))] <- "dependent"
    colnames(summaryStat)[which(colnames(summaryStat) == .v(options$groupingVariable))] <- "groupingVariable"
    
    # Make plot
    descriptivesPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), colour = "black", width = 0.2, position = plotPosition) +
      ggplot2::geom_line(position = plotPosition, size = 0.7) +
      ggplot2::geom_point(position = plotPosition, size = 4) +
      ggplot2::ylab(variable) +
      ggplot2::xlab(options$groupingVariable) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(size = 18),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 18, vjust = -0.2),
                     axis.title.y = ggplot2::element_text(size = 18, vjust = -1),
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                     plot.background = ggplot2::element_rect(fill = "transparent",  colour = NA),
                     legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                     panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     legend.title = ggplot2::element_text(size = 12),
                     legend.text = ggplot2::element_text(size = 12),
                     axis.ticks = ggplot2::element_line(size = 0.5),
                     axis.ticks.margin = grid::unit(1, "mm"),
                     axis.ticks.length = grid::unit(3, "mm"),
                     plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
      base_breaks_y(summaryStat) +
      base_breaks_x(summaryStat$groupingVariable)
    
    # Add plot for variable to results object
    results[[variable]] <- descriptivesPlot
  }
  
  # Return results object
  return(results)
}

.ttisTableParametric <- function(jaspResults, dataset, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisTableParametric"]])) {
    return(NULL)
  }
  
  # Create table
  ttisTableParametric <- createJaspTable(title = "Parametric Independent Samples T-Test")
  ttisTableParametric$showSpecifiedColumnsOnly <- TRUE
  ttisTableParametric$dependOnOptions(c("variables", "groupingVariable", "missingValues", "hypothesis", "students", "welchs",
                                        "VovkSellkeMPR", "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                                        "meanDiffConfidenceIntervalPercent", "effectSize", 
                                        "effectSizeConfidenceIntervalCheckbox","effectSizeConfidenceIntervalPercent", 
                                        "effectSizeSD"))
  
  # Bind table to jaspResults
  jaspResults[["ttisTableParametric"]] <- ttisTableParametric
  
  # Add column info
  ttisTableParametric$addColumnInfo(    name = "variable",        title = "Variable",
                                                           type = "string",          combine = TRUE)
  ttisTableParametric$addColumnInfo(    name = "test",            title = "Test",               
                                                           type = "string")
  ttisTableParametric$addColumnInfo(    name = "statistic",       title = "Statistic",          
                                                           type = "number",          format = "sf:4;dp:3")
  ttisTableParametric$addColumnInfo(    name = "df",              title = "df",                 
                                                           type = "number",          format = "sf:4;dp:3")
  ttisTableParametric$addColumnInfo(    name = "p",               title = "p",
                                                           type = "number",          format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    ttisTableParametric$addColumnInfo(  name = "VovkSellkeMPR",   title = "VS-MPR\u002A",
                                                           type = "number",          format = "sf:4;dp:3")
  }
  if (options$meanDifference) {
    ttisTableParametric$addColumnInfo(  name = "meanDiff",        title = "Mean Difference", type = "number",
                                        format = "sf:4;dp:3")
    ttisTableParametric$addColumnInfo(  name = "seDiff",          title = "SE Difference",   type = "number",
                                        format = "sf:4;dp:3")
    if (options$meanDiffConfidenceIntervalCheckbox) {
      ttisTableParametric$addColumnInfo(name = "meanDiffLowerCI", title = "Lower",           type = "number",
                                        format = "sf:4;dp:3",
                                        overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Mean Difference"))
      ttisTableParametric$addColumnInfo(name = "meanDiffUpperCI", title = "Upper",           type = "number",
                                        format = "sf:4;dp:3",
                                        overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Mean Difference"))
    }
  }
  if (options$effectSize) {
    # Cohen's d
    ttisTableParametric$addColumnInfo(  name = "cohensd",         title = "Cohen's d",
                                                           type = "number",          format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      ttisTableParametric$addColumnInfo(name = "cohensdLowerCI",  title = "Lower",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cohen's d"))
      ttisTableParametric$addColumnInfo(name = "cohensdUpperCI",  title = "Upper",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cohen's d"))
    }
    # Hedges' g
    ttisTableParametric$addColumnInfo(  name = "hedgesg",         title = "Hedges' g",
                                                           type = "number",          format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      ttisTableParametric$addColumnInfo(name = "hedgesgLowerCI",  title = "Lower",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Hedges' g"))
      ttisTableParametric$addColumnInfo(name = "hedgesgUpperCI",  title = "Upper",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Hedges' g"))
    }
  }
  
  # Add data per column
  if (options$ready) {
    ttisTableParametric[["variable"]]        <- ttisResults$ttisStudentResults$variable
    ttisTableParametric[["test"]]            <- ttisResults$ttisStudentResults$test
    ttisTableParametric[["statistic"]]       <- ttisResults$ttisStudentResults$statistic
    ttisTableParametric[["df"]]              <- ttisResults$ttisStudentResults$df
    ttisTableParametric[["p"]]               <- ttisResults$ttisStudentResults$p
    ttisTableParametric[["VovkSellkeMPR"]]   <- ttisResults$ttisStudentResults$VovkSellkeMPR
    ttisTableParametric[["meanDiff"]]        <- ttisResults$ttisStudentResults$meanDiff
    ttisTableParametric[["seDiff"]]          <- ttisResults$ttisStudentResults$seDiff
    ttisTableParametric[["meanDiffLowerCI"]] <- ttisResults$ttisStudentResults$meanDiffLowerCI
    ttisTableParametric[["meanDiffUpperCI"]] <- ttisResults$ttisStudentResults$meanDiffUpperCI
    ttisTableParametric[["cohensd"]]         <- ttisResults$ttisStudentResults$cohensd
    ttisTableParametric[["cohensdLowerCI"]]  <- ttisResults$ttisStudentResults$cohensdLowerCI
    ttisTableParametric[["cohensdUpperCI"]]  <- ttisResults$ttisStudentResults$cohensdUpperCI
    ttisTableParametric[["hedgesg"]]         <- ttisResults$ttisStudentResults$hedgesg
    ttisTableParametric[["hedgesgLowerCI"]]  <- ttisResults$ttisStudentResults$hedgesgLowerCI
    ttisTableParametric[["hedgesgUpperCI"]]  <- ttisResults$ttisStudentResults$hedgesgUpperCI
  } else {
    ttisTableParametric[["variable"]]        <- "."
    ttisTableParametric[["test"]]            <- "."
    ttisTableParametric[["statistic"]]       <- "."
    ttisTableParametric[["df"]]              <- "."
    ttisTableParametric[["p"]]               <- "."
    ttisTableParametric[["VovkSellkeMPR"]]   <- "."
    ttisTableParametric[["meanDiff"]]        <- "."
    ttisTableParametric[["seDiff"]]          <- "."
    ttisTableParametric[["meanDiffLowerCI"]] <- "."
    ttisTableParametric[["meanDiffUpperCI"]] <- "."
    ttisTableParametric[["cohensd"]]         <- "."
    ttisTableParametric[["cohensdLowerCI"]]  <- "."
    ttisTableParametric[["cohensdUpperCI"]]  <- "."
    ttisTableParametric[["hedgesg"]]         <- "."
    ttisTableParametric[["hedgesgLowerCI"]]  <- "."
    ttisTableParametric[["hedgesgUpperCI"]]  <- "."
  }
  
  # Add footnote: Check the equality of variance assumption if Welch is not selected
  if (options$students == TRUE && options$welchs == FALSE) {
    for (variable in options$variables) {
      levene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(options$groupingVariable) ]], "mean")
      if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
        message <- .messages('footnote', 'leveneSign')
        ttisTableParametric$addFootnote(message = message, col_names = "p", row_names = variable)
      }
    }
  }
  
  # Add footnote: VovkSellkeMPR
  if (options$VovkSellkeMPR) {
    ttisTableParametric$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
  # Add footnote: Alternative hypothesis
  if (options$hypothesisRec == "greater") {
    message <- paste0("For all tests, the alternative hypothesis specifies that group <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                      "</em> is greater than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
    ttisTableParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
  } else if (options$hypothesisRec == "less") {
    message <- paste0("For all tests, the alternative hypothesis specifies that group  <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                      "</em> is less than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
    ttisTableParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
  }
  
}

.ttisTableNonParametric <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisTableNonParametric"]])) {
    return(NULL)
  }
  
  # Create table
  ttisTableNonParametric <- createJaspTable(title = "Non-Parametric Independent Samples T-Test")
  ttisTableNonParametric$showSpecifiedColumnsOnly <- TRUE
  ttisTableNonParametric$dependOnOptions(c("variables", "groupingVariable", "missingValues", "hypothesis", "mannWhitneyU",
                                           "VovkSellkeMPR", "meanDifference", "meanDiffConfidenceIntervalCheckbox",
                                           "meanDiffConfidenceIntervalPercent", "effectSize",
                                           "effectSizeConfidenceIntervalCheckbox", "effectSizeConfidenceIntervalPercent"))
  
  # Bind table to jaspResults
  jaspResults[["ttisTableNonParametric"]] <- ttisTableNonParametric
  
  # Add column info
  ttisTableNonParametric$addColumnInfo(    name = "variable",           title = "Variable",
                                           type = "string",             combine = TRUE)
  ttisTableNonParametric$addColumnInfo(    name = "test",               title = "Test",
                                           type = "string")
  ttisTableNonParametric$addColumnInfo(    name = "statistic",          title = "Statistic",
                                           type = "number",             format = "sf:4;dp:3")
  ttisTableNonParametric$addColumnInfo(    name = "p",                  title = "p",
                                           type = "number",             format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    ttisTableNonParametric$addColumnInfo(  name = "VovkSellkeMPR",      title = "VS-MPR\u002A",
                                           type = "number",             format = "sf:4;dp:3")
  }
  if (options$meanDifference) {
    ttisTableNonParametric$addColumnInfo(  name = "hlEst",              title = "Hodges-Lehmann Estimate",
                                           type = "number",             format = "sf:4;dp:3")
    if (options$meanDiffConfidenceIntervalCheckbox) {
      ttisTableNonParametric$addColumnInfo(name = "hlEstLowerCI",       title = "Lower",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Hodges-Lehmann Estimate"))
      ttisTableNonParametric$addColumnInfo(name = "hlEstUpperCI",       title = "Upper",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Hodges-Lehmann Estimate"))
    }
  }
  if (options$effectSize) {
    # Rank-Biserial Correlation
    ttisTableNonParametric$addColumnInfo(  name = "rbc",                title = "Rank-Biserial Correlation",
                                           type = "number",             format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      ttisTableNonParametric$addColumnInfo(name = "rbcLowerCI",         title = "Lower",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Rank-Biserial Correlation"))
      ttisTableNonParametric$addColumnInfo(name = "rbcUpperCI",         title = "Upper",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Rank-Biserial Correlation"))
    }
    # Cliff delta
    ttisTableNonParametric$addColumnInfo(  name = "cliffsDelta",        title = "Cliff's Delta",
                                           type = "number",             format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      ttisTableNonParametric$addColumnInfo(name = "cliffsDeltaLowerCI", title = "Lower",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cliff's Delta"))
      ttisTableNonParametric$addColumnInfo(name = "cliffsDeltaUpperCI", title = "Upper",
                                           type = "number",             format = "sf:4;dp:3",
                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cliff's Delta"))
    }
  }
  
  # Add data per column
  if (options$ready) {
    ttisTableNonParametric[["variable"]]            <- ttisResults$ttisNonParametricResults$variable
    ttisTableNonParametric[["test"]]                <- ttisResults$ttisNonParametricResults$test
    ttisTableNonParametric[["statistic"]]           <- ttisResults$ttisNonParametricResults$statistic
    ttisTableNonParametric[["p"]]                   <- ttisResults$ttisNonParametricResults$p
    ttisTableNonParametric[["VovkSellkeMPR"]]       <- ttisResults$ttisNonParametricResults$VovkSellkeMPR
    ttisTableNonParametric[["hlEst"]]               <- ttisResults$ttisNonParametricResults$hlEst
    ttisTableNonParametric[["hlEstLowerCI"]]        <- ttisResults$ttisNonParametricResults$hlEstLowerCI
    ttisTableNonParametric[["hlEstUpperCI"]]        <- ttisResults$ttisNonParametricResults$hlEstUpperCI
    ttisTableNonParametric[["rbc"]]                 <- ttisResults$ttisNonParametricResults$rbc
    ttisTableNonParametric[["rbcLowerCI"]]          <- ttisResults$ttisNonParametricResults$rbcLowerCI
    ttisTableNonParametric[["rbcUpperCI"]]          <- ttisResults$ttisNonParametricResults$rbcUpperCI
    ttisTableNonParametric[["cliffsDelta"]]         <- ttisResults$ttisNonParametricResults$cliffsDelta
    ttisTableNonParametric[["cliffsDeltaLowerCI"]]  <- ttisResults$ttisNonParametricResults$cliffsDeltaLowerCI
    ttisTableNonParametric[["cliffsDeltaUpperCI"]]  <- ttisResults$ttisNonParametricResults$cliffsDeltaUpperCI
  } else {
    ttisTableNonParametric[["variable"]]            <- "."
    ttisTableNonParametric[["test"]]                <- "."
    ttisTableNonParametric[["statistic"]]           <- "."
    ttisTableNonParametric[["p"]]                   <- "."
    ttisTableNonParametric[["VovkSellkeMPR"]]       <- "."
    ttisTableNonParametric[["hlEst"]]               <- "."
    ttisTableNonParametric[["hlEstLowerCI"]]        <- "."
    ttisTableNonParametric[["hlEstUpperCI"]]        <- "."
    ttisTableNonParametric[["rbc"]]                 <- "."
    ttisTableNonParametric[["rbcLowerCI"]]          <- "."
    ttisTableNonParametric[["rbcUpperCI"]]          <- "."
    ttisTableNonParametric[["cliffsDelta"]]         <- "."
    ttisTableNonParametric[["cliffsDeltaLowerCI"]]  <- "."
    ttisTableNonParametric[["cliffsDeltaUpperCI"]]  <- "."
  }
  
  # Add footnote: VovkSellkeMPR
  if (options$VovkSellkeMPR) {
    ttisTableNonParametric$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
    
  # Add footnote: Alternative hypothesis
  if (options$hypothesisRec == "greater") {
    message <- paste0("For all tests, the alternative hypothesis specifies that group <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                      "</em> is greater than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
    ttisTableNonParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
  } else if (options$hypothesisRec == "less") {
    message <- paste0("For all tests, the alternative hypothesis specifies that group  <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                      "</em> is less than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
    ttisTableNonParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
  }
    
}

.ttisContainerAssumptionChecks <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerAssumptionChecks"]])) {
    return(NULL)
  }
  
  # Create container
  ttisContainerAssumptionChecks <- createJaspContainer(title = "Assumption Checks")
  ttisContainerAssumptionChecks$dependOnOptions(c("variables", "groupingVariable", "missingValues", "normalityTests", 
                                                  "equalityOfVariancesTests"))

  # Bind container to jaspResults
  jaspResults[["ttisContainerAssumptionChecks"]] <- ttisContainerAssumptionChecks
  
}

.ttisTableShapiro <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerAssumptionChecks"]][["ttisTableShapiro"]])) {
    return(NULL)
  }
  
  # Create table
  ttisTableShapiro <- createJaspTable("Test of Normality (Shapiro-Wilk)")
  ttisTableShapiro$showSpecifiedColumnsOnly <- TRUE
  ttisTableShapiro$dependOnOptions(c("variables", "groupingVariable", "missingValues", "normalityTests"))
  
  # Bind table to jaspResults
  jaspResults[["ttisContainerAssumptionChecks"]][["ttisTableShapiro"]] <- ttisTableShapiro
  
  # Add column info
  ttisTableShapiro$addColumnInfo(name = "variable", title = "", type = "string", combine = TRUE)
  ttisTableShapiro$addColumnInfo(name = "level",    title = "", type = "string")
  ttisTableShapiro$addColumnInfo(name = "W",        title = "W", type = "number", format = "sf:4;dp:3")
  ttisTableShapiro$addColumnInfo(name = "p",        title = "p", type = "number", format = "dp:3;p:.001")
  
  # Add data per column
  if (options$ready) {
    ttisTableShapiro[["variable"]] <- ttisResults$ttisShapiroResults$variable
    ttisTableShapiro[["level"]]    <- ttisResults$ttisShapiroResults$level
    ttisTableShapiro[["W"]]        <- ttisResults$ttisShapiroResults$W
    ttisTableShapiro[["p"]]        <- ttisResults$ttisShapiroResults$p
  } else {
    ttisTableShapiro[["variable"]] <- "."
    ttisTableShapiro[["level"]]    <- "."
    ttisTableShapiro[["W"]]        <- "."
    ttisTableShapiro[["p"]]        <- "."
  }

  # Add footnote: Interpretation of Results
  ttisTableShapiro$addFootnote(message = "Significant results suggest a deviation from normality.", symbol = "<em>Note.</em>")
  
}

.ttisTableLevenes <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerAssumptionChecks"]][["ttisTableLevenes"]])) {
    return(NULL)
  }
  
  # Create table
  ttisTableLevenes <- createJaspTable("Test of Equality of Variances (Levene's)")
  ttisTableLevenes$showSpecifiedColumnsOnly <- TRUE
  ttisTableLevenes$dependOnOptions(c("variables", "groupingVariable", "missingValues", "equalityOfVariancesTests"))
  
  # Bind table to jaspResults
  jaspResults[["ttisContainerAssumptionChecks"]][["ttisTableLevenes"]] <- ttisTableLevenes
  
  # Add column info
  ttisTableLevenes$addColumnInfo(name = "variable", title = "",   type = "string")
  ttisTableLevenes$addColumnInfo(name = "F",        title = "F",  type = "number", format = "sf:4;dp:3")
  ttisTableLevenes$addColumnInfo(name = "df",       title = "df", type = "integer")
  ttisTableLevenes$addColumnInfo(name = "p",        title = "p",  type = "number", format = "dp:3;p:.001")
  
  # Add data per column
  if (options$ready) {
    ttisTableLevenes[["variable"]] <- ttisResults$ttisLevenesResults$variable
    ttisTableLevenes[["F"]]        <- ttisResults$ttisLevenesResults$F
    ttisTableLevenes[["df"]]       <- ttisResults$ttisLevenesResults$df
    ttisTableLevenes[["p"]]        <- ttisResults$ttisLevenesResults$p
  } else {
    ttisTableLevenes[["variable"]] <- "."
    ttisTableLevenes[["F"]]        <- "."
    ttisTableLevenes[["df"]]       <- "."
    ttisTableLevenes[["p"]]        <- "."
  }
  
  # Add footnote: Interpretation of Results
  ttisTableLevenes$addFootnote(message = "Significant results suggest a deviation from equality of variance.",
                               symbol = "<em>Note.</em>")
}

.ttisContainerDescriptives <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerDescriptives"]])) {
    return(NULL)
  }
  
  # Create container
  ttisContainerDescriptives <- createJaspContainer(title = "Descriptives")
  ttisContainerDescriptives$dependOnOptions(c("variables", "groupingVariable", "missingValues", "descriptives",
                                              "descriptivesPlots", "descriptivesPlotsConfidenceInterval"))
  
  # Bind container to jaspResults
  jaspResults[["ttisContainerDescriptives"]] <- ttisContainerDescriptives
  
}

.ttisTableDescriptives <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerDescriptives"]][["ttisTableDescriptives"]])) {
    return(NULL)
  }
  
  # Create table
  ttisTableDescriptives <- createJaspTable("Descriptive Statistics")
  ttisTableDescriptives$showSpecifiedColumnsOnly <- TRUE
  ttisTableDescriptives$dependOnOptions(c("variables", "groupingVariable", "missingValues", "descriptives"))
  
  # Bind table to jaspResults
  jaspResults[["ttisContainerDescriptives"]][["ttisTableDescriptives"]] <- ttisTableDescriptives
  
  # Add column info
  ttisTableDescriptives$addColumnInfo(name = "variable", title = "",               type = "string",   combine = TRUE)
  ttisTableDescriptives$addColumnInfo(name = "group",    title = "Group",          type = "string")
  ttisTableDescriptives$addColumnInfo(name = "N",        title = "N",              type = "integer")
  ttisTableDescriptives$addColumnInfo(name = "mean",     title = "Mean",           type = "number",   format = "sf:4;dp:3")
  ttisTableDescriptives$addColumnInfo(name = "sd",       title = "Std. Deviation", type = "number",   format = "sf:4;dp:3")
  ttisTableDescriptives$addColumnInfo(name = "se",       title = "Std. Error",     type = "number",   format = "sf:4;dp:3")
  
  # Add data per column
  if (options$ready) {
    ttisTableDescriptives[["variable"]] <- ttisResults$ttisDescriptivesResults$variable
    ttisTableDescriptives[["group"]]    <- ttisResults$ttisDescriptivesResults$group
    ttisTableDescriptives[["N"]]        <- ttisResults$ttisDescriptivesResults$N
    ttisTableDescriptives[["mean"]]     <- ttisResults$ttisDescriptivesResults$mean
    ttisTableDescriptives[["sd"]]       <- ttisResults$ttisDescriptivesResults$sd
    ttisTableDescriptives[["se"]]       <- ttisResults$ttisDescriptivesResults$se
  } else {
    ttisTableDescriptives[["variable"]] <- "."
    ttisTableDescriptives[["group"]]    <- "."
    ttisTableDescriptives[["N"]]        <- "."
    ttisTableDescriptives[["mean"]]     <- "."
    ttisTableDescriptives[["sd"]]       <- "."
    ttisTableDescriptives[["se"]]       <- "."
  }

}

.ttisContainerDescriptivesPlots <- function(jaspResults, options, ttisResults) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["ttisContainerDescriptives"]][["ttisContainerDescriptivesPlots"]])) {
    return(NULL)
  }
  
  # Create container
  ttisContainerDescriptivesPlots <- createJaspContainer(title = "Descriptives Plot(s)")
  ttisContainerDescriptivesPlots$dependOnOptions(c("variables", "groupingVariable", "missingValues", "descriptivesPlots",
                                                   "descriptivesPlotsConfidenceInterval"))
  
  # Bind container to jaspResults
  jaspResults[["ttisContainerDescriptives"]][["ttisContainerDescriptivesPlots"]] <- ttisContainerDescriptivesPlots
  
}

.ttisPlotDescriptives <- function(jaspResults, options, ttisResults) {
  
  for (variable in options$variables) {
    
    # Check if object can be reused (in case relevant options did not change)
    if (!is.null(jaspResults[["ttisContainerDescriptives"]][["ttisContainerDescriptivesPlots"]][[variable]])) {
      return(NULL)
    }
    
    ttisPlotDescriptives <- createJaspPlot(title = variable, height = 300, width = 350)
    ttisPlotDescriptives$dependOnOptions(c("variables", "groupingVariable", "missingValues", "descriptivesPlots",
                                           "descriptivesPlotsConfidenceInterval"))
    
    # Bind plot to jaspResults
    jaspResults[["ttisContainerDescriptives"]][["ttisContainerDescriptivesPlots"]][[variable]] <- ttisPlotDescriptives
    ttisPlotDescriptives$plotObject <- plot(ttisResults$ttisDescriptivesPlots[[variable]])
  }
}