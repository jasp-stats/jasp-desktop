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

TTestIndependentSamples <- function(jaspResults, dataset, options, state = NULL) {
  
  # Update options
  if (options$hypothesis == "groupOneGreater") {
    options$hypothesisRec <- "greater"
  } else if (options$hypothesis == "groupTwoGreater") {
    options$hypothesisRec <- "less"
  } else {
    options$hypothesisRec <- "two.sided"
  }
  
  # Define state if empty
  if (is.null(state)) {
    state <- list()
  }
  
  # Read dataset
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
  
  # Set title
  jaspResults$title <- "Independent Samples T-Test"
  
  # Check if results can be computed
  ready <- (length(options$variables) > 0 && ! is.null(options$groupingVariable))
  
  # Check for errors
	if (ready) {
	  
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
  
  # Compute Results for Parametric Independent Samples T-Test Table
  # Student
  if (ready == TRUE && (options$student) && is.null(jaspResults[["independentSamplesTTestTableParametric"]])) {
    resultsStudent <- .computeResultsIndependentSamplesTTestTableParametric(dataset = dataset, options = options, 
                                                                            test = "Student")
  }
  # Welch
  if (ready == TRUE && (options$welchs) && is.null(jaspResults[["independentSamplesTTestTableParametric"]])) {
    resultsWelch <- .computeResultsIndependentSamplesTTestTableParametric(dataset = dataset, options = options, test = "Welch")
  }

  # Compute Results for Non-Parametric Independent Samples T-Test Table
  if (ready == TRUE && options$mannWhitneyU && is.null(jaspResults[["independentSamplesTTestTableNonParametric"]])) {
    resultsNonParametric <- .computeResultsIndependentSamplesTTestTableNonParametric(dataset = dataset, options = options)
  }
  
  # Compute Results for Shapiro Wilk Table
  if (ready == TRUE && options$normalityTests && is.null(jaspResults[["independentSamplesTTestAssumptionChecksContainer"]])) {
    resultsShapiroWilk <- .computeResultsIndependentSamplesTTestShapiroWilkTable(dataset = dataset, options = options)
  }
  
  # Compute Results for Levenes Table
  if (ready == TRUE && options$equalityOfVariancesTests && is.null(jaspResults[["independentSamplesTTestAssumptionChecksContainer"]])) {
    resultsLevene <- .computeResultsIndependentSamplesTTestLevenesTable(dataset = dataset, options = options)
  }

  # Compute Results for Descriptives Table
  if (ready == TRUE && options$descriptives && is.null(jaspResults[["independentSamplesTTestDescriptivesContainer"]])) {
    resultsDescriptives <- .computeResultsIndependentSamplesTTestDescriptivesTable(dataset = dataset, options = options)
  }
  
  # Compute Results for Descriptives Plots
  if (ready == TRUE && options$descriptivesPlots && is.null(jaspResults[["independentSamplesTTestDescriptivesContainer"]])) {
    resultsDescriptivesPlots <- .computeResultsIndependentSamplesTTestDescriptivesPlots(dataset = dataset, options = options)
  }
  
  # Create Parametric Independent Samples T-Test Table (if wanted)
  if (options$student || options$welchs) {
    .createIndependentSamplesTTestTableParametric(jaspResults = jaspResults, dataset = dataset, options = options,
                                                  ready = ready, resultsStudent = resultsStudent, resultsWelch = resultsWelch)
  }
  
  # Create Non-Parametric Independent Samples T-Test Table (if wanted)
  if (options$mannWhitneyU) {
    .createIndependentSamplesTTestTableNonParametric(jaspResults = jaspResults, dataset = dataset, options = options,
                                                     ready = ready, resultsNonParametric = resultsNonParametric)
  }
  
  # Create Assumption Checks Container (if wanted)
  if (options$normalityTests || options$equalityOfVariancesTests) {
    .createIndependentSamplesTTestAssumptionChecksContainer(jaspResults = jaspResults, dataset = dataset, options = options, 
                                                            ready = ready, resultsShapiroWilk = resultsShapiroWilk,
                                                            resultsLevene = resultsLevene)
  }
  
  # Create Descriptives Container and Plots (if wanted)
  if (options$descriptives || options$descriptivesPlots) {
    .createIndependentSamplesTTestDescriptivesContainer(jaspResults = jaspResults, dataset = dataset, options = options,
                                                        ready = ready, resultsDescriptives = resultsDescriptives,
                                                        resultsDescriptivesPlots = resultsDescriptivesPlots)
  }

	# Bring state up-to-date
	state[["options"]] <- options
	
	return(state)
}

.computeResultsIndependentSamplesTTestTableParametric <- function(dataset, options, test) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
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
    results[[variable]] <- list(variable = variable, test = test, statistic = stat, df = df, p = p, VovkSellkeMPR = .VovkSellkeMPR(p),
                                meanDiff = meanDiff, seDiff = seDiff, meanDiffLowerCI = meanDiffLowerCI, meanDiffUpperCI = meanDiffUpperCI,
                                cohensd = cohensd, cohensdLowerCI = cohensdLowerCI, cohensdUpperCI = cohensdUpperCI,
                                hedgesg = hedgesg, hedgesgLowerCI = hedgesgLowerCI, hedgesgUpperCI = hedgesgUpperCI)
  }
  
  return(results)
}

.computeResultsIndependentSamplesTTestTableNonParametric <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
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
    results[[variable]] <- list(variable = variable, test = "Mann-Whitney", statistic = stat, p = p, VovkSellkeMPR = .VovkSellkeMPR(p),
                                hlEst = hlEst, hlEstLowerCI = hlEstLowerCI, hlEstUpperCI = hlEstUpperCI,
                                rbc = rbc, rbcLowerCI = rbcLowerCI, rbcUpperCI = rbcUpperCI,
                                cliffsDelta = cliffsDelta, cliffsDeltaLowerCI = cliffsDeltaLowerCI, cliffsDeltaUpperCI = cliffsDeltaUpperCI)
  }
  
  # Return results object
  return(results)
}

.computeResultsIndependentSamplesTTestShapiroWilkTable <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    for (level in levels(dataset[[.v(options$groupingVariable)]])) {
      
      # Get the dependent variable at a certain factor level
      data <- na.omit(dataset[[.v(variable)]][dataset[[.v(options$groupingVariable)]] == level])
      
      # Compute results
      resultsShapiro <- stats::shapiro.test(data)
      W              <- .clean(as.numeric(resultsShapiro$statistic))
      p              <- .clean(as.numeric(resultsShapiro$p.value))
      
      # Add result for level of variable to results object
      results[[variable]][[level]] <- list(dv = variable, level = level, W = W, p = p)
    }
  }
  
  # Return results objects
  return(results)
}

.computeResultsIndependentSamplesTTestLevenesTable <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Compute results
    resultsLevene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(options$groupingVariable) ]], "mean")
    F  <- .clean(as.numeric(resultsLevene[1, "F value"]))
    df <- .clean(as.numeric(resultsLevene[1, "Df"]))
    p  <- .clean(as.numeric(resultsLevene[1, "Pr(>F)"]))
    
    # Add results for variable to results object
    results[[variable]] <- list(variable = variable, F = F, df = df, p = p)
  }

  # Return results object
  return(results)
}

.computeResultsIndependentSamplesTTestDescriptivesTable <- function(dataset, options) {
  
  # This is the return object
  results <- list()
  
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
      results[[variable]][[level]] <- list(variable = variable, group = level, N = n, mean = mean, sd = std, se = sem)
    }
  }
  
  # Return results object
  return(results)
}

.computeResultsIndependentSamplesTTestDescriptivesPlots <- function(dataset, options) {
  
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

.createIndependentSamplesTTestTableParametric <- function(jaspResults, dataset, options, ready, resultsStudent, resultsWelch) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["independentSamplesTTestTableParametric"]])) {
    return(NULL)
  }
  
  # Create table
  independentSamplesTTestTableParametric <- createJaspTable("Parametric Independent Samples T-Test")
  jaspResults[["independentSamplesTTestTableParametric"]] <- independentSamplesTTestTableParametric
  independentSamplesTTestTableParametric$showSpecifiedColumnsOnly <- TRUE
  independentSamplesTTestTableParametric$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                           "hypothesis", "students", "welchs", "VovkSellkeMPR",
                                                           "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                                                           "meanDiffConfidenceIntervalPercent",
                                                           "effectSize", "effectSizeConfidenceIntervalCheckbox",
                                                           "effectSizeConfidenceIntervalPercent", "effectSizeSD"))
  
  # Add columns to table
  independentSamplesTTestTableParametric$addColumnInfo(    name = "variable",        title = "Variable",
                                                           type = "string",          combine = TRUE)
  independentSamplesTTestTableParametric$addColumnInfo(    name = "test",            title = "Test",               
                                                           type = "string")
  independentSamplesTTestTableParametric$addColumnInfo(    name = "statistic",       title = "Statistic",          
                                                           type = "number",          format = "sf:4;dp:3")
  independentSamplesTTestTableParametric$addColumnInfo(    name = "df",              title = "df",                 
                                                           type = "number",          format = "sf:4;dp:3")
  independentSamplesTTestTableParametric$addColumnInfo(    name = "p",               title = "p",
                                                           type = "number",          format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    independentSamplesTTestTableParametric$addColumnInfo(  name = "VovkSellkeMPR",   title = "VS-MPR\u002A",
                                                           type = "number",          format = "sf:4;dp:3")
  }
  if (options$meanDifference) {
    independentSamplesTTestTableParametric$addColumnInfo(  name = "meanDiff",        title = "Mean Difference", 
                                                           type = "number",          format = "sf:4;dp:3")
    independentSamplesTTestTableParametric$addColumnInfo(  name = "seDiff",          title = "SE Difference",      
                                                           type = "number",          format = "sf:4;dp:3")
    if (options$meanDiffConfidenceIntervalCheckbox) {
      independentSamplesTTestTableParametric$addColumnInfo(name = "meanDiffLowerCI", title = "Lower",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Mean Difference"))
      independentSamplesTTestTableParametric$addColumnInfo(name = "meanDiffUpperCI", title = "Upper",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Mean Difference"))
    }
  }
  if (options$effectSize) {
    # Cohen's d
    independentSamplesTTestTableParametric$addColumnInfo(  name = "cohensd",         title = "Cohen's d",
                                                           type = "number",          format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      independentSamplesTTestTableParametric$addColumnInfo(name = "cohensdLowerCI",  title = "Lower",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cohen's d"))
      independentSamplesTTestTableParametric$addColumnInfo(name = "cohensdUpperCI",  title = "Upper",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cohen's d"))
    }
    # Hedges' g
    independentSamplesTTestTableParametric$addColumnInfo(  name = "hedgesg",         title = "Hedges' g",
                                                           type = "number",          format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      independentSamplesTTestTableParametric$addColumnInfo(name = "hedgesgLowerCI",  title = "Lower",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Hedges' g"))
      independentSamplesTTestTableParametric$addColumnInfo(name = "hedgesgUpperCI",  title = "Upper",
                                                           type = "number",          format = "sf:4;dp:3",
                                                           overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Hedges' g"))
    }
  }
  
  # Fill up table with results
  .fillUpIndependentSamplesTTestTableParametric(independentSamplesTTestTableParametric = independentSamplesTTestTableParametric, 
                                                dataset = dataset, options = options, ready = ready,
                                                resultsStudent = resultsStudent, resultsWelch = resultsWelch)
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestTableParametric <- function(independentSamplesTTestTableParametric, dataset, options, ready,
                                                          resultsStudent, resultsWelch) {
  
  # If results can be computed, run each test that the users wants for each variable
  if (ready) {
    
    for (variable in options$variables) {
      
      # Add results from Student t-test (if wanted)
      if (options$student == TRUE) {
        independentSamplesTTestTableParametric$addRows(rows = resultsStudent[[variable]], 
                                                       rowNames = paste0(variable, " - ", "Student"))
      }
      
      # Add results from Welch t-test (if wanted)
      if (options$welchs == TRUE) {
        independentSamplesTTestTableParametric$addRows(rows = resultsWelch[[variable]], 
                                                       rowNames = paste0(variable, " - ", "Welch"))
      }
    }
    
    # Add footnote: Check the equality of variance assumption if Welch is not selected
    if (options$students == TRUE && options$welchs == FALSE) {
      levene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(options$groupingVariable) ]], "mean")
      if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
        message <- .messages('footnote', 'leveneSign')
        independentSamplesTTestTableParametric$addFootnote(message = message, col_names = "p", row_names = variable)
      }
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      independentSamplesTTestTableParametric$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
    # Add footnote: Alternative hypothesis
    if (options$hypothesisRec == "greater") {
      message <- paste0("For all tests, the alternative hypothesis specifies that group <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                        "</em> is greater than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
      independentSamplesTTestTableParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
    } else if (options$hypothesisRec == "less") {
      message <- paste0("For all tests, the alternative hypothesis specifies that group  <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                        "</em> is less than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
      independentSamplesTTestTableParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", test = ".", statistic = ".", df = ".", p = ".", VovkSellkeMPR = ".",
                meanDiff = ".", seDiff = ".", meanDiffLowerCI = ".", meanDiffUpperCI = ".",
                cohensd = ".", cohensdLowerCI = ".", cohensdUpperCI = ".",
                hedgesg = ".", hedgesgLowerCI = ".", hedgesgUpperCI = ".")
    independentSamplesTTestTableParametric$addRows(rows = row)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestTableNonParametric <- function(jaspResults, dataset, options, ready, resultsNonParametric) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["independentSamplesTTestTableNonParametric"]])) {
    return(NULL)
  }
  
  # Create table
  independentSamplesTTestTableNonParametric <- createJaspTable("Non-Parametric Independent Samples T-Test")
  jaspResults[["independentSamplesTTestTableNonParametric"]] <- independentSamplesTTestTableNonParametric
  independentSamplesTTestTableNonParametric$showSpecifiedColumnsOnly <- TRUE
  independentSamplesTTestTableNonParametric$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                           "hypothesis", "mannWhitneyU", "VovkSellkeMPR",
                                                           "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
                                                           "meanDiffConfidenceIntervalPercent",
                                                           "effectSize", "effectSizeConfidenceIntervalCheckbox",
                                                           "effectSizeConfidenceIntervalPercent"))
  
  # Add columns to table
  independentSamplesTTestTableNonParametric$addColumnInfo(    name = "variable",           title = "Variable",
                                                              type = "string",             combine = TRUE)
  independentSamplesTTestTableNonParametric$addColumnInfo(    name = "test",               title = "Test",
                                                              type = "string")
  independentSamplesTTestTableNonParametric$addColumnInfo(    name = "statistic",          title = "Statistic",
                                                              type = "number",             format = "sf:4;dp:3")
  independentSamplesTTestTableNonParametric$addColumnInfo(    name = "p",                  title = "p",
                                                              type = "number",             format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    independentSamplesTTestTableNonParametric$addColumnInfo(  name = "VovkSellkeMPR",      title = "VS-MPR\u002A",
                                                              type = "number",             format = "sf:4;dp:3")
  }
  if (options$meanDifference) {
    independentSamplesTTestTableNonParametric$addColumnInfo(  name = "hlEst",              title = "Hodges-Lehmann Estimate",
                                                              type = "number",             format = "sf:4;dp:3")
    if (options$meanDiffConfidenceIntervalCheckbox) {
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "hlEstLowerCI",       title = "Lower",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Hodges-Lehmann Estimate"))
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "hlEstUpperCI",       title = "Upper",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$meanDiffConfidenceIntervalPercent, "% CI for Hodges-Lehmann Estimate"))
    }
  }
  if (options$effectSize) {
    # Rank-Biserial Correlation
    independentSamplesTTestTableNonParametric$addColumnInfo(  name = "rbc",                title = "Rank-Biserial Correlation",
                                                              type = "number",             format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "rbcLowerCI",         title = "Lower",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Rank-Biserial Correlation"))
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "rbcUpperCI",         title = "Upper",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Rank-Biserial Correlation"))
    }
    # Cliff delta
    independentSamplesTTestTableNonParametric$addColumnInfo(  name = "cliffsDelta",        title = "Cliff's Delta",
                                                              type = "number",             format = "sf:4;dp:3")
    if (options$effectSizeConfidenceIntervalCheckbox) {
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "cliffsDeltaLowerCI", title = "Lower",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cliff's Delta"))
      independentSamplesTTestTableNonParametric$addColumnInfo(name = "cliffsDeltaUpperCI", title = "Upper",
                                                              type = "number",             format = "sf:4;dp:3",
                                                              overtitle = paste0(100*options$effectSizeConfidenceIntervalPercent, "% CI for Cliff's Delta"))
    }
  }
  
  # Fill up table with results
  .fillUpIndependentSamplesTTestTableNonParametric(independentSamplesTTestTableNonParametric = independentSamplesTTestTableNonParametric,
                                                   dataset = dataset, options = options, ready = ready,
                                                   resultsNonParametric = resultsNonParametric)
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestTableNonParametric <- function(independentSamplesTTestTableNonParametric, dataset, options, 
                                                             ready, resultsNonParametric) {
  
  # If results can be computed, run each test that the users wants for each variable
  if (ready) {
    
    for (variable in options$variables) {
      independentSamplesTTestTableNonParametric$addRows(rows = resultsNonParametric[[variable]], rowNames = variable)
    }

    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      independentSamplesTTestTableNonParametric$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
    # Add footnote: Alternative hypothesis
    if (options$hypothesisRec == "greater") {
      message <- paste0("For all tests, the alternative hypothesis specifies that group <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                        "</em> is greater than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
      independentSamplesTTestTableNonParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
    } else if (options$hypothesisRec == "less") {
      message <- paste0("For all tests, the alternative hypothesis specifies that group  <em>", levels(dataset[[.v(options$groupingVariable)]])[1],
                        "</em> is less than group <em>", levels(dataset[[.v(options$groupingVariable)]])[2], "</em>.")
      independentSamplesTTestTableNonParametric$addFootnote(message = message, symbol = "<em>Note.</em>")
    }
    
    # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", test = ".", statistic = ".", p = ".", VovkSellkeMPR = ".",
                hlEst = ".", hlEstLowerCI = ".", hlEstUpperCI = ".",
                rbc = ".", rbcLowerCI = ".", rbcUpperCI = ".",
                cliffsDelta = ".", cliffsDeltaLowerCI = ".", cliffsDeltaUpperCI = ".")
    independentSamplesTTestTableNonParametric$addRows(rows = row)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestAssumptionChecksContainer <- function(jaspResults, dataset, options, ready, resultsShapiroWilk,
                                                                    resultsLevene) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["independentSamplesTTestAssumptionChecksContainer"]])) {
    return(NULL)
  }
  
  # Create container
  independentSamplesTTestAssumptionChecksContainer <- createJaspContainer(title = "Assumption Checks")
  jaspResults[["independentSamplesTTestAssumptionChecksContainer"]] <- independentSamplesTTestAssumptionChecksContainer
  independentSamplesTTestAssumptionChecksContainer$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                                     "normalityTests", "equalityOfVariancesTests"))
  
  # Create Shapiro-Wilk Table (if wanted)
  if (options$normalityTests == TRUE) {
    .createIndependentSamplesTTestShapiroWilkTable(independentSamplesTTestAssumptionChecksContainer = independentSamplesTTestAssumptionChecksContainer, 
                                                   dataset = dataset, options = options, ready = ready,
                                                   resultsShapiroWilk = resultsShapiroWilk)
  }
  
  # Create Levene's Table (if wanted)
  if (options$equalityOfVariancesTests == TRUE) {
    .createIndependentSamplesTTestLevenesTable(independentSamplesTTestAssumptionChecksContainer = independentSamplesTTestAssumptionChecksContainer,
                                               dataset = dataset, options = options, ready = ready, 
                                               resultsLevene = resultsLevene)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestShapiroWilkTable <- function(independentSamplesTTestAssumptionChecksContainer, 
                                                           dataset, options, ready, resultsShapiroWilk) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(independentSamplesTTestAssumptionChecksContainer[["independentSamplesTTestShapiroWilkTable"]])) {
    return(NULL)
  }
  
  # Create table
  independentSamplesTTestShapiroWilkTable <- createJaspTable("Test of Normality (Shapiro-Wilk)")
  independentSamplesTTestAssumptionChecksContainer[["independentSamplesTTestShapiroWilkTable"]] <- independentSamplesTTestShapiroWilkTable
  independentSamplesTTestShapiroWilkTable$showSpecifiedColumnsOnly <- TRUE
  independentSamplesTTestShapiroWilkTable$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                            "normalityTests"))
  
  # Add columns to table
  independentSamplesTTestShapiroWilkTable$addColumnInfo(name = "dv",     title = "",
                                                        type = "string", combine = TRUE)
  independentSamplesTTestShapiroWilkTable$addColumnInfo(name = "level",  title = "",
                                                        type = "string")
  independentSamplesTTestShapiroWilkTable$addColumnInfo(name = "W",      title = "W",
                                                        type = "number", format = "sf:4;dp:3")
  independentSamplesTTestShapiroWilkTable$addColumnInfo(name = "p",      title = "p",
                                                        type = "number", format = "dp:3;p:.001")
  
  # Fill up table with results
  .fillUpIndependentSamplesTTestShapiroWilkTable(independentSamplesTTestShapiroWilkTable = independentSamplesTTestShapiroWilkTable,
                                                 dataset = dataset, options = options, ready = ready, 
                                                 resultsShapiroWilk = resultsShapiroWilk)
  
  # Add footnote: Interpretation of Results
  independentSamplesTTestShapiroWilkTable$addFootnote(message = "Significant results suggest a deviation from normality.",
                                                      symbol = "<em>Note.</em>")
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestShapiroWilkTable <- function(independentSamplesTTestShapiroWilkTable, dataset, options, ready,
                                                           resultsShapiroWilk) {
  
  # If results can be computed, compute them and add row for each level of each variable
  if (ready) {
    for (variable in options$variables) {
      for (level in levels(dataset[[.v(options$groupingVariable)]])) {
        independentSamplesTTestShapiroWilkTable$addRows(rows = resultsShapiroWilk[[variable]][[level]],
                                                        rowNames = paste0(variable, " - ", level))
      }
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(dv = ".", level = ".", W = ".", p = ".")
    independentSamplesTTestShapiroWilkTable$addRows(rows = row)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestLevenesTable <- function(independentSamplesTTestAssumptionChecksContainer, 
                                                       dataset, options, ready, resultsLevene) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(independentSamplesTTestAssumptionChecksContainer[["independentSamplesTTestLevenesTable"]])) {
    return(NULL)
  }
  
  # Create table
  independentSamplesTTestLevenesTable <- createJaspTable("Test of Equality of Variances (Levene's)")
  independentSamplesTTestAssumptionChecksContainer[["independentSamplesTTestLevenesTable"]] <- independentSamplesTTestLevenesTable
  independentSamplesTTestLevenesTable$showSpecifiedColumnsOnly <- TRUE
  independentSamplesTTestLevenesTable$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                        "equalityOfVariancesTests"))
  
  # Add columns to table
  independentSamplesTTestLevenesTable$addColumnInfo(name = "variable", title = "",
                                                    type = "string")
  independentSamplesTTestLevenesTable$addColumnInfo(name = "F",        title = "F",
                                                    type = "number",   format = "sf:4;dp:3")
  independentSamplesTTestLevenesTable$addColumnInfo(name = "df",       title = "df",
                                                    type = "integer")
  independentSamplesTTestLevenesTable$addColumnInfo(name = "p",        title = "p",
                                                    type = "number",   format = "dp:3;p:.001")
  
  # Fill up table with results
  .fillUpIndependentSamplesTTestLevenesTable(independentSamplesTTestLevenesTable = independentSamplesTTestLevenesTable,
                                             dataset = dataset, options = options, ready = ready,
                                             resultsLevene = resultsLevene)
  
  # Add footnote: Interpretation of Results
  independentSamplesTTestLevenesTable$addFootnote(message = "Significant results suggest a deviation from equality of variance.",
                                                  symbol = "<em>Note.</em>")
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestLevenesTable <- function(independentSamplesTTestLevenesTable, dataset, options, ready,
                                                       resultsLevene) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (variable in options$variables) {
      independentSamplesTTestLevenesTable$addRows(rows = resultsLevene[[variable]], rownames = variable)
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", F = ".", df = ".", p = ".")
    independentSamplesTTestLevenesTable$addRows(rows = row)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestDescriptivesContainer <- function(jaspResults, dataset, options, ready, resultsDescriptives,
                                                                resultsDescriptivesPlots) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["independentSamplesTTestDescriptivesContainer"]])) {
    return(NULL)
  }
  
  # Create container
  independentSamplesTTestDescriptivesContainer <- createJaspContainer(title = "Descriptives")
  jaspResults[["independentSamplesTTestDescriptivesContainer"]] <- independentSamplesTTestDescriptivesContainer
  independentSamplesTTestDescriptivesContainer$dependOnOptions(c("variables", "groupingVariable", "missingValues", 
                                                                 "descriptives", "descriptivesPlots",
                                                                 "descriptivesPlotsConfidenceInterval"))
  
  # Create Descriptives Table (if wanted)
  if (options$descriptives == TRUE) {
    .createIndependentSamplesTTestDescriptivesTable(independentSamplesTTestDescriptivesContainer = independentSamplesTTestDescriptivesContainer, 
                                                   dataset = dataset, options = options, ready = ready, 
                                                   resultsDescriptives = resultsDescriptives)
  }
  
  # Create Descriptives Plots Container (if wanted and if results can be computed)
  if (options$descriptivesPlots == TRUE && ready == TRUE) {
    .createIndependentSamplesTTestDescriptivesPlotsContainer(independentSamplesTTestDescriptivesContainer = independentSamplesTTestDescriptivesContainer,
                                               dataset = dataset, options = options,
                                               resultsDescriptivesPlots = resultsDescriptivesPlots)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestDescriptivesTable <- function(independentSamplesTTestDescriptivesContainer, 
                                                            dataset, options, ready, resultsDescriptives) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(independentSamplesTTestDescriptivesContainer[["independentSamplesTTestDescriptivesTable"]])) {
    return(NULL)
  }
  
  # Create table
  independentSamplesTTestDescriptivesTable <- createJaspTable(title = "Descriptive Statistics")
  independentSamplesTTestDescriptivesContainer[["independentSamplesTTestDescriptivesTable"]] <- independentSamplesTTestDescriptivesTable
  independentSamplesTTestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  independentSamplesTTestDescriptivesTable$dependOnOptions(c("variables", "groupingVariable", "missingValues",
                                                             "descriptives"))
  
  # Add columns to table
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "variable", title = "",
                                                         type = "string",   combine = TRUE)
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "group",    title = "Group",
                                                         type = "string")
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "N",        title = "N",
                                                         type = "integer")
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "mean",     title = "Mean",
                                                         type = "number",   format = "sf:4;dp:3")
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "sd",       title = "Std. Deviation",
                                                         type = "number",   format = "sf:4;dp:3")
  independentSamplesTTestDescriptivesTable$addColumnInfo(name = "se",       title = "Std. Error",
                                                         type = "number",   format = "sf:4;dp:3")
  
  # Fill up table with results
  .fillUpIndependentSamplesTTestDescriptivesTable(independentSamplesTTestDescriptivesTable = independentSamplesTTestDescriptivesTable,
                                                  dataset = dataset, options = options, ready = ready,
                                                  resultsDescriptives = resultsDescriptives)
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestDescriptivesTable <- function(independentSamplesTTestDescriptivesTable, dataset, options, ready,
                                                            resultsDescriptives) {
  
  # If results can be computed, compute them and add row for each level of each variable
  if (ready) {
    for (variable in options$variables) {
      for (level in levels(dataset[[.v(options$groupingVariable)]])) {
        independentSamplesTTestDescriptivesTable$addRows(rows = resultsDescriptives[[variable]][[level]],
                                                         rowNames = paste0(variable, " - ", level))
      }
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", group = ".", N = ".", mean = ".", sd = ".", se = ".")
    independentSamplesTTestDescriptivesTable$addRows(rows = row)
  }
  
  # This function does not return anything
}

.createIndependentSamplesTTestDescriptivesPlotsContainer <- function(independentSamplesTTestDescriptivesContainer,
                                                                     dataset, options, resultsDescriptivesPlots) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(independentSamplesTTestDescriptivesContainer[["independentSamplesTTestDescriptivesPlotsContainer"]])) {
    return(NULL)
  }
  
  # Create container
  independentSamplesTTestDescriptivesPlotsContainer <- createJaspContainer(title = "Descriptives Plot(s)")
  independentSamplesTTestDescriptivesContainer[["independentSamplesTTestDescriptivesPlotsContainer"]] <- independentSamplesTTestDescriptivesPlotsContainer
  independentSamplesTTestDescriptivesPlotsContainer$dependOnOptions(c("variables", "groupingVariable", 
                                                                      "missingValues", "descriptivesPlots",
                                                                      "descriptivesPlotsConfidenceInterval"))
  
  # Fill up container with plots
  .fillUpIndependentSamplesTTestDescriptivesPlot(independentSamplesTTestDescriptivesPlotsContainer = independentSamplesTTestDescriptivesPlotsContainer,
                                                 dataset = dataset, options = options, variable = variable,
                                                 resultsDescriptivesPlots = resultsDescriptivesPlots)
  
  # This function does not return anything
}

.fillUpIndependentSamplesTTestDescriptivesPlot <- function(independentSamplesTTestDescriptivesPlotsContainer,
                                                           dataset, options, variable, resultsDescriptivesPlots) {
  
  for (variable in options$variables) {
    
    # Get plot
    descriptivesPlot <- resultsDescriptivesPlots[[variable]]
    
    # Add plot to container
    independentSamplesTTestDescriptivesPlot <- createJaspPlot(plot = descriptivesPlot, title = variable,
                                                            width = 350, height = 300)
    independentSamplesTTestDescriptivesPlotsContainer[[variable]] <- independentSamplesTTestDescriptivesPlot
    independentSamplesTTestDescriptivesPlot$dependOnOptions(c("variables", "groupingVariable",
                                                              "missingValues", "descriptivesPlots",
                                                              "descriptivesPlotsConfidenceInterval"))
  }
  
  # This function does not return anything
}
