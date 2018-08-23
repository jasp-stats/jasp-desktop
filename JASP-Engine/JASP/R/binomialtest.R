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

BinomialTest <- function(jaspResults, dataset, options, state = NULL) {
  
  # Update options
  if (options$hypothesis == "notEqualToTestValue") {
    options$hypothesisRec <- "two.sided"
  } else if (options$hypothesis == "greaterThanTestValue") {
    options$hypothesisRec <- "greater"
  } else {
    options$hypothesisRec <- "less"
  }
  
  # Define state if empty
  if (is.null(state)) {
	  state <- list()
	}
	
  # Read dataset
  if (is.null(dataset)) {
	  dataset <- .readDataSetToEnd(columns=options$variables)
  }
  
  # Update options (using dataset)
  options[["levels"]] <- NULL
  for (variable in options$variables) {
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    options[["levels"]] <- append(options$levels, list(levels))
    names(options$levels)[which(options$variables %in% variable)] <- variable
  }
  
	# Set title
	jaspResults$title <- "Binomial Test"
	
	# Check if results can be computed
	ready <- (length(options$variables) > 0)
	
	# Check for errors
	if (ready) {
	  
	  # Error Check 1: Number of levels of the variables
	  .hasErrors(dataset = dataset, perform = "run", type = 'factorLevels', factorLevels.target = options$variables, 
	             factorLevels.amount = '< 1', exitAnalysisIfErrors = TRUE)
	  
	  # Error check 2: 0 observations for a level of a variable
	  for (variable in options$variables) {
	    
	    column <- dataset[[ .v(variable) ]]
	    data <- column[!is.na(column)]
	    levels <- levels(as.factor(data))

	    for (level in levels) {
	      .hasErrors(data[data == level], perform = "run", type = 'observations', observations.amount = c('< 1'), 
	                 exitAnalysisIfErrors = TRUE)
	    }
	  }
	}
	
	# Compute Results for Binomial Table
	if (ready && is.null(jaspResults[["binomialTable"]])) {
	  resultsTable <- .computeBinomialTableResults(dataset = dataset, options = options)
	}
	
	# Compute Results for Binomial Plots
	if (ready && is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
	  resultsPlots <- .computeBinomialPlotsResults(dataset = dataset, options = options)
	}
	
	# Create Binomial Table
	.createBinomialTable(jaspResults = jaspResults, options = options, ready = ready, resultsTable = resultsTable)
	
	# Create Descriptives Plots Container (if wanted and if results can be computed)
  if (options$descriptivesPlots == TRUE && ready == TRUE) {
    .createBinomialDescriptivesPlotsContainers(jaspResults = jaspResults, options = options, resultsPlots = resultsPlots)
  }
	
	# Bring state up-to-date
	state[["options"]] <- options

  return(state)
}

.computeBinomialTableResults <- function(dataset, options) {
  
  # This will be the return object
  results <- list()
  
  for (variable in options$variables) {
    
    results[[variable]] <- list()
    
    # Prepare for running the binomial test
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    
    for (level in levels) {
      
      nObs <- length(data)
      counts <- sum(data == level)
      proportion <- counts/nObs
      
      resultsBinom <- stats::binom.test(x = counts, n = nObs, p = options$testValue, alternative = options$hypothesisRec,
                                        conf.level = options$confidenceIntervalInterval)
      p <- resultsBinom$p.value
      if (p == FALSE) {
        p <- 0
      } else if (p == TRUE) {
        p <- 1
      }
      lowerCI <- resultsBinom$conf.int[1]
      upperCI <- resultsBinom$conf.int[2]
      
      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- list(variable = variable, level = level, counts = counts, total = nObs, 
                                           proportion = proportion, p = p, VovkSellkeMPR = .VovkSellkeMPR(p), 
                                           lowerCI = lowerCI, upperCI = upperCI)
    }
  }
  # Return results object
  return(results)
}

.computeBinomialPlotsResults <- function(dataset, options) {
  
  # This will be the return object
  results <- list()
  
  for (variable in options$variables) {
    
    # Prepare for running the binomial test
    column <- dataset[[ .v(variable) ]]
    data <- column[!is.na(column)]
    levels <- levels(as.factor(data))
    
    for (level in levels) {
      
      # Define base breaks function for y
      base_breaks_y <- function(x, testValue) {
        d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
        list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                          yend = yend),
                                   inherit.aes = FALSE, size = 1),
             ggplot2::scale_y_continuous(breaks = c(0,  round(testValue,3), 1)))
      }
      
      # Define plot position
      plotPosition <- ggplot2::position_dodge(0.2)
      
      # Compute data for plot
      nObs <- length(data)
      counts <- sum(data == level)
      proportion <- counts/nObs
      resultsBinom <- stats::binom.test(x = counts, n = nObs, p = options$testValue, alternative = "two.sided",
                                        conf.level = options$descriptivesPlotsConfidenceInterval)
      lowerCI <- resultsBinom$conf.int[1]
      upperCI <- resultsBinom$conf.int[2]
      
      summaryStat <- data.frame(label = level, proportion = proportion, lowerCI = lowerCI, upperCI = upperCI)
      dfTestValue <- data.frame(testValue = options$testValue)
      
      # Make plot
      descriptivesPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label, y = proportion, group = 1)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = 0.2, 
                               position = plotPosition) +
        ggplot2::geom_point(position = plotPosition, size = 4) +
        ggplot2::geom_hline(data = dfTestValue, ggplot2::aes(yintercept = options$testValue), linetype = "dashed") +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::ylim(min = 0, max = 1) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(size = 18),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_text(size = 18, vjust = -1),
                       axis.text.x = ggplot2::element_text(size = 15),
                       axis.text.y = ggplot2::element_text(size = 15),
                       panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
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
        base_breaks_y(summaryStat, dfTestValue$testValue)
      
      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- descriptivesPlot
    }
  }
  # Return results object
  return(results)
}

.createBinomialTable <- function(jaspResults, options, ready, resultsTable) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["binomialTable"]])) {
    return(NULL)
  }
  
  # Create table
  binomialTable <- createJaspTable(title = "Binomial Test")
  jaspResults[["binomialTable"]] <- binomialTable
  binomialTable$showSpecifiedColumnsOnly <- TRUE
  binomialTable$dependOnOptions(c("variables", "testValue", "hypothesis", "confidenceInterval", 
                                  "confidenceIntervalInterval", "VovkSellkeMPR"))
  
  # Add columns to table
  binomialTable$addColumnInfo(name = "variable",      title = "Variable",     type = "string", combine = TRUE)
  binomialTable$addColumnInfo(name = "level",         title = "Level",        type = "string")
  binomialTable$addColumnInfo(name = "counts",        title = "Counts",       type = "integer")
  binomialTable$addColumnInfo(name = "total",         title = "Total",        type = "integer")
  binomialTable$addColumnInfo(name = "proportion",    title = "Proportion",   type = "number", format = "sf:4")
  binomialTable$addColumnInfo(name = "p",             title = "p",            type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    binomialTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number", format = "sf:4")
  }
  if (options$confidenceInterval) {
    binomialTable$addColumnInfo(name = "lowerCI",       title = "Lower",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
    binomialTable$addColumnInfo(name = "upperCI",       title = "Upper",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% CI for Proportion"))
  }
  
  # Fill up table with results
  .fillUpBinomialTable(binomialTable = binomialTable, options = options, ready = ready, resultsTable = resultsTable)

  # This function does not return anything
}

.fillUpBinomialTable <- function(binomialTable, options, ready, resultsTable) {
  
  # If results can be computed, compute them and add row for each level of each variable
  if (ready == TRUE) {
    
    for (variable in options$variables) {
      for (level in options$levels[[variable]]) {
        row <- resultsTable[[variable]][[level]]
        binomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
      }
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
    # Add footnote: Alternative hypothesis
    if (options$hypothesisRec == "two.sided") {
      binomialTable$addFootnote(message = .messages("footnote", "binomNeq", value=options$testValue), symbol="<em>Note.</em>")
    } else if (options$hypothesisRec == "greater") {
      binomialTable$addFootnote(message = .messages("footnote", "binomGreater", value=options$testValue), symbol="<em>Note.</em>")
    } else if (options$hypothesisRec == "less") {
      binomialTable$addFootnote(message = .messages("footnote", "binomLess", value=options$testValue), symbol="<em>Note.</em>")
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", level = ".", counts = ".", total = ".", proportion = ".", p = ".", 
                VovkSellkeMPR = ".", lowerCI = ".", upperCI = ".")
    binomialTable$addRows(row)
  }
  
  # This function does not return anything
}

.createBinomialDescriptivesPlotsContainers <- function(jaspResults, options, resultsPlots) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["binomialDescriptivesPlotsContainerTotal"]])) {
    return(NULL)
  }
  
  # Create container for all variables
  binomialDescriptivesPlotsContainerTotal <- createJaspContainer(title = "Descriptives Plots")
  jaspResults[["binomialDescriptivesPlotsContainerTotal"]] <- binomialDescriptivesPlotsContainerTotal
  binomialDescriptivesPlotsContainerTotal$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                            "descriptivesPlotsConfidenceInterval"))
  
  # Create subcontainer for each variable
  for (variable in options$variables) {
    binomialDescriptivesPlotsContainerVariable <- createJaspContainer(title = variable)
    binomialDescriptivesPlotsContainerTotal[[variable]] <- binomialDescriptivesPlotsContainerVariable
    binomialDescriptivesPlotsContainerVariable$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                                 "descriptivesPlotsConfidenceInterval"))
  }
  
  # Fill up containers with plots
  .fillUpBinomialDescriptivesPlotsContainers(binomialDescriptivesPlotsContainerTotal, options, resultsPlots)
  
  # This function does not return anything
}

.fillUpBinomialDescriptivesPlotsContainers <- function(binomialDescriptivesPlotsContainerTotal, options, resultsPlots) {
  
  for (variable in options$variables) {
    for (level in options$levels[[variable]]) {
      descriptivesPlot <- resultsPlots[[variable]][[level]]
      binomialDescriptivesPlot <- createJaspPlot(plot = descriptivesPlot, title = level)
      binomialDescriptivesPlotsContainerTotal[[variable]][[level]] <- binomialDescriptivesPlot
      binomialDescriptivesPlot$dependOnOptions(c("variables", "testValue", "descriptivesPlots",
                                                 "descriptivesPlotsConfidenceInterval"))
      
    }
  }
  
  # This function does not return anything
}