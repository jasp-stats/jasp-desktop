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

	variables <- unlist(options$variables)

	if (is.null(state)) {
	  state <- list()
	}
	
	if (is.null(dataset)) {
	  dataset <- .readDataSetToEnd(columns=variables)
	}
	
	# Set title
	jaspResults$title <- "Binomial Test"
	
	# Create Binomial Table
	.binomialTable(dataset = dataset, options = options, jaspResults = jaspResults)
	
	# Create Descriptives Plots (if wanted)
  if (options[["descriptivesPlots"]]) {
    .binomialDescriptivesPlot(dataset = dataset, options = options, jaspResults = jaspResults)
  }
	
	# Bring state$options up-to-date
	state[["options"]] <- options

  return(state = state)
}


.binomialTable <- function(dataset, options, jaspResults) {
  
  if (!is.null(jaspResults[["binomialTest"]])) {
    return() #The options for this table didn't change so we don't need to rebuild it
  }
  
  variables <- unlist(options$variables)
  binomialTable <- createJaspTable("Binomial Test")
  jaspResults[["binomialTable"]] <- binomialTable

  binomialTable$dependOnOptions(c("variables", "testValue", "hypothesis", "confidenceInterval", "confidenceIntervalInterval", "VovkSellkeMPR"))
  
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
    binomialTable$addColumnInfo(name = "lowerCI",       title = "Lower",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% Confidence Interval"))
    binomialTable$addColumnInfo(name = "upperCI",       title = "Upper",        type = "number", format = "sf:4", overtitle = paste0(100*options$confidenceIntervalInterval, "% Confidence Interval"))
  }
  
  # Fill up table with Results
  .binomialResults(dataset = dataset, options = options, binomialTable = binomialTable)

  if (options$hypothesis == "notEqualToTestValue") {
    binomialTable$addFootnote(message = .messages("footnote", "binomNeq", value=options$testValue), symbol="<em>Note.</em>")
  } else if (options$hypothesis == "greaterThanTestValue") {
    binomialTable$addFootnote(message = .messages("footnote", "binomGreater", value=options$testValue), symbol="<em>Note.</em>")
  } else if (options$hypothesis == "lessThanTestValue") {
    binomialTable$addFootnote(message = .messages("footnote", "binomLess", value=options$testValue), symbol="<em>Note.</em>")
  }

  if (options$VovkSellkeMPR) {
    binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
  return(NULL)
}

  
.binomialResults <- function(dataset, options, binomialTable) {
  
  if (options$hypothesis == "notEqualToTestValue") {
    hyp <- "two.sided"
  } else if (options$hypothesis == "greaterThanTestValue") {
    hyp <- "greater"
  } else {
    hyp <- "less"
  }
  
  if (length(options$variables) == 0) {
    row <- list(variable = ".", level = ".", counts = ".", total = ".", proportion = ".", p = ".", 
                VovkSellkeMPR = ".", lowerCI = ".", upperCI = ".")
    
    if (! options$VovkSellkeMPR) {
      row$VovkSellkeMPR <- NULL
    }
    
    if (! options$confidenceInterval) {
      row$lowerCI <- NULL
      row$upperCI <- NULL
    }
    
    binomialTable$addRows(row)
  
  } else {
    
    for (var in options$variables) {
      
      column <- dataset[[ .v(var) ]]
      d <- column[!is.na(column)]
      levels <- levels(d)
      n <- length(d)
      
      if (n == 0) {
        binomialTable$error <- "badData"
        binomialTable$errorMessage <- paste0("There are no observations for ", var, " (possibly only after rows with missing values are excluded)")
      }

      # Test each level in each variable against test value
      for (lev in levels) {
      
        counts <- sum(d == lev)
        prop <- counts/n
        r <- stats::binom.test(counts, n, p = options$testValue,
                               alternative = hyp,
                               conf.level = options$confidenceIntervalInterval)

        p <- r$p.value
        vovk <- .VovkSellkeMPR(p)
        cilo <- r$conf.int[1]
        ciup <- r$conf.int[2]

        if (p == FALSE) {
          p <- 0
        } else if (p == TRUE) {
          p <- 1
        }
    
        row <- list(variable = var, level = lev, counts = counts, total = n, proportion = prop, p = p, 
                    VovkSellkeMPR = vovk, lowerCI = cilo, upperCI = ciup)
        
        if (! options$VovkSellkeMPR) {
          row$VovkSellkeMPR <- NULL
          }
        
        if (! options$confidenceInterval) {
          row$lowerCI <- NULL
          row$upperCI <- NULL
        }
        
        binomialTable$addRows(row)
      }
    }
  }
  
  return(NULL)
}


.binomialDescriptivesPlot <- function(dataset, options, jaspResults) {
  
  if (!is.null(jaspResults[["descriptivesPlots"]])) {
    return() #The options for this table didn't change so we don't need to rebuild it
  }
  
  testValue <- options$testValue
  confLevel <- options$descriptivesPlotsConfidenceInterval
  
  descriptivesPlotCollectionTotal <- createJaspContainer(title = "Descriptives Plots")
  jaspResults[["descriptivesPlots"]] <- descriptivesPlotCollectionTotal
  descriptivesPlotCollectionTotal$dependOnOptions(c("descriptivesPlots", "variables", "descriptivesPlotsConfidenceInterval"))

  base_breaks_y <- function(x, testValue) {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                            yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(0,  round(testValue,3), 1)))
  }

  pd <- ggplot2::position_dodge(0.2)
  
  for (i in .indices(options$variables)) {
    
    var <- options$variables[[i]]
    d <- dataset[[.v(var)]]
    d <- d[!is.na(d)]
    nObs <- length(d)
    
    descriptivesPlotCollectionVar <- createJaspContainer(title = paste0("Descriptives Plots - ", var))
    descriptivesPlotCollectionTotal[[var]] <- descriptivesPlotCollectionVar
    descriptivesPlotCollectionVar$dependOnOptions(c("descriptivesPlots", "variables", "descriptivesPlotsConfidenceInterval"))
    
    for (k in .indices(levels(d))) {
      
      level <- levels(d)[k]
      count <- sum(d == level)
      rate <- count/nObs
      r <- stats::binom.test(x = count, n = nObs, p = testValue, alternative = "two.sided", conf.level = confLevel)
      ciLower <- r$conf.int[1]
      ciUpper <- r$conf.int[2]
      
      summaryStat <- data.frame(label = level, rate = rate, ciLower = ciLower, ciUpper = ciUpper)
      dfTestValue <- data.frame(testValue = testValue)
      
      descriptivesPlotLevPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label, y = rate, group = 1)) +
		    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), colour = "black", width = 0.2, 
		                           position = pd) +
		    ggplot2::geom_point(position = pd, size = 4) +
		    ggplot2::geom_hline(data = dfTestValue, ggplot2::aes(yintercept = testValue), linetype = "dashed") +
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
      
      descriptivesPlotLev <- createJaspPlot(plot = descriptivesPlotLevPlot, title = level)
      descriptivesPlotCollectionVar[[level]] <- descriptivesPlotLev
      descriptivesPlotLev$dependOnOptions(c("descriptivesPlots", "variables", "descriptivesPlotsConfidenceInterval"))
	  }
  }

  return(NULL)
}
