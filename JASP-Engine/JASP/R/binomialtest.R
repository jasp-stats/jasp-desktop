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

BinomialTest <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$variables) > 0
  
  if (ready) {
    dataset <- .binomReadData(dataset, options)

    .binomCheckErrors(dataset, options)
  }

  .binomTableMain(       jaspResults, dataset, options, ready)
  .binomPlotsDescriptive(jaspResults, dataset, options, ready)
}

# Preprocessing functions ----
.binomReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.factor = options$variables))
  }
}

.binomCheckErrors <- function(dataset, options) {
  # perform a check on the hypothesis
  custom <- function() {
    if (options$testValue == 1 && options$hypothesis == "greaterThanTestValue")
      return(gettext("Cannot test the hypothesis that the test value is greater than 1."))
    else if (options$testValue == 0 && options$hypothesis == "lessThanTestValue")
      return(gettext("Cannot test the hypothesis that the test value is less than 0."))
  }
  
  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = dataset,
    custom               = custom,
    type                 = "factorLevels",
    factorLevels.target  = options$variables,
    factorLevels.amount  = "< 1",
    exitAnalysisIfErrors = TRUE
  )

}

# Results functions ----
.binomComputeTableResults <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["binomTableResults"]]))
    return(jaspResults[["binomTableResults"]]$object)

  # This will be the object that we fill with results
  results <- list()
  hyp <- .binomTransformHypothesis(options$hypothesis)

  for (variable in options$variables) {

    results[[variable]] <- list()

    data <- na.omit(dataset[[.v(variable)]])

    for (level in levels(data)) {
      
      counts <- sum(data == level)
      tableResults <- stats::binom.test(
        x           = counts,
        n           = length(data),
        p           = options$testValue,
        alternative = hyp,
        conf.level  = options$confidenceIntervalInterval
      )

      # sometimes p.value becomes true or false, convert this to 1 or 0
      p <- as.numeric(tableResults$p.value)

      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- list(
        variable      = variable,
        level         = level,
        counts        = counts,
        total         = length(data),
        proportion    = counts / length(data),
        p             = p,
        VovkSellkeMPR = .VovkSellkeMPR(p),
        lowerCI       = tableResults$conf.int[1],
        upperCI       = tableResults$conf.int[2]
      )
      
    }
    
  }

  # Save results to state
  jaspResults[["binomTableResults"]] <- createJaspState(results)
  jaspResults[["binomTableResults"]]$dependOn(
    c("variables", "testValue", "hypothesis", "confidenceIntervalInterval")
  )

  # Return results object
  return(results)
}

.binomComputePlotResults <- function(variable, dataset, testValue, ci) {
  results <- list()
  data <- na.omit(dataset[[.v(variable)]])

  for (level in levels(data)) {
    
    counts <- sum(data == level)
    plotResults <- stats::binom.test(
      x           = counts,
      n           = length(data),
      p           = testValue,
      alternative = "two.sided",
      conf.level  = ci
    )

    # Summary statistics for plots
    results[[level]] <- data.frame(
      label      = level,
      proportion = counts / length(data),
      lowerCI    = plotResults$conf.int[1],
      upperCI    = plotResults$conf.int[2]
    )
    
  }

  return(results)
}

.binomTransformHypothesis <- function(hypothesis) {
  if (hypothesis == "greaterThanTestValue")
    return("greater")
  else if (hypothesis == "lessThanTestValue")
    return("less")
  else
    return("two.sided")
}

# Output functions ----
.binomTableMain <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["binomialTable"]]))
    return()

  # Create table
  binomialTable <- createJaspTable(title = gettext("Binomial Test"))
  binomialTable$dependOn(c("variables", "testValue", "hypothesis", "confidenceInterval",
                                  "confidenceIntervalInterval", "VovkSellkeMPR"))

  binomialTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  binomialTable$addColumnInfo(name = "variable",   title = gettext("Variable"),   type = "string", combine = TRUE)
  binomialTable$addColumnInfo(name = "level",      title = gettext("Level"),      type = "string")
  binomialTable$addColumnInfo(name = "counts",     title = gettext("Counts"),     type = "integer")
  binomialTable$addColumnInfo(name = "total",      title = gettext("Total"),      type = "integer")
  binomialTable$addColumnInfo(name = "proportion", title = gettext("Proportion"), type = "number")
  binomialTable$addColumnInfo(name = "p",          title = gettext("p"),          type = "pvalue")

  if (options$VovkSellkeMPR)
    binomialTable$addColumnInfo(name = "VovkSellkeMPR", title = gettext("VS-MPR"), type = "number")

  if (options$confidenceInterval) {
    binomialTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number",
      overtitle = gettextf("%s%% CI for Proportion", 100 * options$confidenceIntervalInterval))
    binomialTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number",
      overtitle = gettextf("%s%% CI for Proportion", 100 * options$confidenceIntervalInterval))
  }
  
  # Add footnote: VovkSellkeMPR
  if (options$VovkSellkeMPR)
    binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A", colNames="VovkSellkeMPR")
    
  # Add footnote: Alternative hypothesis
  if (options$hypothesis == "lessThanTestValue")
    note <- gettext("For all tests, the alternative hypothesis specifies that the proportion is less than ")
  else if (options$hypothesis == "greaterThanTestValue")
    note <- gettext("For all tests, the alternative hypothesis specifies that the proportion is greater than ")
  else
    note <- gettext("Proportions tested against value: ")
  
  binomialTable$addFootnote(message = paste0(note, options$testValue, "."))
  
  jaspResults[["binomialTable"]] <- binomialTable

  if (!ready)
    return()
  
  binomialTable$setExpectedSize(sum(unlist(lapply(dataset, nlevels))))
  
  # Compute the results and fill the table
  binomResults <- .binomComputeTableResults(jaspResults, dataset, options)
  .binomFillTableMain(binomialTable, binomResults)
}

.binomFillTableMain <- function(binomialTable, binomResults) {
  for (variable in names(binomResults)) {
    
    isNewGroup <- TRUE
    for (level in names(binomResults[[variable]])) {
      row <- binomResults[[variable]][[level]]
      row <- c(list(.isNewGroup = isNewGroup), row)
      isNewGroup <- FALSE
      binomialTable$addRows(row)
    }
    
  }
}

.binomPlotsDescriptive <- function(jaspResults, dataset, options, ready, ciName = "descriptivesPlotsConfidenceInterval") {
  if (!options$descriptivesPlots)
    return()

  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer(gettext("Descriptives Plots"))
    jaspResults[["containerPlots"]]$dependOn(c("descriptivesPlots", "testValue", ciName))
  }
    
  plotContainer <- jaspResults[["containerPlots"]]
  
  if (!ready) {
    # show a placeholder plot if someone says he wants a plot but does not enter any variables
    plotContainer[["placeholder"]] <- createJaspPlot(width = 320, height = 320, dependencies = "variables")
    return()
  }

  # Create subcontainer for each variable
  for (variable in options$variables) {

    # If the plot for this variable already exists, we can skip recalculating the plots
    if (!is.null(plotContainer[[variable]]))
      next

    plotContainer[[variable]] <- createJaspContainer(variable)
    plotContainer[[variable]]$dependOn(optionContainsValue = list(variables=variable))

    plotResults <- .binomComputePlotResults(variable, dataset, options[["testValue"]], options[[ciName]])
    
    for (level in names(plotResults)) {
      plot <- createJaspPlot(title = level, width = 160, height = 320)
      plotContainer[[variable]][[level]] <- plot
      .binomFillDescriptivesPlot(plot, plotResults[[level]], options$testValue)
    }

  }

}

.binomFillDescriptivesPlot <- function(plot, data, testValue) {
  base_breaks_y <- function(testValue) {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(0,  round(testValue, 3), 1)))
  }

  plotPosition <- ggplot2::position_dodge(0.2)

  plot$plotObject <-
    ggplot2::ggplot(data, ggplot2::aes(x = label, y = proportion, group = 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = 0.2,
                           position = plotPosition) +
    ggplot2::geom_point(position = plotPosition, size = 4) +
    ggplot2::geom_hline(yintercept = testValue, linetype = "dashed") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw() +
    ggplot2::ylim(min = 0, max = 1) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = 18),
      panel.grid.major  = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_text(size = 18, vjust = -1),
      axis.text.x       = ggplot2::element_text(size = 15),
      axis.text.y       = ggplot2::element_text(size = 15),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.border      = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      legend.title      = ggplot2::element_text(size = 12),
      legend.text       = ggplot2::element_text(size = 12),
      axis.ticks        = ggplot2::element_line(size = 0.5),
      axis.ticks.margin = grid::unit(1, "mm"),
      axis.ticks.length = grid::unit(3, "mm"),
      plot.margin       = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")
    ) +
    base_breaks_y(testValue)
}
