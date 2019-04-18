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

BinomialTest <- function(jaspResults, dataset, options, ...) {

  # Read dataset
  dataset <- .binomReadData(dataset, options)

  # Error checking
  errors <- .binomCheckErrors(dataset, options)

  # Compute the results
  binomResults <- .binomComputeResults(jaspResults, dataset, options, errors)

  # Output tables and plots
  .binomTableMain(       jaspResults, dataset, options, binomResults, errors)
  .binomContainerPlots(  jaspResults, dataset, options, binomResults, errors)
  .binomPlotsDescriptive(jaspResults, dataset, options, binomResults, errors)

  return()

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

  # Check if results can be computed
  if (length(options$variables) == 0) return("No variables")

  # Error Check 1: Number of levels of the variables
  .hasErrors(
    dataset              = dataset,
    perform              = "run",
    type                 = "factorLevels",
    factorLevels.target  = options$variables,
    factorLevels.amount  = "< 1",
    exitAnalysisIfErrors = TRUE
  )

  # Error check 2: 0 observations for a level of a variable
  for (variable in options$variables) {

    column <- dataset[[.v(variable)]]
    data   <- column[!is.na(column)]
    levels <- levels(data)

    for (level in levels) {
      .hasErrors(
        dataset              = data[data == level],
        perform              = "run",
        type                 = "observations",
        observations.amount  = "< 1",
        exitAnalysisIfErrors = TRUE
      )
    }
  }

}

# Results functions ----
.binomComputeResults <- function(jaspResults, dataset, options, errors) {

  if (!is.null(errors) && errors == "No variables") return()

  # Take results from state if possible
  if (!is.null(jaspResults[["stateBinomResults"]])) return(jaspResults[["stateBinomResults"]]$object)

  # This will be the object that we fill with results
  results <- list()

  # First, we perform precalculation of variables we use throughout the analysis
  results[["spec"]] <- .binomCalcSpecs(dataset, options)
  results[["binom"]] <- list()

  for (variable in options$variables) {

    results[["binom"]][[variable]] <- list()

    # Prepare for running the binomial test
    column <- dataset[[.v(variable)]]
    data   <- column[!is.na(column)]
    levels <- levels(data)

    for (level in levels) {
      nObs   <- length(data)
      counts <- sum(data == level)
      prop   <- counts / nObs

      # Main binomial test result for tables
      tableResults <- stats::binom.test(
        x           = counts,
        n           = nObs,
        p           = options$testValue,
        alternative = results$spec$hypothesisRec,
        conf.level  = options$confidenceIntervalInterval
      )

      # sometimes p.value becomes true or false, convert this to 1 or 0
      p <- as.numeric(tableResults$p.value)
      lowerCI <- tableResults$conf.int[1]
      upperCI <- tableResults$conf.int[2]

      # Binomial test for plots
      plotResults <- stats::binom.test(
        x           = counts,
        n           = nObs,
        p           = options$testValue,
        alternative = "two.sided",
        conf.level  = options$descriptivesPlotsConfidenceInterval
      )

      # Summary statistics for plots
      plotDat <- data.frame(
        label      = level,
        proportion = prop,
        lowerCI    = plotResults$conf.int[1],
        upperCI    = plotResults$conf.int[2]
      )

      # Add results for each level of each variable to results object
      results[["binom"]][[variable]][[level]] <- list(
        variable      = variable,
        level         = level,
        counts        = counts,
        total         = nObs,
        proportion    = prop,
        p             = p,
        VovkSellkeMPR = .VovkSellkeMPR(p),
        lowerCI       = lowerCI,
        upperCI       = upperCI,
        plotDat       = plotDat
      )
    }
  }

  # Save results to state
  jaspResults[["stateBinomResults"]] <- createJaspState(results)
  jaspResults[["stateBinomResults"]]$dependOn(
    c("variables", "testValue", "hypothesis", "confidenceIntervalInterval", "descriptivesPlotsConfidenceInterval")
  )

  # Return results object
  return(results)
}

.binomCalcSpecs <- function(dataset, options) {
  specs <- list()

  # Recode the hypothesis for binom.test()
  if (options$hypothesis == "notEqualToTestValue") {
    specs$hypothesisRec <- "two.sided"
  } else if (options$hypothesis == "greaterThanTestValue") {
    specs$hypothesisRec <- "greater"
  } else {
    specs$hypothesisRec <- "less"
  }

  # Precompute the levels of the different variables
  specs[["levels"]] <- NULL
  for (variable in options$variables) {
    column <- dataset[[.v(variable)]]
    data <- column[!is.na(column)]
    levels <- levels(data)
    specs[["levels"]] <- c(specs$levels, list(levels))
    names(specs$levels)[which(options$variables %in% variable)] <- variable
  }

  return(specs)
}

# Output functions ----
.binomTableMain <- function(jaspResults, dataset, options, binomResults, errors) {
  if (!is.null(jaspResults[["binomialTable"]])) return()

  # Create table
  binomialTable <- createJaspTable(title = "Binomial Test")
  binomialTable$dependOn(c("variables", "testValue", "hypothesis", "confidenceInterval",
                                  "confidenceIntervalInterval", "VovkSellkeMPR"))

  binomialTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  binomialTable$addColumnInfo(name = "variable",   title = "Variable",   type = "string", combine = TRUE)
  binomialTable$addColumnInfo(name = "level",      title = "Level",      type = "string")
  binomialTable$addColumnInfo(name = "counts",     title = "Counts",     type = "integer")
  binomialTable$addColumnInfo(name = "total",      title = "Total",      type = "integer")
  binomialTable$addColumnInfo(name = "proportion", title = "Proportion", type = "number")
  binomialTable$addColumnInfo(name = "p",          title = "p",          type = "pvalue")

  if (options$VovkSellkeMPR) {
    binomialTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR", type = "number")
  }

  if (options$confidenceInterval) {
    binomialTable$addColumnInfo(name = "lowerCI", title = "Lower", type = "number",
      overtitle = paste0(100 * options$confidenceIntervalInterval, "% CI for Proportion"))
    binomialTable$addColumnInfo(name = "upperCI", title = "Upper", type = "number",
      overtitle = paste0(100 * options$confidenceIntervalInterval, "% CI for Proportion"))
  }
  
  jaspResults[["binomialTable"]] <- binomialTable

  if (!is.null(errors) && errors == "No variables")
    return()

  for (variable in options$variables) {
    for (level in binomResults[["spec"]][["levels"]][[variable]]) {
      row <- binomResults[["binom"]][[variable]][[level]][1:9]
      binomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
    }
  }

  # Add footnote: VovkSellkeMPR
  if (options$VovkSellkeMPR) {
    binomialTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A", colNames="VovkSellkeMPR")
  }

  # Add footnote: Alternative hypothesis
  message <- switch(binomResults[["spec"]][["hypothesisRec"]],
                    "two.sided" = .messages("footnote", "binomNeq",     value = options$testValue),
                    "greater"   = .messages("footnote", "binomGreater", value = options$testValue),
                    "less"      = .messages("footnote", "binomLess",    value = options$testValue)
  )

  binomialTable$addFootnote(message = message)
}

.binomContainerPlots <- function(jaspResults, dataset, options, binomResults, errors) {
  if (!options$descriptivesPlots) return()
  if (!is.null(errors) && errors == "No variables") return()

  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Descriptives Plots")
    jaspResults[["containerPlots"]]$dependOn("descriptivesPlots")
  }
}

.binomPlotsDescriptive <- function(jaspResults, dataset, options, binomResults, errors) {
  if (!options$descriptivesPlots) return()
  if (!is.null(errors) && errors == "No variables") return()

  # create pointer towards main container, created before
  pct <- jaspResults[["containerPlots"]]

  # Create subcontainer for each variable
  for (variable in options$variables) {

    # If the plot for this variable already exists, we can skip recalculating the plots
    if (!is.null(pct[[variable]])) next

    pct[[variable]] <- createJaspContainer(variable)
    pct[[variable]]$dependOn(options=c("testValue", "descriptivesPlotsConfidenceInterval"), optionContainsValue=list(variables=variable))


    for (level in binomResults[["spec"]][["levels"]][[variable]]) {
      descriptivesPlot <- .binomPlotHelper(binomResults[["binom"]][[variable]][[level]]$plotDat, options$testValue)
      pct[[variable]][[level]] <- createJaspPlot(plot = descriptivesPlot, title = level, width = 160, height = 320)
      pct[[variable]][[level]]$dependOn("descriptivesPlots")
    }

  }

}

.binomPlotHelper <- function(plotDat, testValue) {

  base_breaks_y <- function(testValue) {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(0,  round(testValue, 3), 1)))
  }

  plotPosition <- ggplot2::position_dodge(0.2)

  p <-
    ggplot2::ggplot(plotDat, ggplot2::aes(x = label, y = proportion, group = 1)) +
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

  return(p)
}
