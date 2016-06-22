#
# Copyright (C) 2015 University of Amsterdam
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

BinomialTest <- function(dataset = NULL, options, perform = "run",
						   callback = function(...) 0,  ...) {

  variables <- unlist(options$variables)

  if (is.null(dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.numeric=NULL,
			                             columns.as.factor=variables,
			                             exclude.na.listwise=NULL)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=NULL,
			                              columns.as.factor=variables)
		}
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=NULL,
		                columns.as.factor=variables)
	}

  # Retrieve state

  state <- .retrieveState()
	diff <- NULL
	if (!is.null(state)){
		diff <- .diff(state$options, options)
	}


  results <- list()

	results[["title"]] <- "Binomial Test"
	results[[".meta"]] <- list(list(name = "binomial", type = "table"),
	                           list(name = "descriptives", type = "collection",
	                                meta = "image"))

  if (is.list(diff) && (!any(diff$variables, diff$confidenceIntervalInterval,
			diff$hypothesis, diff$testValue) || perform == "init")){

		binomResults <- state$binomResults

	} else {

		binomResults <- .binomialTest(dataset, options, variables, perform)

	}


  results[["binomial"]] <- .binomialTable(binomResults, options, variables,
                                          perform)


  if (options$descriptivesPlots){
    plotTitle <- ifelse(length(options$variables) > 1, "Descriptives Plots",
	                      "Descriptives Plot")

		if (!is.null(state) && !is.null(state$options$variables) &&
				(state$options$plotWidth != options$plotWidth || 
					state$options$plotHeight != options$plotHeight) &&
				state$options$descriptivesPlots == TRUE){

			# if only width is changed, use scaling proper
			descriptPlots <- .binomialDescriptivesPlot(dataset, options, variables,
																								 perform, initial = F)

		} else {

			descriptPlots <- .binomialDescriptivesPlot(dataset, options, variables,
																								 perform, initial = T)

		}

    results[["descriptives"]] <- list(collection = descriptPlots,
	                                    title = plotTitle)
  } else {

	  results[["descriptives"]] <- NULL

	}


  if (perform == "run") {

		state <- list()
		state[["options"]] <- options
		state[["binomResults"]] <- binomResults
		state[["binomTable"]] <- results[["binomial"]]
		state[["descriptPlots"]] <- results[["descriptives"]]

    return(list(results=results, status="complete", state=state))

  } else {


		return(list(results=results, status="inited", state=state))

  }

}


.binomialTable <- function(r, options, variables, perform){

  table <- list()
  table[["title"]] <- "Binomial Test"

  fields <- list(
    list(name="case", title="", type="string", combine=TRUE),
    list(name="level", title="Level", type="string"),
    list(name="counts", title="Counts", type="integer"),
    list(name="total", title="Total", type="integer"),
    list(name="proportion", title="Proportion", type="number",
         format="sf:4;dp:3"),
    list(name="p", title="p", type="number", format="dp:3;p:.001")
  )

  if (options$confidenceInterval) {

    interval <- 100 * options$confidenceIntervalInterval
    title <- paste0(interval, "% Confidence Interval")

    fields[[length(fields)+1]] <- list(name = "lowerCI",
                                       type = "number",
                                       format = "sf:4;dp:3", title = "Lower",
                                       overTitle = title)
    fields[[length(fields)+1]] <- list(name = "upperCI",
                                       type = "number",
                                       format = "sf:4;dp:3", title = "Upper",
                                       overTitle = title)
  }

  table[["schema"]] <- list(fields = fields)


  footnotes <- .newFootnotes()

  if (options$hypothesis == "notEqualToTestValue") {

    message <- paste0("Proportions tested against value: ", options$testValue,
                      ".")

  } else if (options$hypothesis == "greaterThanTestValue") {

    note <- "For all tests, the alternative hypothesis specifies that the
    proportion	is greater than "
    message <- paste0(note, options$testValue, ".")

  } else {

    note <- "For all tests, the alternative hypothesis specifies that the
    proportion is less than "
    message <- paste0(note, options$testValue, ".")

  }

  .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
  table[["footnotes"]] <- as.list(footnotes)

  if (!is.null(r)){

    table[["data"]] <- r
    table[["status"]] <- "complete"

  } else {

    data <- list()

    if (is.null(variables)){
      variables <- ""
    }

    for (var in variables){
      data[[length(data) + 1]] <- list(case=var, level=".", counts=".",
                                       total=".",  proportion=".", p=".",
                                       lowerCI=".", upperCI=".")
    }

    table[["data"]] <- data

  }

  return(table)
}

.binomialTest <- function(dataset, options, variables, perform){

	binomResults <- NULL

	if (perform == "run" && !is.null(variables)) {

    if (options$hypothesis == "notEqualToTestValue") {
      hyp <- "two.sided"
    } else if (options$hypothesis == "greaterThanTestValue") {
      hyp <- "greater"
    } else {
      hyp <- "less"
    }

    data <- list()

    for (var in variables) {

      d <- dataset[[.v(var)]]
      d <- d[!is.na(d)]

      levels <- levels(d)
      n <- length(d)

      # !! Test each level in each variable against test value !!
      for (lev in levels) {

        counts <- sum(d == lev)
        prop <- counts/n

        r <- stats::binom.test(counts, n, p = options$testValue,
                               alternative = hyp,
                               conf.level = options$confidenceIntervalInterval)

        p <- r$p.value
        cilo <- r$conf.int[1]
        ciup <- r$conf.int[2]

        if (p == FALSE) {
          p <- 0
        } else if (p == TRUE) {
          p <- 1
        }

        row <- list(case = var, level = lev, counts = .clean(counts),
                    total = .clean(n), proportion = .clean(prop), p = .clean(p),
                    lowerCI = .clean(cilo), upperCI = .clean(ciup))

        if (lev == levels[1]) {
          row[[".isNewGroup"]] <- TRUE
        } else {
          row[[".isNewGroup"]] <- FALSE
        }

        data[[length(data)+1]] <- row

      }

    }

    binomResults <- data

  }

  return(binomResults)

}

.binomialDescriptivesPlot <- function(dataset, options, variables, perform,
																			initial) {

  descriptivesPlotList <- list()

  base_breaks_y <- function(x, testValue) {

    values <- c(testValue, x[, "ciLower"], x[, "ciUpper"])
    ci.pos <- c(min(values), max(values))
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                            yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(0,  round(testValue,3), 1)))
  }

  pd <- ggplot2::position_dodge(0.2)

	for (i in .indices(options$variables)) {

		var <- options$variables[[i]]

    descriptivesPlot <- list("title" = var)
    descriptivesPlot[["width"]] <- options$plotWidth
    descriptivesPlot[["height"]] <- options$plotHeight
    descriptivesPlot[["custom"]] <- list(width = "plotWidth",
                                         height = "plotHeight")

    if (perform == "run") {

      d <- dataset[[.v(var)]]
      d <- d[!is.na(d)]

      levels <- levels(d)
      nLevels <- length(levels)

      # R throws an error with too many levels, here catch it
			if (nLevels > 30){

				descriptivesPlot[["error"]] <- list(errorType="badData",
																						errorMessage="Too many levels to
																						display descriptives plot.")
				descriptivesPlot[["data"]] <- ""

			} else {

	      nObs <- length(d)
	      testValue = options$testValue
	      hyp <- "two.sided"

				if (initial == TRUE){
					# Initial width dependent on levels, later widths not
					descriptivesPlot[["width"]] <- 80 * nLevels
				}

	      counts = rate = ciLower = ciUpper = numeric(nLevels)

	      for (k in 1:nLevels) {
	        counts[k] <- sum(d == levels[k])
	        rate[k] <- counts[k]/nObs

	        r <- stats::binom.test(counts[k], nObs, p = testValue,
	                               alternative = hyp,
	                               conf.level = options$descriptivesPlotsConfidenceInterval)
					ciLower[k] = r$conf.int[1]
					ciUpper[k] = r$conf.int[2]

	      }

	      summaryStat <- data.frame(groupingVariable = levels, rate = rate,
																	ciLower = ciLower, ciUpper = ciUpper)
	      testValue <- data.frame(testValue = testValue)


	      p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
	                                                     y = rate, group = 1)) +
					ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
	                               colour = "black", width = 0.2, position = pd) +
				  ggplot2::geom_point(position = pd, size = 4) +
	        ggplot2::geom_hline(data = testValue,
															ggplot2::aes(yintercept = testValue),
	                      			linetype = "dashed") +
	        ggplot2::ylab(NULL) +
					ggplot2::xlab(NULL) +
	        ggplot2::theme_bw() +
	        ggplot2::ylim(min = 0, max = 1) +
	        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
	                       plot.title = ggplot2::element_text(size = 18),
	                       panel.grid.major = ggplot2::element_blank(),
	                       axis.title.x = ggplot2::element_blank(),
	                       axis.title.y = ggplot2::element_text(size = 18,
	                                                            vjust = -1),
	                       axis.text.x = ggplot2::element_text(size = 15),
	                       axis.text.y = ggplot2::element_text(size = 15),
	                       panel.background =
	                         ggplot2::element_rect(fill = "transparent",
	                                               colour = NA),
	                       plot.background =
	                         ggplot2::element_rect(fill = "transparent",
	                                               colour = NA),
	                       legend.background =
	                         ggplot2::element_rect(fill = "transparent",
	                                               colour = NA),
	                       panel.border = ggplot2::element_blank(),
	                       axis.line = ggplot2::element_blank(),
	                       legend.key = ggplot2::element_blank(),
	                       legend.title = ggplot2::element_text(size = 12),
	                       legend.text = ggplot2::element_text(size = 12),
	                       axis.ticks = ggplot2::element_line(size = 0.5),
	                       axis.ticks.margin = grid::unit(1, "mm"),
	                       axis.ticks.length = grid::unit(3, "mm"),
	                       plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
	        base_breaks_y(summaryStat, testValue$testValue)


				if (initial == TRUE){
					# Initial width dependent on levels, later widths not
					image <- .beginSaveImage(80 * nLevels,
		                               options$plotHeight)
				} else {
					image <- .beginSaveImage(options$plotWidth,
		                               options$plotHeight)
				}

	      print(p)
	      content <- .endSaveImage(image)

	      descriptivesPlot[["data"]] <- content

			}

    } else {
      descriptivesPlot[["data"]] <- ""
    }

    descriptivesPlotList[[i]] <- descriptivesPlot

  }

  descriptivesPlotList
}
