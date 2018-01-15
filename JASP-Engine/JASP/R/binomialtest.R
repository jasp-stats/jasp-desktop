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

BinomialTest <- function(dataset, options, perform, state, ...) {

	variables <- unlist(options$variables)

	binomResults <- state$binomResults
  if (is.null(binomResults))
		binomResults <- .binomialTest(dataset, options, perform, variables)

	descriptPlots <- state$descriptPlots
  if (options[["descriptivesPlots"]] && is.null(descriptPlots))
		descriptPlots <- .binomialDescriptivesPlot(dataset, options, perform, variables)

	results <- list()
	results[["binomial"]] <- .BinomialTable(binomResults, options)

	if (! is.null(descriptPlots)) {
		results[["descriptives"]] <- descriptPlots
		results[["descriptives"]][["title"]] <- "Descriptive Plots"
	}

  if (perform == "init")
		return(list(results=results, status="inited", state=state))

	state <- list(
		options = options,
		binomResults = binomResults,
		descriptPlots = descriptPlots
	)

  return(list(results=results, status="complete", state=state))
}

.binomialTest <- function(dataset, options, perform, variables) {

  binomResults <- jasp.data.frame(colnames=c("case", "level", "counts", "total", "proportion", "p", "VovkSellkeMPR", "lowerCI", "upperCI"))

  if (perform == "init" || is.null(variables)) {

    binomResults <- rbind(binomResults, rep(".", ncol(binomResults)))

  } else {

    if (options$hypothesis == "notEqualToTestValue") {
      hyp <- "two.sided"
    } else if (options$hypothesis == "greaterThanTestValue") {
      hyp <- "greater"
    } else {
      hyp <- "less"
    }

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
        vovk <- .VovkSellkeMPR(p)
        cilo <- r$conf.int[1]
        ciup <- r$conf.int[2]

        if (p == FALSE) {
          p <- 0
        } else if (p == TRUE) {
          p <- 1
        }

        binomResults <- rbind(binomResults, c(var, lev, counts, n, prop, p, vovk, cilo, ciup))
      }
    }

  }

  return(binomResults)
}

.BinomialTable <- function(binomResults, options) {

	footnotes <- .newFootnotes()
	.addFootnote(footnotes, symbol="<em>Note.</em>",
		text = switch(options$hypothesis,
						"notEqualToTestValue" = .messages("footnote", "binomNeq", value=options$testValue),
						"greaterThanTestValue" = .messages("footnote", "binomGreater", value=options$testValue),
						"lessThanTestValue" = .messages("footnote", "binomLess", value=options$testValue)))

	if (options$VovkSellkeMPR)
		.addFootnote(footnotes, symbol = "\u002A", text = .messages("footnote", "VovkSellkeMPR"))

	attr(binomResults, "jasp.footnotes") <- footnotes

	if (options$confidenceInterval) {
		schema <- list(
			list(name="lowerCI", overTitle=paste0(options$confidenceIntervalInterval * 100, "% Confidence Interval")),
			list(name="upperCI", overTitle=paste0(options$confidenceIntervalInterval * 100, "% Confidence Interval"))
		)
		attr(binomResults, "jasp.schema") <- schema
	}

	if (! options$VovkSellkeMPR)
		binomResults <- subset(binomResults, select=-VovkSellkeMPR)

	if (! options$confidenceInterval)
		binomResults <- subset(binomResults, select=-c(lowerCI, upperCI))

	return(binomResults)
}

.binomialDescriptivesPlot <- function(dataset, options, perform, variables) {

	if (is.null(variables))
		return(NULL)

  descriptivesPlotCollection <- list()
	testValue <- options$testValue
	confLevel <- options$descriptivesPlotsConfidenceInterval

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

		descriptivesPlotList <- list()

		for (k in .indices(levels(d))) {

			level <- levels(d)[k]

			if (perform == "init") {

				descriptivesPlot <- plot.new

			} else {

				count <- sum(d == level)
				rate <- count/nObs

			  r <- stats::binom.test(x = count, n = nObs, p = testValue,
																 alternative = "two.sided",
																 conf.level = confLevel)
				ciLower <- r$conf.int[1]
				ciUpper <- r$conf.int[2]

				summaryStat <- data.frame(label = level, rate = rate, ciLower = ciLower,
																	ciUpper = ciUpper)

	      dfTestValue <- data.frame(testValue = testValue)

				descriptivesPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label,
																											 y = rate, group = 1)) +
					ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
																 colour = "black", width = 0.2, position = pd) +
					ggplot2::geom_point(position = pd, size = 4) +
					ggplot2::geom_hline(data = dfTestValue,
															ggplot2::aes(yintercept = testValue),
															linetype = "dashed") +
					ggplot2::ylab(NULL) +
					ggplot2::xlab(NULL) +
					ggplot2::ylim(min = 0, max = 1) +
					base_breaks_y(summaryStat, dfTestValue$testValue) 
					
				descriptivesPlot <- JASPgraphs::themeJasp(descriptivesPlot) + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

				}

				attr(descriptivesPlot, "jasp.title") <- level
				attr(descriptivesPlot, "jasp.name") <- "descriptivesPlot"
				descriptivesPlotList[[k]] <- descriptivesPlot
		}

		descriptivesPlotVar <- list(collection = descriptivesPlotList, title = var)
		descriptivesPlotCollection[[var]] <- descriptivesPlotVar

	}
	return(descriptivesPlotCollection)
}
