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
	results <- list()

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
	descriptPlots <- NULL
	binomResults <- NULL

	if (!is.null(state)){

		diff <- .diff(state$options, options)

		if (is.list(diff) && !any(diff[["variables"]],
				diff[["confidenceIntervalInterval"]],
				diff[["hypothesis"]],
				diff[["testValue"]])){

			binomResults <- state$binomResults
		}

		if (is.list(diff) && options[["descriptivesPlots"]] &&
				!any(diff[["variables"]], diff[["testValue"]],
				diff[["descriptivesPlots"]],
				diff[["descriptivesPlotsConfidenceInterval"]],
				diff[["plotWidth"]],
				diff[["plotHeight"]])){
			descriptPlots <- state$descriptPlots
		}

	}


	# Meta information
	descriptivesPlotsMeta <- list()

	for (j in .indices(variables)){
		descriptivesPlotsMeta[[j]] <- list(name = variables[j], type = "collection",
																			 meta = "image")
	}

	results[["title"]] <- "Binomial Test"
	results[[".meta"]] <- list(list(name = "binomial", type = "table"),
	                           list(name = "descriptives", type = "object",
	                                meta = descriptivesPlotsMeta))


	# Generate results
  if (is.null(binomResults)){

		binomResults <- .binomialTest(dataset, options, variables, perform)

	}


  results[["binomial"]] <- .binomialTable(binomResults, options, variables,
                                          perform)


  if (options[["descriptivesPlots"]]){

		if (is.null(descriptPlots)) {
			descriptPlots <- .binomialDescriptivesPlot(dataset, options, variables,
																								 perform)
		}

		# select list of plot paths to keep when rerunning analysis.
		plotPaths <- list()
		for (i in 1:length(descriptPlots)){
		  if (is.list(descriptPlots[[i]])){
				for (j in 1:length(descriptPlots[[i]]$collection)){
      		plotPaths[[length(plotPaths)+1]] <-
						descriptPlots[[i]]$collection[[j]]$data
		    }
		  }
		}

    results[["descriptives"]] <- descriptPlots
		results[["descriptives"]][["title"]] <- "Descriptive Plots"

  } else {

	  results[["descriptives"]] <- NULL
		plotPaths <- list()

	}



  if (perform == "run") {

		state <- list()
		state[["options"]] <- options
		state[["binomResults"]] <- binomResults
		state[["binomTable"]] <- results[["binomial"]]
		state[["descriptPlots"]] <- results[["descriptives"]]

    return(list(results=results, status="complete", state=state,
								keep = plotPaths))

  } else {

		return(list(results=results, status="inited", state=state,
								keep = plotPaths))

  }

}

.binomialTable <- function(r, options, variables, perform){

  table <- list()
	footnotes <- .newFootnotes()
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

	if (options$VovkSellkeMPR) {
    .addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
    <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
    possible odds in favor of H\u2081 over H\u2080 equals
    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
    (Sellke, Bayarri, & Berger, 2001).")
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR\u002A",
                                        type = "number",
                                        format = "sf:4;dp:3")
 	}

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
		if (options$VovkSellkeMPR){
			for (row in 1:length(table[["data"]])){
			  table[["data"]][[row]][["VovkSellkeMPR"]] <- .VovkSellkeMPR(table[["data"]][[row]][["p"]])
			}
		}
    table[["status"]] <- "complete"

  } else {

    data <- list()

    if (is.null(variables)){
      variables <- ""
    }

    for (var in variables){
		  if (options$VovkSellkeMPR){
				data[[length(data) + 1]] <- list(case=var, level=".", counts=".",
                                         total=".",  proportion=".", p=".",
                                         VovkSellkeMPR=".", lowerCI=".",
																				 upperCI=".")
		 	} else {
				data[[length(data) + 1]] <- list(case=var, level=".", counts=".",
	                                       total=".",  proportion=".", p=".",
	                                       lowerCI=".", upperCI=".")
			}
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

.binomialDescriptivesPlot <- function(dataset, options, variables, perform) {

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


		for (k in .indices(levels(d))){

			level <- levels(d)[k]

			descriptivesPlot <- list("title" = level)

			if (perform == "run") {

				count <- sum(d == level)
				rate <- count/nObs


				descriptivesPlot[["width"]] <- options$plotWidth
				descriptivesPlot[["height"]] <- options$plotHeight
				descriptivesPlot[["custom"]] <- list(width = "plotWidth",
																					 	 height = "plotHeight")

			  r <- stats::binom.test(x = count, n = nObs, p = testValue,
 															 alternative = "two.sided",
 															 conf.level = confLevel)
				ciLower <- r$conf.int[1]
				ciUpper <- r$conf.int[2]

				summaryStat <- data.frame(label = level, rate = rate, ciLower = ciLower,
																	ciUpper = ciUpper)

	      dfTestValue <- data.frame(testValue = testValue)

				p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label,
																											 y = rate, group = 1)) +
					ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
																 colour = "black", width = 0.2, position = pd) +
					ggplot2::geom_point(position = pd, size = 4) +
					ggplot2::geom_hline(data = dfTestValue,
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
					base_breaks_y(summaryStat, dfTestValue$testValue)

					
					content <- .writeImage(width = options$plotWidth, 
										   height = options$plotHeight, 
										   plot = p, obj = TRUE)

					descriptivesPlot[["convertible"]] <- TRUE
					descriptivesPlot[["obj"]] <- content[["obj"]]
					descriptivesPlot[["data"]] <- content[["png"]]
					descriptivesPlot[["status"]] <- "complete"

				} else {

					descriptivesPlot[["width"]] <- options$plotWidth
					descriptivesPlot[["height"]] <- options$plotHeight
					descriptivesPlot[["custom"]] <- list(width = "plotWidth",
																						 	 height = "plotHeight")
					descriptivesPlot[["data"]] <- ""

				}

				descriptivesPlotList[[k]] <- descriptivesPlot
		}

		descriptivesPlotVar <- list(collection = descriptivesPlotList, title = var)

		descriptivesPlotCollection[[var]] <- descriptivesPlotVar

	}

	return(descriptivesPlotCollection)
}
