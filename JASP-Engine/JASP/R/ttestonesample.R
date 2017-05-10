#
# Copyright (C) 2013-2015 University of Amsterdam
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

TTestOneSample <- function(dataset = NULL, options, perform = "run",
						   callback = function(...) 0,  ...) {

 	state <- .retrieveState()

	variables <- unlist(options$variables)
	init <- .initializeTTest(dataset, options, perform, type = "one-sample")
	results <- init[["results"]]

	## if the dataset is not null, let's change it a bit
	if (!is.null(dataset)) {

		if (options$missingValues == "excludeListwise") {
			exclude <- variables
		} else {
			exclude <- NULL
		}
		dataset <- .vdf(dataset, column.as.numeric = variables, exclude.na.listwise = exclude)

	} else {
		dataset <- init[["dataset"]]
	}

	## call the specific one-sample T-Test functions
	results[["ttest"]] <- .ttestOneSample(dataset, options, perform)
	descriptivesTable <- .ttestOneSamplesDescriptives(dataset, options, perform)
	shapiroWilk <- .ttestOneSampleNormalityTest(dataset, options,  perform)
	results[["assumptionChecks"]] <- list(shapiroWilk = shapiroWilk, title = "Assumption Checks")


	keep <- NULL

	## if the user wants descriptive plots, s/he shall get them!
	if (options$descriptivesPlots) {

		plotTitle <- ifelse(length(options$variables) > 1, "Descriptives Plots", "Descriptives Plot")
		descriptivesPlots <- .ttestOneSamplesDescriptivesPlot(dataset, options, perform)
		if (!is.null(descriptivesPlots[[1]][["obj"]])){
			keep <- unlist(lapply(descriptivesPlots, function(x) x[["data"]]),NULL)
		}
		results[["descriptives"]] <- list(descriptivesTable = descriptivesTable, 
																			title = "Descriptives", 
																			descriptivesPlots = list(collection = descriptivesPlots, 
																															 title = plotTitle))
	} else {

		results[["descriptives"]] <- list(descriptivesTable = descriptivesTable, title = "Descriptives")
	}

	## return the results object
	if (perform == "init") {
		return(list(results=results, status="inited"))
	} else {
		return(list(results=results, status="complete", 
								state = list(options = options, results = results),
								keep = keep))
	}
}


.ttestOneSample <- function(dataset, options, perform) {

	ttest <- list()

	## does the user want mean differences, effect sizes and confidence intervals?
	wantsEffect <- options$effectSize
	wantsDifference <- options$meanDifference
	wantsConfidence <- options$confidenceInterval # pah! can't get that classically :P

	wantsStudents <- options$students
	wantsWilcox <- options$mannWhitneyU

	allTests <- c(wantsStudents, wantsWilcox)
	onlyTest <- sum(allTests) == 1

	fields <- list(list(name = "v", type = "string", title = ""),
				   list(name = "df", type = "integer"),
				   list(name = "p", type = "number", format = "dp:3;p:.001"))

	footnotes <- .newFootnotes()
	title <- "One Sample T-Test"

	## get the right title and test statistic for the table

	if (wantsWilcox && onlyTest) {
		#title <- "Mann-Whitney U Test"
		testname <- "Wilcoxon signed-rank test"
		testTypeFootnote <- paste0(testname, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = testTypeFootnote)
		testStat <- "V"

		## additionally, Wilcoxon's test doesn't have degrees of freedoms
		fields <- fields[-2]
	} else if (wantsStudents && onlyTest) {
		#title <- "Student's T-Test"
		testname <- "Student's t-test"
		testTypeFootnote <- paste0(testname, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = testTypeFootnote)
		testStat <- "t"
	} else {
		# title <- "One Sample T-Test"
		testStat <- "Statistic"
	}

	ttest[["title"]] <- title

	## if only conducting Student's, the table should have "t" as column name
	## for the test statistic when doing only Wilcoxon's, the name should be
	## "V"; when doing both, it should be "statistic"
	fields <- append(fields, list(list(name = testStat,
								  type = "number", format = "sf:4;dp:3")), 1)

	## if the user wants all tests, add a column called "Test"
	if (sum(allTests) == 2) {
		fields <- append(fields, list(list(name = "test",
									  type = "string", title = "Test")), 1)
	}

	if (options$VovkSellkeMPR){
    .addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
		<em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR\u002A",
                                        type = "number",
                                        format = "sf:4;dp:3")
 	}

	if (wantsDifference) {
		fields[[length(fields) + 1]] <- list(name = "m", title = "Mean Difference",
											 type = "number", format = "sf:4;dp:3")
	}

	if (wantsEffect && wantsStudents) {
		fields[[length(fields) + 1]] <- list(name = "d", title = "Cohen's d",
											 type = "number",  format = "sf:4;dp:3")
	}

	if (wantsConfidence) {
		interval <- 100 * options$confidenceIntervalInterval
		title <- paste0(interval, "% Confidence interval")

		fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
											 format = "sf:4;dp:3", title = "Lower",
											 overTitle = title)
		fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
											 format = "sf:4;dp:3", title = "Upper",
											 overTitle = title)
	}

	ttest[["schema"]] <- list(fields = fields)

	###########################
	### check the directionality
	if (options$hypothesis == "greaterThanTestValue") {
		direction <- "greater"
		note <- "For all tests, the alternative hypothesis specifies that the mean
					is greater than "
	} else if (options$hypothesis == "lessThanTestValue") {
		direction <- "less"
		note <- "For all tests, the alternative hypothesis specifies that the mean
					is less than "
	} else {
	  direction <- "two.sided"
		note <- "For all tests, the alternative hypothesis specifies that the population mean is different from "
	}
	
	if (options$testValue != 0) {
  	message <- paste0(note, options$testValue, ".")
  	if (wantsWilcox && !wantsStudents) {
  	  message <- gsub(pattern = "mean", replacement = "median", x = message)
  	} else if (wantsWilcox && wantsStudents){
  	  tMessage <- gsub(pattern = "For all tests", replacement = "For the Student t-tests", x = paste0(note, options$testValue))
  	  wilcoxMessage <- gsub(pattern = "mean", replacement = "median", x = paste0(note, options$testValue))
  	  wilcoxMessage <- gsub(pattern = "For all tests", replacement = "for the Wilcoxon test", x = wilcoxMessage)
  	  message <- paste0(tMessage, "; ", wilcoxMessage)
  	}
  	.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)
	}


	variables <- options$variables
	if (length(variables) == 0)  variables = "."

	rowNo <- 1
	ttest.rows <- list() # for each variable and each test, save stuff in there
	whichTests <- list("1" = wantsStudents, "2" = wantsWilcox)

	## add a row for each variable, even before we are conducting tests
	for (variable in variables) {
		ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
	}

	for (variable in variables) {

		for (test in seq_len(length(whichTests))) {

			currentTest <- whichTests[[test]]

			## don't run a test the user doesn't want
			if (!currentTest) {
				next
			}

			if (perform == "run" && length(options$variables) > 0) {

				row <- try(silent = TRUE, expr = {

					ci <- options$confidenceIntervalInterval
					dat <- na.omit(dataset[[ .v(variable) ]])

					if (test == 2) {
						r <- stats::wilcox.test(dat, alternative = direction,
												mu = options$testValue,
												conf.level = ci, conf.int = TRUE)
					} else {
						r <- stats::t.test(dat, alternative = direction,
										   mu = options$testValue, conf.level = ci)
					}

					## same for all tests
					p <- as.numeric(r$p.value)
					stat <- as.numeric(r$statistic)
					m <- as.numeric(r$estimate - r$null.value)
					ciLow <- .clean(as.numeric(r$conf.int[1]))
					ciUp <- .clean(as.numeric(r$conf.int[2]))

					## only for Student's t-test
					df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
					d <- ifelse(is.null(r$parameter), "", .clean((mean(dat) - options$testValue) / sd(dat)))
					
					if (is.na(t)) {
						stop("data are essentially constant")
					}

					res <- list(v = variable, df = df, p = p,
								m = m, d = d, lowerCI = ciLow, upperCI = ciUp)
					res[[testStat]] <- stat
					if (options$VovkSellkeMPR){
					  res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
					}
					res
				})

				## if there has been an error, find out which and log as a footnote
				if (class(row) == "try-error") {

					errorMessage <- .extractErrorMessage(row)

					if (errorMessage == "missing value where TRUE/FALSE needed") {

						err <- "t-statistic is undefined - the sample contains infinity"

					} else if (errorMessage == "data are essentially constant") {

						err <- paste0("t-statistic is undefined - the sample ",
									  "contains all the same value (zero variance)")

					} else if (errorMessage == "not enough 'x' observations") {

						err <- "t-statistic is undefined - sample contains only one value"
					}

					index <- .addFootnote(footnotes, err)
					row.footnotes <- list(t = list(index))

					row <- list(v = variable, df = "", p = "", m = "",
								lowerCI = "", upperCI = "",
								.footnotes = row.footnotes)
					row[[testStat]] <- .clean(NaN)
				}

			## if we are not yet ready to perform, just create an empty table
			} else {
				row <- list(v = variable, df = ".", p = ".",
							m = ".",  d = ".", lowerCI = ".", upperCI = ".")
				row[[testStat]] <- "."
			}

			## if we have both tests, we do not want to have two variables
			## and we want another column with the test type
			hasBoth <- rowNo %% 2 == 0 && sum(allTests) == 2
			if (hasBoth) {
				row[["v"]] <- ""
				row[["test"]] <- "Wilcoxon"
				ttest.rows[[rowNo - 1]][["test"]] <- "Student"
			}

			ttest.rows[[rowNo]] <- row
			rowNo <- rowNo + 1
		}
	}

	ttest[["data"]] <- ttest.rows
	ttest[["footnotes"]] <- as.list(footnotes)
	ttest
}


.ttestOneSamplesDescriptives <- function(dataset, options, perform) {
	if (!options$descriptives) return(NULL)

	descriptives <- list()
	descriptives[["title"]] <- "Descriptives"
	descriptives[["cases"]] <- I(options$variables)

	fields <- list(
		list(name = "v", title = "", type = "string"),
		list(name = "N", title = "N", type = "number", format = "sf:4;dp:3"),
		list(name = "mean", title = "Mean", type = "number", format = "sf:4;dp:3"),
		list(name = "sd", title = "SD", type = "number", format = "sf:4;dp:3"),
		list(name = "se", title = "SE", type = "number", format = "sf:4;dp:3")
	)

	descriptives[["schema"]] <- list(fields = fields)
	descriptives.results <- list()

	variables <- options$variables
	if (length(variables) == 0) variables = "."

	for (variable in variables) {

		if (perform == "run" && length(options$variables) > 0) {

			data <- na.omit(dataset[[.v(variable)]])

			if (class(data) != "factor") {

				n <- .clean(length(data))
				mean <- .clean(mean(data))
				stdDeviation <- .clean(sd(data))
				stdErrorMean <- .clean(sd(data) / sqrt(length(data)))
				newGroup <- length(descriptives.results) == 0

				result <- list(v = variable, N = n, mean = mean,
							   sd = stdDeviation, se = stdErrorMean,
							   .isNewGroup = newGroup)
			} else {

				newGroup <- length(descriptives.results) == 0
				n <- .clean(length(data))
				result <- list(v = variable, N = n, mean = "",
							   sd = "", se = "", .isNewGroup = newGroup)
			}
		} else {

			newGroup <- length(descriptives.results) == 0
			result <- list(v = variable, N = ".", mean = ".",
						   sd = ".", se = ".", .isNewGroup = newGroup)
		}
		descriptives.results[[length(descriptives.results) + 1]] <- result
	}

	descriptives[["data"]] <- descriptives.results
	descriptives
}


.ttestOneSampleNormalityTest <- function(dataset, options, perform) {
	if (!options$normalityTests) return(NULL)

	normalityTests <- list("title" = "Test of Normality (Shapiro-Wilk)")
	normalityTests[["cases"]] <- I(options$variables)

	fields <- list(
		list(name = "v", title = "", type = "string"),
		list(name = "W", title = "W", type = "number", format = "sf:4;dp:3"),
		list(name = "p", title = "p", type = "number", format = "dp:3;p:.001")
	)

	normalityTests[["schema"]] <- list(fields = fields)

	footnotes <- .newFootnotes()
	.addFootnote(footnotes, symbol = "<em>Note.</em>",
				 text = "Significant results suggest a deviation from normality.")

	normalityTests.results <- list()

	variables <- options$variables
	if (length(variables) == 0) variables = "."

	for (variable in variables) {

		if (perform == "run" && length(options$variables) > 0) {

			data <- na.omit(dataset[[.v(variable)]])

			row.footnotes <- NULL
			error <- FALSE

			if (length(data) < 3) {

				err <- "Too few observations (N < 3) to compute statistic reliably."
				foot.index <- .addFootnote(footnotes, err)
				row.footnotes <- list(W = list(foot.index), p = list(foot.index))
				error <- TRUE

			} else if (length(data) > 5000) {

				err <- "Too many observations (N > 5000) to compute statistic reliably."
				foot.index <- .addFootnote(footnotes, err)
				row.footnotes <- list(W = list(foot.index), p = list(foot.index))
				error <- TRUE
			}

			if (!error) {

			  r <- stats::shapiro.test(data)
			  W <- .clean(as.numeric(r$statistic))
			  p <- .clean(r$p.value)

			  if (length(normalityTests.results) == 0) {
				newGroup <- TRUE
			  } else {
				newGroup <- FALSE
			  }

			  result <- list(v = variable, W = W, p = p, .isNewGroup = newGroup)

			} else {

			  if (length(normalityTests.results) == 0) {
				newGroup <- TRUE
			  } else {
				newGroup <- FALSE
			  }

			  result <- list(v = variable, W = "NaN", p = "NaN",
							 .isNewGroup = newGroup, .footnotes = row.footnotes)
			}

		} else {

			if (length(normalityTests.results) == 0) {
			  newGroup <- TRUE
			} else {
			  newGroup <- FALSE
			}

			result <- list(v = variable, W = ".", p = ".", .isNewGroup = newGroup)

		}

		normalityTests.results[[length(normalityTests.results) + 1]] <- result
	}

	normalityTests[["data"]] <- normalityTests.results
	normalityTests[["footnotes"]] <- as.list(footnotes)
	normalityTests
}


.ttestOneSamplesDescriptivesPlot <- function(dataset, options, perform) {

	descriptivesPlotList <- list()
	base_breaks_y <- function(x, options) {

		values <- c(options$testValue, x[, "dependent"] - x[, "ci"],
					x[, "dependent"] + x[, "ci"])
		ci.pos <- c(min(values), max(values))
		b <- pretty(ci.pos)
		d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
		list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
			yend = yend), inherit.aes = FALSE, size = 1),
			ggplot2::scale_y_continuous(breaks = c(min(b),  options$testValue, max(b))))
	}

	for (i in .indices(options$variables)) {

		var <- options$variables[[i]]

		descriptivesPlot <- list("title" = var)
		descriptivesPlot[["width"]] <- options$plotWidth
		descriptivesPlot[["height"]] <- options$plotHeight
		descriptivesPlot[["custom"]] <- list(width = "plotWidth", height = "plotHeight")

		if (perform == "run" && var != "") {

			dataSubset <- data.frame(dependent = dataset[[.v(var)]],
						  groupingVariable = rep(var, length(dataset[[.v(var)]])))

			ci <- options$descriptivesPlotsConfidenceInterval
			summaryStat <- .summarySE(dataSubset, measurevar = "dependent",
									  groupvars = "groupingVariable",
									  conf.interval = ci, na.rm = TRUE, .drop = FALSE)

			testValue <- data.frame(testValue = options$testValue)
			pd <- ggplot2::position_dodge(0.2)

			p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
				y = dependent, group = 1)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,
				ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
				ggplot2::geom_line(position = pd, size = 0.7) + ggplot2::geom_point(position = pd,
				size = 4) + ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept = testValue),
				linetype = "dashed") + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
				ggplot2::theme_bw() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
				plot.title = ggplot2::element_text(size = 18), panel.grid.major = ggplot2::element_blank(),
				axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_text(size = 18,
				  vjust = -1), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 15),
				panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
				plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
				legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
				panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
				legend.key = ggplot2::element_blank(), legend.title = ggplot2::element_text(size = 12),
				legend.text = ggplot2::element_text(size = 12), axis.ticks = ggplot2::element_line(size = 0.5),
				axis.ticks.margin = grid::unit(1, "mm"), axis.ticks.length = grid::unit(3,
				"mm"), axis.ticks.x = ggplot2::element_blank(), plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
				base_breaks_y(summaryStat, options)

			imgObj <- .writeImage(width = options$plotWidth, 
														height = options$plotHeight, 
														plot = p)

			descriptivesPlot[["data"]] <- imgObj[["png"]]
			descriptivesPlot[["obj"]] <- imgObj[["obj"]]
			descriptivesPlot[["convertible"]] <- TRUE
			descriptivesPlot[["status"]] <- "complete"

		} else {

			descriptivesPlot[["data"]] <- ""
		}

		descriptivesPlotList[[i]] <- descriptivesPlot
	}
	descriptivesPlotList
}
