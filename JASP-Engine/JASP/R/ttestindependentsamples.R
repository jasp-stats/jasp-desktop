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

TTestIndependentSamples <- function(dataset = NULL, options, perform = "run",
									callback = function(...) 0, ...) {

	## call the common initialization function
	init <- .initializeTTest(dataset, options, perform, type = "independent-samples")

	results <- init[["results"]]
	dataset <- init[["dataset"]]
	
	if (length(options$variables) != 0 && options$groupingVariable != '') {
		errors <- .hasErrors(dataset, perform, type = 'factorLevels',
												factorLevels.target = options$groupingVariable, factorLevels.amount = '!= 2',
												exitAnalysisIfErrors = TRUE)
	}

	## call the specific independent T-Test functions
	results[["ttest"]] <- .ttestIndependentSamplesTTest(dataset, options, perform)
	descriptivesTable <- .ttestIndependentSamplesDescriptives(dataset, options, perform)
	levene <- .ttestIndependentSamplesInequalityOfVariances(dataset, options, perform)
	shapiroWilk <- .ttestIndependentSamplesNormalityTest(dataset, options, perform)
	results[["assumptionChecks"]] <- list(shapiroWilk = shapiroWilk, levene = levene, title = "Assumption Checks")

	## if the user wants descriptive plots, s/he shall get them!
	if (options$descriptivesPlots) {

		plotTitle <- ifelse(length(options$variables) > 1, "Descriptives Plots", "Descriptives Plot")
		descriptivesPlots <- .independentSamplesTTestDescriptivesPlot(dataset, options, perform)
		results[["descriptives"]] <- list(descriptivesTable = descriptivesTable, title = "Descriptives", descriptivesPlots = list(collection = descriptivesPlots, title = plotTitle))

	} else {

		results[["descriptives"]] <- list(descriptivesTable = descriptivesTable, title = "Descriptives")
	}

	## return the results object
	results
}


.ttestIndependentSamplesTTest <- function(dataset, options, perform) {

	ttest <- list()
	wantsEffect <- options$effectSize
	wantsDifference <- options$meanDifference
	wantsConfidence <- options$confidenceInterval

	## can make any combination of the following tests:
	wantsWelchs <- options$welchs
	wantsStudents <- options$students
	wantsWilcox <- options$mannWhitneyU

	## setup table for the independent samples t-test; add column test at the
	## beginning, and remove it later should the user only specify one test
	fields <- list(list(name = "v", title = "", type = "string", combine = TRUE),
				   list(name = "test", type = "string", title = "Test"),
				   list(name = "df", type = "number", format = "sf:4;dp:3"),
				   list(name = "p", type = "number", format = "dp:3;p:.001"))

	allTests <- c(wantsStudents, wantsWelchs, wantsWilcox)
	onlyTest <- sum(allTests) == 1

	title <- "Independent Samples T-Test"
	footnotes <- .newFootnotes()

	## get the right statistics for the table and, if only one test type, add footnote

	if (wantsWilcox && onlyTest) {

		testname <- "Mann-Whitney U Test"
		testTypeFootnote <- paste0(testname, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = testTypeFootnote)
		testStat <- "W"
		## additionally, Wilcoxon's test doesn't have degrees of freedoms
		fields <- fields[-3]
	} else if (wantsWelchs && onlyTest) {

		testname <- "Welch's T-Test"
		testTypeFootnote <- paste0(testname, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = testTypeFootnote)
		testStat <- "t"
	} else if (wantsStudents && onlyTest) {

		testname <- "Student's T-Test"
		testTypeFootnote <- paste0(testname, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = testTypeFootnote)
		testStat <- "t"
	} else {

		testStat <- "statistic"
	}

	ttest["title"] <- title

	## if only doing Student's / Welch's, the table should have "t" as column name
	## for the test statistic; when doing only Wilcoxon's, the name should be "W";
	## when doing both, it should be "statistic"
	fields <- append(fields, list(list(name = testStat, type = "number",
									   format = "sf:4;dp:3")), 2)

  ## add max(BF_10) from commonBF
	if (options$VovkSellkeMPR){
		.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
	  <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
		fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
																				 title = "VS-MPR\u002A",
 											 							 		 type = "number",
																				 format = "sf:4;dp:3")
 	}


	## add mean difference and standard error difference
	if (wantsDifference) {
		fields[[length(fields) + 1]] <- list(name = "md", title = "Mean Difference",
											 type = "number", format = "sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name = "sed", title = "SE Difference",
											 type = "number", format = "sf:4;dp:3")
	}

	## add Cohen's d
	if (wantsEffect) {
		fields[[length(fields) + 1]] <- list(name = "d", title = "Cohen's d",
											 type = "number", format = "sf:4;dp:3")
	}

	## I hope they know what they are doing! :)
	if (wantsConfidence) {
		interval <- 100 * options$confidenceIntervalInterval
		title <- paste0(interval, "% Confidence Interval")

		fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
											 format = "sf:4;dp:3", title = "Lower",
											 overTitle = title)
		fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
											 format = "sf:4;dp:3", title = "Upper",
											 overTitle = title)
	}

	## add all the fields that we may or may not have added
	## in the initialization phase, remove the Test column
	ttest[["schema"]] <- list(fields = fields[-2])

	## check if we are ready to perform!
	ready <- (perform == "run" && length(options$variables) != 0
			  && options$groupingVariable != "")

	ttest.rows <- list()
	variables <- options$variables
	if (length(variables) == 0) variables <- "."

	## add a row for each variable, even before we are conducting tests
	for (variable in variables) {
		ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
	}

	if (ready) {
		levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])

		## does the user have a direction in mind?
		if (options$hypothesis == "groupOneGreater") {

			direction <- "greater"
			message <- paste0("For all tests, the alternative hypothesis specifies that group <em>", levels[1],
							  "</em> is greater than group <em>", levels[2], "</em>.")
			.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)

		} else if (options$hypothesis == "groupTwoGreater") {

			direction <- "less"
			message <- paste0("For all tests, the alternative hypothesis specifies that group  <em>", levels[1],
							  "</em> is less than group <em>", levels[2], "</em>.")
			.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)

		} else {
			direction <- "two.sided"
		}


		rowNo <- 1
		whichTests <- list("1" = wantsStudents, "2" = wantsWelchs, "3" = wantsWilcox)
		groupingData <- dataset[[ .v(options$groupingVariable) ]]

		## for each variable specified, run each test that the user wants
		for (variable in options$variables) {
			
			errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
													all.target = variable, all.grouping = options$groupingVariable,
													observations.amount = '< 1')

			variableData <- dataset[[ .v(variable) ]]

			## test is a number, indicating which tests should be run
			for (test in seq_len(length(whichTests))) {

				currentTest <- whichTests[[test]]

				## don't run a test the user doesn't want
				if (!currentTest) {
					next
				}
				
				errorMessage <- NULL
				row.footnotes <- NULL

				if (!identical(errors, FALSE)) {
					errorMessage <- errors$message
				} else {
					## try to run the test, catching eventual errors
					row <- try(silent = FALSE, expr = {

						
						ci <- options$confidenceIntervalInterval # what a mouthful!
						f <- as.formula(paste(.v(variable), "~",
											  .v(options$groupingVariable)))

						if (test == 3) {
							whatTest <- "Mann-Whitney"
							r <- stats::wilcox.test(f, data = dataset,
													alternative = direction,
													conf.int = TRUE, conf.level = ci, paired = FALSE)
							df <- ""
							m <- as.numeric(r$estimate)

						} else {
							whatTest <- ifelse(test == 2, "Welch's", "Student's")
							r <- stats::t.test(f, data = dataset, alternative = direction,
											   var.equal = test != 2, conf.level = ci, paired = FALSE)

							df <- as.numeric(r$parameter)
							m <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
						}

						## if the user doesn't want a Welch's t-test,
						## give a footnote indicating if the equality of variance
						## assumption is met; seems like in this setting there is no
						## sampling plan, thus the p-value is not defined. haha!
						if (!wantsWelchs && wantsStudents) {
							levene <- car::leveneTest(variableData, groupingData, "mean")

							## arbitrary cut-offs are arbitrary
							if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05) {
								error <- .messages('footnote', 'leveneSign')
								foot.index <- .addFootnote(footnotes, error)
								row.footnotes <- list(p = list(foot.index))

							}
						}

						## same for all t-tests
						p <- as.numeric(r$p.value)
						stat <- as.numeric(r$statistic)
						y <- dataset[[ .v(variable) ]]
						groups <- dataset[[ .v(options$groupingVariable) ]]

						sds <- tapply(y, groups, sd, na.rm = TRUE)
						ms <- tapply(y, groups, mean, na.rm = TRUE)
						ns <- tapply(y, groups, function(x) length(na.omit(x)))

						num <- (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
						sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
						d <- as.numeric((ms[1] - ms[2]) / sdPooled) # Cohen's d

						sed <- .clean(as.numeric(sqrt(sds[1]^2 / ns[1] + sds[2]^2 / ns[2])))
						ciLow <- .clean(r$conf.int[1])
						ciUp <- .clean(r$conf.int[2])

						# this will be the results object
						res <- list(v = variable, test = whatTest, df = df, p = p,
												md = m, d = d, lowerCI = ciLow,
												upperCI = ciUp, sed = sed, .footnotes = row.footnotes)
						res[[testStat]] <- stat
						if (options$VovkSellkeMPR){
							res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
						}
						res
					 })
					 
					## if there has been an error in computing the test, log it as footnote
					if (class(row) == "try-error") {
						errorMessage <- .extractErrorMessage(row)
					}
				}
				
				if (!is.null(errorMessage)) {
					## log the error in a footnote
					index <- .addFootnote(footnotes, errorMessage)
					row.footnotes <- list(t = list(index))

					row <- list(v = variable, test = "", df = "", p = "",
								md = "", lowerCI = "", upperCI = "",
								sed = "", .footnotes = list(t = list(index)))
					row[[testStat]] <- .clean(NaN)
				}

				## if the user only wants more than one test
				## update the table so that it shows the "Test" and "statistic" column
				if (sum(allTests) > 1) {
					ttest[["schema"]] <- list(fields = fields)
				}

				ttest.rows[[rowNo]] <- row
				rowNo <- rowNo + 1
			}
		}
		ttest[["footnotes"]] <- as.list(footnotes)
	}

	ttest[["data"]] <- ttest.rows
	ttest
}


.ttestIndependentSamplesDescriptives <- function(dataset, options, perform,
												 state = NULL, diff = NULL) {
	if (options$descriptives == FALSE) return(NULL)

	descriptives = list("title" = "Group Descriptives")

	## sets up the table for the descriptives
	fields <- list(
		list(name = "variable", title = "", type = "string", combine = TRUE),
		list(name = "group", title = "Group", type = "string"),
		list(name = "N", title = "N", type = "number"),
		list(name = "mean", title = "Mean", type = "number", format = "sf:4;dp:3"),
		list(name = "sd", title = "SD", type = "number", format = "sf:4;dp:3"),
		list(name = "se", title = "SE", type = "number", format = "sf:4;dp:3")
	)

	descriptives[["schema"]] <- list(fields = fields)
	data <- list()


	## function to check if everything is alright with the options
	isAllright <- function(variable, options, state = NULL, diff = NULL) {

		# check if the variable is in the state variables
		cond1 <- !is.null(state) && variable %in% state$options$variables

		# check if either diff is true, or it's a list and descriptives,
		# and groupingVariable, missingValues are FALSE
		cond2 <- (!is.null(diff) && (is.logical(diff) && diff == FALSE) || (is.list(diff)
				 && !any(diff$descriptives,diff$groupingVariable, diff$missingValues)))

		cond1 && cond2
	}

	variables <- options$variables
	if (length(variables) == 0) variables <- "."

	for (variable in variables) {

		if (isAllright(variable, options, state, diff)) {

			stateDat <- state$results$descriptives$data
			descriptivesVariables <- as.character(length(stateDat))

			for (i in seq_along(stateDat))
				descriptivesVariables[i] <- stateDat[[i]]$variable

			indices <- which(descriptivesVariables == variable)
			data[[length(data) + 1]] <- stateDat[[indices[1]]]
			data[[length(data) + 1]] <- stateDat[[indices[2]]]

		} else {
			data[[length(data) + 1]] <- list(variable = variable, .isNewGroup = TRUE)
			data[[length(data) + 1]] <- list(variable = variable)
		}
	}

	## check if we are done with all this crap
	done <- (!is.null(state) &&
			 state$options$descriptives &&
			 all(variables %in% state$options$variables))

	if (done) descriptives[["status"]] <- "complete"

	groups <- options$groupingVariable

	## if we actually have to do the test, and we have a grouping variable
	if (perform == "run" && groups != "") {
		levels <- base::levels(dataset[[ .v(groups) ]])

		rowNo <- 1
		groupingData <- dataset[[.v(groups)]]

		## do the whole loop as above again
		for (variable in variables) {

			# if everything is alright, add stuff to data
			if (isAllright(variable, options, state, diff)) {

				stateDat <- state$results$descriptives$data
				descriptivesVariables <- as.character(length(stateDat))

				for (i in seq_along(stateDat))
					descriptivesVariables[i] <- stateDat[[i]]$variable

				indices <- which(descriptivesVariables == variable)

				data[[rowNo]] <- stateDat[[indices[1]]]
				data[[rowNo]] <- stateDat[[indices[2]]]

				rowNo <- rowNo + 2

			} else {

				for (i in 1:2) {

				  level <- levels[i]
				  variableData <- dataset[[.v(variable)]]

				  groupData <- variableData[groupingData == level]
				  groupDataOm <- na.omit(groupData)

				  if (class(groupDataOm) != "factor") {

					  n <- .clean(length(groupDataOm))
					  mean <- .clean(mean(groupDataOm))
					  std <- .clean(sd(groupDataOm))
					  sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

					  result <- list(variable = variable, group = level,
									 N = n, mean = mean, sd = std, se = sem)

				  } else {

					n <- .clean(length(groupDataOm))
					result <- list(variable = variable, group = "",
								   N = n, mean = "", sd = "", se = "")
				}

				if (i == 1) {
					result[[".isNewGroup"]] <- TRUE
				}

				data[[rowNo]] <- result
				rowNo <- rowNo + 1
				}
			}
		}
		descriptives[["status"]] <- "complete"
	}

	descriptives[["data"]] <- data
	descriptives
}


.ttestIndependentSamplesInequalityOfVariances <- function(dataset, options, perform) {
	if (options$equalityOfVariancesTests == FALSE) return(NULL)

	levenes <- list("title" = "Test of Equality of Variances (Levene's)")
	footnotes <- .newFootnotes()

	## setup table for Levene's test
	fields <- list(list(name = "variable", title = "", type = "string"),
				   list(name = "F", type = "number", format = "sf:4;dp:3"),
				   list(name = "df", type = "integer"),
				   list(name = "p", type = "number", format = "dp:3;p:.001"))

	levenes[["schema"]] <- list(fields = fields)

	data <- list()
	variables <- options$variables
	groups <- options$groupingVariable
	if (length(variables) == 0) variables <- "."

	for (variable in variables) {
		data[[length(data) + 1]] <- list(variable = variable)
	}

	if (perform == "run" && groups != "") {

		levels <- base::levels(dataset[[ .v(groups) ]])

		rowNo <- 1

		for (variable in variables) {

			result <- try(silent = TRUE, expr = {

			  levene <- car::leveneTest(dataset[[ .v(variable) ]],
										dataset[[ .v(groups) ]], "mean")

			  F <- .clean(levene[1, "F value"])
			  df <- .clean(levene[1, "Df"])
			  p <- .clean(levene[1, "Pr(>F)"])

			  row <- list(variable = variable, F = F, df = df, p = p)

			  if (is.na(levene[1, "F value"])) {
				  note <- "F-statistic could not be calculated"
				  index <- .addFootnote(footnotes, note)
				  row[[".footnotes"]] <- list(F = list(index))
			  }

			  row
			})

			if (class(result) == "try-error") {
			  result <- list(variable = variable, F = "", df = "", p = "")
			}

			data[[rowNo]] <- result
			rowNo <- rowNo + 1
		}
	}
	levenes[["data"]] <- data
	levenes[["footnotes"]] <- as.list(footnotes)
	levenes
}


.ttestIndependentSamplesNormalityTest <- function(dataset, options, perform) {
	if (options$normalityTests == FALSE) return(NULL)

	normalityTests <- list("title" = "Test of Normality (Shapiro-Wilk)")

	## these are the table fields associated with the normality test
	fields <- list(
		list(name = "dep", type = "string", title = "", combine = TRUE),
		list(name = "lev", type = "string", title = ""),
		list(name = "W", title = "W", type = "number", format = "sf:4;dp:3"),
		list(name = "p", title = "p", type = "number", format = "dp:3;p:.001")
	)

	normalityTests[["schema"]] <- list(fields = fields)

	footnotes <- .newFootnotes()
	.addFootnote(footnotes, symbol = "<em>Note.</em>",
				 text = "Significant results suggest a deviation from normality.")

	## for a independent t-test, we need to check both group vectors for normality
	normalityTests.results <- list()

	variables <- options$variables
	factor <- options$groupingVariable
	levels <- levels(dataset[[.v(factor)]])

	if (length(variables) == 0) variables = "."
	if (length(levels) == 0) levels = c(".", ".")

	for (variable in variables) {
		count <- 0

		## there will be maximal two levels
		for (level in levels) {
			count <- count + 1

			## if everything looks fine, and we are ready to run
			if (perform == "run" && length(variables) > 0 && !is.null(levels)) {

				## get the dependent variable at a certain factor level
				data <- na.omit(dataset[[.v(variable)]][dataset[[.v(factor)]] == level])

				row.footnotes <- NULL
				error <- FALSE

				if (length(data) < 3) {
					err <- .generateErrorMessage(type='levene', observations.amount='< 3', variables=variable, grouping=factor)
					foot.index <- .addFootnote(footnotes, err)
					row.footnotes <- list(W = list(foot.index), p = list(foot.index))
					error <- TRUE

				} else if (length(data) > 5000) {
					err <- .generateErrorMessage(type='levene', observations.amount='> 5000', variables=variable, grouping=factor)
					foot.index <- .addFootnote(footnotes, err)
					row.footnotes <- list(W = list(foot.index), p = list(foot.index))
					error <- TRUE
				}

				## if the user did everything correctly :)
				if (!error) {
					r <- stats::shapiro.test(data)
					W <- .clean(as.numeric(r$statistic))
					p <- .clean(r$p.value)

					## if that's the first variable, add a new row
					newGroup <- level == levels[1]
					result <- list(dep = variable, lev = level,
								   W = W, p = p, .isNewGroup = newGroup)

				## if there was a problem, foonote it
				} else {

					newGroup <- level == levels[1]
					result <- list(dep = variable, lev = level,
								   W = "NaN", p = "NaN", .isNewGroup = newGroup,
								   .footnotes = row.footnotes)
				}

			## if we are not yet ready to perform
			## create an empty table for immediate feedback
			} else {

				newGroup <- count == 1
				result <- list(dep = variable, lev = level,
							   W = ".", p = ".", .isNewGroup = newGroup)
			}
			normalityTests.results[[length(normalityTests.results) + 1]] <- result
		}
	}

	normalityTests[["data"]] <- normalityTests.results
	normalityTests[["footnotes"]] <- as.list(footnotes)
	normalityTests
}


.independentSamplesTTestDescriptivesPlot <- function(dataset, options, perform) {

	descriptivesPlotList <- list()

	variables <- options$variables
	groups <- options$groupingVariable

	if (perform == "run" && length(variables) > 0 && groups != "") {

		base_breaks_x <- function(x) {
			b <- unique(as.numeric(x))
			d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
			list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
				yend = yend), inherit.aes = FALSE, size = 1))
		}

		base_breaks_y <- function(x) {
			ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
			b <- pretty(ci.pos)
			d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
			list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,xend = xend,
				 yend = yend), inherit.aes = FALSE, size = 1),
				 ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
		}

		for (var in .indices(variables)) {
			descriptivesPlot <- list("title" = variables[var])
			descriptivesPlot[["width"]] <- options$plotWidth
			descriptivesPlot[["height"]] <- options$plotHeight
			descriptivesPlot[["custom"]] <- list(width = "plotWidth", height = "plotHeight")

			summaryStat <- .summarySE(as.data.frame(dataset), measurevar = .v(options$variables[var]),
				groupvars = .v(options$groupingVariable), conf.interval = options$descriptivesPlotsConfidenceInterval,
				na.rm = TRUE, .drop = FALSE)

			colnames(summaryStat)[which(colnames(summaryStat) == .v(variables[var]))] <- "dependent"
			colnames(summaryStat)[which(colnames(summaryStat) == .v(groups))] <- "groupingVariable"

			pd <- ggplot2::position_dodge(0.2)

			p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
				y = dependent, group = 1)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,
				ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
				ggplot2::geom_line(position = pd, size = 0.7) + ggplot2::geom_point(position = pd,
				size = 4) + ggplot2::ylab(unlist(options$variables[var])) + ggplot2::xlab(options$groupingVariable) +
				ggplot2::theme_bw() +
				ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 18),
				  panel.grid.major = ggplot2::element_blank(), axis.title.x = ggplot2::element_text(size = 18,
					vjust = -0.2), axis.title.y = ggplot2::element_text(size = 18,
					vjust = -1), axis.text.x = ggplot2::element_text(size = 15),
				  axis.text.y = ggplot2::element_text(size = 15), panel.background = ggplot2::element_rect(fill = "transparent",
					colour = NA), plot.background = ggplot2::element_rect(fill = "transparent",
					colour = NA), legend.background = ggplot2::element_rect(fill = "transparent",
					colour = NA), panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
				  legend.key = ggplot2::element_blank(), legend.title = ggplot2::element_text(size = 12),
				  legend.text = ggplot2::element_text(size = 12), axis.ticks = ggplot2::element_line(size = 0.5),
				  axis.ticks.margin = grid::unit(1, "mm"), axis.ticks.length = grid::unit(3,
					"mm"), plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
				base_breaks_y(summaryStat) + base_breaks_x(summaryStat$groupingVariable)

			image <- .beginSaveImage(options$plotWidth, options$plotHeight)
			print(p)
			content <- .endSaveImage(image)

			descriptivesPlot[["data"]] <- content
			descriptivesPlotList[[var]] <- descriptivesPlot

		}

	} else {

		for (var in .indices(variables)) {
			descriptivesPlot <- list("title" = variables[var])
			descriptivesPlot[["width"]] <- options$plotWidth
			descriptivesPlot[["height"]] <- options$plotHeight
			descriptivesPlot[["custom"]] <- list(width = "plotWidth", height = "plotHeight")
			descriptivesPlot[["data"]] <- ""
			descriptivesPlotList[[var]] <- descriptivesPlot
		}
	}

	descriptivesPlotList
}
