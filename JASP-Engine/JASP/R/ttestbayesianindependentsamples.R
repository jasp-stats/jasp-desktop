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

TTestBayesianIndependentSamples <- function(jaspResults, dataset, options, state = NULL) {

  # initialize & error handling ----
	options <- .initAnalysisOptions(jaspResults, options)
	jaspResults$title <- if (options[["wilcoxTest"]]) "Bayesian Mann-Whitney U Test" else "Bayesian Independent Samples T-Test"
	dataset <- .ttestBayesianIndependentSamplesReadData(options, dataset)
	errors <- .getErrorsPerVariable(options = options, dataset = dataset)

	# Needs to include following function for reactive progress bar for MCMC sampling
	# env <- environment()
	# callBackWilcoxonMCMC <- function(results = NULL, progress = NULL) {
	#
	#   response <- callback(results, progress)
	#   if (response[["status"]] == "changed") {
	#
	#     optsForSampling <- c(
	#       "variables", "groupingVariable", "wilcoxTest", "wilcoxonSamplesNumber",
	#       "missingValues", "priorWidth"
	#     )
	#
	#     change <- .diff(env[["options"]], response[["options"]])
	#     env[["options"]] <- response[["options"]]
	#
	#     # if not any of the relevant options changed, status is ok
	#     if (!any(unlist(change[optsForSampling])))
	#       response[["status"]] <- "ok"
	#   }
	#   return(response)
	#
	# }
	# main analysis ----
	ttestResults <- .ttestBayesianIndependentSamplesTTest(jaspResults, dataset, options, errors, options[["stateKey"]][["ttestResults"]])
	jaspResults$send()

	# descriptives ----
	if (is.null(jaspResults[["Descriptives"]])) {
		print("remake descriptivesCollection")
	  descriptivesCollection <- createJaspContainer("Descriptives")
	  jaspResults[["Descriptives"]] <- descriptivesCollection
	  descriptivesCollection$dependOnOptions("groupingVariable")
	}	else {
		print("descriptivesCollection from state")
		descriptivesCollection <- jaspResults[["Descriptives"]]
	}

	.ttestBayesianIndependentSamplesDescriptives(descriptivesCollection, dataset, options)
	.makeDescriptivePlots(descriptivesCollection, options, dataset, errors, ttestResults)
	jaspResults$send()

	# inferential plots ----
	.initBayesFactorPackageOptions(options)

	if (is.null(jaspResults[["inferentialPlots"]])) {
		print("remake inferentialPlots")
		inferentialPlots <- createJaspContainer("Inferential Plots")
		jaspResults[["inferentialPlots"]] <- inferentialPlots
	  inferentialPlots$dependOnOptions("groupingVariable")
	}	else {
		print("inferentialPlots from state")
		inferentialPlots <- jaspResults[["inferentialPlots"]]
	}
	priorAndPosteriorPlots <- .makePriorAndPosteriorPlots(jaspResults, inferentialPlots, options, ttestResults, errors)
	robustnessPlots <-               .makeRobustnessPlots(jaspResults, inferentialPlots, options, dataset, ttestResults, errors)
	sequentialPlots <-               .makeSequentialPlots(jaspResults, inferentialPlots, options, dataset, ttestResults, errors)

	return()
}

# main table ----
.ttestBayesianIndependentSamplesTTest <- function(jaspResults, dataset, options, errors, dependencies) {

	# this function is the main workhorse, and also makes a table

	# check if we actually need to compute things
  if (!is.null(jaspResults[["ttestTable"]]) && !options[["anyNewVariables"]]) {
		print("NOTHING CHANGED IN ttestTable")
  	return(jaspResults[["stateTTestResults"]]$object)
  }
	print("remake ttestTable from scratch!")
	dependents <- options[["variables"]]
	ttestTable <- createJaspTable(title = "")
	jaspResults[["ttestTable"]] <- ttestTable
	ttestTable$dependOnOptions(c(dependencies, "bayesFactorType"))

	grouping   <- options$groupingVariable
	levels <- base::levels(dataset[[.v(grouping)]])
	g1 <- levels[1]
	g2 <- levels[2]
  # does all addcolumninfo etc.
	.ttestBayesianIndependentSamplesTTestMarkup(ttestTable, options, g1, g2)

	BFH1H0 <- !options$bayesFactorType == "BF01"

	nvar <- length(options[["variables"]])
	status <- rep("ok", nvar)
	BF10post <- numeric(nvar)
	tValue <- rep(NA, nvar)
	n_group2 <- rep(NA, nvar)
	n_group1 <- rep(NA, nvar)
	plottingError <- rep("error", nvar)
	errorFootnotes <- rep("no", nvar)
	delta <- list()

	oneSided <- options[["oneSided"]]
  null.interval <- switch(oneSided,
    "right" = c(0, Inf),
    "left"  = c(-Inf, 0),
    c(-Inf, Inf)
  )

  ttestState <- jaspResults[["stateTTestResults"]]$object # is there useable data?
  ttest.rows <- ttestState$ttest.rows
  print("ttest.rows")
  str(ttest.rows, max.level = 3)

	# can we actually do the analysis?
  options[["canDoAnalysis"]]
	canDoAnalysis <- options[["canDoAnalysis"]]

	# user provided no grouping variable or empty columns
	if (!canDoAnalysis) {

    dat <- data.frame(variable = dependents)
    ttestTable$setData(dat)

	} else { # we can do the analysis

		if (options$wilcoxTest) {

			# all variables - the ones we sampled before
			print(str(ttestState$delta))
			todo <- nvar
			if (!is.null(ttestState$delta))
				todo <- todo - sum(sapply(ttestState$delta, function(x) isTRUE(!is.null(x) && !is.na(x))))
			if (todo > 0)
				jaspResults$startProgressbar(expectedTicks = todo * options[["wilcoxonSamplesNumber"]],
																		 timeBetweenUpdatesInMs = 100)

		}

		for (var in dependents) {

			# BayesFactor package doesn't handle NAs, so it is necessary to exclude them

			subDataSet <- subset(dataset, select=c(.v(var), .v(grouping)))
			subDataSet <- na.omit(subDataSet)

			gs <- base::levels(levels)

			group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(var)]
			group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(var)]

			f <- as.formula(paste( .v(var), "~", .v(options$groupingVariable)))
			r.size <- options$priorWidth

			if (!is.null(ttest.rows[[var]])) {

				# row retrieved from state, only possible change is BF01 to BF10/ log(BF01)
				if (is.null(ttest.rows[[var]][[".footnotes"]])) {

					# if BFtype changed, change it. (BF01, BF10, log(BF01))
					ttest.rows[[var]][["BF"]] <-
						.recodeBFtype(bfOld = ttest.rows[[var]][["BF"]],
													newBFtype = options[["bayesFactorType"]],
													oldBFtype = ttest.rows[["options"]][["bayesFactorType"]]
						)
				}

				# TODO: is this still necessary?
				BF10post[var] <- ttest.rows[[var]]$BF # state$BF10post[index]
				tValue[var] <- ttest.rows$tValue[var]
				n_group2[var] <- ttest.rows$n_group2[var]
				n_group1[var] <- ttest.rows$n_group1[var]
				status[var] <- ttest.rows$status[var]
				plottingError[var] <- ttest.rows$plottingError[var]
				delta[[var]] <- ttest.rows$delta[[var]]

				row <- ttest.rows[[var]]

			} else { # compute row

				if (!isFALSE(errors)) {
					errorMessage <- errors$message
				} else {
					errorMessage <- NULL
				}

				n_group2[var] <- length(group2)
				n_group1[var] <- length(group1)

				if (!options$wilcoxTest) {

					r <- try (silent=FALSE, expr= {
						.generalTtestBF(x = group2, y = group1, paired = FALSE, oneSided = oneSided, options = options)
					})

					if (isTryError(r))
						r <- list(bf = NA, error = "", tValue = NA)

					bf.raw <- r[["bf"]]
					error <- .clean(r[["error"]])
					tValue[var] <- r[["tValue"]]
					delta[[var]] <- NA

				} else if (options$wilcoxTest) {

					# If the samples can be reused, don't call the Gibbs sampler again, but recalculate the
					# Bayes factor with new settings and take the samples from state.
					if (!is.null(ttest.rows$delta[[var]]) && !is.na(ttest.rows$delta[[var]])) {

						delta[[var]] <- ttest.rows$delta[[var]]
						bf.raw <- try(silent=FALSE, expr= {
							.computeBayesFactorWilcoxon(deltaSamples = delta[[var]], cauchyPriorParameter = options$priorWidth, oneSided = oneSided)
						})

					} else {

						r <- try(silent = FALSE, {
							.rankSumGibbsSampler(x = group2, y = group1, nSamples = options$wilcoxonSamplesNumber, nBurnin = 0,
																	 cauchyPriorParameter = options$priorWidth, jaspResults = jaspResults)
						})
						# if(is.null(r)) return() # Return null if settings are changed
						if (isTryError(r)) {
							delta[[var]] <- NULL
							bf.raw <- NA
						} else {
							delta[[var]] <- r[["deltaSamples"]]
							bf.raw <- .computeBayesFactorWilcoxon(deltaSamples = r[["deltaSamples"]], cauchyPriorParameter = options$priorWidth, oneSided = oneSided)
						}
					}

					wValue <- unname(wilcox.test(group2, group1, paired = FALSE)$statistic)
					error <- wValue
					tValue[var] <- median(delta[[var]])

				}

				bf.raw <- .recodeBFtype(bfOld = bf.raw,
																newBFtype = options$bayesFactorType,
																oldBFtype = "BF10"
				)

				BF10post[var] <- bf.raw
				BF <- .clean(bf.raw)

				if (is.na(bf.raw)) {
					status[var] <- "error"
					plottingError[var] <- "Plotting is not possible: Bayes factor could not be calculated"
				}

				if (is.infinite(bf.raw) || is.infinite(1/bf.raw)) {

					if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
						status[var] <- "error"
						if (is.infinite(bf.raw)) {
							plottingError[var] <- "Plotting is not possible: Bayes factor is infinite"
						} else {
							plottingError[var] <- "Plotting is not possible: The Bayes factor is too small"
						}
					}
				}

				if(!is.null(errorMessage)){

					BF <- .clean(NaN)
					error <- ""
					ttestTable$addFootnote(message(errorMessage, row_names = var))
					status[var] <- "error"
					errorFootnotes[var] <- errorMessage

				}
				row <- list(variable=var, BF=BF, error=error)
				ttest.rows[[var]] <- row
			}
			ttestTable$addRows(row)
		}
	}
	ttestTable$status <- "complete"

	ttestResults <- list(status = status, g1 = g1, g2 = g2, BFH1H0 = BFH1H0, plottingError = plottingError,
	       BF10post = BF10post, errorFootnotes = errorFootnotes, tValue = tValue, n_group2 = n_group2,
	       n_group1 = n_group1, delta = delta, ttestData = ttest.rows,
	       dependents = dependents, grouping = grouping)
	tmp <- createJaspState(
	  object = ttestResults,
	  title = "mainResultsObject"#,
	  #dependencies = stateKey[["ttestResults"]]
	)
	tmp$dependOnOptions(dependencies)
	jaspResults[["stateTTestResults"]] <- tmp#createJaspState(
	#   object = list(status = status, g1 = g1, g2 = g2, BFH1H0 = BFH1H0, plottingError = plottingError,
	#        BF10post = BF10post, errorFootnotes = errorFootnotes, tValue = tValue, n_group2 = n_group2,
	#        n_group1 = n_group1, delta = delta, ttestData = ttest.rows,
	#        dependents = dependents, grouping = grouping),
	#   title = "mainResultsObject",
	#   dependencies = stateKey[["ttestResults"]]
	# )
	return(ttestResults)
}

.ttestBayesianIndependentSamplesTTestMarkup <- function(jaspTable, options, g1, g2) {

	jaspTable$title <- ifelse(options$wilcoxTest, "Bayesian Mann-Whitney U Test", "Bayesian Independent Samples T-Test")
		if (options$effectSizeStandardized == "default" & !options$wilcoxTest) {
			citation <- list(
				"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
				"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237."
			)
		} else if (options$wilcoxTest) {
			citation <- list(
				"van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E. J. (2018). Bayesian Latent-Normal Inference for the Rank Sum Test, the Signed Rank Test, and Spearman's rho. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1703.01805"
			)
		} else if (options$effectSizeStandardized == "informative") {
			citation <- list(
				"Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479"
			)
		}
		# can jaspResults do this?
		# jaspTable$citation <- citation
		jaspTable$addColumnInfo(name = "variable", title="", type="string")

		if (options$bayesFactorType == "BF01") {

			if (options$hypothesis == "groupsNotEqual"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")
			} else if (options$hypothesis == "groupOneGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208A")
			} else if (options$hypothesis == "groupTwoGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208B")
			}

		} else if (options$bayesFactorType == "BF10") {

			if (options$hypothesis == "groupsNotEqual"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")
			} else if (options$hypothesis == "groupOneGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u208A\u2080")
			} else if (options$hypothesis == "groupTwoGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="BF\u208B\u2080")
			}

		} else if (options$bayesFactorType == "LogBF10") {

			if (options$hypothesis == "groupsNotEqual"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="Log(\u0042\u0046\u2081\u2080)")
			} else if (options$hypothesis == "groupOneGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="Log(\u0042\u0046\u208A\u2080)")
			} else if (options$hypothesis == "groupTwoGreater"){
				jaspTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title="Log(\u0042\u0046\u208B\u2080)")
			}

		}

		if (options$hypothesis == "groupsNotEqual" & !options$wilcoxTest) {
			jaspTable$addColumnInfo(name="error", type="number", format="sf:4;dp:3", title="error %")
		} else if(!options$wilcoxTest) {
			jaspTable$addColumnInfo(name="error", type="number", format="sf:4;dp:3;~", title="error %")
		} else if (options$wilcoxTest) {
			# display Wilcoxon statistic instead of error %
			jaspTable$addColumnInfo(name="error", type="number", format="sf:4;dp:3;", title="W")
		}

		message <- NULL
		if (options$hypothesis == "groupOneGreater") {
			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", g1, "</em> is greater than group <em>", g2, "</em>.", sep="")
			# .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		} else if (options$hypothesis == "groupTwoGreater") {
			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", g1, "</em> is less than group <em>", g2, "</em>.", sep="")
			# .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		}
		if (!is.null(message))
			jaspTable$addFootnote(message)
}

# Descriptives ----
.ttestBayesianIndependentSamplesDescriptives <- function(jaspCollection, dataset, options) {
  # this function makes the table for descriptives
	if (!options$descriptives) return()

  if (!is.null(jaspCollection[["descriptivesTable"]]) && !options[["anyNewVariables"]]) {
    print("NOTHING CHANGED IN DESCRIPTIVESTABLE")
    return(NULL)
  }
  print("SOMETHING CHANGED IN DESCRIPTIVESTABLE")

	descriptives <- createJaspTable(title = "Group Descriptives")
	jaspCollection[["descriptivesTable"]] <- descriptives
	dependencies <- options[["stateKey"]][["descriptives"]]
	descriptives$dependOnOptions(c(dependencies, "variables", "groupingVariable", "descriptives", "descriptivesPlots"))

	descriptives$addColumnInfo(name = "variable", title = "",      type = "string", combine = TRUE)
	descriptives$addColumnInfo(name = "group",    title = "Group", type = "string")
	descriptives$addColumnInfo(name = "N",        title = "N",     type = "number")
	descriptives$addColumnInfo(name = "mean",     title = "Mean",  type = "number", format = "sf:4;dp:3")
	descriptives$addColumnInfo(name = "sd",       title = "SD",    type = "number", format = "sf:4;dp:3")
	descriptives$addColumnInfo(name = "se",       title = "SE",    type = "number", format = "sf:4;dp:3")

	## add credible interval values if asked for in plot
	hasCRI <- options$descriptivesPlots
	if (hasCRI) {
		interval <- 100 * options$descriptivesPlotsCredibleInterval
		title <- paste0(interval, "% Credible Interval")
		descriptives$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = "Lower", overtitle = title)
		descriptives$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = "Upper", overtitle = title)
	}

	stateDescriptivesTable <- jaspCollection[["stateDescriptivesTable"]]$object

	dependents <- unlist(options$variables)
	grouping   <- options$groupingVariable
	nvar <- length(dependents)
	if (nvar == 0) dependents <- "."

	if (!options[["canDoAnalysis"]]) {

		tmp <- rep(dependents, each = 2)
		tmp[seq(2, length(tmp), 2)] <- ""
		dat <- data.frame(variable = tmp)
		descriptives$setData(dat)

	} else {
		levels <- base::levels(dataset[[ .v(grouping) ]])
		groupingData <- dataset[[.v(grouping)]]
		for (var in dependents) {
			if (is.null(stateDescriptivesTable[[var]])) {
				for (i in 1:2) {

					level <- levels[i]
					variableData <- dataset[[.v(var)]]

					groupData <- variableData[groupingData == level]
					groupDataOm <- na.omit(groupData)

					if (class(groupDataOm) != "factor") {

						posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
						ciLower <- .clean(posteriorSummary$ciLower)
						ciUpper <- .clean(posteriorSummary$ciUpper)

						n <- .clean(length(groupDataOm))
						mean <- .clean(mean(groupDataOm))
						std <- .clean(sd(groupDataOm))
						sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

						row <- list(variable = var, group = level,
												N = n, mean = mean, sd = std, se = sem)
						if (hasCRI)
							row[c("lowerCI", "upperCI")] <- list(ciLower, ciUpper)

					} else {

						n <- .clean(length(groupDataOm))
						row <- list(variable = var, group = "", N = n,
												mean = "", sd = "", se = "")
						if (hasCRI)
							row[c("lowerCI", "upperCI")] <- list("", "")
					}

					descriptives$addRows(row)
					stateDescriptivesTable[[var]][[i]] <- row

				}
			} else {
				for (i in 1:2) {
					descriptives$addRows(stateDescriptivesTable[[var]][[i]])
				}
			}
		}
	}
	descriptives$status <- "complete"

	tmp <- createJaspState(object = stateDescriptivesTable, title = "stateDescriptivesTable")
	tmp$dependOnOptions(dependencies)
	jaspCollection[["stateDescriptivesTable"]] <- tmp

	return()

}

.makeDescriptivePlots <- function(jaspCollection, options, dataset, errors, ttestResults) {

  if (!(options[["canDoAnalysis"]] && options[["descriptivesPlots"]])) return()

  if (!is.null(jaspCollection[["DescriptivePlots"]])) {
  	if (options[["anyNewVariables"]]) {
  		print("partial changes in DescriptivePlots")
  		descriptivesPlotCollection <- jaspCollection[["DescriptivePlots"]]
  		dependents <- options[["newVariables"]]
  	} else {
  		print("Nothing changed in DescriptivePlots")
  		return()
  	}
  } else {
    print("remake DescriptivePlots completeley")
  	descriptivesPlotCollection <- createJaspContainer("descriptivesPlots")
	  jaspCollection[["DescriptivePlots"]] <- descriptivesPlotCollection
	  dependents <- unlist(options$variables)
  }

	dependencies <- options[["stateKey"]][["descriptives"]]
	descriptivesPlotCollection$dependOnOptions(c(dependencies, "descriptivesPlots"))

	grouping   <- options$groupingVariable
	g1 <- ttestResults[["g1"]]
	g2 <- ttestResults[["g2"]]
	idxG1 <- dataset[[.v(grouping)]] == g1
	idxG2 <- dataset[[.v(grouping)]] == g2

	for (var in dependents) {

		# if (is.null(descriptivesPlotCollection[[var]])) {
			if (isFALSE(errors[[var]])) {

				idxC <- !is.na(dataset[[.v(var)]])
				group1 <- na.omit(dataset[idxG1 & idxC, .v(var)])
				group2 <- na.omit(dataset[idxG2 & idxC, .v(var)])

				obj <- try({.plot2GroupMeansBayesIndTtest(
					v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2,
					groupingName = grouping, dependentName = var,
					descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
				})
				plot <- .addPlotToJaspObj(var, errors, obj, dependencies)
			} else {
				plot <- .addPlotToJaspObj(var, errors, NULL, dependencies)
			}
			descriptivesPlotCollection[[var]] <- plot
		}
	# }
	return()
}

# Inferential Plots ----
.makePriorAndPosteriorPlots <- function(jaspResults, jaspCollection, options, ttestResults, errors, dependencies) {

	if (!(options[["canDoAnalysis"]] && options[["plotPriorAndPosterior"]])) return()

	 if (!is.null(jaspCollection[["priorAndPosterior"]])) {
  	if (options[["anyNewVariables"]]) {
  		print("partial changes in plotPriorAndPosterior")
  		dependents <- options[["newVariables"]]
  		PriorAndPosteriorPlotCollection <- jaspCollection[["priorAndPosterior"]]
  	} else {
  		print("Nothing changed in plotPriorAndPosterior")
  		return()
  	}
  } else {
    print("remake plotPriorAndPosterior completely")
		PriorAndPosteriorPlotCollection <- createJaspContainer("Prior and Posterior Plots")
		jaspCollection[["priorAndPosterior"]] <- PriorAndPosteriorPlotCollection
		dependents <- unlist(options$variables)
  }

	dependencies <- options[["stateKey"]][["priorAndPosteriorPlots"]]
	PriorAndPosteriorPlotCollection$dependOnOptions(c(dependencies, "plotPriorAndPosterior"))
	grouping   <- options$groupingVariable

	tValue             <- ttestResults[["tValue"]]
	n_group1           <- ttestResults[["n_group1"]]
	n_group2           <- ttestResults[["n_group2"]]
	BF10post           <- ttestResults[["BF10post"]]
	BFH1H0             <- ttestResults[["BFH1H0"]]
	deltaSamplesWilcox <- ttestResults[["delta"]]

	for (var in dependents) {

		if (is.null(PriorAndPosteriorPlotCollection[[var]])) {
			if (isFALSE(errors[[var]])) {

				obj <- function() {.plotPosterior.summarystats.ttest(
					t = tValue[var], n1 = n_group2[var],          n2 = n_group1[var],
					paired = FALSE, 			oneSided = options[["oneSided"]], BF = BF10post[var],
					BFH1H0 = BFH1H0, 			rscale = options$priorWidth,
					options = options, 		delta = deltaSamplesWilcox[[var]],
					addInformation = options$plotPriorAndPosteriorAdditionalInfo
				)}

				plot <- .addPlotToJaspObj(var, errors, obj, dependencies)
			} else {
				plot <- .addPlotToJaspObj(var, errors, NULL, dependencies)
			}
			PriorAndPosteriorPlotCollection[[var]] <- plot
			jaspResults$send()
		}
	}
	return()
}

.makeRobustnessPlots <- function(jaspResults, jaspCollection, options, dataset, ttestResults, errors) {

	if (!(options[["canDoAnalysis"]] && options[["plotBayesFactorRobustness"]])) return()

	if (!is.null(jaspCollection[["robustness"]])) {
		if (options[["anyNewVariables"]]) {
			print("partial changes in robustness")
			dependents <- options[["newVariables"]]
			robustnessPlotCollection <- jaspCollection[["robustness"]]
		} else {
			print("Nothing changed in robustness")
			return()
		}
	} else {
		print("remake robustness completely")
		robustnessPlotCollection <- createJaspContainer("Robustness Plots")
		jaspCollection[["robustness"]] <- robustnessPlotCollection
		dependents <- unlist(options$variables)
	}

	dependencies <- options[["stateKey"]][["robustnessPlots"]]
	robustnessPlotCollection$dependOnOptions(c(dependencies, "plotBayesFactorRobustness"))
	grouping   <- options$groupingVariable

	BF10post <- ttestResults[["BF10post"]]
	if(options$bayesFactorType=="LogBF10")
		BF10post <- exp(BF10post)

	BFH1H0   <- ttestResults[["BFH1H0"]]
	g1       <- ttestResults[["g1"]]
	g2       <- ttestResults[["g2"]]

	idxG1 <- dataset[[.v(grouping)]] == g1
	idxG2 <- dataset[[.v(grouping)]] == g2

	for (var in dependents) {

		# if (is.null(stateRobustnessPlots[[var]])) {
			if (options$effectSizeStandardized == "informative") {
				plot <- createJaspPlot(title = var, width = 480, height = 320, error = "badData",
															 errorMessage = "Bayes factor robustness check plot currently not supported for informed prior.")
			} else if (isFALSE(errors[[var]])) {

				# plot <- .addPlotToJaspObj(var, errors, "empty", dependencies)
				# robustnessPlotCollection[[var]] <- plot
				# plot$status <- "running"

				idxC <- !is.na(dataset[[.v(var)]])
				group1 <- na.omit(dataset[idxG1 & idxC, .v(var)])
				group2 <- na.omit(dataset[idxG2 & idxC, .v(var)])

				obj <- function() {.plotBF.robustnessCheck.ttest(
					x = group2, y = group1, BF10post = BF10post[var],
					paired = FALSE, oneSided = options[["oneSided"]], rscale = options$priorWidth, BFH1H0 = BFH1H0,
					additionalInformation = options$plotBayesFactorRobustnessAdditionalInfo)
				}
				plot <- .addPlotToJaspObj(var, errors, obj, dependencies)
			} else {
				plot <- .addPlotToJaspObj(var, errors, NULL, dependencies)
			}
			robustnessPlotCollection[[var]] <- plot
		# }
		jaspResults$send()
	}
	return()
}

.makeSequentialPlots <- function(jaspResults, jaspCollection, options, dataset, ttestResults, errors, dependencies) {

	if (!(options[["canDoAnalysis"]] && options[["plotSequentialAnalysis"]])) return()

	if (!is.null(jaspCollection[["sequential"]])) {
		if (options[["anyNewVariables"]]) {
			print("partial changes in sequentialPlots")
			dependents <- options[["newVariables"]]
			sequentialPlotCollection <- jaspCollection[["sequential"]]
		} else {
			print("Nothing changed in sequentialPlots")
			return()
		}
	} else {
		print("remake sequentialPlots completely")
		sequentialPlotCollection <- createJaspContainer("Sequential Plots")
		jaspCollection[["sequential"]] <- sequentialPlotCollection
		dependents <- unlist(options$variables)
	}

	dependencies <- options[["stateKey"]][["sequentialPlots"]]
	sequentialPlotCollection$dependOnOptions(c(dependencies, "plotSequentialAnalysis"))
	grouping   <- options$groupingVariable

	BF10post <- ttestResults[["BF10post"]]
	BFH1H0   <- ttestResults[["BFH1H0"]]
	g1 <- ttestResults[["g1"]]
	g2 <- ttestResults[["g2"]]
	idxG1 <- dataset[[.v(grouping)]] == g1
	idxG2 <- dataset[[.v(grouping)]] == g2

	for (var in dependents) {

		# if (is.null(stateSequentialPlots[[var]])) {

			if (options$effectSizeStandardized == "informative") {
				plot <- createJaspPlot(title = var, width = 480, height = 320, error = "badData",
															 errorMessage = "Sequential analysis robustness check plot currently not supported for informed prior.")
			} else if (isFALSE(errors[[var]])) {

				idxC <- !is.na(dataset[[.v(var)]])
				group1 <- na.omit(dataset[idxG1 & idxC, .v(var)])
				group2 <- na.omit(dataset[idxG2 & idxC, .v(var)])
				subDataSet <- na.omit(dataset[, c(.v(var), .v(grouping))])

				obj <- function() {.plotSequentialBF.ttest(
					x = group2, y = group1, paired = FALSE, oneSided = options[["oneSided"]],
					rscale = options$priorWidth, BFH1H0 = BFH1H0, BF10post = BF10post[var],
					plotDifferentPriors = options$plotSequentialAnalysisRobustness,
					subDataSet = subDataSet, level1 = g1, level2 = g2, options = options) # <- why is OPTIONS here???
				}

				plot <- .addPlotToJaspObj(var, errors, obj, dependencies)
			} else {
				plot <- .addPlotToJaspObj(var, errors, NULL, dependencies)
			}
			sequentialPlotCollection[[var]] <- plot
		# }
		jaspResults$send()
	}
	return()
}

# Wilcoxon functions ----
.rankSumGibbsSampler <- function(xVals, yVals, nSamples = 1e3, cauchyPriorParameter = 1/sqrt(2),
                                 nBurnin = 0, nGibbsIterations = 10, jaspResults){
  n1 <- length(xVals)
  n2 <- length(yVals)
  allRanks <- rank(c(xVals,yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1+1):(n1+n2)]

  allVals <- sort(rnorm((n1+n2)))[allRanks] # initial values
  nSamples <- nSamples + nBurnin
  deltaSamples <- gSamples <- muSamples <- numeric(nSamples)

  oldMuProp <- oldDeltaProp <- 0

  for (j in 1:nSamples) {

    # if (j %% 1e2 == 0 ) {
    #   response <- progressbar()
    #   if (response[["status"]] != "ok")
    #     return()
    # }

    for (i in sample(1:(n1+n2))) {
      underx <- allVals[allRanks < allRanks[i]][order(allVals[allRanks < allRanks[i]], decreasing = T)][1]
      upperx <- allVals[allRanks > allRanks[i]][order(allVals[allRanks > allRanks[i]], decreasing = F)][1]
      if (is.na(underx)) {underx <- -Inf}
      if (is.na(upperx)) {upperx <- Inf}

      if (i <= n1) {
        allVals[i] <- .truncNormSample(mu = (-0.5*oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      } else if (i > n1) {
        allVals[i] <- .truncNormSample(mu = (0.5*oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      }
    }

    xVals <- allVals[1:n1]
    yVals <- allVals[(n1+1):(n1+n2)]

    gibbsResult <- .sampleGibbsTwoSampleWilcoxon(x = xVals, y = yVals, n1 = n1, n2 = n2, nIter = nGibbsIterations,
                                                 rscale = cauchyPriorParameter)

    muSamples[j] <- oldMuProp <- gibbsResult[3]
    deltaSamples[j] <- oldDeltaProp <- gibbsResult[1]
    jaspResults$progressbarTick()
  }

  deltaSamples <- -1 * deltaSamples[-(1:nBurnin)]

  return(list(deltaSamples = deltaSamples))
}

.sampleGibbsTwoSampleWilcoxon <- function(x, y, n1, n2, nIter = 10, rscale = 1/sqrt(2)) {
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  for(i in 1:nIter){
    #sample mu
    varMu <- (4 * g * sigmaSq) / ( 4 + g * (n1 + n2) )
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu))
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2*sigmaSq)
    g <- 1/rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  return(c(delta, sigmaSq, mu, g))
}

.truncNormSample <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1) {

  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)

  return(mySample)
}

.computeBayesFactorWilcoxon <- function(deltaSamples, cauchyPriorParameter, oneSided) {
  postDens <- logspline::logspline(deltaSamples)
  densZeroPoint <- logspline::dlogspline(0, postDens)
  priorDensZeroPoint <- dcauchy(0, scale = cauchyPriorParameter)

  corFactorPosterior <- ifelse(oneSided == "right" , 1 - logspline::plogspline(0, postDens), logspline::plogspline(0, postDens) )
  corFactorPrior <-  pcauchy(0, scale = cauchyPriorParameter, lower.tail = (oneSided != "right" ))

  bf <- ifelse(oneSided == FALSE,  priorDensZeroPoint / densZeroPoint,
               (priorDensZeroPoint / corFactorPrior) / (densZeroPoint / corFactorPosterior))
  return(bf)
}

# plot helper functions ----
.plot2GroupMeansBayesIndTtest <- function(v1=NULL, v2=NULL, nameV1=NULL, nameV2=NULL, groupingName=NULL, dependentName=NULL, descriptivesPlotsCredibleInterval=.95) {

	v1 <- na.omit(v1)
	v2 <- na.omit(v2)

	posteriorSummary1 <- .posteriorSummaryGroupMean(variable=v1, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	posteriorSummary2 <- .posteriorSummaryGroupMean(variable=v2, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	summaryStat <- data.frame(	groupingVariable=c(nameV1, nameV2), dependent=c(posteriorSummary1$median, posteriorSummary2$median),
								ciLower=c(posteriorSummary1$ciLower, posteriorSummary2$ciLower), ciUpper=c(posteriorSummary1$ciUpper,
								posteriorSummary2$ciUpper),
								group = 1)

	pd <- ggplot2::position_dodge(.2)

	p <-	ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=group)) +
			ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
			ggplot2::geom_line(position=pd, size = .7) +
			ggplot2::geom_point(position=pd, size=4) +
			ggplot2::ylab(dependentName) +
			ggplot2::xlab(groupingName) +
			.base_breaks_y3(summaryStat) +
			.base_breaks_x(summaryStat$groupingVariable) 
			
	p <- JASPgraphs::themeJasp(p)

	return(p)
}

.base_breaks_x <- function(x) {

	b <- unique(as.numeric(x))
	d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
	list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))

}

.base_breaks_y3 <- function(x) {

	ci.pos <- c(x$ciLower, x$ciUpper)
	b <- pretty(ci.pos)
	d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
	list(	ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
			ggplot2::scale_y_continuous(breaks=c(min(b),max(b)))	)
}

# helper functions ----
.getErrorsPerVariable <- function(options, dataset) {

	# does all error handling
	#
	# return:
	#
	# - if no analysis can be done (bad data) returns NULL
	# - if an analysis breaking error occurs, calls stop
	# - otherwise returns a named list where each name is an entry of dependents:
	#   - element is FALSE if no errors are found
	#   - element is a list witha message if errors are found

	if (options[["canDoAnalysis"]]) {

		dependents <- unlist(options$variables)
		grouping   <- options$groupingVariable
		errors <- list()

		# analysis breaking errors
		.hasErrors(dataset, "run", type = c('factorLevels', 'variance'),
							 factorLevels.target = grouping, factorLevels.amount = '!= 2',
							 variance.target = dependents,
							 variance.grouping = grouping,
							 exitAnalysisIfErrors = TRUE)

		for (var in dependents) {

			errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
																	type = c('infinity','observations','variance'),
																	all.target = var, observations.amount = "< 2", all.grouping = grouping)
		}

		return(errors)
	} else {
		return(NULL)
	}
}

.addPlotToJaspObj <- function(var, errors, obj = NULL, dependencies = NULL, w = 480, h = 320) {

	# convenience function
	if (is.null(obj)) {
		plot <- createJaspPlot(title = var, width = w, height = h,
													 error = "badData", errorMessage = errors[[var]][["message"]])
	} else if (identical(obj, "empty")) {
		plot <- createJaspPlot(title = var, width = w, height = h)
	} else if (isTryError(obj)) {
		plot <- createJaspPlot(title = var, width = w, height = h,
													 error = "badData", errorMessage = .extractErrorMessage(obj))
	} else {
		plot <- createJaspPlot(title = var, width = w, height = h, plot = obj)
	}
	plot$dependOnOptions(dependencies)
	plot$setOptionMustContainDependency("variables",  var)
	return(plot)
}

.initBayesFactorPackageOptions <- function(options) {

  # note that options$... / options[[...]] and options(...) are two COMPLETELY DIFFERENT things!!
  # also, options(...) is, by R's default, global so this works

  if (options[["canDoAnalysis"]] && (options[["plotPriorAndPosterior"]] || options[["plotBayesFactorRobustness"]] || options[["plotSequentialAnalysis"]])) {
    if(is.null(options("BFMaxModels"))) options(BFMaxModels = 50000)
    if(is.null(options("BFpretestIterations"))) options(BFpretestIterations = 100)
    if(is.null(options("BFapproxOptimizer"))) options(BFapproxOptimizer = "optim")
    if(is.null(options("BFapproxLimits"))) options(BFapproxLimits = c(-15,15))
    if(is.null(options("BFprogress"))) options(BFprogress = interactive())
    if(is.null(options("BFfactorsMax"))) options(BFfactorsMax = 5)
  }
}

.initAnalysisOptions <- function(jaspResults, options) {

	# initialize options: takes user input and determines:
	#
	# - can any analysis be done?
	# - which variables will need to be computed for plots?
	#
	# also defines the dependencies for all objects

	dependents <- unlist(options$variables)
	if (!is.null(jaspResults[["stateVariables"]])) {
		oldDependents <- jaspResults[["stateVariables"]]$object
	} else {
		oldDependents <- NULL
	}

	if (!is.null(dependents)) {
		tmp <- createJaspState(object = dependents, title = "dependents")
		tmp$dependOnOptions("groupingVariable")
		jaspResults[["stateVariables"]] <- tmp
	}
	options[["anyNewVariables"]] <- any(!dependents %in% oldDependents)
	options[["newVariables"]] <- dependents[!dependents %in% oldDependents]
	print(options[["anyNewVariables"]])
	print(options[["newVariables"]])

	# options is updated to avoid computing these in multiple functions
	options[["wilcoxTest"]] <- options$testStatistic ==  "Wilcoxon"
  options[["canDoAnalysis"]] <- length(dependents) > 0 && options$groupingVariable != ""
  options[["oneSided"]] <- switch(options[["hypothesis"]],
    "groupOneGreater" = "right",
    "groupTwoGreater" = "left",
    FALSE
  )
  AtTheEndResetPlotRobustnessSequential <- NULL
  if (options[["wilcoxTest"]]) {
    # when a user requests robustness/ sequential plots first and then selects wilcoxTest
    # jasp will still provide these as TRUE, but they shouldn't be.
    AtTheEndResetPlotRobustnessSequential <- options[c("plotBayesFactorRobustness", "plotSequentialAnalysis")]
    options[["plotBayesFactorRobustness"]] <- FALSE
    options[["plotSequentialAnalysis"]] <- FALSE
  }

  # dependencies specified here to pass as depends
  defaults <- c("priorWidth", "hypothesis", "groupingVariable", "missingValues",
                "effectSizeStandardized", "informativeStandardizedEffectSize",
                "informativeCauchyLocation", "informativeCauchyScale", "informativeTLocation",
                "informativeTScale", "informativeTDf", "informativeNormalMean",
                "informativeNormalStd", "testStatistic", "wilcoxonSamplesNumber")
  options[["stateKey"]] <- list(
    ttestResults = defaults,
    descriptives = c("groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval"),
    priorAndPosteriorPlots = c(defaults, "plotHeight", "plotWidth", "plotPriorAndPosteriorAdditionalInfo"),
    robustnessPlots = c(defaults, "plotHeight", "plotWidth", "plotBayesFactorRobustnessAdditionalInfo"),
    sequentialPlots = c(defaults, "plotHeight", "plotWidth", "plotSequentialAnalysisRobustness"),
    delta = c("priorWidth", "groupingVariable", "missingValues", "wilcoxonSamplesNumber"),
    descriptivesData = c("descriptives", "groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval")
  )

  return(options)

}

.ttestBayesianIndependentSamplesReadData <- function(options, dataset = NULL) {

	dependents <- unlist(options$variables)
  grouping   <- options$groupingVariable
  if (grouping == "")
    grouping <- NULL

  if (is.null(dataset)) {
    excl <- grouping
    if (options$missingValues == "excludeListwise") {
      excl <- c(excl, dependents)
    }
    dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=excl)
  }
  return(dataset)
}