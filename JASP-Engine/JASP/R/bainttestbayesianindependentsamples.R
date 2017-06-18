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

BainTTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)

	grouping   <- options$groupingVariable

	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {

			if (options$missingValues == "excludeListwise") {

				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=c(dependents, grouping))

			} else {

				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=grouping)
			}

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=dependents, columns.as.factor=grouping)
		}
	}

	.hasErrors(dataset=dataset, perform=perform, type="factorLevels",
	           factorLevels.target=grouping, factorLevels.amount = "!= 2",
	           exitAnalysisIfErrors = TRUE)

	results <- list()

	meta <- list()

	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="table")
	meta[[3]] <- list(name="BFplots", type="collection", meta="image")
	meta[[4]] <- list(name = 'descriptivesPlots', type = "collection", meta = "image")

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Informative Hypothesis T-Test"


	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state)) {

		diff <- .diff(options, state$options)

	}

	ttest.results <- .BainttestBayesianIndependentSamplesTTest(dataset, options, perform, state=state, diff=diff)

	results[["ttest"]] <- ttest.results[[1]]
	status <- ttest.results[[2]]
	g1 <- ttest.results[[3]]
	g2 <- ttest.results[[4]]
	BFH1H0 <- ttest.results[[5]]
	plottingError <- ttest.results[[6]]
	BF10post <- ttest.results[[7]]
	errorFootnotes <- ttest.results[[8]]
	plotres <- ttest.results[[9]]
	Bainresult <- ttest.results[[10]]
	plotVariables <- ttest.results[[11]]


	if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
	if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
	if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
	if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
	if(is.null(options()$BFprogress)) options(BFprogress = interactive())
	if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)

	descriptivesTable <- .BainttestBayesianIndependentSamplesDescriptives(dataset, options, perform)
	results[["descriptives"]] <- descriptivesTable


	plotGroups <- list()
	#plots.ttest <- list()
	descriptPlotVariables <- list()
	descriptivesPlots <- list()
	plotTypes <- list()
	plotVariables <- list()
	BFplots <- list()
	BFplotvariables <- list()


	if (options$descriptivesPlots | options$plotPriorAndPosterior) {

		iint <- 1
		q <- 1
		descriptInd <- 1
		BFind <- 1

		for (variable in options[["variables"]]){

			plotGroups[[iint]] <- list()
			plotGroups[[iint]][["title"]] <- variable
			plotGroups[[iint]][["name"]] <- variable

			if (options$descriptivesPlots) {

				if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
					diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {


					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it

					index <- which(state$descriptPlotVariables == variable)

					descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


				} else {

					descriptivesPlot <- list()

					descriptivesPlot[["title"]] <- variable
					descriptivesPlot[["width"]] <- options$plotWidth
					descriptivesPlot[["height"]] <- options$plotHeight
					descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
					descriptivesPlot[["status"]] <- "waiting"
					descriptivesPlot[["data"]] <- ""

					descriptivesPlots[[descriptInd]] <- descriptivesPlot
				}


				descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable

				descriptInd <- descriptInd + 1
			}

			if (options$plotPriorAndPosterior){

			    if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE && diff$hypothesis == FALSE &&
			                                                                                                                                                     diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {


			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			        # then, if the requested plot already exists, use it

			        index <- which(state$BFplotvariables == variable)

			        BFplots[[BFind]] <- state$BFplots[[index]]


			    } else {

			        BFplot <- list()

			        BFplot[["title"]] <- variable
			        BFplot[["width"]] <- options$plotWidth
			        BFplot[["height"]] <- options$plotHeight
			        BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")
			        BFplot[["status"]] <- "waiting"
			        BFplot[["data"]] <- ""

			        BFplots[[BFind]] <- BFplot
			    }


			    BFplotvariables[[length(BFplotvariables)+1]] <- variable

			    BFind <- BFind + 1

			}

			iint <- iint + 1

		}


		if (options$plotPriorAndPosterior)
			results[["BFplots"]] <- list(title=ifelse(length(options[["variables"]]) > 1,
				"Bayes Factors plots", "Bayes Factor plot"), collection=BFplots)

		if (options$descriptivesPlots)
			results[["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)


		if (perform == "run" && length(options$variables) > 0 && !is.null(grouping)) {

			if ( ! .shouldContinue(callback(results)))
				return()

			statusInd <- 1
			i <- 1
			z <- 1
			descriptInd <- 1
			BFind <- 1


			for (variable in options[["variables"]]) {


				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)

				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]


				if (options$descriptivesPlots) {


					if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
						diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {

						# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it

						index <- which(state$descriptPlotVariables == variable)

						descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


					} else {

						if ( ! .shouldContinue(callback(results))){
							return()
						}

						plot <- descriptivesPlots[[descriptInd]]

						if(Bainresult[[i]] == "error"){

						    plot[["data"]] <- ""
						    plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])

						} else {

						p <- try(silent= FALSE, expr= {

						    figure <- .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, groupingName = options$groupingVariable, dependentName = variable, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)

						    content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = figure, obj = TRUE)
						    plot[["convertible"]] <- TRUE
						    plot[["obj"]] <- content[["obj"]]
						    plot[["data"]] <- content[["png"]]

								# image <- .beginSaveImage(options$plotWidth, options$plotHeight)
								# .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, groupingName = options$groupingVariable, dependentName = variable, descriptivesPlotsCredibleInterval=
								# options$descriptivesPlotsCredibleInterval)
								# plot[["data"]] <- .endSaveImage(image)
							})

						}

						# if (class(p) == "try-error") {
						#
						# 	errorMessageTmp <- .extractErrorMessage(p)
						# 	errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
						# 	plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
						# }

						plot[["status"]] <- "complete"

						descriptivesPlots[[descriptInd]] <- plot

					}

					results[["descriptivesPlots"]][["collection"]] <- descriptivesPlots

					descriptInd <- descriptInd + 1

					if ( ! .shouldContinue(callback(results)))
						return()

				}

				if (options$plotPriorAndPosterior) {

				    if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE && diff$hypothesis == FALSE &&
				                                                                                                                                                     diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {

				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				        # then, if the requested plot already exists, use it

				        index <- which(state$BFplotvariables == variable)

				        BFplots[[BFind]] <- state$BFplots[[index]]


				    } else {

				        if ( ! .shouldContinue(callback(results)))
				            return()

				        plot <- BFplots[[BFind]]

				        # if (class(p) == "try-error") {
				        #
				        # 	errorMessageTmp <- .extractErrorMessage(p)
				        # 	errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
				        # 	plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
				        # }

				        if(Bainresult[[i]] == "error"){

				            plot[["data"]] <- ""
				            plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])

				        } else {

				        p <- try(silent= FALSE, expr= {

				            .plotFuncBF <- function() {
				                Bain::plot.BainT(plotres[[BFind]])
				            }
				            content2 <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFuncBF, obj = TRUE)
				            plot[["convertible"]] <- TRUE
				            plot[["obj"]] <- content2[["obj"]]
				            plot[["data"]] <- content2[["png"]]

				        })

				        }

				        plot[["status"]] <- "complete"

				        BFplots[[BFind]] <- plot
				    }

				    results[["BFplots"]][["collection"]] <- BFplots

				    BFind <- BFind + 1

				    if ( ! .shouldContinue(callback(results)))
				        return()

				}

				statusInd <- statusInd + 1
				i <- i + 1
			}
		}
	}


	keep <- NULL

	# for (plot in plots.ttest)
	# 	keep <- c(keep, plot$data)

	for(plot in BFplots)
	    keep <- c(keep, plot$data)

	for (plot in descriptivesPlots)
	    keep <- c(keep, plot$data)

	if (perform == "init") {

		return(list(results=results, status="inited", state=state, keep=keep))

	} else {

		return(list(results=results, status="complete", state=list(options=options, results=results, plotTypes=plotTypes, plotVariables=plotVariables,
		descriptPlotVariables=descriptPlotVariables, descriptivesPlots=descriptivesPlots, status=status, plottingError=plottingError, BF10post=BF10post, errorFootnotes=errorFootnotes,
		BFplotvariables = BFplotvariables, BFplots = BFplots, plotres = plotres, Bainresult = Bainresult),
		keep=keep))
	}

}


.BainttestBayesianIndependentSamplesTTest <- function(dataset, options, perform, state, diff) {

    plotres <- list()
    plotVariables <- list()

	g1 <- NULL
	g2 <- NULL

	ttest <- list()

	ttest[["title"]] <- "Bayesian Informative Hypothesis Independent Samples T-Test"

	ttest[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	bf.type <- options$bayesFactorType

	if (bf.type == "BF10") {

	    BFH1H0 <- TRUE

	    if (options$hypothesis == "groupsNotEqual") {
	        bf.title <- "BF\u2081\u2080"
	    }
	    if (options$hypothesis == "groupTwoGreater") {
	        bf.title <- "BF\u208A\u2080"
	    }
	    if (options$hypothesis == "groupOneGreater") {
	        bf.title <- "BF\u208B\u2080"
	    }
	    if(options$hypothesis == "allTypes"){
	        bf.title <- "BF\u2081\u2080" # same as not equal to
	    }

	} else if (bf.type == "LogBF10") {

	    BFH1H0 <- TRUE

	    if (options$hypothesis == "groupsNotEqual") {
	        bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
	    }
	    if (options$hypothesis == "groupTwoGreater") {
	        bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
	    }
	    if (options$hypothesis == "groupOneGreater") {
	        bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
	    }
	    if(options$hypothesis == "allTypes"){
	        bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)" # same as not equal to
	    }

	} else if (bf.type == "BF01") {

	    BFH1H0 <- FALSE

	    if (options$hypothesis == "groupsNotEqual") {
	        bf.title <- "BF\u2080\u2081"
	    }
	    if (options$hypothesis == "groupTwoGreater") {
	        bf.title <- "BF\u2080\u208A"
	    }
	    if (options$hypothesis == "groupOneGreater") {
	        bf.title <- "BF\u2080\u208B"
	    }
	    if(options$hypothesis == "allTypes"){
	        bf.title <- "BF\u2080\u2081" # same as not equal to
	    }
	}


	# Make the fields for the t-test table

	if (options$hypothesis == "groupsNotEqual" |
	    options$hypothesis == "groupOneGreater" |
	    options$hypothesis == 'groupTwoGreater') {


	    fields <- list(
	        list(name="Variable", type="string", title=""),
	        list(name = "hypothesis[type1]", type = "string", title = "Hypothesis"),
	        list(name="BF[type1]", type="number", format="sf:4;dp:3", title=bf.title),
	        list(name="pmp[type1]", type="number", format="sf:4;dp:3", title="Posterior probability"),
	        list(name = "hypothesis[type2]", type = "string", title = "Hypothesis"),
	        list(name="BF[type2]", type="number", format="sf:4;dp:3", title=bf.title),
	        list(name="pmp[type2]", type="number", format="sf:4;dp:3", title="Posterior probability"))


	}

	if(options$hypothesis == "allTypes"){

	    fields <- list(
	        list(name="Variable", type="string", title=""),
	        list(name = "type[greater]", type = "string", title = "Hypothesis"),
	        list(name="BF[greater]", type="number", format="sf:4;dp:3", title=bf.title),
	        list(name="pmp[greater]", type="number", format="sf:4;dp:3", title="Posterior probability"),
	        list(name = "type[less]", type = "string", title = "Hypothesis"),
	        list(name="BF[less]", type="number", format="sf:4;dp:3", title=bf.title),
	        list(name="pmp[less]", type="number", format="sf:4;dp:3", title="Posterior probability"),
	        list(name = "type[equal]", type = "string", title = "Hypothesis"),
	        list(name = "BF[equal]", type="number", format="sf:4;dp:3", title = bf.title),
	        list(name="pmp[equal]", type="number", format="sf:4;dp:3", title="Posterior probability"))

	}

	ttest[["schema"]] <- list(fields=fields)

	footnotes <- .newFootnotes()

	levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])

	if (length(levels) != 2) {

		g1 <- "1"
		g2 <- "2"

	} else {

		g1 <- levels[1]
		g2 <- levels[2]
	}

	if (options$hypothesis == "groupOneGreater") {

	    type <- 3

		message <- "For all tests, the alternative hypothesis specifies that group 1 is greater than group 2"
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "groupTwoGreater") {

	    type <- 2

		message <- "For all tests, the alternative hypothesis specifies that group 1 is less than group 2"
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "allTypes"){

	    type <- 4

	    message <- "For all tests, all of the informative hypotheses are tested"
	    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "groupsNotEqual"){

	    type <- 1

	    message <- "For all tests, the alternative hypothesis specifies that the mean of group 1 does not equal that of group 2"
	    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	}


	ttest.rows <- list()

	status <- rep("ok", length(options$variables))
	BF10post <- numeric(length(options$variables))
	plottingError <- rep("error", length(options$variables))
	errorFootnotes <- rep("no", length(options$variables))

	Bainresult <- list()

	for (variable in options[["variables"]]) {

		if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
			&& diff$groupingVariable == FALSE && diff$missingValues == FALSE)))) {

				index <- which(state$options$variables == variable)

				if (state$Bainresult[index] != "error") {

					ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]

					# if (! (is.logical(diff) && diff == FALSE) && diff$bayesFactorType) {
					#
					# 	if (state$options$bayesFactorType == "BF10") {
					#
					# 		if (options$bayesFactorType == "BF01") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- 1 / state$results$ttest$data[[index]]$BF
					# 		} else if (options$bayesFactorType == "LogBF10") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- log(state$results$ttest$data[[index]]$BF)
					# 		}
					#
					# 	} else if (state$options$bayesFactorType == "BF01") {
					#
					# 		if (options$bayesFactorType == "BF10") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- 1 / state$results$ttest$data[[index]]$BF
					# 		} else if (options$bayesFactorType == "LogBF10") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- log(1 / state$results$ttest$data[[index]]$BF)
					# 		}
					#
					# 	} else if (state$options$bayesFactorType == "LogBF10") {
					#
					# 		if (options$bayesFactorType == "BF10") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- exp(state$results$ttest$data[[index]]$BF)
					# 		} else if (options$bayesFactorType == "BF01") {
					# 			ttest.rows[[length(ttest.rows)]]$BF <- 1 / exp(state$results$ttest$data[[index]]$BF)
					# 		}
					# 	}
					# }

				} else {

					index2 <- .addFootnote(footnotes, state$errorFootnotes[[index]])

					if(options$hypothesis == "groupsNotEqual"){
					    result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
					                        "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "groupTwoGreater"){
					    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
					                       "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "groupOneGreater"){
					    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					    "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "allTypes"){
					    result_test <-list(Variable=variable,
					                       "type[greater]" = "Equal vs. Bigger",
					                       "BF[greater]"= .clean(NaN),
					                       "pmp[greater]" = .clean(NaN),
					                       "type[less]"= "Equal vs. Smaller",
					                       "BF[less]" = .clean(NaN),
					                       "pmp[less]" = .clean(NaN),
					                       "type[equal]" = "Bigger vs. Smaller",
					                       "BF[equal]" = .clean(NaN),
					                       "pmp[equal]" = .clean(NaN),
					                       .footnotes = list(BF=list(index2)))
					}

					ttest.rows[[length(ttest.rows)+1]] <- result_test

				}

			} else {

				ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable)
			}
	}

	rowCompleted <- logical(length(ttest.rows))

	for (i in seq_along(ttest.rows))
		rowCompleted[i] <- ifelse(length(ttest.rows[[i]]) > 1, TRUE, FALSE)

	if (!is.null(state) && all(options[["variables"]] %in% state$options$variables) && options$groupingVariable == state$options$groupingVariable && all(rowCompleted))
		ttest[["status"]] <- "complete"

	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		if (length(levels) != 2) {

			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")

			status <- rep("error", length(options$variables))
			plottingError <- rep("Plotting is not possible: The Grouping Variable must have 2 levels", length(options$variables))

		} else {

			rowNo <- 1

			i <- 1

			errorFootnotes <- list()

			for (variable in options[["variables"]]) {

				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)

				gs <- base::levels(levels)

				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]


				if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
				&& diff$groupingVariable == FALSE && diff$missingValues == FALSE)))) {

					index <- which(state$options$variables == variable)

					if (state$Bainresult[[index]] != "error") {

						ttest.rows[[i]] <- state$results$ttest$data[[index]]



						# if (! (is.logical(diff) && diff == FALSE) && diff$bayesFactorType) {
						#
						# 	if (state$options$bayesFactorType == "BF10") {
						#
						# 		if (options$bayesFactorType == "BF01") {
						# 			ttest.rows[[rowNo]]$BF <- 1 / state$results$ttest$data[[index]]$BF
						# 		} else if (options$bayesFactorType == "LogBF10") {
						# 			ttest.rows[[rowNo]]$BF <- log(state$results$ttest$data[[index]]$BF)
						# 		}
						#
						# 	} else if (state$options$bayesFactorType == "BF01") {
						#
						# 		if (options$bayesFactorType == "BF10") {
						# 			ttest.rows[[rowNo]]$BF <- 1 / state$results$ttest$data[[index]]$BF
						# 		} else if (options$bayesFactorType == "LogBF10") {
						# 			ttest.rows[[rowNo]]$BF <- log(1 / state$results$ttest$data[[index]]$BF)
						# 		}
						#
						# 	} else if (state$options$bayesFactorType == "LogBF10") {
						#
						# 		if (options$bayesFactorType == "BF10") {
						# 			ttest.rows[[rowNo]]$BF <- exp(state$results$ttest$data[[index]]$BF)
						# 		} else if (options$bayesFactorType == "BF01") {
						# 			ttest.rows[[rowNo]]$BF <- 1 / exp(state$results$ttest$data[[index]]$BF)
						# 		}
						# 	}
						# }

					} else if (state$Bainresult[[index]] == "error") {

					    index2 <- .addFootnote(footnotes, state$errorFootnotes[index])

					    errorFootnotes[i] <- state$errorFootnotes[index]

					    if(options$hypothesis == "groupsNotEqual"){
					        result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
					                            "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "groupTwoGreater"){
					        result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
					                           "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "groupOneGreater"){
					        result_test <-list(Variable=variable, "hypothesis[type1]" = "mu1 = mu2", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					        "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "allTypes"){
					        result_test <-list(Variable=variable,
					                           "type[greater]" = "Equal vs. Bigger",
					                           "BF[greater]"= .clean(NaN),
					                           "pmp[greater]" = .clean(NaN),
					                           "type[less]"= "Equal vs. Smaller",
					                           "BF[less]" = .clean(NaN),
					                           "pmp[less]" = .clean(NaN),
					                           "type[equal]" = "Bigger vs. Smaller",
					                           "BF[equal]" = .clean(NaN),
					                           "pmp[equal]" = .clean(NaN),
					                           .footnotes = list(BF=list(index2)))
					    }

					    ttest.rows[[i]] <- result_test

					}

					BF10post[rowNo] <- ttest.rows[[rowNo]] # state$BF10post[index]  # changed from ttest.rows[[rowNo]]$BF
					status[rowNo] <- state$status[index]
					plottingError[rowNo] <- state$plottingError[index]
					plotres[[i]] <- state$plotres[[index]]
					Bainresult[[i]] <- state$Bainresult[[index]]
					#plotVariables[[i]] <- state$plotVariables[[index]]

				} else {

				    errors <- .hasErrors(dataset=dataset, perform=perform, type=c('observations', 'variance', "infinity"),
				                         all.target=variable, observations.amount = "< 2", message = "short")

				    errorMessage <- NULL

				    if (!identical(errors, FALSE)) {
				        errorMessage <- errors$message
				    }

				    if (!is.null(errorMessage)) {

				        ## log the error in a footnote
				        index <- .addFootnote(footnotes, errorMessage)
				        row.footnotes <- list(t = list(index))

				        if(options$hypothesis == "groupsNotEqual"){
				            result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
				                                "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "groupTwoGreater"){
				            result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
				                               "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "groupOneGreater"){
				            result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
				            "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "allTypes"){
				            result_test <-list(Variable=variable,
				                               "type[greater]" = "Equal vs. Bigger",
				                               "BF[greater]"= .clean(NaN),
				                               "pmp[greater]" = .clean(NaN),
				                               "type[less]"= "Equal vs. Smaller",
				                               "BF[less]" = .clean(NaN),
				                               "pmp[less]" = .clean(NaN),
				                               "type[equal]" = "Bigger vs. Smaller",
				                               "BF[equal]" = .clean(NaN),
				                               "pmp[equal]" = .clean(NaN),
				                               .footnotes = row.footnotes)
				        }


				        Bainresult[[i]] <- "error"
				        plotVariables[[i]] <- variable
				        errorFootnotes[i] <- errorMessage
				        plotres[[i]] <- "error"

				    } else {

						r <- Bain::Bain_ttestData(group1, group2, type = type)
						Bainresult[[i]] <- "no error"

						plotres[[i]] <- r

						if(type == 1){
						    BF_0u <- r$BF_0u
						    PMP_u <- r$PMP_u
						    PMP_0 <- r$PMP_0
						}
						if(type == 2){
						    BF_01 <- r$BF_01
						    PMP_1 <- r$PMP_1
						    PMP_0 <- r$PMP_0
						}
						if(type == 3){
						    BF_01 <- r$BF_01
						    PMP_0 <- r$PMP_0
						    PMP_1 <- r$PMP_1
						}
						if(type == 4){
						    BF_01 <- r$BF_01
						    BF_02 <- r$BF_02
						    BF_12 <- r$BF_12
						    PMP_0 <- r$PMP_0
						    PMP_1 <- r$PMP_1
						    PMP_2 <- r$PMP_2
						}


						if(options$hypothesis == "groupsNotEqual"){
						    result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
						    "hypothesis[type2]" = "Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
						}
						if(options$hypothesis == "groupTwoGreater"){
						    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
						                       "hypothesis[type2]" = "Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
						}
						if(options$hypothesis == "groupOneGreater"){
						    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
						                       "hypothesis[type2]" = "Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
						}
						if(options$hypothesis == "allTypes"){
						    result_test <-list(Variable=variable,
						                       "type[greater]" = "Equal vs. Bigger",
						                       "BF[greater]"= .clean(BF_01),
						                       "pmp[greater]" = .clean(PMP_0),
						                       "type[less]"= "Equal vs. Smaller",
						                       "BF[less]" = .clean(BF_02),
						                       "pmp[less]" = .clean(PMP_1),
						                       "type[equal]" = "Bigger vs. Smaller",
						                       "BF[equal]" = .clean(BF_12),
						                       "pmp[equal]" = .clean(PMP_2))
						}

						# if (options$bayesFactorType == "BF01")
						# 	bf.raw <- 1 / bf.raw
						#
						# BF10post[i] <- bf.raw
						# BF <- .clean(bf.raw)
						#
						# if (options$bayesFactorType == "LogBF10") {
						#
						# 	BF <- log(BF10post[i])
						# 	BF <- .clean(BF)
						# }

						# errorMessage <- NULL

					# 	if(!is.null(errorMessage)){
					#
					# 		index <- .addFootnote(footnotes, errorMessage)
					# 		list(.variable=variable, BF=BF, error=error, .footnotes=list(BF=list(index)))
					# 	} else {
					#
					# 		list(.variable=variable, BF=BF, error=error)
					# 	}
					#
					#
					# if (class(result) == "try-error") {
					#
					# 	errorMessage <- .extractErrorMessage(result)
					#
					# 	plottingError[rowNo] <- paste("Plotting is not possible:", errorMessage, sep=" ")
					#
					# 	if (errorMessage == "Dependent variable must not contain missing or infinite values.") {
					#
					# 		errorMessage <- "Bayes factor is undefined - the dependent variable contains infinity"
					# 		status[rowNo] <- "error"
					# 		plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - the dependent variable contains infinity"
					#
					# 	} else if (errorMessage == "grouping factor must have exactly 2 levels") {
					#
					# 		# We know that the grouping factor *does* have two levels, because we've checked this earlier on
					# 		# This error means that all of one factor has been excluded because of missing values in the dependent
					#
					# 		errorMessage <- "Bayes factor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
					# 		status[rowNo] <- "error"
					# 		plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
					#
					# 	} else if (errorMessage == "data are essentially constant") {
					#
					# 		errorMessage <- "Bayes factor is undefined - one or both levels of the dependent contain all the same value (zero variance)"
					# 		status[rowNo] <- "error"
					# 		plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - one or both levels of the dependent contain all the same value (zero variance)"
					#
					# 	} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					#
					# 		errorMessage <- "Bayes factor is undefined - one or both levels of the dependent contain too few observations"
					# 		status[rowNo] <- "error"
					# 		plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - one or both levels of the dependent contain too few observations"
					# 	}
					#
					# 	index <- .addFootnote(footnotes, errorMessage)
					#
					# 	errorFootnotes[rowNo] <- errorMessage
					#
					# 	result <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
					#
					# 	status[rowNo] <- "error"
					# }

				    }

					ttest.rows[[i]] <- result_test

				}

				rowNo <- rowNo + 1
				i <- i + 1
			}
		}

		ttest[["status"]] <- "complete"
	}

	ttest[["footnotes"]] <- as.list(footnotes)
	ttest[["data"]] <- ttest.rows


	list(ttest, status, g1, g2, BFH1H0, plottingError, BF10post, errorFootnotes, plotres, Bainresult, plotVariables)
}

# .base_breaks_x <- function(x) {
#
# 	b <- unique(as.numeric(x))
# 	d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
# 	list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
#
# }
#
# .base_breaks_y3 <- function(x) {
#
# 	ci.pos <- c(x$ciLower, x$ciUpper)
# 	b <- pretty(ci.pos)
# 	d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
# 	list(	ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
# 			ggplot2::scale_y_continuous(breaks=c(min(b),max(b)))	)
# }

# .plot2GroupMeansBayesIndTtest <- function(v1=NULL, v2=NULL, nameV1=NULL, nameV2=NULL, groupingName=NULL, dependentName=NULL, descriptivesPlotsCredibleInterval=.95) {
#
# 	v1 <- na.omit(v1)
# 	v2 <- na.omit(v2)
#
# 	if (any(is.infinite(v1)) || any(is.infinite(v2)))
# 		stop("Plotting not possible: Variable contains infinity")
#
# 	posteriorSummary1 <- .posteriorSummaryGroupMean(variable=v1, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
# 	posteriorSummary2 <- .posteriorSummaryGroupMean(variable=v2, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
# 	summaryStat <- data.frame(	groupingVariable=c(nameV1, nameV2), dependent=c(posteriorSummary1$median, posteriorSummary2$median),
# 								ciLower=c(posteriorSummary1$ciLower, posteriorSummary2$ciLower), ciUpper=c(posteriorSummary1$ciUpper,
# 								posteriorSummary2$ciUpper))
#
# 	pd <- ggplot2::position_dodge(.2)
#
# 	p <-	ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
# 			ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
# 			ggplot2::geom_line(position=pd, size = .7) +
# 			ggplot2::geom_point(position=pd, size=4) +
# 			ggplot2::ylab(dependentName) +
# 			ggplot2::xlab(groupingName) +
# 			ggplot2::theme_bw() +
# 			ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
# 				panel.grid.major=ggplot2::element_blank(),
# 				axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
# 				axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
# 				panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
# 				plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
# 				legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
# 				panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
# 				legend.key = ggplot2::element_blank(),
# 				legend.title = ggplot2::element_text(size=12),
# 				legend.text = ggplot2::element_text(size = 12),
# 				axis.ticks = ggplot2::element_line(size = 0.5),
# 				axis.ticks.margin = grid::unit(1,"mm"),
# 				axis.ticks.length = grid::unit(3, "mm"),
# 				plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
# 			.base_breaks_y3(summaryStat) +
# 			.base_breaks_x(summaryStat$groupingVariable)
#
# 	print(p)
# }

.BainttestBayesianIndependentSamplesDescriptives <- function(dataset, options, perform,
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

	## add credible interval values if asked for in plot
	if (options$descriptivesPlots) {
		interval <- 100 * options$descriptivesPlotsCredibleInterval
		title <- paste0(interval, "% Credible Interval")
		fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
											 format = "sf:4;dp:3", title = "Lower",
											 overTitle = title)
		fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
											 format = "sf:4;dp:3", title = "Upper",
											 overTitle = title)
	}

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

		## if people don't know what a t-test is...
		if (length(levels) != 2) {
			descriptives[["error"]] <- list(errorType = "badData")

		} else {

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

							posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
							ciLower <- .clean(posteriorSummary$ciLower)
							ciUpper <- .clean(posteriorSummary$ciUpper)

						  n <- .clean(length(groupDataOm))
						  mean <- .clean(mean(groupDataOm))
						  std <- .clean(sd(groupDataOm))
						  sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

						  result <- list(variable = variable, group = level,
										 N = n, mean = mean, sd = std, se = sem, lowerCI = ciLower,
										 upperCI = ciUpper)

					  } else {

						n <- .clean(length(groupDataOm))
						result <- list(variable = variable, group = "",
									   N = n, mean = "", sd = "", se = "", lowerCI = "",
										 upperCI = "")
					}

					if (i == 1) {
						result[[".isNewGroup"]] <- TRUE
					}

					data[[rowNo]] <- result
					rowNo <- rowNo + 1
					}
				}
			}
		}
		descriptives[["status"]] <- "complete"
	}

	descriptives[["data"]] <- data
	descriptives
}
