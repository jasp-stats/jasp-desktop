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

Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
    if (is.null(dataset)) {
        if (perform == "run") {
            if (options$missingValues == "excludeListwise") {
                dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, exclude.na.listwise=options$variables)
            } else {
                dataset <- .readDataSetToEnd(columns.as.numeric=options$variables)
			}
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=options$variables)
		}
    }
    
    results <- list()

	meta <- list(
	    list(name="title", type="title"),
		list(name="correlations", type="table"),
		list(name="plot", type="image"))

	results[[".meta"]] <- meta

	results[["title"]] <- "Correlation Matrix"
	
	state <- .retrieveState()
	diff <- NULL

	if (!is.null(state)) {
	    diff <- .diff(options, state$options)
	}
	
	correlation.plot <- NULL

	if (perform == "init" & options$plotCorrelationMatrix) {
	    if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$confidenceIntervals == FALSE && diff$confidenceIntervalsInterval == FALSE
			&& diff$hypothesis == FALSE && diff$kendallsTauB == FALSE && diff$missingValues == FALSE && diff$pearson == FALSE && diff$plotCorrelationMatrix == FALSE
			&& diff$plotDensities == FALSE && diff$plotStatistics == FALSE && diff$spearman == FALSE && diff$variables == FALSE)))) {

			# if only "Report significance", "Flag significant correlations", "Means and Standard Deviations" or "Cross-product deviations and covariances" have changed, the previous plot can be used

			correlation.plot <- state$correlationPlot

		} else {

			variables <- unlist(options$variables)


			if (length(variables) > 1) {

				l <- length(variables)

				if (l <= 2 && (options$plotDensities || options$plotStatistics)) {

					width <- 580
					height <- 580

				} else if (l <= 2) {

					width <- 400
					height <- 400

				} else {

					width <- 250 * l
					height <- 250 * l

				}

				plot <- list()

				plot[["title"]] <- "Correlation Plot"
				plot[["width"]]  <- width
				plot[["height"]] <- height

				correlation.plot <- plot
			}
		}
	}

	if (perform == "run" && length(options$variables) > 0 && options$plotCorrelationMatrix) {


		if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$confidenceIntervals == FALSE && diff$confidenceIntervalsInterval == FALSE
			&& diff$hypothesis == FALSE && diff$kendallsTauB == FALSE && diff$missingValues == FALSE && diff$pearson == FALSE && diff$plotCorrelationMatrix == FALSE
			&& diff$plotDensities == FALSE && diff$plotStatistics == FALSE && diff$spearman == FALSE && diff$variables == FALSE)))) {

			# if only "Report significance", "Flag significant correlations", "Means and Standard Deviations" or "Cross-product deviations and covariances" have changed, the previous plot can be used

			correlation.plot <- state$correlationPlot

		} else {

			variables <- unlist(options$variables)

			# check variables
			d <- vector("character", length(.v(variables)))
			sdCheck <- vector("numeric", length(.v(variables)))
			infCheck <- vector("logical", length(.v(variables)))

			for (i in seq_along(.v(variables))) {

				variable2check <- na.omit(dataset[[.v(variables)[i]]])
				d[i] <- class(variable2check)
				sdCheck[i] <- sd(variable2check) > 0
				infCheck[i] <- all(is.finite(variable2check))
			}

			numericCheck <- d == "numeric" | d == "integer"
			variables <- .v(variables)
			variable.statuses <- vector("list", length(variables))

			for (i in seq_along(variables)) {

				variable.statuses[[i]]$unplotable <- FALSE
				variable.statuses[[i]]$plottingError <- NULL

				if ( ! (numericCheck[i] && sdCheck[i] && infCheck[i])) {

					variable.statuses[[i]]$unplotable <- TRUE

					if ( ! numericCheck[i]) {
						variable.statuses[[i]]$plottingError <- "Variable is not continuous"
					} else if ( ! infCheck[i]) {
						variable.statuses[[i]]$plottingError <- "Variable contains infinity"
					} else if ( ! sdCheck[i]) {
						variable.statuses[[i]]$plottingError <- "Variable has zero variance"
					}

				}
			}


			if (length(variables) > 0) {

				l <- length(variables)

				if (l <= 2 && (options$plotDensities || options$plotStatistics)) {

					width <- 580
					height <- 580

				} else if (l <= 2) {

					width <- 400
					height <- 400

				} else {

					width <- 250 * l
					height <- 250 * l

				}


				plot <- list()

				plot[["title"]] <- "Correlation Plot"
				plot[["width"]]  <- width
				plot[["height"]] <- height

				correlation.plot <- plot
				cexText <- 1.6

				# image <- .beginSaveImage(width, height)

				.plotFunc <- function() {
				if (l == 1) {

					# par(mfrow= c(1, 1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
					#
					# if ( ! variable.statuses[[1]]$unplotable) {
					#
					# 	.plotMarginalCor(dataset[[variables[1]]]) # plot marginal (histogram with density estimator)
					# 	mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)
					#
					# } else {
					#
					# 	.displayError(variable.statuses[[1]]$plottingError)
					# }

				} else if (l == 2 && !options$plotDensities && !options$plotStatistics) {

					par(mfrow= c(1, 1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))

					if ( ! variable.statuses[[1]]$unplotable && ! variable.statuses[[2]]$unplotable) {

						maxYlab <- .plotScatter(dataset[[variables[1]]], dataset[[variables[2]]])
						distLab <- maxYlab / 1.8

						mtext(text = .unv(variables)[1], side = 1, cex=1.5, line = 3)
						mtext(text = .unv(variables)[2], side = 2, cex=1.5, line = distLab + 2, las=0)

					} else {

						errorMessages <- c(variable.statuses[[1]]$plottingError, variable.statuses[[2]]$plottingError)
						errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
						.displayError(errorMessagePlot, cexText=cexText)
					}

				} else if (l >= 2) {

				if (l == 2)
					cexText <- 1.3

					par(mfrow= c(l, l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))

					for (row in seq_len(l)) {

						for (col in seq_len(l)) {

							if (row == col) {

								if (options$plotDensities) {

									if ( ! variable.statuses[[row]]$unplotable) {
										.plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
									} else {
										.displayError(variable.statuses[[row]]$plottingError, cexText=cexText)
									}

								} else {

									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}

							if (col > row) {

								if (options$plotCorrelationMatrix) {

									if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
										.plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
									} else {
										errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
										errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
										.displayError(errorMessagePlot, cexText=cexText)
									}

								} else {

									plot(1, type= "n", axes= FALSE, ylab="", xlab="")

								}
							}

							if (col < row) {

								if (l < 7) {

									if (options$plotStatistics) {

										if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
											.plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], hypothesis= options$hypothesis,
											pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval) # plot r= ...
										} else {
											errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
											errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
											.displayError(errorMessagePlot, cexText=cexText)
										}

									} else {

										plot(1, type= "n", axes= FALSE, ylab="", xlab="")

									}
								}

								if (l >= 7) {

									if (options$plotStatistics) {

										if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
											.plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2, hypothesis= options$hypothesis,
											pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval)
										} else {
											errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
											errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
											.displayError(errorMessagePlot, cexText=cexText)
										}

									} else {

										plot(1, type= "n", axes= FALSE, ylab="", xlab="")

									}
								}
							}
						}
					}
				}


				if (l > 2 || ((l == 2 && options$plotDensities) || (l == 2 && options$plotStatistics))) {

					textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))

					if (!options$plotDensities && !options$plotStatistics) {

							for (t in seq_along(textpos)) {

								mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)

								if (t < length(textpos)) {

									mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)

								}
							}

					} else {

						for (t in seq_along(textpos)) {

								mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
								mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
						}
					}
				}
				}

				# content <- .endSaveImage(image)
				content <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)

				plot <- correlation.plot

				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				# plot[["data"]]  <- content

				correlation.plot <- plot
			}
		}
	}

	results[["plot"]] <- correlation.plot

	keep <- NULL

	if (length(correlation.plot) > 0)
		keep <- correlation.plot$data

	correlationTableOutput <- .correlationTable(dataset, perform,
								variables=options$variables, pearson=options$pearson,
								kendallsTauB=options$kendallsTauB, spearman=options$spearman,
								hypothesis=options$hypothesis, reportSignificance=options$reportSignificance,
								flagSignificant=options$flagSignificant, reportVovkSellkeMPR=options$VovkSellkeMPR,
								meansAndStdDev=options$meansAndStdDev, crossProducts=options$crossProducts, state=state, diff=diff, options=options)

	# print(correlationTableOutput)
	tableVariables <- correlationTableOutput$variables
	tableTests <- correlationTableOutput$tests
	tableRows <- correlationTableOutput$rows
	tablePValues <- correlationTableOutput$pValues
	tableMPRs <- correlationTableOutput$MPRs
	tableUpperCIs <- correlationTableOutput$upperCIs
	tableLowerCIs <- correlationTableOutput$lowerCIs


	results[["correlations"]] <- correlationTableOutput$correlationTable

	if (perform == "init") {

		if (length(options$variables) < 2) {

			results <- list(results=results, status="complete", keep=keep)
			return(results)

		} else {

			results <- list(results=results, status="inited", state=state, keep=keep)
			return(results)
		}

	} else {

		return(list(results=results, status="complete", state=list(options=options, results=results, correlationPlot=correlation.plot, tableVariables=tableVariables, tableTests=tableTests,
					tableRows=tableRows, tablePValues=tablePValues, tableMPRs=tableMPRs, tableUpperCIs=tableUpperCIs, tableLowerCIs=tableLowerCIs), keep=keep))
	}
}

.correlationTable <- function(dataset, perform, variables=c(), pearson=TRUE, kendallsTauB=FALSE,
	spearman=FALSE, hypothesis="correlated", reportSignificance=FALSE, reportVovkSellkeMPR=FALSE,
	flagSignificant=FALSE, meansAndStdDev=FALSE, crossProducts=FALSE, state, diff, options) {

	correlation.table <- list()

	numberOfVariables <- length(variables)
	
	if (perform == "init") {
	    if (numberOfVariables == 0){
	        variables <- c(variables, "...", "... ")
	    } else if (numberOfVariables == 1){
	        variables <- c(variables, "... ")
	    }
	}
	
	# update number of variables, hence it's always >= 2, thus, outputting a table of 2 by 2
	#
	numberOfVariables <- length(variables)
	
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")

	if (length(tests) != 1) {

		correlation.table[["title"]] <- "Correlation Table"
	}
	else if (pearson) {

		correlation.table[["title"]] <- "Pearson Correlations"
	}
	else if (spearman) {

		correlation.table[["title"]] <- "Spearman Correlations"
	}
	else if (kendallsTauB) {

		correlation.table[["title"]] <- "Kendall's Tau"
	}
	else {

		correlation.table[["title"]] <- "Correlation Table"
	}

	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()

	footnotes <- .newFootnotes()

	if (flagSignificant || reportSignificance) {

		if (hypothesis == "correlatedPositively") {

			.addFootnote(footnotes, "all tests one-tailed, for positive correlation", symbol="<i>Note</i>.")

		} else if (hypothesis == "correlatedNegatively") {

			.addFootnote(footnotes, "all tests one-tailed, for negative correlation", symbol="<i>Note</i>.")
		}
	}

	if (flagSignificant) {

		if (hypothesis == "correlated") {

			.addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001", symbol="*")

		} else {

			.addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001, one-tailed", symbol="*")
		}
	}

	if (reportVovkSellkeMPR) {
		if (flagSignificant){
	    .addFootnote(footnotes, symbol = "\u207A", text = "Vovk-Sellke Maximum
	    <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
	    possible odds in favor of H\u2081 over H\u2080 equals
	    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
	    (Sellke, Bayarri, & Berger, 2001).")
		} else {
			.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
	    <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
	    possible odds in favor of H\u2081 over H\u2080 equals
	    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
	    (Sellke, Bayarri, & Berger, 2001).")
		}
	}

	pValueList <- list()
	MPRList <- list()
	upperCIList <- list()
	lowerCIList <- list()


	if (numberOfVariables > 0) {

		test.names <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau B")

		column.names <- c()

		for (test in tests) {

			if (length(tests) > 1 || reportSignificance || (test == "pearson" && options$confidenceIntervals) || reportVovkSellkeMPR) {

				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}

			for (variable.name in variables) {

				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}

			if (reportSignificance) {

				column.name <- paste(".test[", test, "-p]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")


				for (variable.name in variables) {

					column.name <- paste(variable.name, "[", test, "-p]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3;p:.001")
				}
			}

			if (reportVovkSellkeMPR){
				column.name <- paste(".test[", test, "-MPR]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")


				for (variable.name in variables) {

					column.name <- paste(variable.name, "[", test, "-MPR]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="sf:4;dp:3")
				}
			}

			if (test == "pearson" && options$confidenceIntervals) {

				column.name <- paste(".test[", test, "-upperCI]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")


				for (variable.name in variables) {

					column.name <- paste(variable.name, "[", test, "-upperCI]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
				}

				column.name <- paste(".test[", test, "-lowerCI]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")


				for (variable.name in variables) {

					column.name <- paste(variable.name, "[", test, "-lowerCI]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
				}
			}
		}


		for (i in 1:numberOfVariables) {

			row <- list()
			row.footnotes <- list()

			variable.name <- variables[[i]]

			for (test in tests) {

				p.values <- list()
				MPRs <- list()
				upperCIs <- list()
				lowerCIs <- list()

				if (length(tests) > 1 || reportSignificance || (test == "pearson" && options$confidenceIntervals))
					row[[length(row)+1]] <- test.names[[test]]

				if (reportSignificance)
					p.values[[length(p.values)+1]] <- "p-value"

				if (reportVovkSellkeMPR){
					if (flagSignificant){
						MPRs[[length(MPRs)+1]] <- "VS-MPR\u207A"
					} else {
						MPRs[[length(MPRs)+1]] <- "VS-MPR\u002A"
					}
				}

				if (test == "pearson" && options$confidenceIntervals) {

					upperCIs[[length(upperCIs)+1]] <- paste("Upper ", 100 * options$confidenceIntervalsInterval, "% CI", sep="")
					lowerCIs[[length(lowerCIs)+1]] <- paste("Lower ", 100 * options$confidenceIntervalsInterval, "% CI", sep="")
				}

				for (j in .seqx(1, i-1)) {

					row[[length(row)+1]] <- ""
					p.values[[length(p.values)+1]] <- ""
					MPRs[[length(MPRs)+1]] <- ""
					upperCIs[[length(upperCIs)+1]] <- ""
					lowerCIs[[length(lowerCIs)+1]] <- ""
				}

				row[[length(row)+1]] <- "\u2014" # em-dash
				p.values[[length(p.values)+1]] <- "\u2014"
				MPRs[[length(MPRs)+1]] <- "\u2014"
				upperCIs[[length(upperCIs)+1]] <- "\u2014"
				lowerCIs[[length(lowerCIs)+1]] <- "\u2014"


				for (j in .seqx(i+1, numberOfVariables)) {

					variable.2.name <- variables[[j]]
					column.name <- paste(variable.2.name, "[", test, "]", sep="")

					v1 <- dataset[[ .v(variable.name) ]]
					v2 <- dataset[[ .v(variable.2.name) ]]


					if (!is.null(state) && !is.null(diff) && test %in% state$tableTests && variable.name %in% state$tableVariables && column.name %in% names(state$tableRows[[which(state$tableVariable == variable.name)]])
						&& ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE && diff$missingValues == FALSE && diff$confidenceIntervals == FALSE && diff$confidenceIntervalsInterval == FALSE)))) {
            
					  variableIndex <- which(state$tableVariable == variable.name)
						estimate <- state$tableRows[[variableIndex]][[column.name]]
						p.value <- state$tablePValues[[variable.name]][[column.name]]
						MPR <- state$tableMPRs[[variable.name]][[column.name]]

						pValueList[[variable.name]][[column.name]] <- p.value
						MPRList[[variable.name]][[column.name]] <- MPR

						if (test == "pearson" && options$confidenceIntervals) {

							upperCI <- state$tableUpperCIs[[variable.name]][[column.name]]
							lowerCI <- state$tableLowerCIs[[variable.name]][[column.name]]

							upperCIList[[variable.name]][[column.name]] <- upperCI
							lowerCIList[[variable.name]][[column.name]] <- lowerCI

							upperCIs[[length(upperCIs)+1]] <- .clean(upperCI)
							lowerCIs[[length(lowerCIs)+1]] <- .clean(lowerCI)
						}


					 	row[[length(row)+1]] <- estimate

					 	if (flagSignificant && is.numeric(p.value) && is.na(p.value) == FALSE) {

					 		if (p.value < .001) {

					 			row.footnotes[[column.name]] <- list("***")

					 		} else if (p.value < .01) {

					 			row.footnotes[[column.name]] <- list("**")

					 		} else if (p.value < .05) {

					 			row.footnotes[[column.name]] <- list("*")
					 		}
					 	}

					 	if (reportSignificance)
					 		p.values[[length(p.values)+1]] <- .clean(p.value)

						if (reportVovkSellkeMPR)
							MPRs[[length(MPRs)+1]] <- MPR


					 } else {

						if (perform == "run") {
						  errors <- .hasErrors(dataset, perform = perform, message = 'short', type = c('variance', 'infinity'),

						                       all.target = c(variable.name, variable.2.name))
						  if (!identical(errors, FALSE)) {
						    index <- .addFootnote(footnotes, errors$message)
						    row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
						  }
						  

							if (hypothesis == "correlated") {

								result <- cor.test(v1, v2, method=test, alternative="two.sided", conf.level= options$confidenceIntervalsInterval)
							}
							else if (hypothesis == "correlatedPositively") {

								result <- cor.test(v1, v2, method=test, alternative="greater", conf.level= options$confidenceIntervalsInterval)

							} else {

								result <- cor.test(v1, v2, method=test, alternative="less", conf.level= options$confidenceIntervalsInterval)
							}

							estimate <- as.numeric(result$estimate)
							p.value  <- as.numeric(result$p.value)
							MPR <- .VovkSellkeMPR(p.value)

							if (test == "pearson" && options$confidenceIntervals) {

								upperCI <- as.numeric(result$conf.int[2])
								lowerCI <- as.numeric(result$conf.int[1])

								upperCIList[[variable.name]][[column.name]] <- upperCI
								lowerCIList[[variable.name]][[column.name]] <- lowerCI
							}


							pValueList[[variable.name]][[column.name]] <- p.value
							MPRList[[variable.name]][[column.name]] <- MPR

							row[[length(row)+1]] <- .clean(estimate)

							if (flagSignificant && is.na(p.value) == FALSE) {

								if (p.value < .001) {

									row.footnotes[[column.name]] <- list("***")

								} else if (p.value < .01) {

									row.footnotes[[column.name]] <- list("**")

								} else if (p.value < .05) {

									row.footnotes[[column.name]] <- list("*")
								}
							}

							if (reportSignificance)
								p.values[[length(p.values)+1]] <- .clean(p.value)

							if (reportVovkSellkeMPR)
								MPRs[[length(MPRs)+1]] <- MPR


							if (test == "pearson" && options$confidenceIntervals) {

								upperCIs[[length(upperCIs)+1]] <- .clean(upperCI)
								lowerCIs[[length(lowerCIs)+1]] <- .clean(lowerCI)
							}

						} else {

							row[[length(row)+1]] <- "."
							p.values[[length(p.values)+1]] <- "."
							MPRs[[length(MPRs)+1]] <- "."

							if (test == "pearson" && options$confidenceIntervals) {

								upperCIs[[length(upperCIs)+1]] <- "."
								lowerCIs[[length(lowerCIs)+1]] <- "."
							}
						}
					}
				}

				if (reportSignificance) {

					for (p.value in p.values)
						row[[length(row)+1]] <- p.value

				}

				if (reportVovkSellkeMPR){
					for (MPR in MPRs)
						row[[length(row)+1]] <- MPR
				}


				if (test == "pearson" && options$confidenceIntervals) {

					for (upperCI in upperCIs)
						row[[length(row)+1]] <- upperCI

					for (lowerCI in lowerCIs)
						row[[length(row)+1]] <- lowerCI

				}

			}

			names(row) <- column.names
			row[[".variable"]] <- variable.name

			if (length(row.footnotes) > 0)
				row[[".footnotes"]] <- row.footnotes

			rows[[i]] <- row
		}
	}


	schema <- list(fields=fields)

	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	correlation.table[["footnotes"]] <- as.list(footnotes)

	return(list(correlationTable=correlation.table, rows=rows, tests=tests, variables=variables, pValues=pValueList, MPRs=MPRList, upperCIs=upperCIList, lowerCIs=lowerCIList))
}

#### histogram with density estimator ####
.plotMarginalCor <- function(variable, cexYlab= 1.3, lwd= 2, rugs= FALSE) {

	variable <- variable[!is.na(variable)]

	density <- density(variable)
	h <- hist(variable, plot = FALSE)
	jitVar <- jitter(variable)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(variable, h$breaks), min.n= 3)
	plot(range(xticks), c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks)
	par(las=0)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.08*diff(range(ax1)), cex.axis= 1.7, mgp= c(3, 0.7, 0))

	if (rugs) {
		rug(jitVar)
	}

	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)
}


#### scatterplots ####

# predictions of fitted model
.poly.pred <- function(fit, line=FALSE, xMin, xMax, lwd) {

	# create function formula
	f <- vector("character", 0)

	for (i in seq_along(coef(fit))) {

		if (i == 1) {

			temp <- paste(coef(fit)[[i]])
			f <- paste(f, temp, sep="")
		}

		if (i > 1) {

			temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
			f <- paste(f, temp, sep="+")
		}
	}

	x <- seq(xMin, xMax, length.out = 100)
	predY <- eval(parse(text=f))

	if (line == FALSE) {
		return(predY)
	}

	if (line) {
		lines(x, predY, lwd=lwd)
	}
}


.plotScatter <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2) {

	d <- data.frame(xx= xVar, yy= yVar)
	d <- na.omit(d)
	xVar <- d$xx
	yVar <- d$yy

	# fit different types of regression
	fit <- vector("list", 1)# vector("list", 4)
	fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
	# fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
	# fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
	# fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)

	# find parsimonioust, best fitting regression model
	# Bic <- vector("numeric", 4)
	# for(i in 1:4){
	#	Bic[i] <- BIC(fit[[i]])
	# }

	bestModel <- 1 # which.min(Bic)


	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))
	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))


	yticks <- pretty(c(ylow, yhigh))

	yLabs <- vector("character", length(yticks))

	for (i in seq_along(yticks)) {

		if (yticks[i] < 10^6 && yticks[i] > 10^-6) {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)

		} else {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}
	}

	plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)
	.poly.pred(fit[[bestModel]], line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)

	par(las=1)

	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis)
	axis(2, line= 0.2, labels= yLabs, at= yticks, cex.axis= cexYAxis)

	invisible(max(nchar(yLabs)))
}

#### display correlation value ####
.plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.7, hypothesis = "correlated", pearson=options$pearson,
	kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=0.95) {

	CIPossible <- TRUE

	tests <- c()

	if (pearson)
		tests <- c(tests, "pearson")

	if (spearman)
		tests <- c(tests, "spearman")

	if (kendallsTauB)
		tests <- c(tests, "kendall")


	plot(1, 1, type="n", axes=FALSE, ylab="", xlab="")

	lab <- vector("list")

	for (i in seq_along(tests)) {

		if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == 1){

			CIPossible <- FALSE

			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == "1.000")
			}

			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(italic(rho) == "1.000")
			}

			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(italic(tau) == "1.000")
			}

		} else if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == -1){

			CIPossible <- FALSE

			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == "-1.000")
			}

			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(italic(rho) == "-1.000")
			}

			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(italic(tau) == "-1.000")
			}

		} else {

			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}

			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(rho == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}

			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(tau == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}
		}
	}


	if (length(tests) == 1) {
		ypos <- 1
	}

	if (length(tests) == 2) {
		ypos <- c(1.1, 0.9)
	}

	if (length(tests) == 3) {
		ypos <- c(1.2, 1, 0.8)
	}


	for (i in seq_along(tests)) {

		text(1, ypos[i], labels= lab[[i]], cex= cexText)
	}


	if (hypothesis == "correlated" & length(tests) == 1 & any(tests == "pearson")) {

		alternative <- "two.sided"
		ctest <- cor.test(xVar, yVar, method= tests, conf.level=confidenceInterval)
	}

	if (hypothesis != "correlated" & length(tests) == 1 & any(tests == "pearson")) {

		if (hypothesis == "correlatedPositively") {

			ctest <- cor.test(xVar, yVar, method=tests, alternative="greater", conf.level=confidenceInterval)

		} else if (hypothesis == "correlatedNegatively") {

			ctest <- cor.test(xVar, yVar, method=tests, alternative="less", conf.level=confidenceInterval)
		}

	}


	if (any(tests == "pearson")& length(tests) == 1 && CIPossible) {

		CIlow <- formatC(round(ctest$conf.int[1],3), format = "f", digits = 3)
		CIhigh <- formatC(round(ctest$conf.int[2],3), format = "f", digits = 3)

		text(1,0.8, labels= paste(100 * confidenceInterval, "% CI: [", CIlow, ", ", CIhigh, "]", sep=""), cex= cexCI)
	}

}

### empty Plot with error message ###
.displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2) {

	plot(1, type='n', xlim=c(-1,1), ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
	text(0, .5, errorMessage, cex=cexText)

}
