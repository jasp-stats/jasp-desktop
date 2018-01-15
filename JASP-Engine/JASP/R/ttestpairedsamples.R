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

TTestPairedSamples <- function(dataset = NULL, options, perform = "run",
							   callback = function(...) 0,  ...) {
	
	state <- .retrieveState()	
	
	## call the common initialization function
	init <- .initializeTTest(dataset, options, perform, type = 'paired')

	results <- init[["results"]]
	dataset <- init[["dataset"]]
	
	# test whether pairs are identical
	if (length(options$pairs) > 0) {
	  for (i in 1:length(options$pairs)) {
	    datasetErrorCheck <- data.frame(dataset[[.v(options$pairs[[i]][[1]])]] - dataset[[.v(options$pairs[[i]][[2]])]])
	    colnames(datasetErrorCheck) <- .v(paste0("Difference between ", options$pairs[[i]][[1]], " and ", options$pairs[[i]][[2]]))
	    .hasErrors(datasetErrorCheck, perform, type = "variance", exitAnalysisIfErrors = TRUE)
	  }
	}
	
	## call the specific paired T-Test functions
	results[["ttest"]] <- .ttestPairedSamples(dataset, options, perform)
	descriptivesTable <- .ttestPairedSamplesDescriptives(dataset, options, perform)
	shapiroWilk <- .ttestPairedNormalityTest(dataset, options, perform)
	results[["assumptionChecks"]] <- list(shapiroWilk = shapiroWilk, title = "Assumption Checks")


	keep <- NULL
	## if the user wants descriptive plots, s/he shall get them!
	if (options$descriptivesPlots) {

		plotTitle <- ifelse(length(options$pairs) > 1, "Descriptives Plots", "Descriptives Plot")
		descriptivesPlots <- .pairedSamplesTTestDescriptivesPlot(dataset, options, perform)
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


.ttestPairedSamples <- function(dataset, options, perform) {

	ttest <- list()

	## what does the user want? what does s/he really want??
	wantsEffect <- options$effectSize
	wantsDifference <- options$meanDifference
	wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
	wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
	percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
	percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
	wantsStudents <- options$students
	wantsWilcox <- options$wilcoxonSignedRank

	allTests <- c(wantsStudents, wantsWilcox)
	onlyTest <- sum(allTests) == 1

	## setup table for paired t-test results
	fields <- list(list(name = "v1", type = "string", title = ""),
				   list(name = "sep",  type = "separator", title = ""),
				   list(name = "v2", type = "string", title = ""),
				   list(name = "df",  type = "integer"),
				   list(name = "p", type = "number", format = "dp:3;p:.001"))

	footnotes <- .newFootnotes()
	title <- "Paired Samples T-Test"

	## get the right statistics for the table and, if only one test type, add footnote
	if (wantsWilcox && onlyTest) {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = "Wilcoxon signed-rank test.")
		testStat <- "W"
		fields <- fields[-4] #Wilcoxon's test doesn't have degrees of freedoms
		nameOfLocationParameter <- "Hodges-Lehmann Estimate"
		nameOfEffectSize <- "Rank-Biserial Correlation"
	} else if (wantsStudents && onlyTest) {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = "Student's t-test.")
		testStat <- "t"
		nameOfLocationParameter <- "Mean Difference"
		nameOfEffectSize <- "Cohen's d"
	} else {
		testStat <- "Statistic"
		nameOfLocationParameter <-  "Location Parameter"
		nameOfEffectSize <-  "Effect Size"
	}

	ttest[["title"]] <- title

	## if only conducting Student's, the table should have "t" as column name
	## for the test statistic when doing only Wilcoxon's, the name should be "V";
	## when doing both, it should be "statistic"
	fields <- append(fields, list(list(name = testStat,
								  type = "number", format = "sf:4;dp:3")), 3)

	## if the user wants all tests, add a column called "Test"
	if (sum(allTests) == 2) {
		fields <- append(fields, list(list(name = "test",
									  type = "string", title = "Test")), 3)
	}

	if (options$VovkSellkeMPR) {
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
		fields[[length(fields) + 1]] <- list(name = "md", title = nameOfLocationParameter,
											 type = "number", format = "sf:4;dp:3")
		if(wantsStudents) {
		  fields[[length(fields) + 1]] <- list(name = "sed", title = "SE Difference",
											 type = "number", format = "sf:4;dp:3")
		}
		if (wantsWilcox && wantsStudents) {
		  .addFootnote(footnotes, symbol = "<em>Note.</em>", text = "For the Student t-test, 
  	               location parameter is given by mean difference <em>d</em>; for the Wilcoxon test, 
  	               effect size is given by the Hodges-Lehmann estimate.")
		} 
	}
	
	if (wantsConfidenceMeanDiff) {
	  interval <- 100 * percentConfidenceMeanDiff
	  title <- paste0(interval, "% CI for ", nameOfLocationParameter)
	  
	  fields[[length(fields) + 1]] <- list(name = "lowerCIlocationParameter", type = "number",
	                                       format = "sf:4;dp:3", title = "Lower",
	                                       overTitle = title)
	  fields[[length(fields) + 1]] <- list(name = "upperCIlocationParameter", type = "number",
	                                       format = "sf:4;dp:3", title = "Upper",
	                                       overTitle = title)
	}

	if (wantsEffect) {
		fields[[length(fields) + 1]] <- list(name = "d", title = nameOfEffectSize,
											 type = "number",  format = "sf:4;dp:3")
		if (wantsWilcox && wantsStudents) {
		  .addFootnote(footnotes, symbol = "<em>Note.</em>", text = "For the Student t-test, 
  	               effect size is given by Cohen's <em>d</em>; for the Wilcoxon test, 
  	               effect size is given by the matched rank biserial correlation.")
		} 
	}

	if (wantsConfidenceEffSize) {
	  interval <- 100 * percentConfidenceEffSize
	  title <- paste0(interval, "% CI for ", nameOfEffectSize)
	  
	  fields[[length(fields) + 1]] <- list(name = "lowerCIeffectSize", type = "number",
	                                       format = "sf:4;dp:3", title = "Lower",
	                                       overTitle = title)
	  fields[[length(fields) + 1]] <- list(name = "upperCIeffectSize", type = "number",
	                                       format = "sf:4;dp:3", title = "Upper",
	                                       overTitle = title)
	}

	ttest[["schema"]] <- list(fields = fields)

	#########################
	## check the directionality
	if (options$hypothesis == "groupOneGreater") {
		direction <- "greater"
		message <- "All tests, hypothesis is measurement one greater than measurement two."
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)

	} else if (options$hypothesis == "groupTwoGreater") {
		direction <- "less"
		message <- "All tests, hypothesis is measurement one less than measurement two."
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)
	} else {
		direction <- "two.sided"
	}

	rowNo <- 1
	ttest.rows <- list() # for each pair and each test, save stuff in there
	whichTests <- list("1" = wantsStudents, "2" = wantsWilcox)

	## add a row for each variable, even before we are conducting tests
    for (pair in options$pairs) {
        ttest.rows[[length(ttest.rows) + 1]] <- list(v1 = pair[[1]],
                                                     sep = '-',
                                                     v2 = pair[[2]])
    }

	## for each pair, run the checked tests and update the table
	for (pair in options$pairs) {

		p1 <- pair[[1]]
		p2 <- pair[[2]]
		
		errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
							 all.target = c(p1, p2),
							 observations.amount = '< 2')
		
		row <- list(v1 = "", sep = "", v2 = "")

		## test is a number, indicating which tests should be run
		for (test in seq_len(length(whichTests))) {

			currentTest <- whichTests[[test]]

			## don't run a test the user doesn't want
			if (!currentTest) {
				next
			}
			
			if (!identical(errors, FALSE)) {
				errorMessage <- errors$message

			} else {
			    errorMessage <- NULL
			}

			if (perform == "run") {

				row.footnotes <- NULL

				if (p1 != "" && p2 != "") {

					c1 <- dataset[[ .v(p1) ]]
					c2 <- dataset[[ .v(p2) ]]

					df <- na.omit(data.frame(c1 = c1, c2 = c2))

					c1 <- df$c1
					c2 <- df$c2
					n <- length(c1)

					if(is.null(errorMessage)){

					result <- try(silent = FALSE, expr = {

						## if Wilcox box is ticked, run a paired wilcoxon signed rank test
						if (test == 2) {
							res <- stats::wilcox.test(c1, c2, paired = TRUE,
													  conf.level = percentConfidenceMeanDiff, conf.int = TRUE,
													  alternative = direction)
							maxw <- (n*(n+1))/2
							d <- as.numeric((res$statistic/maxw) * 2 - 1)
							wSE <- sqrt((n*(n+1)*(2*n+1))/6) /2
							mrSE <- sqrt(wSE^2  * 4 * (1/maxw^2)) 
							# zSign <- (ww$statistic - ((n*(n+1))/4))/wSE
							zmbiss <- atanh(d)
							d <- .clean(d)
							if(direction == "two.sided") {
							  confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize)/2)*mrSE), tanh(zmbiss + qnorm((1+percentConfidenceEffSize)/2)*mrSE)))
							}else if (direction == "less") {
							  confIntEffSize <- sort(c(-Inf, tanh(zmbiss + qnorm(percentConfidenceEffSize)*mrSE)))
							}else if (direction == "greater") {
							  confIntEffSize <- sort(c(tanh(zmbiss + qnorm((1-percentConfidenceEffSize))*mrSE), Inf))
							}
						## else run a simple paired t-test
						} else {
							res <- stats::t.test(c1, c2, paired = TRUE, conf.level = percentConfidenceMeanDiff,
												 alternative = direction)
							df <- ifelse(is.null(res$parameter), "", as.numeric(res$parameter))
							d <- .clean(mean(c1 - c2) / sd(c1 - c2))
							t <- as.numeric(res$statistic)
							confIntEffSize <- c(0,0)
							if (wantsConfidenceEffSize) {
							
							  alphaLevels <- sort( c( (1-percentConfidenceEffSize), percentConfidenceEffSize ) )
							  
							  if (direction == "two.sided") {
							    alphaLevels[1] <- (1-percentConfidenceEffSize) / 2
							    alphaLevels[2] <- (percentConfidenceEffSize + 1) / 2
							  } 
							  
							  end1 <- abs(t)
							  while( pt(q=t,df=df,ncp=end1) > alphaLevels[1]){
							    end1 = end1 * 2
							  }
							  ncp1 <- uniroot(function(x) alphaLevels[1] - pt(q=t, df=df, ncp=x),
							                  c(2*t-end1,end1))$root
							  
							  end2 = -abs(t)
							  while( pt(q=t,df=df,ncp=end2) < alphaLevels[2]){
							    end2 = end2 * 2
							    # end2 <- end2 - abs(t)
							  }
							  ncp2 <- uniroot(function(x) alphaLevels[2] - pt(q=t, df=df, ncp=x),
							                  c(end2,2*t-end2))$root						    
							  confIntEffSize <- sort(c(ncp1/sqrt(n), ncp2/sqrt(n)))[order(c(1-percentConfidenceEffSize, percentConfidenceEffSize ))]
							  
							  if (direction == "greater") {
							    confIntEffSize[2] <- Inf
							  } else if (direction == "less") 
							    confIntEffSize[1] <- -Inf	
							  
							  confIntEffSize <- sort(confIntEffSize)
							}
						}

						## don't use a return statement in expressions
						## ... it will take ages to debug :)
						res
					})

					## if testing raised an error, we extract information from it
					if (class(result) != "try-error") {

						## same for all tests
						p <- as.numeric(result$p.value)
						stat <- .clean(as.numeric(result$statistic))
						sed <- .clean(sd(c1 - c2) / sqrt(length(c1)))
						# num <- sqrt(sd(c1)^2 + sd(c2)^2 -  2 * cov(c1, c2))
						# d <- .clean(mean(c1) - mean(c2) / num)

						m <- as.numeric(result$estimate)
						ciLow <- ifelse(direction == "less", .clean(-Inf),
										as.numeric(result$conf.int[1]))
						ciUp <- ifelse(direction == "greater", .clean(Inf),
									   as.numeric(result$conf.int[2]))
						ciLowEffSize = .clean(as.numeric(confIntEffSize[1]))
						ciUpEffSize = .clean(as.numeric(confIntEffSize[2]))

						## paired t-test has it, wilcox doesn't!
						df <- ifelse(is.null(result$parameter), "", as.numeric(result$parameter))
						sed <- ifelse(is.null(result$parameter), "", sed)
						
						# add things to the intermediate results object
						row <- list(df = df, p = p, md = m, d = d,
			            lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp, 
			            lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize,
									sed = sed, .footnotes = row.footnotes)

						if (options$VovkSellkeMPR){
						  row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
						}

						row[[testStat]] <- stat

					## however, if there has been an error
					## find out which and log it as a footnote

					} else if (isTryError(result)) {
					    errorMessage <- .extractErrorMessage(result)
					}
					
					}
					
					if (!is.null(errorMessage)){


						index <- .addFootnote(footnotes, errorMessage)
						row.footnotes <- list(t = list(index))

						## since an error occured, we could not extract information
						row <- list(df = "", p = "", md = "",
									d = "", lowerCI = "", upperCI = "",
									sed = "", .footnotes = row.footnotes)
						row[[testStat]] <- .clean(NaN)
					}

				  } else {

					  ## if p1 or p2 are undefined (empty string), also show an empty table
					  row <- list(df = "", p = "", md = "", d = "",
								lowerCI = "", upperCI = "", sed = "")
					  row[[testStat]] <- ""
				  }
			}

			## if this is the first test / row for specific variables, add variable names
			## since we have only two tests, the first test always will be an odd number
			isFirst <- (rowNo %% 2 == 1 && sum(allTests) == 2) || (sum(allTests) == 1)
			row[["v1"]] <- ifelse(isFirst, p1, "")
			row[["sep"]] <- ifelse(isFirst, "-", "")
			row[["v2"]] <- ifelse(isFirst, p2, "")

			## if the user wants both tests, change the columns such that there is
			## one called "Test" and "statistic" to differentiate Student's from Wilcoxon
			if (!isFirst) {
				row[["test"]] <- "Wilcoxon"
				ttest.rows[[rowNo - 1]][["test"]] <- "Student"
			}

			ttest.rows[[rowNo]] <- row
			rowNo <- rowNo + 1
		}
	}

	## if no test was conducted
	if (length(ttest.rows) == 0) {
		ttest.rows[[1]] <- list()
	}

	ttest[["data"]] <- ttest.rows
	ttest[["footnotes"]] <- as.list(footnotes)
	ttest
}


.ttestPairedSamplesDescriptives <- function(dataset, options, perform) {
	if (!options$descriptives) return(NULL)

	descriptives <- list(title = "Descriptives")

	fields <- list(
		list(name = "v", title = "", type = "string"),
		list(name = "N",  type = "integer"),
		list(name = "mean", title = "Mean", type = "number",  format = "sf:4;dp:3"),
		list(name = "sd", title = "SD", type = "number", format = "sf:4;dp:3"),
		list(name = "se", title = "SE", type = "number",  format = "sf:4;dp:3")
	)

	descriptives[["schema"]] <- list(fields = fields)

	descriptives.results <- list()
	desc.vars <- unique(unlist(options$pairs))
	desc.vars <- desc.vars[desc.vars != ""]

	for (var in desc.vars) {

		row <- list(v = var)

		if (perform == "run") {

			dat <- na.omit(dataset[[ .v(var) ]])
			n <- .clean(as.numeric(length(dat)))
			m <- .clean(as.numeric(mean(dat, na.rm = TRUE)))
			std <- .clean(as.numeric(sd(dat, na.rm = TRUE)))

			if (is.numeric(std)) {
				se <- .clean(as.numeric(std/sqrt(n)))
			} else {
				se <- .clean(NaN)
			}

			row[["N"]] <- n
			row[["mean"]] <- m
			row[["sd"]] <- std
			row[["se"]] <- se
		}

		descriptives.results[[length(descriptives.results) + 1]] <- row
	}

	descriptives[["data"]] <- descriptives.results
	descriptives
}


.ttestPairedNormalityTest <- function(dataset, options, perform) {
	if (!options$normalityTests) return(NULL)

	normalityTests <- list(title = "Test of Normality (Shapiro-Wilk)")
	normalityTests[["cases"]] <- I(options$variables)

	fields <- list(
		list(name = "v1", type = "string", title = ""),
		list(name = "sep",  type = "separator", title = ""),
		list(name = "v2", type = "string", title = ""),
		list(name = "W", title = "W", type = "number", format = "sf:4;dp:3"),
		list(name = "p", title = "p", type = "number", format = "dp:3;p:.001")
	)

	normalityTests[["schema"]] <- list(fields = fields)

	footnotes <- .newFootnotes()
	normalityTests.results <- list()
	.addFootnote(footnotes, symbol = "<em>Note.</em>",
				 text = "Significant results suggest a deviation from normality.")


	pairs <- options$pairs
	if (length(pairs) == 0) {
		pairs[[1]] <- list(".", ".")
	}

	for (pair in pairs) {

		p1 <- pair[[1]]
		p2 <- pair[[2]]
		
		if (perform == "run" && length(options$pairs) > 0 && p1 != p2) {

			c1 <- dataset[[ .v(p1) ]]
			c2 <- dataset[[ .v(p2) ]]
			data <- na.omit(c1 - c2)

			row.footnotes <- NULL
			error <- FALSE
			
			errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
			                     all.target = c(p1, p2),
			                     observations.amount = c('< 3', '> 5000'))
			
			if (!identical(errors, FALSE)) {
			    errorMessage <- errors$message
			    foot.index <- .addFootnote(footnotes, errorMessage)
			    row.footnotes <- list(W = list(foot.index), p = list(foot.index))
			    error <- TRUE
			}

			if (!error) {

				r <- stats::shapiro.test(data)
				W <- .clean(as.numeric(r$statistic))
				p <- .clean(r$p.value)
				newGroup <- length(normalityTests.results) == 0

				result <- list(v1 = p1, sep = "-", v2 = p2,
							   W = W,  p = p, .isNewGroup = newGroup)
			} else {
				newGroup <- length(normalityTests.results) == 0
				result <- list(v1 = p1, sep = "-", v2 = p2, W = "NaN",
							 p = "NaN", .isNewGroup = newGroup,
							 .footnotes = row.footnotes)
			}
		} else {

			newGroup <- length(normalityTests.results) == 0
			result <- list(v1 = p1, sep = "-", v2 = p2,
						   W = ".",  p = ".", .isNewGroup = newGroup)
		}
		normalityTests.results[[length(normalityTests.results) + 1]] <- result
	}

	normalityTests[["data"]] <- normalityTests.results
	normalityTests[["footnotes"]] <- as.list(footnotes)
	normalityTests
}


.pairedSamplesTTestDescriptivesPlot <- function(dataset, options, perform) {
	if (!options$descriptivesPlots) return(NULL)

	descriptivesPlotList <- list()
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
		list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
			yend = yend), inherit.aes = FALSE, size = 1), ggplot2::scale_y_continuous(breaks = c(min(b),
			max(b))))
	}

	for (i in .indices(options$pairs)) {
		
		pair <- options$pairs[[i]]
		descriptivesPlot <- list(title = paste(pair, collapse=" - "))
		descriptivesPlot[["width"]] <- options$plotWidth
		descriptivesPlot[["height"]] <- options$plotHeight
		descriptivesPlot[["custom"]] <- list(width = "plotWidth", height = "plotHeight")

		if (perform == "run" && pair[[1]] != "" && pair[[2]] != "") {

			c1 <- dataset[[ .v(pair[[1]]) ]]
			c2 <- dataset[[ .v(pair[[2]]) ]]
			###
			errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
								 all.target = c(pair[[1]], pair[[2]]),
								 observations.amount = '< 2')
			
			if (!identical(errors, FALSE)) {
				errorMessage <- errors$message
				
				descriptivesPlot[["data"]] <- ""
				descriptivesPlot[["error"]] <- list(error="badData", errorMessage=errorMessage)
			} else {

			####
			data <- data.frame(id = rep(1:length(c1), 2), dependent = c(c1, c2),
				groupingVariable = c(rep(paste("1.", pair[[1]], sep = ""), length(c1)),
				  rep(paste("2.", pair[[2]], sep = ""), length(c2))))

			summaryStat <- .summarySEwithin(data, measurevar = "dependent", withinvars = "groupingVariable",
				idvar = "id", conf.interval = options$descriptivesPlotsConfidenceInterval,
				na.rm = TRUE, .drop = FALSE)

			pd <- ggplot2::position_dodge(0.2)

			p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
				y = dependent, group = 1)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,
				ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
				ggplot2::geom_line(position = pd, size = 0.7) + ggplot2::geom_point(position = pd,
				size = 4) + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat) +
				base_breaks_x(summaryStat$groupingVariable) + ggplot2::scale_x_discrete(labels = c(pair[[1]], pair[[2]]))
				
			p <- JASPgraphs::themeJasp(p)

			imgObj <- .writeImage(width = options$plotWidth, 
														height = options$plotHeight, 
														plot = p)

			descriptivesPlot[["data"]] <- imgObj[["png"]]
			descriptivesPlot[["obj"]] <- imgObj[["obj"]]
			
		}

			descriptivesPlot[["convertible"]] <- TRUE
			descriptivesPlot[["status"]] <- "complete"


		}

		descriptivesPlotList[[i]] <- descriptivesPlot
	}

	descriptivesPlotList
}
