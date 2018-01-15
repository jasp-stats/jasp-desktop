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
	wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
	wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
	percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
	percentConfidenceEffSize <- options$effSizeConfidenceIntervalPercent
	wantsStudents <- options$students
	wantsWilcox <- options$mannWhitneyU
	wantsZtest <- options$zTest

	allTests <- c(wantsStudents, wantsWilcox, wantsZtest)
	onlyTest <- sum(allTests) == 1

	fields <- list(list(name = "v", type = "string", title = ""),
				   list(name = "df", type = "integer"),
				   list(name = "p", type = "number", format = "dp:3;p:.001"))

	footnotes <- .newFootnotes()
	title <- "One Sample T-Test"

	## get the right title and test statistic for the table
	if (wantsWilcox && onlyTest) {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = "Wilcoxon signed-rank test.")
		testStat <- "V"
		# potentially dangerous next line - removing df. Not posssible to remove by name
		fields <- fields[-2] #Wilcoxon's test doesn't have degrees of freedoms
		nameOfLocationParameter <- "Hodges-Lehmann Estimate"
		nameOfEffectSize <- "Rank-Biserial Correlation"
	} else if (wantsStudents && onlyTest) {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = "Student's t-test.")
		testStat <- "t"
		nameOfLocationParameter <- "Mean Difference"
		nameOfEffectSize <- "Cohen's d"
	} else if(wantsZtest && onlyTest){
	  .addFootnote(footnotes, symbol = "<em>Note.</em>", text = "Z test.")
	  testStat <- "Z"
	  # potentially dangerous next line - removing df. Not posssible to remove by name
	  fields <- fields[-2] #Z test doesn't have degrees of freedoms
	  nameOfLocationParameter <- "Mean Difference"
	  nameOfEffectSize <- "Cohen's d"
	} else {
		testStat <- "Statistic"
		nameOfLocationParameter <-  "Location Parameter"
		nameOfEffectSize <-  "Effect Size"
	}

	ttest[["title"]] <- title

	## if only conducting Student's, the table should have "t" as column name
	## for the test statistic when doing only Wilcoxon's, the name should be
	## "V", when doing only Z-test, it should be "Z"; 
	## when doing at least two of them, it should be "statistic"
	fields <- append(fields, list(list(name = testStat,
								  type = "number", format = "sf:4;dp:3")), 1)

	## if the user wants more than one tests, add a column called "Test"
	if (sum(allTests) > 1) {
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
		fields[[length(fields) + 1]] <- list(name = "m", title = nameOfLocationParameter,
											 type = "number", format = "sf:4;dp:3")
		
		# preparing footnote - paste if selected
		textDifference <- ""
		if(wantsStudents){
		  textDifference <- "For the Student t-test, location parameter is given by mean difference <em>d</em>"
		}
		if(wantsWilcox){
		  if(wantsStudents){
		    textDifference <- paste0(textDifference, 
		                             "; for the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate")
		  } else{
		    textDifference <- "For the Wilcoxon test, location parameter is given by the Hodges-Lehmann estimate"
		  }
		}
		if(wantsZtest){
		  if(onlyTest){
		    textDifference <- "For the Z-test, location parameter is given by mean difference <em>d</em>"
		  } else{
		    textDifference <- paste0(textDifference, 
		                          "; for the Z-test, location parameter is given by mean difference <em>d</em>")
		  }
		}
		textDifference <- paste0(textDifference, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = textDifference)
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
		
		# preparing footnote - paste if selected
		textEffect <- ""
		if(wantsStudents){
		  textEffect <- "For the Student t-test, effect size is given by Cohen's <em>d</em>"
		}
		
		if(wantsWilcox){
		  if(wantsStudents){
		    textEffect <- paste0(textEffect,
		                         "; for the Wilcoxon test, effect size is given by the matched rank biserial correlation")
		  } else{
		    textEffect <- "For the Wilcoxon test, effect size is given by the matched rank biserial correlation"
		  }
		}
		
		if(wantsZtest){
		  if(onlyTest){
		    textEffect <- "For the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)"
		  } else{
		    textEffect <- paste0(textEffect,
		                         "; for the Z test, effect size is given by Cohen's <em>d</em> (based on the provided population standard deviation)")
		  }
		}
		
		textEffect <- paste0(textEffect, ".")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = textEffect)
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
	whichTests <- list("1" = wantsStudents, "2" = wantsWilcox, "3" = wantsZtest)

	## add a row for each variable, even before we are conducting tests
	for (variable in variables) {
		ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
	}

	for (variable in variables) {

		errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
							 all.target = variable,
							 observations.amount = '< 2')

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

			if (perform == "run" && length(options$variables) > 0) {

				row <- try(silent = TRUE, expr = {

					dat <- na.omit(dataset[[ .v(variable) ]])
          n <- length(dat)
					if (test == 2) {
						r <- stats::wilcox.test(dat, alternative = direction,
												mu = options$testValue,
												conf.level = percentConfidenceMeanDiff, conf.int = TRUE)
						df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
						maxw <- (n*(n+1))/2
						d <- as.numeric((r$statistic/maxw) * 2 - 1)
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
					} else if(test == 3){
					  r <- BSDA::z.test(dat, alternative = direction,
					                    mu = options$testValue, sigma.x = options$stddev,
					                    conf.level = percentConfidenceMeanDiff)
					  df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
					  d <- .clean((mean(dat) - options$testValue) / options$stddev)
					  
					  if(direction == "less"){
					    r$conf.int[1] <- -Inf
					  } else if(direction == "greater"){
					    r$conf.int[2] <- Inf
					  }
					  Z <- as.numeric(r$statistic)
					  confIntEffSize <- c(0,0)
					  if(wantsConfidenceEffSize){
					    
				      confIntEffSize <- r$conf.int/options$stddev
					  }
					} else {
						r <- stats::t.test(dat, alternative = direction,
										   mu = options$testValue, conf.level = percentConfidenceMeanDiff)
						df <- ifelse(is.null(r$parameter), "", as.numeric(r$parameter))
						d <- .clean((mean(dat) - options$testValue) / sd(dat))
						t <- as.numeric(r$statistic)
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

					## same for all tests
					p <- as.numeric(r$p.value)
					stat <- as.numeric(r$statistic)
					m <- as.numeric(r$estimate - r$null.value)
					ciLow <- .clean(as.numeric(r$conf.int[1] - r$null.value))
					ciUp <- .clean(as.numeric(r$conf.int[2] - r$null.value))
          ciLowEffSize = .clean(as.numeric(confIntEffSize[1]))
          ciUpEffSize = .clean(as.numeric(confIntEffSize[2]))
					if (suppressWarnings(is.na(t))) { # do not throw warning when test stat is not 't' 
						stop("data are essentially constant")
					}

					res <- list(v = variable, df = df, p = p,
								m = m, d = d, lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp, 
								lowerCIeffectSize = ciLowEffSize, upperCIeffectSize = ciUpEffSize)
					res[[testStat]] <- stat
					if (options$VovkSellkeMPR){
					  res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
					}
					res
				})
				
				if (isTryError(row) && is.null(errorMessage)) {
				    errorMessage <- .extractErrorMessage(row)
				}

				## if there has been an error, find out which and log as a footnote

				if (!is.null(errorMessage)) {

				index <- .addFootnote(footnotes, errorMessage)
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

      # if we have multiple tests, we want only variable name at the first row;
			# and having additional column indicating the name of the tests
			allTestNames <- c("Student", "Wilcoxon", "Z")
			testName <- allTestNames[test]
      row[["test"]] <- testName
			
      firstSelectedTest <- allTestNames[unlist(whichTests)][1]
			#if(rowNo %% 2 == 0 || rowNo %% 3 == 0){
      if(testName != firstSelectedTest){
        row[["v"]] <- ""
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
			
			errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
			                     all.target = variable,
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
	
	if (perform == "run" && length(options$variables) > 0) {
	    
	    
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
		#### new error handling
		errors <- .hasErrors(dataset, perform, message = 'short', type = c('observations', 'variance', 'infinity'),
							 all.target = var,
							 observations.amount = '< 2')
		
		if (!identical(errors, FALSE)) {
			errorMessage <- errors$message
			
			descriptivesPlot[["error"]] <- list(error="badData", errorMessage=errorMessage)
			descriptivesPlot[["data"]] <- ""
		} else {
		
		descriptivesPlot[["width"]] <- options$plotWidth
		descriptivesPlot[["height"]] <- options$plotHeight
		descriptivesPlot[["custom"]] <- list(width = "plotWidth", height = "plotHeight")

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
				base_breaks_y(summaryStat, options) 
			
			p <- JASPgraphs::themeJasp(p) + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

			imgObj <- .writeImage(width = options$plotWidth, 
														height = options$plotHeight, 
														plot = p)

			descriptivesPlot[["data"]] <- imgObj[["png"]]
			descriptivesPlot[["obj"]] <- imgObj[["obj"]]
			
	}
	
			descriptivesPlot[["convertible"]] <- TRUE
			descriptivesPlot[["status"]] <- "complete"
			
			descriptivesPlotList[[i]] <- descriptivesPlot
			
	}
	
	return(descriptivesPlotList)
		
	} else {
	    
	    return(NULL)
	    
	}
}
