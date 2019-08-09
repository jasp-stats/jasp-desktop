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

TTestIndependentSamples <- function(jaspResults, dataset = NULL, options, ...) {
  #at least one variable and one grouping variable
  ready <- length(options$variables) > 0 && options$groupingVariable != ""
  
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type = "independent")
    .ttestCheckErrors(        dataset, options, type = "independent")
  }
  # Output tables (each calls its own results function)
  .ttestIndependentMainTable(jaspResults, dataset, options, ready)
  .ttestNormalTable(jaspResults, dataset, options, ready, type = "independent")
  .ttestIndependentEqVarTable(jaspResults, dataset, options, ready)
  # Descriptives
  .ttestDescriptivesTable(jaspResults, dataset, options, ready, type = "independent")
  .ttestIndependentDescriptivesPlot(jaspResults, dataset, options, ready)
  
  return()
}

# Tables
.ttestIndependentMainTable <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["ttest"]])) return()
  
  # Create table
  ttest <- createJaspTable(title = "Independent Samples T-Test")
  dependList <- c("variables", "groupingVariable",
  "effectSize", "effSizeConfidenceIntervalCheckbox", 
  "descriptivesEffectSizeConfidenceIntervalPercent", "effectSizesType",
  "meanDifference", "meanDiffConfidenceIntervalCheckbox", 
  "descriptivesMeanDiffConfidenceIntervalPercent",
  "students", "welchs", "mannWhitneyU",
  "hypothesis", "VovkSellkeMPR")
  ttest$dependOn(dependList)
  ttest$showSpecifiedColumnsOnly <- TRUE
  
  wantsEffect <- options$effectSize
  wantsDifference <- options$meanDifference
  wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox &&  options$meanDifference)
  wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  percentConfidenceMeanDiff <- options$descriptivesMeanDiffConfidenceIntervalPercent
  percentConfidenceEffSize <- options$descriptivesEffectSizeConfidenceIntervalPercent
  ## can make any combination of the following tests:
  wantsWelchs <- options$welchs
  wantsStudents <- options$students
  wantsWilcox <- options$mannWhitneyU
  
  allTests <- c(wantsStudents, wantsWilcox)
  onlyTest <- sum(allTests) == 1
  
  if (wantsWilcox && onlyTest) {
    ttest$addFootnote("Mann-Whitney U test.")
    testStat <- "W"
    #fields <- fields[-3] # Wilcoxon's test doesn't have degrees of freedoms
  } else if (wantsWelchs && onlyTest) {
    ttest$addFootnote("Welch's t-test.")
    testStat <- "t"
  } else if (wantsStudents && onlyTest) {
    ttest$addFootnote("Student's t-test.")
    testStat <- "t"
  } else
    testStat <- "Statistic"
  
  ttest$addColumnInfo(name = "v", title = "", type = "string", combine = TRUE)
  if(!onlyTest)
    ttest$addColumnInfo(name = "test", type = "string", title = "Test")
  ttest$addColumnInfo(name = testStat, type = "number")
  #type integer, not number, correct?
  ttest$addColumnInfo(name = "df",  type = "number")
  ttest$addColumnInfo(name = "p", type = "pvalue")
  .ttestVovkSellke(ttest, options)
  
  if (options$effectSizesType == "cohensD")
    effSize <- "cohen"
  else if (options$effectSizesType == "glassD")
    effSize <- "glass"
  else if (options$effectSizesType == "hedgesG")
    effSize <- "hedges"
  
  nameOfEffectSizeParametric <- switch(effSize, 
                                       cohen = "Cohen's d", 
                                       glass = "Glass' delta",
                                       hedges = "Hedges' g")
  
  if (!wantsWilcox) {
    nameOfLocationParameter <- "Mean Difference"
    nameOfEffectSize        <- nameOfEffectSizeParametric
  } else if (wantsWilcox && onlyTest) {
    nameOfLocationParameter <- "Hodges-Lehmann Estimate"
    nameOfEffectSize        <- "Rank-Biserial Correlation"
  } else if (wantsWilcox && (wantsStudents || wantsWelchs)) {
    nameOfLocationParameter <-  "Location Parameter"
    nameOfEffectSize        <-  "Effect Size"
  }
  
  ## add mean difference and standard error difference
  if (wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")
    if (!(wantsWilcox && onlyTest))  # Only add SE Difference if not only MannWhitney is requested
      ttest$addColumnInfo(name = "sed", title = "SE Difference", type = "number")
  }
  
  if (wantsDifference && wantsWilcox && wantsStudents && wantsWelchs) {
    message <- "For the Student t-test and Welch t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test, 
                location parameter is given by the Hodges-Lehmann estimate."
    ttest$addFootnote(message)
  } else if (wantsDifference && wantsWilcox && wantsStudents) {
    message <- "For the Student t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test, 
                location parameter is given by Hodges-Lehmann estimate."
    ttest$addFootnote(message)
  } else if (wantsDifference && wantsWilcox && wantsWelchs) {
    message <- "For the Welch t-test, 
                location parameter is given by mean difference; for the Mann-Whitney test,
                location parameter is given by Hodges-Lehmann estimate."
    ttest$addFootnote(message)
  }
  
  if (wantsConfidenceMeanDiff) {
    interval <- 100 * percentConfidenceMeanDiff
    title <- paste0(interval, "% CI for ", nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number",
                                            title = "Lower", overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number",
                                            title = "Upper", overtitle = title)
  }
  
  ## add Cohen's d
  if (wantsEffect) {
    ttest$addColumnInfo(name = "d", title = nameOfEffectSize, type = "number")
    if (wantsWilcox) {
      if (wantsStudents || wantsWelchs) 
        message <- paste0("For the Mann-Whitney test, effect size is given by the rank biserial correlation. 
                           For the other test(s), by ", nameOfEffectSizeParametric, ".")
      else
        message <- "For the Mann-Whitney test, effect size is given by the rank biserial correlation."
      ttest$addFootnote(message)
    }
  }
  
  if (wantsConfidenceEffSize) {
    interval <- 100 * percentConfidenceEffSize
    title <- paste0(interval, "% CI for ", nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number",
                                            title = "Lower", overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number",
                                            title = "Upper", overtitle = title)
  }
  jaspResults[["ttest"]] <- ttest
  
  res <- try(.ttestIndependentMainFill(jaspResults, dataset, options, ready, testStat))
  .ttestSetError(res, ttest)
}

.ttestIndependentEqVarTable <- function(jaspResults, dataset, options, ready){
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$equalityOfVariancesTests || !is.null(container[["equalityVariance"]])) 
    return()
  # Create table
  equalityVariance <- createJaspTable(title = "Test of Equality of Variances (Levene's)")
  #dependList <- c()
  #ttestMainTable$dependOn(dependList)
  equalityVariance$showSpecifiedColumnsOnly <- TRUE
  equalityVariance$addColumnInfo(name = "variable", type = "string", title = "")
  equalityVariance$addColumnInfo(name = "F",  type = "number")
  equalityVariance$addColumnInfo(name = "df", type = "integer")
  equalityVariance$addColumnInfo(name = "p",  type = "pvalue")
  
  container[["equalityVariance"]] <- equalityVariance
  res <- try(.ttestIndependentEqVarFill(container, dataset, options, ready))
  .ttestSetError(res, equalityVariance)
}

# Table fill
.ttestIndependentMainFill <- function(jaspResults, dataset, options, ready, testStat) {
  wantsEffect <- options$effectSize
  wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  percentConfidenceMeanDiff <- options$descriptivesMeanDiffConfidenceIntervalPercent
  percentConfidenceEffSize <- options$descriptivesEffectSizeConfidenceIntervalPercent
  ## can make any combination of the following tests:
  wantsWelchs <- options$welchs
  wantsStudents <- options$students
  wantsWilcox <- options$mannWhitneyU
  whichTests <- list("1" = wantsStudents, "2" = wantsWelchs, "3" = wantsWilcox)
  
  if (options$effectSizesType == "cohensD")
    effSize <- "cohen"
  else if (options$effectSizesType == "glassD")
    effSize <- "glass"
  else if (options$effectSizesType == "hedgesG")
    effSize <- "hedges"
  
  nameOfEffectSizeParametric <- switch(effSize, 
                                       cohen  = "Cohen's d", 
                                       glass  = "Glass' delta",
                                       hedges = "Hedges' g")
  
  ttest.rows <- list()
	variables  <- options$variables
	if (length(variables) == 0) 
	  variables <- "."

	## add a row for each variable, even before we are conducting tests
	#for (variable in variables) 
#		ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)

	if (ready) {
		levels <- levels(dataset[[ .v(options$groupingVariable) ]])

		msgStart <- "For all tests, the alternative hypothesis specifies that group <em>"
		if (options$hypothesis == "groupOneGreater") {
			direction    <- "greater"
			msgDirection <- "</em> is greater than group <em>"
		} else if (options$hypothesis == "groupTwoGreater") {
			direction    <- "less"
			msgDirection <- "</em> is less than group <em>"
		} else {
		  direction    <- "two.sided"
		  msgDirection <- "</em> is not equal to group <em>"
		}
		message <- paste0(msgStart, levels[1], msgDirection, levels[2], "</em>.")
		jaspResults[["ttest"]]$addFootnote(message)
			
		#whichTests <- list("1" = wantsStudents, "2" = wantsWelchs, "3" = wantsWilcox)
		groupingData <- dataset[[ .v(options$groupingVariable) ]]

		## for each variable specified, run each test that the user wants
		for (variable in options$variables) {
			variableData <- dataset[[ .v(variable) ]]

			## test is a number, indicating which tests should be run
			for (test in seq_len(length(whichTests))) {

				currentTest <- whichTests[[test]]

				## don't run a test the user doesn't want
				if (!currentTest)
					next

				## try to run the test, catching eventual errors
				row <- try(silent = FALSE, expr = {
					ciEffSize  <- percentConfidenceEffSize
					ciMeanDiff <- percentConfidenceMeanDiff
					f <- as.formula(paste(.v(variable), "~",
																.v(options$groupingVariable)))

					y <- dataset[[ .v(variable) ]]
					groups <- dataset[[ .v(options$groupingVariable) ]]

					sds <- tapply(y, groups, sd, na.rm = TRUE)
					ms  <- tapply(y, groups, mean, na.rm = TRUE)
					ns  <- tapply(y, groups, function(x) length(na.omit(x)))


					if (test == 3) {
						whatTest <- "Mann-Whitney"
						r <- stats::wilcox.test(f, data = dataset,
																		alternative = direction,
																		conf.int = TRUE, conf.level = ciMeanDiff, paired = FALSE)
						df   <- ""
						sed  <- ""
						stat <- as.numeric(r$statistic)
						m    <- as.numeric(r$estimate)
						d    <- abs(as.numeric(1-(2*stat)/(ns[1]*ns[2]))) * sign(m)
						# rankBis <- 1 - (2*stat)/(ns[1]*ns[2])
						wSE <- sqrt((ns[1]*ns[2] * (ns[1]+ns[2] + 1))/12)
						rankBisSE <- sqrt(4 * 1/(ns[1]*ns[2])^2 * wSE^2)
						zRankBis  <- atanh(d)
						if(direction == "two.sided")
							confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize)/2)*rankBisSE), 
							                         tanh(zRankBis + qnorm((1+ciEffSize)/2)*rankBisSE)))
						else if (direction == "less")
							confIntEffSize <- sort(c(-Inf, tanh(zRankBis + qnorm(ciEffSize)*rankBisSE)))
						else if (direction == "greater")
							confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize))*rankBisSE), Inf))
					} else {
						whatTest <- ifelse(test == 2, "Welch", "Student")
						r <- stats::t.test(f, data = dataset, alternative = direction,
															 var.equal = test != 2, conf.level = ciMeanDiff, paired = FALSE)

						df   <- as.numeric(r$parameter)
						m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
						stat <- as.numeric(r$statistic)

						num <-  (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
						sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
						if (test == 2)  # Use different SE when using Welch T test!
							sdPooled <- sqrt(((sds[1]^2) + (sds[2]^2)) / 2)

						d <- "."
						if (wantsEffect) {
							# Sources are https://en.wikipedia.org/wiki/Effect_size for now.
							if (options$effectSizesType == "cohensD")
								d <- as.numeric((ms[1] - ms[2]) / sdPooled)
							else if (options$effectSizesType == "glassD")
								d <- as.numeric((ms[1] - ms[2]) / sds[2])
								# Should give feedback on which data is considered 2.
							else if (options$effectSizesType == "hedgesG") {
								a <- sum(ns) - 2
								logCorrection <- lgamma(a / 2) - (log(sqrt(a / 2)) + lgamma((a - 1) / 2))
								d <- as.numeric((ms[1] - ms[2]) / sdPooled) * exp(logCorrection) # less biased / corrected version
							}
						}
						sed <- (as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])) / stat
						confIntEffSize <- c(0,0)

						if (wantsConfidenceEffSize){
							# From MBESS package by Ken Kelley, v4.6
							dfEffSize <-  ifelse(effSize == "glass", ns[2] - 1, df)
							alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
							confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt((prod(ns)) / (sum(ns))), 
							                                               df = dfEffSize, alpha.lower = alphaLevel, 
							                                               alpha.upper = alphaLevel)[c(1, 3)]
							confIntEffSize <- unlist(confIntEffSize) * sqrt((sum(ns)) / (prod(ns)))
							
							if (direction == "greater")
								confIntEffSize[2] <- Inf
							else if (direction == "less")
								confIntEffSize[1] <- -Inf

							confIntEffSize <- sort(confIntEffSize)
						}
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
					p     <- as.numeric(r$p.value)
					ciLow <- r$conf.int[1]
					ciUp  <- r$conf.int[2]
					lowerCIeffectSize <- as.numeric(confIntEffSize[1])
					upperCIeffectSize <- as.numeric(confIntEffSize[2])
					# this will be the results object
					res <- list(v = variable, test = whatTest, df = df, p = p, md = m, d = d, 
											lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
											lowerCIeffectSize = lowerCIeffectSize, upperCIeffectSize = upperCIeffectSize,
											sed = sed)
					res[[testStat]] <- stat
					if (options$VovkSellkeMPR)
						res[["VovkSellkeMPR"]] <- .VovkSellkeMPR(p)
					res
				})

				ttest.rows[[length(ttest.rows) + 1]] <- row
			}
		}

		if (effSize == "glass") {
			sdMessage <- paste0("Glass' delta uses the standard deviation of group ", names(ns[2]),
										 " of variable ", options$groupingVariable, ".")
			jaspResults[["ttest"]]$addFootnote(sdMessage)
		}
	}
	else
	  for (variable in variables) 
	    ttest.rows[[length(ttest.rows) + 1]] <- list(v = variable)
	jaspResults[["ttest"]]$addRows(ttest.rows)
}

.ttestIndependentEqVarFill <- function(container, dataset, options, ready){

	data <- list()
	variables <- options$variables
	groups <- options$groupingVariable
	if (length(variables) == 0) variables <- "."

	for (variable in variables)
		data[[length(data) + 1]] <- list(variable = variable)

	if (groups != "") {

		levels <- levels(dataset[[ .v(groups) ]])

		for (variable in variables) {

			result <- try(silent = TRUE, expr = {

				levene <- car::leveneTest(dataset[[ .v(variable) ]],
																	dataset[[ .v(groups) ]], "mean")

				F  <- levene[1, "F value"]
				df <- levene[1, "Df"]
				p  <- levene[1, "Pr(>F)"]

				row <- list(variable = variable, F = F, df = df, p = p)

				if (is.na(levene[1, "F value"])) {
					note <- "F-statistic could not be calculated"
					container[["equalityVariance"]]$addFootnote(note)
				}
				row
			})

			if (isTryError(result))
				result <- list(variable = variable, F = "", df = "", p = "")

			container[["equalityVariance"]]$addRows(result)
		}
	}
}

# Plot
.ttestIndependentDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(variable in options$variables) {
    title <- variable
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[title]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestIndependentDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesPlotFill <- function(dataset, options, variable) {

	groups <- options$groupingVariable

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
		list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,xend = xend,
																											yend = yend), inherit.aes = FALSE, size = 1),
				 ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
	}
	
	dataset <- na.omit(dataset)
	ci <- options$descriptivesPlotsConfidenceInterval
	summaryStat <- .summarySE(as.data.frame(dataset), 
	                          measurevar = .v(variable),
														groupvars = .v(groups), 
														conf.interval = ci, na.rm = TRUE, .drop = FALSE)
	
	colnames(summaryStat)[which(colnames(summaryStat) == .v(variable))] <- "dependent"
	colnames(summaryStat)[which(colnames(summaryStat) == .v(groups))] <- "groupingVariable"
	
	pd <- ggplot2::position_dodge(0.2)
	
	p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
																								 y = dependent, group = 1)) + 
	  ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), 
	                         colour = "black", width = 0.2, position = pd) +
		ggplot2::geom_line(position = pd, size = 0.7) + 
	  ggplot2::geom_point(position = pd, size = 4) + 
	  ggplot2::ylab(unlist(variable)) + 
	  ggplot2::xlab(options$groupingVariable) +
		base_breaks_y(summaryStat) + base_breaks_x(summaryStat$groupingVariable)
  
	p <- JASPgraphs::themeJasp(p)
	
	return(p)
}

.confidenceLimitsEffectSizes <- function(ncp, df, conf.level=.95, alpha.lower=NULL, 
                                         alpha.upper=NULL, t.value, tol=1e-9, ...) {
	# This function comes from the MBESS package, version 4.6, by Ken Kelley
	# https://cran.r-project.org/web/packages/MBESS/index.html
	# Note this function is new in version 4, replacing what was used in prior versions.
	# Internal functions for the noncentral t distribution; two appraoches.
	###########


	# General stop checks.
	if(!is.null(conf.level) && is.null(alpha.lower) && is.null(alpha.upper)) {
		alpha.lower <- (1 - conf.level) / 2
		alpha.upper <- (1 - conf.level) / 2
	}

	.conf.limits.nct.M1 <- function(ncp, df, conf.level=NULL, alpha.lower, alpha.upper, tol=1e-9, ...) {

		min.ncp <- min(-150, -5 * ncp)
		max.ncp <- max(150, 5 * ncp)

		# Internal function for upper limit.
		# Note the upper tail is used here, as we seek to find the NCP that has, in its upper tail (alpha.lower, 
		# for the lower limit), the specified value of the observed t/ncp.
		###########################
		
		.ci.nct.lower <- function(val.of.interest, ...)
			(qt(p=alpha.lower, df=df, ncp=val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
		###########################

		# Internal function for lower limit.
		# Note the lower tail is used here, as we seek to find the NCP that has, in its lower tail (alpha.upper, 
		# for the upper limit), the specified value of the observed t/ncp.
		###########################
		.ci.nct.upper <- function(val.of.interest, ...)
			(qt(p=alpha.upper, df=df, ncp=val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
		
		if(alpha.lower != 0) 
		  Low.Lim <- suppressWarnings(optimize(f=.ci.nct.lower, interval=c(min.ncp, max.ncp),
		                                       alpha.lower=alpha.lower, df=df, ncp=ncp, 
			                                     maximize=FALSE, tol=tol))
		
		if(alpha.upper != 0) {
			Up.Lim <- suppressWarnings(optimize(f=.ci.nct.upper, interval=c(min.ncp, max.ncp), 
			                                    alpha.upper=alpha.upper, df=df, ncp=ncp, 
			                                    maximize=FALSE, tol=tol))
		}

		if(alpha.lower == 0) 
		  Result <- list(Lower.Limit=-Inf, Prob.Less.Lower=0, Upper.Limit=Up.Lim$minimum, 
		                 Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$minimum, df=df))
		if(alpha.upper == 0) 
		  Result <- list(Lower.Limit = Low.Lim$minimum, 
		                 Prob.Less.Lower = pt(q=ncp, ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
		                 Upper.Limit = Inf, Prob.Greater.Upper = 0)
		if(alpha.lower != 0 && alpha.upper != 0) 
		  Result <- list(Lower.Limit=Low.Lim$minimum, 
		                 Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$minimum, df=df, lower.tail=FALSE), 
		                 Upper.Limit=Up.Lim$minimum, 
		                 Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$minimum, df=df))

		return(Result)
	}
	################################################
	.conf.limits.nct.M2 <- function(ncp, df, conf.level=NULL, alpha.lower, alpha.upper, tol=1e-9, ...) {

		# Internal function for upper limit.
		###########################
		.ci.nct.lower <- function(val.of.interest, ...)
			(qt(p = alpha.lower, df = df, ncp = val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
		
		# Internal function for lower limit.
		###########################
		.ci.nct.upper <- function(val.of.interest, ...)
			(qt(p = alpha.upper, df = df, ncp = val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
		
		Low.Lim <- suppressWarnings(nlm(f = .ci.nct.lower, p = ncp, ...))
		Up.Lim  <- suppressWarnings(nlm(f = .ci.nct.upper, p = ncp, ...))

		if(alpha.lower == 0) 
		  Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, Upper.Limit = Up.Lim$estimate, 
		                 Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$estimate, df = df))
		if(alpha.upper == 0) 
		  Result <- list(Lower.Limit=Low.Lim$estimate, 
		                 Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$estimate, df=df, lower.tail=FALSE), 
		                 Upper.Limit=Inf, Prob.Greater.Upper=0)
		if(alpha.lower!=0 & alpha.upper!=0) 
		  Result <- list(Lower.Limit=Low.Lim$estimate, 
		                 Prob.Less.Lower=pt(q=ncp, ncp=Low.Lim$estimate, df=df, lower.tail=FALSE), 
		                 Upper.Limit=Up.Lim$estimate, 
		                 Prob.Greater.Upper=pt(q=ncp, ncp=Up.Lim$estimate, df=df))

		return(Result)
	}

	# Now, use the each of the two methods.
	Res.M1 <- Res.M2 <- NULL
	try(Res.M1 <- .conf.limits.nct.M1(ncp=ncp, df=df, conf.level=NULL, 
	                                  alpha.lower=alpha.lower, 
	                                  alpha.upper=alpha.upper, tol=tol), silent=TRUE)
	if(length(Res.M1)!=4) 
	  Res.M1 <- NULL

	try(Res.M2 <- .conf.limits.nct.M2(ncp=ncp, df=df, conf.level=NULL, 
	                                  alpha.lower=alpha.lower,
	                                  alpha.upper=alpha.upper, tol=tol), silent=TRUE)
	if(length(Res.M2)!=4) 
	  Res.M2 <- NULL

	# Now, set-up the test to find the best method.
	Low.M1        <- Res.M1$Lower.Limit
	Prob.Low.M1   <- Res.M1$Prob.Less.Lower
	Upper.M1      <- Res.M1$Upper.Limit
	Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper

	Low.M2        <- Res.M2$Lower.Limit
	Prob.Low.M2   <- Res.M2$Prob.Less.Lower
	Upper.M2      <- Res.M2$Upper.Limit
	Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper

	# Choose the best interval limits:
	##Here low
	Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2)-alpha.lower)^2)

	if(!is.null(Res.M1))
	  if(Min.for.Best.Low == (Prob.Low.M1-alpha.lower)^2) 
	    Best.Low <- 1
	if(!is.null(Res.M2))
	  if(Min.for.Best.Low == (Prob.Low.M2-alpha.lower)^2) 
	    Best.Low <- 2

	##Here high
	Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2)-alpha.upper)^2)

	if(!is.null(Res.M1))
	  if(Min.for.Best.Up == (Prob.Upper.M1-alpha.upper)^2) 
	    Best.Up <- 1
	if(!is.null(Res.M2))
	  if(Min.for.Best.Up == (Prob.Upper.M2-alpha.upper)^2) 
	    Best.Up <- 2
	#####################################

	if(is.null(Res.M1)) 
	  Low.M1 <- Prob.Low.M1 <- Upper.M1 <- Prob.Upper.M1 <- NA
	if(is.null(Res.M2)) 
	  Low.M2 <- Prob.Low.M2 <- Upper.M2 <- Prob.Upper.M2 <- NA

	Result <- list(Lower.Limit = c(Low.M1, Low.M2)[Best.Low], 
	               Prob.Less.Lower = c(Prob.Low.M1, Prob.Low.M2)[Best.Low], 
	               Upper.Limit = c(Upper.M1, Upper.M2)[Best.Up], 
	               Prob.Greater.Upper = c(Prob.Upper.M1, Prob.Upper.M2)[Best.Up])

	return(Result)
}
