
TTestIndependentSamples <- function(dataset, options, perform="run", callback=NULL) {

	results <- list()
	ttest <- list()
	descriptives <- list()
	footnotes <- list()

	ttest[["title"]] <- "Independent Samples T-Test"

	fields <- list(
		list(id="Variance Assumption", type="text"),
		list(id="t", type="number", format="sf:4"),
		list(id="df", type="number", format="sf:4"),
		list(id="p", type="number", format="dp:4;p:.001"))

	if (options$meanDifference) {
		fields[[length(fields) + 1]] <- list(id="Mean Difference", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(id="Std. Error Difference", type="number", format="sf:4")	
	}
	if (options$confidenceInterval) {
		fields[[length(fields) + 1]] <- list(id="Lower CI", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(id="Upper CI", type="number", format="sf:4")
		
		# Footnote informing what confidence interval is printed 
		CINote <- paste("The Confidence Interval is ",options$confidenceIntervalInterval*100,"%",sep="") 
		footnotes[[length(footnotes) +1]] <- CINote
	}
		
	ttest[["schema"]] <- list(fields=fields)
	
	if (perform == "run") {	
		
		ttest.results <- list()
		
		na <- FALSE
		if (options$missingValues == "excludeListwise") {
			for (variable in options[["variables"]]) {
				na <- is.na(dataset[[variable]]) | na
			}
		}	
		
		if (options$tails == "oneTailedGreaterThan") {
			testType <- "greater"
			hypothesisNote <- "Alternative hypothesis: true difference in means is greater than 0"
			footnotes[[length(footnotes) +1]] <- hypothesisNote
		} else if (options$tails == "oneTailedLessThan") {
			testType <- "less"
			hypothesisNote <- "Alternative hypothesis: true difference in means is less than 0"
			footnotes[[length(footnotes) +1]] <- hypothesisNote
		} else {
			testType <- "two.sided"
			hypothesisNote <- "Alternative hypothesis: true difference in means is not equal to 0"
			footnotes[[length(footnotes) +1]] <- hypothesisNote
		}
		
		if (options$equalityOfVariances == "assumeEqual") { 
			assume <- c(TRUE, NA)
			assumption=c("equal", NA)
			repet <- 1
		} else if (options$equalityOfVariances == "assumeUnequal") {
			assume <- c(FALSE, NA)
			assumption=c("unequal", NA)
			repet <- 1
		} else {  # assuming both
			assume <- c(TRUE, FALSE)
			assumption=c("equal", "unequal")
			repet <- 2
		}
		
		cases <- list()
		groupingVar <- dataset[[options$groupingVariable]][!na] 
		
		for (variable in options[["variables"]]) {

			variableData <- dataset[[variable]][!na]
			
			result <- try (silent=TRUE, expr= {
				levene <- car::leveneTest(variableData, groupingVar, "mean")
				
				if (levene[1,3] > .05) {
					LeveneNote <- "Levene's Test suggests variances being equal"
				} else {
					LeveneNote <- "Levene's Test suggests that variances are NOT equal"
				}
				footnotes[[length(footnotes) +1]] <- LeveneNote
			
			})
			
			for (rep in 1:repet) {
				
				cases[[length(cases) + 1]] <- variable				
				
				result <- try (silent=TRUE, expr= {
					
		
					r <- t.test(variableData ~ groupingVar, alternative=testType,
								var.equal=assume[rep], conf.level=options$confidenceIntervalInterval)
									
					
					t <- as.numeric(r$statistic)
					df <- as.numeric(r$parameter)
					p <- as.numeric(r$p.value)
					m <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
					ciLow <- .clean(r$conf.int[1])
					ciUp <- .clean(r$conf.int[2])
					if (testType == "two.sided") {
						sed <- .clean((ciUp - ciLow) / (2 * qt(options$confidenceIntervalInterval,r$parameter)))  # beckward approach - getting spread of CI and deviding by critical value for t
					} else if (testType == "less") {
						sed <- "???"# .clean((ciUp - m) / (qt(options$confidenceIntervalInterval,r$parameter)))
					} else {
						sed <- "???"# .clean((m - ciLow) / (qt(options$confidenceIntervalInterval,r$parameter)))
					}
					
					list("Variance Assumption" = assumption[rep], t=t, df=df, p=p, "Mean Difference"=m, 
						 "Lower CI"=ciLow, "Upper CI"=ciUp, "Std. Error Difference"=sed)
				})

				if (class(result) == "try-error") {
					result <- list("Variance Assumption" = "", t="", df="", p="", "Mean Difference"="",
								   "Lower CI"="", "Upper CI"="", "Std. Error Difference"="")
				}
				ttest.results[[length(ttest.results)+1]] <- result
			}
		}
		ttest[["cases"]] <- cases
		
		if (options$descriptives) {
			
			cases <- list()
			for (i in seq(length(options$variables))) {  # This could be done better I guess, also for nominal variables we would prefere just one (empty) line instead 
				for (x in 1:2) {
					cases[[length(cases) + 1]] <- options$variables[i]
				}
			}

			fields <- list(
				list(id="groups", type="text"),
				list(id="N", type="number", format="sf:4"),
				list(id="Mean", type="number", format="sf:4"),
				list(id="Std. Deviation", type="number", format="dp:4;p:.001"),
				list(id="Std. Error Mean", type="number", format="sf:4"))

			descriptives[["title"]] <- "Group Statistics"	
			descriptives[["cases"]] <- cases	
			descriptives[["schema"]] <- list(fields=fields)
			
			descriptives.results <- list()
			
			for (variable in options[["variables"]]) {
				for (group in 1:2) {
					variableData <- dataset[[variable]][!na]  # Listwise NA reduction 
					
					level <- levels(groupingVar)[group]  # deviding variable of interest into 2 groups
					groupData <- variableData[groupingVar == level]
					groupDataOm <- na.omit(groupData)

					if (class(groupDataOm) != "factor") {

						n <- .clean(length(groupDataOm))
						mean <- .clean(mean(groupDataOm))
						stdDeviation <- .clean(sd(groupDataOm))
						stdErrorMean <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))
						
						result <- list(groups=level, N=n, Mean=mean, "Std. Deviation"=stdDeviation,
										"Std. Error Mean"=stdErrorMean)						
					} else {
						n <- .clean(length(groupDataOm))
						result <- list(groups="", N=n, Mean="", "Std. Deviation"="", "Std. Error Mean"="")
					}
					descriptives.results[[length(descriptives.results) + 1]] <- result
				}
			}
			
			descriptives[["data"]] <- descriptives.results
		}
		
		ttest[["data"]] <- ttest.results
		ttest[["footnotes"]] <- footnotes
	}

	results[["ttest"]] <- ttest
	results[["inequalityOfVariances"]] <- NULL  # levene's in a separate table
	results[["descriptives"]] <- descriptives

	results
}

