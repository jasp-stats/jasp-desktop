
TTestIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- read.dataset.to.end(exclude.na.listwise = options$variables)
			
			} else {
		
				dataset <- read.dataset.to.end()
			}
		

		} else {
		
			dataset <- read.dataset.header()
		}
	}

	results <- list()

	results[["ttest"]] <- .ttestIndependentSamplesTTest(dataset, options, perform)
	results[["descriptives"]] <- .ttestIndependentSamplesDescriptives(dataset, options, perform)
	results[["inequalityOfVariances"]] <- .ttestIndependentSamplesInequalityOfVariances(dataset, options, perform)

	results
}

.ttestIndependentSamplesDescriptives <- function(dataset, options, perform) {

	if (options$descriptives == FALSE)
		return(NULL)
		
	descriptives <- list()

	descriptives[["title"]] <- "Group Statistics"
	
	fields <- list(
		list(name="Variable", title="", type="text", combine=TRUE),
		list(name="groups", type="text"),
		list(name="N", type="number", format="sf:4"),
		list(name="Mean", type="number", format="sf:4"),
		list(name="Std. Deviation", type="number", format="dp:4;p:.001"),
		list(name="Std. Error Mean", type="number", format="sf:4"))

	descriptives[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	for (variable in options[["variables"]]) {
	
		data[[length(data)+1]] <- list(Variable=variable)
		data[[length(data)+1]] <- list(Variable=variable)
	}
	
	if (perform == "run" && options$groupingVariable != "") {
	
		levels <- unique(dataset[[options$groupingVariable]])
		
		if (length(levels) != 2) {
		
			descriptives[["error"]] <- list(errorType="badData")
			
		} else {
		
			rowNo <- 1
			groupingVar <- dataset[[ options$groupingVariable ]]
		
			for (variable in options[["variables"]]) {
		
				for (level in levels) {
			
					variableData <- dataset[[variable]]
				
					groupData <- variableData[groupingVar == level]
					groupDataOm <- na.omit(groupData)

					if (class(groupDataOm) != "factor") {

						n <- .clean(length(groupDataOm))
						mean <- .clean(mean(groupDataOm))
						stdDeviation <- .clean(sd(groupDataOm))
						stdErrorMean <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))
					
						result <- list(Variable=variable, groups=level, N=n, Mean=mean, "Std. Deviation"=stdDeviation,
							"Std. Error Mean"=stdErrorMean)						
							
					} else {
				
						n <- .clean(length(groupDataOm))
						result <- list(Variable=variable, groups="", N=n, Mean="", "Std. Deviation"="", "Std. Error Mean"="")
					}
				
					data[[rowNo]] <- result
					rowNo <- rowNo + 1
				}
			}
		}
	}
	
	descriptives[["data"]] <- data

	descriptives
}

.ttestIndependentSamplesTTest <- function(dataset, options, perform) {

	ttest <- list()

	ttest[["title"]] <- "Independent Samples T-Test"

	fields <- list(
		list(name="Variable", title="", type="text", combine=TRUE),
		list(name="Variance Assumption", type="text"),
		list(name="t", type="number", format="sf:4"),
		list(name="df", type="number", format="sf:4"),
		list(name="p", type="number", format="dp:4;p:.001"))

	if (options$meanDifference) {
	
		fields[[length(fields) + 1]] <- list(name="Mean Difference", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="Std. Error Difference", type="number", format="sf:4")	
	}
	if (options$confidenceInterval) {
	
		fields[[length(fields) + 1]] <- list(name="Lower CI", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="Upper CI", type="number", format="sf:4")
		
		# Footnote informing what confidence interval is printed 
		#CINote <- paste("The Confidence Interval is ",options$confidenceIntervalInterval*100,"%",sep="") 
		#footnotes[[length(footnotes) +1]] <- CINote
	}
		
	ttest[["schema"]] <- list(fields=fields)
	
	ttest.results <- list()
	
	for (variable in options[["variables"]]) {
	
		if (options$equalityOfVariances == "assumeEqual" || options$equalityOfVariances == "both")
			ttest.results[[length(ttest.results)+1]] <- list(Variable=variable, "Variance Assumption"="assumed equal")
		if (options$equalityOfVariances == "assumeUnequal" || options$equalityOfVariances == "both")
			ttest.results[[length(ttest.results)+1]] <- list(Variable=variable, "Variance Assumption"="no assumption")
	}
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		levels <- unique(dataset[[options$groupingVariable]])
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			
		} else {
		
			if (options$tails == "oneTailedGreaterThan") {
		
				testType <- "greater"
				#hypothesisNote <- "Alternative hypothesis: true difference in means is greater than 0"
				#footnotes[[length(footnotes) +1]] <- hypothesisNote
			
			} else if (options$tails == "oneTailedLessThan") {
		
				testType <- "less"
				#hypothesisNote <- "Alternative hypothesis: true difference in means is less than 0"
				#footnotes[[length(footnotes) +1]] <- hypothesisNote
			
			} else {
		
				testType <- "two.sided"
				#hypothesisNote <- "Alternative hypothesis: true difference in means is not equal to 0"
				#footnotes[[length(footnotes) +1]] <- hypothesisNote
			}
		
			if (options$equalityOfVariances == "assumeEqual") {
		
				assume <- c(TRUE)
				assumption=c("assumed equal", NA)
			
			} else if (options$equalityOfVariances == "assumeUnequal") {
		
				assume <- c(FALSE)
				assumption=c("no assumption", NA)
			
			} else {  # assuming both
		
				assume <- c(TRUE, FALSE)
				assumption=c("assumed equal", "no assumption")
			}
		
			groupingVar <- dataset[[options$groupingVariable]]
			footnotes <- list()
			rowNo <- 1
		
			for (variable in options[["variables"]]) {

				variableData <- dataset[[variable]]
			
				for (i in .indices(assume)) {
				
					result <- try (silent=TRUE, expr= {
					
						assume.equal = assume[i]

						r <- t.test(variableData ~ groupingVar, alternative=testType,
									var.equal=assume.equal, conf.level=options$confidenceIntervalInterval)
								
						variance.assumption.violated <- FALSE

						if (assume.equal) {
					
							levene <- car::leveneTest(variableData, groupingVar, "mean")
							
							print(levene)
				
							if (levene[1,3] < .05) {
						
								variance.assumption.violated <- TRUE
								footnotes <- list("Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
							}
						}
									
					
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
					
						if (variance.assumption.violated) {
					
							list(Variable=variable, "Variance Assumption"=assumption[i], t=t, df=df, p=p, "Mean Difference"=m, 
								 "Lower CI"=ciLow, "Upper CI"=ciUp, "Std. Error Difference"=sed, "~footnotes"=list("Variance Assumption"=list(0)))
							 
						} else {
					
							list(Variable=variable, "Variance Assumption" = assumption[i], t=t, df=df, p=p, "Mean Difference"=m, 
								 "Lower CI"=ciLow, "Upper CI"=ciUp, "Std. Error Difference"=sed)					
						}
					})

					if (class(result) == "try-error") {
				
						result <- list(Variable=variable, "Variance Assumption"=assumption[i], t="", df="", p="", "Mean Difference"="",
								"Lower CI"="", "Upper CI"="", "Std. Error Difference"="")
					}
				
					ttest.results[[rowNo]] <- result
					rowNo <- rowNo + 1
				}
			}
		
			ttest[["footnotes"]] <- footnotes
		}
		
	}
	
	ttest[["data"]] <- ttest.results
	
	ttest

}

.ttestIndependentSamplesInequalityOfVariances <- function(dataset, options, perform) {

	if (options$testUnequalVariances == FALSE)
		return(NULL)
		
	levenes <- list()

	levenes[["title"]] <- "Test of Inequality of Variances (Levene's)"
	
	fields <- list(
		list(name="Variable", title="", type="text"),
		list(name="F", type="number", format="sf:4"),
		list(name="df", type="number", format="sf:4"),
		list(name="p", type="number", format="dp:4;p:.001"))

	levenes[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	for (variable in options[["variables"]])
		data[[length(data)+1]] <- list(Variable=variable)
	
	if (perform == "run" && options$groupingVariable != "") {
	
		levels <- unique(dataset[[options$groupingVariable]])
		
		if (length(levels) != 2) {
		
			levenes[["error"]] <- list(errorType="badData")
			
		} else {
	
			rowNo <- 1

			for (variable in options[["variables"]]) {
		
				result <- try (silent=TRUE, expr= {

					levene <- car::leveneTest(dataset[[variable]], dataset[[options$groupingVariable]], "mean")
		
					F  <- .clean(as.numeric(levene[1,1]))
					df <- .clean(as.numeric(levene[1,2]))
					p  <- .clean(as.numeric(levene[1,3]))
		
					list(Variable=variable, F=F, df=df, p=p)
				})

				if (class(result) == "try-error")
					result <- list(Variable=variable, F="", df="", p="")
	
				data[[rowNo]] <- result
				rowNo <- rowNo + 1
			}
		}
	
	}
	
	levenes[["data"]] <- data

	levenes
}


