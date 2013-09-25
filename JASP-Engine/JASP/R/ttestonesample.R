
TTestOneSample <- function(dataset, options, perform="run", callback=NULL) {

	results <- list()

	ttest <- list()
	descriptives <- list()
	footnotes <- list()
	
	ttest[["title"]] <- "One Sample T-Test"
	ttest[["cases"]] <- I(options$variables)

	fields <- list(
		list(id="t", type="number", format="sf:4"),
		list(id="df", type="number", format="sf:4"),
		list(id="p", type="number", format="dp:4;p:.001"))
		
	if (options$meanDifference) {
		fields[[length(fields) + 1]] <- list(id="Mean Difference", type="number", format="sf:4")
	}
	if (options$confidenceInterval) {
		fields[[length(fields) + 1]] <- list(id="Lower CI", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(id="Upper CI", type="number", format="sf:4")
		
		# Footnote informing what confidence interval is printed 
		CINote <- paste("The Confidence Interval is ", options$confidenceIntervalInterval * 100, "%", sep="") 
		footnotes[[2]] <- CINote
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
		for (variable in options[["variables"]]) {

			result <- try (silent = TRUE, expr = {
			
				if (options$tails == "oneTailedGreaterThan") {
					testType <- "greater"
					hypothesisNote <- paste("Alternative hypothesis: true mean is greater than ",
											options$testValue, ".", sep="")
					footnotes[[1]] <- hypothesisNote
				} else if (options$tails == "oneTailedLessThan") {
					testType <- "less"
					hypothesisNote <- paste("Alternative hypothesis: true mean is less than ",
											options$testValue, ".", sep="")
					footnotes[[1]] <- hypothesisNote
				} else {
					testType <- "two.sided"
					hypothesisNote <- paste("Alternative hypothesis: true difference in means is not equal to ",
											options$testValue, ".", sep="")
					footnotes[[1]] <- hypothesisNote
				}
				
				r <- t.test(dataset[[variable]][!na], alternative=testType, mu=options$testValue,
							conf.level=options$confidenceIntervalInterval)
				
				t <- as.numeric(r$statistic)
				df <- as.numeric(r$parameter)
				p <- as.numeric(r$p.value)
				m <- as.numeric(r$estimate - r$null.value)
				ciLow <- .clean(as.numeric(r$conf.int[1]) - as.numeric(r$null.value)) 
				ciUp <- .clean(as.numeric(r$conf.int[2]) - as.numeric(r$null.value)) 
				
				list(t=t, df=df, p=p, "Mean Difference"=m, "Lower CI"=ciLow, "Upper CI"=ciUp)
			})
			if (class(result) == "try-error") {
				result <- list(t="", df="", p="", "Mean Difference"="",	"Lower CI"="", "Upper CI"="")
			}
			ttest.results[[length(ttest.results)+1]] <- result
		}

		if (options$descriptives) {
			descriptives[["title"]] <- "Descriptives"
			descriptives[["cases"]] <- I(options$variables)

			fields <- list(
				list(id="N", type="number", format="sf:4"),
				list(id="Mean", type="number", format="sf:4"),
				list(id="Std. Deviation", type="number", format="dp:4;p:.001"),
				list(id="Std. Error Mean", type="number", format="sf:4"))

			descriptives[["schema"]] <- list(fields=fields)
			descriptives.results <- list()
			
			for (variable in options[["variables"]]) {
				variable <- dataset[[variable]][!na]
				omitVariable <- na.omit(variable)
				
				if (class(omitVariable) != "factor") {

					n <- .clean(length(omitVariable))
					mean <- .clean(mean(omitVariable))
					stdDeviation <- .clean(sd(omitVariable))
					stdErrorMean <- .clean(sd(omitVariable)/sqrt(length(omitVariable)))

					result <- list(N = n, Mean = mean, "Std. Deviation" = stdDeviation,
									"Std. Error Mean" = stdErrorMean)
				} else {
					n <- .clean(length(omitVariable))
					result <- list(N = n, Mean = "", "Std. Deviation" = "", "Std. Error Mean" = "")
				}
				descriptives.results[[length(descriptives.results)+1]] <- result
			}
			descriptives[["data"]] <- descriptives.results
		}	
		ttest[["data"]] <- ttest.results
		ttest[["footnotes"]] <- footnotes
	}
	
	results[["ttest"]] <- ttest
	
	if (options$descriptives)
		results[["descriptives"]] <- descriptives

	
	results
}

