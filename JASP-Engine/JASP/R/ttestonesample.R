
TTestOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unlist(options$variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
			
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			}
			else {
			
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
		
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
	}

	results <- list()
	
	
	
	meta <- list()
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="table")
	
	results[[".meta"]] <- meta
	
	

	ttest <- list()
	descriptives <- list()
	

	if (options$hypothesis == "notEqualToTestValue") {

		ttest[["title"]] <- "One Sample T-Test"
	
	} else {
	
		ttest[["title"]] <- "One Tailed One Sample T-Test"	
	}
	


	fields <- list(
		list(name=".variable", type="string", title=""),
		list(name="t", type="number", format="sf:4;dp:3"),
		list(name="df", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
		
	if (options$meanDifference) {
	
		fields[[length(fields) + 1]] <- list(name="Mean Difference", type="number", format="sf:4;dp:3")
	}
	
	if (options$confidenceInterval) {
	
		fields[[length(fields) + 1]] <- list(name="Lower CI", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="Upper CI", type="number", format="sf:4;dp:3")
	}
	
	ttest[["schema"]] <- list(fields=fields)
	
	ttest.results <- list()
	
	for (variable in options[["variables"]]) {

		if (perform == "run") {

			result <- try (silent = TRUE, expr = {
		
				if (options$hypothesis == "greaterThanTestValue") {

					testType <- "greater"

				} else if (options$hypothesis == "lessThanTestValue") {

					testType <- "less"

				} else {
			
					testType <- "two.sided"
				}
			
				r <- t.test(dataset[[ .v(variable) ]], alternative=testType, mu=options$testValue,
							conf.level=options$confidenceIntervalInterval)
			
				t  <- as.numeric(r$statistic)
				df <- as.numeric(r$parameter)
				p  <- as.numeric(r$p.value)
				m  <- as.numeric(r$estimate - r$null.value)
				ciLow <- .clean(as.numeric(r$conf.int[1]) - as.numeric(r$null.value)) 
				ciUp  <- .clean(as.numeric(r$conf.int[2]) - as.numeric(r$null.value)) 
			
				list(.variable=variable, t=t, df=df, p=p, "Mean Difference"=m, "Lower CI"=ciLow, "Upper CI"=ciUp)
			})
				
			if (class(result) == "try-error") {
		
				result <- list(.variable=variable, t="", df="", p="", "Mean Difference"="", "Lower CI"="", "Upper CI"="")
			}
			
		} else {

			result <- list(.variable=variable, t=".", df=".", p=".", "Mean Difference"=".", "Lower CI"=".", "Upper CI"=".")
		
		}
		
		ttest.results[[length(ttest.results)+1]] <- result
	}
	
	ttest[["data"]] <- ttest.results
	
	

	if (options$descriptives) {
	
		descriptives[["title"]] <- "Descriptives"
		descriptives[["cases"]] <- I(options$variables)

		fields <- list(
			list(name=".variable", type="string", title=""),
			list(name="N", type="number", format="sf:4;dp:3"),
			list(name="Mean", type="number", format="sf:4;dp:3"),
			list(name="Std. Deviation", type="number", format="dp:3;p:.001"),
			list(name="Std. Error Mean", type="number", format="sf:4;dp:3"))

		descriptives[["schema"]] <- list(fields=fields)
		descriptives.results <- list()
		
		for (variable in options[["variables"]]) {
			
			if (perform == "run") {

				data <- na.omit(dataset[[ .v(variable) ]])

				if (class(data) != "factor") {

					n    <- .clean(length(data))
					mean <- .clean(mean(data))
					stdDeviation <- .clean(sd(data))
					stdErrorMean <- .clean(sd(data)/sqrt(length(data)))

					result <- list(.variable=variable, N = n, Mean = mean, "Std. Deviation" = stdDeviation,
									"Std. Error Mean" = stdErrorMean)
				} else {
			
					n <- .clean(length(data))
					result <- list(.variable=variable, N = n, Mean = "", "Std. Deviation" = "", "Std. Error Mean" = "")
				}
			
			} else {
			
				result <- list(.variable=variable, N = ".", Mean = ".", "Std. Deviation" = ".", "Std. Error Mean" = ".")			
			
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- result
		}
		
		descriptives[["data"]] <- descriptives.results
	}
	
	
	
	results[["ttest"]] <- ttest
	
	if (options$descriptives)
		results[["descriptives"]] <- descriptives

	
	results
}

