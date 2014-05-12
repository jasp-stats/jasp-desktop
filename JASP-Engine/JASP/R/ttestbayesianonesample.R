
TTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unlist(options$variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end(columns.as.numeric=all.variables)
		} else {
			dataset <- read.dataset.header(columns.as.numeric=all.variables)
		}
	}

	results <- list()

	ttest <- list()

	ttest[["title"]] <- "Bayesian One Sample T-Test"
	ttest[["cases"]] <- I(options$variables)

	fields <- list(
		list(name="Variable", type="string", title=""),
		list(name="BF<sub>10</sub>", type="number", format="sf:4"),
		list(name="error", type="number", format="sf:4"))

	ttest[["schema"]] <- list(fields=fields)

	if (length(options[["variables"]]) > 0)
	{
		ttest.results <- list()

		for (variable in options[["variables"]])
		{
			ttest.results[[length(ttest.results)+1]] <- list(Variable=variable, "BF<sub>10</sub>"=".", error=".")	
		}
		
		if (perform == "run") {

			c <- 1

			for (variable in options[["variables"]])
			{
				result <- try (silent = TRUE, expr = {

					r <- BayesFactor::ttestBF(dataset[[ .v(variable) ]])
		
					BF <- .clean(exp(as.numeric(r@bayesFactor$bf)))
					error <- .clean(as.numeric(r@bayesFactor$error))

					list(Variable=variable, "BF<sub>10</sub>"=BF, error=error)
				})

				if (class(result) == "try-error")
					result <- list(Variable=variable, "BF<sub>10</sub>"="", error="")
		
				ttest.results[[c]] <- result
				c <- c + 1
		
				ttest[["data"]] <- ttest.results
	
				results[["ttest"]] <- ttest

				#for (junk in 1:20) {
				#	Sys.sleep(.1)
				#	if (callback() != 0)
				#		return(results)
				#}
			
				#callback(results)
			}
		}
		
	}

	results[["ttest"]] <- ttest

	results
}

