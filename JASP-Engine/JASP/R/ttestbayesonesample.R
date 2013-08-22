
TTestBayesianOneSample <- function(data, options, perform="run", callback=NULL, changed.options=NULL, former.options=NULL, ...) {

	results <- list()

	ttest <- list()

	ttest[["title"]] <- "Bayesian One Sample T-Test"
	ttest[["cases"]] <- I(options$variables)

	fields <- list(
		list(id="BF<sub>10</sub>", type="number", format="sf:4"),
		list(id="error", type="number", format="sf:4"))

	ttest[["schema"]] <- list(fields=fields)

	#ttest[["testValue"]] <- options$testValue

	if (perform == "run")
	{
		ttest.results <- list()

		for (variable in options[["variables"]])
		{
			result <- try (silent = TRUE, expr = {

				r <- BayesFactor::ttestBF(dataset[[variable]])
			
				BF <- .clean(exp(as.numeric(r@bayesFactor$bf)))
				error <- .clean(as.numeric(r@bayesFactor$error))

				list("BF<sub>10</sub>"=BF, error=error)
			})

			if (class(result) == "try-error")
				result <- list("BF<sub>10</sub>"="", error="")

			ttest.results[[length(ttest.results)+1]] <- result
		}

		ttest[["data"]] <- ttest.results
	}

	results[["ttest"]] <- ttest

	results
}

