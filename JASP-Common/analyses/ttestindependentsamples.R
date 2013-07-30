
results <- list()

ttest <- list()

ttest[["title"]] <- "Independent Samples T-Test"
ttest[["cases"]] <- I(options$variables)

fields <- list(
	list(id="t", type="number", format="sf:4"),
	list(id="df", type="number", format="sf:4"),
	list(id="p", type="number", format="dp:4;p:.001"),
	list(id="mean difference", type="number", format="sf:4"))

ttest[["schema"]] <- list(fields=fields)

if (perform == "run")
{
	ttest.results <- list()

	for (variable in options[["variables"]])
	{
		result <- try (silent = TRUE, expr = {

			r <- t.test(dataset[[variable]])

			t <- as.numeric(r$statistic)
			df <- as.numeric(r$parameter)
			p <- as.numeric(r$p.value)
			m <- as.numeric(r$estimate)

			list(t=t, df=df, p=p, "mean difference"=m)
		})

		if (class(result) == "try-error")
			result <- list(t="", df="", p="", "mean difference"="")

		ttest.results[[length(ttest.results)+1]] <- result
	}
	
	ttest[["data"]] <- ttest.results
}

results[["ttest"]] <- ttest
results[["inequalityOfVariances"]] <- NULL  # levene's in a separate table
results[["descriptives"]] <- NULL

results
