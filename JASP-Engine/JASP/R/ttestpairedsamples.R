
TTestPairedSamples <- function(data, options, perform="run", callback=NULL) {

	results <- list()


	results <- list()

	ttest <- list()

	ttest[["title"]] <- "Paired Samples T-Test"

	cases <- list()

	for (pair in options$pairs)
	{
		pair[pair == ""] <- "..."	
		cases[[length(cases)+1]] <- paste(pair[1], "-", pair[2])
	}

	ttest[["cases"]] <- cases

	fields <- list(
		list(id="t", type="number", format="sf:4"),
		list(id="df", type="number", format="sf:4"),
		list(id="p", type="number", format="dp:4;p:.001"),
		list(id="mean difference", type="number", format="sf:4"))

	ttest[["schema"]] <- list(fields=fields)

	if (perform == "run")
	{

	}

	results[["ttest"]] <- ttest

	results

	results
}

