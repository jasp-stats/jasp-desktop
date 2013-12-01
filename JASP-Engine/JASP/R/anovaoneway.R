
AnovaOneWay <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	anova.table <- list()


	anova.table[["title"]] <- "ANOVA"
	anova.table[["cases"]] <- I(options$variables)

	fields <- list(
		list(id="bruce"))

	anova.table[["schema"]] <- list(fields=fields)

	results[["anova"]] <- anova.table 

	results
}

