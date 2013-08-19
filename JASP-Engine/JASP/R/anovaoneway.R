
AnovaOneWay <- function(data, options, perform="run", callback=NULL) {

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

