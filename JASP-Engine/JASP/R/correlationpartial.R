
CorrelationPartial <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd()
		} else {
			dataset <- .readDataSetHeader()
		}
	}

	results <- list()
	
	correlations <- list()
	
	correlations[["title"]] <- "Correlation"
	correlations[["cases"]] <- c("line1", "line2", "line3")
	
	fields <- list(
		list(id="col1", type="number", format="sf:4"),
		list(id="col2", type="number", format="sf:4"),
		list(id="col3", type="number", format="dp:4;p:.001"))
		
	schema <- list(fields=fields)
	
	correlations[["schema"]] <- schema
	
	results[["correlations"]] <- correlations

	results
}

