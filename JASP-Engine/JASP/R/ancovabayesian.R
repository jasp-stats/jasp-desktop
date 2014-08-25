
AncovaBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd()
		} else {
			dataset <- .readDataSetHeader()
		}
	}

	results <- list()
	
	results[[".meta"]] <- list(list(name="anova", type="table"))
	
	
	
	anova <- list()
	
	anova[["title"]] <- options$model
	
	fields <- list(
		list(name="col1", type="number", format="sf:4"),
		list(name="col2", type="number", format="sf:4"),
		list(name="col3", type="number", format="dp:4;p:.001"))
		
	schema <- list(fields=fields)
	
	anova[["schema"]] <- schema
	
	results[["anova"]] <- anova

	results
}

