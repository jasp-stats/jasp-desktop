
CorrelationBayesianPairs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]
	
	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
		
	} else {
	
		if (options$missingValues == "excludeListwise") {
	
			dataset <- .vdf(dataset, columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
		
		} else {
	
			dataset <- .vdf(dataset, columns.as.numeric=all.variables)
		}	
	}
	
	dataset <- na.omit(dataset)
	
	

	results <- list()
	
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation"
		
	results
}

