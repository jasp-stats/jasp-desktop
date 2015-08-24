
RegressionLogLinear <- function(dataset, options, perform="run", callback, ...) {
	
	all.variables <- options$factors
	if (options$counts != "")
		all.variables <- c(all.variables, options$counts)

	if (is.null(dataset)) {
	
		if (perform == "run") {
		
			dataset <- .readDataSetToEnd(columns.as.factor=all.variables)
		
		} else {
		
			dataset <- .readDataSetHeader(columns.as.factor=all.variables)
		}
	}

	results <- list()
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="table", type="table")

	
	results[[".meta"]] <- meta
	results[["title"]] <- "T-Test"
	

	llTable <- list()
	
	llTable[["title"]] <- "Log Linear Regression"

	fields <- list(
		list(name="x", type="string", title=""),
		list(name="y", type="number", format="sf:4;dp:3"),
		list(name="z", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
	
	llTable[["schema"]] <- list(fields=fields)
	

	llTableData <- list()
	
	llTableData[[1]] <- list(x="Jonathon")

	if (perform == "run") {
	
		llTableData[[1]] <- list(x="Jonathon", y=123.456, z=456.789, p=0)
		
	}
	
	llTable[["data"]] <- llTableData
	
	
	results[["title"]] <- "Log Linear Regression"
	results[["table"]] <- llTable
	
	if (perform == "init") {

		list(results=results, status="inited")
		
	} else {
	
		list(results=results, status="complete")
	}
}
