
RegressionLogLinearBayesian <- function(dataset, options, perform="run", callback, ...) {
	
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
	results[["title"]] <- "Bayesian Log Linear Regression"
	

	llTable <- list()
	
	llTable[["title"]] <- "Bayesian Log Linear Regression"

	fields <- list()
	
	fields[[length(fields)+1]] <- list(name="model", type="string", title="Models")
	fields[[length(fields)+1]] <- list(name="PM", type="number", format="dp:3", title="P(M)")
	fields[[length(fields)+1]] <- list(name="PMdata", type="number", format="dp:3", title="P(M|data)")
	
	if (options$bayesFactorType == "BF10") {
		bfTitle <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfTitle <- "BF<sub>01</sub>"
	} else {
		bfTitle <- "Log(BF<sub>10</sub>)"
	}
	
	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3;log10", title=bfTitle)
	
	llTable[["schema"]] <- list(fields=fields)
	
	llTableData <- list()
	
	llTableData[[1]] <- list(model="Gregory") 
	llTableData[[2]] <- list(model="Bronson")
	llTableData[[3]] <- list(model="Lachlan")

	if (perform == "run") {
	
		llTableData[[1]] <- list(model="Gregory", PM=1, PMdata=1, BF=1) 
		llTableData[[2]] <- list(model="Bronson", PM=1, PMdata=1, BF=2)
		llTableData[[3]] <- list(model="Lachlan", PM=1, PMdata=1, BF=3)
	}

	llTable[["data"]] <- llTableData

	

	results[["table"]] <- llTable
	
	if (perform == "init") {

		list(results=results, status="inited")
		
	} else {
	
		list(results=results, status="complete")
	}
}
