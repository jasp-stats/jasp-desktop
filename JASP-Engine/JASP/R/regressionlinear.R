
RegressionLinear <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	regression <- list()
	
	regression[["title"]] <- "Linear Regression w00t!"
	regression[["cases"]] <- c("line1", "line2", "line3")
	
	fields <- list(
		list(id="col1", type="number", format="sf:4"),
		list(id="col2", type="number", format="sf:4"),
		list(id="col3", type="number", format="dp:4;p:.001"))
		
	schema <- list(fields=fields)
	
	regression[["schema"]] <- schema
	
	results[["regression"]] <- regression

	results
}

