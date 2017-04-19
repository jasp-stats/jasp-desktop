
BFFromT <- function(dataset=NULL, options, perform = 'run', callback = function(...) 0, ...) {

	results <- list()

	results[[".meta"]] <- list(list(name="table", type="table"))

	table <- list()
	table[["title"]] <- "BF from <i>t</i>"

	table[["schema"]] <- list(fields=list())
	table[["data"]] <- list()

	results[["table"]] <- table

	list(results=results, status="complete")
}
