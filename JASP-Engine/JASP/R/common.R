
init <- function(name, options.as.json.string) {

	options <- RJSONIO::fromJSON(options.as.json.string, asText=TRUE, simplify=FALSE)
	analysis <- eval(parse(text=name))
	results <- analysis(dataset=NULL, options=options, perform="init")
	RJSONIO::toJSON(results)
}

run <- function(name, options.as.json.string) {

	options <- RJSONIO::fromJSON(options.as.json.string, asText=TRUE, simplify=FALSE)
	analysis <- eval(parse(text=name))
	results <- analysis(dataset=NULL, options=options, perform="run", callback=callback)
	RJSONIO::toJSON(results)
}

read.dataset.to.end <- function(exclude.na.listwise=NULL, ...) {	

	dataset <- .read.dataset.native()
	
	if ( ! is.null(exclude.na.listwise))
	{
		rows.to.exclude <- c()
		
		for (col in exclude.na.listwise) {
			
			rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))
		}
		
		rows.to.exclude <- unique(rows.to.exclude)

		rows.to.keep <- 1:dim(dataset)[1]
		rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]
		
		dataset <- dataset[rows.to.keep,]
	}
	
	dataset
}

read.dataset.header <- function() {

	.read.dataset.header.native()
}

callback <- function(results=NULL) {

	if (is.null(results)) {
		json.string <- "null"
	} else {
		json.string <- RJSONIO::toJSON(results)
	}
	
	.callback.native(json.string);

}

.clean <- function(value) {

	if (is.null(value))
		return ("")

	if (is.finite(value))
		return(value)

	if (is.na(value))
		return("NaN")

	if (value == Inf)
		return("\u221E")

	if (value == -Inf)
		return("-\u221E")

	NULL
}
