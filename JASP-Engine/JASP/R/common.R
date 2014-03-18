
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

.cat <- function(object) {
	
	cat(RJSONIO::toJSON(object))
}

.data.frame.to.row.list <- function(df, discard.column.names=FALSE) {

	if (dim(df)[1] == 0 || dim(df)[2] == 0)
		return(list())
		
	column.names <- names(df)
	rows <- list()

	for (i in 1:dim(df)[1]) {
	
		row <- list()
		
		for (j in 1:length(column.names))
			row[[j]] <- df[i,j]
		
		if ( ! discard.column.names)
			names(row) <- column.names
		
		rows[[i]] <- row
	}

	rows
}

.indices <- function(v) {

	indices <- c()

	if (length(v) > 0)
		indices <- 1:length(v)
	
	indices
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
