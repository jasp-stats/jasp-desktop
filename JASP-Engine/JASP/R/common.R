
init <- function(name, options.as.json.string) {

	options <- RJSONIO::fromJSON(options.as.json.string, asText=TRUE, simplify=FALSE)
	analysis <- eval(parse(text=name))
	
	results <- try (silent = TRUE, expr = {
	
		analysis(dataset=NULL, options=options, perform="init")
	})
	
	if (class(results) == "try-error") {
	
		"{ \"error\" : 	1, \"errorMessage\" : \"This analysis terminated unexpectedly. Please contact its author.\" }"
	
	} else {
	
		RJSONIO::toJSON(results)	
	}

}

run <- function(name, options.as.json.string) {

	options <- RJSONIO::fromJSON(options.as.json.string, asText=TRUE, simplify=FALSE)
	analysis <- eval(parse(text=name))
	
	results <- try (silent = TRUE, expr = {
	
		analysis(dataset=NULL, options=options, perform="run", callback=callback)
	})
	
	if (class(results) == "try-error") {
	
		print(results)
	
		"{ \"error\" : 	1, \"errorMessage\" : \"This analysis terminated unexpectedly. Please contact its author.\" }"
	
	} else {
	
		RJSONIO::toJSON(results)	
	}

}

read.dataset.to.end <- function(read.as.factors=c(), exclude.na.listwise=NULL, ...) {	

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
	
	for (variable in read.as.factors) {

		if (is.factor( dataset[[variable]] ))
			dataset[[variable]] <- as.factor(dataset[[variable]])
	}
	
	dataset
}

read.dataset.header <- function() {

	.read.dataset.header.native()
}

.v <- function(variable.names) {

	vs <- c()

	for (v in variable.names)
		vs[length(vs)+1] <- paste(".", .toBase64(v), sep="")

	vs
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

.dataFrameToRowList <- function(df, discard.column.names=FALSE) {

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

.seqx <- function(from, to) {

	if (from > to)
		seq <- c()
	else
		seq <- from:to
		
	seq
}

.beginSaveImage <- function(width=320, height=320) {
		
	file <- paste(tempfile(), "png", sep=".")
			
	grDevices::png(filename=file, width=2 * width, height=2 * height, pointsize=24, bg="transparent")
	
	list(format="png", encoding="dataURI;base64", file=file)
}

.endSaveImage <- function(image.descriptor) {

	grDevices::dev.off()
	
	file <- tempfile()
	
	base64::encode(image.descriptor$file, file, linesize=1024*1024*1024)
	
	content <- paste("data:image/png;base64,", base::readChar(file, 1024*1024*1024), sep="")
	
	base::file.remove(image.descriptor$file)
	base::file.remove(file)

	content
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

