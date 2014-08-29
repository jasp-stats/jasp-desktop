
init <- function(name, options.as.json.string) {

	options <- RJSONIO::fromJSON(options.as.json.string, asText=TRUE, simplify=FALSE)
	analysis <- eval(parse(text=name))
	
	results <- try (silent = TRUE, expr = {
	
		analysis(dataset=NULL, options=options, perform="init")
	})
	
	if (class(results) == "try-error") {
	
		print(results)
	
		"{ \"error\" : 	1, \"errorMessage\" : \"This analysis terminated unexpectedly. Please contact its author.\" }"
	
	} else {
	
		RJSONIO::toJSON(results, digits=12)
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
	
		RJSONIO::toJSON(results, digits=12)
	}

}

.readDataSetToEnd <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, exclude.na.listwise=c(), ...) {	

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .readDatasetToEndNative(unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	
	if ( ! is.null(exclude.na.listwise))
	{
		rows.to.exclude <- c()
		
		for (col in .v(exclude.na.listwise)) {
			
			rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))
		}
		
		rows.to.exclude <- unique(rows.to.exclude)

		rows.to.keep <- 1:dim(dataset)[1]
		rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]
		
		dataset <- dataset[rows.to.keep,]
	}
	
	dataset
}

.readDataSetHeader <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, ...) {

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .readDataSetHeaderNative(unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	
	dataset
}

.shortToLong <- function(dataset, rm.factors, rm.vars, bt.vars) {

	f  <- rm.factors[[length(rm.factors)]]
	df <- data.frame(as.factor(unlist(f$levels)))
	names(df) <- paste("F", .v(f$name), sep="")

	i <- length(rm.factors) - 1
	while (i > 0) {
	
		f <- rm.factors[[i]]
	
		new.df <- df

		j <- 2
		while (j <= length(f$levels)) {
		
			new.df <- rbind(new.df, df)
			j <- j + 1
		}
		
		df <- new.df
		
		row.count <- dim(df)[1]

		cells <- rep(unlist(f$levels), each=row.count / length(f$levels))
		cells <- as.factor(cells)
		
		
		df <- cbind(cells, df)
		names(df)[[i]] <- paste("F", .v(f$name), sep="")
		
		i <- i - 1
	}
	
	ds <- subset(dataset, select=.v(rm.vars))
	ds <- t(as.matrix(ds))
	
	df <- cbind(df, dependent=as.numeric(c(ds)))
	
	for (bt.var in bt.vars) {

		cells <- rep(dataset[[.v(bt.var)]], each=row.count)
		new.col <- list()
		new.col[[.v(bt.var)]] <- cells
		
		df <- cbind(df, new.col)
	}

	df
}

.v <- function(variable.names) {

	vs <- c()

	for (v in variable.names)
		vs[length(vs)+1] <- paste(".", .toBase64(v), sep="")

	vs
}

.unv <- function(variable.names) {

	vs <- c()
	
	for (v in variable.names)
		vs[length(vs)+1] <- .fromBase64(substr(v, 2, nchar(v)))
	
	vs
}

.vf <- function(formula) {
  
  in.pieces <- .decompose(formula)
  ved <- .jrapply(in.pieces, .v)
  .compose(ved)
}

.unvf <- function(formula) {
  
  in.pieces <- .decompose(formula)
  unved <- .jrapply(in.pieces, .unv)
  .compose(unved)
}

.decompose <- function(formulas) {
  
  lapply(as.list(formulas), function(formula) {
  
    sides <- strsplit(formula, "~", fixed=TRUE)[[1]]
    
    lapply(sides, function(formula) {
    
      terms <- strsplit(formula, "+", fixed=TRUE)
      
      lapply(terms, function(term) {
        components <- strsplit(term, ":")
        components <- sapply(components, stringr::str_trim, simplify=FALSE)
      })[[1]]
      
    })
  })
}

.compose <- function(formulas) {
  
  sapply(formulas, function(formula) {
    
    formula <- sapply(formula, function(side) {
      
      side <- sapply(side, function(term) {
        paste(term, collapse=":")
      })
      
      paste(side, collapse=" + ")
    })
    
    paste(formula, collapse=" ~ ")    
  })
}


.jrapply <- function(X, FUN) {
	
	if (is.list(X) && length(X) > 0) {
		
		for (i in 1:length(X)) {
			X[[i]] <- .jrapply(X[[i]], FUN)
		}
	}
	else {
		X <- FUN(X)
	}
	
	X
}

callback <- function(results=NULL) {

	if (is.null(results)) {
		json.string <- "null"
	} else {
		json.string <- RJSONIO::toJSON(results)
	}
	
	.callbackNative(json.string);

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

	file <- tempfile(fileext=".svg")
			
	grDevices::svg(filename=file, width=width/72, height=height/72, bg="transparent")
	
	list(format="svg", encoding="dataURI;base64", file=file)
}

.endSaveImage <- function(image.descriptor) {

	grDevices::dev.off()
	
	file <- tempfile(fileext=".base64")
	
	base64::encode(image.descriptor$file, file, linesize=1024*1024*1024)
	
	file.size <- base::file.info(file)$size
	
	content <- paste("data:image/svg+xml;base64,", base::readChar(file, file.size), sep="")
	
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

