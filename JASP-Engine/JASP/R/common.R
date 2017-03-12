#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

run <- function(name, options.as.json.string, perform="run") {

	options <- rjson::fromJSON(options.as.json.string)
	analysis <- eval(parse(text=name))
	
	env <- new.env()
	env$callback <- callback
	env$time <- Sys.time()
	env$i <- 1

	if (perform == "init") {
	
		the.callback <- function(...) list(status="ok")
		
	} else {
	
		the.callback <- function(...) {
			
			t <- Sys.time()
			
			cat(paste("Callback", env$i, ":", t - env$time, "\n"))
			
			env$time <- t
			env$i <- env$i + 1
			
			return(env$callback(...))
		}
	}

	results <- tryCatch(expr={
		
				withCallingHandlers(expr={
					
					analysis(dataset=NULL, options=options, perform=perform, callback=the.callback)
					
				}, 
				error=.addStackTrace)
				
	},
	error=function(e) e)
	
	if (inherits(results, "expectedError")) {
		
		errorResponse <- paste("{ \"status\" : \"error\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", results$message, "\" } }", sep="")
		
		errorResponse
		
	} else if (inherits(results, "error")) {
		
		error <- gsub("\"", "'", as.character(results), fixed=TRUE)
		
		stackTrace <- as.character(results$stackTrace)
		stackTrace <- gsub("\"", "'", stackTrace, fixed=TRUE)
		stackTrace <- gsub("\\\\", "", stackTrace)
		stackTrace <- paste(stackTrace, collapse="<br><br>")
		
		errorMessage <- .generateErrorMessage(type='exception', error=error, stackTrace=stackTrace)
		errorResponse <- paste("{ \"status\" : \"exception\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", errorMessage, "\" } }", sep="")
		
		errorResponse
		
	} else if (is.null(results)) {
	
		"null"
	
	} else {
	
		keep <- NULL
	
		if ("state" %in% names(results)) {

			state <- results$state
			
			location <- .requestStateFileNameNative()

			relativePath <- location$relativePath
			base::Encoding(relativePath) <- "UTF-8"

			fullPath <- paste(location$root, location$relativePath, sep="/")
			base::Encoding(fullPath) <- "UTF-8"
			base::save(state, file=fullPath)
			
			keep <- relativePath
		}
		
		if ("results" %in% names(results)) {
		
			results$results <- .addCitationToResults(results$results)
			results$state   <- NULL # remove the state object
			results$keep    <- c(results$keep, keep)  # keep the state file
			
		} else {
		
			results <- .addCitationToResults(results)
		}
		
		json <- try({ rjson::toJSON(results) })
		
		if (class(json) == "try-error") {
		
			return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }", sep=""))
			
		} else {
		
			return(json)
		}
	}

}

checkPackages <- function() {

	rjson::toJSON(.checkPackages())
}

.addCitationToTable <- function(table) {

	if ("citation" %in% names(table) ) {

		cite <- c(.baseCitation, table$citation)
		
		for (i in seq_along(cite))
			base::Encoding(cite[[i]]) <- "UTF-8"
		
		table$citation <- cite

	} else {
	
		cite <- .baseCitation
		base::Encoding(cite) <- "UTF-8"
	
		table$citation <- list(cite)
	}
	
	table
}

.addCitationToResults <- function(results) {

	if ("status" %in% names(results)) {
	
		res <- results$results
		
	} else {
	
		res <- results
	}
	
	for (m in res$.meta) {

		item.name <- m$name
		
		if (item.name %in% names(res)) {
	
			if (m$type == "table") {
	
				res[[item.name]] <- .addCitationToTable(res[[item.name]])
				
			} else if (m$type == "tables") {

				for (i in .indices(res[[item.name]]))
					res[[item.name]][[i]] <- .addCitationToTable(res[[item.name]][[i]])
			}
		}
	}
	
	
	if ("status" %in% names(results)) {
	
		results$results <- res
		
	} else {
	
		results <- res
	}
	
	results
}

.readDataSetToEnd <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, exclude.na.listwise=c(), ...) {	

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .readDatasetToEndNative(unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	dataset <- .excludeNaListwise(dataset, exclude.na.listwise)
	
	dataset
}

.readDataSetHeader <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, ...) {

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .readDataSetHeaderNative(unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	
	dataset
}


.vdf <- function(df, columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, exclude.na.listwise=c(), ...) {

	new.df <- NULL
	namez <- NULL
	
	for (column.name in columns) {
	
		column <- df[[column.name]]

		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}
		
		namez <- c(namez, column.name)
	}
		
	for (column.name in columns.as.ordinal) {
	
		column <- as.ordered(df[[column.name]])
	
		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}

		namez <- c(namez, column.name)
	}
		
	for (column.name in columns.as.factor) {
	
		column <- as.factor(df[[column.name]])

		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}
	
		namez <- c(namez, column.name)
	}
		
	for (column.name in columns.as.numeric) {

		column <- as.numeric(as.character(df[[column.name]]))

		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}
	
		namez <- c(namez, column.name)
	}

	if (is.null(new.df))
		return (data.frame())

	names(new.df) <- .v(namez)
	
	new.df <- .excludeNaListwise(new.df, exclude.na.listwise)
	
	new.df
}

.excludeNaListwise <- function(dataset, exclude.na.listwise) {

	if ( ! is.null(exclude.na.listwise)) {
	
		rows.to.exclude <- c()
		
		for (col in .v(exclude.na.listwise))
			rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))
		
		rows.to.exclude <- unique(rows.to.exclude)

		rows.to.keep <- 1:dim(dataset)[1]
		rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]
		
		new.dataset <- dataset[rows.to.keep,]
		
		if (class(new.dataset) != "data.frame") {   # HACK! if only one column, R turns it into a factor (because it's stupid)
		
			dataset <- na.omit(dataset)
			
		} else {
		
			dataset <- new.dataset
		}
	}
	
	dataset
}

.requestTempFileName <- function(extension) {

	.requestTimeFileNameNative(extension)
}

.saveState <- function(state) {

	if (base::exists(".requestStateFileNameNative")) {

		location <- .requestStateFileNameNative()

		relativePath <- location$relativePath
		base::Encoding(relativePath) <- "UTF-8"

		fullPath <- paste(location$root, location$relativePath, sep="/")
		base::Encoding(fullPath) <- "UTF-8"
		
		base::save(state, file=fullPath, compress=FALSE)
	}
	
	NULL
}

.retrieveState <- function() {

	state <- NULL
	
	if (base::exists(".requestStateFileNameNative")) {

		location <- .requestStateFileNameNative()

		relativePath <- location$relativePath
		base::Encoding(relativePath) <- "UTF-8"

		fullPath <- paste(location$root, location$relativePath, sep="/")
		base::Encoding(fullPath) <- "UTF-8"

		base::try(base::load(fullPath), silent=TRUE)
	}
	
	state
}

.shortToLong <- function(dataset, rm.factors, rm.vars, bt.vars) {

	f  <- rm.factors[[length(rm.factors)]]
	df <- data.frame(factor(unlist(f$levels), unlist(f$levels)))
	
	names(df) <- .v(f$name)
	
	row.count <- dim(df)[1]
	

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
		cells <- factor(cells, unlist(f$levels))
		
		df <- cbind(cells, df)
		names(df)[[1]] <- .v(f$name)
		
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
	
	subjects <- 1:(dim(dataset)[1])
	subjects <- as.factor(rep(subjects, each=row.count))
	
	df <- cbind(df, subject=subjects)

	df
}

.v <- function(variable.names, prefix="X") {

	vs <- c()

	for (v in variable.names)
		vs[length(vs)+1] <- paste(prefix, .toBase64(v), sep="")

	vs
}

.unv <- function(variable.names) {

	vs <- c()
	
	for (v in variable.names) {
	
		if (nchar(v) == 0)
			stop(paste("bad call to .unv() : v is \"\""))
	
		firstChar <- charToRaw(substr(v, 1, 1))
	
		if (firstChar >= 0x41 && firstChar <= 0x5A) {  # A to Z
		
			vs[length(vs)+1] <- .fromBase64(substr(v, 2, nchar(v)))
			
		} else {
		
			stop(paste("bad call to .unv() : v is \"", v, "\"", sep=""))
		}
	}
	
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
	
	interaction.symbol <- "\u2009\u273B\u2009"
	base::Encoding(interaction.symbol) <- "UTF-8"
	
	.compose(unved, interaction.symbol)
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

.compose <- function(formulas, i.symbol=":") {

	sapply(formulas, function(formula) {
		
		formula <- sapply(formula, function(side) {
			
			side <- sapply(side, function(term) {
			
				term <- sapply(term, function(component) { base::Encoding(component) <- "UTF-8" ; component })
			
				paste(term, collapse=i.symbol)
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

.shouldContinue <- function(value) {

	base::identical(value, 0) || base::identical(value, as.integer(0)) || (is.list(value) && value$status == "ok")
}

callback <- function(results=NULL) {

	ret <- 0

	if (base::exists(".callbackNative")) {

		if (is.null(results)) {
			json.string <- "null"
		} else {
			json.string <- rjson::toJSON(results)
		}
	
		response <- .callbackNative(json.string)
		
		if (is.character(response)) {
		
			ret <- rjson::fromJSON(base::paste("[", response, "]"))[[1]]
			
		} else {
		
			ret <- response
		}
	}

	ret
}

.cat <- function(object) {
	
	cat(rjson::toJSON(object))
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

	#filename <- .requestTempFileNameNative("svg")
	#grDevices::svg(filename=filename, width=width/72, height=height/72, bg="transparent")
	
	type <- "cairo"
	
	if (Sys.info()["sysname"]=="Darwin")  # OS X
		type <- "quartz"
	
	multip <- .ppi / 96
	
	location <- .requestTempFileNameNative("png")
	
	relativePath <- location$relativePath
	base::Encoding(relativePath) <- "UTF-8"
	
	fullPath <- paste(location$root, location$relativePath, sep="/")
	base::Encoding(fullPath) <- "UTF-8"
	
	grDevices::png(filename=fullPath, width=width * multip, height=height * multip, bg="transparent", res=72 * multip, type=type)
	
	relativePath
}

.endSaveImage <- function(filename) {

	grDevices::dev.off()
	
	filename
}

.extractErrorMessage <- function(error) {

	split <- base::strsplit(as.character(error), ":")[[1]]
	last <- split[[length(split)]]
	stringr::str_trim(last)
}

.addFootnote <- function(footnotes, message, symbol=NULL) {
	
	if (length(footnotes) == 0) {
		
		if (is.null(symbol)) {
			
			footnotes <- list(message)
			
		} else {
			
			footnotes <- list(symbol=symbol, text=message)
		}
		
		return(list(footnotes=footnotes, index=0))
		
	} else {
		
		for (i in 1:length(footnotes)) {
			
			footnote <- footnotes[[i]]
			
			if ("text" %in% names(footnote)) {
				existingMessage <- footnote$text
			} else {
				existingMessage <- footnote
			}
				
			if (existingMessage == message)
				return(list(footnotes=footnotes, index=i-1))
		}
		
		if (is.null(symbol)) {
			new.footnote <- message
		} else {
			new.footnote <- list(symbol=symbol, message=message)
		}
	
		index <- length(footnotes)+1
		footnotes[[index]] <- new.footnote
		
		return(list(footnotes=footnotes, index=index-1))
	}
}

.clean <- function(value) {

	if (is.list(value)) {
	
		if (is.null(names(value))) {
			
			for (i in length(value))
				value[[i]] <- .clean(value[[i]])
				
		} else {
		
			for (name in names(value))
				value[[name]] <- .clean(value[[name]])
		}
		
		return(value)
	}

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

	stop("could not clean value")
}

.newFootnotes <- function() {
	
	footnotes <- new.env()
	footnotes$footnotes <- list()
	footnotes$next.symbol <- 0
	
	class(footnotes) <- c("footnotes", class(footnotes))
	
	footnotes
}

as.list.footnotes <- function(footnotes) {
	
	footnotes$footnotes
}

.addFootnote <- function(footnotes, text, symbol=NULL) {

	if (length(footnotes$footnotes) == 0) {
		
		if (is.null(symbol)) {
		
			symbol <- footnotes$next.symbol
			footnotes$next.symbol <- symbol + 1	
		}
		
		footnotes$footnotes <- list(list(symbol=symbol, text=text))
		
		return(0)
		
	} else {
		
		for (i in 1:length(footnotes$footnotes)) {
			
			footnote <- footnotes$footnotes[[i]]
			
			if ("text" %in% names(footnote)) {
				existingMessage <- footnote$text
			} else {
				existingMessage <- footnote
			}
			
			if (existingMessage == text)
				return(i-1)
		}
		
		if (is.null(symbol)) {
		
			symbol <- footnotes$next.symbol
			footnotes$next.symbol <- symbol + 1	
		}

		new.footnote <- list(symbol=symbol, text=text)
		
		index <- length(footnotes$footnotes)+1
		footnotes$footnotes[[index]] <- new.footnote
		
		return(index-1)
	}
}


.diff <- function(one, two) {

	# returns TRUE if different or not really comparable
	# returns a list of what has changed if non-identical named lists provided
	
	if (is.null(names(one)) == ( ! is.null(names(two))))  # if one list has names, and the other not
		return(TRUE)
	
	changed <- list()
	
	if (is.null(names(one)) == FALSE) {
		
		names1 <- names(one)
		names2 <- names(two)
		
		for (name in names1) {
			
			if (name %in% names2) {
				
				item1 <- one[[name]]
				item2 <- two[[name]]
				
				if (base::identical(item1, item2) == FALSE) {
				
					changed[[name]] <- TRUE
					
				} else {
				
					changed[[name]] <- FALSE
				}
				
			} else {
				
				changed[[name]] <- TRUE
				
			}
			
		}
		
		for (name in names2) {
			
			if ((name %in% names1) == FALSE)
				changed[[name]] <- TRUE
		}
		
	} else if (base::identical(one, two)) {
		
		return(FALSE)
		
	} else {
	
		return (TRUE)
	}
	
	changed
}
