
.descriptivesKurtosis <- function(x) {

	# Kurtosis function as in SPSS: 
	# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
	# http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis
	
	n <- length(x)
	s4 <- sum((x - mean(x))^4)
	s2 <- sum((x - mean(x))^2)
	v <- s2 / (n-1)
	a <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
	b <- s4 / (v^2)
	c <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))
	kurtosis <- a * b + c
	return(kurtosis)
}

.descriptivesSkewness <- function(x) {

	# Skewness function as in SPSS (for samlpes spaces): 
	# http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005
	
	n <- length(x)
	m <- mean(x)
	s <- sd(x) 
	z <- (x - m) / s  # z scores
	a <- n / ((n - 1) * (n - 2))
	skewness <- sum(z^3) * a
	return(skewness)
}

.descriptivesSES <- function(x) {

	# Standard Error of Skewness
	# Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf
	
	n <- length(x)
	SES <- sqrt((6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))))
	return(SES)
}

.descriptivesSEK <- function(x) {

	# Standard Error of Kurtosis
	# Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf
	
	n <- length(x)
	SEK <- 2 * .descriptivesSES(x) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
	return(SEK)
} 

Descriptives <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	variables <- unlist(options$main$fields)

	if (is.null(dataset)) {
	
		if (perform == "run") {
		
			dataset <- read.dataset.to.end(columns.as.numeric=variables)
			
		} else {
		
			dataset <- read.dataset.header(columns.as.numeric=variables)
		}
	}

	stats.options <- options[["statistics"]]
	central.tendency <- stats.options[["centralTendency"]]
	dispersion <- stats.options[["dispersion"]]
	distribution <- stats.options[["distribution"]]
	percentileValues <- stats.options[["percentileValues"]]
	equalGroupsNo <- options$statistics$percentileValues$equalGroupsNo 
	percentilesPercentiles  <- options$statistics$percentileValues$percentilesPercentiles

	run <- perform == "run"

	results <- list()
	
	#### META
	
	meta <- list()
	
	meta[[1]] <- list(name="stats", type="table")
	meta[[2]] <- list(name="tables", type="tables")
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta

	#### STATS TABLE

	stats.results <- list()

	fields <- list()

	fields[[length(fields) + 1]] <- list(name="Variable", type="string")
	fields[[length(fields) + 1]] <- list(name="Valid", type="integer")
	fields[[length(fields) + 1]] <- list(name="Missing", type="integer")

	if (central.tendency[["mean"]])
		fields[[length(fields) + 1]] <- list(name="Mean", type="number", format="sf:4")
	if (dispersion[["standardErrorMean"]])
		fields[[length(fields) + 1]] <- list(name="Std. Error of Mean", type="text")	
	if (central.tendency[["median"]])
		fields[[length(fields) + 1]] <- list(name="Median", type="number", format="sf:4")
	if (central.tendency[["mode"]])
		fields[[length(fields) + 1]] <- list(name="Mode", type="number", format="sf:4")
	if (dispersion[["standardDeviation"]])
		fields[[length(fields) + 1]] <- list(name="Std. Deviation", type="text")
	if (dispersion[["variance"]])
		fields[[length(fields) + 1]] <- list(name="Variance", type="text")
		
	if (distribution[["skewness"]]) {
	
		fields[[length(fields) + 1]] <- list(name="Skewness", type="text")  
		fields[[length(fields) + 1]] <- list(name="Std. Error of Skewness", type="text") 
	}
	
	if (distribution[["kurtosis"]]) {
	
		fields[[length(fields) + 1]] <- list(name="Kurtosis", type="text")  
		fields[[length(fields) + 1]] <- list(name="Std. Error of Kurtosis", type="text")
	}
	
	if (dispersion[["range"]])
		fields[[length(fields) + 1]] <- list(name="Range", type="text")
	if (dispersion[["minimum"]])
		fields[[length(fields) + 1]] <- list(name="Minimum", type="text")
	if (dispersion[["maximum"]])
		fields[[length(fields) + 1]] <- list(name="Maximum", type="text")
	if (central.tendency[["sum"]])
		fields[[length(fields) + 1]] <- list(name="Sum", type="number", format="sf:4")
	
	if (percentileValues[["quartiles"]]) {
	
		fields[[length(fields) + 1]] <- list(name="25th percentile", type="text")  
		fields[[length(fields) + 1]] <- list(name="50th percentile", type="text") 
		fields[[length(fields) + 1]] <- list(name="75th percentile", type="text")
	}
	
	if (percentileValues[["equalGroups"]]) {  # I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way
	
		for (i in seq(equalGroupsNo - 1))
			fields[[length(fields) + 1]] <- list(name=paste(100 * i / equalGroupsNo, "th percentile", sep=""), type="text") 
	}
	
	if (percentileValues[["percentiles"]]) { 
	
		for (i in percentilesPercentiles) 
			fields[[length(fields) + 1]] <- list(name=paste(i, "th percentile", sep=""), type="text") 
	} 
  
	stats.results[["title"]] <- "Descriptive Statistics"
	stats.results[["schema"]] <- list(fields=fields)
	stats.results[["casesAcrossColumns"]] <- TRUE

	footnotes <- list()

	if (perform == "init") {
	
		stats.values <- list()

		for (variable in variables)
			stats.values[[length(stats.values)+1]] <- list(Variable=variable)
		
		stats.results[["data"]] <- stats.values

	} else if (perform == "run") {
	
		stats.values <- list()

		for (variable in variables) {

			variable.results <- list(Variable=variable)
			column <- dataset[[ .v(variable) ]]

			rows <- nrow(dataset)
			na.omitted = na.omit(column)

			variable.results[["Valid"]] = length(na.omitted)
			variable.results[["Missing"]] = rows - length(na.omitted)

			if (central.tendency[["mean"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Mean"]] <- .clean(mean(na.omitted))
					
				} else {
				
					variable.results[["Mean"]] <- ""
				}
			}
			
			if (central.tendency[["median"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Median"]] <- .clean(median(na.omitted))
					
				} else {
				
					variable.results[["Median"]] <- ""
				}
			}
			
			if (central.tendency[["mode"]]) {
		
				if (class(na.omitted) != "factor") {
			
					mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))

					if (length(mode) > 1) {

						warning <- "More than one mode exists, only the first is reported"
						if ( ! (warning %in% footnotes))
							footnotes[[length(footnotes)+1]] <- warning
						index <- which.max(footnotes == warning) - 1
					
						variable.results[[".footnotes"]] <- list(Mode=list(index));
					}
			
					variable.results[["Mode"]] <- .clean(mode[1])
				
				} else {
			
					variable.results[["Mode"]] <- ""
				}
			
			
			}
			
			if (central.tendency[["sum"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Sum"]] <- .clean(sum(na.omitted))
					
				} else {
				
					variable.results[["Sum"]] <- ""
				}
			}
			
			if (dispersion[["maximum"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Maximum"]] <- .clean(max(na.omitted))
					
				} else {
				
					variable.results[["Maximum"]] <- ""
				}
			}
			
			if (dispersion[["minimum"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Minimum"]] <- .clean(min(na.omitted))
					
				} else {
				
					variable.results[["Minimum"]] <- ""
				}
			}
			
			if (dispersion[["range"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Range"]] <- .clean(range(na.omitted)[2]-range(na.omitted)[1])
					
				} else {
				
					variable.results[["Range"]] <- ""
				}
			}
			
			if (dispersion[["standardDeviation"]]) {
			
				if (class(na.omitted) != "factor"){
				
					variable.results[["Std. Deviation"]] <- .clean(sd(na.omitted))
					
				} else {
				
					variable.results[["Std. Deviation"]] <- ""
				}
			}
			
			if (dispersion[["standardErrorMean"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Std. Error of Mean"]] <- .clean(sd(na.omitted)/sqrt(length(na.omitted)))
					
				} else {
				
					variable.results[["Std. Error of Mean"]] <- ""
				}
			}
			
			if (dispersion[["variance"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Variance"]] <- .clean(var(na.omitted))
					
				} else {
				
					variable.results[["Variance"]] <- ""
				}
			}
			
			if (distribution[["kurtosis"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Kurtosis"]] <- .clean(.descriptivesKurtosis(na.omitted))
					variable.results[["Std. Error of Kurtosis"]] <- .clean(.descriptivesSEK(na.omitted))
					
				} else {
				
					variable.results[["Kurtosis"]] <- ""
					variable.results[["Std. Error of Kurtosis"]] <- ""
				}
			}
			
			if (distribution[["skewness"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["Skewness"]] <- .clean(.descriptivesSkewness(na.omitted))
					variable.results[["Std. Error of Skewness"]] <- .clean(.descriptivesSES(na.omitted))
					
				} else {
				
					variable.results[["Skewness"]] <- ""
					variable.results[["Std. Error of Skewness"]] <- ""
				}
			}
			
			if (percentileValues[["quartiles"]]) {
			
				if (class(na.omitted) != "factor") {
				
					variable.results[["25th percentile"]] <- .clean(quantile(na.omitted, c(.25), type=6, names=F))
					variable.results[["50th percentile"]] <- .clean(quantile(na.omitted, c(.5), type=6, names=F))
					variable.results[["75th percentile"]] <- .clean(quantile(na.omitted, c(.75), type=6, names=F))
					
				} else {
				
					variable.results[["25th percentile"]] <- ""
					variable.results[["50th percentile"]] <- ""
					variable.results[["75th percentile"]] <- ""
				}
			}
			
			if (percentileValues[["equalGroups"]]) {
			
				if (class(na.omitted) != "factor") {
				
					for (i in seq(equalGroupsNo - 1))
						variable.results[[paste(100 * i / equalGroupsNo, "th percentile", sep="")]] <- .clean(quantile(na.omitted, c(i / equalGroupsNo), type=6, names=F))
						
				} else {
				
					for (i in seq(equalGroupsNo - 1))
						variable.results[[paste(100 * i / equalGroupsNo, "th percentile", sep="")]] <- ""
						
				}
			}
				
			if (percentileValues[["percentiles"]]) {
			
				if (class(na.omitted) != "factor") {
				
					for (i in percentilesPercentiles)
						variable.results[[paste(i,"th percentile", sep="")]] <- .clean(quantile(na.omitted, c(i / 100), type=6, names=F))
						
				} else {
				
					for (i in 1:(equalGroupsNo - 1))
						variable.results[[paste(i,"th percentile", sep="")]] <- ""
				}
			}
			stats.values[[length(stats.values) + 1]] <- variable.results
		}
	
		stats.results[["data"]] <- stats.values
		stats.results[["footnotes"]] <- footnotes
	}

	results[["stats"]] <- stats.results

	#### FREQUENCIES TABLES

	if (options$main$displayFrequencyTables) {
	
		frequency.tables <- list()
		
		for (variable in variables) {
	
			column <- dataset[[ .v(variable) ]]
		
			if (class(column) == "numeric")
				next		
			
			frequency.table <- list()
		
			fields <- list(
							list(name="Level", type="string", title=""),
							list(name="Frequency", type="integer"),
							list(name="Percent", type="number", format="dp:1"),
							list(name="Valid Percent", type="number", format="dp:1"),
							list(name="Cumulative Percent", type="number", format="dp:1"))

			frequency.table[["title"]] <- paste("Frequencies for", variable)
			frequency.table[["schema"]] <- list(fields=fields)
		
			if (perform == "run") {
		
				lvls <- c()

				if (class(column) == "factor") {
				
					lvls <- levels(dataset[[ .v(variable) ]])
					
				} else if (class(column) == "integer") {
				
					lvls <- sort(unique(dataset[[ .v(variable) ]]))
				}

				t <- table(column)
				total <- sum(t)

				ns <- list()
				freqs <- list()
				percent <- list()
				validPercent <- list()
				cumPercent <- list()

				cumFreq <- 0

				for (n in names(t)) {

					ns[[length(ns)+1]] <- n
					freq <- as.vector(t[n])
					cumFreq <- cumFreq + freq
	
					freqs[[length(freqs) + 1]] <- freq
					percent[[length(percent) + 1]] <- freq / total * 100
					validPercent[[length(validPercent) + 1]] <- freq / total * 100
					cumPercent[[length(cumPercent)+1]] <- cumFreq / total * 100
				}

				ns[[length(ns)+1]] <- "Total"
				freqs[[length(freqs)+1]] <- total
				percent[[length(percent)+1]] <- 100
				validPercent[[length(validPercent)+1]] <- 100
				cumPercent[[length(cumPercent)+1]] <- ""

				data <- list()

				for (i in seq(freqs))
					data[[length(data)+1]] <- list(Level=ns[[i]], "Frequency"=freqs[[i]], "Percent"=percent[[i]], "Valid Percent"=validPercent[[i]], "Cumulative Percent"=cumPercent[[i]])

				frequency.table[["data"]] <- data

			} else {
			
				data <- list()
		
				if (class(column) == "factor") {
			
					for (level in levels(dataset[[ .v(variable) ]]))
						data[[length(data)+1]] <- list(level=level)
				
				}
				
				frequency.table[["data"]] <- data
			}
		
			frequency.tables[[length(frequency.tables)+1]] <- frequency.table
		}
		results[["tables"]] <- frequency.tables
	}

	#### FREQUENCY PLOTS

	if (options$charts$chartType != "noCharts") {
	
		frequency.plots <- list()
		
		i <- 1

		for (variable in variables) {

			column <- dataset[[ .v(variable) ]]

			if (class(column) == "numeric" || is.factor(column))
				next
				
			plot <- list()
		
			plot[["title"]] <- variable
			plot[["width"]]  <- options$chartWidth
			plot[["height"]] <- options$chartHeight
			plot[["itemOptions"]] <- list(width="chartWidth", height="chartHeight")
		
			frequency.plots[[i]] <- plot
			i <- i + 1
		}
		
		results[["plots"]] <- frequency.plots

		if (perform=="run") {
			
			i <- 1

			for (variable in variables) {
	
				column <- dataset[[ .v(variable) ]]

				if (class(column) == "numeric" || is.factor(column))
					next
				
				if (callback(results) != 0)
					return()
			
				image <- .beginSaveImage(options$chartWidth, options$chartHeight)
			
				par(lwd=2)
				hist(column, main=paste("Frequencies for", variable), xlab=variable, col=rainbow(10))
			
				content <- .endSaveImage(image)
			
				plot <- frequency.plots[[i]]
			
				plot[["data"]]  <- content
			
				frequency.plots[[i]] <- plot
				i <- i + 1

				results[["plots"]] <- frequency.plots
			
			}
		
		}
	
	}

	results
}
