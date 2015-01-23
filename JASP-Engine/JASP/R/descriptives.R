
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

.barplotJASP <- function(column, variable){

	maxFrequency <- max(summary(column))
	yticks <- seq(0, maxFrequency, 1)
	
	while (length(yticks) > 8) {
		
		yticks <- yticks[seq(1,length(yticks), 2)]
	}
	
	while (max(yticks) < maxFrequency) {
		
		stepSize <- yticks[2] - yticks[1]
		yticks <- c(yticks, yticks[length(yticks)] + stepSize)		
	}
	
	yLabs <- vector("character", length(yticks))
	
	for(i in seq_along(yticks)){
		
		if(yticks[i] < 10^6){
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
			
		} else{
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}		
	}
	
	distLab <- max(nchar(yLabs))/1.8
	
	par(mar= c(5, 7.2, 4, 2) + 0.1)
	barplot(summary(column), cex.names= 1.3, axes= FALSE, ylim= range(yticks))
	axis(2, las=1, at= yticks, labels= yLabs, cex.axis= 1.4)
	mtext(text = variable, side = 1, cex=1.5, line = 3)
	mtext(text = "Frequency", side = 2, cex=1.5, line = distLab+2, las=0)
}

.plotMarginal <- function(variable, variableName, cexYlab= 1.3, lwd= 2, rugs= FALSE){

	par(mar= c(5, 4.5, 4, 2) + 0.1)
	
	density <- density(variable)
	
	h <- hist(variable, plot = FALSE)
	jitVar <- jitter(variable)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(variable, h$breaks), min.n= 3)
	
	plot(1, xlim= range(xticks), ylim= c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks, cex.axis = 1.2)
	mtext(text = variableName, side = 1, cex=1.5, line = 3)
	par(las=0)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), mgp=c(3,0.2,0), cex.axis= 1.5, mgp= c(3, 0.7, 0))
	
	if(rugs){
		rug(jitVar)
	}
	
	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)
}

Descriptives <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	variables <- unlist(options$main$fields)
	
	if (is.null(dataset)) {
	
		if (perform == "run") {
		
			dataset <- .readDataSetToEnd(columns=variables)
			
		} else {
		
			dataset <- .readDataSetHeader(columns=variables)
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
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="stats", type="table")
	meta[[3]] <- list(name="frequenciesHeading", type="h1")
	meta[[4]] <- list(name="tables", type="tables")
	meta[[5]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Descriptives"

	#### STATS TABLE

	stats.results <- list()

	fields <- list()

	fields[[length(fields) + 1]] <- list(name="Variable", title="", type="string")
	fields[[length(fields) + 1]] <- list(name="Valid", type="integer")
	fields[[length(fields) + 1]] <- list(name="Missing", type="integer")

	if (central.tendency[["mean"]])
		fields[[length(fields) + 1]] <- list(name="Mean", type="number", format="sf:4")
	if (dispersion[["standardErrorMean"]])
		fields[[length(fields) + 1]] <- list(name="Std. Error of Mean", type="number", format="sf:4")
	if (central.tendency[["median"]])
		fields[[length(fields) + 1]] <- list(name="Median", type="number", format="sf:4")
	if (central.tendency[["mode"]])
		fields[[length(fields) + 1]] <- list(name="Mode", type="number", format="sf:4")
	if (dispersion[["standardDeviation"]])
		fields[[length(fields) + 1]] <- list(name="Std. Deviation", type="number", format="sf:4")
	if (dispersion[["variance"]])
		fields[[length(fields) + 1]] <- list(name="Variance", type="number", format="sf:4")
		
	if (distribution[["skewness"]]) {
	
		fields[[length(fields) + 1]] <- list(name="Skewness", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="Std. Error of Skewness", type="number", format="sf:4")
	}
	
	if (distribution[["kurtosis"]]) {
	
		fields[[length(fields) + 1]] <- list(name="Kurtosis", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="Std. Error of Kurtosis", type="text", format="sf:4")
	}
	
	if (dispersion[["range"]])
		fields[[length(fields) + 1]] <- list(name="Range", type="number", format="sf:4")
	if (dispersion[["minimum"]])
		fields[[length(fields) + 1]] <- list(name="Minimum", type="number", format="sf:4")
	if (dispersion[["maximum"]])
		fields[[length(fields) + 1]] <- list(name="Maximum", type="number", format="sf:4")
	if (central.tendency[["sum"]])
		fields[[length(fields) + 1]] <- list(name="Sum", type="number", format="sf:4")
	
	if (percentileValues[["quartiles"]]) {
	
		fields[[length(fields) + 1]] <- list(name="q1", title="25th percentile", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="q2", title="50th percentile", type="number", format="sf:4")
		fields[[length(fields) + 1]] <- list(name="q3", title="75th percentile", type="number", format="sf:4")
	}
	
	if (percentileValues[["equalGroups"]]) {  # I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way
	
		for (i in seq(equalGroupsNo - 1))
			fields[[length(fields) + 1]] <- list(name=paste("eg", i, sep=""), title=paste(100 * i / equalGroupsNo, "th percentile", sep=""), type="number", format="sf:4")
	}
	
	if (percentileValues[["percentiles"]]) { 
	
		for (i in percentilesPercentiles) 
			fields[[length(fields) + 1]] <- list(name=paste("pc", i, sep=""), title=paste(i, "th percentile", sep=""), type="number", format="sf:4")
	} 
  
	stats.results[["title"]] <- "Descriptive Statistics"
	stats.results[["schema"]] <- list(fields=fields)
	stats.results[["casesAcrossColumns"]] <- TRUE

	footnotes <- .newFootnotes()
	
	note.symbol <- "<i>Note.</i>"
	na.for.categorical <- "Not all values are available for nominal and ordinal variables"
	
	
	stats.values <- list()

	for (variable in variables) {

		variable.results <- list(Variable=variable)
		column <- dataset[[ .v(variable) ]]

		if (perform == "run") {

			rows <- nrow(dataset)
			na.omitted <- na.omit(column)
			
			variable.results[["Valid"]] = length(na.omitted)
			variable.results[["Missing"]] = rows - length(na.omitted)
		}
		else {

			na.omitted <- column

			variable.results[["Valid"]] = "."
			variable.results[["Missing"]] = "."
		}



		if (central.tendency[["mean"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run")
					variable.results[["Mean"]] <- .clean(mean(na.omitted))
				else
					variable.results[["Mean"]] <- "."
				
			} else {
			
				variable.results[["Mean"]] <- ""				
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (central.tendency[["median"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")			
					variable.results[["Median"]] <- .clean(median(na.omitted))
				else
					variable.results[["Median"]] <- "."
				
			} else {
			
				variable.results[["Median"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (central.tendency[["mode"]]) {
	
			if (base::is.factor(na.omitted) == FALSE) {
		
				if (perform == "run") {

					mode <- as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))

					if (length(mode) > 1) {

						index <- .addFootnote(footnotes, "More than one mode exists, only the first is reported")
						variable.results[[".footnotes"]] <- list(Mode=list(index))
					}
		
					variable.results[["Mode"]] <- .clean(mode[1])
					
				} else {
				
					variable.results[["Mode"]] <- "."
				}
			
			} else {
		
				variable.results[["Mode"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		
		
		}
		
		if (central.tendency[["sum"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")
					variable.results[["Sum"]] <- .clean(sum(na.omitted))
				else
					variable.results[["Sum"]] <- "."
				
			} else {
			
				variable.results[["Sum"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["maximum"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")			
					variable.results[["Maximum"]] <- .clean(max(na.omitted))
				else
					variable.results[["Maximum"]] <- "."
				
			} else {
			
				variable.results[["Maximum"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["minimum"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run")
					variable.results[["Minimum"]] <- .clean(min(na.omitted))
				else
					variable.results[["Minimum"]] <- "."
				
			} else {
			
				variable.results[["Minimum"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["range"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")			
					variable.results[["Range"]] <- .clean(range(na.omitted)[2]-range(na.omitted)[1])
				else
					variable.results[["Range"]] <- "."
				
			} else {
			
				variable.results[["Range"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["standardDeviation"]]) {
		
			if (base::is.factor(na.omitted) == FALSE){
			
				if (perform == "run")
					variable.results[["Std. Deviation"]] <- .clean(sd(na.omitted))
				else
					variable.results[["Std. Deviation"]] <- "."
				
			} else {
			
				variable.results[["Std. Deviation"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["standardErrorMean"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")
					variable.results[["Std. Error of Mean"]] <- .clean(sd(na.omitted)/sqrt(length(na.omitted)))
				else
					variable.results[["Std. Error of Mean"]] <- "."
				
			} else {
			
				variable.results[["Std. Error of Mean"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (dispersion[["variance"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run")
					variable.results[["Variance"]] <- .clean(var(na.omitted))
				else
					variable.results[["Variance"]] <- "."
				
			} else {
			
				variable.results[["Variance"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (distribution[["kurtosis"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {

				if (perform == "run") {
				
					variable.results[["Kurtosis"]] <- .clean(.descriptivesKurtosis(na.omitted))
					variable.results[["Std. Error of Kurtosis"]] <- .clean(.descriptivesSEK(na.omitted))
					
				} else {
					variable.results[["Kurtosis"]] <- "."
					variable.results[["Std. Error of Kurtosis"]] <- "."
				}
				
			} else {
			
				variable.results[["Kurtosis"]] <- ""
				variable.results[["Std. Error of Kurtosis"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (distribution[["skewness"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run") {
				
					variable.results[["Skewness"]] <- .clean(.descriptivesSkewness(na.omitted))
					variable.results[["Std. Error of Skewness"]] <- .clean(.descriptivesSES(na.omitted))
					
				} else {

					variable.results[["Skewness"]] <- "."
					variable.results[["Std. Error of Skewness"]] <- "."
				}
				
			} else {
			
				variable.results[["Skewness"]] <- ""
				variable.results[["Std. Error of Skewness"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (percentileValues[["quartiles"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run") {
				
					variable.results[["q1"]] <- .clean(quantile(na.omitted, c(.25), type=6, names=F))
					variable.results[["q2"]] <- .clean(quantile(na.omitted, c(.5), type=6, names=F))
					variable.results[["q3"]] <- .clean(quantile(na.omitted, c(.75), type=6, names=F))
					
				} else {
				
					variable.results[["q1"]] <- "."
					variable.results[["q2"]] <- "."
					variable.results[["q3"]] <- "."
				}
				
			} else {
			
				variable.results[["q1"]] <- ""
				variable.results[["q2"]] <- ""
				variable.results[["q3"]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		if (percentileValues[["equalGroups"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run") {
			
					for (i in seq(equalGroupsNo - 1))
						variable.results[[paste("eg", i, sep="")]] <- .clean(quantile(na.omitted, c(i / equalGroupsNo), type=6, names=F))
					
				} else {

					for (i in seq(equalGroupsNo - 1))
						variable.results[[paste("eg", i, sep="")]] <- "."
				}
					
			} else {
			
				for (i in seq(equalGroupsNo - 1))
					variable.results[[paste("eg", i, sep="")]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
			
		if (percentileValues[["percentiles"]]) {
		
			if (base::is.factor(na.omitted) == FALSE) {
			
				if (perform == "run") {
			
					for (i in percentilesPercentiles)
						variable.results[[paste("pc", i, sep="")]] <- .clean(quantile(na.omitted, c(i / 100), type=6, names=F))
					
				} else {
				
					for (i in percentilesPercentiles)
						variable.results[[paste("pc", i, sep="")]] <- "."
				}
					
			} else {
			
				for (i in percentilesPercentiles)
					variable.results[[paste("pc", i, sep="")]] <- ""
				.addFootnote(footnotes, na.for.categorical, note.symbol)
			}
		}
		
		stats.values[[length(stats.values) + 1]] <- variable.results
			
	
		stats.results[["data"]] <- stats.values
		stats.results[["footnotes"]] <- as.list(footnotes)
	}

	results[["stats"]] <- stats.results

	#### FREQUENCIES TABLES

	if (options$main$displayFrequencyTables) {
	
		frequency.tables <- list()
		
		for (variable in variables) {
	
			column <- dataset[[ .v(variable) ]]
		
			if (base::is.factor(column) == FALSE)
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
		
			lvls <- levels(dataset[[ .v(variable) ]])

			if (perform == "run") {

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

				for (level in lvls)
					data[[length(data)+1]] <- list(level=level)
					
				data[[length(data)+1]] <- list(level="Total", "Cumulative Percent"="")
				
				frequency.table[["data"]] <- data
			}
		
			frequency.tables[[length(frequency.tables)+1]] <- frequency.table
		}
		
		if (length(frequency.tables) > 0)
			results[["frequenciesHeading"]] <- "Frequencies"
		
		results[["tables"]] <- frequency.tables
	}

    ####  PLOTS
	
	if (options$plots == TRUE) {
		
		frequency.plots <- list()
			
		i <- 1
	
		for (variable in variables) {
	
			column <- dataset[[ .v(variable) ]]
			
			
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- options$chartWidth
			plot[["height"]] <- options$chartHeight
			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
			
			frequency.plots[[i]] <- plot
			i <- i + 1
		}
			
		results[["plots"]] <- frequency.plots
	
		if (perform=="run") {
				
			i <- 1
	
			for (variable in variables) {
<<<<<<< HEAD
				
				if (callback(results) != 0)
					return()
					
				column <- dataset[[ .v(variable) ]]				
				column <- na.omit(column)
								
				if (length(column) > 0 && is.factor(column) || length(column) > 0 && all(!is.infinite(column)) && all(column %% 1 == 0) && length(unique(column)) <= 24) {
				
					if (!is.factor(column)) {
					
=======
		
				column <- dataset[[ .v(variable) ]]
				
				if (is.factor(column)) {
					
					column <- na.omit(column)
					column <- as.character(column)
					column <- column[column != "NA"]					
					column <- as.factor(column)
				} else {
				
					column <- column[!is.na(column)]
				}
				
				
				if (length(column) > 0 && is.factor(column) || length(column) > 0 && all(!is.infinite(column)) && all(column %% 1 == 0) && length(unique(column)) <= 24) {
				
					if (!is.factor(column)) {
					
>>>>>>> Descriptives plots: fixes #142 and fixes #198
						column <- as.factor(column)
					}
				
					image <- .beginSaveImage(options$chartWidth, options$chartHeight)
										
					.barplotJASP(column, variable)
					
					content <- .endSaveImage(image)
					
					plot <- frequency.plots[[i]]
					
					plot[["data"]]  <- content
<<<<<<< HEAD
					plot[["status"]] <- "complete"
					
					frequency.plots[[i]] <- plot
										
				} else if (length(column) > 0 && !is.factor(column) && all(!is.infinite(column))) {
				
=======
					
					frequency.plots[[i]] <- plot
					
					i <- i + 1
										
				} else if (length(column) > 0 && !is.factor(column) && all(!is.infinite(column))) {
				
					if (callback(results) != 0)
						return()
				
>>>>>>> Descriptives plots: fixes #142 and fixes #198
					image <- .beginSaveImage(options$chartWidth, options$chartHeight)
				
					.plotMarginal(column, variableName= variable)
				
					content <- .endSaveImage(image)
					
					plot <- frequency.plots[[i]]
					
					plot[["data"]]  <- content
					plot[["status"]] <- "complete"
					
					frequency.plots[[i]] <- plot
<<<<<<< HEAD
			
				}				
						
				results[["plots"]] <- frequency.plots	

				i <- i + 1
=======
					
					i <- i + 1				
				}				
						
				results[["plots"]] <- frequency.plots				
>>>>>>> Descriptives plots: fixes #142 and fixes #198
			}
		}
		
	}
	
	if (perform == "init") {
	
		if (length(variables) == 0) {
		
			return(list(results=results, status="complete"))
			
		} else {
		
			return(list(results=results, status="inited"))
		}
		
	} else {
	
		return(list(results=results, status="complete"))
	}
}

<<<<<<< HEAD
=======

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

.barplotJASP <- function(column, variable){

	yticks <- seq(0,max(summary(column)),1)
	yticks <- pretty(yticks)
	
	yLabs <- vector("character", length(yticks))
	
	for(i in seq_along(yticks)){
		
		if(yticks[i] < 10^6){
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
			
		} else{
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}		
	}
	
	distLab <- max(nchar(yLabs))/1.8
	
	par(mar= c(5, 7.2, 4, 2) + 0.1)
	barplot(summary(column), cex.names= 1.3, axes= FALSE, ylim= range(yticks))
	axis(2, las=1, at= yticks, labels= yLabs, cex.axis= 1.4)
	mtext(text = variable, side = 1, cex=1.5, line = 3)
	mtext(text = "Frequency", side = 2, cex=1.5, line = distLab+2, las=0)
}

.plotMarginal <- function(variable, variableName, cexYlab= 1.3, lwd= 2, rugs= FALSE){

	par(mar= c(5, 4.5, 4, 2) + 0.1)
	
	density <- density(variable)
	
	h <- hist(variable, plot = FALSE)
	jitVar <- jitter(variable)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(variable, h$breaks), min.n= 3)
	
	plot(1, xlim= range(xticks), ylim= c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks, cex.axis = 1.2)
	mtext(text = variableName, side = 1, cex=1.5, line = 3)
	par(las=0)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), mgp=c(3,0.2,0), cex.axis= 1.5, mgp= c(3, 0.7, 0))
	
	if(rugs){
		rug(jitVar)
	}
	
	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)
}

>>>>>>> Descriptives plots: fixes #142 and fixes #198
