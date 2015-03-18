#### histogram with density estimator ####
.plotMarginalCor <- function(variable, cexYlab= 1.3, lwd= 2, rugs= FALSE){
	
	variable <- variable[!is.na(variable)]
	
	density <- density(variable)
	h <- hist(variable, plot = FALSE)
	jitVar <- jitter(variable)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(variable, h$breaks), min.n= 3)
	plot(range(xticks), c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks)
	par(las=0)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.08*diff(range(ax1)), cex.axis= 1.7, mgp= c(3, 0.7, 0))
	
	if(rugs){
		rug(jitVar)
	}
	
	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)

}


#### scatterplots ####
.plotScatter <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2){
	
	d <- data.frame(xx= xVar, yy= yVar)
	d <- na.omit(d)
	xVar <- d$xx
	yVar <- d$yy
	
	# fit different types of regression
	fit <- vector("list", 1)# vector("list", 4)
	fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
	# fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
	# fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
	# fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)
	
	# find parsimonioust, best fitting regression model
	# Bic <- vector("numeric", 4)
	# for(i in 1:4){
	#	Bic[i] <- BIC(fit[[i]])	
	# }
	
	bestModel <- 1 # which.min(Bic)
	
	# predictions of the model
	poly.pred <- function(fit, line=FALSE, xMin, xMax){
	
		# create function formula
		f <- vector("character", 0)
		
		for(i in seq_along(coef(fit))){
		
			if(i == 1){
			
				temp <- paste(coef(fit)[[i]])
				f <- paste(f, temp, sep="")
			}
			
			if(i > 1){
			
				temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
				f <- paste(f, temp, sep="+")
			}
		}
		
		x <- seq(xMin, xMax, length.out = 100)
		predY <- eval(parse(text=f))
		
		if(line == FALSE){
		return(predY)
		}
		
		if(line){
		lines(x, predY, lwd=lwd)
		}
	}
	
	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))
	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)])))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)])))
	

	yticks <- pretty(c(ylow, yhigh))
	
	yLabs <- vector("character", length(yticks))
	
	for(i in seq_along(yticks)){
		
		if(yticks[i] < 10^6){
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
			
		} else{
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}		
	}
	
	plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)
	poly.pred(fit[[bestModel]], line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)])
	
	par(las=1)
	
	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis)
	axis(2, line= 0.2, labels= yLabs, at= yticks, cex.axis= cexYAxis)
	
	invisible(max(nchar(yLabs)))

}

#### display correlation value ####
.plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.7, hypothesis = "correlated", pearson=options$pearson,
	kendallsTauB=options$kendallsTauB, spearman=options$spearman) {

	CIPossible <- TRUE
	
	tests <- c()
	
	if (pearson)
		tests <- c(tests, "pearson")
		
	if (spearman)
		tests <- c(tests, "spearman")
		
	if (kendallsTauB)
		tests <- c(tests, "kendall")

		
	plot(1,1, type="n", axes=FALSE, ylab="", xlab="")
	
	lab <- vector("list")
	
	for(i in seq_along(tests)){
	
		if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == 1){
		
			CIPossible <- FALSE
		
			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == "1.000")
			}
			
			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(italic(rho) == "1.000")
			}
			
			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(italic(tau) == "1.000")
			}
			
		} else if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == -1){
		
			CIPossible <- FALSE
		
			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == "-1.000")
			}
			
			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(italic(rho) == "-1.000")
			}
			
			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(italic(tau) == "-1.000")
			}
			
		} else {
		
			if(tests[i] == "pearson"){
				lab[[i]] <- bquote(italic(r) == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}
			
			if(tests[i] == "spearman"){
				lab[[i]] <- bquote(rho == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}
			
			if(tests[i] == "kendall"){
				lab[[i]] <- bquote(tau == .(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
			}
		}
	}

	
	if(length(tests) == 1){
		ypos <- 1
	}
	
	if(length(tests) == 2){
		ypos <- c(1.1, 0.9)
	}
	
	if(length(tests) == 3){
		ypos <- c(1.2, 1, 0.8)
	}
	
	
	for(i in seq_along(tests)){
	
		text(1, ypos[i], labels= lab[[i]], cex= cexText)
	}
	
	
	if(hypothesis == "correlated" & length(tests) == 1 & any(tests == "pearson")){
		
		alternative <- "two.sided"
		ctest <- cor.test(xVar, yVar, method= tests)
	}
	
	if(hypothesis != "correlated" & length(tests) == 1 & any(tests == "pearson")){
		
		if (hypothesis == "correlatedPositively") {
		
			ctest <- cor.test(xVar, yVar, method=tests, alternative="greater")
			# p.value  <- min(as.numeric(result1[[1]]$p.value), as.numeric(result1[[2]]$p.value))
		} else if (hypothesis == "correlatedNegatively") {
		
			ctest <- cor.test(xVar, yVar, method=tests, alternative="less")
		}
		
		# ctest <- result1[[which.min(c(as.numeric(result1[[1]]$p.value), as.numeric(result1[[2]]$p.value)))]]
	}
	
	
	if(any(tests == "pearson")& length(tests) == 1 && CIPossible){
	
		CIlow <- formatC(round(ctest$conf.int[1],2), format = "f",digits = 2)
		CIhigh <- formatC(round(ctest$conf.int[2],2), format = "f",digits = 2)
		
		text(1,0.8, labels= paste("95% CI: [", CIlow, ", ", CIhigh, "]", sep=""), cex= cexCI)
	}
}

### display dash
#.plotDash <- function(x= .2, lwd= 2){
#	
#	plot(1, 1, type= "n", axes=FALSE, xlab= "", ylab= "")
#	segments(1 - x/2, 1, 1 + x/2, 1, lwd= lwd)
#}

Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

print(options)

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, exclude.na.listwise=options$variables)
				
			} else {

				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=options$variables)
		}
	}

	results <- list()
	
	meta <- list(
		list(name="title", type="title"),
		list(name="correlations", type="table"),
		list(name="plots", type="images"))
	
	results[[".meta"]] <- meta
	
	results[["title"]] <- "Correlation Matrix"
	
	frequency.plots <- list()
	
	if (perform == "init" & options$plotCorrelationMatrix) {
	
		variables <- unlist(options$variables)
		
		l <- length(variables)
		
			l <- length(variables)
			
		if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
			
			width <- 580
			height <- 580
				
		} else if (l <= 2) {
			
			width <- 400
			height <- 400
				
		} else {
			
			width <- 250 * l
			height <- 250 * l
				
		}
				
		plot <- list()
			
		plot[["title"]] <- "" # variables #paste(variables,  collapse= ",")
		plot[["width"]]  <- width
		plot[["height"]] <- height
		
		frequency.plots[[1]] <- plot
	}
	
	if (perform == "run" && length(options$variables) > 0 && options$plotCorrelationMatrix) {
				
		variables <- unlist(options$variables)
		
		# check for numeric/integer variables					
		d <- vector("character", length(.v(variables)))
		sdCheck <- vector("numeric", length(.v(variables)))
		infCheck <- vector("logical", length(.v(variables)))
		
		for (i in seq_along(.v(variables))) {
		
			d[i] <- class(dataset[[.v(variables)[i]]])
			sdCheck[i] <- sd(dataset[[.v(variables)[i]]], na.rm=TRUE)
			infCheck[i] <- any(is.infinite(dataset[[.v(variables)[i]]]) == TRUE)
		}
		
	
		ind1 <- d == "numeric" | d == "integer"
		ind2 <- sdCheck > 0
		ind <- ind1 & ind2 & infCheck == FALSE
		
				
		variables <- .v(variables)[ind]

		if (length(variables) > 0) {
			
			l <- length(variables)
			
			if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
			
				width <- 580
				height <- 580
				
			} else if (l <= 2) {
			
				width <- 400
				height <- 400
				
			} else {
			
				width <- 250 * l
				height <- 250 * l
				
			}
			
			frequency.plots <- list()
					
			plot <- list()
				
			plot[["title"]] <- "" # .unv(variables)
			plot[["width"]]  <- width
			plot[["height"]] <- height
					
			frequency.plots[[1]] <- plot	
			
			image <- .beginSaveImage(width, height)
			
			if (l == 1) {
			
				par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
				
				.plotMarginalCor(dataset[[variables[1]]]) # plot marginal (histogram with density estimator)
				mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)
				
			} else if (l == 2 && !options$plotDensities && !options$plotStatistics) {
				
				par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
				
				maxYlab <- .plotScatter(dataset[[variables[1]]], dataset[[variables[2]]])
				distLab <- maxYlab / 1.8
				
				mtext(text = .unv(variables)[1], side = 1, cex=1.5, line = 3)
				mtext(text = .unv(variables)[2], side = 2, cex=1.5, line = distLab + 2, las=0)
				
			} else if (l > 1) {
			
				par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))
			
				for (row in seq_len(l)) {
				
					for (col in seq_len(l)) {
					
						if (row == col) {
							
							if (options$plotDensities) {
							
								.plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
								
							} else {
							
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								
							}
						}
							
						if (col > row) {
						
							if (options$plotCorrelationMatrix) {
							
								.plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
								
							} else {
							
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								
							}
						}
						
						if (col < row) {
						
							if (l < 7) {
							
								if (options$plotStatistics) {
								
									.plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], hypothesis= options$hypothesis,
									pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman) # plot r= ...
									
								} else {
								
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
									
								}
							}
								
							if (l >= 7) {
							
								if (options$plotStatistics) {
								
									.plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2, hypothesis= options$hypothesis,
									pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman)
									
								} else {
								
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
									
								}
							}
						}		
					}
				}
			}
			
			
			if (l > 2 || ((l == 2 && options$plotDensities) || (l == 2 && options$plotStatistics))) {
			
				textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
				
				if (!options$plotDensities && !options$plotStatistics) {
				
						for (t in seq_along(textpos)) {
						
							mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
							
							if (t < length(textpos)) {
							
								mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
								
							}
						}
					
				} else {
				
					for (t in seq_along(textpos)) {
						
							mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
							mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
					}
				}
			}
			
			content <- .endSaveImage(image)
					
			plot <- frequency.plots[[1]]
					
			plot[["data"]]  <- content
					
			frequency.plots[[1]] <- plot	
			
		}	
	}
	
	results[["plots"]] <- frequency.plots
		
	results[["correlations"]] <- .correlationTable(dataset, perform,
	variables=options$variables, pearson=options$pearson,
	kendallsTauB=options$kendallsTauB, spearman=options$spearman,
	hypothesis=options$hypothesis, reportSignificance=options$reportSignificance,
	flagSignificant=options$flagSignificant,
	meansAndStdDev=options$meansAndStdDev, crossProducts=options$crossProducts)
	
	if (perform == "init") {
	
		if (length(options$variables) < 2) {
		
			results <- list(results=results, status="complete")
			
		} else {
		
			results <- list(results=results, status="inited")
		}
		
	} else {
	
		results <- list(results=results, status="complete")
	}
	
	results
}

.correlationTable <- function(dataset, perform, variables=c(), pearson=TRUE, kendallsTauB=FALSE,
	spearman=FALSE, hypothesis="correlated", reportSignificance=FALSE,
	flagSignificant=FALSE, meansAndStdDev=FALSE, crossProducts=FALSE) {
	
	correlation.table <- list()
	
	if (perform == "init") {
	
		if (length(variables) < 2)
			variables <- c(variables, "...")
		if (length(variables) < 2)
			variables <- c(variables, "... ")
	}
	
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")

	if (length(tests) != 1) {
	
		correlation.table[["title"]] <- "Correlation Table"
	}
	else if (pearson) {
	
		correlation.table[["title"]] <- "Pearson Correlations"
	}
	else if (spearman) {
	
		correlation.table[["title"]] <- "Spearman Correlations"
	}
	else if (kendallsTauB) {
	
		correlation.table[["title"]] <- "Kendall's Tau"
	}
	else {
	
		correlation.table[["title"]] <- "Correlation Table"
	}
	
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	
	footnotes <- .newFootnotes()
	
	if (flagSignificant || reportSignificance) {
	
		if (hypothesis == "correlatedPositively") {

			.addFootnote(footnotes, "all tests one-tailed, for positive correlation", symbol="<i>Note</i>.")
			
		} else if (hypothesis == "correlatedNegatively") {
		
			.addFootnote(footnotes, "all tests one-tailed, for negative correlation", symbol="<i>Note</i>.")
		}
	}
	
	if (flagSignificant) {
	
		if (hypothesis == "correlated") {
		
			.addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001", symbol="*")
			
		} else {
		
			.addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001, one-tailed", symbol="*")
		}
	}
	
	v.c <- length(variables)
	
	if (v.c > 0) {
			
		test.names <- list(pearson="Pearson's R", spearman="Spearman's Rho", kendall="Kendall's Tau B")

		column.names <- c()

		for (test in tests) {
		
			if (length(tests) > 1 || reportSignificance) {
			
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
				
			for (variable.name in variables) {
			
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			if (reportSignificance) {
			
				column.name <- paste(".test[", test, "-p]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")

				
				for (variable.name in variables) {
			
					column.name <- paste(variable.name, "[", test, "-p]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3;p:.001")
				}
			}
		}
		
		for (i in 1:v.c) {

			row <- list()
			row.footnotes <- list()
			
			variable.name <- variables[[i]]
			
			for (test in tests) {
			
				p.values <- list()

				if (length(tests) > 1 || reportSignificance)
					row[[length(row)+1]] <- test.names[[test]]
					
				if (reportSignificance)
					p.values[[length(p.values)+1]] <- "p-value"
			
				for (j in .seqx(1, i-1)) {
				
					row[[length(row)+1]] <- ""
					p.values[[length(p.values)+1]] <- ""
				}

				row[[length(row)+1]] <- "\u2014" # em-dash
				p.values[[length(p.values)+1]] <- ""

				for (j in .seqx(i+1, v.c)) {

					variable.2.name <- variables[[j]]
					column.name <- paste(variable.2.name, "[", test, "]", sep="")
					
					v1 <- dataset[[ .v(variable.name) ]]
					v2 <- dataset[[ .v(variable.2.name) ]]
					
							
					if (perform == "run") {
				
						if (hypothesis == "correlated") {

							result <- cor.test(v1, v2, method=test, alternative="two.sided")
						}
						else if (hypothesis == "correlatedPositively") {
						
							result <- cor.test(v1, v2, method=test, alternative="greater")
							
						} else {
						
							result <- cor.test(v1, v2, method=test, alternative="less")
						}

						estimate <- as.numeric(result$estimate)
						p.value  <- as.numeric(result$p.value)
						
						
						if (is.na(estimate)) {
							
							if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
							
								index <- .addFootnote(footnotes, "Correlation co-efficient is undefined - one (or more) variables contain infinity")
								row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
								
							}
						}
						
						row[[length(row)+1]] <- .clean(estimate)
						
						if (flagSignificant && is.na(p.value) == FALSE) {

							if (p.value < .001) {
							
								row.footnotes[[column.name]] <- list("***")
							
							} else if (p.value < .01) {
							
								row.footnotes[[column.name]] <- list("**")
							
							} else if (p.value < .05) {
							
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						if (reportSignificance)
							p.values[[length(p.values)+1]] <- .clean(p.value)
					
					} else {
				
						row[[length(row)+1]] <- "."
						p.values[[length(p.values)+1]] <- "."
						
					}
				}
				
				if (reportSignificance) {

					for (p.value in p.values)
						row[[length(row)+1]] <- p.value
				}
			
			}
	
			names(row) <- column.names
			row[[".variable"]] <- variable.name
			
			if (length(row.footnotes) > 0)
				row[[".footnotes"]] <- row.footnotes
	
			rows[[i]] <- row
		}
	}
	
	schema <- list(fields=fields)
	
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	correlation.table[["footnotes"]] <- as.list(footnotes)
	
	correlation.table
}
