
Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

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
		list(name="correlations", type="table"),
		list(name="plots", type="images")
		)
	
	results[[".meta"]] <- meta
	
	#### histogram with density estimator ####
	plotMarginal <- function(variable, cexYlab= 1.3, lwd= 2){
	density <- density(variable)
	h <- hist(variable, plot = FALSE)
	jitVar <- jitter(variable)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(variable, jitVar), min.n= 3)
	plot(range(xticks), c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(variable, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(variable)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks)
	par(las=0)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.08*diff(range(ax1)), mgp=c(3,0.2,0), cex.axis= 1.7, mgp= c(3, 0.7, 0))
	rug(jitVar)
	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)
	}


	#### scatterplots ####
	plotScatter <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2){
	
	# fit different types of regression
	fit <- vector("list", 4)
	fit[[1]] <- lm(yVar ~ poly(xVar, 1, raw= TRUE))
	fit[[2]] <- lm(yVar ~ poly(xVar, 2, raw= TRUE))
	fit[[3]] <- lm(yVar ~ poly(xVar, 3, raw= TRUE))
	fit[[4]] <- lm(yVar ~ poly(xVar, 4, raw= TRUE))
	
	# find parsimonioust, best fitting regression model
	Bic <- vector("numeric", 4)
	for(i in 1:4){
		Bic[i] <- BIC(fit[[i]])	
	}
	bestModel <- which.min(Bic)
	
	# predictions of the model
	poly.pred <- function(fit, line=FALSE){
		# create function formula
		f <- vector("character", 0)
		for(i in seq_along(coef(fit))){
			if(i ==1){
				temp <- paste(coef(fit)[[i]])
				f <- paste(f, temp, sep="")
			}
			if(i >1){
				temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
			f <- paste(f, temp, sep="+")
			}
		}
		x <- seq(min(xVar), max(xVar), length.out = 100)
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
	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(poly.pred(fit[[bestModel]], line= FALSE)))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(poly.pred(fit[[bestModel]], line= FALSE)))
	xticks <- pretty(c(xlow, xhigh))
	yticks <- pretty(c(ylow, yhigh))
	plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)
	poly.pred(fit[[bestModel]], line= TRUE)
	par(las=1)
	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis)
	axis(2, line= 0.2, labels= yticks, at= yticks, cex.axis= cexYAxis)
	}
	
	
	#### display correlation value ####
	plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.8){
	plot(1,1, type="n", axes=FALSE, ylab="", xlab="")
	
	if(cor(xVar, yVar)>= 0 & cor(xVar, yVar) < 1){
	lab=bquote(italic(r) == .(substr(x = formatC(round(cor(xVar, yVar),2), format="f", digits= 2), start=2, stop=4)))
	text(1,1, labels= lab, cex= cexText)
	}
	if(cor(xVar, yVar)<0){
	lab=bquote(italic(r) == -.(substr(x = formatC(round(cor(xVar, yVar),2), format= "f", digits= 2), start=3, stop=5)))
	text(1,1, labels= lab, cex= cexText)
	}
	if(cor(xVar, yVar) == 1){
		lab=bquote(italic(r) == 1)
	text(1,1, labels= lab, cex= cexText)
	}
	
	ctest <- cor.test(xVar, yVar)
	CIlow <- formatC(round(ctest$conf.int[1],2), format = "f",digits = 2)
	CIhigh <- formatC(round(ctest$conf.int[2],2), format = "f",digits = 2)
	if(CIlow < 0){
		CIlow <- paste("-", substr(CIlow, 3, 5), sep="")
	}
	if(CIlow > 0){
		CIlow <- substr(CIlow, 2, 4)
	}
	if(CIhigh < 0){
		CIhigh <- paste("-", substr(CIhigh, 3, 5), sep="")
	}
	if(CIhigh > 0){
		CIhigh <- substr(CIhigh, 2, 4)
	}
	text(1,0.8, labels= paste("95% CI: [", CIlow, ", ", CIhigh, "]", sep=""), cex= cexCI)
	}
	
	if (perform == "run" & length(options$variables) > 0) {
				
		variables <- unlist(options$variables)
		l <- length(variables)
		if(l <= 2){
			width <- 500
			height <- 500
		}
		if(l == 3){
			width <- 550
			height <- 550
		}
		if(l == 4){
			width <- 900
			height <- 900
		}
		if(l >= 5){
			width <- 1100
			height <- 1100
		}
	
		frequency.plots <- list()
				
		plot <- list()
			
		plot[["title"]] <- variables
		plot[["width"]]  <- width
		plot[["height"]] <- height
				
			
		frequency.plots[[1]] <- plot
				
	
		if (perform=="run" & length(options$variables) >0) {
				
			i <- 1
													
			# check for numeric/integer variables
					
			d <- vector("character", length(.v(variables)))
			sdCheck <- vector("numeric", length(.v(variables)))
			for(i in seq_along(.v(variables))){
				d[i] <- class(dataset[[.v(variables)[i]]])
				sdCheck[i] <- sd(dataset[[.v(variables)[i]]])
			}
			ind1 <- which(d == "numeric" | d == "integer")
			ind2 <- which(sdCheck > 0)
			ind <- ind1 %in% ind2
			variables <- .v(variables)[ind]
					
	
		image <- .beginSaveImage(width, height)
		if(l == 1){
		par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 0, 0, 0))	
		}
		if(l > 1){
		par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0, 2.2, 2, 0))
		}
		for(row in seq_len(l)){
			for(col in seq_len(l)){
				if(row == col){
					plotMarginal(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
				}
				if(col > row){
					plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
				}
				if(col < row){
					if(l < 7){
						plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]]) # plot r= ...
					}
					if(l >= 7){
						plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2) 
					}
				}		
			}
		}
		
		if(l == 1){
			mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)
		}
		if(l > 1){
		textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
			for(t in seq_along(textpos)){
				mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.9, line= -0.8)
				mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.9, line= -0.1)
			}
		}
		
		content <- .endSaveImage(image)
				
		plot <- frequency.plots[[1]]
				
		plot[["data"]]  <- content
				
		frequency.plots[[1]] <- plot
		results[["plots"]] <- frequency.plots
		}			
	}	
			
		
	results[["correlations"]] <- .correlationTable(dataset, perform,
	variables=options$variables, pearson=options$pearson,
	kendallsTauB=options$kendallsTauB, spearman=options$spearman,
	hypothesis=options$hypothesis, reportSignificance=options$reportSignificance,
	flagSignificant=options$flagSignificant,
	meansAndStdDev=options$meansAndStdDev, crossProducts=options$crossProducts)

	results
}

.correlationTable <- function(dataset, perform, variables=c(), pearson=TRUE, kendallsTauB=FALSE,
	spearman=FALSE, hypothesis="correlated", reportSignificance=FALSE,
	flagSignificant=FALSE, meansAndStdDev=FALSE, crossProducts=FALSE) {
	
	correlation.table <- list()
	
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
		
					if (perform == "run") {
				
						if (hypothesis == "correlated") {

							result   <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="two.sided")
							estimate <- as.numeric(result$estimate)

							p.value  <- as.numeric(result$p.value)
							
						}
						else {
						
							result1 <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="less")
							result2 <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="greater")
							estimate <- as.numeric(result1$estimate)

							p.value  <- min(as.numeric(result1$p.value), as.numeric(result2$p.value))
						}
						
						row[[length(row)+1]] <- .clean(estimate)
						
						if (flagSignificant && is.na(p.value) == FALSE) {
						
							column.name <- paste(variable.2.name, "[", test, "]", sep="")

							if (p.value < .001) {
							
								row.footnotes[[column.name]] <- list("***")
							
							} else if (p.value < .01) {
							
								row.footnotes[[column.name]] <- list("**")
							
							} else if (p.value < .05) {
							
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						if (reportSignificance)
							p.values[[length(p.values)+1]] <- p.value
					
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
	
	if (flagSignificant) {
	
		if (hypothesis == "correlated") {
		
			correlation.table[["footnotes"]] <- list(list(symbol="*", text="p < .05, ** p < .01, *** p < .001"))
			
		} else {
		
			correlation.table[["footnotes"]] <- list(list(symbol="*", text="p < .05, ** p < .01, *** p < .001, all one-tailed"))
		}
	}
	
	correlation.table
}
