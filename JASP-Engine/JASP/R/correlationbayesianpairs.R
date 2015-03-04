.plotPosterior.correlation <- function(r, n, alpha=1, oneSided= FALSE, BF, BFH1H0, addInformation= TRUE, dontPlotData=FALSE, drawCI= FALSE, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2) {	
	
	
	if (addInformation) {
	
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
		
	} else {
	
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
	}
	
	
	if (dontPlotData) {
	
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		mtext(expression("Population correlation" ~ rho), side = 1, cex = cexXlab, line= 2.6)
	
		return()
	}
	
	# set limits plot
	xlim <- c(-1, 1)
	
	if (oneSided == FALSE) {
		stretch <- 1.2
	}
	
	if (oneSided == "right") {
		stretch <- 1.32
	}
	
	if (oneSided == "left") {
		stretch <- 1.32
	}
	
	ylim <- vector("numeric", 2)
	
	if (oneSided == FALSE) {
		
		dmax <- optimize(f= function(x).posteriorRho(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
		
	} else if (oneSided == "right") {
		
		dmax <- optimize(f= function(x).posteriorRhoPlus(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
		
	} else if (oneSided == "left") {
		
		dmax <- optimize(f= function(x).posteriorRhoMin(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	}
	
	
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	# calculate position of "nice" tick marks and create labels
	xticks <- seq(-1.0, 1.0, 0.25)
	yticks <- pretty(ylim)
	xlabels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
	ylabels <- formatC(yticks, 1, format= "f")
	

	# compute 95% credible interval & median:
	if (drawCI) {
		
		if (oneSided == FALSE) {
		
			CIlow <- myQPosterior(p = 0.025, n = n, r = r)
			CIhigh <- myQPosterior(p = 0.975, n = n, r = r)
			medianPosterior <- myQPosterior(p = 0.5, n = n, r = r)
		}
	}
	
	rho <- seq(min(xticks), max(xticks),length.out = 1000)	
	
	if (oneSided == FALSE) {
		
		priorLine <- .priorRho(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRho(rho= rho, n= n, r= r, alpha= alpha)
		
	} else if (oneSided == "right") {
		
		priorLine <- .priorRhoPlus(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRhoPlus(rho= rho, n= n, r= r, alpha= alpha)
		
	} else if (oneSided == "left") {
		
		priorLine <- .priorRhoMin(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRhoMin(rho= rho, n= n, r= r, alpha= alpha)
	}	
	
	
	plot(1, 1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(rho, posteriorLine, lwd= lwd)
	lines(rho, priorLine, lwd= lwd, lty=3)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
		
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext(expression("Population correlation" ~ rho), side = 1, cex = cexXlab, line= 2.6)	
		
	
	evalPosterior <- posteriorLine[posteriorLine > 0]
	
	if (oneSided == "right") {
		
		heightPosteriorAtZero <- evalPosterior[1]
		points(0, .priorRhoPlus(0,alpha=alpha), col="black", pch=21, bg = "grey", cex= cexPoints)		
		points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
		
	} else if (oneSided == "left") {
		
		heightPosteriorAtZero <- evalPosterior[length(evalPosterior)]
		points(0, .priorRhoMin(0,alpha=alpha), col="black", pch=21, bg = "grey", cex= cexPoints)		
		points(1e-15, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
		
	} else {
		
		points(0, .priorRho(0,alpha=alpha), col="black", pch=21, bg = "grey", cex= cexPoints)		
		points(1e-15, .posteriorRho(rho=1e-8, n=n, r=r, alpha=alpha), col="black", pch=21, bg = "grey", cex= cexPoints)
	}
	
		
	# enable plotting in margin
	par(xpd=TRUE)	

	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")

	if (drawCI) {
		
		arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
		medianText <- formatC(medianPosterior, digits= 3, format="f")
	}	

	
	if (addInformation) {
		
		# if (oneSided == FALSE) {
		# 	
		# 	BF10 <- .bf10Corrie(n = n, r = r, alpha = 1)
		# 	
		# } else if (oneSided == "right") {
		# 	
		# 	BF10 <- .bfPlus0(n = n, r = r, alpha = 1)
		# 	
		# } else if (oneSided == "left") {
		# 	
		# 	BF10 <- .bfMin0(n = n, r = r, alpha = 1)
		# 	
		# }
		
		if (BFH1H0) {
		
			BF10 <- BF
			BF01 <- 1 / BF10
			
		} else {
		
			BF01 <- BF
			BF10 <- 1 / BF01
		}
		
		# display BF10 value
		offsetTopPart <- 0.06	
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000) {
			BF10t <- format(BF10, digits= 4, scientific = TRUE)
			BF01t <- format(BF01, digits= 4, scientific = TRUE)
		}
		
		if (BF10 < 1000000 & BF01 < 1000000) {
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (oneSided == FALSE) {
			
			text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		if (drawCI) {
			
			CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
			medianLegendText <- paste("median =", medianText)
			
			text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
			text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		}		

		
		# probability wheel
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		# make sure that colored area is centered		
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel		
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (oneSided == FALSE) {
			
			text(xx, yy, "data|H1", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy, "data|H+", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy, "data|H-", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
 		# add legend		
# 		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
# 		
# 		medianLegendText <- paste("median =", medianText)
	}
	
	if (oneSided == "right") {
		
		legendPosition <- min(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1)
		
	} else if (oneSided == "left") {
		
		legendPosition <- max(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1)
		
	} else if (oneSided == FALSE) {
		
		if (r >= 0) {
			
			legendPosition <- min(xticks)
			legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1)
			
		} else {
			
			legendPosition <- max(xticks)
			legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1)
			
		}		
	}
}

CorrelationBayesianPairs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]
	
	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
		
	} else {
	
		if (options$missingValues == "excludeListwise") {
	
			dataset <- .vdf(dataset, columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
		
		} else {
	
			dataset <- .vdf(dataset, columns.as.numeric=all.variables)
		}	
	}
	
	dataset <- na.omit(dataset)	
	

	results <- list()	
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="correlation", type="table")
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Pairs"
	
	correlation <- list()
	
	correlation[["title"]] <- "Bayesian Pearson Correlation"
	
	correlation[["citation"]] <- list(
		"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
		)
	
	
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
	
		BFH1H0 <- TRUE
	
		if (options$hypothesis == "correlated") {
		
			bf.title <- "BF\u2081\u2080"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "BF\u208A\u2080"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "BF\u208B\u2080"
			oneSided <- "left"
		}
		
	} else if (bf.type == "LogBF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "correlated") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
			oneSided <- "left"
		}
		
	} else if (bf.type == "BF01") {
	
		BFH1H0 <- FALSE
	
		if (options$hypothesis == "correlated") {
		
			bf.title <- "BF\u2080\u2081"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "BF\u2080\u208A"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "BF\u2080\u208B"
			oneSided <- "left"
		}
	}
	
	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="separator", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="r", type="number", format="sf:4;dp:3", title="r"),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)
		)
		
	correlation[["schema"]] <- list(fields=fields)
	
	correlation.rows <- list()
	
	pair.statuses <- list()
	
	footnotes <- .newFootnotes()	
	
	plots.correlation <- list()
	
	
	for (pair in options$pairs)	{
	
		if (options$plotPriorAndPosterior) {

			plot <- list()
			
			plot[["title"]] <- paste(pair, collapse=" - ")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "running"
			
			image <- .beginSaveImage(530, 400)
			.plotPosterior.correlation(r=NULL, n=NULL, oneSided=oneSided, dontPlotData=TRUE, addInformation=options$plotPriorAndPosteriorAdditionalInfo)
			plot[["data"]] <- .endSaveImage(image)
						
			plots.correlation[[length(plots.correlation)+1]] <- plot
		}
	}
	
	results[["plots"]] <- plots.correlation
	
	rs <- numeric()
	ns <- numeric()
	BF10post <- numeric()
	
	for (i in .indices(options$pairs)) {
	
		index <- NULL
	
		pair <- options$pairs[[i]]
	
		if (pair[[1]] == "" || pair[[2]] == "") {
		
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
		
			pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)

			result <- list(.variable1=p1, .separator="-", .variable2=p2, BF="", error="")
		
		} else {

			if (perform == "init") {
			
				pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)

				result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=".", error=".")		
			
			} else {
			
				unplotable <- FALSE
				unplotableMessage <- NULL
				
				subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				subDataSet <- na.omit(subDataSet)
				
				v1 <- subDataSet[[ .v(pair[[1]]) ]]
				v2 <- subDataSet[[ .v(pair[[2]]) ]]
				
				#----------------------- compute r & BF ----------------------#
				some.r <- cor(v1, v2)
				some.n <- length(v1)
				
								
				# Note: Data and bfs check [start]
				if (is.na(some.r) || some.n <= 1) {
				
					# Note: Data: NOT ok, 
					# 		bf10: can't
					if (some.n <= 1){
					
						index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - not enough observations")
						unplotable <- TRUE
						unplotableMessage <- "Sample correlation co-efficient r is undefined - not enough observations"						
						
					} else if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
					
						index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - one (or more) variables contain infinity")
						unplotable <- TRUE
						unplotableMessage <- "Sample correlation co-efficient r is undefined - one (or more) variables contain infinity"
						
					} else {
					
						index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - one (or more) variables do not vary")
						unplotable <- TRUE
						unplotableMessage <- "Sample correlation co-efficient r is undefined - one (or more) variables do not vary"
					}
					#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
					#row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
					
					some.r <- NaN
					some.bf10 <- NaN
					some.bfPlus0 <- NaN
					some.bfMin0 <- NaN
					
				} else {
				
					all.bfs <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
					method.number <- 1
					
					while (any(is.na(all.bfs)) && method.number <=3){
					
						# Note: Try all normal methods
						all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, alpha=1, method=method.number)
						method.number <- method.number + 1
					}
					
					if (any(is.na(all.bfs))){
					
						# Note: all normal methods FAILED. Use Jeffreys approximation
						all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, alpha=1, method="jeffreysApprox")
					}
					
					some.bf10 <- all.bfs$bf10
					some.bfPlus0 <- all.bfs$bfPlus0
					some.bfMin0 <- all.bfs$bfMin0
					# Note: Data: OK, 
					
					rs[i] <- some.r
					ns[i] <- some.n
				}
				
				# Note: Assign bfs to be reported
				if (options$hypothesis == "correlated") {
				
					some.bf <- some.bf10
					BF10post[i] <- some.bf
					
					if (options$bayesFactorType == "BF01") {
					
						some.bf <- 1/some.bf
						BF10post[i] <- some.bf
						
					} else if (options$bayesFactorType == "LogBF10") {
					
						some.bf <- log(some.bf)
					}
					
										
				} else if (options$hypothesis == "correlatedPositively") {
				
					# TODO: Still need to implement this for general rho0, rather than rho0=0
					some.bf <- some.bfPlus0
					BF10post[i] <- some.bf
					
					if (options$bayesFactorType == "BF01") {
					
						some.bf <- 1/some.bf
						BF10post[i] <- some.bf
					
					} else if (options$bayesFactorType == "LogBF10") {
					
						some.bf <- log(some.bf)
					}
					
				} else if (options$hypothesis == "correlatedNegatively") {
				
					some.bf <- some.bfMin0
					BF10post[i] <- some.bf
					
					if (options$bayesFactorType == "BF01") {
					
						some.bf <- 1/some.bf
						BF10post[i] <- some.bf
						
					} else if (options$bayesFactorType == "LogBF10") {
					
						some.bf <- log(some.bf)
					}
				}
				
				if (is.infinite(BF10post[i])) {
				
					unplotable <- TRUE
					unplotableMessage <- "Bayes factor is infinity"
				}
				
												
				if (!is.null(index)) {
				
					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(some.r), BF=.clean(some.bf), .footnotes=list(BF=list(index)))
				
				} else {
				
					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(some.r), BF=.clean(some.bf))
				}
				
				pair.statuses[[i]] <- list(ready=TRUE, error=FALSE, unplotable=unplotable, unplotableMessage=unplotableMessage)
			}
		}
		
		correlation.rows[[length(correlation.rows)+1]] <- result
		
	}
	
	if (length(correlation.rows) == 0)
		correlation.rows <- list(list(.variable1="...", .separator="-", .variable2="...", r= "", BF=""))
	
	correlation[["data"]] <- correlation.rows
	correlation[["footnotes"]] <- as.list(footnotes)
	
	results[["correlation"]] <- correlation
	
	
	# PLOTS
	
	if (length(options$pairs) > 0 && (options$plotPriorAndPosterior)) {
	
		if (callback(results) != 0)
			return()
			
		j <- 1
		
		for (i in .indices(options$pairs)) {
			
			pair <- options$pairs[[i]]
			
			status <- pair.statuses[[i]]
			
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
	
			if (perform == "run" && status$unplotable == FALSE) {
				
				subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				subDataSet <- na.omit(subDataSet)
				
				v1 <- subDataSet[[ .v(pair[[1]]) ]]
				v2 <- subDataSet[[ .v(pair[[2]]) ]]
			
			} else {
			
				v1 <- NULL
				v2 <- NULL
			}
			
			if (options$plotPriorAndPosterior) {
			
				plot <- plots.correlation[[j]]
	
				if (status$unplotable == FALSE) {
				
					image <- .beginSaveImage(530, 400)
					
					.plotPosterior.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, addInformation=options$plotPriorAndPosteriorAdditionalInfo)
					
					plot[["data"]] <- .endSaveImage(image)
				
				} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
				
					message <- paste("Plotting is not possible:", status$unplotableMessage)
					plot[["error"]] <- list(error="badData", errorMessage=message)
				}
				
				plot[["status"]] <- "complete"
				
				plots.correlation[[j]] <- plot
				
				j <- j + 1
				
				results[["plots"]] <- plots.correlation
				
				if (callback(results) != 0)
						return()
			}
		}
	}				
				
	results
}

