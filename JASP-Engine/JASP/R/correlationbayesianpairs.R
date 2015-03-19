.plotPosterior.correlation <- function(r, n, alpha=1, oneSided= FALSE, BF, BFH1H0, addInformation= TRUE, dontPlotData=FALSE, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2) {	
	
	
	if (addInformation) {
	
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
		drawCI <- TRUE
		
	} else {
	
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
		drawCI <- FALSE
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
	
	# 
	# 
	# if (oneSided == FALSE) {
	# 	
	# 	dmax <- optimize(f= function(x).posteriorRho(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	# 	
	# } else if (oneSided == "right") {
	# 	
	# 	dmax <- optimize(f= function(x).posteriorRhoPlus(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	# 	
	# } else if (oneSided == "left") {
	# 	
	# 	dmax <- optimize(f= function(x).posteriorRhoMin(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	# }
	
	
	# calculate position of "nice" tick marks and create labels
	xticks <- seq(-1.0, 1.0, 0.25)
	xlabels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
	
	
	# compute 95% credible interval & median:
	if (oneSided != FALSE)
		drawCI <- FALSE
		
	if (drawCI) {
		
		rhoQuantiles <- .rhoQuantile(n = n, r = r)
		CIlow <- rhoQuantiles[1]
		CIhigh <- rhoQuantiles[3]
		medianPosterior <- rhoQuantiles[2]
		
		if (any(is.na(rhoQuantiles)))
				drawCI <- FALSE
	
	}
	
	rho <- seq(min(xticks), max(xticks),length.out = 1000)	
	
	if (oneSided == FALSE) {
		
		priorLine <- .priorRho(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRho(rho= rho, n= n, r= r, alpha= alpha)
		
		posteriorLine <- .posteriorRho(rho=rho, n=n, r=r, alpha=alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
		
			aParameter <- .posteriorAParameter(n=n, r=r)
			bParameter <- .posteriorBParameter(n=n, r=r)
			
			if (any(is.na(c(aParameter, bParameter))))
				stop("Posterior is too peaked")
			
			posteriorLine <- .myScaledBeta(alpha=aParameter, beta=bParameter, rho=rho)
			
			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
				stop("Posterior is too peaked")
		}
		
	} else if (oneSided == "right") {
		
		priorLine <- .priorRhoPlus(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRhoPlus(rho= rho, n= n, r= r, alpha= alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
			stop("Posterior is too peaked")
		
	} else if (oneSided == "left") {
		
		priorLine <- .priorRhoMin(rho=rho, alpha=alpha)
		posteriorLine <- .posteriorRhoMin(rho= rho, n= n, r= r, alpha= alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
			stop("Posterior is too peaked")
	}	
	
	dmax <- max(posteriorLine)
		
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format= "f")
	
	
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

.plotSequentialBF.correlation <- function(x= NULL, y= NULL, BF10post, callback=function(...) 0, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.6,
 cexTextBF= 1.4, cexText=1.2, cexLegend= 1.2, cexEvidence= 1.6,	lwdAxis= 1.2, plotDifferentPriors= FALSE, BFH1H0= TRUE, dontPlotData= FALSE) {
	
	#### settings ####
	
	if (!plotDifferentPriors) {
		
		evidenceText <-  TRUE
	} else {
		
		evidenceText <-  FALSE
	}
	
	
	par(mar= c(5.6, 6, 7, 7) + 0.1, las=1)
	
	if (dontPlotData) {
		
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext("n", side = 1, cex = cexXlab, line= 2.5)
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}			
		}
		
		return()
	}
	
	BF10 <- vector("numeric", length(x))
	
	for (i in seq_along(x)) {
		
		if (i == 1) {
			
			BF10[i] <- 1
			
		} else if (sd(x[1:i]) == 0 || sd(y[1:i]) == 0) {
		
			BF10[i] <- 1
		
		} else {
		
			some.r <- cor(x[1:i], y[1:i])
			some.n <- i
	
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
			
			
			if (oneSided == FALSE) {
				
				BF10[i] <- all.bfs$bf10
				
			} else if (oneSided == "right") {
			
				BF10[i] <- all.bfs$bfPlus0
				
			} else if (oneSided == "left") {
			
				BF10[i] <- all.bfs$bfMin0
			}
			
			if (is.na(BF10[i]))
				stop("One or more Bayes factors cannot be computed")
		}
	}
	
	
	if (BFH1H0) {
	
		BF <- BF10
		
	} else {
	
		BF <- 1 / BF10
	}
	
	
	# y-axis labels larger than 1
	
	y1h <- "1"
	
	i <- 1
	
	while (eval(parse(text= y1h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y1h[i])) {
			
			newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y1h[i], "0", sep= "")
		}
		
		if (eval(parse(text=newy)) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	
	y3h <- "3"
	
	i <- 1
	
	while (eval(parse(text= y3h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y3h[i])) {
			
			newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y3h[i], "0", sep= "")
		}
		
		if (as.numeric(newy) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y3h <- c(y3h, newy)
		
		i <- i + 1
	}
	
	if(callback() != 0)
				return()
	
	yhigh <- vector("numeric", length(y1h) + length(y3h))
	
	o <- 1
	e <- 1
	
	for (i in seq_along(yhigh)) {
		
		if (i %% 2 == 1) {
			
			yhigh[i] <- y1h[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			yhigh[i] <- y3h[e]
			e <- e + 1
		}
	} 
	
	yhighLab <- as.character(yhigh)
	
	
	# y-axis labels smaller than 1
	
	y1l <- "1/1"
	
	i <- 1
	
	while (eval(parse(text= y1l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y1l[i])) {
			
			newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y1l[i], "0", sep= "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6)) {
			
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y1l <- c(y1l, newy)
		i <- i + 1
	}
	
	
	y3l <- "1/3"
	
	i <- 1
	
	while (eval(parse(text= y3l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y3l[i])) {
			
			newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y3l[i], "0", sep= "")
		}
		
		if (newy == "1/3e+9") {
			
			newy <- "1/3e+09"
		}	
		
		if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {
			
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
			newy <- sub(".33", "", newy)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y3l <- c(y3l, newy)
		i <- i + 1
	}
	
	if(callback() != 0)
				return()
	
	ylow <- vector("numeric", length(y1l) + length(y3l))
	o <- 1
	e <- 1
	
	for (i in seq_along(ylow)) {
		
		if (i %% 2 == 1) {
			
			ylow[i] <- y1l[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			ylow[i] <- y3l[e]
			e <- e + 1
		}
	}
	
	yLab <- c(rev(ylow[-1]), yhighLab)
	
	
	# remove 3's if yLab vector is too long
	omit3s <- FALSE
	
	if (length(yLab) > 9) {
		
		omit3s <- TRUE
		
		ind <- which(yLab == "3")
		
		yLabsHigh <- yLab[ind:length(yLab)]
		
		if (length(yLabsHigh) > 1) {
			
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
			
		} else {
			
			yLabsHigh <- character(0)
		}		
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		
		if (max(BF) > eval(parse(text= yLab1s[length(yLab1s)]))) {
			
			for (i in 1:2) {
				
				if(grepl(pattern = "e",yLab1s[length(yLab1s)])){
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][2])+1, sep="")
					
				} else {
					
					newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
				}
				
				if (eval(parse(text=newy)) >= 10^6) {
					
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			}
		}
		
		
		if (yLab1s[1] == "1") {
			
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		
		if (yLab1s[length(yLab1s)] == "1") {
			
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF) < eval(parse(text= yLab1s[1]))) {
			
			for (i in 1:2) {
				
				if (grepl(pattern = "e",yLab1s[1])) {
					
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
					
				} else {
					
					newy <- paste(yLab1s[1], "0", sep= "")
				}
				
				if (eval(parse(text= newy)) <= 10^(-6)) {
					
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy)-4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			}
			
			yLab1s <- c(newy, yLab1s)
		}
		
		yLab <- yLab1s
	}
	
	if(callback() != 0)
		return()
	
	while (length(yLab) > 9) {
		
		ind <- which(yLab == "1")
		
		if (ind == 1) {
			
			yLabLow <- character(0)
			
		} else {
			
			yLabLow <- yLab[1:(ind-1)]
		}
		
		if (ind == length(yLab)) {
			
			yLabHigh <- character(0)
			
		} else {
			
			yLabHigh <- yLab[(ind+1):length(yLab)]
		}		
		
		if (length(yLabLow) > 1) {
			
			yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
			
		} else {
			
			yLabLow <- yLabLow
		}
		
		
		if (length(yLabHigh) > 1) {
			
			yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
			
		} else {
			
			yLabHigh <- yLabHigh
		}
		
		if (length(yLabLow) == 1) {
			
			yLabLow <- paste("1/", yLabHigh[1], sep="")
		}
		
		if (length(yLabHigh) == 1) {
			
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
		}
		
		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	
	while (eval(parse(text=yLab[1])) > min(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
			
			newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
			yLab <- c(newy, yLab)
		}		
	}		
	
	while (eval(parse(text=yLab[length(yLab)])) < max(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed= TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
			newy <- paste(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
			yLab <- c( yLab, newy)
		}
	}		
	
	if(callback() != 0)
		return()
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab)) {
		
		yAt[i] <- log(eval(parse(text= yLab[i])))
	}
	
	####################### plot ###########################
	
	xLab <- pretty(c(0, length(BF10)+2))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	
	if (is.infinite(yhigh)) {
		
		yhigh <- 1e+308
	}
	

	ylim <- c(ylow, yhigh)
	
	plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	
	for (i in seq_along(yAt)) {
		
		lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
	}
	
	lines(xlim, rep(0, 2), lwd= lwd)
	
	axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)
	
	# enable plotting in margin
	par(xpd= TRUE)
	xx <- grconvertX(0.79, "ndc", "user")
	
	yAthigh <- yAt[yAt >= 0]
	
	if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {
		
		for (i in 1:(length(yAthigh)-1)) {
			
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}		
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1)) {
			
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}		
		}
		
		if(callback() != 0)
				return()
		
		axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		
		text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
	}
	
	if (omit3s) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}			
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}			
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}			
		}
	}
	
	if (omit3s == FALSE) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}			
		}
	}
	
	mtext("n", side = 1, cex = cexXlab, line= 2.5)
	
	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	xxt <- grconvertX(0.28, "npc", "user")
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}		
	}
	
	if (oneSided == "right") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}		
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}		
	}
	
	
	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
		}		
	}
	
	if (oneSided == "right"){
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
		}		
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
		}		
	}
	
		
	if (BFH1H0) {
	
		BF10e <- BF10post
		BF01e <- 1 / BF10e
		
	} else {
	
		BF01e <- BF10post
		BF10e <- 1 / BF01e
	}
	
	# display BF10 value
	
	offsetTopPart <- 0.06	
	
	xx <- min(xLab)
	yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
	
	if (BF10e >= 1000000 | BF01e >= 1000000) {
		
		BF10t <- formatC(BF10e,3, format = "e")
		BF01t <- formatC(BF01e,3, format = "e")
	}
	
	if (BF10e < 1000000 & BF01e < 1000000) {
		
		BF10t <- formatC(BF10e, 3, format = "f")
		BF01t <- formatC(BF01e, 3, format = "f")
	}
	
	if (oneSided == FALSE) {
		
		text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
	}
	
	if (oneSided == "right") {
		
		text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
	}
	
	if (oneSided == "left") {
		
		text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
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
		xx <- grconvertX(0.445 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	}
	
	yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
	
	
	# make sure that colored area is centered
	
	radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", "user")
	A <- radius^2*pi
	alpha <- 2 / (BF01e + 1) * A / radius^2
	startpos <- pi/2 - alpha/2
	
	if(callback() != 0)
				return()
	
	# draw probability wheel
	
	plotrix::floating.pie(xx, yy,c(BF10e, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
	
	yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
	
	if (oneSided == FALSE) {
		
		text(xx, yy, "data|H1", cex= 1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (oneSided == "right") {
		
		text(xx, yy, "data|H+", cex=  1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (oneSided == "left") {
		
		text(xx, yy, "data|H-", cex=  1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (length(BF10) <= 60) {
		
		points(log(BF), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
		
	} else {
		
		lines(log(BF), col="black", lwd = 2.7) # user prior
	}
	
	BFevidence <- BF10e
	
	if (evidenceText) {
		
		if (BF10e < 1) {
			BFevidence <- 1 / BF10e
		}		
		if (BFevidence >= 1 & BFevidence <= 3) {
			lab <- "Anecdotal"
		}
		if (BFevidence > 3 & BFevidence <= 10) {
			lab <- "Moderate"
		}
		if (BFevidence > 10 & BFevidence <= 30) {
			lab <- "Strong"
		}
		if (BFevidence > 30 & BFevidence <= 100) {
			lab <- "Very strong"
		}
		if (BFevidence > 100) {
			lab <- "Extreme"
		}
		
		xxT <- max(xLab)
		yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
		
		if (BF10e >= 1) {
			
			if (oneSided == FALSE) {
				text(xxT, yyT, paste("Evidence for H1:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
			if (oneSided == "right") {
				text(xxT, yyT, paste("Evidence for H+:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
			if (oneSided == "left") {
				text(xxT, yyT, paste("Evidence for H-:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
		}
		
		if (BF10e < 1) {
			text(xxT, yyT, paste("Evidence for H0:\n", lab), cex= 1.4, pos= 2, offset= -.2)
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
		
		if (options$plotSequentialAnalysis){

			plot <- list()
			
			plot[["title"]] <- paste(pair, collapse=" - ")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"
			
			image <- .beginSaveImage(530, 400)
			.plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
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
				
				if (identical(all.equal(some.r, 1), TRUE) || identical(all.equal(some.r, -1), TRUE)) {
				
					unplotable <- TRUE
					unplotableMessage <- "Sample correlation co-efficient r is 1 or -1"
				}
				
								
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
				
				} else if (BF10post[i] == 1 / Inf) {
				
					unplotable <- TRUE
					unplotableMessage <- "The Bayes factor is too small"
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
	
	if (length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$plotSequentialAnalysis)) {
	
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
			
				plots.correlation[[j]]$status <- "running"
				
				results[["plots"]] <- plots.correlation
				
				if (callback(results) != 0)
						return()
			
				plot <- plots.correlation[[j]]
	
				if (status$unplotable == FALSE) {
				
					p <- try(silent=FALSE, expr= {
				
						image <- .beginSaveImage(530, 400)
						
						.plotPosterior.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, addInformation=options$plotPriorAndPosteriorAdditionalInfo)
						
						plot[["data"]] <- .endSaveImage(image)
					})
					
					if (class(p) == "try-error") {
					
						errorMessage <- .extractErrorMessage(p)
						plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
					}
					
				
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
			
			if (options$plotSequentialAnalysis) {
			
				plots.correlation[[j]]$status <- "running"
				
				results[["plots"]] <- plots.correlation
				
				if (callback(results) != 0)
						return()
			
				plot <- plots.correlation[[j]]
	
				if (status$unplotable == FALSE) {
				
					p <- try(silent=FALSE, expr= {
				
						image <- .beginSaveImage(530, 400)
						
						.plotSequentialBF.correlation(x=v1, y=v2, oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0)
						
						plot[["data"]] <- .endSaveImage(image)
					})
					
					if (class(p) == "try-error") {
					
						errorMessage <- .extractErrorMessage(p)
						plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
					}
					
				
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

