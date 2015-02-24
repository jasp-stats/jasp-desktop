.plotPosterior.ttest <- function(x= NULL, y= NULL, paired= FALSE, oneSided= FALSE, iterations= 10000, rscale= "medium", lwd= 2, cexPoints= 1.5,
 cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addInformation= TRUE, dontPlotData=FALSE) {
	
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
		mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
	
		return()
	}
	
	if (rscale == "medium") {
		r <- sqrt(2) / 2
	}
	if (rscale == "wide") {
		r <- 1
	}
	if (rscale == "ultrawide") {
		r <- sqrt(2)
	}
	if (mode(rscale) == "numeric") {
		r <- rscale
	}
	
	if (oneSided == FALSE) {
		nullInterval <- NULL
	}
	if (oneSided == "right") {
		nullInterval <- c(0, Inf)
	}
	if (oneSided == "left") {
		nullInterval <- c(-Inf, 0)
	}
	
	# sample from delta posterior
	samples <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval= nullInterval, posterior = TRUE, iterations = iterations, rscale= r)
	
	delta <- samples[,"delta"]
	
	# pdf cauchy prior
	dprior <- function(x,r, oneSided= oneSided){
		
		if (oneSided == "right") {
			
			y <- ifelse(x < 0, 0, 2/(pi*r*(1+(x/r)^2)))
			return(y)
		}
		
		if (oneSided == "left") {
			
			y <- ifelse(x > 0, 0, 2/(pi*r*(1+(x/r)^2)))
			return(y)
		}	else {
			
			return(1/(pi*r*(1+(x/r)^2)))
		}
	}
	
	BF <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval= nullInterval, posterior = FALSE, rscale= r)
	BF10 <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
	BF01 <- 1 / BF10
	
	# fit denisty estimator
	fit.posterior <-  logspline::logspline(delta)
	
	# density function posterior
	dposterior <- function(x, oneSided= oneSided, delta= delta){
		
		if (oneSided == FALSE) {
			
			k <- 1
			return(k*logspline::dlogspline(x, fit.posterior))
		}
		
		if (oneSided == "right") {
			
			k <- 1 / (length(delta[delta >= 0]) / length(delta))
			return(ifelse(x < 0, 0, k*logspline::dlogspline(x, fit.posterior)))
		}
		
		if (oneSided == "left") {
			
			k <- 1 / (length(delta[delta <= 0]) / length(delta))
			return(ifelse(x > 0, 0, k*logspline::dlogspline(x, fit.posterior)))
		}	
	}	
	
	
	# set limits plot
	xlim <- vector("numeric", 2)
	
	if (oneSided == FALSE) {
		
		xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
		
		if (length(x) < 10) {
			
			if (addInformation) {
			
				stretch <- 1.52
			} else {
			
				stretch <- 1.4
			}
			
		} else {
		
			stretch <- 1.2
		}
		
	}
	
	
	if (oneSided == "right") {
		
		if (length(delta[delta >= 0]) < 10)
			return("Plotting is not possible: To few posterior samples in tested interval")
		
		xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])
		stretch <- 1.32
	}
	
	if (oneSided == "left") {
		
		if (length(delta[delta <= 0]) < 10)
			return("Plotting is not possible: To few posterior samples in tested interval")
		
		xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])
		stretch <- 1.32
	}
	
	ylim <- vector("numeric", 2)
	
	ylim[1] <- 0
	ylim[2] <- max(stretch * dprior(0,r, oneSided= oneSided), stretch * max(dposterior(x= delta, oneSided= oneSided, delta=delta)))
	
	
	# calculate position of "nice" tick marks and create labels
	xticks <- pretty(xlim)
	yticks <- pretty(ylim)
	xlabels <- formatC(xticks, 1, format= "f")
	ylabels <- formatC(yticks, 1, format= "f")
	
	# compute 95% credible interval & median:
	if (oneSided == FALSE) {
		
		CIlow <- quantile(delta, probs = 0.025)[[1]]
		CIhigh <- quantile(delta, probs = 0.975)[[1]]
		medianPosterior <- median(delta)
	}
	
	if (oneSided == "right") {
		
		CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
		medianPosterior <- median(delta[delta >= 0])
	}
	
	if (oneSided == "left") {
		
		CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
		medianPosterior <- median(delta[delta <= 0])
	}	
	
	
	posteriorLine <- dposterior(x= seq(min(xticks), max(xticks),length.out = 1000), oneSided = oneSided, delta=delta)
	
	xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
	
	plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(seq(min(xticks), max(xticks),length.out = 1000),posteriorLine, lwd= lwd)
	lines(seq(min(xticks), max(xticks),length.out = 1000), dprior(seq(min(xticks), max(xticks),length.out = 1000), r=r, oneSided= oneSided), lwd= lwd, lty=3)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
	
	points(0, dprior(0,r, oneSided= oneSided), col="black", pch=21, bg = "grey", cex= cexPoints)
	
	evalPosterior <- posteriorLine[posteriorLine > 0]
	
	if (oneSided == "right") {
		
		heightPosteriorAtZero <- evalPosterior[1]
		points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
	} else if (oneSided == "left") {
		
		heightPosteriorAtZero <- evalPosterior[length(evalPosterior)]
		points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
	} else {
		
		points(0, dposterior(0, delta=delta, oneSided=oneSided), col="black", pch=21, bg = "grey", cex= cexPoints)
	}
	
	# 95% credible interval
	dmax <- optimize(function(x)dposterior(x,oneSided= oneSided, delta=delta), interval= range(xticks), maximum = TRUE)$objective # get maximum density
	
	# enable plotting in margin
	par(xpd=TRUE)
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
	
	medianText <- formatC(medianPosterior, digits= 3, format="f")
	
	
	if (addInformation) {
		
		# display BF10 value
		offsetTopPart <- 0.06	
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000) {
			BF10t <- formatC(BF10,3, format = "e")
			BF01t <- formatC(BF01,3, format = "e")
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
		
		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		medianLegendText <- paste("median =", medianText)
		
		text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
		text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		
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
		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		
		medianLegendText <- paste("median =", medianText)
	}
	
	mostPosterior <- mean(delta > mean(range(xticks)))
	
	if (mostPosterior >= .5) {
		
		legendPosition <- min(xticks)
		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1, x.intersp= .6, seg.len= 1.2)
	} else {
		
		legendPosition <- max(xticks)
		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1,  x.intersp= .6, seg.len= 1.2)
	}
}

.plotSequentialBF.ttest <- function(x= NULL, y= NULL, paired= FALSE, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.6,
 cexTextBF= 1.4, cexText=1.2, cexLegend= 1.2, cexEvidence= 1.6,	lwdAxis= 1.2, plotDifferentPriors= FALSE, BFH1H0= TRUE, dontPlotData= FALSE) {
	
	#### settings ####
	
	if (!plotDifferentPriors) {
		
		evidenceText <-  TRUE
	} else {
		
		evidenceText <-  FALSE
	}	
	
	
	if (rscale == "medium") {
		
		r <- sqrt(2) / 2
	}
	
	if (rscale == "wide") {
		
		r <- 1
	}
	
	if (rscale == "ultrawide") {
		
		r <- sqrt(2)
	}
	
	if (mode(rscale) == "numeric") {
		
		r <- rscale
	}
	
	
	if (oneSided == FALSE) {
		
		nullInterval <- NULL
	}
	
	if (oneSided == "right") {
		
		nullInterval <- c(0, Inf)
	}
	
	if (oneSided == "left") {
		
		nullInterval <- c(-Inf, 0)
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
	
	
	BF10 <- vector("numeric", max(length(x), length(y)))
	BF10m <- vector("numeric", max(length(x), length(y)))
	BF10u <- vector("numeric", max(length(x), length(y)))
	
	idData <- 1
	
	if (is.null(y)) {
		
		ind <- which(x == x[1])
		idData <- sum((ind+1)-(1:(length(ind))) == 1)
		
	} else {
		
		idData <- 1
		
		
		for (i in 2:(min(c(length(x), length(y))))) {
			
			previous  <- c(x[i-1], y[i-1])
			
			if (all(c(x[i], y[i]) == previous)) {
				
				idData <- idData + 1
				
			} else if (x[i] == y[i]) {
				
				idData <- idData + 1
				
			} else {
				
				break
			}		
		}
	}
	
	
	BF10[1:idData] <- 1
	BF10m[1:idData] <- 1
	BF10u[1:idData] <- 1
	
	
	if (idData < length(x)) {
		
		i <- idData + 1
		
	} else {
		
		i <- idData
		
	}
	
	if (idData < length(y)) {
		
		j <- idData + 1
		
	} else {
		
		j <- idData
		
	}
	
	k <- idData + 1
	
	
	while ((i <= length(x) | j <= length(y)) & k <= length(BF10)) {
		
		BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j],paired = FALSE, rscale= r, nullInterval = nullInterval)
		BF10[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
		
		k <- k+1	
		
		if (i < length(x)) {
			
			i <- i+1
		}
		if (j < length(y)) {
			
			j <- j+1
		}
	}
	
	BF10 <- BF10[is.finite(BF10)]
	
	
	if (plotDifferentPriors) {
		
		if (idData < length(x)) {
			
			i <- idData + 1
			
		} else {
			
			i <- idData
			
		}
		
		if (idData < length(y)) {
			
			j <- idData + 1
			
		} else {
			
			j <- idData
			
		}
		
		k <- idData + 1
		
		
		while ((i <= length(x) | j <= length(y)) & k <= length(BF10u)) {
			
			BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j],paired = FALSE, rscale= "ultrawide", nullInterval = nullInterval)
			BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
			
			k <- k+1	
			
			if (i < length(x)) {
				
				i <- i+1
			}
			if (j < length(y)) {
				
				j <- j+1
			}
		}
		
		
		BF10u <- BF10u[is.finite(BF10u)]
		
		
		if (idData < length(x)) {
			
			i <- idData + 1
			
		} else {
			
			i <- idData
			
		}
		
		if (idData < length(y)) {
			
			j <- idData + 1
			
		} else {
			
			j <- idData
			
		}
		
		k <- idData + 1
		
		
		while ((i <= length(x) | j <= length(y)) & k <= length(BF10m)) {
			
			BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j],paired = FALSE, rscale= "medium", nullInterval = nullInterval)
			BF10m[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
			
			k <- k+1	
			
			if (i < length(x)) {
				
				i <- i+1
			}
			if (j < length(y)) {
				
				j <- j+1
			}
		}
		
		BF10m <- BF10m[is.finite(BF10m)]
		
	}
	
	
	
	####################### scale y axis ###########################
	
	if (plotDifferentPriors) {
		
		BF <- c(BF10, BF10u, BF10m)
		
	} else {
		
		BF <- BF10
		
	}
	
	
	if (!BFH1H0) {
		
		BF <- 1 / BF
		BF10 <- 1 / BF10
		
		if (plotDifferentPriors) {
			
			BF10u  <- 1 / BF10u
			BF10m <- 1 / BF10m
		}
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
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
																																																										 split = "+", fixed=TRUE)[[1]][2])+1, sep="")
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
	
	plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	
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
	
	
	# display BF10 value
	if (idData < length(BF10)) {
		
		BFe <- BayesFactor::ttestBF(x=x, y=y, paired= paired, nullInterval= nullInterval, rscale= r)
		BF10e <- BayesFactor::extractBF(BFe, logbf = FALSE, onlybf = F)[1, "bf"]
		
	} else {
		
		BF10e <- 1
	}	
	
	BF01e <- 1 / BF10e
	
	
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
	
	if (length(x) <= 60) {
		
		points(log(BF10), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
	} else {
		
		lines(log(BF10), col="black", lwd = 2.7) # user prior
	}
	
	if (plotDifferentPriors) {
		
		if (length(x) <= 60) {
			
			points(log(BF10u), pch=21, bg= "white", cex= 0.7, lwd= 1.3) # "ultrawide" prior
			points(log(BF10m), pch=21, bg= "black", cex= 0.7, lwd= 1.3) # "medium" prior
		} else {
			
			greycol <- rgb(0,0,0, alpha=0.95)
			greycol2 <- rgb(0,0,0, alpha=0.5)
			lines(log(BF10u), col= greycol2, cex= 0.7, lwd= 1.3, lty= 1) # "ultrawide" prior
			lines(log(BF10m), col= greycol, cex= 0.7, lwd= 1.3, lty=3) # "medium" prior
		}
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
		
	} else {
		
		# add legend
		xx <- grconvertX(0.56, "ndc", "user")
		yy <- grconvertY(0.872 + offsetTopPart, "ndc", "user")
		
		BFind <- sort(c(BF10[length(x)], BF10u[length(x)], BF10m[length(x)]), decreasing = TRUE, index.return=TRUE)$ix
		legend <- c("user prior", "ultrawide prior", "medium prior")
		
		if (length(x) <= 60) {
			
			pt.bg <-  c("grey", "white", "black")
			pt.cex <-  c(cexPoints, 0.7, 0.7)
			legend(xx, yy, legend = legend[BFind], pch=rep(21,3), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=rep(1.3,3), pt.cex= pt.cex[BFind])
		} else {
			
			xx <- grconvertX(0.55, "ndc", "user")
			lty <- c(1, 1, 3)
			lwd <- c(2.7, 1.3, 1.3)
			col <- c("black", greycol2, greycol)
			legend(xx, yy, legend = legend[BFind], lty= lty[BFind], bty= "n", cex= cexLegend, lwd= lwd[BFind], col= col[BFind], seg.len= .7)
		}		
	}	
}

		
.plotBF.robustnessCheck.ttest <- function(x= NULL, y= NULL, paired= FALSE, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2,
 cexYXlab= 1.5,  cexText=1.2, cexLegend= 1.4, lwdAxis= 1.2, cexEvidence= 1.6, BFH1H0 = TRUE, dontPlotData= FALSE) { 
	
	#### settings ####
	if (rscale == "medium") {
		r <- sqrt(2) / 2
	}
	if (rscale == "wide") {
		r <- 1
	}
	if (rscale == "ultrawide") {
		r <- sqrt(2)
	}
	if (mode(rscale) == "numeric") {
		r <- rscale
	}
	
	if (oneSided == FALSE) {
		nullInterval <- NULL
	}
	if (oneSided == "right") {
		nullInterval <- c(0, Inf)
	}
	if (oneSided == "left") {
		nullInterval <- c(-Inf, 0)
	}
	
	
	par(mar= c(5, 6, 6, 7) + 0.1, las=1)
	
	if (dontPlotData) {
	
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}			
		}
	
		mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)
	
		return()
	}
	
	#### get BFs ###
	rValues <- seq(0.0001, 1.5, length.out = 400)
	
	# BF10
	BF10 <- vector("numeric", length(rValues))
	
	for (i in seq_along(rValues)) {
	
		BF <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= rValues[i])
		BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
	}
	
	# BF10 "medium" prior
	BF10m <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "medium")
	BF10m <- BayesFactor::extractBF(BF10m, logbf = FALSE, onlybf = F)[1, "bf"]
	BF10mText <- BF10m	
	
	# BF10 "wide" prior
	BF10w <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "wide")
	BF10w <- BayesFactor::extractBF(BF10w, logbf = FALSE, onlybf = F)[1, "bf"]
	BF10wText <- BF10w
	
	# BF10 "ultrawide" prior
	BF10ultra <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "ultrawide")
	BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, onlybf = F)[1, "bf"]
	BF10ultraText <- BF10ultra
	
	# BF10 user prior
	BF10user <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= r)
	BF10user <- BayesFactor::extractBF(BF10user, logbf = FALSE, onlybf = F)[1, "bf"]
	BF10userText <- BF10user
	
	####################### scale y axis ###########################
	
	BF <- c(BF10, BF10m, BF10w, BF10ultra, BF10user)
	
	if (!BFH1H0) {
		
		BF <- 1 / BF
		BF10 <- 1 / BF10
		BF10m  <- 1 / BF10m
		BF10w <- 1 / BF10w
		BF10ultra <- 1 / BF10ultra
		BF10user <- 1 / BF10user
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
				
				if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
					split = "+", fixed=TRUE)[[1]][2])+1, sep="")
				} else {
					
					newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
				}
				
				if (eval(parse(text=newy)) >= 10^6) {
					
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			}
		}
		
		if (max(BF) > eval(parse(text= yLab1s[length(yLab1s)-1]))) {
			
			if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
					
				newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
				split = "+", fixed=TRUE)[[1]][2])+1, sep="")
			} else {
					
				newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
			}
				
			if (eval(parse(text=newy)) >= 10^6) {
					
				newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			}
				
			yLab1s <- c(yLab1s, newy)
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
		
		if (min(BF) < eval(parse(text= yLab1s[2]))) {
			
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
			
			
			yLab1s <- c(newy, yLab1s)
		}
		
		yLab <- yLab1s
	}
	
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
	
	
	while (eval(parse(text=yLab[2])) > min(BF)) {
		
		interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
			
		newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
		yLab <- c(newy, yLab)			
	}		
	
	while (eval(parse(text=yLab[length(yLab)-1])) < max(BF)) {
		
		interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed= TRUE)[[1]][2])
		pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
		newy <- paste(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
		yLab <- c( yLab, newy)		
	}		
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab)) {
		
		yAt[i] <- log(eval(parse(text= yLab[i])))
	}	
	
	####################### plot ###########################
	
	xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	ylim <- c(ylow, yhigh)
	
	plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	
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
		
		axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
	}
	
	if (omit3s) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			}			
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			}			
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 4.3)
			}			
		}
	}
	
	if (omit3s == FALSE) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}			
		}
	}
	
	mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)
	
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
	
	
	# display BF10
	lines(rValues,log(BF10), col="black", lwd = 2.7)
	
	# display "medium", user, and "ultrawide" prior BFs
	points(r, log(BF10user), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
	points(sqrt(2) / 2, log(BF10m), pch=21, bg= "black", cex= 1.1, lwd= 1.3) # "medium" prior
	points(sqrt(2), log(BF10ultra), pch=21, bg= "white", cex= 1.1, lwd= 1.3) # "ultrawide" prior
	
	#### add legend
	# BF values
	
	# BFuser
	BF01userText <- 1 / BF10userText
	
	if (BF10userText >= 1000000 | BF01userText >= 1000000) {
	
		BF10usert <- format(BF10userText, digits= 4, scientific = TRUE)
		BF01usert <- format(BF01userText, digits= 4, scientific = TRUE)
	}
	if (BF10userText < 1000000 & BF01userText < 1000000) {
	
		BF10usert <- formatC(BF10userText, 3, format = "f")
		BF01usert <- formatC(BF01userText, 3, format = "f")
	}
	
	if (oneSided == FALSE) {
	
		if( BF10userText >= BF01userText) {
			userBF <- bquote(BF[10]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0][1]==.(BF01usert))
		}		
	}
	if (oneSided == "right") {
	
		if (BF10userText >= BF01userText) {
			userBF <- bquote(BF["+"][0]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0]["+"]==.(BF01usert))
		}	
	}
	if (oneSided == "left") {
	
		if (BF10userText >= BF01userText) {
			userBF <- bquote(BF["-"][0]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0]["-"]==.(BF01usert))
		}	
	}
	
	# BFmedium
	BF01mText <- 1 / BF10mText
	
	if (BF10mText >= 1000000 | BF01mText >= 1000000) {
		BF10mt <- format(BF10mText, digits= 4, scientific = TRUE)
		BF01mt <- format(BF01mText, digits= 4, scientific = TRUE)
	}
	if (BF10mText < 1000000 & BF01mText < 1000000) {
		BF10mt <- formatC(BF10mText, 3, format = "f")
		BF01mt <- formatC(BF01mText, 3, format = "f")
	}
	
	if (oneSided == FALSE) {
	
		if (BF10mText >= BF01mText) {
			mBF <- bquote(BF[10]==.(BF10mt))
		} else {
			mBF <- bquote(BF[0][1]==.(BF01mt))
		}		
	}
	if (oneSided == "right") {
	
		if (BF10mText >= BF01mText) {
			mBF <- bquote(BF["+"][0]==.(BF10mt))
		} else {
			mBF <- bquote(BF[0]["+"]==.(BF01mt))
		}	
	}
	if (oneSided == "left") {
	
		if (BF10mText >= BF01mText) {
			mBF <- bquote(BF["-"][0]==.(BF10mt))
		} else {
			mBF <- bquote(BF[0]["-"]==.(BF01mt))
		}	
	}
	
	# BFultrawide
	BF01ultraText <- 1 / BF10ultraText
	
	if (BF10ultraText >= 1000000 | BF01ultraText >= 1000000) {
		BF10ultrat <- format(BF10ultraText, digits= 4, scientific = TRUE)
		BF01ultrat <- format(BF01ultraText, digits= 4, scientific = TRUE)
	}
	if (BF10ultraText < 1000000 & BF01ultraText < 1000000) {
		BF10ultrat <- formatC(BF10ultraText, 3, format = "f")
		BF01ultrat <- formatC(BF01ultraText, 3, format = "f")
	}
	
	if (oneSided == FALSE) {
	
		if (BF10ultraText >= BF01ultraText) {
			ultraBF <- bquote(BF[10]==.(BF10ultrat))
		} else {
			ultraBF <- bquote(BF[0][1]==.(BF01ultrat))
		}		
	}
	
	if (oneSided == "right") {
	
		if (BF10ultraText >= BF01ultraText) {
			ultraBF <- bquote(BF["+"][0]==.(BF10ultrat))
		} else{
			ultraBF <- bquote(BF[0]["+"]==.(BF01ultrat))
		}	
	}
	
	if (oneSided == "left") {
	
		if (BF10ultraText >= BF01ultraText) {
			ultraBF <- bquote(BF["-"][0]==.(BF10ultrat))
		} else {
			ultraBF <- bquote(BF[0]["-"]==.(BF01ultrat))
		}	
	}	
	
	xx <- grconvertX(0.2, "ndc", "user")
	yy <- grconvertY(0.965, "ndc", "user")
	
	BFind <- sort(c(BF10userText, BF10ultraText, BF10mText), decreasing = TRUE, index.return=TRUE)$ix
	BFsort <- sort(c(BF10userText, BF10ultraText, BF10mText), decreasing = TRUE, index.return=TRUE)$x
	
	legend <- c("user prior:", "ultrawide prior:", "medium prior:")
	pt.bg <-  c("grey", "white", "black")
	pt.cex <-  c(cexPoints, 1.1, 1.1)
	
	legend(xx, yy, legend = legend[BFind], pch=rep(21,3), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=rep(1.3,3), pt.cex= pt.cex[BFind])
	
	xx <- grconvertX(0.5, "ndc", "user")
	y1 <- grconvertY(0.902, "ndc", "user")
	y2 <- grconvertY(0.852, "ndc", "user")
	y3 <- grconvertY(0.802, "ndc", "user")
	yy <- c(y1, y2, y3)
	
	text(xx, yy[BFsort== BF10userText], userBF, cex= 1.3,pos = 4)
	text(xx, yy[BFsort== BF10ultraText], ultraBF, cex= 1.3, pos= 4)
	text(xx, yy[BFsort== BF10mText], mBF, cex= 1.3, pos= 4)
}

TTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
	if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
	if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
	if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
	if(is.null(options()$BFprogress)) options(BFprogress = interactive())
	if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)
	
	
	all.variables <- unlist(options$variables)
	
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
	}
	
	results <- list()
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="plots", type="images")
	
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"
	
	ttest <- list()
	
	ttest[["title"]] <- "Bayesian One Sample T-Test"
	
	ttest[["citation"]] <- list(
		"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16(2), 225–237.")
	
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
	
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue") {
			bf.title <- "BF\u2081\u2080"
		}
		if (options$hypothesis == "greaterThanTestValue") {
			bf.title <- "BF\u208A\u2080"
		}
		if (options$hypothesis == "lessThanTestValue") {
			bf.title <- "BF\u208B\u2080"
		}
		
	} else {
	
		BFH1H0 <- FALSE
		
		if (options$hypothesis == "notEqualToTestValue") {
			bf.title <- "BF\u2080\u2081"
		}
		if (options$hypothesis == "greaterThanTestValue") {
			bf.title <- "BF\u2080\u208A"
		}
		if (options$hypothesis == "lessThanTestValue") {
			bf.title <- "BF\u2080\u208B"
		}
	}
	
	fields <- list(
		list(name="Variable", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))
	
	ttest[["schema"]] <- list(fields=fields)
	
	results[["ttest"]] <- ttest
	
	footnotes <- .newFootnotes()
	
	#if (options$hypothesis == "greaterThanTestValue") {
	#
	#	message <- paste("All tests, hypothesis is sample mean is greater than ", 0, sep="")
	#	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	#
	#} else if (options$hypothesis == "lessThanTestValue") {
	#
	#	message <- paste("All tests, hypothesis is sample mean is less than ", 0, sep="")
	#	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	#	
	#} else {
	#
	#}
	
	if (options$hypothesis == "notEqualToTestValue") {
		nullInterval <- NULL
		oneSided <- FALSE
	}
	if (options$hypothesis == "greaterThanTestValue") {
		nullInterval <- c(0, Inf)
		oneSided <- "right"
	}
	if (options$hypothesis == "lessThanTestValue") {
		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
	}
	

	ttest.rows <- list()
	plots.ttest <- list()

	
	for (variable in options[["variables"]])
	{
		ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", error=".")
		
		
		if (options$plotPriorAndPosterior){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "running"
			
			image <- .beginSaveImage(530, 400)
			.plotPosterior.ttest(x=NULL, y=NULL, paired=FALSE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo, dontPlotData=TRUE)
			plot[["data"]] <- .endSaveImage(image)
						
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
		
		if (options$plotBayesFactorRobustness){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "running"
			
			image <- .beginSaveImage(530, 400)
			.plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
			plot[["data"]] <- .endSaveImage(image)
			
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
		
		if (options$plotSequentialAnalysis || options$plotSequentialAnalysisRobustness){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "running"
			
			image <- .beginSaveImage(530, 400)
			.plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
			plot[["data"]] <- .endSaveImage(image)
			
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
	}
	
	ttest[["data"]] <- ttest.rows
	results[["ttest"]] <- ttest
	results[["plots"]] <- plots.ttest
	
	
	if (perform == "run") {
		
		i <- 1
		
		status <- rep("ok", length(options$variables))
	
		plottingError <- rep("error", length(options$variables))
		
		for (variable in options[["variables"]])
		{
			
			result <- try (silent = TRUE, expr = {
				
				variableData <- dataset[[ .v(variable) ]]
				variableData <- variableData[ ! is.na(variableData) ]
				
				r <- BayesFactor::ttestBF(variableData, r=options$priorWidth, nullInterval = nullInterval)
				
				bf.raw <- exp(as.numeric(r@bayesFactor$bf))[1]
								
				if (bf.type == "BF01")
					bf.raw <- 1 / bf.raw
				
				BF <- .clean(bf.raw)
				
				error <- .clean(as.numeric(r@bayesFactor$error)[1])
				
				list(Variable=variable, BF=BF, error=error)
			})
			
						
			if (class(result) == "try-error") {
				
				errorMessage <- .extractErrorMessage(result)
				
				if (errorMessage == "x or y must not contain missing or infinite values.") {
					
					errorMessage <- paste("Bayes factor is undefined - the sample contains infinity")
					
					status[i] <- "error"
					plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample contains infinity"
					
					#} else if (errorMessage == "data are essentially constant") {
					#				
					#	errorMessage <- paste("Bayes factor is undefined - the sample contains all the same value (zero variance)")
					#
				} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
					errorMessage <- "Bayes factor is undefined - too few observations"	
					
					status[i] <- "error"
					plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample has too few observations"
				}
				
				index <- .addFootnote(footnotes, errorMessage)
				
				result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
				ttest.rows[[i]] <- result
				
			} else {			
						
				if (is.na(bf.raw)) {
				
					status[i] <- "error"
					plottingError[i] <- "Plotting is not possible: Bayes factor is NaN"
				} else if(bf.raw == Inf & (options$plotPriorAndPosterior | options$plotBayesFactorRobustness | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
				
					status[i] <- "error"
					plottingError[i] <- "Plotting is not possible: Bayes factor is infinite"
				} else if (is.infinite(1 / bf.raw)) {
				
					status[i] <- "error"
					plottingError[i] <- "Plotting is not possible: The Bayes factor is too small"
				}
				
				ind <- which(variableData == variableData[1])
				idData <- sum((ind+1)-(1:(length(ind))) == 1)
				
				if(idData > 1 & (options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
					
					#seqFootnote <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")
					#plotSequentialStatus <- "error"	
					# status[i] <- "sequentialNotPossible"
					# plottingError[i] <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")
				}
				
				
				ttest.rows[[i]] <- result				
			}
			
			i <- i + 1
		}
		
		ttest[["data"]] <- ttest.rows
		ttest[["footnotes"]] <- as.list(footnotes)
		ttest[["status"]] <- "complete"		
		results[["ttest"]] <- ttest
		
		if(callback() != 0)
			return()
				
		if (callback(results) != 0)
					return()
		
		i <- 1
		
		for (variable in options[["variables"]])
		{		
			
			variableData <- dataset[[ .v(variable) ]]
			variableData <- variableData[ ! is.na(variableData) ]
				
			numberPlotsPerVariable <- sum(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, any(options$plotSequentialAnalysis, options$plotSequentialAnalysisRobustness))
			z <- numberPlotsPerVariable * (which(options$variables == variable) -1) + 1
			
			
			if (options$plotPriorAndPosterior) {
			
				plot <- plots.ttest[[z]]
				
				if (status[i] != "error") {
					
					p <- try(silent= FALSE, expr= {
					
							image <- .beginSaveImage(530, 400)
					
							.plotPosterior.ttest(x= variableData, oneSided= oneSided, rscale = options$priorWidth, addInformation= options$plotPriorAndPosteriorAdditionalInfo)
					
							plot[["data"]] <- .endSaveImage(image)
						})
						
					if (class(p) == "try-error") {
					
						errorMessage <- .extractErrorMessage(p)
						
						if (errorMessage == "not enough data") {
						
								errorMessage <- "Plotting is not possible: The Bayes factor is too small"
						} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
						
							errorMessage <- "Plotting is not possible: The Bayes factor is too small"
						}
						
						plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
					}					
				} else {
				
						plot[["error"]] <- list(error="badData", errorMessage= plottingError[i])
				}					
				
				plot[["status"]] <- "complete"
				
				plots.ttest[[z]] <- plot
				
				results[["plots"]] <- plots.ttest
				
				if (callback(results) != 0)
					return()

				z <- z + 1
			}
			
			if (options$plotBayesFactorRobustness) {
			
				plot <- plots.ttest[[z]]
				
				if (status[i] != "error") {
				
					image <- .beginSaveImage(530, 400)
					.plotBF.robustnessCheck.ttest (x= variableData, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0)					
					content <- .endSaveImage(image)
					plot[["data"]]  <- content
					
				} else {
				
					plot[["error"]] <- list(error="badData", errorMessage= plottingError[i])
					
				}
				
				plot[["status"]] <- "complete"
				
				plots.ttest[[z]] <- plot
				
				results[["plots"]] <- plots.ttest
			
				
				if (callback(results) != 0)
					return()
				
				z <- z + 1
			}
			
			if (options$plotSequentialAnalysis || options$plotSequentialAnalysisRobustness) {
			
				plot <- plots.ttest[[z]]
				
				if (status[i] != "error" && status[i] != "sequentialNotPossible") {	
				
					image <- .beginSaveImage(530, 400)
					.plotSequentialBF.ttest (x= variableData, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0, , plotDifferentPriors= options$plotSequentialAnalysisRobustness)					
					content <- .endSaveImage(image)
					plot[["data"]]  <- content
					
				} else {
					
					plot[["error"]] <- list(error="badData", errorMessage=plottingError[i])
				}
				
				plot[["status"]] <- "complete"
				
				plots.ttest[[z]] <- plot
				
				results[["plots"]] <- plots.ttest
				
				if (callback(results) != 0)
					return()
				
				z <- z + 1
			}

			i <- i + 1
		}
	}
	
	results
}

