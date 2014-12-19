
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
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	
	
	
	ttest <- list()

	ttest[["title"]] <- "Bayesian One Sample T-Test"
	
	ttest[["citation"]] <- list(
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 752-760")

	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
		bf.title <- "BF\u2081\u2080"
	} else {
		bf.title <- "BF\u2080\u2081"
	}

	fields <- list(
		list(name="Variable", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))

	ttest[["schema"]] <- list(fields=fields)

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

	if (length(options[["variables"]]) > 0)
	{
		ttest.rows <- list()

		for (variable in options[["variables"]])
		{
			ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", error=".")	
		}
		
		if (perform == "run") {

			i <- 1

			for (variable in options[["variables"]])
			{
				result <- try (silent = TRUE, expr = {
				
					variableData <- dataset[[ .v(variable) ]]
					variableData <- variableData[ ! is.na(variableData) ]

					r <- BayesFactor::ttestBF(variableData, r=options$priorWidth)
		
					bf.raw <- exp(as.numeric(r@bayesFactor$bf))
					if (bf.type == "BF01")
						bf.raw <- 1 / bf.raw
						
					BF <- .clean(bf.raw)
					error <- .clean(as.numeric(r@bayesFactor$error))

					list(Variable=variable, BF=BF, error=error)
				})

				if (class(result) == "try-error") {
				
					errorMessage <- .extractErrorMessage(result)
						
					if (errorMessage == "x or y must not contain missing or infinite values.") {
				
						errorMessage <- paste("BayesFactor is undefined - the sample contains infinity")
					
					#} else if (errorMessage == "data are essentially constant") {
					#				
					#	errorMessage <- paste("BayesFactor is undefined - the sample contains all the same value (the variance is zero)")
					#
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - the sample has too few observations"	
					}
				
					index <- .addFootnote(footnotes, errorMessage)
				
					result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
				}
		
				ttest.rows[[i]] <- result
				
				i <- i + 1
		
				ttest[["data"]] <- ttest.rows
				ttest[["footnotes"]] <- as.list(footnotes)
	
				results[["ttest"]] <- ttest
				
				if (callback(results) != 0)
					return(NULL)
			}
		}
		
		### plot posterior of effect size ####
		plotPosterior.ttest <- function(x= NULL, oneSided= FALSE, iterations= 10000, rscale, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.4, lwdAxis= 1.2){
		
		if(rscale == "medium"){
			r <- sqrt(2) / 2
		}
		if(rscale == "wide"){
			r <- 1
		}
		if(rscale == "ultrawide"){
			r <- sqrt(2)
		}
		if(mode(rscale) == "numeric"){
			r <- rscale
		}
		
		if(oneSided == FALSE){
			nullInterval <- NULL
		}
		if(oneSided == "right"){
			nullInterval <- c(0, Inf)
		}
		if(oneSided == "left"){
			nullInterval <- c(-Inf, 0)
		}
		
		
		# sample from delta posterior
		samples <- BayesFactor::ttestBF(x=x, nullInterval= nullInterval, posterior = TRUE, iterations = iterations, rscale= r)
		delta <- samples[,"delta"]
		
		# fit denisty estimator
		
		fit.posterior <-  logspline::logspline(delta)
		
		# density function posterior
		dposterior <- function(x, oneSided= oneSided, delta= delta){
			if(oneSided == FALSE){
				k <- 1
				return(k*logspline::dlogspline(x, fit.posterior))
			}
			if(oneSided == "right"){
				k <- 1 / (length(delta[delta >= 0]) / length(delta))
				return(ifelse(x < 0, 0, k*logspline::dlogspline(x, fit.posterior)))
			}
			if(oneSided == "left"){
				k <- 1 / (length(delta[delta <= 0]) / length(delta))
				return(ifelse(x > 0, 0, k*logspline::dlogspline(x, fit.posterior)))
			}	
		}
		
		
		
		# pdf cauchy prior
		dprior <- function(delta,r, oneSided= oneSided){
			if(oneSided == "right"){
				y <- ifelse(delta < 0, 0, 2/(pi*r*(1+(delta/r)^2)))
				return(y)
			}
			if(oneSided == "left"){
				y <- ifelse(delta > 0, 0, 2/(pi*r*(1+(delta/r)^2)))
				return(y)
			}	else{
			return(1/(pi*r*(1+(delta/r)^2)))
			}
		}
		
		# set limits plot
		xlim <- vector("numeric", 2)
		if(oneSided == FALSE){
		xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
		}
		if(oneSided == "right"){
		xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])
		}
		if(oneSided == "left"){
		xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])
		}
		
		ylim <- vector("numeric", 2)
		ylim[1] <- 0
		ylim[2] <- max(dprior(0,r, oneSided= oneSided), 1.28*max(dposterior(x= delta, oneSided= oneSided, delta=delta)))
		
		# calculate position of "nice" tick marks and create labels
		xticks <- pretty(xlim)
		yticks <- pretty(ylim)
		xlabels <- formatC(pretty(xlim), 1, format= "f")
		ylabels <- formatC(pretty(ylim), 1, format= "f")
		
		# 95% credible interval:
		if(oneSided == FALSE){
		CIlow <- quantile(delta, probs = 0.025)[[1]]
		CIhigh <- quantile(delta, probs = 0.975)[[1]]
		}
		if(oneSided == "right"){
		CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
		}
		if(oneSided == "left"){
		CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
		}
		
		
		par(mar= c(5, 5, 7, 4) + 0.1, las=1)
		xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
		plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
		lines(seq(min(xticks), max(xticks),length.out = 1000),dposterior(x=seq(min(xticks), max(xticks),length.out = 1000), oneSided = oneSided, delta=delta), lwd= lwd, xlim= xlim, ylim= range(yticks), ylab= "", xlab= "")
		lines(seq(min(xticks), max(xticks),length.out = 1000), dprior(seq(min(xticks), max(xticks),length.out = 1000), r=r, oneSided= oneSided), lwd= lwd, lty=3)
		
		axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
		axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3)
		mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
		
		points(0, dprior(0,r, oneSided= oneSided), col="black", pch=21, bg = "grey", cex= cexPoints)
		points(0, dposterior(0, oneSided = oneSided, delta=delta), col="black", pch=21, bg = "grey", cex= cexPoints)
		
		# 95% credible interval
		dmax <- optimize(function(x)dposterior(x,oneSided= oneSided, delta=delta), interval= range(xticks), maximum = TRUE)$objective # get maximum density
		yCI <- grconvertY(dmax, "user", "ndc") + 0.08
		yCIt <- grconvertY(dmax, "user", "ndc") + 0.04
		y95 <- grconvertY(dmax, "user", "ndc") + 0.1
		yCI <- grconvertY(yCI, "ndc", "user")
		yCIt <- grconvertY(yCIt, "ndc", "user")
		y95 <- grconvertY(y95, "ndc", "user")
		arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
		text(mean(c(CIlow, CIhigh)), y95,"95%", cex= cexCI)
		
		text(CIlow, yCIt, bquote(.(formatC(CIlow,2, format="f"))), cex= cexCI)
		text(CIhigh, yCIt, bquote(.(formatC(CIhigh,2, format= "f"))), cex= cexCI)
		
		# enable plotting in margin
		par(xpd=TRUE)
		
		# display BF10 value
		BF <- BayesFactor::ttestBF(x=x, nullInterval= nullInterval, posterior = FALSE, iterations = iterations, rscale= r)
		BF10 <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
		BF01 <- 1 / BF10
		xx <- grconvertX(0.3, "ndc", "user")
		yy <- grconvertY(0.802, "ndc", "user")
		yy2 <- grconvertY(0.858, "ndc", "user")
		if(BF10 >= 1000000 | BF01 >= 1000000){
			BF10t <- format(BF10, digits= 3, scientific = TRUE)
			BF01t <- format(BF01, digits= 3, scientific = TRUE)
		}
		if(BF10 < 1000000 & BF01 < 1000000){
		BF10t <- formatC(BF10,2, format = "f")
		BF01t <- formatC(BF01,2, format = "f")
		}
		
		if(oneSided == FALSE){
		text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF)
		text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF)
		}
		if(oneSided == "right"){
		text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF)
		text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF)
		}
		if(oneSided == "left"){
		text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF)
		text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF)
		}
		
		
		if(max(nchar(BF10t), nchar(BF01t)) <= 4){
		xx <- grconvertX(0.44, "ndc", "user")
		}
		if(max(nchar(BF10t), nchar(BF01t)) == 5){
		xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		if(max(nchar(BF10t), nchar(BF01t)) == 6){
		xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		if(max(nchar(BF10t), nchar(BF01t)) == 7){
		xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		if(max(nchar(BF10t), nchar(BF01t)) == 8){
		xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		if(max(nchar(BF10t), nchar(BF01t)) > 8){
		xx <- grconvertX(0.44 + 0.004* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		yy <- grconvertY(0.83, "ndc", "user")
		
		# make sure that colored area is centered
		radius <- 0.06*diff(range(xticks))
		A <- radius^2*pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
		
		yy <- grconvertY(0.907, "ndc", "user")
		yy2 <- grconvertY(0.75, "ndc", "user")
		
		if(oneSided == FALSE){
		text(xx, yy, "data|H1", cex= cexCI)
		text(xx, yy2, "data|H0", cex= cexCI)
		}
		if(oneSided == "right"){
		text(xx, yy, "data|H+", cex= cexCI)
		text(xx, yy2, "data|H0", cex= cexCI)
		}
		if(oneSided == "left"){
		text(xx, yy, "data|H-", cex= cexCI)
		text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		# add legend
		xx <- grconvertX(0.57, "ndc", "user")
		yy <- grconvertY(0.90, "ndc", "user")
		legend(xx, yy, legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend)
		
		}
 		
 		
 		effectSize.plot <- list()
 
 		i <- 1
 		
 		for (variable in options[["variables"]])
 		{
 			plot <- list()
 		
 			plot[["title"]] <- variable
 			plot[["width"]]  <- 530
 			plot[["height"]] <- 400
 			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
 		
 			effectSize.plot[[i]] <- plot
 			i <- i + 1
 		}
 		
 		results[["plots"]] <- effectSize.plot
 		
 		if (perform=="run") {
 		
			i <- 1
			
			for (variable in options[["variables"]])
			{
			variableData <- dataset[[ .v(variable) ]]
			variableData <- variableData[ ! is.na(variableData) ]
					
			image <- .beginSaveImage(530, 400)
			
			plotPosterior.ttest(x= variableData, oneSided= FALSE, rscale = options$priorWidth)
			
			
			content <- .endSaveImage(image)
				
					plot <- effectSize.plot[[i]]
				
					plot[["data"]]  <- content
				
					effectSize.plot[[i]] <- plot
					i <- i + 1
					results[["plots"]] <- effectSize.plot
 			}
 		}
		
		ttest[["data"]] <- ttest.rows
		ttest[["footnotes"]] <- as.list(footnotes)
	}

	results[["ttest"]] <- ttest

	results
}

