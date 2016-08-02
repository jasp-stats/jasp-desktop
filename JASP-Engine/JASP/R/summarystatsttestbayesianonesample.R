#
# Copyright (C) 2016 University of Amsterdam
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

SummaryStatsTTestBayesianOneSample <- function(dataset=NULL, options, perform = 'run', callback)
{
	run <- (perform == "run")

	results <- list()

	meta <- list()

	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="PriorPosteriorPlot", type="image"),
																		list(name="BFrobustnessPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"

	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	fields=list()
	fields[[length(fields)+1]] <- list(name="tStatistic", type="number", format="sf:4;dp:3", title="t")
	fields[[length(fields)+1]] <- list(name="n1Size", type="number", title="n")

	#Bayes factor type (BF10, BF01, log(BF10))
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "BF\u2081\u2080"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <- "BF\u208A\u2080"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <- "BF\u208B\u2080"
		}
	}
	else if (bf.type == "LogBF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
		
	}
	else if (bf.type == "BF01")
	{
		BFH1H0 <- FALSE
		
		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "BF\u2080\u2081"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <- "BF\u2080\u208A"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <- "BF\u2080\u208B"
		}
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)


	table <- list()
	table[["title"]] <- "Bayesian One Sample T-Test"
	
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	#add footnotes to the analysis result
	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "greaterThanTestValue")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that the mean is greater than 0.", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	else if(options$hypothesis == "lessThanTestValue")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that the mean is lesser than 0.", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}

	table[["footnotes"]] <- as.list(footnotes)

	if(options$hypothesis == "notEqualToTestValue")
	{
		oneSidedHypothesis <- FALSE
	}
	else if (options$hypothesis == "greaterThanTestValue")
	{
		oneSidedHypothesis <- "right"
	}
	else
	{
		oneSidedHypothesis <- "left"
	}

	rowsTTestBayesianOneSample <- list()
	bayesFactorObject <- NULL
	priorAndPosteriorPlot <- NULL
	priorAndPosteriorPlotAddInfo <- NULL
	bayesFactorRobustnessPlot <- NULL
	plots.sumstats.ttest <- list()
	plotTypes <- list()


	if(perform=="run")
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsTTestBayesianOneSample <- state$rowsTTestBayesianOneSample
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			status <- .isInputValidOneSample(options) #check validity of data
			row <- status$row

			if(status$ready)                           #check if data has been entered
			{
				bayesFactor10 <- .calcluateBFOneSample(options, state, diff) #calculate Bayes factor from t value

				if(options$bayesFactorType == "BF10")
				{
					BF <- .clean(exp(bayesFactor10$bf))
				}
				else if(options$bayesFactorType == "BF01")
				{
					BF <- .clean(1/exp(bayesFactor10$bf))
				}
				else
				{
					BF <- .clean(bayesFactor10$bf)
				}

				bayesFactorObject <- bayesFactor10
				row <- list(BF=BF, tStatistic=options$tStatistic, n1Size=options$n1Size, errorEstimate=.clean(bayesFactor10$properror))
			}

			rowsTTestBayesianOneSample <- row
		}


		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which(state$plotTypes == "posteriorPlot")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				results[["inferentialPlots"]][["priorAndPosteriorPlot"]][["status"]] <- "running"

				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Prior and Posterior"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotPosterior.ttest.summaryStats (t=options$tStatistic, n1=options$n1Size, n2=NULL, paired=FALSE, BFH1H0=(options$bayesFactorType == "BF10"), 
												   dontPlotData= FALSE, rscale=options$priorWidth, addInformation = options$plotPriorAndPosteriorAdditionalInfo,
												   BF = exp(bayesFactorObject$bf), oneSided = oneSidedHypothesis)
				plot[["data"]]   <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]

			if(options$plotPriorAndPosteriorAdditionalInfo)
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			}
			else
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
		}

		if(options$plotBayesFactorRobustness)
		{
			BFtypeRequiresNewPlot <- TRUE
			
			if (!(is.null(state)))
			{
				BFtypeRequiresNewPlot <- FALSE
				BFtype <- options$bayesFactorType
				BFtypeState <- state$options$bayesFactorType
				
				if ((BFtypeState == "LogBF10" || BFtypeState == "BF10") && BFtype == "BF01")
				{
					BFtypeRequiresNewPlot <- TRUE
				}
				else if(BFtypeState == "BF01" && (BFtype == "LogBF10" || BFtype == "BF10"))
				{
					BFtypeRequiresNewPlot <- TRUE
				}
			}

			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				"robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotBF.robustnessCheck.bffromt (t=options$tStatistic, n1=options$n1Size, n2=0, BFH1H0=(options$bayesFactorType == "BF10"), 
												 dontPlotData= FALSE, rscale=options$priorWidth, 
												 BF10post = ifelse((options$bayesFactorType == "BF10"),.clean(exp(bayesFactorObject$bf)), .clean(1/exp(bayesFactorObject$bf))), oneSided = oneSidedHypothesis)
				plot[["data"]]   <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}
	else #init phase
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsTTestBayesianOneSample <- state$rowsTTestBayesianOneSample
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			row <- .isInputValidOneSample(options)$row
			rowsTTestBayesianOneSample <- row
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which( state$plotTypes == "posteriorPlot")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				plot <- list()
				
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				
				image <- .beginSaveImage(530, 400)
				.plotPosterior.ttest.summaryStats(BF = 1, dontPlotData = TRUE, addInformation = options$plotPriorAndPosteriorAdditionalInfo)
				plot[["data"]] <- .endSaveImage(image)

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]

			if(options$plotPriorAndPosteriorAdditionalInfo)
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			}
			else
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
		}

		if(options$plotBayesFactorRobustness)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && 
				diff$hypothesis==FALSE))) && "robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotBF.robustnessCheck.ttest (oneSided= oneSidedHypothesis, BFH1H0= (options$bayesFactorType == "BF10"), dontPlotData= TRUE)
				plot[["data"]] <- .endSaveImage(image)

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}


	if(rowsTTestBayesianOneSample$errorEstimate!="NaN")
	{
		fields[[length(fields)+1]] <- list(name="errorEstimate", type="number", format="sf:4;dp:3", title="error %")
	}

	table[["schema"]] <- list(fields=fields)
	table[["data"]] <- list(rowsTTestBayesianOneSample)
	results[["table"]] <- table

	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness)
	{
		results[["inferentialPlots"]] <- list(title=ifelse(sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness)) > 1,"Inferential Plots", "Inferential Plot"), PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)
	}


	keep <- NULL
	
	for (plot in plots.sumstats.ttest)
		keep <- c(keep, plot$data)


	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state, keep=keep))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactorObject=bayesFactorObject, bayesFactorRobustnessPlot=bayesFactorRobustnessPlot, priorAndPosteriorPlot=priorAndPosteriorPlot, 
					rowsTTestBayesianOneSample=rowsTTestBayesianOneSample, plotsTtest=plots.sumstats.ttest, priorAndPosteriorPlotAddInfo=priorAndPosteriorPlotAddInfo, plotTypes=plotTypes), keep=keep))
	}
}


.calcluateBFOneSample <- function(options, state, diff)
{
	if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && 
		(diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$tStatistic == FALSE && diff$n1Size==FALSE))))
	{
		bf10 <- state$bayesFactorObject
	}
	else
	{
		if (options$hypothesis == "notEqualToTestValue")
		{
			nullInterval <- NULL
			oneSided <- FALSE
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			nullInterval <- c(0, Inf)
			oneSided <- "right"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			nullInterval <- c(-Inf, 0)
			oneSided <- "left"
		}

		bf10 <- BayesFactor::ttest.tstat(t = options$tStatistic, n1 = options$n1Size, n2 = 0, rscale = options$priorWidth, nullInterval = nullInterval)
	}

	list(bf=bf10$bf, properror=bf10$properror)
}


# checks if the input given is valid
.isInputValidOneSample <- function(options)
{
	ready <- TRUE 

	n1Value <- options$n1Size
	tStatValue <- options$tStatistic

	if(options$n1Size==0 || is.null(options$n1Size))
	{
		ready <- FALSE
		n1Value <- "."
	}

	if(is.null(options$tStatistic))
	{
		ready <- FALSE
		tStatValue <- "."
	}

	row <- list(BF = ".", tStatistic = tStatValue, n1Size = n1Value, errorEstimate = ".")

	list(ready=ready, row=row)
}


#function to plot the Bayes factor Robustness Check plots
.plotBF.robustnessCheck.bffromt <- function(t= NULL, n1= NULL, n2=NULL, BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2,
 cexYXlab= 1.5,  cexText=1.2, cexLegend= 1.4, lwdAxis= 1.2, cexEvidence= 1.6, BFH1H0 = TRUE, dontPlotData= FALSE)
{
	#### settings ####
	if (rscale == "medium")
	{
		r <- sqrt(2) / 2
	}
	else if (rscale == "wide")
	{
		r <- 1
	}
	else if (rscale == "ultrawide")
	{
		r <- sqrt(2)
	}
	else if (mode(rscale) == "numeric")
	{
		r <- rscale
	}
	
	if (oneSided == FALSE || t == 0)
	{
		nullInterval <- NULL
	}
	else if (oneSided == "right")
	{
		nullInterval <- c(0, Inf)
	}
	else if (oneSided == "left")
	{
		nullInterval <- c(-Inf, 0)
	}

	par(mar= c(5, 6, 6, 7) + 0.1, las=1)

	if (dontPlotData)
	{
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		if (oneSided == FALSE)
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		else if (oneSided == "right")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		else if (oneSided == "left")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}

		mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)
		return()
	}

	#### get BFs ###
	if(r > 1.5)
	{
		rValues <- seq(0.0005, 2, length.out = 535)	
	}
	else
	{
		rValues <- seq(0.0005, 1.5, length.out = 400)
	}

	# BF10
	BF10 <- vector("numeric", length(rValues))

	for (i in seq_along(rValues)) {

		BF <- BayesFactor::ttest.tstat(t=t, n1=n1, n2=n2, nullInterval=nullInterval, rscale=rValues[i])
		BF10[i] <- .clean(exp(BF$bf))
		
		if ( ! .shouldContinue(callback()))
			return()
	}

	# BF10 "medium" prior
	BF10m <- BayesFactor::ttest.tstat(t=t, n1=n1, n2=n2, nullInterval=nullInterval, rscale= "medium")
	BF10m <- .clean(exp(BF10m$bf))
	BF10mText <- BF10m

	# BF10 "wide" prior
	BF10w <- BayesFactor::ttest.tstat(t=t, n1=n1, n2=n2, nullInterval= nullInterval, rscale= "wide")
	BF10w <- .clean(exp(BF10w$bf))
	BF10wText <- BF10w

	# BF10 "ultrawide" prior
	BF10ultra <- BayesFactor::ttest.tstat(t=t, n1=n1, n2=n2, nullInterval= nullInterval, rscale= "ultrawide")
	BF10ultra <- .clean(exp(BF10ultra$bf))
	BF10ultraText <- BF10ultra

	# BF10 user prior
	BF10user <- BF10post 
	BF10userText <- BF10user

	if (!(.shouldContinue(callback())))
	{
		return()
	}
	
	####################### scale y axis ###########################
	
	BF <- c(BF10, BF10m, BF10w, BF10ultra, BF10user)
	
	if (!BFH1H0) {
		
		BF <- 1 / BF
		BF10 <- 1 / BF10
		BF10m  <- 1 / BF10m
		BF10w <- 1 / BF10w
		BF10ultra <- 1 / BF10ultra
	}
	
	# y-axis labels larger than 1
	y1h <- "1"
	i <- 1
	
	while (eval(parse(text= y1h[i])) < max(BF10))
	{
		if (grepl(pattern = "e",y1h[i]))
		{
			newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		}
		else
		{
			newy <- paste(y1h[i], "0", sep= "")
		}
		
		if (eval(parse(text=newy)) >= 10^6)
		{
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	y3h <- "3"
	i <- 1
	
	while (eval(parse(text= y3h[i])) < max(BF10))
	{
		if (grepl(pattern = "e",y3h[i]))
		{
			newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		}
		else
		{
			newy <- paste(y3h[i], "0", sep= "")
		}
		
		if (as.numeric(newy) >= 10^6)
		{
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y3h <- c(y3h, newy)
		i <- i + 1
	}

	yhigh <- vector("numeric", length(y1h) + length(y3h))
	o <- 1
	e <- 1

	for (i in seq_along(yhigh))
	{
		if (i %% 2 == 1)
		{
			yhigh[i] <- y1h[o]
			o <- o + 1
		}

		if (i %% 2 == 0)
		{
			yhigh[i] <- y3h[e]
			e <- e + 1
		}
	}

	yhighLab <- as.character(yhigh)
	
	# y-axis labels smaller than 1
	y1l <- "1/1"
	i <- 1

	while (eval(parse(text= y1l[i])) > min(BF10))
	{
		if (grepl(pattern = "e",y1l[i]))
		{
			newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		}
		else
		{
			newy <- paste(y1l[i], "0", sep= "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6))
		{
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y1l <- c(y1l, newy)
		i <- i + 1
	}
	
	y3l <- "1/3"
	i <- 1
	
	while (eval(parse(text= y3l[i])) > min(BF10))
	{
		if (grepl(pattern = "e",y3l[i]))
		{
			newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		}
		else
		{
			newy <- paste(y3l[i], "0", sep= "")
		}
		
		if (newy == "1/3e+9")
		{
			newy <- "1/3e+09"
		}

		if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9))
		{
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

	if ( ! .shouldContinue(callback()))
	{
		return()
	}

	for (i in seq_along(ylow))
	{
		if (i %% 2 == 1)
		{
			ylow[i] <- y1l[o]
			o <- o + 1
		}
		if (i %% 2 == 0)
		{
			ylow[i] <- y3l[e]
			e <- e + 1
		}
	}
	
	yLab <- c(rev(ylow[-1]), yhighLab)
	
	# remove 3's if yLab vector is too long
	omit3s <- FALSE
	
	if (length(yLab) > 9)
	{
		omit3s <- TRUE
		
		ind <- which(yLab == "3")
		
		yLabsHigh <- yLab[ind:length(yLab)]
		
		if (length(yLabsHigh) > 1)
		{
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
		}
		else
		{
			yLabsHigh <- character(0)
		}
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)])))
		{
			for (i in 1:2)
			{
				if (grepl(pattern = "e",yLab1s[length(yLab1s)]))
				{
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
					split = "+", fixed=TRUE)[[1]][2])+1, sep="")
				}
				else
				{
					newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
				}
				
				if (eval(parse(text=newy)) >= 10^6)
				{
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				}

				yLab1s <- c(yLab1s, newy)
			}
		}
		
		if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)-1])))
		{
			if (grepl(pattern = "e",yLab1s[length(yLab1s)]))
			{
				newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
				split = "+", fixed=TRUE)[[1]][2])+1, sep="")
			}
			else
			{
				newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
			}
			
			if (eval(parse(text=newy)) >= 10^6) 
			{
				newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			}
			
			yLab1s <- c(yLab1s, newy)
		}
		
		if (yLab1s[1] == "1")
		{
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		if (yLab1s[length(yLab1s)] == "1")
		{
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF10) < eval(parse(text= yLab1s[1])))
		{
			for (i in 1:2)
			{
				if (grepl(pattern = "e",yLab1s[1]))
				{
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
				}
				else
				{
					newy <- paste(yLab1s[1], "0", sep= "")
				}

				if (eval(parse(text= newy)) <= 10^(-6))
				{
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy)-4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			}
			
			yLab1s <- c(newy, yLab1s)
		}
		
		if (min(BF10) < eval(parse(text= yLab1s[2])))
		{
			if (grepl(pattern = "e",yLab1s[1]))
			{
				newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
			}
			else
			{
				newy <- paste(yLab1s[1], "0", sep= "")
			}
			
			if (eval(parse(text= newy)) <= 10^(-6))
			{
				newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				newy <-  sub("-", "+", x = newy)
				newy <- substring(newy, nchar(newy)-4, nchar(newy))
				newy <- paste0("1/", newy)
			}
			
			
			yLab1s <- c(newy, yLab1s)
		}
		
		yLab <- yLab1s
	}
	
	if (!(.shouldContinue(callback())))
	{
		return()
	}

	while (length(yLab) > 9)
	{
		ind <- which(yLab == "1")
		
		if (ind == 1)
		{
			yLabLow <- character(0)
		}
		else
		{
			yLabLow <- yLab[1:(ind-1)]
		}
		
		if (ind == length(yLab))
		{
			yLabHigh <- character(0)
		}
		else
		{
			yLabHigh <- yLab[(ind+1):length(yLab)]
		}

		if (length(yLabLow) > 1)
		{
			yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
		}
		else
		{
			yLabLow <- yLabLow
		}
		
		if (length(yLabHigh) > 1)
		{
			yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
		}
		else
		{
			yLabHigh <- yLabHigh
		}

		if (length(yLabLow) == 1)
		{
			yLabLow <- paste("1/", yLabHigh[1], sep="")
		}

		if (length(yLabHigh) == 1)
		{
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
		}

		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	if (!(.shouldContinue(callback())))
	{
		return()
	}

	while (eval(parse(text=yLab[2])) > min(BF10))
	{
		interval <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[2])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) + interval
		
		if (nchar(pot) == 1)
		{
			pot <- paste("0", pot, sep="")
		}
		
		newy <- paste("1/1e", "+", pot, sep="")
		yLab <- c(newy, yLab)
	}

	while (eval(parse(text=yLab[length(yLab)-1])) < max(BF10))
	{
		interval <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)-1])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) + interval
		
		if (nchar(pot) == 1)
		{
			pot <- paste("0", pot, sep="")
		}

		newy <- paste(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
		yLab <- c( yLab, newy)
	}
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab))
	{
		yAt[i] <- log(eval(parse(text= yLab[i])))
	}
	
	####################### plot ###########################

	if(r > 1.5)
	{
		xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
	}
	else
	{
		xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
	}
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	ylim <- c(ylow, yhigh)

	plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)

	for (i in seq_along(yAt))
	{
		lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
	}

	lines(xlim, rep(0, 2), lwd= lwd)

	axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)

	# enable plotting in margin
	par(xpd= TRUE)
	xx <- grconvertX(0.79, "ndc", "user")

	yAthigh <- yAt[yAt >= 0]

	if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300)
	{
		for (i in 1:(length(yAthigh)-1))
		{
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1))
			{
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(3))
			{
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(10))
			{
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(30))
			{
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(100))
			{
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1))
		{
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1))
			{
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/3))
			{
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/10))
			{
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/30))
			{
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/100))
			{
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
		axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
	}
	
	if (omit3s)
	{
		if (eval(parse(text= yLab[1])) <= 1/10^6)
		{
			line <- 4.75
			
		}
		else
		{
			line <- 4.3
		}
		
		if (oneSided == FALSE)
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= line)
			}
			else
			{
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
		
		if (oneSided == "right")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= line)
			}
			else
			{
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
		
		if (oneSided == "left")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= line)
			}
			else
			{
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
	}
	
	if (omit3s == FALSE)
	{
		if (oneSided == FALSE)
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "right")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "left")
		{
			if (BFH1H0)
			{
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
			else
			{
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

	if (oneSided == FALSE)
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "right")
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "left")
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}

	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))

	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

	if (oneSided == FALSE)
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
		}
	}

	if (oneSided == "right")
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
		}
	}
	
	if (oneSided == "left")
	{
		if (BFH1H0)
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
		else
		{
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
		}
	}
	
	if ((! .shouldContinue(callback())))
	{
		return()
	}

	# display BF10
	lines(rValues,log(BF10), col="black", lwd = 2.7)

	# display "wide", user, and "ultrawide" prior BFs
	points(r, log(BF10user), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
	points(1, log(BF10w), pch=21, bg= "black", cex= 1.1, lwd= 1.3) # "wide" prior
	points(sqrt(2), log(BF10ultra), pch=21, bg= "white", cex= 1.1, lwd= 1.3) # "ultrawide" prior

	#### add legend
	# BF values
	# BFuser

	if (BFH1H0)
	{
		BF01userText <- 1 / BF10userText
	}
	else
	{
		BF10userText <- 1 / BF10userText
		BF01userText <- 1 / BF10userText
	}

	if (BF10userText >= 1000000 | BF01userText >= 1000000)
	{
		BF10usert <- format(BF10userText, digits= 4, scientific = TRUE)
		BF01usert <- format(BF01userText, digits= 4, scientific = TRUE)
	}

	if (BF10userText < 1000000 & BF01userText < 1000000)
	{
		BF10usert <- formatC(BF10userText, 3, format = "f")
		BF01usert <- formatC(BF01userText, 3, format = "f")
	}

	if (oneSided == FALSE)
	{
		if( BF10userText >= BF01userText)
		{
			userBF <- bquote(BF[10]==.(BF10usert))
		}
		else
		{
			userBF <- bquote(BF[0][1]==.(BF01usert))
		}
	}
	if (oneSided == "right")
	{
		if (BF10userText >= BF01userText)
		{
			userBF <- bquote(BF["+"][0]==.(BF10usert))
		}
		else
		{
			userBF <- bquote(BF[0]["+"]==.(BF01usert))
		}
	}
	if (oneSided == "left")
	{
		if (BF10userText >= BF01userText)
		{
			userBF <- bquote(BF["-"][0]==.(BF10usert))
		}
		else
		{
			userBF <- bquote(BF[0]["-"]==.(BF01usert))
		}
	}

	# BFwide
	BF01wText <- 1 / BF10wText

	if (BF10wText >= 1000000 | BF01wText >= 1000000)
	{
		BF10wt <- format(BF10wText, digits= 4, scientific = TRUE)
		BF01wt <- format(BF01wText, digits= 4, scientific = TRUE)
	}
	if (BF10wText < 1000000 & BF01wText < 1000000)
	{
		BF10wt <- formatC(BF10wText, 3, format = "f")
		BF01wt <- formatC(BF01wText, 3, format = "f")
	}
	
	if (oneSided == FALSE)
	{
		if (BF10wText >= BF01wText)
		{
			wBF <- bquote(BF[10]==.(BF10wt))
		}
		else
		{
			wBF <- bquote(BF[0][1]==.(BF01wt))
		}
	}
	if (oneSided == "right")
	{
		if (BF10wText >= BF01wText)
		{
			wBF <- bquote(BF["+"][0]==.(BF10wt))
		}
		else
		{
			wBF <- bquote(BF[0]["+"]==.(BF01wt))
		}
	}
	if (oneSided == "left")
	{
		if (BF10wText >= BF01wText)
		{
			wBF <- bquote(BF["-"][0]==.(BF10wt))
		}
		else
		{
			wBF <- bquote(BF[0]["-"]==.(BF01wt))
		}
	}

	# BFultrawide
	BF01ultraText <- 1 / BF10ultraText

	if (BF10ultraText >= 1000000 | BF01ultraText >= 1000000)
	{
		BF10ultrat <- format(BF10ultraText, digits= 4, scientific = TRUE)
		BF01ultrat <- format(BF01ultraText, digits= 4, scientific = TRUE)
	}
	if (BF10ultraText < 1000000 & BF01ultraText < 1000000)
	{
		BF10ultrat <- formatC(BF10ultraText, 3, format = "f")
		BF01ultrat <- formatC(BF01ultraText, 3, format = "f")
	}
	
	if (oneSided == FALSE)
	{
		if (BF10ultraText >= BF01ultraText)
		{
			ultraBF <- bquote(BF[10]==.(BF10ultrat))
		}
		else
		{
			ultraBF <- bquote(BF[0][1]==.(BF01ultrat))
		}
	}

	if (oneSided == "right")
	{
		if (BF10ultraText >= BF01ultraText)
		{
			ultraBF <- bquote(BF["+"][0]==.(BF10ultrat))
		}
		else
		{
			ultraBF <- bquote(BF[0]["+"]==.(BF01ultrat))
		}
	}
	
	if (oneSided == "left")
	{
		if (BF10ultraText >= BF01ultraText)
		{
			ultraBF <- bquote(BF["-"][0]==.(BF10ultrat))
		}
		else
		{
			ultraBF <- bquote(BF[0]["-"]==.(BF01ultrat))
		}
	}

	xx <- grconvertX(0.2, "ndc", "user")
	yy <- grconvertY(0.965, "ndc", "user")

	BFind <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, index.return=TRUE)$ix
	BFsort <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, index.return=TRUE)$x

	legend <- c("user prior:", "ultrawide prior:", "wide prior:")
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
	text(xx, yy[BFsort== BF10wText], wBF, cex= 1.3, pos= 4)
}

.plotPosterior.ttest.summaryStats <- function(t=NULL, n1= NULL, n2= NULL, paired= FALSE, oneSided= FALSE, BF, BFH1H0, callback=function(...) 0, iterations= 10000, rscale= "medium", lwd= 2, cexPoints= 1.5,
 cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addInformation= TRUE, dontPlotData=FALSE) {
	
	if (addInformation)
	{
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
	}
	else
	{
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
	}
	
	if (dontPlotData)
	{
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
		
		return()
	}
	
	if (rscale == "medium")
	{
		r <- sqrt(2) / 2
	}
	if (rscale == "wide")
	{
		r <- 1
	}
	if (rscale == "ultrawide")
	{
		r <- sqrt(2)
	}
	if (mode(rscale) == "numeric")
	{
		r <- rscale
	}
	
	if (oneSided == FALSE)
	{
		nullInterval <- NULL
	}
	if (oneSided == "right")
	{
		nullInterval <- c(0, Inf)
	}
	if (oneSided == "left")
	{
		nullInterval <- c(-Inf, 0)
	}

	# sample from delta posterior
	bfObject <- BayesFactor::meta.ttestBF(t=t, n1=n1, n2=n2, rscale=r, nullInterval=nullInterval)
	library(BayesFactor)
	samples <- BayesFactor::posterior(model=bfObject, iterations=iterations, index=1)
	delta <- samples[,"delta"]

	if ( ! .shouldContinue(callback()))
	{
		return()
	}
	
	# fit shifted t distribution
	if (is.null(n2))
	{
		deltaHat <- t / sqrt(n1)
		N <- n1
		df <- N - 1
		sigmaStart <- 1 / N
	}
	else if (paired)
	{
		deltaHat <- t*sqrt(n1/2)
		N <- n1
		df <- N - 1
		sigmaStart <- 1 / N
	}
	else if (!is.null(n2) && !paired)
	{
		deltaHat <- t * sqrt((n1*n2)/(n1+n2))
		df <- n1 + n2 - 2
		sigmaStart <- sqrt(n1 * n2 / (n1 + n2))
	}

	if (sigmaStart < .01) 
	{
		sigmaStart <- .01
	}

	parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="BFGS")$par)
	
	if (class(parameters) == "try-error") {
		
		parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="Nelder-Mead")$par)
	}
	
	if ( ! .shouldContinue(callback()))
	{
		return()
	}
	
	if (BFH1H0)
	{
		BF10 <- BF
		BF01 <- 1 / BF10
	}
	else
	{
		BF01 <- BF
		BF10 <- 1 / BF01
	}
	
	
	# set limits plot
	xlim <- vector("numeric", 2)
	
	if (oneSided == FALSE)
	{
		xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
		
		if (n1 < 10)
		{
			if (addInformation)
			{
				stretch <- 1.52
			}
			else
			{
				stretch <- 1.4
			}
		}
		else
		{
			stretch <- 1.2
		}
	}	
	
	if (oneSided == "right")
	{
		#if (length(delta[delta >= 0]) < 10)
		#	return("Plotting is not possible: To few posterior samples in tested interval")
		
		xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])
		
		if (any(is.na(xlim)))
		{
			xlim[1] <- min(-2, .qShiftedT(0.01, parameters, oneSided="right"))
			xlim[2] <- max(2, .qShiftedT(0.99, parameters, oneSided="right"))
		}
		
		stretch <- 1.32
	}
	
	if (oneSided == "left")
	{
		#if (length(delta[delta <= 0]) < 10)
		#	return("Plotting is not possible: To few posterior samples in tested interval")
		
		xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
		xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])
		
		if (any(is.na(xlim)))
		{
			xlim[1] <-  min(-2, .qShiftedT(0.01, parameters, oneSided="left"))
			xlim[2] <- max(2,.qShiftedT(0.99, parameters, oneSided="left"))
		}
		
		stretch <- 1.32
	}
	
	xticks <- pretty(xlim)
	
	ylim <- vector("numeric", 2)
	
	ylim[1] <- 0
	dmax <- optimize(function(x).dposteriorShiftedT(x, parameters=parameters, oneSided= oneSided), interval= range(xticks), maximum = TRUE)$objective
	ylim[2] <- max(stretch * .dprior(0,r, oneSided= oneSided), stretch * dmax)# get maximum density
	
	if ( ! .shouldContinue(callback()))
	{
		return()
	}
	
	# calculate position of "nice" tick marks and create labels
	yticks <- pretty(ylim)
	xlabels <- formatC(xticks, 1, format= "f")
	ylabels <- formatC(yticks, 1, format= "f")
	
	# compute 95% credible interval & median:
	if (oneSided == FALSE)
	{
		CIlow <- quantile(delta, probs = 0.025)[[1]]
		CIhigh <- quantile(delta, probs = 0.975)[[1]]
		medianPosterior <- median(delta)
		
		if (any(is.na(c(CIlow, CIhigh, medianPosterior))))
		{
			CIlow <- .qShiftedT(0.025, parameters, oneSided=FALSE)
			CIhigh <- .qShiftedT(0.975, parameters, oneSided=FALSE)
			medianPosterior <- .qShiftedT(0.5, parameters, oneSided=FALSE)
		}
	}
	
	if (oneSided == "right")
	{
		CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
		medianPosterior <- median(delta[delta >= 0])
		
		if (any(is.na(c(CIlow, CIhigh, medianPosterior))))
		{
			CIlow <- .qShiftedT(0.025, parameters, oneSided="right")
			CIhigh <- .qShiftedT(0.975, parameters, oneSided="right")
			medianPosterior <- .qShiftedT(0.5, parameters, oneSided="right")
		}
	}
	
	if (oneSided == "left")
	{
		CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
		CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
		medianPosterior <- median(delta[delta <= 0])
		
		if (any(is.na(c(CIlow, CIhigh, medianPosterior))))
		{
			CIlow <- .qShiftedT(0.025, parameters, oneSided="left")
			CIhigh <- .qShiftedT(0.975, parameters, oneSided="left")
			medianPosterior <- .qShiftedT(0.5, parameters, oneSided="left")
		}
	}
	
	posteriorLine <- .dposteriorShiftedT(x= seq(min(xticks), max(xticks),length.out = 1000), parameters=parameters, oneSided = oneSided)
	
	xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
	
	plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(seq(min(xticks), max(xticks),length.out = 1000),posteriorLine, lwd= lwd)
	lines(seq(min(xticks), max(xticks),length.out = 1000), .dprior(seq(min(xticks), max(xticks),length.out = 1000), r=r, oneSided= oneSided), lwd= lwd, lty=3)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4)
	{
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
	}
	else if (nchar(ylabels[length(ylabels)]) == 4)
	{
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
	}
	else if (nchar(ylabels[length(ylabels)]) < 4)
	{
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext(expression(paste("Effect size", ~delta)), side = 1, cex = cexXlab, line= 2.5)
	
	points(0, .dprior(0,r, oneSided= oneSided), col="black", pch=21, bg = "grey", cex= cexPoints)
	
	if (oneSided == FALSE)
	{
		heightPosteriorAtZero <- .dposteriorShiftedT(0, parameters=parameters, oneSided=oneSided)
	}
	else if (oneSided == "right")
	{
		posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
		heightPosteriorAtZero <- posteriorLineLargerZero[1]
	}
	else if (oneSided == "left")
	{
		posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
		heightPosteriorAtZero <- posteriorLineLargerZero[length(posteriorLineLargerZero)]
	}
	
	points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
	
	# 95% credible interval
	
	# enable plotting in margin
	par(xpd=TRUE)
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
	
	medianText <- formatC(medianPosterior, digits= 3, format="f")	
	
	if (addInformation)
	{
		# display BF10 value
		offsetTopPart <- 0.06
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000)
		{
			BF10t <- formatC(BF10,3, format = "e")
			BF01t <- formatC(BF01,3, format = "e")
		}
		
		if (BF10 < 1000000 & BF01 < 1000000)
		{
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (oneSided == FALSE)
		{
			text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "right")
		{
			text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "left")
		{
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
		if (max(nchar(BF10t), nchar(BF01t)) <= 4)
		{
			xx <- grconvertX(0.44, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 5)
		{
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 6)
		{
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 7)
		{
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 8)
		{
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) > 8)
		{
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
		
		if (oneSided == FALSE)
		{
			text(xx, yy, "data|H1", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "right")
		{
			text(xx, yy, "data|H+", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "left")
		{
			text(xx, yy, "data|H-", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		# add legend
		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		
		medianLegendText <- paste("median =", medianText)
	}
	
	mostPosterior <- mean(delta > mean(range(xticks)))
	
	if (mostPosterior >= .5)
	{
		legendPosition <- min(xticks)
		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1, x.intersp= .6, seg.len= 1.2)
	}
	else
	{
		legendPosition <- max(xticks)
		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1,  x.intersp= .6, seg.len= 1.2)
	}
}
