#
# Copyright (C) 2017 University of Amsterdam
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

SummaryStatsRegressionLinearBayesian <- function(dataset=NULL, options, perform = 'run', callback = function(...) 0,  ...)
{
	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	run <- (perform == "run")

	results <- list()

	nullModelSpecified <- TRUE

	#check if null model values are different from the default values
	if(options$numberOfCovariatesNull==0 && options$unadjustedRSquaredNull==0)
	{
		nullModelSpecified <- FALSE #use default null model
	}

	meta <- list()
	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="BFrobustnessPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Linear Regression"


	fields=list()
	if(nullModelSpecified)
	{
		sampleSize.title <- paste0("n = ", options$sampleSize)
		fields[[length(fields)+1]] <- list(name="sampleSize", type="string", title=sampleSize.title)

		bf.title <- "BF"

		if(options$bayesFactorType == "LogBF10")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
	}
	else
	{
		#Bayes factor type (BF10, BF01, log(BF10))
		if (options$bayesFactorType == "BF01")
		{
			bf.title <- "BF\u2080\u2081"
		}
		else if (options$bayesFactorType == "BF10")
		{
			bf.title <- "BF\u2081\u2080"
		}
		else if (options$bayesFactorType == "LogBF10")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}

		fields[[length(fields)+1]] <- list(name="sampleSize", type="integer", title="n")
	}
	fields[[length(fields)+1]] <- list(name="numberOfCovariates", type="integer", title="Number of covariates")
	fields[[length(fields)+1]] <- list(name="unadjustedRSquared", type="number", title="R\u00B2", format = "dp:3")

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)

	fields[[length(fields)+1]] <- list(name="properror", type="number", format="sf:4;dp:3", title="% error")

	table <- list()
	if(nullModelSpecified)
	{
		table[["title"]]  <- "Model Comparison"
	}
	else
	{
		table[["title"]]  <- "Bayesian Linear Regression"
	}
	table[["schema"]]   <- list(fields=fields)
	table[["citation"]] <- list("Liang, F. and Paulo, R. and Molina, G. and Clyde, M. A. and Berger, J. O. (2008). Mixtures of g-priors for Bayesian Variable Selection. Journal of the American Statistical Association, 103, pp. 410-423",
															"Rouder, J. N. and Morey, R. D. (in press, Multivariate Behavioral Research). Bayesian testing in regression.")

	#add footnotes to the analysis result
	footnotes <- .newFootnotes()

	message <- paste0("r scale used is: ", options$priorWidth, ".")
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	table[["footnotes"]] <- as.list(footnotes)

	#initialize variables
	rowsRegressiontest <- list()
	row <- NULL
	bayesFactorRobustnessPlot <- NULL
	bayesFactorObjectNull <- NULL
	bayesFactorObject <- NULL
	bayesFactorObjectAlternative <- NULL
	plotGroups <- list()
	plots.sumstats.regression <- list()
	plotTypes <- list()
	plotVariables <- list()

	if(run)
	{
		status <- .isInputValidRegressionSummaryStatistics(options, nullModelSpecified)

		if(status$ready)
		{
			if(status$error)
			{
				table[["error"]] <- list(errorType = "badData", errorMessage = status$errorMessage)
				row <- status$row
			}
			else
			{
				if(nullModelSpecified)
				{
					BFType <- options$bayesFactorType
					BFRequiresNewRow <- TRUE

					if(!is.null(state))
					{
						BFRequiresNewRow <- FALSE
						BFTypeState <- state$options$bayesFactorType

						if((BFTypeState=="BF10" || BFTypeState=="BF01") && BFType=="LogBF10")
						{
							BFRequiresNewRow <- TRUE
						}
						else if((BFType=="BF10" || BFType=="BF01") && BFTypeState=="LogBF10")
						{
							BFRequiresNewRow <- TRUE
						}
					}

					if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) &&
					    (diff$sampleSize==FALSE && diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE && diff$numberOfCovariatesAlternative==FALSE &&
						  diff$unadjustedRSquaredAlternative==FALSE && diff$priorWidth == FALSE))) && !is.null(state$bayesFactorObjectAlternative) && !BFRequiresNewRow)
					{
						row <- state$rowsRegressiontest
						bayesFactorObjectAlternative <- state$bayesFactorObjectAlternative
						bayesFactorObjectNull <- state$bayesFactorObjectNull
					}
					else
					{
						bayesFactorObjectNull <- BayesFactor::linearReg.R2stat(N = options$sampleSize, p=options$numberOfCovariatesNull, R2=options$unadjustedRSquaredNull, rscale = options$priorWidth)
						bayesFactorObjectAlternative <- BayesFactor::linearReg.R2stat(N = options$sampleSize, p=options$numberOfCovariatesAlternative, R2=options$unadjustedRSquaredAlternative, rscale = options$priorWidth)

						BFNull <- exp(bayesFactorObjectNull[["bf"]])/exp(bayesFactorObjectAlternative[["bf"]])
						BFAlternative <- exp(bayesFactorObjectAlternative[["bf"]])/exp(bayesFactorObjectNull[["bf"]])

						if(options$bayesFactorType=="LogBF10")
						{
							BFNull <- log(BFNull)
							BFAlternative <- log(BFAlternative)
						}

						row[[1]] <- list(numberOfCovariates=options$numberOfCovariatesNull, sampleSize="Null model", unadjustedRSquared=.clean(options$unadjustedRSquaredNull), BF=.clean(BFNull), properror=.clean(bayesFactorObjectNull[["properror"]]))
						row[[2]] <- list(sampleSize="Alternative model", numberOfCovariates=.clean(options$numberOfCovariatesAlternative), unadjustedRSquared=.clean(options$unadjustedRSquaredAlternative), BF=.clean(BFAlternative), properror=.clean(bayesFactorObjectAlternative[["properror"]]))
					}
				}
				else
				{
					if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE &&
						diff$sampleSize==FALSE && diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE && diff$numberOfCovariatesAlternative==FALSE &&
						diff$unadjustedRSquaredAlternative==FALSE && diff$priorWidth == FALSE))) && !is.null(state$bayesFactorObjectAlternative))
					{
						row <- state$rowsRegressiontest
						bayesFactorObjectAlternative <- state$bayesFactorObjectAlternative
						bayesFactorObjectNull <- state$bayesFactorObjectNull
					}
					else
					{
						bayesFactorObject <- BayesFactor::linearReg.R2stat(N = options$sampleSize, p=options$numberOfCovariatesAlternative, R2=options$unadjustedRSquaredAlternative, rscale = options$priorWidth)
						BF <- unname(bayesFactorObject[["bf"]])
						errorEstimate <- unname(bayesFactorObject[["properror"]])

						if (options$bayesFactorType == "BF01")
						{
							BF <- 1/exp(bayesFactorObject[["bf"]])
						}
						else if(options$bayesFactorType == "LogBF10")
						{
							BF <- bayesFactorObject[["bf"]]
						}
						else if(options$bayesFactorType == "BF10")
						{
							BF <- exp(bayesFactorObject[["bf"]])
						}

						errorEstimate <- bayesFactorObject[["properror"]]

						row <- list(sampleSize=.clean(options$sampleSize), numberOfCovariates=.clean(options$numberOfCovariatesAlternative), unadjustedRSquared=.clean(options$unadjustedRSquaredAlternative), BF=.clean(BF), properror=.clean(errorEstimate))
					}
				}

				###### plots ######
				if(options$plotBayesFactorRobustness)
				{
					if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE &&
							diff$sampleSize==FALSE && diff$numberOfCovariatesAlternative==FALSE && diff$unadjustedRSquaredAlternative==FALSE && diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE &&
							diff$priorWidth == FALSE))) && !is.null(state$bayesFactorRobustnessPlot))
					{
						bayesFactorRobustnessPlot <- state$bayesFactorRobustnessPlot
					}
					else
					{
						plot <- list()
						plot[["title"]] <- "Bayes Factor Robustness Check"
						plot[["width"]]  <- 530
						plot[["height"]] <- 400

						if(nullModelSpecified)
						{
							p <- try(silent=FALSE, expr= {
										# image <- .beginSaveImage(530, 400)
										# .plotBF.robustnessCheck.regression.summaryStatistics(sampleSize=options$sampleSize, numberOfCovariatesNull=options$numberOfCovariatesNull, unadjustedRSquaredNull=options$unadjustedRSquaredNull, nullModelSpecified=nullModelSpecified,, numberOfCovariatesAlternative=options$numberOfCovariatesAlternative, unadjustedRSquaredAlternative=options$unadjustedRSquaredAlternative, rscale=options$priorWidth, BFH1H0=(options$bayesFactorType == "BF10"),
										# 					BF10post = ifelse((options$bayesFactorType == "BF10"), .clean(exp(bayesFactorObjectAlternative[["bf"]])/exp(bayesFactorObjectNull[["bf"]])), .clean(exp(bayesFactorObjectNull[["bf"]])/exp(bayesFactorObjectAlternative[["bf"]]))))
										# plot[["data"]] <- .endSaveImage(image)
										
										.plotFunc <- function() {
											.plotBF.robustnessCheck.regression.summaryStatistics(sampleSize=options$sampleSize, numberOfCovariatesNull=options$numberOfCovariatesNull, unadjustedRSquaredNull=options$unadjustedRSquaredNull, nullModelSpecified=nullModelSpecified,, numberOfCovariatesAlternative=options$numberOfCovariatesAlternative, unadjustedRSquaredAlternative=options$unadjustedRSquaredAlternative, rscale=options$priorWidth, BFH1H0=(options$bayesFactorType == "BF10"),
															BF10post = ifelse((options$bayesFactorType == "BF10"), .clean(exp(bayesFactorObjectAlternative[["bf"]])/exp(bayesFactorObjectNull[["bf"]])), .clean(exp(bayesFactorObjectNull[["bf"]])/exp(bayesFactorObjectAlternative[["bf"]]))))
										}
										content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
										plot[["convertible"]] <- TRUE
										plot[["obj"]] <- content[["obj"]]
										plot[["data"]] <- content[["png"]]
										
									})
						}
						else
						{
							p <- try(silent=FALSE, expr= {
										# image <- .beginSaveImage(530, 400)
										# .plotBF.robustnessCheck.regression.summaryStatistics(sampleSize=options$sampleSize, numberOfCovariatesNull=options$numberOfCovariatesNull, unadjustedRSquaredNull=options$unadjustedRSquaredNull, numberOfCovariatesAlternative=options$numberOfCovariatesAlternative, unadjustedRSquaredAlternative=options$unadjustedRSquaredAlternative,
										# 			rscale=options$priorWidth, BFH1H0=(options$bayesFactorType == "BF10"), nullModelSpecified=nullModelSpecified, BF10post = ifelse((options$bayesFactorType == "BF10"), .clean(exp(bayesFactorObject[["bf"]])), .clean(1/exp(bayesFactorObject[["bf"]]))))
										# plot[["data"]] <- .endSaveImage(image)
										
										.plotFunc <- function() {
											.plotBF.robustnessCheck.regression.summaryStatistics(sampleSize=options$sampleSize, numberOfCovariatesNull=options$numberOfCovariatesNull, unadjustedRSquaredNull=options$unadjustedRSquaredNull, numberOfCovariatesAlternative=options$numberOfCovariatesAlternative, unadjustedRSquaredAlternative=options$unadjustedRSquaredAlternative,
														rscale=options$priorWidth, BFH1H0=(options$bayesFactorType == "BF10"), nullModelSpecified=nullModelSpecified, BF10post = ifelse((options$bayesFactorType == "BF10"), .clean(exp(bayesFactorObject[["bf"]])), .clean(1/exp(bayesFactorObject[["bf"]]))))
										}
										content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
										plot[["convertible"]] <- TRUE
										plot[["obj"]] <- content[["obj"]]
										plot[["data"]] <- content[["png"]]
										
									})
						}

						if (class(p) == "try-error")
						{
							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}

						bayesFactorRobustnessPlot <- plot
					}
				}
			}
		}
		else
		{
			row <- status$row
		}

		rowsRegressiontest <- row
	}
	else #init phase
	{
		if(nullModelSpecified)
		{
			BFType <- options$bayesFactorType
			BFRequiresNewRow <- TRUE

			if(!is.null(state))
			{
				BFRequiresNewRow <- FALSE
				BFTypeState <- state$options$bayesFactorType

				if((BFTypeState=="BF10" || BFTypeState=="BF01") && BFType=="LogBF10")
				{
					BFRequiresNewRow <- TRUE
				}
				else if((BFType=="BF10" || BFType=="BF01") && BFTypeState=="LogBF10")
				{
					BFRequiresNewRow <- TRUE
				}
			}

			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (
					diff$sampleSize==FALSE && diff$numberOfCovariatesAlternative==FALSE && diff$unadjustedRSquaredAlternative==FALSE && diff$priorWidth == FALSE &&
					diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE))) && !is.null(state$bayesFactorObjectAlternative) && !BFRequiresNewRow)
			{
				rowsRegressiontest <- state$rowsRegressiontest
				bayesFactorObjectNull <- state$bayesFactorObjectNull
				bayesFactorObjectAlternative <- state$bayesFactorObjectAlternative
			}
			else
			{
				rowsRegressiontest <- .isInputValidRegressionSummaryStatistics(options, nullModelSpecified)$row
			}
		}
		else
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE &&
					diff$sampleSize==FALSE && diff$numberOfCovariatesAlternative==FALSE && diff$unadjustedRSquaredAlternative==FALSE && diff$priorWidth == FALSE &&
					diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE))) && !is.null(state$bayesFactorObjectAlternative))
			{
				rowsRegressiontest <- state$rowsRegressiontest
				bayesFactorObjectAlternative <- state$bayesFactorObjectAlternative
			}
			else
			{
				rowsRegressiontest <- .isInputValidRegressionSummaryStatistics(options, nullModelSpecified)$row
			}
		}

		if(options$plotBayesFactorRobustness)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE &&
					diff$sampleSize==FALSE && diff$numberOfCovariatesAlternative==FALSE && diff$unadjustedRSquaredAlternative==FALSE && diff$numberOfCovariatesNull==FALSE && diff$unadjustedRSquaredNull==FALSE &&
					diff$priorWidth == FALSE))) && !is.null(state$bayesFactorRobustnessPlot))
			{
				bayesFactorRobustnessPlot <- state$bayesFactorRobustnessPlot
			}
			else
			{
				plot <- list()

				plot[["title"]] <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400

				p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotBF.robustnessCheck.regression.summaryStatistics(BFH1H0=(options$bayesFactorType == "BF10"), dontPlotData= TRUE)
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotBF.robustnessCheck.regression.summaryStatistics(BFH1H0=(options$bayesFactorType == "BF10"), dontPlotData= TRUE)
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

				if (class(p) == "try-error")
				{
					errorMessage <- .extractErrorMessage(p)
					plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
				}

				bayesFactorRobustnessPlot <- plot
			}
		}
	}

	if(nullModelSpecified)
	{
		table[["data"]][[1]] <- rowsRegressiontest[[1]]
		table[["data"]][[2]] <- rowsRegressiontest[[2]]
	}
	else
	{
		table[["data"]] <- list(rowsRegressiontest)
	}

	if (options$plotBayesFactorRobustness)
	{
		results[["inferentialPlots"]] <- list(title="Inferential Plot", BFrobustnessPlot=bayesFactorRobustnessPlot)
	}

	results[["table"]] <- table

	keep <- NULL
	keep <- c(keep, bayesFactorRobustnessPlot$data)

	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state, keep=keep))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactorRobustnessPlot=bayesFactorRobustnessPlot,
								rowsRegressiontest=rowsRegressiontest, bayesFactorObjectAlternative=bayesFactorObjectAlternative, bayesFactorObjectNull=bayesFactorObjectNull), keep=keep))
	}
}


.isInputValidRegressionSummaryStatistics <- function(options, nullModelSpecified)
{
	error <- FALSE
	errorMessage <- NULL
	ready <- TRUE
	row <- NULL

	sampleSizeValue <- options$sampleSize

	if(nullModelSpecified)
	{
		unadjustedRSquaredNullValue <- options$unadjustedRSquaredNull
		numberOfCovariatesNullValue <- options$numberOfCovariatesNull
	}

	numberOfCovariatesAlternativeValue <- options$numberOfCovariatesAlternative
	unadjustedRSquaredAlternativeValue <- options$unadjustedRSquaredAlternative

	if(nullModelSpecified)
	{
		if(numberOfCovariatesNullValue==0 || is.null(numberOfCovariatesNullValue))
		{
			ready <- FALSE
			numberOfCovariatesNullValue <- "."
		}

		if(options$numberOfCovariatesNull!=0 && options$sampleSize!=0)
		{
			if((options$sampleSize - options$numberOfCovariatesNull) < 2)
			{
				ready <- TRUE
				error <- TRUE
				errorMessage <- paste("Number of Covariates must be less than N-1 (sample size minus 1)")
			}
		}
	}

	if(!error)
	{
		if((sampleSizeValue==0 && numberOfCovariatesAlternativeValue==0 && unadjustedRSquaredAlternativeValue==0) || is.null(unadjustedRSquaredAlternativeValue))
		{
			ready <- FALSE
			unadjustedRSquaredAlternativeValue <- "."
		}

		if(sampleSizeValue==0 || is.null(sampleSizeValue))
		{
			ready <- FALSE
			sampleSizeValue <- "."
		}

		if(numberOfCovariatesAlternativeValue==0 || is.null(numberOfCovariatesAlternativeValue))
		{
			ready <- FALSE
			numberOfCovariatesAlternativeValue <- "."
		}

		if(options$numberOfCovariatesAlternative > options$numberOfCovariatesNull)
		{
			if(options$unadjustedRSquaredAlternative < options$unadjustedRSquaredNull)
			{
				ready <- TRUE
				error <- TRUE
				errorMessage <- paste("Input: When number of covariates for Alternative hypothesis is greater than that of Null hypothesis, the R\u00B2 has to be higher under Alternative than under Null hypothesis")
			}
		}

		if(options$numberOfCovariatesAlternative!=0 && options$sampleSize!=0)
		{
			if((options$sampleSize - options$numberOfCovariatesAlternative) < 2)
			{
				error <- TRUE
				errorMessage <- paste("Number of Covariates must be less than N-1 (sample size minus 1)")
			}
		}
	}

	if(nullModelSpecified)
	{
		row[[1]] <- list(sampleSize="Null model", numberOfCovariates=numberOfCovariatesNullValue, unadjustedRSquared=unadjustedRSquaredNullValue, BF=".", properror=".")
		row[[2]] <- list(sampleSize="Alternative model", numberOfCovariates=numberOfCovariatesAlternativeValue, unadjustedRSquared=unadjustedRSquaredAlternativeValue, BF=".", properror=".")
	}
	else
	{
		row <- list(sampleSize=sampleSizeValue, numberOfCovariates=numberOfCovariatesAlternativeValue, unadjustedRSquared=unadjustedRSquaredAlternativeValue, BF=".", properror=".")
	}

	list(ready=ready, row=row, error=error, errorMessage=errorMessage)
}


#function to plot the Bayes factor Robustness Check plots
.plotBF.robustnessCheck.regression.summaryStatistics <- function(sampleSize= NULL, numberOfCovariatesNull= NULL, unadjustedRSquaredNull=NULL, numberOfCovariatesAlternative= NULL, unadjustedRSquaredAlternative=NULL, nullModelSpecified=FALSE,
	BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, lwd= 2, cexPoints= 1.4, cexAxis= 1.2, cexYXlab= 1.5,  cexText=1.2, cexLegend= 1.4, lwdAxis= 1.2, cexEvidence= 1.6, BFH1H0 = TRUE, dontPlotData= FALSE)
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


	par(mar= c(5, 6, 6, 7) + 0.1, las=1)

	if (dontPlotData)
	{
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")

		if (BFH1H0)
		{
			mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
		}
		else
		{
			mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
		}

		mtext("r scale", side = 1, cex = cexYXlab, line= 2.5)
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

	if(nullModelSpecified)
	{
		for (i in seq_along(rValues)) {

			BFNull <- BayesFactor::linearReg.R2stat(N = sampleSize, p = numberOfCovariatesNull, R2 = unadjustedRSquaredNull, rscale = rValues[i])
			BFAlternative <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = rValues[i])
			BF10[i] <- .clean(exp(BFAlternative[["bf"]])/exp(BFNull[["bf"]]))

			if ( ! .shouldContinue(callback()))
				return()
		}
	}
	else
	{
		for (i in seq_along(rValues)) {

			BF <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = rValues[i])
			BF10[i] <- .clean(exp(BF[["bf"]]))

			if ( ! .shouldContinue(callback()))
				return()
		}
	}

	if(nullModelSpecified)
	{
		# BF10 "medium" prior
		BF10mNull <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesNull, R2=unadjustedRSquaredNull, rscale = sqrt(2) / 2)
		BF10mAlternative <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = sqrt(2) / 2)
		BF10m <- .clean(exp(BF10mAlternative[["bf"]])/exp(BF10mNull[["bf"]]))
		BF10mText <- BF10m

		# BF10 "wide" prior
		BF10wNull <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesNull, R2=unadjustedRSquaredNull, rscale = 1)
		BF10wAlternative <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = 1)
		BF10w <- .clean(exp(BF10wAlternative[["bf"]])/exp(BF10wNull[["bf"]]))
		BF10wText <- BF10w

		# BF10 "ultrawide" prior
		BF10ultraNull <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesNull, R2=unadjustedRSquaredNull, rscale = sqrt(2))
		BF10ultraAlternative <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = sqrt(2))
		BF10ultra <- .clean(exp(BF10ultraAlternative[["bf"]])/exp(BF10ultraNull[["bf"]]))
		BF10ultraText <- BF10ultra
	}
	else
	{
		# BF10 "medium" prior
		BF10m <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = sqrt(2) / 2)
		BF10m <- .clean(exp(BF10m[["bf"]]))
		BF10mText <- BF10m

		# BF10 "wide" prior
		BF10w <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = 1)
		BF10w <- .clean(exp(BF10w[["bf"]]))
		BF10wText <- BF10w

		# BF10 "ultrawide" prior
		BF10ultra <- BayesFactor::linearReg.R2stat(N = sampleSize, p=numberOfCovariatesAlternative, R2=unadjustedRSquaredAlternative, rscale = sqrt(2))
		BF10ultra <- .clean(exp(BF10ultra[["bf"]]))
		BF10ultraText <- BF10ultra
	}

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

		if (BFH1H0)
		{
			mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= line)
		}
		else
		{
			mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= line)
		}
	}

	if (omit3s == FALSE)
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

	mtext("r scale", side = 1, cex = cexYXlab, line= 2.5)

	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))

	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

	xxt <- grconvertX(0.28, "npc", "user")

	if (BFH1H0)
	{
		text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
	}
	else
	{
		text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
	}

	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))

	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

	if (BFH1H0)
	{
		text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
	}
	else
	{
		text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
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

	if( BF10userText >= BF01userText)
	{
		userBF <- bquote(BF[10]==.(BF10usert))
	}
	else
	{
		userBF <- bquote(BF[0][1]==.(BF01usert))
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

	if (BF10wText >= BF01wText)
	{
		wBF <- bquote(BF[10]==.(BF10wt))
	}
	else
	{
		wBF <- bquote(BF[0][1]==.(BF01wt))
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

	if (BF10ultraText >= BF01ultraText)
	{
		ultraBF <- bquote(BF[10]==.(BF10ultrat))
	}
	else
	{
		ultraBF <- bquote(BF[0][1]==.(BF01ultrat))
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
