#
# Copyright (C) 2013-2015 University of Amsterdam
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

BainRegressionLinearBayesian <- function (jaspResults, dataset, options, state=NULL) {
	# Read in data and check for errors
	readList 							<- .readDataBainLinearRegression(options, dataset)
	dataset                                         <- readList[["dataset"]]
	missingValuesIndicator                          <- readList[["missingValuesIndicator"]]
	# Null state
	if(is.null(state))
	  state 							<- list()
	# Pass the title
	jaspResults$title 					<- "Bain Linear Regression"	
	# Create the main results table
	.bainLinearRegressionTable(dataset, options, jaspResults, missingValuesIndicator)
	# Save analysis result in state
	bainResult 							<- jaspResults[["bainResult"]]$object	
	# Legend Table 
	.bainLegendRegression(dataset, options, jaspResults)
	# Coefficients
	if (options$coefficients)
	{
		if(is.null(jaspResults[["coefficients"]]))
			.bainCoefficientsRegression(dataset, options, jaspResults, bainResult)        
	}
	# Bayes factor matrix
	if (options$BFmatrix)
	{
		if(is.null(jaspResults[["Bainmatrix"]]))
			.BainBFmatrix(dataset, options, jaspResults, bainResult, type = "regression")        
	}
	# Bayes factor plot
	if(options$BFplot)
    {
        if(is.null(jaspResults[["BFplot"]]))
        {
        jaspResults[["BFplot"]] 		<- .bainRegressionPlot(dataset, options, bainResult, "Bayes Factor Comparison")
        jaspResults[["BFplot"]]			$dependOnOptions(c("BFplot", "covariates", "dependent", "model", "standardized"))
				jaspResults[["BFplot"]] 		$position <- 4
		}
	}
    # Save the state    
	state[["options"]] 					<- options
	return(state)    
}

.bainLinearRegressionTable <- function(dataset, options, jaspResults, missingValuesIndicator){
	
	if(!is.null(jaspResults[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
	variables                      	<- c(options$dependent, unlist(options$covariates))
	
	dependent 						<- .v(options$dependent)
	covariates 						<- .v(options$covariates)
	bainTable                      	<- createJaspTable("Bain Linear Regression Result")
	jaspResults[["bainTable"]]     	<- bainTable
	
	bainTable$dependOnOptions(c("bayesFactorType", "dependent", "covariates", "model", "standardized"))
	
	if (options$bayesFactorType == "BF10") {      
		bf.title <- "BF.c"        
	} else if (options$bayesFactorType == "BF01") {               
		bf.title <- "BFc."        
	}    
	if(options$logScale == "logBF"){
		bf.title <- paste0("Log(",bf.title,")")
	}
	
	bainTable$addColumnInfo(name="hypotheses", type="string", title="")
	bainTable$addColumnInfo(name="BF", type="number", format="sf:4;dp:3", title=bf.title)
	bainTable$addColumnInfo(name="PMP1", type="number", format="sf:4;dp:3", title="PMP a")
	bainTable$addColumnInfo(name="PMP2", type="number", format="sf:4;dp:3", title="PMP b")
	
	bainTable$position <- 1
	
	message <- "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement. 
				Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities."	
	bainTable$addFootnote(message=message, symbol="<i>Note.</i>")
	
	if(any(variables %in% missingValuesIndicator)){
		i <- which(variables %in% missingValuesIndicator)
		if(length(i) > 1){
			bainTable$addFootnote(message= paste0("The variables ", paste(variables[i], collapse = ", "), " contain missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		} else if (length(i) == 1){
			bainTable$addFootnote(message= paste0("The variable ", variables[i], " contains missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		}
	}
	
	bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology. DOI:10.1111/bmsp.12110")
	bainTable$addCitation("Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods.")
	bainTable$addCitation("Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. Britisch Journal of Mathematical and Statistical Psychology. DOI: 10.1111/bmsp.12145")
		
	if(length(variables) > 1 && options$dependent != ""){
	
		if(options$model == ""){
			
			formula <- paste(dependent, "~", paste(covariates, collapse=' + '))
			
			# We have to make a default matrix depending on the levels of the grouping variable...meh
			# The default hypothesis is that all covariates = 0 (e.g., 2 covariates, "p1=p2=0")
			len <- length(covariates)
			
			if(len == 1){
				
				ERr <- matrix(c(1,0), ncol = 2, byrow = TRUE)
				
			} else {
			
			null.mat <- matrix(0, nrow = (len-1), ncol = (len+1))
			indexes <- row(null.mat) - col(null.mat)
			null.mat[indexes == 0] <- 1
			null.mat[indexes == -1] <- -1
			
			ERr <- null.mat
			
			}
			
		    IRr<-NULL
			
			p <- try({
				
				bainResult <- Bain::Bain_regression(formula = formula, data = dataset, standardize = options$standardized, ERr, IRr)
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("bayesFactorType", "dependent", "covariates", "model", "standardized"))
				
			})
		
		} else {
			
			jaspResults$startProgressbar(3)
			jaspResults$progressbarTick()
			
			formula <- paste(.unv(dependent), "~", paste(.unv(covariates), collapse=' + '))
			
			rest.string <- options$model
			rest.string <- gsub("\n", ";", rest.string)
			
			jaspResults$progressbarTick()
			
			inpt <- list()
			inpt[[1]] <- formula
			names(dataset) <- .unv(names(dataset))
			inpt[[2]] <- dataset
			inpt[[3]] <- rest.string
			inpt[[4]] <- options$standardized
			
			p <- try({
				
				bainResult <- Bain::Bain_regression_cm(formula = inpt[[1]], data = inpt[[2]], hyp = inpt[[3]], standardize = inpt[[4]])
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("bayesFactorType", "dependent", "covariates", "model", "standardized"))
			
			})
		}
		
		if(class(p) == "try-error"){
			
			message <- "An error occurred in the analysis. Please make sure your hypotheses are formulated correctly."
			bainTable$errorMessage <- message
			bainTable$error <- "badData"
			return()
			
		} else {
			
			jaspResults$progressbarTick()
		
			BF <- bainResult$BF			
			if(options$bayesFactorType == "BF01"){		
				BF <- 1/BF		
			} 		
			if(options$logScale == "logBF"){
				BF <- log(BF)
			}		
			for(i in 1:length(BF)){
				row <- list(hypotheses = paste0("H",i), BF = .clean(BF[i]), PMP1 = .clean(bainResult$PMPa[i]), PMP2 = .clean(bainResult$PMPb[i]))
				bainTable$addRows(row)
			}
			row <- list(hypotheses = "Hu", BF = "", PMP1 = "", PMP2 = .clean(1-sum(bainResult$PMPb)))
			bainTable$addRows(row)
				
			}
			
		} else {
			
			row <- list(hypotheses = "H1", BF = ".", PMP1 = ".", PMP2 = ".")
			bainTable$addRows(row)
			row <- list(hypotheses = "Hu", BF = ".", PMP1 = ".", PMP2 = ".")
			bainTable$addRows(row)
			
		}
	
}

.bainRegressionPlot <- function(dataset, options, bainResult, title){
	if(is.null(bainResult))
	  return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))
	png(tempfile())
	p <- .plot.BainR(bainResult)
	dev.off()
	BFplot <- createJaspPlot(plot=p, title=title, width = options$plotWidth, height = options$plotHeight)    
	return(BFplot)
}

.bainCoefficientsRegression <- function(dataset, options, jaspResults, bainResult){
	
	if(!is.null(jaspResults[["coefficients"]])) return() #The options for this table didn't change so we don't need to rebuild it

	variables                                               <- c(unlist(options$dependent), unlist(options$covariates))
	coefficients                                            <- createJaspTable("Coefficients")
	jaspResults[["coefficients"]]                           <- coefficients
	coefficients$dependOnOptions(c("dependent", "covariates", "model", "standardized", "coefficients"))
	
	interval <- options$CredibleInterval
	overTitle <- title <- paste0(interval, "% Credible Interval")
	
	coefficients$addColumnInfo(name="v",    				title="Covariate",   type="string")
	coefficients$addColumnInfo(name="mean", 				title="Coefficient", type="number", format="sf:4;dp:3")
	coefficients$addColumnInfo(name = "SE", 				title = "se", type = "number", format="sf:4;dp:3")
	coefficients$addColumnInfo(name="CiLower",              title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
  coefficients$addColumnInfo(name="CiUpper",              title = "upperCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
	
	coefficients$position <- 2
	
	if(is.null(bainResult))
		return()
					
	sum_model <- bainResult$estimate_res
			
	if(!options$standardized){
				
		covcoef <- data.frame(sum_model$coefficients)
				
				groups <- rownames(covcoef)
				estim <- summary(sum_model)$coefficients[, 1]
				SE <- summary(sum_model)$coefficients[, 2]
				CiLower <- estim - (1.96 * SE)
				CiUpper <- estim + (1.96 * SE)
			
		} else {
			
			covcoef <- data.frame(sum_model$CIs)
			groups <- .v(options$covariates)
			estim <- covcoef[, 2]
			SE <- sum_model$SEs
			CiLower <- covcoef[, 1]
			CiUpper <- covcoef[, 3]
			
	}
			
	for(i in 1:length(estim)){
		if(i == 1 && !options$standardized){
			row <- list(v = groups[i], mean = .clean(estim[i]), SE = .clean(SE[i]), CiLower = .clean(CiLower[i]), CiUpper = .clean(CiUpper[i]))
			coefficients$addRows(row) 
		} else {
			row <- list(v = .unv(groups[i]), mean = .clean(estim[i]), SE = .clean(SE[i]), CiLower = .clean(CiLower[i]), CiUpper = .clean(CiUpper[i]))
			coefficients$addRows(row) 
		}		
	}
	
}

.readDataBainLinearRegression <- function(options, dataset){
	
	all.variables 						<- c(options$dependent, unlist(options$covariates))
	all.variables 						<- all.variables[all.variables != ""]
	
	if (is.null(dataset)) {		
		
		trydata                                 <- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator                  <- .unv(names(which(apply(trydata, 2, function(x){ any(is.na(x))} ))))
		
		dataset 						<- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)		
	} else {
		dataset 						<- .vdf(dataset, columns.as.numeric=all.variables)
	}	
	
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, message="short", observations.amount="< 3", 
				exitAnalysisIfErrors = TRUE)
	
	readList <- list()
  readList[["dataset"]] <- dataset 
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  
  return(readList)
}

.bainLegendRegression <- function(dataset, options, jaspResults){
	
	if(!is.null(jaspResults[["legendTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
	legendTable                      	<- createJaspTable("Hypothesis Legend")
	jaspResults[["legendTable"]]     	<- legendTable
	
	legendTable$dependOnOptions(c("model", "covariates"))
	legendTable$position <- 0
	
	legendTable$addColumnInfo(name="number", type="string", title="Abbreviation")
	legendTable$addColumnInfo(name="hypothesis", type="string", title="Hypothesis")
	
	if(options$model != ""){	
		rest.string <- options$model
		rest.string <- gsub("\n", ";", rest.string)
		hyp.vector <- unlist(strsplit(rest.string, "[;]"))
		
			for(i in 1:length(hyp.vector)){
				row <- list(number = paste0("H",i), hypothesis = hyp.vector[i])
				legendTable$addRows(row) 
			}
	} else {
		
		variables <- options$covariates
		if(length(variables) == 1){
			string <- paste(variables, "= 0")
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row) 
		} else {
			string <- paste(variables, collapse = " = ")
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row) 
		}
		
	}
	
}

.plot.BainR <- function (x, y, ...) 
{
    PMPa <- x$PMPa
    PMPb <- c(x$PMPb, 1 - sum(x$PMPb))
    numH <- length(x$BF)
    P_lables <- paste("H", 1:numH, sep = "")
    ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
    ggdata2 <- data.frame(lab = c(P_lables, "Hu"), PMP = PMPb)
    if (numH == 1) {
        p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, 
                            fill = lab)) + 
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) + 
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) + 
            ggplot2::labs(x = "", y = "", title = "PMP") + 
						ggplot2::theme(panel.grid = ggplot2::element_blank(), 
                          legend.position = "none") + 
						ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, 
																				labels = rev(c(P_lables, "Hu")))
				pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(), 
																	axis.text=ggplot2::element_text(size=17, color = "black"), 
																	plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
        return(pp)
    }
    if (numH > 1) {
        p <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP, 
                            	fill = lab)) + 
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) + 
            ggplot2::geom_col()
        p1 <- p + ggplot2::coord_polar(theta = "y", direction = -1) + 
            ggplot2::labs(x = "", y = "", title = "PMP excluding Hu") + 
            ggplot2::theme(panel.grid = ggplot2::element_blank(), 
													legend.position = "none") + 
            ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2, 
                               					labels = rev(P_lables))
				p1 <- p1 + ggplot2::theme(panel.background = ggplot2::element_blank(), 
	                                axis.text=ggplot2::element_text(size=17, color = "black"), 
	                                plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				p1 <- p1 + ggplot2::scale_fill_brewer(palette="Set1")
        p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, 
                            	fill = lab)) + 
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) + 
            ggplot2::geom_col()
        p2 <- p + ggplot2::coord_polar(theta = "y", direction = -1) + 
            ggplot2::labs(x = "", y = "", title = "PMP including Hu") + 
            ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") + 
            ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, 
                               					labels = rev(c(P_lables, "Hu")))
				p2 <- p2 + ggplot2::theme(panel.background = ggplot2::element_blank(), 
	                                axis.text=ggplot2::element_text(size=17, color = "black"), 
	                                plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				p2 <- p2 + ggplot2::scale_fill_brewer(palette="Set1")
        pp <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p1, p2, ncol = 2))
        return(pp)
    }
}
