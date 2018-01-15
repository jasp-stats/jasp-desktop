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

BainAncovaBayesian	 <- function (jaspResults, dataset, options, state=NULL) {
	# Read in data and check for errors
	dataset 								<- .readDataBainAncova(options, dataset)
	# Null state
	if(is.null(state))
	  state 								<- list()
	# Pass the title
	jaspResults$title 						<- "Bayesian Informative ANCOVA"
	# Create the main results table
	.bainANCOVATable(dataset, options, jaspResults)
	# Save analysis result in state
	bainResult 								<- jaspResults[["bainResult"]]$object
	# Bayes factor matrix
	if (options$BFmatrix)
	{
		if(is.null(jaspResults[["Bainmatrix"]]))
			.BainBFmatrix(dataset, options, jaspResults, bainResult, type = "ancova")        
	}
	# Coefficients
	if (options$coefficients)
	{
		if(is.null(jaspResults[["coefficients"]]))
			.bainCoefficients(dataset, options, jaspResults, bainResult)        
	}
	# Bayes factor plot
	if(options$BFplot)
    {
        if(is.null(jaspResults[["BFplot"]]))
        {
        	jaspResults[["BFplot"]] 			<- .bainANCOVAPlot(dataset, options, bainResult, "Bayes factor Plot")
        	jaspResults[["BFplot"]]				$dependOnOptions(c("dependent", "fixedFactors", "covariates", "BFplot", "model"))
		}
	}
	# Descriptives plot
	if(options$plotDescriptives)
	{
		if(is.null(jaspResults[["descriptivesPlot"]])){
			jaspResults[["descriptivesPlot"]] 	<- createJaspPlot(plot= .bainDescriptivesPlot(dataset, bainResult, options, type = "ancova"), 
												title="Descriptives plot", width = options$plotWidth, height = options$plotHeight) 
			jaspResults[["descriptivesPlot"]]	$dependOnOptions(c("dependent", "fixedFactors", "plotDescriptives", "covariates"))
		}
	}	
	# Save the state	
	state[["options"]] <- options
	return(state)	
}

.bainANCOVATable <- function(dataset, options, jaspResults){
	
	if(!is.null(jaspResults[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
	if (options$bayesFactorType == "BF10") {      
		bf.title <- "BF.c"        
	} else if (options$bayesFactorType == "BF01") {               
		bf.title <- "BFc."        
	}    
	if(options$logScale == "logBF"){
		bf.title <- paste0("Log(",bf.title,")")
	}
	
	dependent 						<- .v(options$dependent)
	group 							<- .v(options$fixedFactors)
	covariates 						<- .v(options$covariates)	
	bainTable                      	<- createJaspTable("ANCOVA result")
	jaspResults[["bainTable"]]     	<- bainTable
	
	bainTable$dependOnOptions(c("dependent", "fixedFactors", "covariates", "logScale", "model"))
	
	bainTable$addColumnInfo(name="hypotheses", 				type="string", title="")
	bainTable$addColumnInfo(name="BF", 						type="number", format="sf:4;dp:3", title=bf.title)
	bainTable$addColumnInfo(name="PMP1", 					type="number", format="sf:4;dp:3", title="PMP a")
	bainTable$addColumnInfo(name="PMP2", 					type="number", format="sf:4;dp:3", title="PMP b")
	
	message <-  "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement. 
				PMP a indicates posterior probability for each hypothesis excluding unconstrained hypothesis. 
				PMP b indicates posterior probability for each hypothesis including unconstrained hypothesis."	
	bainTable$addFootnote(message=message, symbol="<i>Note.</i>")
	
	bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110")
				
	bain.variables <- c(unlist(options$dependent),
						unlist(options$covariates)[1],
						unlist(options$fixedFactors))
	bain.variables <- bain.variables[bain.variables != ""]
	
 	if(length(bain.variables) > 2){
			
		groupVars <- options$fixedFactors
		groupVars <- unlist(groupVars)		
		groupCol <- dataset[ , .v(groupVars)]
		varLevels <- levels(groupCol)
			
		if(length(varLevels) > 15){	
			
			message <- "The fixed factor has too many levels for a Bain analysis."	
			bainTable$errorMessage <- message
			bainTable$error <- "badData"
			return()
			
		} 
			
		if(options$model == ""){
			
			# We have to make a default matrix depending on the levels of the grouping variable...meh
			# The default hypothesis is that all groups are equal (e.g., 3 groups, "p1=p2=p3")
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			len <- length(varLevels)
			
			null.mat <- matrix(0, nrow = (len-1), ncol = (len+1))
			indexes <- row(null.mat) - col(null.mat)
			null.mat[indexes == 0] <- 1
			null.mat[indexes == -1] <- -1
			
			ERr <- null.mat
		    IRr<-NULL
			
			p <- try({
				
				bainResult <- Bain::Bain_ancova(X = dataset, dep_var = dependent, covariates = covariates, group = group, ERr, IRr)
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("dependent", "fixedFactors", "covariates", "logScale", "model"))
				
			})
		
		} else {
			
			jaspResults$startProgressbar(3)
			jaspResults$progressbarTick()
			
			rest.string <- options$model
			rest.string <- gsub("\n", ";", rest.string)
			
			jaspResults$progressbarTick()

			inpt <- list()
			names(dataset) <- .unv(names(dataset))
			inpt[[1]] <- dataset
			inpt[[2]] <- .unv(dependent)
			inpt[[3]] <- .unv(covariates)
			inpt[[4]] <- .unv(group)
			inpt[[5]] <- rest.string
			
			p <- try({
				
				bainResult <- Bain::Bain_ancova_cm(X = inpt[[1]], dep_var = inpt[[2]], covariates = inpt[[3]], group = inpt[[4]], hyp = inpt[[5]])
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("dependent", "fixedFactors", "covariates", "logScale", "model"))
			
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
					
			if(options$bayesFactorType == "BF01")		
				BF <- 1/BF					
			if(options$logScale == "logBF")
				BF <- log(BF)
					
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

.BainBFmatrix <- function(dataset, options, jaspResults, bainResult, type){
	
	if(!is.null(jaspResults[["Bainmatrix"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
	Bainmatrix                                            <- createJaspTable("Bayes factor matrix")
	jaspResults[["Bainmatrix"]]                           <- Bainmatrix
	
	if(type == "regression")
		Bainmatrix$dependOnOptions(c("dependent", "covariates", "logScale", "model", "BFmatrix"))
	if(type == "ancova")
		Bainmatrix$dependOnOptions(c("dependent", "fixedFactors", "covariates", "logScale", "model", "BFmatrix"))
		if(type == "anova")
			Bainmatrix$dependOnOptions(c("dependent", "fixedFactors", "logScale", "model", "BFmatrix"))
	
	if(options$logScale == "logBF"){
		message <- "Logarithmic Bayes factors are displayed."
		Bainmatrix$addFootnote(message=message, symbol="<i>Note.</i>")
	}
	
	if(is.null(bainResult)){
		return()
	} 
	if(!is.null(bainResult)) {
		
		BFmatrix <- diag(1, length(bainResult$BF))
		
		for (h1 in 1:length(bainResult$BF)) {
			for (h2 in 1:length(bainResult$BF)) {
				BFmatrix[h1, h2] <- bainResult$fit[h1]/bainResult$fit[h2]/(bainResult$complexity[h1]/bainResult$complexity[h2])
			}
		}
		
		if(options$logScale == "logBF"){
			BFmatrix <- log(BFmatrix)
		}
		
	}
	
	Bainmatrix$addColumnInfo(name = "hypothesis", title = "", type = "string")
	for(i in 1:nrow(BFmatrix)){
		Bainmatrix$addColumnInfo(name = paste0("H", i), title = paste0("H", i), type = "number", format="sf:4;dp:3")
	}
	
	if(is.null(bainResult)){
		for(i in 1:nrow(BFmatrix)){
			tmp <- list(hypothesis = paste0("H", i))
			for(j in 1:ncol(BFmatrix)){
				tmp[[paste0("H", j)]] <- "."
			}
			row <- tmp
			Bainmatrix$addRows(row) 
		}
	} else {		
			for(i in 1:nrow(BFmatrix)){
				tmp <- list(hypothesis = paste0("H", i))
				for(j in 1:ncol(BFmatrix)){
					tmp[[paste0("H", j)]] <- .clean(BFmatrix[i,j])
				}
				row <- tmp
				Bainmatrix$addRows(row) 
			}		
		}	
	}

.bainANCOVAPlot <- function(dataset, options, bainResult, title){
	if(is.null(bainResult))
	  return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))
	BFplot <- createJaspPlot(plot=function() { Bain::plot.BainA(bainResult) }, title=title, width = options$plotWidth, height = options$plotHeight)    
	return(BFplot)
}

.bainCoefficients <- function(dataset, options, jaspResults, bainResult){
	
	if(!is.null(jaspResults[["coefficients"]])) return() #The options for this table didn't change so we don't need to rebuild it

	coefficients                      	<- createJaspTable("Coefficients for Groups plus Covariates")
	jaspResults[["coefficients"]]     	<- coefficients
	coefficients$dependOnOptions(c("dependent", "fixedFactors", "covariates", "coefficients", "model"))
	
	coefficients$addColumnInfo(name="v",    				title="Covariate",   type="string")
	coefficients$addColumnInfo(name="N",					title = "N", type = "integer")
	coefficients$addColumnInfo(name="mean", 				title="Coefficient", type="number", format="sf:4;dp:3")
	coefficients$addColumnInfo(name = "SE", 				title = "SE", type = "number", format = "sf:4;dp:3")
	coefficients$addColumnInfo(name="CiLower",              title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = "95% Credible Interval")
    coefficients$addColumnInfo(name="CiUpper",              title = "upperCI", type="number", format="sf:4;dp:3", overtitle = "95% Credible Interval")
		
	if(is.null(bainResult))
		return()
	
	if(!is.null(bainResult)){
			
		sum_model <- bainResult$estimate_res
		covcoef <- data.frame(sum_model$coefficients)
		SEs <- summary(sum_model)$coefficients[, 2]
		
		rownames(covcoef) <- gsub("groupf", "", rownames(covcoef))
		x <- rownames(covcoef)
		x <- sapply(regmatches(x, gregexpr("covars", x)), length)
		x <- sum(x)	
		if(x > 1){
		    rownames(covcoef)[(length(rownames(covcoef)) - (x-1)):length(rownames(covcoef))] <- options$covariates
		} else {
		    rownames(covcoef) <- gsub("covars", options$covariates, rownames(covcoef))
		}
		# mucho fixo
			
		groups <- rownames(covcoef)
		estim <- covcoef[, 1]
		CiLower <- estim - 1.96 * SEs
		CiUpper <- estim + 1.96 * SEs
			
		groupVars <- options$fixedFactors
		groupVars <- unlist(groupVars)	
		groupCol <- dataset[ , .v(groupVars)]
		varLevels <- levels(groupCol)
		
		N <- NULL
		
		for(variable in varLevels){
			
			column <- dataset[ , .v(options$dependent)]
			column <- column[which(groupCol == variable)]
			
			N <- c(N,length(column))
			
		}
		
		covVars <- options$covariates
		covVars <- unlist(covVars)
		
		for(var in covVars){
			
			col <- dataset[ , .v(var)]
			col <- na.omit(col)
			N <- c(N, length(col))
			
		}
				
		for(i in 1:length(groups)){
			row <- list(v = groups[i], mean = .clean(estim[i]), N = N[i], SE = .clean(SEs[i]), CiLower = .clean(CiLower[i]), CiUpper = .clean(CiUpper[i]))
			coefficients$addRows(row)	
		}
			
	} else {
		row <- list(v = ".", mean = ".", N = ".", SE = ".", CiLower = ".", CiUpper = ".")
		coefficients$addRows(row)		
	}
}

.readDataBainAncova <- function(options, dataset){
	
	numeric.variables 						<- c(unlist(options$dependent),unlist(options$covariates))
	numeric.variables 						<- numeric.variables[numeric.variables != ""]
	factor.variables 						<- unlist(options$fixedFactors)
	factor.variables 						<- factor.variables[factor.variables != ""]
	all.variables							<- c(numeric.variables, factor.variables)
	
	if (is.null(dataset)) {		
		dataset 							<- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)		
	} else {
		dataset 							<- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}
	
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, message="short", observations.amount="< 3", 
				exitAnalysisIfErrors = TRUE)
	
	return(dataset)
	
}
