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

BainAnovaBayesian <- function (jaspResults, dataset, options, state=NULL) {
	# Read in data and check for errors
	dataset 									<- .readDataBainAnova(options, dataset)
	# Null state
	if(is.null(state))
	  state 									<- list()
	# Pass the title
	jaspResults$title 							<- "Bayesian Informative ANOVA"	
	# Create the main results table
	.bainANOVATable(dataset, options, jaspResults)
	# Save analysis result in state
	bainResult 									<- jaspResults[["bainResult"]]$object
	# Bayes factor matrix
	if (options$BFmatrix)
	{
		if(is.null(jaspResults[["Bainmatrix"]]))
			.BainBFmatrix(dataset, options, jaspResults, bainResult, type = "anova")        
	}
	# Descriptives
	if (options$descriptives)
	{
		if(is.null(jaspResults[["coefficients"]]))
			.bainANOVADescriptivesTable(dataset, options, jaspResults)        
	}
	# Bayes factor plot
	if(options$BFplot)
    {
        if(is.null(jaspResults[["BFplot"]]))
        {
        	jaspResults[["BFplot"]] 			<- .bainANCOVAPlot(dataset, options, bainResult, "Bayes factor Plot")
        	jaspResults[["BFplot"]]				$dependOnOptions(c("dependent", "fixedFactors", "BFplot", "model"))
		}
	}
	# Descriptives plot
	if(options$plotDescriptives)
	{
		if(is.null(jaspResults[["descriptivesPlot"]])){
			jaspResults[["descriptivesPlot"]] 	<- createJaspPlot(plot= .bainDescriptivesPlot(dataset, bainResult, options, type = "anova"), 
												title="Descriptives plot", width = options$plotWidth, height = options$plotHeight) 
			jaspResults[["descriptivesPlot"]]	$dependOnOptions(c("dependent", "fixedFactors", "plotDescriptives"))
		}
	}	
	# Save the state	
	state[["options"]] <- options
	return(state)	
}

.bainANOVATable <- function(dataset, options, jaspResults){
	
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
	bainTable                      	<- createJaspTable("ANOVA result")
	jaspResults[["bainTable"]]     	<- bainTable
		
	bainTable$dependOnOptions(c("dependent", "fixedFactors", "logScale", "model"))
	
	bainTable$addColumnInfo(name="hypotheses", 				type="string", title="")
	bainTable$addColumnInfo(name="BF", 						type="number", format="sf:4;dp:3", title=bf.title)
	bainTable$addColumnInfo(name="PMP1", 					type="number", format="sf:4;dp:3", title="PMP a")
	bainTable$addColumnInfo(name="PMP2", 					type="number", format="sf:4;dp:3", title="PMP b")
	
	message <- "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement. 
				PMP a indicates posterior probability for each hypothesis excluding unconstrained hypothesis. 
				PMP b indicates posterior probability for each hypothesis including unconstrained hypothesis."
	bainTable$addFootnote(message=message, symbol="<i>Note.</i>")
	
	bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110")

	bain.variables <- c(unlist(options$dependent),
						unlist(options$fixedFactors))
	bain.variables <- bain.variables[bain.variables != ""]
	
	if(length(bain.variables) > 1){	
					
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
			
			p <- try(silent= FALSE, expr= {
				
				bainResult <- Bain::Bain_anova(X = dataset, dep_var = dependent, group = group, ERr, IRr)
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("dependent", "fixedFactors", "logScale", "model"))
				
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
			inpt[[3]] <- .unv(group)
			inpt[[4]] <- rest.string
			
			p <- try(silent= FALSE, expr= {
				
				bainResult <- Bain::Bain_anova_cm(X = inpt[[1]], dep_var = inpt[[2]], group = inpt[[3]], hyp = inpt[[4]])
				jaspResults[["bainResult"]] <- createJaspState(bainResult)
				jaspResults[["bainResult"]]$dependOnOptions(c("dependent", "fixedFactors", "logScale", "model"))
				
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

.bainANOVADescriptivesTable <- function(dataset, options, jaspResults){
	
	if(!is.null(jaspResults[["descriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
	descriptives                      	<- createJaspTable("Descriptives")
	jaspResults[["descriptives"]]     	<- descriptives
	
	descriptives$dependOnOptions(c("dependent", "fixedFactors", "descriptives", "CredibleInterval"))

	descriptives$addColumnInfo(name="v",    		title="Level",   type="string")
	descriptives$addColumnInfo(name="N",    		title="N",  type="integer")
	descriptives$addColumnInfo(name="mean", 		title="Mean", type="number", format="sf:4;dp:3")
	descriptives$addColumnInfo(name="sd",   		title="sd", type="number",   format="sf:4;dp:3")
	descriptives$addColumnInfo(name="se",   		title="se", type="number",   format="sf:4;dp:3")
		
	interval <- options$CredibleInterval
	overTitle <- paste0(interval, "% Credible Interval")
	descriptives$addColumnInfo(name="lowerCI",      title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
    descriptives$addColumnInfo(name="upperCI",      title = "upperCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
	
	variables <- c(options$dependent, options$fixedFactors)
	variables <- variables[variables != ""]
	
	if(length(variables) > 1){
			
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			for(variable in varLevels){
				
					column <- dataset[ , .v(options$dependent)]
					column <- column[which(groupCol == variable)]
					
					posteriorSummary <- .posteriorSummaryGroupMean(variable=column, descriptivesPlotsCredibleInterval=options$CredibleInterval/100)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)
					
					row <- list(v = variable, N = .clean(length(column)), mean = .clean(mean(column)), sd = .clean(round(sd(column),3)), 
													se = .clean(sd(column)/sqrt(length(column))), lowerCI = ciLower, upperCI = ciUpper)	
					descriptives$addRows(row)
			}
			
		} else {
			
			row <- list(v = ".", N = ".", mean = ".", sd= ".", se = ".", lowerCI = ".", upperCI = ".")
			descriptives$addRows(row)	
			
		}
}

.bainDescriptivesPlot <- function(dataset, bainResult, options, type = NULL) {

		base_breaks_y <- function(x, plotErrorBars = TRUE){
				ci.pos <- c(x[,"dependent"], x[,"ciLower"], x[,"ciUpper"])
				b <- pretty(ci.pos)
				d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
				list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
					 ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
				 }
				 
	if(!is.null(bainResult)){
		
		groupVars <- options$fixedFactors
		groupVars <- unlist(groupVars)

		groupVarsV <- .v(groupVars)
		dependentV <- .v(options$dependent)
			
		sum_model <- bainResult$estimate_res
		summaryStat <- summary(sum_model)$coefficients
			
		if(type == "ancova"){			
			summaryStat <- summaryStat[-(nrow(summaryStat) - 0:(length(options$covariates)-1)), ] # Remove covars rows		
		}
		
		summaryStat <- cbind(summaryStat, 1:nrow(summaryStat))
		colnames(summaryStat)[length(colnames(summaryStat))] <- "plotHorizontalAxis"
		colnames(summaryStat)[which(colnames(summaryStat) == "Estimate")] <- "dependent"

			summaryStatSubset <- as.data.frame(summaryStat)
			#########################3
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			ciLower <- summaryStatSubset[, 1] - 1.96*summaryStatSubset[, 2]
			ciUpper <- summaryStatSubset[, 1] + 1.96*summaryStatSubset[, 2]
			
			summaryStatSubset$ciLower <- ciLower
			summaryStatSubset$ciUpper <- ciUpper
			
			summaryStat <- summaryStatSubset

			p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
										y=dependent,
										group=1))


			pd <- ggplot2::position_dodge(.2)
			p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower,
														ymax=ciUpper),
														colour="black", width=.2, position=pd)


		p <- p + ggplot2::geom_line(position=pd, size = .7) +
			ggplot2::geom_point(position=pd, size=4) +
			ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_color_manual(values = rep("black",200),guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::ylab(options$dependent) +
			ggplot2::xlab(groupVars) +
			base_breaks_y(summaryStat, TRUE)
			
			p <- JASPgraphs::themeJasp(p)

			return(p)
		} 
}

.readDataBainAnova <- function(options, dataset){
	
	numeric.variables 							<- c(unlist(options$dependent))
	numeric.variables 							<- numeric.variables[numeric.variables != ""]
	factor.variables 							<- unlist(options$fixedFactors)
	factor.variables 							<- factor.variables[factor.variables != ""]
	all.variables 								<- c(numeric.variables, factor.variables)
	
	if (is.null(dataset)) {		
		dataset 								<- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)	
	} else {
		dataset 								<- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}
	
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, message="short", observations.amount="< 3", 
				exitAnalysisIfErrors = TRUE)
	
	return(dataset)
	
}
