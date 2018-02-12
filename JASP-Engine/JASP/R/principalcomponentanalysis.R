#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <http://www.gnu.org/licenses/>.
#


PrincipalComponentAnalysis <- function(dataset = NULL, options, perform = "run",
																			callback = function(...) list(status="ok"), state = NULL, ...) { 
	return(mainFunctionPCAEFA(type = "pca", dataset = NULL, options, perform = "run",
																			callback = function(...) list(status="ok"), state = NULL, ...))
}

mainFunctionPCAEFA <- function(type, dataset = NULL, options, perform = "run",
																			callback = function(...) list(status="ok"), state = NULL, ...) {
	## call the common initialization function
	init <- .initializePCAEFA(dataset, options, perform)

	results <- init[["results"]]
	dataset <- init[["dataset"]]

	## initialize result ## ----
	results[["title"]] = ifelse(type =="pca","Principal Component Analysis","Exploratory Factor Analysis")
	meta <- list(
		list(name="title", type="title"),
		list(name="factorLoadings", type = "table"),
		list(name="factorCorrelations", type = "table"),
		list(name="goodnessOfFit", type="table"),
		list(name="fitMeasures", type="table"),
		list(name="pathDiagram", type="image"),
		list(name="screePlot", type="image")
	)
	results[[".meta"]] = meta

	## initialize state ## ----
	stateKey <- list(
		analysisResults = c(
		'rotationMethod',
		'orthogonalSelector',
		'obliqueSelector',
		'variables', 
		'factorMethod',
		'eigenValuesBox',
		'numberOfFactors'),
		pathDiagram = c(
		'rotationMethod',
		'orthogonalSelector',
		'obliqueSelector',
		'variables', 
		'factorMethod',
		'eigenValuesBox',
		'numberOfFactors',
		'highlightSlider', 
		'highlightText',
		'plotWidthPathDiagram',
		'plotHeightPathDiagram'),
		screePlot = c(
		'rotationMethod',
		'orthogonalSelector',
		'obliqueSelector',
		'variables', 
		'factorMethod',
		'eigenValuesBox',
		'numberOfFactors',
		'plotWidthScreePlot',
		'plotHeightScreePlot')
	)

	keep <- NULL

	if (is.null(state[["analysisResults"]])) {
		state <- NULL
		analysisResults <- NULL

		if (perform == "run" && !is.null(dataset) && nrow(dataset) > 1) {
			customChecksPCAEFA <- list(
				function(){
					if (length(options$variables) > 0 && options$numberOfFactors > length(options$variables)) {
						return(paste0("Too many factors requested (", options$numberOfFactors, ") for the amount of included variables"))
					}
				},
				function(){
					if(nrow(dataset) < 3){
						return(paste0("Not enough valid cases (", nrow(dataset),") to run this analysis"))
					}
				},
				# check whether all row variance == 0
				function(){
					varianceZero <- 0
					for (i in 1:nrow(dataset)){
						if(sd(dataset[i,], na.rm = TRUE) == 0) varianceZero <- varianceZero + 1
					}
					if(varianceZero == nrow(dataset)){
						return("Data not valid: variance is zero in each row")
					}
				},
				# check whether all variables correlates to each other
				function(){
					allCorr <- 0
					nVar <- ncol(dataset)
					for (i in 1:(nVar-1)) {
						for (j in (i+1):nVar) {
							thisCor <- cor(dataset[,i],dataset[,j])
							if(!is.na(thisCor) && thisCor == 1) allCorr <- allCorr + 1
						}
					}
					if(allCorr == nVar*(nVar-1)/2){
						return("Data not valid: all variables correlate with each other")
					}
				}
			)
			error <- .hasErrors(dataset=dataset, perform=perform, type=c("infinity", "variance"), custom=customChecksPCAEFA, exitAnalysisIfErrors=TRUE)
			analysisResults <- try(silent = FALSE, expr = {
				.estimatePCAEFA(dataset, options, perform, type)
			})
		}

	} else {

		analysisResults <- state[["analysisResults"]]

	}
	# Output table
	# Create fit measures tables:
	results[["goodnessOfFit"]] <- .goodnessOfFit(analysisResults, options, perform)
	# Create factor correlation table:
	results[["factorLoadings"]] <- .getLoadings(analysisResults, dataset, options, perform, "pca")

	if (options$incl_correlations) {
		results[["factorCorrelations"]] <- .getFactorCorrelations(analysisResults, options, perform, "pca")
	}

	if (type == "efa" && options$incl_fitIndices) {
		results[["fitMeasures"]] <- .fitMeasures(analysisResults, options, perform)
	}

	# Output Plot
	# Create path diagram:
	if (isTRUE(options$incl_pathDiagram)) {
		p <- try(silent = FALSE, expr = {
			.pathDiagramPCAEFA(analysisResults, options, perform, type, oldPlot = state[["pathDiagram"]])
		})

		if (isTryError(p)) {
			errorMessage <- .extractErrorMessage(p)
			results[["pathDiagram"]][["error"]] <- list(error="badData", errorMessage=errorMessage)
		} else {
			results[["pathDiagram"]] <- p
			keep <- c(keep, results[["pathDiagram"]][["data"]])
		}
	}

	# Scree plot:
	if (isTRUE(options$incl_screePlot)) {
		p <- try(silent = FALSE, expr = {
			.screePlot(dataset, options, perform, oldPlot = state[["screePlot"]],"pca")
		})

		if (isTryError(p)) {
			errorMessage <- .extractErrorMessage(p)
			results[["screePlot"]][["error"]] <- list(error="badData", errorMessage=errorMessage)
		} else {
			results[["screePlot"]] <- p
			keep <- c(keep, results[["screePlot"]][["data"]])
		}
	}

	#save state
	state <- list(
		options = options,
		analysisResults = analysisResults,
		pathDiagram = results[["pathDiagram"]],
		screePlot = results[["screePlot"]]
	)

	attr(state, "key") <- stateKey


	if (perform == "run") {

		return(list(results = results, status = "complete", state = state, keep = keep))

	} else {

		return(list(results = results, status = "inited", state = state, keep = keep))

	}
}

.estimateNFactorPCAEFA <- function(dataset, options, perform, type){
	# Number of factors:
	nVariable <- length(options$variables)

	# get nFactor
	nFactor <- 1
	message <- NULL

	if(perform == "run" && !is.null(dataset) && nrow(dataset)>1 && nVariable > 0) {

		if (options$factorMethod == "parallelAnalysis") {

			parallelAnalysis <- try(silent = FALSE, expr = {
				pa <- .suppressPlotFA(dataset)
			})

		if (isTryError(parallelAnalysis)) {
			nFactor <- 1
		} else {
			if (type == "pca") {
				if (is.na(pa$ncomp)) pa$ncomp <- 1
				nFactor <- max(1,pa$ncomp)
			} else {
				if (is.na(pa$nfact)) pa$nfact <- 1
				nFactor <- max(1,pa$nfact)
			}
		}

		} else if (options$factorMethod == "eigenValues") {
			# Compute ev:
			eigenValues <- try(silent = FALSE, expr = {
				pa <- .suppressPlotFA(dataset)
			})
			if (isTryError(eigenValues)) {
				nFactor <- 1
			} else {
				if (type == "pca") {
					nFactor <- sum(pa$pc.values > options$eigenValuesBox)
				} else {
					nFactor <- sum(pa$fa.values > options$eigenValuesBox)
				}
			}
		} else if (options$factorMethod == "manual"){
			nFactor <- options$numberOfFactors
		}
	}

	return(list(nFactor = nFactor, message = NULL))

}


### Inner functions ###
# Estimate PCA:
.estimatePCAEFA <- function(dataset, options, perform, type) {

	if (options$rotationMethod == "orthogonal") {
		Rotation <- options$orthogonalSelector
	} else {
		Rotation <- options$obliqueSelector
	}

	res <- .estimateNFactorPCAEFA(dataset, options, perform, type)
	nFactor <- res$nFactor
	message <- res$message

	if (type == "pca") {
		Results <- psych::principal(dataset, nFactor, rotate = Rotation)
	} else {
		Results <- psych::fa(dataset, nFactor, rotate = Rotation)
		}

	return(list(Results = Results, nFactor = nFactor, message = message))

}

# Get loadings matrix:
.getLoadings <- function(analysisResults, dataset, options, perform, type){

	# Create JASP table:
	Loadings <- list()
	Loadings[["title"]] <- "Component Loadings"
 	Loadings[["schema"]] <- list(fields = list())

	if (options$rotationMethod == "orthogonal") {
		Rotation <- options$orthogonalSelector
	} else {
		Rotation <- options$obliqueSelector
	}

	if (type == "pca") {
		colName = ifelse(Rotation=="none","PC","RC")
	}

	if (type == "efa") {
		colName = "Factor"
	}
	
	footnotes <- .newFootnotes()
	# Extract loadings:
	if (is.null(analysisResults) || isTryError(analysisResults)) {
		if (is.null(options$numberOfFactors)){
			nFactor <- 0
		} else {
			nFactor <- options$numberOfFactors
		}

		loadingsMatrix <- matrix(NA,length(options$variables),nFactor+1)

		colnames(loadingsMatrix) <- c( paste(colName,seq_len(nFactor)),"Uniqueness")
		rownames(loadingsMatrix) <- colnames(dataset)

	} else {

		message <- analysisResults$message
		.addFootnote(footnotes, symbol = "", text = message)

		analysisResults <- analysisResults$Results
		loadingsMatrix <- as.matrix(loadings(analysisResults))

		if (ncol(loadingsMatrix) > 0) {
			colnames(loadingsMatrix) <- paste(colName,seq_len(ncol(loadingsMatrix)))
		}
		
		# Add uniqueness:
		loadingsMatrix <- cbind(loadingsMatrix, Uniqueness = unname(analysisResults$uniquenesses))
	}

	# Add columns:
	Loadings[["schema"]][["fields"]][[1]] <- list(name = "VAR", title = "", type="string")
	
	for (j in seq_len(ncol(loadingsMatrix))) {
		Loadings[["schema"]][["fields"]][[j+1]] <- list(name = colnames(loadingsMatrix)[j], title = colnames(loadingsMatrix)[j], type="number", format = "dp:3")
	}

	Loadings[["data"]] <- list()

	# Add rows:
	if (nrow(loadingsMatrix)==0) {
		Loadings[["data"]][[1]] <- as.list(rep(".",ncol(loadingsMatrix)+1))
	}
	
	for (i in seq_len(nrow(loadingsMatrix))) {

		dat <- loadingsMatrix[i,]

		Loadings[["data"]][[i]] <- list(VAR = .unv(rownames(loadingsMatrix)[i]))

		for (j in seq_along(dat)) {
			if (is.na(dat[j])) {
				Loadings[["data"]][[i]][[j+1]] <- "."
			} else {
				Loadings[["data"]][[i]][[j+1]] <- unname(ifelse(abs(dat[j]) < options$highlightText & j != length(dat),".",dat[j]))
			}
		}
		names( Loadings[["data"]][[i]]) <- sapply(Loadings[["schema"]][["fields"]],"[[",'name')
	}
	return(Loadings)
}

# Init:
.initializePCAEFA <- function(dataset, options, perform) {
	
	groups <- options$groupingVariable
	depvars <- unlist(options$variables)
	depvars <- depvars[depvars != ""]
	
	
	if (!is.null(groups) && groups == "") groups <- NULL

	if (is.null(dataset)) {
	## if we are ready to run, read in the dataset
		if (perform == "run" && length(options$variables) > 1) {
			
			exclude <- c()
			if (options[["missingValues"]] == "listwise")
				exclude <- c(depvars, groups)

			dataset <- .readDataSetToEnd(columns.as.numeric = depvars, columns.as.factor = groups, exclude.na.listwise = exclude)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric = depvars, columns.as.factor = groups)
		}
	}
	
	## this is the main object; we add stuff to it and return it
	results <- list(
		"citation" = list("Revelle, W. (2015) psych: Procedures for Personality and Psychological Research, Northwestern University,
		Evanston, Illinois, USA, http://CRAN.R-project.org/package=psych Version = 1.5.8.")
	)

	list("results" = results, "dataset" = dataset)
}


# Path diagram:
.pathDiagramPCAEFA <- function(analysisResults, options, perform, type, oldPlot=NULL){

	if (!is.null(oldPlot) && !identical(oldPlot[["data"]], "")) {
		return(oldPlot)
	}

	pathDiagram <- list()
	pathDiagram$title <- "Path Diagram"

	if (options$rotationMethod == "orthogonal") {
		Rotation <- options$orthogonalSelector
	} else {
		Rotation <- options$obliqueSelector
	}

	xName <- ifelse(Rotation == "none", "PC", "RC")

	pathDiagram$width <- options$plotWidthPathDiagram
	pathDiagram$height <- options$plotHeightPathDiagram
	if (pathDiagram$height==0) {
		pathDiagram$height <- 1 + 299 * (length(options$variables)/5)
	}
	pathDiagram$custom <- list(width="plotWidthPathDiagram", height="plotHeightPathDiagram")
	
	if (is.null(analysisResults) || isTryError(analysisResults)) { 

		pathDiagram$data <- NULL

	} else {

		analysisResults <- analysisResults$Results

		LY <- as.matrix(loadings(analysisResults))
		TE <- diag(analysisResults$uniqueness)
		PS <- analysisResults$r.scores

		# Variable names:
		labels <- .unv(rownames(LY))
		if (type == "pca") {
			factors <- paste0(xName,seq_len(ncol(LY)))
		} else {
			factors <- paste0("F",seq_len(ncol(LY)))
		}

		# Number of variables:
		nFactor <- length(factors)
		nIndicator <- length(labels)
		nTotal <- nFactor + nIndicator

		# Make layout:
		# For each manifest, find strongest loading:
		strongest <- apply(abs(LY),1,which.max)
		ord <- order(strongest)

		# Reshuffle labels and LY:
		labels <- labels[ord]
		LY <- LY[ord,]

		# Edgelist:
		# Factor loadings
		if (type == "pca") {
			E_loadings <- data.frame(from = rep(labels,times=nFactor), to = rep(factors,each=nIndicator) ,weight=c(LY),
														 stringsAsFactors = FALSE)
		} else {
			E_loadings <- data.frame(from = rep(factors,each=nIndicator), to = rep(labels,times=nFactor),weight=c(LY),
														 stringsAsFactors = FALSE)
		}

		# Residuals:
		E_resid <- data.frame(from=labels,to=labels,weight=diag(TE))
		E_resid$weight <- 0

		# Factor correlations:
		E_cor <- data.frame(from = c(factors[col(PS)]), to =	c(factors[row(PS)]),weight=c(PS),
												stringsAsFactors = FALSE)
		E_cor <- E_cor[E_cor$from != E_cor$to,]

		# Total:
		E <- rbind(E_loadings,E_resid,E_cor)

		# Make the layout:
		sq <- function(x){
			seq(-1,1,length.out = x+2)[-c(1,x+2)]
		}

		L <- cbind(
		c(rep(-1,nFactor),rep(1,nIndicator)),
		c(sq(nFactor),sq(nIndicator))
		)

		# Compute curvature of correlations:
		# Numeric edgelist:
		E_cor_numeric <- cbind(match(E_cor$from,factors),match(E_cor$to,factors))

		# Compute distance:
		dist <- abs(L[E_cor_numeric[,1],2] - L[E_cor_numeric[,2],2])

		min <- 2
		max <- 8
		# Scale to max:
		dist <- min + dist/(max(dist))*(max - min)
		if (length(unique(dist))==1){
			dist[] <- mean(c(max,min))
		}

		# Scale to plot width:
		Scale <- sqrt(pathDiagram$width^2 + pathDiagram$height^2)/sqrt(480^2 + 300^2)
		dist <- 1/Scale * dist

		# Curvature:
		curve <- c(rep(0,nrow(E_loadings)),
							 rep(0,nrow(E_resid)),
							 dist)

		# Edge connectpoints:
		ECP <- matrix(NA,nrow(E),2)
		ECP[nrow(E_loadings) + nrow(E_resid) + seq_len(nrow(E_cor)),1:2] <- 1.5*pi
		if (type == "pca") {
			ECP[seq_len(nrow(E_loadings)),1] <- 1.5*pi
		} else {
			ECP[seq_len(nrow(E_loadings)),2] <- 1.5*pi
		}

		# Loop rotation:
		loopRotation <- 0.5*pi

		# bidirectional:
		bidir <- c(rep(FALSE,nrow(E_loadings) + nrow(E_resid)), rep(TRUE, nrow(E_cor)))

		# Shape:
		shape <- c(rep("circle",nFactor), rep("rectangle",nIndicator))

		# Size:
		size1 <- c(
			rep(12,nFactor),
			rep(30,nIndicator)
		)
		size2 <- c(
			rep(12,nFactor),
			rep( 7,nIndicator)
		)

		# Plot:
		label.scale.equal <- c(rep(1,nFactor),rep(2,nIndicator))

		# Run once without plotting to obtain the scaled label sizes:
		.plotFunc <- function(){
			qgraph::qgraph(E, layout = L, directed=TRUE, bidirectional=bidir, residuals = TRUE, residScale	= 10,
										 labels = c(factors,labels), curve = curve, curveScale = FALSE, edgeConnectPoints = ECP,
										 loopRotation=loopRotation, shape = shape, vsize = size1, vsize2 = size2,label.scale.equal=label.scale.equal,
										 residScale = 2, mar = c(5,10,5,12), normalize = FALSE, label.fill.vertical = 0.75, cut = options$highlightText,
										 bg = "transparent"
			)
		}

		content <- .writeImage(width = pathDiagram$width, 
			height = pathDiagram$height, plot = .plotFunc, obj = TRUE)
		pathDiagram[["convertible"]] <- TRUE
		pathDiagram[["obj"]] <- content[["obj"]]
		pathDiagram[["data"]] <- content[["png"]]
		pathDiagram[["status"]] <- "complete"

	}

	return(pathDiagram)

}


# Factor correlations:
.getFactorCorrelations <- function(analysisResults, options, perform, type){

	# Create JASP table:
	FactorCorrelations <- list()
	FactorCorrelations[["title"]] <- ifelse(type == "pca","Component Correlations", "Factor Correlations")

	# Extract loadings	
	if (is.null(analysisResults) || isTryError(analysisResults)) {#|| perform == "init"
		if (is.null(options$numberOfFactors)) {
			nFact <- 0
		} else {
			nFact <- options$numberOfFactors
		}

		corMatrix <- matrix(,0,0)

	} else {

		analysisResults <- analysisResults$Results
		corMatrix <- as.matrix(analysisResults$r.scores)

	}

	if (options$rotationMethod == "orthogonal") {
		Rotation <- options$orthogonalSelector
	} else {
		Rotation <- options$obliqueSelector
	}

	xName = ifelse(type == "pca",ifelse(Rotation == "none","PC","RC"), "Factor")

	if (ncol(corMatrix) > 0) {
		colnames(corMatrix) <- rownames(corMatrix) <- paste(xName,seq_len(ncol(corMatrix)))
	}

	
	# Add columns:
	FactorCorrelations[["schema"]] <- list(fields = list())
	FactorCorrelations[["schema"]][["fields"]][[1]] <- list(name = "VAR", title = "", type="string")

	for (j in seq_len(ncol(corMatrix))) {
		FactorCorrelations[["schema"]][["fields"]][[j+1]] <- list(name = colnames(corMatrix)[j], title = colnames(corMatrix)[j], type="number", format = "dp:3")
	}

	FactorCorrelations[["data"]] <- list()

	# Add rows:
	for (i in seq_len(nrow(corMatrix))) {

		dat <- corMatrix[i,]
		FactorCorrelations[["data"]][[i]] <- list(VAR = rownames(corMatrix)[i])
		for (j in 1:i) {
			FactorCorrelations[["data"]][[i]][[j+1]] <- unname(dat[j])
		}

		names(FactorCorrelations[["data"]][[i]]) <- sapply(FactorCorrelations[["schema"]][["fields"]],"[[",'name')[seq_along(FactorCorrelations[["data"]][[i]])]
	}
	
	return(FactorCorrelations)
}


# for number, this function switch NA, Inf and null to "." in tables
.DotIfNULLPCAEFA <- function(x){

	if (is.null(x) || is.na(x) || !is.finite(x)) {
		return(".")
	} else {
		return(x)
	}
}


# Goodness of fit
.goodnessOfFit <- function(analysisResults, options, perform){

	# Create JASP table:
	goodnessOfFit <- list()
	goodnessOfFit[["title"]] <- "Chi-squared Test"

	footnotes <- .newFootnotes()

	# Create the columns:
	goodnessOfFit[["schema"]] <- list(fields = list(
		list(name = "model", title="", type = "string"),
		list(name = "chisq", title = "Value", type="number", format = "dp:3"),
		list(name = "df", title = "df", type="integer"),
		list(name = "p", title = "p", type="number", format = "dp:3;p:.001")	))

	# Extract loadings:
	if(is.null(analysisResults) || isTryError(analysisResults)){
		Fits <- list(
			CHI = ".",
			PVAL = ".",
			DF = "."
		)

	}else{
		analysisResults <- analysisResults$Results

		Fits <- list(
			CHI = .DotIfNULLPCAEFA(analysisResults$STATISTIC),
			PVAL = .DotIfNULLPCAEFA(analysisResults$PVAL),
			DF = .DotIfNULLPCAEFA(analysisResults$dof)
		)
	}

	# Create and fill the row(s):
	goodnessOfFit[["data"]] <- list(
		list(
			model = "Model", 
			chisq = ifelse(Fits$DF>0,Fits$CHI,"."),
			df = ifelse(Fits$DF>0,Fits$DF,"."),
			p = ifelse(Fits$DF>0,Fits$PVAL,".")
		)
	)

	return(goodnessOfFit)

}


# fitMeasures
.fitMeasures <- function(analysisResults, options, perform){
	
	# Create JASP table:
	FitMeasures <- list()
	FitMeasures[["title"]] <- "Additional fit indices"

	# Create the columns:
	FitMeasures[["schema"]] <- list(fields = list(
		list(name = "model", title="", type = "string"),
		list(name = "RMSEA", title = "RMSEA", type="number", format = "dp:3"),
		list(name = "RMSEAci", title = "RMSEA 90% confidence", type="string"),
		list(name = "TLI", title = "TLI", type="number", format = "dp:3"),
		list(name = "BIC", title = "BIC", type="number", format = "dp:3")
	))

	# Extract loadings:
	if(is.null(analysisResults) || isTryError(analysisResults)){
		Fits <- list(
			CHI = ".",
			PVAL = ".",
			DF = ".",
			RMSEA = ".",
			RMSEAlower = ".",			
			RMSEAupper = ".",
			TLI = ".",
			RMS = ".",
			CRMS = ".",
			BIC = "."
		)

	}else{
		analysisResults <- analysisResults$Results
		Fits <- list(
			CHI = .DotIfNULLPCAEFA(analysisResults$STATISTIC),
			PVAL = .DotIfNULLPCAEFA(analysisResults$PVAL),
			DF = .DotIfNULLPCAEFA(analysisResults$dof),
			RMSEA = .DotIfNULLPCAEFA(unname(analysisResults$RMSEA['RMSEA'])),
			RMSEAlower = .DotIfNULLPCAEFA(unname(analysisResults$RMSEA['lower'])),
			RMSEAupper = .DotIfNULLPCAEFA(unname(analysisResults$RMSEA['upper'])),			
			TLI = .DotIfNULLPCAEFA(analysisResults$TLI),
			RMS = .DotIfNULLPCAEFA(analysisResults$rms),
			CRMS = .DotIfNULLPCAEFA(analysisResults$crms),
			BIC = .DotIfNULLPCAEFA(analysisResults$BIC)
		)
	}

	# Create and fill the row(s):
	if (is.numeric(Fits$RMSEAlower)) {
		Fits$RMSEAlower <- round(Fits$RMSEAlower,3)
	}
	
	if (is.numeric(Fits$RMSEAupper)) {
		Fits$RMSEAupper <- round(Fits$RMSEAupper,3)
	}
	
	FitMeasures[["data"]] <- list(
		list(
			model = "Model", 
			RMSEA = Fits$RMSEA,
			RMSEAci = paste(Fits$RMSEAlower,"-",Fits$RMSEAupper),
			TLI = Fits$TLI,
			BIC = Fits$BIC
		)
	)

	return(FitMeasures)

}


### Screeplot:

.screePlot <- function(dataset, options, perform, oldPlot = NULL, plotType) {

	# After image can be wrote to file, the state system can be used to return unchanged screePlot
	if (!is.null(oldPlot) && !identical(oldPlot[["data"]], "") && !is.null(oldPlot[["data"]])) {
		return(oldPlot)
	}
	
	screePlot <- list()
	screePlot$title <- "Scree Plot"
	screePlot$width <- options$plotWidthScreePlot
	screePlot$height <- options$plotHeightScreePlot
	screePlot$custom <- list(width="plotWidthScreePlot", height="plotHeightScreePlot")

	if (!is.null(dataset) && nrow(dataset)> 1 && length(options$variables) > 1) { 

		# Compute ev:
		pa <- .suppressPlotFA(dataset)

		if (plotType == "pca") {
			ev_ev <- pa$pc.values
			pa_ev <- pa$pc.sim
			xName = "Components"
		}

		if (plotType == "efa") {
			ev_ev <- pa$fa.values
			pa_ev <- pa$fa.sim
			xName = "Factors"
		}
		# Eigenvalues:
		EV <- data.frame(
			id = seq_len(ncol(dataset)),
			ev = ev_ev,
			type = "Data"
		)

		# Parallel analysis:
		PA <- data.frame(
			id = seq_len(ncol(dataset)),
			ev = pa_ev,
			type = "Simulated (95th quantile)"
		)

		combined <- rbind(EV,PA)

	p <- ggplot2::ggplot(combined, ggplot2::aes_string(x="id",y="ev",lty="type",pch="type")) + ggplot2::geom_point(na.rm = TRUE, size=3) +
		ggplot2::xlab("") + ggplot2::ylab("Eigenvalue")+ ggplot2::xlab(xName) +ggplot2::geom_line(na.rm = TRUE) +
		ggplot2::ggtitle("") + ggplot2::theme_bw() + ggplot2::geom_hline(yintercept = options$eigenValuesBox) +
		ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
			panel.grid.major=ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black", size=1.2),
			axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=1.2),
			axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
			panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
			plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
			panel.border = ggplot2::element_blank(),
			axis.ticks = ggplot2::element_line(size = 0.5),
			axis.ticks.margin = grid::unit(1,"mm"),
			axis.ticks.length = grid::unit(3, "mm"),
			plot.margin = grid::unit(c(0,0,.5,.5), "cm")) + 
		ggplot2::scale_linetype_discrete("") +	ggplot2::scale_shape_discrete("") +
		ggplot2::theme(legend.position = c(0.99,0.99),legend.justification = c(1,1),#legend.key = ggplot2::element_blank(),
			legend.text=ggplot2::element_text(size=12.5),
			panel.background=ggplot2::element_rect(fill="transparent",colour=NA),
			plot.background=ggplot2::element_rect(fill="transparent",colour=NA),
			legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
			legend.background=ggplot2::element_rect(fill="transparent",colour=NA))

		content <- .writeImage(width = options$plotWidthScreePlot, height = options$plotHeightScreePlot, plot = p, obj = TRUE)
		screePlot[["convertible"]] <- TRUE
		screePlot[["obj"]] <- content[["obj"]]
		screePlot[["data"]] <- content[["png"]]
		screePlot[["status"]] <- "complete"

	} else {

		screePlot$data <- NULL

	}

	return(screePlot)
}

.suppressPlotFA <- function(...) {
	tempfile <- tempfile()
	png(filename=tempfile)
	result <- psych::fa.parallel(...)
	dev.off()
	unlink(tempfile)
	return(result)
}
