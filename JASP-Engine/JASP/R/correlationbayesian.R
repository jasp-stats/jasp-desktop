# "variables": data.frame thingie vfc
# "pearson": TRUE/FALSE
# "kendallsTauB": TRUE/FALSE
# "spearman": TRUE/FALSE
# "hypothesis": "correlated", "correlatedPositively", "correlatedNegatively"
# "reportBayesFactors": TRUE/FALSE
# "flagSupported": TRUE/FALSE
# "credibleIntervals": TRUE/FALSE
# "credibleIntervalsInterval": OptionNumber(.95, 0, 1, "%"));
# "priorWidth": c(0.5, Inf)
# "bayesFactorType": BF10/BF01
CorrelationBayesian <- function(dataset=NULL, options, perform="run",
								callback=function(...) 0, ...) {
	# dataset is data.frame
	# options is a list
	#
	if (is.null(dataset)) {
		if (perform == "run") {
			if (options$missingValues == "excludeListwise") {
				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, exclude.na.listwise=options$variables)
			} else {
				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables)
			}
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=options$variables)
		}
	}
	results <- list()
	meta <- list()
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="correlations", type="table")	
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Matrix"
	results[["correlations"]] <-
		.correlationTableBayesian(dataset, perform, variables=options$variables,
								  pearson=options$pearson,
								  kendallsTauB=options$kendallsTauB,
								  spearman=options$spearman,
								  hypothesis=options$hypothesis,
								  reportBayesFactors=options$reportBayesFactors,
								  flagSupported=options$flagSupported,
								  credibleIntervals=options$credibleIntervals,
								  credibleIntervalsInterval=options$credibleIntervalsInterval,
								  priorWidth=options$priorWidth,
								  bayesFactorType=options$bayesFactorType,
								  missingValues=options$missingValues)
	results[["plots"]] <- .correlationMatrixPlotBayesian(dataset, perform, options, hypothesis=options$hypothesis)
	
	if (perform == "init") {
		if (length(options$variables) < 2) {
			results <- list(results=results, status="complete")
		} else {
			results <- list(results=results, status="inited")
		}
	} else {
		results <- list(results=results, status="complete")
	}
	return(results)
}
# "variables": data.frame thingie vfc
# "pearson": TRUE/FALSE
# "kendallsTauB": TRUE/FALSE
# "spearman": TRUE/FALSE
# "hypothesis": "correlated", "correlatedPositively", "correlatedNegatively"
# "reportBayesFactors": TRUE/FALSE
# "flagSupported": TRUE/FALSE
# "credibleIntervals": TRUE/FALSE
# "credibleIntervalsInterval": OptionNumber(.95, 0, 1, "%"));
# "priorWidth": c(0.5, Inf)
# "bayesFactorType": BF10/BF01
.correlationTableBayesian <- function(dataset, perform, variables, pearson=TRUE,
									  kendallsTauB=FALSE, spearman=FALSE,
									  hypothesis="correlated",
									  reportBayesFactors=TRUE,
									  flagSupported=FALSE,
									  credibleIntervals=FALSE,
									  credibleIntervalsInterval=0.95,
									  priorWidth=priorWidth,
									  bayesFactorType=bayesFactorType,
									  missingValues="excludePairwise") {
	# TODO: check for all arguments in particular meansAndStdDev,
	# hypothesis="correlated"
	#
	#
	correlation.table <- list()
	correlation.table[["citation"]] <- list(
		"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
	)
	if (perform == "init") {
		if (length(variables) < 2)
			variables <- c(variables, "...")
		if (length(variables) < 2)
			variables <- c(variables, "... ")
	}
	if (hypothesis == "correlated") {
		if (bayesFactorType=="BF10"){
			bf.title <- "BF\u2081\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u2081"
		}
	} else if (hypothesis == "correlatedPositively") {
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208A\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208A"
		}
	} else if (hypothesis == "correlatedNegatively"){
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208B\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208B"
		}
	}
	# Note: test contains the tests that are performed
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")
	# Note: Naming of the table
	if (length(tests) != 1) {
		correlation.table[["title"]] <- paste("Bayesian Correlation Table")
	} else if (pearson) {
		correlation.table[["title"]] <- paste("Bayesian Pearson Correlations")
	} else if (spearman) {
		correlation.table[["title"]] <- paste("Bayesian Spearman Correlations")
	} else if (kendallsTauB) {
		correlation.table[["title"]] <- paste("Bayesian Kendall's Tau")
	} else {
		correlation.table[["title"]] <- paste("Bayesian Correlation Table")
	}
	
	# Note: Describe column names to the returned object
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	
	# Note: create footnote function
	footnotes <- .newFootnotes()
	if (flagSupported) {
		.addFootnote(footnotes, paste(bf.title, " > 10, ** , ", bf.title, " > 30, *** ", bf.title, " > 100"), symbol="*")
	}
	
	# Note: Go over each variable
	v.c <- length(variables)
	if (v.c > 0) {
		# Note: There are variables: 
		test.names <- list(pearson="Pearson's R", spearman="Spearman's Rho", kendall="Kendall's Tau B")
		column.names <- c()
		for (test in tests) {
			
			# Note: create columns per test
			if (length(tests) > 1 || reportBayesFactors) {
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
			
			# Note: create columns per test variable 
			for (variable.name in variables) {
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			# Note: create column for bfs
			if (reportBayesFactors) {
				column.name <- paste(".test[", test, "-p]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=bf.title, type="string")
				for (variable.name in variables) {
					column.name <- paste(variable.name, "[", test, "-p]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="sf:4;dp:3")
				}
			}
		}
		
		for (i in 1:v.c) {
			# Note: Create row given a column
			row <- list()
			row.footnotes <- list()
			variable.name <- variables[[i]]
			
			for (test in tests) {
				# Note: Create test row given a column
				bayes.factors <- list()
				
				if (length(tests) > 1 || reportBayesFactors)
					# Note: Create test name for each test row given a column
					row[[length(row)+1]] <- test.names[[test]]
				if (reportBayesFactors)
					# Note: Create bf row each test given each column (variable)
					bayes.factors[[length(bayes.factors)+1]] <- bf.title
				for (j in .seqx(1, i-1)) {
					# Note: Fill in blanks
					row[[length(row)+1]] <- ""
					bayes.factors[[length(bayes.factors)+1]] <- ""
				}
				
				row[[length(row)+1]] <- "\u2014" # em-dash # Note: Fill in blanks
				bayes.factors[[length(bayes.factors)+1]] <- ""
				
				for (j in .seqx(i+1, v.c)) {
					# Note: fill in blanks in table upper left-hand off diaganols
					variable.2.name <- variables[[j]]
					column.name <- paste(variable.2.name, "[", test, "]", sep="")
					v1 <- dataset[[ .v(variable.name) ]]
					v2 <- dataset[[ .v(variable.2.name) ]]
					
					# Note: Data screening 
					if (missingValues=="excludePairwise"){
						removeIndex1 <- which(is.na(v1))
						removeIndex2 <- which(is.na(v2))
						removeIndex <- unique(c(removeIndex1, removeIndex2))
						if (length(removeIndex) > 0){
							v1 <- v1[-(removeIndex)]
							v2 <- v2[-(removeIndex)]
						}
					}
					
					if (perform == "run") {
						# Note: Data: PREPARE
						some.r <- cor(v1, v2)
						some.n <- length(v1)
						
						# Note: Data and bfs check [start]
						if (is.na(some.r) || some.n <= 1) {
							# Note: Data: NOT ok, 
							# 		bf10: can't
							if (some.n <= 1){
								index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - not enough observations")
							} else if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
								index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - one (or more) variables contain infinity")
							} else {
								index <- .addFootnote(footnotes, "Sample correlation co-efficient r is undefined - one (or more) variables do not vary")
							}
							#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
							row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
							
							some.r <- NaN
							some.bf10 <- NaN
							some.bfPlus0 <- NaN
							some.bfMin0 <- NaN
						} else {
							# Note: Data: OK, 
							# 		bf10: PREPARE
							some.bf10 <- as.numeric(.bf10Corrie(n=some.n, r=some.r))
							
							if (is.na(some.bf10)){
								# Note: Data: OK
								# 		bf10: NOT ok 
								some.bfPlus0 <- NaN
								some.bfMin0 <- NaN
							} else {
								# Note: Data: OK, 
								# 		bf10: OK. 
								#		bfPlus0, bfMin0: PREPARE
								some.bfPlus0 <- as.numeric(.bfPlus0(n=some.n, r=some.r))
								some.bfMin0 <- as.numeric(.bfMin0(n=some.n, r=some.r))
								
								if (some.r > 0 && some.bfPlus0 > 1 && some.bfMin0 > 1){
									# Note: Data: OK, 
									# 		bf10: OK. 
									#		bfPlus0: OK
									#		bfMin0: NOT ok 
									# 
									# bfMin0 is bigger than one due to overflow: bfMin0 = 2*bf10 - bfPlus0. 
									# Example: 2*1.2.... 10^ 24 - 2.... 10^24 = 1... 10^12 (due to round off)
									#
									some.bfMin0 <- 10^(-317) 
									some.bfPlus0 <- 2*some.bf10 - some.bfMin0
								} else if (some.r < 0 && some.bfPlus0 > 1 && some.bfMin0 > 1){
									# Note: Data: OK, 
									# 		bf10: OK. 
									#		bfPlus0: NOT ok
									#		bfMin0: OK
									some.bfPlus0 <- 10^(-317) 
									some.bfMin0 <- 2*some.bf10 - some.bfPlus0
								} 
								# Note: bf10 [close]
							}
							# Note: data [close]
						}
						
						# Note: Assign bfs to be reported
						if (hypothesis == "correlated") {
							some.bf <- some.bf10
							
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							}
						} else if (hypothesis == "correlatedPositively"){
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							some.bf <- some.bfPlus0
							
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							}
						} else if (hypothesis == "correlatedNegatively") {
							some.bf <- some.bfMin0
							
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							}
						}
						
						# Note: Data [report]
						row[[length(row)+1]] <- .clean(some.r)
						
						# Note: Flagging at the data [report]
						if (flagSupported && is.na(some.bf) == FALSE) {
							if (some.bf > 100) {
								row.footnotes[[column.name]] <- list("***")
							} else if (some.bf > 30) {
								row.footnotes[[column.name]] <- list("**")
							} else if (some.bf > 10) {
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						# Note: Flagging and report bfs [report]
						if (reportBayesFactors) {
							bayes.factors[[length(bayes.factors)+1]] <- .clean(some.bf)
						} 
						# Note: run end, fill in blanks
					} else {
						# Note: run == NO
						row[[length(row)+1]] <- "."
						bayes.factors[[length(bayes.factors)+1]] <- "."
					}
				}
				
				
				if (reportBayesFactors) {
					for (bf in bayes.factors){
						row[[length(row)+1]] <- bf
					}
				}
				# Note: close each test	
			}
			
			names(row) <- column.names
			row[[".variable"]] <- variable.name
			if (length(row.footnotes) > 0)
				row[[".footnotes"]] <- row.footnotes
			rows[[i]] <- row
		}
	}
	schema <- list(fields=fields)
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	correlation.table[["footnotes"]] <- as.list(footnotes)
	correlation.table
}
## Help functions ------------------------------------------------------------
# 0. Prior specification
.priorRho <- function(rho, alpha=1) {
	priorDensity <- 2^(1-2*alpha)/beta(alpha, alpha)*(1-rho^2)^(alpha-1)
	logNormalisationConstant <- -lbeta(alpha, alpha)
	result <- exp(logNormalisationConstant)*priorDensity
	return(result)
}
.priorRhoPlus <- function(rho, alpha=1) {
	nonNegativeIndex <- rho >=0
	lessThanOneIndex <- rho <=1
	valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
	myResult <- rho*0
	myResult[valueIndex] <- 2*.priorRho(rho[valueIndex], alpha)
	return(myResult)
}
.priorRhoMin <- function(rho, alpha=1) {
	negativeIndex <- rho <=0
	greaterThanMinOneIndex <- rho >= -1
	valueIndex <- as.logical(negativeIndex*greaterThanMinOneIndex)
	myResult <- rho*0
	myResult[valueIndex] <- 2*.priorRho(rho[valueIndex], alpha)
	return(myResult)
}
# 1.0. Built-up for likelihood functions
.myAFunction <- function(n, r, rho) {
	hyperTerm <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), (1/2), (r*rho)^2))
	myResult <- (1-rho^2)^((n-1)/2)*hyperTerm
	return(myResult)
}
.myBFunction <- function(n, r, rho) {
	hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2))
	hyperTerm2 <- Re(hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2))
	logTerm <- 2*lgamma(n/2)-2*lgamma((n+1)/2)
	myResult <- 2^(-1)*(1-rho^2)^((n-1)/2)*exp(logTerm)*
		((1-2*n*(r*rho)^2)/(r*rho)*hyperTerm1 -(1-(r*rho)^2)/(r*rho)*hyperTerm2)
	return(myResult)
}
.myHFunction <- function(n, r, rho) {
	myResult <- .myAFunction(n, r, rho) + .myBFunction(n, r, rho)
	return(myResult)
}
.jeffreysApproxH <- function(n, r, rho) {
	return(((1 - rho^(2))^(0.5*(n - 1)))/((1 - rho*r)^(n - 1 - 0.5)))
}
# 1.1 Explicit marginal likelihood functions
.m0MarginalLikelihood <- function(s, t, n) {
	logTerm <- 2*lgamma(0.5*(n-1))
	result <- 1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*exp(logTerm)
	return(result)
}
.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
	return(.m0MarginalLikelihood(s, t, n)*
		   	(.myAFunction(n, r, rho)+.myBFunction(n, r, rho)))
}
#
# 2.1 Two-sided main Bayes factor ----------------------------------------------
.bf10Corrie <- function(n, r, alpha=1) {
	# See Ly et al 2014
	# Check for sample correlations
	#
	if (any(is.na(r))){
		return(NaN)
	}
	# TODO: use which
	checkR <- abs(r) >= 1 # check whether |r| >= 1
	if (alpha <= 1 && n > 2 && checkR) {
		return(Inf)
	}
	myExactResult <- .bf10Exact(n, r, alpha)
	if (is.na(myExactResult) || myExactResult < 0 ){
		# Exact failed :(
		if (alpha == 1){
			ejResult <- .bf10JeffreysIntegrate(n, r)
			if (is.na(ejResult) || ejResult < 0){
				# EJ result failed
				numericalResult <- .bf10Numerical(n, r, alpha)
				if (is.na(numericalResult) || numericalResult < 0){
					# Numerical result failed
					jeffreysResult <- .bf10JeffreysApprox(n, r)
					if (is.na(jeffreysResult) || jeffreysResult < 0){
						# ALL failed
						return(NaN)
					} else if (jeffreysResult >= 0){
						# Jeffreys result success (numerical, EJ, Exact failed)
						return(jeffreysResult)
					}
				} else if (numericalResult >= 0){
					# Numerical result success (EJ, Exact failed)
					return(numericalResult)
				}
			} else if (ejResult >= 0){
				# EJ result success (Exact failed)
				return(ejResult)
			}
		} else if (alpha != 1){
			numericalResult <- .bf10Numerical(n, r, alpha)
			if (is.na(numericalResult) || numericalResult < 0){
				# Numerical result failed
				return(NaN)
			} else if (numericalResult >= 0){
				# Numerical result success (Exact failed)
				return(numericalResult)
			}
		}
	} else if (myExactResult >= 0){
		return(myExactResult)
	} else {
		# NO IDEA, EVERYTHING FAILED :(
		return(NaN)
	}
}
.bf10Exact <- function(n, r, alpha=1) {
	# Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	#
	#
	if (any(is.na(r))){
		return(NaN)
	}
	# TODO: use which
	checkR <- abs(r) >= 1 # check whether |r| >= 1
	if (alpha <= 1 && n > 2 && checkR) {
		return(Inf)
	}
	logHyperTerm <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	myLogResult <- log(2^(1-2*alpha))+0.5*log(pi)-lbeta(alpha, alpha)+
		lgamma((n+2*alpha-1)/2)-lgamma((n+2*alpha)/2)+logHyperTerm
	realResult <- exp(Re(myLogResult))
	#return(realResult)
	return(realResult)
}
# 2.2 Two-sided secondairy Bayes factor
.bf10JeffreysIntegrate <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	if ( any(is.na(r)) ){
		return(NaN)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	hyperTerm <- Re(hypergeo::hypergeo((2*n-3)/4, (2*n-1)/4, (n+2)/2, r^2))
	logTerm <- lgamma((n+1)/2)-lgamma((n+2)/2)
	myResult <- sqrt(pi)/2*exp(logTerm)*hyperTerm
	return(myResult)
}
# 2.3 Two-sided third Bayes factor
.bf10Numerical <- function(n, r, alpha=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is a numerical approximation for .bf10JeffreysIntegrate,
	# when it explodes
	# #
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	# TODO: REMOVE ALL NUMERICAL STUFF
	return(NaN)
	if ( any(is.na(r)) ){
		return(NaN)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	# TODO: be very careful here, might integrate over non-finite function
	integrand <- function(rho){.myHFunction(n, r, rho)*priorRho(rho, alpha)}
	exactIntegrate <- integrate(integrand, -1, 1)
	if (is.na(exactIntegrate) || exactIntegrate < 0){
		# Exact Integrate failed
		if (alpha==1){
			# TODO: be very careful here, might integrate over non-finite function
			integrand <- function(rho){.jeffreysApproxH(n, r, rho)*priorRho(rho, alpha=1)}
			jeffreysNumericalIntegrate <- integrate(integrand, -1, 1)
			if (is.na(jeffreysNumericalIntegrate) || jeffreysNumericalIntegrate < 0){
				return(NaN)
			} else if (jeffreysNumericalIntegrate >= 0){
				# jeffreys numerical integrate success (exact Integrate failed)
				return(jeffreysNumericalIntegrate)
			}
		} else if (alpha != 1){
			# ALL failed :(
			return(NaN)
		}
	} else if (exactIntegrate >= 0){
		# Exact Integrate success!
		return(exactIntegrate)
	} else {
		# NO IDEA, EVERYTHING FAILED :(
		return(NaN)
	}
}
# 2.4. Two-sided fourth Bayes factor
.bf10JeffreysApprox <- function(n, r) {
	#Jeffreys' test for whether a correlation is zero or not
	#Jeffreys (1961), pp. 291 Eq. 14
	#
	if ( any(is.na(r)) ){
		return(NaN)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	myResult <- ((2*n-3)/pi)^(.5)*(1-r^2)^((n-4)/2)
	return(1/myResult)
}
# 3.0 One-sided preparation ----------------------------------------------------
.mPlusMarginalB <- function(n, r, alpha=1){
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
										   L=c(1/2, (n+2*alpha+3)/2), z=r^2))
	hyperTerm2 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
										   L=c(3/2, (n+2*alpha+1)/2), z=r^2))
	hyperTerm3 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
										   L=c(3/2, (n+2*alpha+3)/2), z=r^2))
	sumTerm <- -((n*r)^2*hyperTerm1-n^2*(n+2*alpha+1)*hyperTerm2+2*n^3*r^2*
				 	hyperTerm3+(2*n^2-2*alpha*(1-2*n)+n-1))
	productTerm <- (2^(1-2*alpha)*r)/((n+2*alpha-1)*(n+2*alpha+1))
	logTerm <- 2*lgamma(n/2)-2*lgamma((n+1)/2)-lbeta(alpha, alpha)
	result <- productTerm*exp(logTerm)*sumTerm
	return(result)
}
# 3.1 One-sided 1 BF
.bfPlus0 <- function(n, r, alpha=1){
	# Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#
	if ( any(is.na(r)) ){
		return(NaN)
	}
	if (alpha <= 1 && n > 2 && r>=1) {
		return(Inf)
	} else if (alpha <= 1 && n > 2 && r<=-1){
		return(0)
	}
	# TODO: Calculate TWO SIDED FIRST, if it is infinite
	# and everythings going into the same direction then just return infinity as well
	#
	#TODO: Test for NAs
	myResult <- .bf10Exact(n, r, alpha) + .mPlusMarginalB(n, r, alpha)
	# TODO: use which to make it dynamic (only when I actually want to draw it as a function of r)
	if (is.na(myResult) || myResult < 0){
		# Exact BFPlus failed (negative or undefined)
		if (alpha==1){
			# Try EJs BF
			bf10EJ <- .bf10JeffreysIntegrate(n, r)
			mPlusEJ <- .mPlusMarginalBJeffreysIntegrate(n, r)
			ejResult <- bf10EJ+mPlusEJ
			if (is.na(ejResult) || ejResult < 0){
				# EJs BF failed (negative or undefined)
				# Try numerical BF
				numericalResult <- .numericalPlus0(n, r, alpha)
				#numericalResult <- NaN
				if (is.na(numericalResult) || numericalResult < 0){
					# ALL failed :(
					if (r > 0 && n > 2 && alpha <= 1){
						# postive hypothesis and positive r
						return(Inf)
					} else if (r < 0 && n > 2 && alpha <= 1){
						# postive hypothesis and negative r
						return(0)
					} else {
						# Totally no idea EVERYTHING FAILED UNIVERSAL COLLAPSE ETC
						return(NaN)
					}
				} else if (numericalResult >= 0){
					# Numerical BF success (EJ and exact failed)
					return(numericalResult)
				}
			} else if (ejResult >= 0) {
				# EJ BF success (Exact failed)
				return(ejResult)
			}
		} else if (alpha != 1){
			# Try numerical BF
			numericalResult <- .numericalPlus0(n, r, alpha)
			if (is.na(numericalResult) || numericalResult < 0){
				# ALL failed
				return(NaN)
			} else if (numericalResult >= 0){
				# Numerical BF success (Exact failed)
				return(numericalResult)
			}
		}
	} else if (myResult >= 0){
		# Exact BF success
		return(myResult)
	} else {
		# NO IDEA, EVERYTHING FAILED :(
		return(NaN)
	}
}
.numericalPlus0 <- function(n, r, alpha=1){
	# Ly et al 2014
	# This is a numerical approximation
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#
	# TODO: REMOVE ALL NUMERICAL STUFF
	return(NaN)
	if ( any(is.na(r)) ){
		return(NaN)
	}
	if (alpha <= 1 && n > 2 && r>=1) {
		return(Inf)
	} else if (alpha <= 1 && n > 2 && r<=-1){
		return(0)
	}
	if (alpha == 1){
		# TODO: be very careful here, might integrate over non-finite function
		#
		integrand <- function(rho){.jeffreysApproxH(n, r, rho)*
								   	.priorRhoPlus(rho, alpha)}
		myJeffreysHResult <- integrate(integrand, lower=0, upper=1)
		if (is.na(myJeffreysHResult) || myJeffreysHResult < 0){
			# All numerical failed
			return(NaN)
		} else if (myJeffreysHResult >= 0){
			# Numerical Jeffreys H success (Exact H failed)
			return(myJeffreysHResult)
		}
	} else if (alpha != 1){
		return(NaN)
	}
	# NO IDEA, EVERYTHING FAILED :(
	return (NaN)
}
.bfMin0 <- function(n, r, alpha=1){
	# Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	#
	myResult <- .bfPlus0(n, -r, alpha=alpha)
	return(myResult)
}
.mPlusMarginalBJeffreysIntegrate <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	if ( any(is.na(r)) ){
		return(NaN)
	}
	# TODO: use which
	if (n > 2 && r==1) {
		return(Inf)
	} else if (n > 2 && r==-1){
		return(0)
	}
	hyperTerm <- Re(hypergeo::genhypergeo(U=c(1, (2*n-1)/4, (2*n+1)/2),
										  L=c(3/2, (2*n+3)/2), z=r^2))
	myResult <- (2*n-3)/(2*n+2)*r*hyperTerm
	return(myResult)
}
.bfPlus0JeffreysIntegrate <- function(n, r){
	myResult <- .bf10JeffreysIntegrate(n, r) + .mPlusMarginalBJeffreysIntegrate(n, r)
	return(myResult)
}
# 4.0 Posteriors to graph TODO: we have to think about this, different
# results, thus, also switching of the illustrations?
#
# 4.1 Two-sided
.posteriorRho <- function(n, r, rho, alpha=1){
	return(1/.bf10Exact(n,r)*.myHFunction(n, r, rho)*.priorRho(rho, alpha))
}
.posteriorRhoPlus <- function(n, r, rho, alpha=1){
	return(1/.bfPlus0(n, r, alpha)*.myHFunction(n, r, rho)*.priorRhoPlus(rho, alpha))
}
.posteriorRhoMin <- function(n, r, rho, alpha=1){
	return(1/.bfMin0(n, r, alpha)*.myHFunction(n, r, rho)*.priorRhoMin(rho, alpha))
}


#------------------------------------------------- Matrix Plot -------------------------------------------------#

#### Plotting Function for posterior ####
.plotPosterior.BayesianCorrelationMatrix <- function(x, y, alpha=1, oneSided= FALSE, addInformation= FALSE, drawCI= FALSE, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.28, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2) {


	r <- cor(x, y)
	n <- length(x)
	
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
	
	rho <- seq(min(xticks), max(xticks),length.out = 1000)	
	
	
	if (oneSided == FALSE) {
		
		posteriorLine <- .posteriorRho(rho= rho, n= n, r= r, alpha= alpha)
		
	} else if (oneSided == "right") {
		
		posteriorLine <- .posteriorRhoPlus(rho= rho, n= n, r= r, alpha= alpha)
		
	} else if (oneSided == "left") {
		
		posteriorLine <- .posteriorRhoMin(rho= rho, n= n, r= r, alpha= alpha)
		
	}
	
	ylim <- range(yticks)
	
	plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(rho, posteriorLine, lwd= lwd)
		
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at = c(ylim[1], mean(ylim), ylim[2]) , pos= range(xticks)- 0.08*diff(range(xticks)), labels = c("", "Density", ""), lwd.ticks=0, cex.axis= 1.7, mgp= c(3, 0.7, 0), las=0)
	
	mtext(expression(rho), side = 1, cex = cexXlab, line= 2.08)
	
}


#### Matrix Plot function #####
.correlationMatrixPlotBayesian <- function(dataset, perform, options, hypothesis=options$hypothesis) {

	if (!options$plotCorrelationMatrix)
		return()
	
	correlation.plot <- list()
	
	if (hypothesis == "correlated") {
	
		oneSided <- FALSE
		
	} else if (hypothesis == "correlatedPositively") {
	
		oneSided <- "right"
		
	} else if (hypothesis == "correlatedNegatively") {
	
		oneSided <- "left"
		
	}

	if (perform == "init") {
	
		variables <- unlist(options$variables)
		
		l <- length(variables)
		
		if (l <= 2 && (options$plotDensities || options$plotPosteriors)) {
		
			width <- 580
			height <- 580
				
		} else if (l <= 2) {
			
			width <- 400
			height <- 400
				
		} else {
			
			width <- 250 * l
			height <- 250 * l
			
		}	
				
		plot <- list()
			
		plot[["title"]] <- variables 
		plot[["width"]]  <- width
		plot[["height"]] <- height

		correlation.plot[[1]] <- plot
	}
	
	if (perform == "run" && length(options$variables) > 0) {
				
		variables <- unlist(options$variables)
		
		l <- length(variables)
		
		# check for numeric/integer variables & !infinity & standard deviation > 0				
		d <- vector("character", length(.v(variables)))
		sdCheck <- vector("numeric", length(.v(variables)))
		infCheck <- vector("logical", length(.v(variables)))
		
		for (i in seq_along(.v(variables))) {
		
			d[i] <- class(dataset[[.v(variables)[i]]])
			sdCheck[i] <- sd(dataset[[.v(variables)[i]]], na.rm=TRUE)
			infCheck[i] <- any(is.infinite(dataset[[.v(variables)[i]]]) == TRUE)
		}
		
	
		ind1 <- d == "numeric" | d == "integer"
		ind2 <- sdCheck > 0
		ind <- ind1 & ind2 & infCheck == FALSE
		
				
		variables <- .v(variables)[ind]
		
		l <- length(variables)
			
			
		if (l <= 2 && (options$plotDensities || options$plotPosteriors)) {
		
			width <- 580
			height <- 580
				
		} else if (l <= 2) {
			
			width <- 400
			height <- 400
				
		} else {
			
			width <- 250 * l
			height <- 250 * l
			
		}	
		# } else if (l <= 2) {
		# 
		# 	width <- 400
		# 	height <- 400
		# 	
		# } else if (l == 3) {
		# 
		# 	width <- 700
		# 	height <- 700
		# 	
		# } else if (l == 4) {
		# 
		# 	width <- 900
		# 	height <- 900
		# 	
		# } else if (l >= 5) {
		# 
		# 	width <- 1100
		# 	height <- 1100
		# 	
		# }
		
		correlation.plot <- list()
				
		plot <- list()
			
		plot[["title"]] <- .unv(variables)
		plot[["width"]]  <- width
		plot[["height"]] <- height
				
		correlation.plot[[1]] <- plot

		if (length(variables) > 0) {
								
			image <- .beginSaveImage(width, height)
			
				if (l == 1) {
				
					par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))	
					
					.plotMarginalCor(dataset[[variables[1]]]) 
					mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)	
					
				} else if (l == 2 && !options$plotDensities && !options$plotPosteriors) {
					
					par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
					
					maxYlab <- .plotScatter(dataset[[variables[1]]], dataset[[variables[2]]])
					distLab <- maxYlab / 1.8
					
					mtext(text = .unv(variables)[1], side = 1, cex=1.5, line = 3)
					mtext(text = .unv(variables)[2], side = 2, cex=1.5, line = distLab + 2, las=0)
					
				} else if (l > 1) {
				
					par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))
				
					for (row in seq_len(l)) {
					
						for (col in seq_len(l)) {
						
							if (row == col) {
								
								if (options$plotDensities) {
									.plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
								} else {
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}
								
							if (col > row) {
							
								if (options$plotCorrelationMatrix) {
									.plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
								} else {
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}
							
							if (col < row) {							
							
									if (options$plotPosteriors) {
										.plotPosterior.BayesianCorrelationMatrix(dataset[[variables[col]]], dataset[[variables[row]]], oneSided=oneSided)
									} else {
										plot(1, type= "n", axes= FALSE, ylab="", xlab="")
									}
							}
						}		
					}
				}
				
				
				if (l > 2 || ((l == 2 && options$plotDensities) || (l == 2 && options$plotPosteriors))) {
				
					textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
					
					if (!options$plotDensities && !options$plotPosteriors) {
					
							for (t in seq_along(textpos)) {
							
								mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
								
								if (t < length(textpos)) {
									mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
								}
							}
						
					} else {
					
						for (t in seq_along(textpos)) {
							
								mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
								mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
						}
					}
				}
			
			content <- .endSaveImage(image)
					
			plot <- correlation.plot[[1]]
			plot[["data"]]  <- content
			correlation.plot[[1]] <- plot
			
		}	
	}
	
	correlation.plot
}
