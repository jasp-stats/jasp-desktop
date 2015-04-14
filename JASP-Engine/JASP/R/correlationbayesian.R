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
# "bayesFactorType": BF10/BF01/LogBF10
CorrelationBayesian <- function(dataset=NULL, options, perform="run",
								callback=function(...) 0, ...) {
	
	state <- .retrieveState()
	
	diff <- NULL
	
	if (!is.null(state)) {
	
		diff <- .diff(options, state$options)
	
	}
	
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
	
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$credibleIntervals == FALSE && diff$credibleIntervalsInterval == FALSE
			&& diff$hypothesis == FALSE && diff$kendallsTauB == FALSE && diff$missingValues == FALSE && diff$pearson == FALSE && diff$plotCorrelationMatrix == FALSE
			&& diff$plotDensitiesForVariables == FALSE && diff$plotPosteriors == FALSE && diff$spearman == FALSE && diff$variables == FALSE)))) {
			
			results[["plots"]] <- state$correlationPlots
			
	} else {
	
		results[["plots"]] <- .correlationMatrixPlotBayesian(dataset, perform, options, hypothesis=options$hypothesis)
	}
	
	if (perform == "init") {
		if (length(options$variables) < 2) {
			results <- list(results=results, status="complete")
			return(results)
		} else {
			results <- list(results=results, status="inited")
			return(results)
		}
	} else {
		return(list(results=results, status="complete", state=list(options=options, results=results, correlationPlots=results$plots)))
	}
	
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
		"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2015). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
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
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u2081\u2080)"
		}
	} else if (hypothesis == "correlatedPositively") {
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208A\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208A"
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u208A\u2080)"
		}
	} else if (hypothesis == "correlatedNegatively"){
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208B\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208B"
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u208B\u2080)"
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
		if (bayesFactorType=="LogBF10"){
			.addFootnote(footnotes, paste(bf.title, " > log(10), ** , ", bf.title, " > log(30), *** ", bf.title, " > log(100)"), symbol="*")
		} else {
			.addFootnote(footnotes, paste(bf.title, " > 10, ** , ", bf.title, " > 30, *** ", bf.title, " > 100"), symbol="*")
		}
		
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
						pairwise.excluded.data <- .excludePairwiseCorData(v1, v2)
						v1 <- pairwise.excluded.data$v1
						v2 <- pairwise.excluded.data$v2
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
								index <- .addFootnote(footnotes, "Pearson's sample correlation coefficient r is undefined -- too few observations")
							} else if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
								index <- .addFootnote(footnotes, "Pearson's sample correlation coefficient r is undefined -- one or more variables contain infinity")
							} else {
								index <- .addFootnote(footnotes, "Pearson's sample correlation coefficient r is undefined -- one or more variables do not vary")
							}
							#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
							row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
							
							
							some.r <- NaN
							
							# TODOTODO: Generalise to other alphas and rho0s
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
						} else {
							# TODOTODO: Generalise to other alphas and rho0s
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
						}
						
						# Note: Assign bfs to be reported
						if (hypothesis == "correlated") {
							report.bf <- all.bfs$bf10
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							} 
						} else if (hypothesis == "correlatedPositively"){
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							report.bf <- all.bfs$bfPlus0
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							} 
							
						} else if (hypothesis == "correlatedNegatively") {
							report.bf <- all.bfs$bfMin0
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							} 
						}
						
						# Note: Data [report]
						# TODO: also for other alphas and find place to report the credible interval
						#	We now report the posterior median 
						median.rho <- try(.rhoQuantile(some.n, some.r, alpha=1)[2])
						
						if (is(median.rho, "try-error")){
							report.r <- some.r
						} else {
							report.r <- median.rho
						}
						
						row[[length(row)+1]] <- .clean(report.r)
						
						# Note: Flagging at the data [report]
						if (flagSupported && is.na(report.bf) == FALSE) {
							if (report.bf > 100) {
								row.footnotes[[column.name]] <- list("***")
							} else if (report.bf > 30) {
								row.footnotes[[column.name]] <- list("**")
							} else if (report.bf > 10) {
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						# Note: Flagging and report bfs [report]
						if (reportBayesFactors) {
							if (bayesFactorType == "LogBF10") {
								report.bf <- base::log10(report.bf)
							}
							bayes.factors[[length(bayes.factors)+1]] <- .clean(report.bf)
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
.excludePairwiseCorData <- function(v1, v2){
	# To exclude the data pairwise
	#
	myScreenedData <- list(v1=v1, v2=v2)
		
	removeIndex1 <- which(is.na(v1))
	removeIndex2 <- which(is.na(v2))
	removeIndex <- unique(c(removeIndex1, removeIndex2))
	if (length(removeIndex) > 0){
		myScreenedData$v1 <- v1[-(removeIndex)]
		myScreenedData$v2 <- v2[-(removeIndex)]
	}
	
	return(myScreenedData)
}


.myScaledBeta <- function(rho, alpha, beta){
	priorDensity <- ((1+rho)/2)^(alpha-1)*((1-rho)/2)^(beta-1)
	logNormalisationConstant <- -lbeta(alpha, beta)
	result <- 1/2*exp(logNormalisationConstant)*priorDensity
	return(result)
}

.priorRho <- function(rho, alpha=1) {
	.myScaledBeta(rho, alpha, alpha)	
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
	#hyperTerm <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), (1/2), (r*rho)^2))
	hyperTerm <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(1/2), z=(r*rho)^2))
	myResult <- (1-rho^2)^((n-1)/2)*hyperTerm
	return(myResult)
}
.myBFunction <- function(n, r, rho) {
	#hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2))
	hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(1/2), z=(r*rho)^2))
	hyperTerm2 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(-1/2), z=(r*rho)^2))
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
.bf10Exact <- function(n, r, alpha=1) {
	# Ly et al 2015
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
	#logHyperTerm <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	logHyperTerm <- log(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=((n+2*alpha)/2), z=r^2))
	myLogResult <- log(2^(1-2*alpha))+0.5*log(pi)-lbeta(alpha, alpha)+
		lgamma((n+2*alpha-1)/2)-lgamma((n+2*alpha)/2)+logHyperTerm
	realResult <- exp(Re(myLogResult))
	#return(realResult)
	return(realResult)
}

# 2.2 Two-sided secondairy Bayes factor
.bf10JeffreysIntegrate <- function(n, r, alpha=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	if ( any(is.na(r)) ){
		return(NaN)
	}
	
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	hyperTerm <- Re(hypergeo::genhypergeo(U=c((2*n-3)/4, (2*n-1)/4), L=(n+2*alpha)/2, z=r^2))
	logTerm <- lgamma((n+2*alpha-1)/2)-lgamma((n+2*alpha)/2)-lbeta(alpha, alpha)
	myResult <- sqrt(pi)*2^(1-2*alpha)*exp(logTerm)*hyperTerm
	return(myResult)
}

# 2.3 Two-sided third Bayes factor
.bfCorNumerical <- function(n, r, alpha=1, lowerRho=-1, upperRho=1) {
	# Numerically integrate Jeffreys approximation of the likelihood
	integrand <- function(rho){.jeffreysApproxH(n, r, rho)*.priorRho(rho, alpha)}
	someIntegral <- try(integrate(integrand, lowerRho, upperRho))
	
	if (is(someIntegral, "try-error")) {
		return(NA)
	}
	
	if (someIntegral$message=="OK"){
		return(someIntegral$value)
	} else {
		return(NA)
	}
}

.bf10Numerical <- function(n, r, alpha=1, lowerRho=-1, upperRho=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is a numerical approximation for .bf10JeffreysIntegrate,
	# when it explodes
	# #
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	# TODO: REMOVE ALL NUMERICAL STUFF
	if ( any(is.na(r)) ){
		return(NA)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	
	
	# TODO: be very careful here, might integrate over non-finite function
	jeffreysNumericalIntegrate <- .bfCorNumerical(n, r, alpha, lowerRho=-1, upperRho=1)
	
	if (is.na(jeffreysNumericalIntegrate) || jeffreysNumericalIntegrate < 0){
		return(NA)
	} else if (jeffreysNumericalIntegrate >= 0){
		# jeffreys numerical integrate success
		return(jeffreysNumericalIntegrate)
	} else {
		# NO IDEA, EVERYTHING FAILED :(
		return(NA)
	}
	return(jeffreysNumericalIntegrate)
}

# 2.4. Two-sided fourth Bayes factor
.bf10JeffreysApprox <- function(n, r) {
	#Jeffreys' test for whether a correlation is zero or not
	#Jeffreys (1961), pp. 291 Eq. 14
	#
	if ( any(is.na(r)) ){
		return(NA)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	myResult <- ((2*n-3)/pi)^(.5)*(1-r^2)^((n-4)/2)
	return(1/myResult)
}

# 3.0 One-sided preparation ----------------------------------------------------
# For .bfPlus0Exact
.mPlusExact <- function(n, r, alpha=1){
	# Ly et al 2015
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

# For .bfPlus0EJeffreysIntegrate
.mPlusJeffreysIntegrate <- function(n, r, alpha=1){
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	# This is the contribution of one-sided test
	#
	#	
	hyperTerm <- Re(hypergeo::genhypergeo(U=c(1, (2*n-1)/4, (2*n+1)/4),
										  L=c(3/2, (n+1+2*alpha)/2), z=r^2))
	logTerm <- -lbeta(alpha, alpha)
	myResult <- 2^(1-2*alpha)*r*(2*n-3)/(n+2*alpha-1)*exp(logTerm)*hyperTerm
	return(myResult)
}

.bfPlus0Numerical <- function(n, r, alpha=1, lowerRho=0, upperRho=1){
	# Ly et al 2015
	# This is a numerical approximation
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2015
	# This is the contribution of one-sided test
	#
	#
	if ( any(is.na(r)) ){
		return(NA)
	}
	if (alpha <= 1 && n > 2 && r>=1) {
		return(Inf)
	} else if (alpha <= 1 && n > 2 && r<=-1){
		return(0)
	}
	
	myNumericalJeffreys <- .bfCorNumerical(n, r, alpha, lowerRho, upperRho)
	# TODO: be very careful here, might integrate over non-finite function
	# in particular with the exact h function. 
	#
	if (!is.na(myNumericalJeffreys) && myNumericalJeffreys >= 0){
		# Note: Numerical Jeffreys okay
		return(myNumericalJeffreys)
	} else if (is.na(myNumericalJeffreys) || myNumericalJeffreys < 0){
		# All numerical failed
		return(NaN)
	} 
	return (NaN)
}

.bfPlus0JeffreysIntegrate <- function(n, r, alpha=1){
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	#	
	if ( any(is.na(r)) ){
		return(NaN)
	}
	if (n > 2 && r>=1) {
		return(Inf)
	} else if (n > 2 && r<=-1){
		return(0)
	}
	
	bf10 <- .bf10JeffreysIntegrate(n, r, alpha)
	mPlus <- .mPlusJeffreysIntegrate(n, r, alpha)
	
	if (is.na(bf10) || is.na(mPlus)){
		return(NA)
	}
	
	myResult <- bf10+mPlus	
	return(myResult)
}

## Suit:
.bfCorrieKernel <- function(n, r, alpha=1, method="exact"){
	myOutput <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
	
	# Note: Data check
	#
	if ( any(is.na(r)) ){
		return(myOutput)
	}
	# Note: Data: OK
	
	checkR <- abs(r) >= 1 # check whether |r| >= 1
	if (alpha <= 1 && n > 2 && checkR) {
		myOutput$bf10 <- Inf
		if (r > 0){
			myOutput$bfPlus0 <- Inf
			myOutput$bfMin0 <- 0
		} else if (r <= 0){
			myOutput$bfPlus0 <- 0
			myOutput$bfMin0 <- Inf
		}
		return(myOutput)
	}
	
	# Note: Different methods
	#
	if (method=="exact" || method==1){
		myOutput$bf10 <- .bf10Exact(n, r, alpha)
		
		# Note: bf10: CHECK
		if (is.na(myOutput$bf10) || myOutput$bf10 < 0){
			myOutput$bf10 <- NA
			return(myOutput)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(myOutput$bf10)){
			# Note: Check extreme
			if (r >= 0){
				myOutput$bfPlus0 <- Inf
				myOutput$bfMin0 <- 0
			} else if (r < 0){
				myOutput$bfPlus0 <- 0
				myOutput$bfMin0 <- Inf
			}
			return(myOutput)
		} else if (!base::is.infinite(myOutput$bf10)){
			# Note: bfPlus0, bfMin0: PREPARE
			myOutput$bfPlus0 <- myOutput$bf10 + .mPlusExact(n, r, alpha)
			myOutput$bfMin0 <- myOutput$bf10 + .mPlusExact(n, -r, alpha)
		}
	} else if (method=="jeffreysIntegrate" || method==2){
		myOutput$bf10 <- .bf10JeffreysIntegrate(n, r, alpha)
		
		# Note: bf10: CHECK
		if (is.na(myOutput$bf10) || myOutput$bf10 < 0){
			myOutput$bf10 <- NA
			return(myOutput)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(myOutput$bf10)){
			# Note: Check extreme
			if (r >= 0){
				myOutput$bfPlus0 <- Inf
				myOutput$bfMin0 <- 0
			} else if (r < 0){
				myOutput$bfPlus0 <- 0
				myOutput$bfMin0 <- Inf
			}
			return(myOutput)
		} else if (!base::is.infinite(myOutput$bf10)){
			# Note: bfPlus0, bfMin0: PREPARE
			myOutput$bfPlus0 <- myOutput$bf10 + .mPlusJeffreysIntegrate(n, r, alpha)
			myOutput$bfMin0 <- myOutput$bf10 + .mPlusJeffreysIntegrate(n, -r, alpha)
		}
	} else if (method=="numerical" || method==3){
		myOutput$bf10 <- .bf10Numerical(n, r, alpha, lowerRho=-1, upperRho=1)
		
		# Note: bf10: CHECK
		if (is.na(myOutput$bf10) || myOutput$bf10 < 0){
			myOutput$bf10 <- NA
			return(myOutput)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(myOutput$bf10)){
			# Note: Check extreme
			if (r >= 0){
				myOutput$bfPlus0 <- Inf
				myOutput$bfMin0 <- 0
			} else if (r < 0){
				myOutput$bfPlus0 <- 0
				myOutput$bfMin0 <- Inf
			}
			return(myOutput)
		} else if (!base::is.infinite(myOutput$bf10)){
			# Note: bfPlus0, bfMin0: PREPARE
			myOutput$bfPlus0 <- .bfPlus0Numerical(n, r, alpha, lowerRho=0, upperRho=1)
			myOutput$bfMin0 <- .bfPlus0Numerical(n, r, alpha, lowerRho=-1, upperRho=0)
		}
	} else if (method=="jeffreysApprox" || method==4){
		myOutput$bf10 <- .bf10JeffreysApprox(n, r)
		
		# Note: bf10: CHECK
		if (is.na(myOutput$bf10) || myOutput$bf10 < 0){
			myOutput$bf10 <- NA
			return(myOutput)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(myOutput$bf10)){
			# Note: Check extreme
			if (r >= 0){
				myOutput$bfPlus0 <- Inf
				myOutput$bfMin0 <- 0
			} else if (r < 0){
				myOutput$bfPlus0 <- 0
				myOutput$bfMin0 <- Inf
			}
			return(myOutput)
		}
		return(myOutput)
	}
	
	# Note: bfPlus0, bfMin0: CHECK
	if (any(is.na(c(myOutput$bfPlus0, myOutput$bfMin0)))){
		# Note: bfPlus0, bfMin0: NOT ok
		# 	if one is NA, then both are NA
		myOutput$bfPlus0 <- NA
		myOutput$bfMin0 <- NA
		return(myOutput)
	} else if (any(c(myOutput$bfPlus0, myOutput$bfMin0)==0)){
		# Note: bfPlus0, bfMin0: EXTREME
		# 	if one is extreme, so is the other
		if (myOutput$bfPlus0==0){
			myOutput$bfPlus0 <- 0
			myOutput$bfMin0 <- Inf
		} else if (myOutput$bfMin0==0){
			myOutput$bfPlus0 <- Inf
			myOutput$bfMin0 <- 0
		}
		return(myOutput)
	} 
	
	
	# Note: bfPlus0, bfMin0: CHECK COHERENCE:
	if (r > 0 && myOutput$bfPlus0 > 1 && myOutput$bfMin0 > 1 || any(c(myOutput$bfPlus0, myOutput$bfMin0)<0)){
		# Note: Data: OK, 
		# 		bf10: OK. 
		#		bfPlus0: OK
		#		bfMin0: NOT ok 
		# 
		# bfMin0 is bigger than one due to overflow: bfMin0 = 2*bf10 - bfPlus0. 
		# Example: 2*1.2.... 10^ 24 - 2.... 10^24 = 1... 10^12 (due to round off)
		#
		myOutput$bfMin0 <- 10^(-317) 
		myOutput$bfPlus0 <- 2*myOutput$bf10 - myOutput$bfMin0
	} else if (r < 0 && myOutput$bfMin0 > 1 && myOutput$bfPlus0 > 1 || any(c(myOutput$bfPlus0, myOutput$bfMin0)<0)){
		# Note: Data: OK, 
		# 		bf10: OK. 
		#		bfPlus0: NOT ok
		#		bfMin0: OK
		myOutput$bfPlus0 <- 10^(-317) 
		myOutput$bfMin0 <- 2*myOutput$bf10 - myOutput$bfPlus0
	}
	return(myOutput)
}



# 4.0 Posteriors to graph TODO: we have to think about this, different
# results, thus, also switching of the illustrations?
#


# 4.1 Two-sided
.posteriorRho <- function(n, r, rho, alpha=1){
	if (!is.na(r) && !r==0){
		return(1/.bf10Exact(n,r)*.myHFunction(n, r, rho)*.priorRho(rho, alpha))
	} else if (!is.na(r) && r==0){
		return(1/.bf10JeffreysIntegrate(n, r, alpha)*.jeffreysApproxH(n, r, rho)*.priorRho(rho, alpha))
	}	
}

.posteriorRhoPlus <- function(n, r, rho, alpha=1){
	if (!is.na(r) && !r==0){
		return(1/.bfCorrieKernel(n, r, alpha, method="exact")$bfPlus0*.myHFunction(n, r, rho)*.priorRhoPlus(rho, alpha))
	} else if (!is.na(r) && r==0){
		return(1/.bfCorrieKernel(n, r, alpha, method="jeffreysIntegrate")$bfPlus0*.jeffreysApproxH(n, r, rho)*.priorRhoPlus(rho, alpha))
	}	
}

.posteriorRhoMin <- function(n, r, rho, alpha=1){
	if (!is.na(r) && !r==0){
		return(1/.bfCorrieKernel(n, r, alpha, method="exact")$bfMin0*.myHFunction(n, r, rho)*.priorRhoMin(rho, alpha))
	} else if (!is.na(r) && r==0){
		return(1/.bfCorrieKernel(n, r, alpha, method="jeffreysIntegrate")$bfMin0*.jeffreysApproxH(n, r, rho)*.priorRhoMin(rho, alpha))
	}	
	
}


# 4.2 
.posteriorMean <- function(n, r, alpha=1){
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
	# 	and also for logTerm almost being 1 (it works okay if I cut off the hyperTerm1 
	# 	with three terms and hyperTerm2 with three terms and then divide them, though, 
	#	this is rather bad as a formal procedure due to the fact that it violates the 
	#	definition of products of sum sequences. Though it yields a good approximation.
	#
	# 	if (abs(r) < 0.5 && n <= 200){
	# 		logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	# 		hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2*alpha+2)/2), r^2))
	# 		hyperTerm2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	# 		
	# 		someFactor <- exp(logTerm)*hyperTerm1/hyperTerm2
	# 	} else {
	# 		# TODO: a linear approximation, needs proof
	# 		someFactor <- n/2
	# 	}
	
	logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	
	# Note: interestingly, it breaks down from n=199 to n=200 when using hypergeo
	# that is:
	#
	# 	.posteriorMean(200, 0.8) yields -2.600069e+26
	# 	.posteriorMean(199, 0.8) yields 0.7948551
	#
	#hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2*alpha+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	
	# Note: interestingly, it breaks down from n=199 to n=200 when using the integral form f15.3.1
	# that is:
	#
	# 	.posteriorMean(339, 0.8) yields 0.796992
	# 	.posteriorMean(340, 0.8) yields Inf
	# 
	# 		In hypergeo::f15.3.1((n/2), (n/2), ((n + 2 * alpha + 2)/2), r^2) :
	#			value out of range in 'gammafn'
	#
	#
	#hyperTerm1 <- Re(hypergeo::f15.3.1((n/2), (n/2), ((n+2*alpha+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::f15.3.1(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	
	# Note: interestingly, the continued fraction solution goes haywire, that is:
	#
	#
	#	.posteriorMean(n=67, 0.8) yielding 0.8526101  (a peak)
	#	.posteriorMean(n=299, 0.8) yielding -1.179415
	#
	#hyperTerm1 <- Re(hypergeo::hypergeo_contfrac((n/2), (n/2), ((n+2*alpha+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo_contfrac(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2))
	
	hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((n+2*alpha+2)/2), z=r^2))
	hyperTerm2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((n+2*alpha)/2), z=r^2))
	
	someFactor <- exp(logTerm)*hyperTerm1/hyperTerm2
	
	myResult <- 2*r/(n+2*alpha)*someFactor
	return(myResult)
}

.posteriorVariance <- function(n, r, alpha=1){
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
	# 	and also for logTerm almost being 1
	#
	# 	.posteriorVariance(199, 0.8) yields 6808.702
	# 	
	#
	hyperTerm3 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n+1)/2),L=(n+2*alpha)/2, z=r^2))
	hyperTerm4 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(n+2*alpha)/2, z=r^2))
	
	myResult0 <- 1/((r+2*alpha*r)^2)*(
		(n-1)*(n+2*alpha-1)-
			(2*alpha+1)*(n-2)*r^2+
			(n-1)*(n+2*alpha-1)*(r^2-1)*hyperTerm3/hyperTerm4)
	myResult <- myResult0-(.posteriorMean(n, r, alpha))^2
	return(myResult)
}

.posteriorAParameter <- function(n, r, alpha=1){
	# Method of moments estimate for a beta distribution
	# First scale back to means on the (0, 1) domain
	myMu <- (.posteriorMean(n, r, alpha)+1)/2
	myVar <- .posteriorVariance(n, r, alpha)/2^2
	
	myA <- myMu*(myMu*(1-myMu)/myVar-1)
	return(myA)
}

.posteriorBParameter <- function(n, r, alpha=1){
	# Method of moments estimate for a beta distribution
	# First scale back to means on the (0, 1) domain
	myMu <- (.posteriorMean(n, r, alpha)+1)/2
	myVar <- .posteriorVariance(n, r, alpha)/2^2
	
	myB <- (1-myMu)*(myMu*(1-myMu)/myVar-1)
	return(myB)
}

.rhoQuantile <- function(n, r, alpha=1, ciPercentage=.95){
	# Fitting parameters
	myA <- try(.posteriorAParameter(n, r, alpha))
	myB <- try(.posteriorBParameter(n, r, alpha))
	
	if (is(myA, "try-error") || is(myB, "try-error") || is.na(myA) || is.na(myB)) {
		return(c(NA, r, NA))
	}
	
	# Output median
	someMedian <- 2*qbeta(.5, myA, myB)-1
	
	# Calculate CI
	typeOne <- 1-ciPercentage
	
	leftCI <- try(2*qbeta(typeOne/2, myA, myB)-1)
	rightCI <- try(2*qbeta((1-typeOne/2), myA, myB)-1)
	
	# TODO: This actually doesn't override leftCI or rigthCI even if they are try-errors
	if ( is(leftCI, "try-error") || is(rightCI, "try-error") || is.na(leftCI) || is.na(rightCI) ){
		return(c(NA, r, NA))
	} else {
		return(c(leftCI, someMedian, rightCI))
	}
}

#------------------------------------------------- Matrix Plot -------------------------------------------------#

### empty posterior Plot with error message ###
.displayErrorPosterior <- function(errorMessage=NULL , xticks, xlabels, xlim, cexText=1.6, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.28, lwdAxis= 1.2) {

	plot(1, 1, xlim= xlim, ylim= 0:1, ylab= "", xlab="", type= "n", axes= FALSE)
	
	text(0, .5, errorMessage, cex=cexText)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at = c(0, .5, 1), pos= range(xticks)- 0.08*diff(range(xticks)), labels = c("", "Density", ""), lwd.ticks=0, cex.axis= 1.7, mgp= c(3, 0.7, 0), las=0)
	
	mtext(expression(rho), side = 1, cex = cexXlab, line= 2.5) #2.25

}

#### Plotting Function for posterior ####
.plotPosterior.BayesianCorrelationMatrix <- function(x, y, alpha=1, oneSided= FALSE, addInformation= FALSE, drawCI= FALSE, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.28, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2) {
	
	screenedData <- .excludePairwiseCorData(x, y)
	
	x <- screenedData$v1
	y <- screenedData$v2
	
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
		
	#if (oneSided == FALSE) {
	#	
	#	dmax <- optimize(f= function(x).posteriorRho(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	#	
	#} else if (oneSided == "right") {
	#	
	#	dmax <- optimize(f= function(x).posteriorRhoPlus(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	#	
	#} else if (oneSided == "left") {
	#	
	#	dmax <- optimize(f= function(x).posteriorRhoMin(x, n=n, r=r, alpha=alpha), interval= c(-1, 1), maximum = TRUE)$objective # get maximum density
	#}	
	
	# calculate position of "nice" tick marks and create labels
	xticks <- seq(-1.0, 1.0, 0.25)
	xlabels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
		
	rho <- seq(min(xticks), max(xticks),length.out = 1000)	
	
	
	if (oneSided == FALSE) {
		
		posteriorLine <- .posteriorRho(rho=rho, n=n, r=r, alpha=alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
		
			aParameter <- .posteriorAParameter(n=n, r=r)
			bParameter <- .posteriorBParameter(n=n, r=r)
			
			if (any(is.na(c(aParameter, bParameter)))) {
				
				errorMessage <- "Posterior is too peaked"
				.displayErrorPosterior(errorMessage=errorMessage, xticks=xticks, xlim=xlim, xlabels=xlabels)
				return()
			}
			
			posteriorLine <- .myScaledBeta(alpha=aParameter, beta=bParameter, rho=rho)
			
			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
				
				errorMessage <- "Posterior is too peaked"
				.displayErrorPosterior(errorMessage=errorMessage, xticks=xticks, xlim=xlim, xlabels=xlabels)
				return()
			}
			
		}
		
	} else if (oneSided == "right") {
		
		posteriorLine <- .posteriorRhoPlus(rho=rho, n=n, r=r, alpha=alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
				
				errorMessage <- "Posterior is too peaked"
				.displayErrorPosterior(errorMessage=errorMessage, xticks=xticks, xlim=xlim, xlabels=xlabels)
				return()
		}
			
			
		
	} else if (oneSided == "left") {
		
		posteriorLine <- .posteriorRhoMin(rho=rho, n=n, r=r, alpha=alpha)
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
				
				errorMessage <- "Posterior is too peaked"
				.displayErrorPosterior(errorMessage=errorMessage, xticks=xticks, xlim=xlim, xlabels=xlabels)
				return()
		}			
	}
	
	dmax <- max(posteriorLine)
		
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format= "f")
	
	plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(rho, posteriorLine, lwd= lwd)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at = c(ylim[1], mean(ylim), ylim[2]) , pos= range(xticks)- 0.08*diff(range(xticks)), labels = c("", "Density", ""), lwd.ticks=0, cex.axis= 1.7, mgp= c(3, 0.7, 0), las=0)
	
	mtext(expression(rho), side = 1, cex = cexXlab, line= 2.5) #2.25
	
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
		
		plot[["title"]] <- ""
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
		
		plot[["title"]] <-  "" # .unv(variables)
		plot[["width"]]  <- width
		plot[["height"]] <- height
		
		correlation.plot[[1]] <- plot
		
		if (length(variables) > 0) {
		
			p <- try(silent=FALSE, expr= {
				
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
					
					par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(1, 2.2, 2, 0))
					
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
			})
			
			if (class(p) == "try-error") {
			
				errorMessage <- .extractErrorMessage(p)
				plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
			}
			
			correlation.plot[[1]] <- plot
			
		}	
	}
	
	correlation.plot
}