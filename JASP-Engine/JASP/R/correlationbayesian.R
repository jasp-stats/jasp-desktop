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
				dataset <-
					.readDataSetToEnd(columns.as.numeric=options$variables,
									  exclude.na.listwise=options$variables)
			} else {
				dataset <-
					.readDataSetToEnd(columns.as.numeric=options$variables)
			}
		} else {
			dataset <-
				.readDataSetHeader(columns.as.numeric=options$variables)
		}
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=options$variables)
	}
	results <- list()
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="correlations", type="table")
	
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
								  bayesFactorType=options$bayesFactorType)
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
									  bayesFactorType=bayesFactorType) {
	# TODO: check for all arguments in particular meansAndStdDev,
	# hypothesis="correlated"
	#
	#
	correlation.table <- list()
	correlation.table[["citation"]] <- list(
		"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
	)
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
	# test contains the tests that are performed
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")
	# Naming of the tests
	if (length(tests) != 1) {
		correlation.table[["title"]] <- paste("Bayes Factors ", bf.title, ": Correlation Table")
	} else if (pearson) {
		correlation.table[["title"]] <- paste("Bayes Factors ", bf.title, ": Pearson Correlations")
	} else if (spearman) {
		correlation.table[["title"]] <- paste("Bayes Factors ", bf.title, ": Spearman Correlations")
	} else if (kendallsTauB) {
		correlation.table[["title"]] <- paste("Bayes Factors ", bf.title, ": Kendall's Tau")
	} else {
		correlation.table[["title"]] <- paste("Bayes Factors ", bf.title, ": Correlation Table")
	}
	# Describe column names to the returned object
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	# create footnote function
	footnotes <- .newFootnotes()
	if (flagSupported) {
		if (hypothesis == "correlated") {
			.addFootnote(footnotes, paste(bf.title, " > 3, ** , ", bf.title, " > 10, *** ", bf.title, " > 30"), symbol="*")
		} else {
			.addFootnote(footnotes, paste(bf.title, " > 3, ** , ", bf.title, " > 10, *** ", bf.title, " > 30, all one-tailed"), symbol="*")
		}
	}
	v.c <- length(variables)
	if (v.c > 0) {
		# If there are variables
		test.names <- list(pearson="Pearson's R", spearman="Spearman's Rho", kendall="Kendall's Tau B")
		column.names <- c()
		for (test in tests) {
			# Creating columns
			if (length(tests) > 1 || reportBayesFactors) {
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
			for (variable.name in variables) {
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
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
			# For each variable
			row <- list()
			row.footnotes <- list()
			variable.name <- variables[[i]]
			for (test in tests) {
				bayes.factors <- list()
				if (length(tests) > 1 || reportBayesFactors)
					row[[length(row)+1]] <- test.names[[test]]
				if (reportBayesFactors)
					bayes.factors[[length(bayes.factors)+1]] <- bf.title
				for (j in .seqx(1, i-1)) {
					row[[length(row)+1]] <- ""
					bayes.factors[[length(bayes.factors)+1]] <- ""
				}
				row[[length(row)+1]] <- "\u2014" # em-dash
				bayes.factors[[length(bayes.factors)+1]] <- ""
				for (j in .seqx(i+1, v.c)) {
					# fill in blanks in table upper left-hand off diaganols
					variable.2.name <- variables[[j]]
					column.name <- paste(variable.2.name, "[", test, "]", sep="")
					v1 <- dataset[[ .v(variable.name) ]]
					v2 <- dataset[[ .v(variable.2.name) ]]
					if (perform == "run") {
						some.r <- cor(v1, v2)
						some.n <- length(v1)
						if (is.na(some.r)) {
							if (some.n <= 1){
								index <- .addFootnote(footnotes, "Correlation co-efficient is undefined - not enough observations")
							} else if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
								index <- .addFootnote(footnotes, "Correlation co-efficient is undefined - one (or more) variables contain infinity")
							} else {
								index <- .addFootnote(footnotes, "Standard deviation is undefined - one (or more) variables contain an NA, or do not vary")
							}
							#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
							row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
							some.r <- NaN
							some.bf <- NaN
						}
						if (hypothesis == "correlated") {
							# TODOTODO:
							# Check up on NA for some.bf
							some.bf <- as.numeric(.bf10Corrie(n=some.n, r=some.r))
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							}
						} else if (hypothesis == "correlatedPositively"){
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							some.bf <- as.numeric(.bfPlus0(n=some.n, r=some.r))
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							} 
						} else if (hypothesis == "correlatedNegatively") {
							some.bf <- as.numeric(.bfMin0(n=some.n, r=some.r))
							if (bayesFactorType == "BF01"){
								some.bf <- 1/some.bf
							}
						} 
						
						row[[length(row)+1]] <- .clean(some.r)
						if (flagSupported && is.na(some.bf) == FALSE) {
							if (some.bf > 30) {
								row.footnotes[[column.name]] <- list("***")
							} else if (some.bf > 10) {
								row.footnotes[[column.name]] <- list("**")
							} else if (some.bf > 3) {
								row.footnotes[[column.name]] <- list("*")
							}
						}
						if (reportBayesFactors)
							bayes.factors[[length(bayes.factors)+1]] <- .clean(some.bf)
					} else {
						row[[length(row)+1]] <- "."
						bayes.factors[[length(bayes.factors)+1]] <- "."
					}
				}
				if (reportBayesFactors) {
					for (bf in bayes.factors)
						row[[length(row)+1]] <- bf
				}
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
	# 	and everythings going into the same direction then just return infinity as well
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
					} else if  (r < 0 && n > 2  && alpha <= 1){
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