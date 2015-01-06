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
	
	meta <- list(
		list(name="correlations", type="table"))
	results[[".meta"]] <- meta
	results[["correlations"]] <- 
		.correlationTableBayesian(dataset, perform, variables=options$variables, 
								  pearson=options$pearson, 
								  kendallsTauB=options$kendallsTauB, 
								  spearman=options$spearman,
								  hypothesis=options$hypothesis, 
								  flagSignificant=options$flagSignificant,
								  reportSignificance=options$reportSignificance)
	#bf.type=options$bayesFactorType)
	return(results)
}



.correlationTableBayesian <- function(dataset, perform, variables, pearson=TRUE, 
									  kendallsTauB=FALSE, spearman=FALSE, 
									  hypothesis="correlated", 
									  reportSignificance=FALSE,
									  flagSignificant=FALSE, 
									  crossProducts=FALSE, bf.type="BF10") {
	# TODO: check for all arguments in particular meansAndStdDev, 
	# hypothesis="correlated"
	#
	#
	correlation.table <- list()
	
	correlation.table[["citation"]] <- list(
		"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication.",
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software]."
	)	
	
	if (bf.type == "BF10") {
		bf.title <- "BF\u2081\u2080"
	} else {
		bf.title <- "BF\u2080\u2081"
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
	
	if (flagSignificant) {	
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
			if (length(tests) > 1 || reportSignificance) {
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
			
			for (variable.name in variables) {
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			if (reportSignificance) {
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
				
				if (length(tests) > 1 || reportSignificance)
					row[[length(row)+1]] <- test.names[[test]]
				if (reportSignificance)
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
								index <- .addFootnote(footnotes, "Correlation co-efficient is undefined - one (or more) variables do not vary")
							}
							#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
							row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
							some.r <- NaN
							some.bf <- NaN
						} 
						
						if (hypothesis == "correlated") {
							# TODO: this is a two sided BF. Fix this up for general rho0
							# TODOTODO:
							# Check up on NA for some.bf
							some.bf   <- as.numeric(.bf10Exact(n=some.n, r=some.r))
							
							# TODO: flips BFs
							if (bf.type == "BF01"){
								some.bf <- 1/some.bf
							}
						} else if (bf.type == "BF+0"){ 
							# TODO: add the labels "BF01" "BF10"  and "BF+0"  etc
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							some.bf <- as.numeric(.bfPlus0(n=some.n, r=some.r))
						} else if (bf.type == "BF0+") {
							some.bf <- as.numeric(.bfPlus0(n=some.n, r=some.r))
							some.bf <- 1/some.bf
						} else if (bf.type == "BF-0") {
							some.bf <- as.numeric(.bfMin0(n=some.n, r=some.r))
						} else if (bf.type == "BF0-") {
							some.bf <- as.numeric(.bfMin0(n=some.n, r=some.r))
							some.bf <- 1/some.bf
						} else {
							# TODO: Remove this after resolving the labelling of bf.type
							# TODOTODO: Something with erro messaging
							some.bf <- as.numeric(.bfPlus0(n=some.n, r=some.r))
							#some.bf <- as.numeric(.bf10Exact(n=some.n, r=some.r))
							#some.bf <- 1
						}
						
						row[[length(row)+1]] <- .clean(some.r)
						
						if (flagSignificant && is.na(some.bf) == FALSE) {
							if (some.bf > 30) {
								row.footnotes[[column.name]] <- list("***")
							} else if (some.bf > 10) {
								row.footnotes[[column.name]] <- list("**")
							} else if (some.bf > 3) {
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						if (reportSignificance)
							bayes.factors[[length(bayes.factors)+1]] <- .clean(some.bf)
						
					} else {
						row[[length(row)+1]] <- "."
						bayes.factors[[length(bayes.factors)+1]] <- "."
					}
				}
				
				if (reportSignificance) {
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

# .myBFunction <- function(n, r, rho) {
# 	hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2))
# 	hyperTerm2 <- Re(hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2))
# 	
# 	myResult <- 2^(-1)*(gamma(n/2)/gamma((n+1)/2))^2*(1-rho^2)^((n-1)/2)*
# 		((1-2*n*(r*rho)^2)/(r*rho)*hyperTerm1 -(1-(r*rho)^2)/(r*rho)*hyperTerm2)
# 	return(Re(myResult))
# }

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
#  OLD
# .m0MarginalLikelihood <- function(s, t, n) {
# 	return(1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*(gamma(0.5*(n-1)))^2)
# }

.m0MarginalLikelihood <- function(s, t, n) {
	logTerm <- 2*lgamma(0.5*(n-1))
	result <- 1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*exp(logTerm)
	return(result)
}

.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
	return(.m0MarginalLikelihood(s, t, n)*
		   	(.myAFunction(n, r, rho)+.myBFunction(n, r, rho)))
}

# OLD
#
# 
# .bf10Exact0 <- function(n, r, alpha=1) {
# 	# Ly et al 2014
# 	# This is the exact result with symmetric beta prior on rho
# 	# with parameter alpha. If alpha = 1 then uniform prior on rho
# 	hyperTerm <- hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2*alpha)/2), r^2)
# 	myResult <- 2^(1-2*alpha)*sqrt(pi)/beta(alpha, alpha)*
# 	gamma((n+2*alpha-1)/2)/gamma((n+2*alpha)/2)*hyperTerm
# 	realResult <- Re(myResult)
# 	return(realResult)
# }
#


# 2.1 Two-sided main Bayes factor
.bf10Exact <- function(n, r, alpha=1) {
	# Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#       2. check for r=1, r=-1
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
#  OLD
# 
# .bf10JeffreysIntegrate <- function(n, r) {
# 	# Jeffreys' test for whether a correlation is zero or not
# 	# Jeffreys (1961), pp. 289-292
# 	# This is the exact result, see EJ
# 	#
# 	hyperTerm <- hypergeo::hypergeo((2*n-3)/4, (2*n-1)/4, (n+2)/2, r^2)
# 	myResult <- sqrt(pi)/2*gamma((n+1)/2)/gamma((n+2)/2)*hyperTerm
# 	realResult <- Re(myResult)
# 	return(realResult)
# }

.bf10JeffreysIntegrate <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#       2. check for r=1, r=-1
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
.bf10JeffreysNumericallyIntegrate <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is a numerical approximation for .bf10JeffreysIntegrate, 
	# when it explodes
	# #
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#       2. check for r=1, r=-1
	#
	integrand <- function(rho){.jeffreysApproxH(n, r, rho)*priorRho(rho, alpha=1)}
	myResult <- integrate(integrand, -1, 1)
	return(myResult)
}


# 2.4. Two-sided fourth Bayes factor
.bf10JeffreysApprox <- function(n, r) {
	#Jeffreys' test for whether a correlation is zero or not
	#Jeffreys (1961), pp. 291 Eq. 14
	#
	myResult <- ((2*n-3)/pi)^(.5)*(1-r^2)^((n-4)/2)
	return(1/myResult)
}


# 3.0 One-sided preparation
#  OLD:
# .mPlusMarginalB <- function(n, r, alpha=1){
# 	hyperTerm1 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
# 										L=c(1/2, (n+2*alpha+3)/2), z=r^2)
# 	hyperTerm2 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
# 										L=c(3/2, (n+2*alpha+1)/2), z=r^2)
# 	hyperTerm3 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
# 										L=c(3/2, (n+2*alpha+3)/2), z=r^2)
# 	
# 	-(2^(1-2*alpha)*r)/((n+2*alpha-1)*(n+2*alpha+1)*beta(alpha, alpha))*
# 		(gamma(n/2)/gamma((n+1)/2))^2*
# 		((n*r)^2*hyperTerm1-n^2*(n+2*alpha+1)*hyperTerm2+2*n^3*r^2*
# 		 	hyperTerm3+(2*n^2-2*alpha*(1-2*n)+n-1))
# }

.mPlusMarginalB <- function(n, r, alpha=1){
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#       2. check for r=1, r=-1
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
	# Mathematica Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2014
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	#       2. check for r=1, r=-1
	#
	if ( any(is.na(r)) ){
		return(NaN)
	}
	
	if (alpha <= 1 && n > 2 && r>=1) {
		return(Inf)
	} else if (alpha <= 1 && n > 2 && r<=-1){
		return(0)
	}
	
	#TODO: Test for NAs
	myResult <- .bf10Exact(n, r, alpha) + .mPlusMarginalB(n, r, alpha)
	
	# TODO: use which
	if (myResult < 0){
		# Safe guard
		if (alpha==1){
			bf10EJ <- .bf10JeffreysIntegrate(n, r)
			mPlusEJ <- .mPlusMarginalBJeffreysIntegrate(n, r)
			bfPlus0EJ <- bf10EJ+mPlusEJ
			if (bfPlus0EJ < 0){
				return(NaN)
			} else {
				return(bfPlus0EJ)
			}
		} else {
			#TODO: Fix this for other alphas
			return("Negative")
		}		
	} else if (myResult >= 0){
		return(myResult)
	}
	return(NaN)
}

.bfMin0 <- function(n, r, alpha=1){
	# Mathematica Ly et al 2014
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
	#       2. check for r=1, r=-1
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



# 3.2 One-sided 2 BF

# 3.3 One-sided 3 BF

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