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
	}
	results <- list()
	
	meta <- list(
		list(name="correlations", type="table"))
	results[[".meta"]] <- meta
	results[["correlations"]] <- 
		.correlationTableBayesian(dataset, perform, 
								  variables=options$variables)
	return(results)
}

.correlationTableBayesian <- function(dataset, perform, variables) {
	# This will be returned
	correlation.table <- list()
	correlation.table[["title"]] <- "Correlation Table"
	
	# Describe column names to the returned object
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	v.c <- length(variables)  # variable count
	
	if (v.c > 0) {
		column.names <- c()
		
		for (variable.name in variables) {
			column.names[[length(column.names)+1]] <- variable.name
			fields[[length(fields)+1]] <- list(name=variable.name, 
											   title=variable.name, 
											   type="number", format="dp:3")
		}
		
		for (i in 1:v.c) {
			row <- list()
			row.footnotes <- list()
			variable.name <- variables[[i]]
			
			for (j in .seqx(1, i-1)) {         
				# fill in blanks in table lower left-hand off diaganols 
				variable.2.name <- variables[[j]]
				row[[variable.2.name]] <- ""
			}
			
			row[[".variable"]] <- variable.name

			row[[variable.name]] <- "\u2014" # em-dash

			for (j in .seqx(i+1, v.c)) {
				# fill in blanks in table upper left-hand off diaganols 

				variable.2.name <- variables[[j]]
	
				if (perform == "run") {
					someCor <- cor(dataset[[ .v(variable.name) ]], 
								   dataset[[ .v(variable.2.name) ]])
					someLength <- dataset[[ .v(variable.name) ]]
					
					# TODO: here the analysis for the two-sided tests with four cases 

					result <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method="pearson", alternative="two.sided")
					
					row[[variable.2.name]] <- .clean(unname(result$estimate))
				
				} else {
			
					row[[variable.2.name]] <- "."
				}
			}
	
			rows[[i]] <- row
		}
	}
	
	schema <- list(fields=fields)
	
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	
	return(correlation.table)
}


# 0. Prior specification
.priorRho <- function(rho, alpha=1) {
	return(2^(1-2*alpha)/(beta(alpha, alpha))*(1-rho^2)^(alpha-1))
}

.priorRhoPlus <- function(rho, alpha=1) {
	nonNegativeIndex <- rho >=0
	lessThanOneIndex <- rho <=1 
	valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
	myResult <- rho*0
	
	myResult[valueIndex] <- 2*priorRho(rho[valueIndex], alpha)
	return(myResult)
}

# 1.0. Built-up for likelihood functions
.myAFunction <- function(n, r, rho) {
	hyperTerm <- hypergeo::hypergeo(((n-1)/2), ((n-1)/2), (1/2), (r*rho)^2)
	myResult <- (1-rho^2)^((n-1)/2)*hyperTerm
	return(Re(myResult))
}

.myBFunction <- function(n, r, rho) {
	hyperTerm1 <- hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2)
	hyperTerm2 <- hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2)
	
	myResult <- 2^(-1)*(gamma(n/2)/gamma((n+1)/2))^2*(1-rho^2)^((n-1)/2)*
		((1-2*n*(r*rho)^2)/(r*rho)*hyperTerm1 -(1-(r*rho)^2)/(r*rho)*hyperTerm2)
	return(Re(myResult))
}

.myHFunction <- function(n, r, rho) {
	myResult <- .myAFunction(n, r, rho) + .myBFunction(n, r, rho)
	return(Re(myResult))
}

.jeffreysApproxH <- function(n, r, rho) {
	return(((1 - rho^(2))^(0.5*(n - 1)))/((1 - rho*r)^(n - 1 - 0.5)))
}

# 1.1 Explicit marginal likelihood functions
.m0MarginalLikelihood <- function(s, t, n) {
	return(1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*(gamma(0.5*(n-1)))^2)
}

.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
	return(.m0MarginalLikelihood(s, t, n)*
		   	(.myAFunction(n, r, rho)+.myBFunction(n, r, rho)))
}

# TODO: So far only for \rho_{0}=0
#
# 
# .bf10Exact <- function(n, r, alpha=1) {
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
	#
	hyperTerm <- hypergeo::hypergeo((2*n-3)/4, (2*n-1)/4, (n+2)/2, r^2)
	myResult <- sqrt(pi)/2*gamma((n+1)/2)/gamma((n+2)/2)*hyperTerm
	realResult <- Re(myResult)
	return(realResult)
}

.bf10JeffreysIntegrate2 <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	#
	logHyperTerm <- log(hypergeo::hypergeo((2*n-3)/4, (2*n-1)/4, (n+2)/2, r^2))
	myLogResult <- 0.5*log(pi)-log(2)+lgamma((n+1)/2)-
		lgamma((n+2)/2)+logHyperTerm
	realResult <- exp(Re(myLogResult))
	return(realResult)
}


# 2.3 Two-sided third Bayes factor
.bf10JeffreysNumericallyIntegrate <- function(n, r) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is a numerical approximation for .bf10JeffreysIntegrate, 
	# when it explodes
	# TODO: 
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
.mPlusMarginalB <- function(n, r, alpha=1){
	hyperTerm1 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
										L=c(1/2, (n+2*alpha+3)/2), z=r^2)
	hyperTerm2 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
										L=c(3/2, (n+2*alpha+1)/2), z=r^2)
	hyperTerm3 <- hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2), 
										L=c(3/2, (n+2*alpha+3)/2), z=r^2)
	
	-(2^(1-2*alpha)*r)/((n+2*alpha-1)*(n+2*alpha+1)*beta(alpha, alpha))*
		(gamma(n/2)/gamma((n+1)/2))^2*
		((n*r)^2*hyperTerm1-n^2*(n+2*alpha+1)*hyperTerm2+2*n^3*r^2*
		 	hyperTerm3+(2*n^2-2*alpha*(1-2*n)+n-1))
}

# 3.1 One-sided 1 BF
.bfPlus0 <- function(n, r, alpha=1){
	# Mathematica Ly et al 2014
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If alpha = 1 then uniform prior on rho
	# bf positive vs null
	myResult <- .bf10Exact(n, r, alpha) + .mPlusMarginalB(n, r, alpha)
	return(Re(myResult))
}

# 3.2 One-sided 2 BF

# 3.3 One-sided 3 BF

# 4.0 Posteriors to graph TODO: we have to think about this, different 
# results, thus, also switching of the illustrations? 
#
# 4.1 Two-sided 
.posteriorRho <- function(n, r, rho, alpha=1){
	Re((1/.bf10Exact(n,r))*.myHFunction(n, r, rho)*.priorRho(rho, alpha))
}

.posteriorRhoPlus <- function(n, r, rho, alpha=1){
	Re((1/.bfPlus0(n, r))*.myHFunction(n, r, rho)*.priorRhoPlus(rho, alpha))
}