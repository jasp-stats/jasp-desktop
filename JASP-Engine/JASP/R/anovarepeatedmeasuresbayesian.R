AnovaRepeatedMeasuresBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	# The idea is to use Bayesian Ancova to cover all Bayesian linear models, including this one.
	# The way that this model is set-up (options, dataset) in Jasp is different than is expected by bayesian Ancova.
	# Therefore, we need to rebuild the list of options and the dataset to work under Bayesian Ancova.

	#These options are currently part of Bayesian Ancova, but will not be used (for now):
	options$covariates <- NULL
	options$outputEffects <- FALSE
	options$posteriorDistributions <- FALSE
	options$posteriorEstimates <- FALSE
	options$effectsStepwise <- FALSE
	#These are the options that we need to build:
	#options$dependent
	#options$fixedFactors  		These are the between subject factors as well as the RM factors.
	#options$randomFactors 		This is simply the subject number in RM

	ready <- (("" %in% options$repeatedMeasuresCells) == FALSE)
	if (ready) {
		rm.vars <- options$repeatedMeasuresCells
	} else {
		rm.vars <- c()
	}

	if (ready && is.null(dataset)) {
		bt.vars <- options$betweenSubjectFactors
		rm.factors <- options$repeatedMeasuresFactors

		all.variables <- c(bt.vars,rm.vars)
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.numeric = rm.vars,
				columns.as.factor = bt.vars,
				exclude.na.listwise = all.variables)
			dataset <- .shortToLong(dataset, rm.factors, rm.vars, bt.vars)

#Jonathon
#prefix F in .shortToLong()
			data.names <- names(dataset)
			XorF <- substr(data.names,1,1)
			if(any(XorF == "F")){
				data.names[XorF == "F"] <- paste("X",substr(data.names[XorF == "F"],2,1000),sep="")
			}
			names(dataset) <- data.names
#end

		} else {
			n.factors <- length(rm.factors) + length(bt.vars)
			dataset <- data.frame(dependent = numeric(), subject = factor(levels=2))
			if(length(rm.factors)>0){
				for(i in 1:length(rm.factors)){
					dataset <- cbind(dataset,factor(levels=2))
					names(dataset)[dim(dataset)[2]] <- .v(unlist(rm.factors[[i]]$name))
				}
			}
			if(length(bt.vars)>0){
				bt.names <- .v(unlist(bt.vars))
				for(i in 1:length(bt.vars)){
					dataset <- cbind(dataset,factor(levels=2))
					names(dataset)[dim(dataset)[2]] <- bt.names[i]
				}
			}
		}

		#specify relevant options
		options$dependent <- "dependent"
		options$randomFactors <- "subject"
		variable.names <- names(dataset)
		i <- which(variable.names == "dependent")
		j <- which(variable.names == "subject")
		variable.names[i] <- .v("dependent")
		variable.names[j] <- .v("subject")
		names(dataset) <- variable.names
		variable.names <- variable.names[-c(i,j)]
		if(length(variable.names) > 0)
			options$fixedFactors <- as.list(.unv(variable.names))

#Jonathon:
#This creates modelTerms (main effects) when its not done already..
		if(length(options$modelTerms) == 0){
			modelTerms <- list()
			if(length(variable.names) > 0){
				for(i in 1:length(variable.names)){
					modelTerms[[length(modelTerms)+1]] <- list(components = .unv(variable.names[i]), isNuisance = FALSE)
				}
			}
			options$modelTerms <- modelTerms
		}
		options$modelTerms[[length(options$modelTerms) + 1]] <- list(components = "subject", isNuisance = TRUE)		
#end
		results <- AncovaBayesian(dataset = dataset, options = options, perform = perform, callback = callback)

		results$results$title <- "Bayesian Repeated Measures ANOVA"		
		results$results$posterior$title <- "Model Comparison"
		if(options$outputEffects == TRUE){
			results$results$effect$title <- "Analysis of Effects"
		}
	} else {
		#Create empty table
		results <- list()
		meta <- list()
		meta[[1]] <- list(name="title", type="title")
		meta[[2]] <- list(name="posterior", type="table")
		meta[[3]] <- list(name="effect", type="table")
		results[[".meta"]] <- meta
		results[["title"]] <- "Bayesian Repeated Measures ANOVA"

		posterior <- list()
		posterior[["title"]] <- "Model Comparison"
		posterior[["citation"]] <- list(
			"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374.")

		fields <- list(
			list(name="Models", type="string"),
			list(name="P(M)", type="number", format="sf:4;dp:3"),
			list(name="P(M|data)", type="number", format="sf:4;dp:3"),
			list(name="BFM", type="number", format="sf:4;dp:3", title="BF<sub>M</sub>"),
			list(name="BF10", type="number", format="sf:4;dp:3", title="BF<sub>10</sub>"),
			list(name="% error", type="number", format="sf:4;dp:3")
		)

		schema <- list(fields=fields)
		posterior[["schema"]] <- schema
		posterior[["data"]] <- list(list("Models" = "Null model"))

		results[["posterior"]] <- posterior

		if(options$outputEffects == TRUE){
			effect <- list()
			effect[["title"]] <- "Analysis of Effects"
			effect[["citation"]] <- list(
				"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
				"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374.")

			fields <- list(
				list(name="Effects", type="string"),
				list(name="P(incl)", type="number", format="sf:4;dp:3"),
				list(name="P(incl|data)", type="number", format="sf:4;dp:3"),
				list(name="BF<sub>Inclusion</sub>", type="number", format="sf:4;dp:3")
			)

			schema <- list(fields=fields)
			effect[["schema"]] <- schema
			results[["effect"]] <- effect
		}
	}
	results
}