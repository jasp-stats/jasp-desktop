AncovaBayesian	 <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	if(is.null(base::options()$BFMaxModels)) base::options(BFMaxModels = 50000)
	if(is.null(base::options()$BFpretestIterations)) base::options(BFpretestIterations = 100)
	if(is.null(base::options()$BFapproxOptimizer)) base::options(BFapproxOptimizer = "optim")
	if(is.null(base::options()$BFapproxLimits)) base::options(BFapproxLimits = c(-15,15))
	if(is.null(base::options()$BFprogress)) base::options(BFprogress = interactive())
	if(is.null(base::options()$BFfactorsMax)) base::options(BFfactorsMax = 5) 

	numeric.variables <- c(unlist(options$covariates),unlist(options$dependent),unlist(options$wlsWeight)) #
	numeric.variables <- numeric.variables[numeric.variables != ""]
	
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors))
	factor.variables <- factor.variables[factor.variables != ""]
	
	all.variables <- c(numeric.variables, factor.variables)
	
	if (is.null(dataset)) {
		
		if (perform == "run") {
			
			dataset <-	.readDataSetToEnd(columns.as.numeric = numeric.variables,
				columns.as.factor = factor.variables,
				exclude.na.listwise = all.variables)
			
		} else {
			
			dataset <- .readDataSetHeader( columns.as.numeric = c(numeric.variables),
				columns.as.factor = c(factor.variables) )
		}
	}
	
	#### META
	results <- list()
	meta <- list()
	
	meta[[1]] <- list(name="posterior", type="table")
	meta[[2]] <- list(name="effect", type="table")
	
	results[[".meta"]] <- meta
	 
	jasp.callback <- function(...) as.integer(callback())
	#### Check for errors
	if (options$dependent != "" && length(options$modelTerms) > 0) {
		errorcheck <- .checkErrorsBayesianAnCova(options, dataset, perform)
		error.present <- errorcheck$error.present
		specific.error <- errorcheck$specific.error
	} else {
		error.present <- 0
		specific.error <- "none"
		errorcheck <- list(error.present = error.present, specific.error = specific.error)
	}

	#### Generate models
	if (options$dependent != "" && length(options$modelTerms) > 0) {
		tmp.models.list <- .generateBayesianAnCovaModels (options, errorcheck)

		model.formula <- tmp.models.list$model.formula
		null.name <- tmp.models.list$null.name
		terms.nuisance <- tmp.models.list$terms.nuisance
		terms.as.strings <- tmp.models.list$terms.as.strings
		all.models <- tmp.models.list$all.models
	} else {
		null.name <- as.character("Null model")
	}
		
	#####BUILD Bayesian ANOVA table ##########
	
	posterior <- list()
	posterior[["title"]] <- "Bayesian ANOVA: Model Comparison"
	posterior <- list(
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software].",
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
	if (options$dependent != "" && length(options$modelTerms) > 0 ){
		if ( perform == "run" && error.present == 0){
			if (length(terms.nuisance)> 0) {
				fields[[1]][[".footnotes"]] <- list(0)
				footrand <- paste(.unvf(terms.nuisance), collapse=" and ")
				footnotes <- list(paste("All models include", footrand))
				posterior[["footnotes"]] <- footnotes				
			}
			
			########ANALYSIS#######
			#This estimates the Bayes Factors. MM

			result <- .estimateBayesFactorBayesianAnCova( mode = "withmain", model.formula = model.formula, 
				dataset = dataset, terms.nuisance = terms.nuisance, options = options, jasp.callback = jasp.callback)

			withmain <- result$bf.object
			BFmain <- result$bf
			errormain <- result$error
			#####PREPARE VECTORS/MATRICES WITHOUT NUISANCE######
			# Prepare for model selection: lists of components of effects and models
			# Modelnames and effect names for table
			# Matrix of effects (of interest -without nuisance) vs models
			
			# Make a list of all models and their components, without nuisance terms, for later use
			
			modelsmain <- result$models
			
			a <- .modelnames(modelsmain,null.name,terms.nuisance)
			for (n in 1: length(a)){
			#This sets: n.mod, n.mod.0, models.tab, models, models.no.null
				assign(names(a)[n], a[[n]])
			}
			
			##PROCESS RESULTS
			BFmain.0	<- c(1,BFmain)
			# prior probability: 1/ number of models
			pprior <- .clean(1/n.mod.0)			
			# posterior probability: computed from Bayes factors (only with equal prior probability)
			
			warning.message <- NULL
			if( any(BFmain.0 == "NaN") || any(!is.finite(BFmain.0))){
				if (all(is.finite(BFmain.0))){ #containing nan, but not infinity
					tmp <- which(BFmain.0 != "NaN")
					posterior.probability <- rep(0,length(BFmain.0))
					posterior.probability[tmp] <- c(BFmain.0[tmp])/sum(BFmain.0[tmp])										
				} else if(all(BFmain.0 != "NaN")){ #containing infinity, but not nan
					tmp <- which(!is.finite(BFmain.0))
					posterior.probability <- rep(0,length(BFmain.0))
					posterior.probability[tmp] <- 1/length(tmp)		
				
				} else{ #containing both
					tmp <- which(BFmain.0 == "Inf")
					posterior.probability <- rep(0,length(BFmain.0))
					posterior.probability[tmp] <- 1/length(tmp)												
				}												
			} else { #containing neither
				posterior.probability <- c(BFmain.0)/sum(BFmain.0)				
			}
			BFmodels <-	(posterior.probability/(1-posterior.probability))/(pprior/(1-pprior))
			errormain.0 <- c(NA,errormain)

			#This produces the results. MM
			posterior.results <- .posteriorResultsBayesianAnCova (table = "run", n.mod.0 = n.mod.0, 
				posterior.probability = posterior.probability, BFmain.0 = BFmain.0, 
				errormain.0 = errormain.0, models.tab = models.tab, BFmodels = BFmodels, 
				options = options, pprior = pprior, terms.nuisance = terms.nuisance )
	
		} else {
			posterior.results <- .posteriorResultsBayesianAnCova (table = "init", null.name = null.name,
				all.models = all.models, terms.nuisance = terms.nuisance)
		}
				
	} else {
		posterior.results <- .posteriorResultsBayesianAnCova (table = "null")
	}

	#Show errormessages, if any. MM
	posterior <- .errorMessageBayesianAnCova(posterior, errorcheck, options, dataset)
	
	posterior[["data"]] <- posterior.results

	results[["posterior"]] <- posterior		

	###################
	###EFFECTS TABLE###
	##################
	
	
	if (options$outputEffects == TRUE) {
		
		effect <- list()
		effect[["title"]] <- "Bayesian ANOVA: Analysis of Effects"
		
		fields <- list(
			list(name="Effects", type="string"),
			list(name="P(incl)", type="number", format="sf:4;dp:3"),
			list(name="P(incl|data)", type="number", format="sf:4;dp:3"),
			list(name="BF<sub>Inclusion</sub>", type="number", format="sf:4;dp:3"),
			list(name="BF<sub>Backward</sub>", type="number", format="sf:4;dp:3"),
			list(name="% errorB", type="number", format="sf:4;dp:3"),
			list(name="BF<sub>Forward</sub>", type="number", format="sf:4;dp:3"),
			list(name="% errorF", type="number", format="sf:4;dp:3")
		)
		
		schema <- list(fields=fields)
		effect[["schema"]] <- schema
		
		if (options$dependent != "" && length(options$modelTerms) > 0) {
						
			### TABLE when non-run or error 
			if (perform == "init" || error.present > 0) {
			#This produces the effects table. MM
				effect.results <- .effectResultsBayesianAnCova(nmodels = 0, options = options)

				if (perform == "run" && error.present > 0) {
					effect[["error"]] <- list(errorType="badData")
				}
			} else {	
			#############PREPARE NAMES AND MATCHES 
				b <- .modelsandeffects(modelsmain, terms.nuisance, terms.as.strings)
				for (n in 1: length(b)){
				#This sets: n.comp.mod, comp.mod, n.eff, comp.eff, n.comp.eff
				# effects, effects.tab, match.eff.mod, match.eff.interactions
					assign(names(b)[n], b[[n]])
				}
				
			###########ANALYSIS###
				
			#This produces the effects table. MM
				effect.results <- .effectResultsBayesianAnCova(nmodels = length(models),
					posterior.probability = posterior.probability, n.eff = n.eff, n.mod.0 = n.mod.0, 
					match.eff.mod = match.eff.mod, match.eff.interactions = match.eff.interactions, 
					withmain =  withmain, effects = effects, errormain.0 = errormain.0, models.tab = models.tab, 
					pprior = pprior, BFmodels = BFmodels, BFmain.0 = BFmain.0, model.formula = model.formula, 
					dataset = dataset, terms.nuisance = terms.nuisance, jasp.callback = jasp.callback, 
					n.comp.eff = n.comp.eff, models = models, effects.tab = effects.tab, options = options)
				
			} 
				
			effect[["data"]] <- effect.results
			
		}
		results[["effect"]] <- effect	   
	} 
		
	results
	
}
	
#########################################################
##########SUPPORTING FUNCTIONS###########################
#########################################################
.checkErrorsBayesianAnCova <- function(options, dataset,perform)	{ 

	# Error messages 
	specific.error <- "none"
	error.present <- 0
	
	# error message when less than two levels are observed for a factor
	if(perform == "run"){
		for (fact in options$fixedFactors) {
			if (length(levels(dataset[[.v(fact)]])) < 2){
				error.present <- 1
				specific.error <- "levels"
			}
		}
	}
	if(error.present > 0)	
		return(list(error.present = error.present, specific.error = specific.error))
		
	# error message when less than two levels are observed for a factor after deleting NA's
	if(perform == "run"){
		for (fact in options$fixedFactors) {
			if (length(unique(dataset[[.v(fact)]])) < 2){
				error.present <- 1
				specific.error <- "observed levels"
			} 
		}
	}
	if(error.present > 0)
		return(list(error.present = error.present, specific.error = specific.error))
	
	# error message when interaction specified without main effect	
	for (term in options$modelTerms) {
		lmtc <- length(term$components)
		check <- 0
		if(lmtc > 1) {
			for (term2 in options$modelTerms) {
				if (length(term2$components) == 1){
					check <- sum(unlist(term2$components) == term$components) + check
				}
			}
			if ( check != lmtc) {
				error.present <- 1
				specific.error <- "interaction" 
			}
		}
	}
	if(error.present > 0)
		return(list(error.present = error.present, specific.error = specific.error))

	# error message when interaction specified without main effect in nuisance terms 	
	for (term in options$modelTerms) {
		if (term$isNuisance == TRUE) {
			lmtc <- length(term$components)
			check <- 0
			if(lmtc > 1) {
				for (term2 in options$modelTerms) {
					if (length(term2$components) == 1 && term2$isNuisance == TRUE){
						check <- sum(unlist(term2$components) == term$components) + check
					}
				}
				if ( check != lmtc) {
					error.present <- 1
					specific.error <- "interaction nuisance" 
				}
			}
		}
	}
	if(error.present > 0)
		return(list(error.present = error.present, specific.error = specific.error))
	
	# error message when all variables are specified as nuisance 	
	no.nuisance <- 0
	for (term in options$modelTerms) {
		if (term$isNuisance == TRUE) { 
			no.nuisance <- no.nuisance + 1
		}
	}
	if (no.nuisance == length(options$modelTerms)) {
		error.present <- 1
		specific.error <- "all nuisance"
	}
	if(error.present > 0)
		return(list(error.present = error.present, specific.error = specific.error))

	# error message when the number of effects, p, is at least as large as the number of observations, n, minus one.
	if( perform == "run"){
		n <- length(dataset[[.v(options$dependent)]])
		p <- length(options$modelTerms)
		if(p >= (n-1)){
			error.present <- 1
			specific.error <- "p>=(n-1)"
		}
	}
	if(error.present > 0)
		return(list(error.present = error.present, specific.error = specific.error))
	

	# error message when higher order effects are included (in model builder) without the lower order effects 
	nmbr.mt <- length(options$modelTerms)
	lngth.mt <- sapply(options$modelTerms,function(term) length(term))
	mx.lngth.mt <- max(lngth.mt)

	if(mx.lngth.mt > 1){	
		trms <- sapply(options$modelTerms,function(terms){	
			cmpnnts <- paste(unlist(terms$components), collapse=":")
		    as.character(sapply(cmpnnts, stringr::str_trim, simplify=FALSE))
		})	
		for(lngth in 2:mx.lngth.mt){
			index.n <- which(lngth.mt == lngth)
			if(length(index.n) > 0 ){
				for(i in index.n){		
					terms <- unlist(options$modelTerms[[i]]$components)
					smpl.trms <- utils::combn(terms,(lngth-1),simplify = FALSE)
					chck.trms <- sapply(smpl.trms,function(simple.terms){					
						cmpnnts <- paste(unlist(simple.terms), collapse=":")
				    	cmpnnts <- as.character(sapply(cmpnnts, stringr::str_trim, simplify=FALSE))
						cmpnnts%in%trms
					})				
					if(sum(chck.trms) != choose(lngth,lngth-1)){
						error.present <- 1
						specific.error <- "lower order effects"
					}
					if(error.present > 0) 
						break 
				}
			} else{
				error.present <- 1
				specific.error <- "lower order effects"			
			}		
			if(error.present > 0)
				break 
		}
	}	
	list(error.present = error.present, specific.error = specific.error)

}

.modelnames <- function(modelsmain, null.name, terms.nuisance) {
	
	n.mod <- length(modelsmain)
	
	models.tab <- null.name
	models <- .vf(null.name) 
	models.no.null <- c()
	for (i in 1:n.mod) { 
		if (length(terms.nuisance)	== 0) {
			models.tab <-	 c(models.tab,as.character(.unvf(modelsmain[i]))) 
			models <-	 c(models,as.character(modelsmain[i])) 
			models.no.null <- c(models.no.null, as.character(modelsmain[i]))		
		} else {
			nuisance.plus	<- paste(c("", terms.nuisance), collapse=" + ") 
			mi <- as.character(modelsmain[i])
			mi2	<- gsub(nuisance.plus, "", mi, fixed = TRUE)
			models <- c(models, mi2) 
			models.tab <- c(models.tab, .unvf(mi2)) 
			models.no.null <- c(models.no.null, mi2)
		}
	}
	n.mod.0 <- n.mod + 1
	list(n.mod=n.mod, n.mod.0=n.mod.0, models.tab = models.tab, models = models,models.no.null = models.no.null)
}



.modelsandeffects <- function(modelsmain, terms.nuisance, terms.as.strings) {
	
	n.mod <- length(modelsmain)
	n.comp.mod <- rep(0,n.mod)
	
	comp.mod <-	vector("list",n.mod)
	for (i in 1:n.mod) { 
		comp.mod[[i]] <- .decompose(modelsmain[i])[[1]][[1]] # list of components of each model
		n.comp.mod[i] <- length(comp.mod[[i]]) # number of components of each model
		
		if (length(terms.nuisance > 0 )) {	# take the nuisance terms out of the model specification
			a <- .decompose(modelsmain[i])[[1]][[1]]	 
			comp.mod[[i]] <-	 a[-which(is.element(a, terms.nuisance))]
			n.comp.mod[i] <- length(comp.mod[[i]])
		}
	}
	
	# Create model names and names for table without nuisance effects
	#!!!! watchout,	gave models a different name!!!!!!
	#! models includes the null model, n.mod and comp.mod and n.comp.mod not
	
	# Make a list of all effects and their components	
	effects <- terms.as.strings
	if (length(terms.nuisance) > 0 ) {
	effects <- terms.as.strings[-which(terms.as.strings %in% terms.nuisance)] 
	}
	n.eff<- length(effects)
	comp.eff <- list()
	n.comp.eff <- c()
	for (n1 in 1:n.eff) {
		comp.eff[[n1]] <- unlist(.decompose(effects[n1]))
		n.comp.eff[n1] <- length(comp.eff[[n1]])			 
	}
	effects.tab <-	.unvf(effects)
	
	
	# create a matrix matching effects of interest to corresponding models
	match.eff.mod <- matrix(0,n.eff,n.mod)
	for (n1 in 1:n.eff) {
		for (n2 in 1:n.mod) {
			abs <- 0
			for (j in 1:n.comp.mod[n2]) {
				if (length(comp.mod[[n2]][[j]]) == n.comp.eff[n1]) {
					# sort to make sure to get the right order in interaction effects
					A <- paste(sort(comp.mod[[n2]][[j]]), sep="", collapse="")
					B <- paste(sort(comp.eff[[n1]]), sep="", collapse="")	
					abs	<- abs + (A==B) # if the effect matches give indicator 
				}
			}
			match.eff.mod[n1,n2] <- abs
		}
	}
	
	# include a column for the null model 
	match.eff.mod <- cbind(rep(0,n.eff),match.eff.mod)
	
	
	#################
	
	match.eff.interactions <- matrix(0,n.eff,n.mod)
	#match 2 gives for every effect the models which include an interaction in which the effect is included (lower order)
	for(n1 in 1:n.eff) { # for each effect 
		for(n2 in 1:n.mod) {
			comp.length <- sapply(comp.mod[[n2]], length) # length of each model component 
			# which model components are longer than effect[n1] 
			interaction.possible <- which(comp.length>n.comp.eff[n1])
			if (length(interaction.possible) > 0) {
				TT <-	matrix(0,n.comp.eff[n1],length(interaction.possible)) 
				for (ii in 1:n.comp.eff[n1]) {
					for (jj in 1:length(interaction.possible)) {
						# which model components contain components of effect[n1]
						TT[ii,] <-	length(grep(comp.eff[[n1]][ii],comp.mod[[n2]][[interaction.possible[jj] ]]))
					}	
				}
			
				# which model components contain ALL components of effect[n1]				
				if (sum(colSums(TT) == n.comp.eff[n1]) > 0) {
					match.eff.interactions[n1,n2] <- 1
				}
			}
		}
	}
	
	match.eff.interactions <- cbind(rep(0,n.eff),match.eff.interactions)
	
		################
	
	
	list(n.comp.mod = n.comp.mod, comp.mod = comp.mod, n.eff=n.eff, 
			 comp.eff = comp.eff, n.comp.eff = n.comp.eff, effects=effects, 
			 effects.tab = effects.tab, match.eff.mod=match.eff.mod,
			 match.eff.interactions=match.eff.interactions)
}


.errorMessageBayesianAnCova <- function(table, errorcheck, options, dataset){
	specific.error <- errorcheck$specific.error
	
	if (specific.error == "interaction nuisance"){
		table[["error"]] <- list(errorType="badData", errorMessage="The main effects of variables should be specified as nuisance whenever their interaction is specified as nuisance")
	}
	if (specific.error == "interaction"){
		table[["error"]] <- list(errorType="badData", errorMessage="The main effects of variables should be included whenever their interaction is included")
	}
	
	if (specific.error == "levels"){
		table[["error"]] <- list(errorType="badData", errorMessage="Factors must have 2 or more levels")
	}
	
	if (specific.error == "observed levels"){
		observed.levels <- vector(length = length(options$fixedFactors))
		counter <- 0
		for (fact in options$fixedFactors) {
			counter <- counter + 1
			observed.levels[counter] <- length(unique(dataset[[.v(fact)]]))
		}
		factor.names <- unlist(options$fixedFactors[which(observed.levels < 2)])
		if(length(fact) > 1){
			factor.names <- paste(factor.names,collapse=", ")
		}
		table[["error"]] <- list(errorType="badData", errorMessage=paste("Factor(s): ",factor.names," contain(s) less than two levels. (Possibly only after rows with missing values are excluded)",sep=""))
	}
	
	if (specific.error =="all nuisance"){
		table[["error"]] <- list(errorType="badData", errorMessage="BayesFactor is undefined -- all effects are specified as nuisance")
	}
	if (specific.error == "p>=(n-1)"){
		table[["error"]] <- list(errorType="badData", errorMessage="There needs to be at least one more (valid) observation than there are effects specified in the model")
	}
	
	return(table)
}

.generateBayesianAnCovaModels <- function(options, errorcheck){
	
	terms.as.strings <- c()
	terms.nuisance <- c()
	null.name <- as.character("Null model")
	
	if ( errorcheck$error.present == 0){
		
		for (term in options$modelTerms) {
			term.as.string <- paste(.v(term$components), collapse=":")
			terms.as.strings <- c(terms.as.strings, term.as.string)
		}
		
		for (term in options$modelTerms) {
			if(term$isNuisance == TRUE) {
				term.nuisance <- paste(.v(term$components), collapse=":")
				terms.nuisance <- c(terms.nuisance, term.nuisance)
			}
		}
		
		if (length(terms.nuisance)> 0) {
			nuisance.plus	<- paste(.unvf(terms.nuisance), collapse=" + ") #
			null.name <- as.character(paste("Null model (incl. ",	nuisance.plus, ")", sep = "" ) )#
		}
	} else {
		# If interactions are specified without corresponding main effects
		# all.models is not running: table should be made alternatively
		for (term in options$modelTerms) {
			if (length(term$components) == 1){
				term.as.string <- paste(.v(term$components), collapse=":")
				terms.as.strings <- c(terms.as.strings, term.as.string)
			}
		}
		
		for (term in options$modelTerms) {
			if(term$isNuisance == TRUE) {
				if (length(term$components) ==1){
					term.nuisance <- paste(.v(term$components), collapse=":")
					terms.nuisance <- c(terms.nuisance, term.nuisance)
				}
			}
		}
		
		if (length(terms.nuisance)> 0) {
			nuisance.plus	<- paste(.unvf(terms.nuisance), collapse=" + ") #
			null.name <- as.character(paste("Null model (incl. ",	nuisance.plus, ")", sep = "" ) )#
		}
	}
	
	rhs <- paste(terms.as.strings, collapse="+")
	model.def <- paste(.v(options$dependent), "~", rhs)
	model.formula <- as.formula(model.def)
	
	all.models <- BayesFactor::enumerateGeneralModels(model.formula,
	whichModels = "withmain", neverExclude = paste("^",terms.nuisance,"$", sep = ""))
	
	return.list<- list(model.formula = model.formula, terms.nuisance = terms.nuisance,
		terms.as.strings = terms.as.strings, null.name = null.name,
		all.models = all.models)
	return(return.list)
}

.estimateBayesFactorBayesianAnCova <- function( mode, model.formula, dataset, terms.nuisance, options, jasp.callback ){
	
	if (mode != "bottom" & mode != "withmain")
		stop( 'Mode is not correctly set for generalTestBF. Should be "bottom" or "withmain".' )

	result <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = mode, 
		neverExclude = paste("^",terms.nuisance,"$", sep = ""), whichRandom = .v(options$randomFactors),
		progress=FALSE, callback=jasp.callback)

	if (length(terms.nuisance) > 0) {
		ind.nui <- which(names(result)$numerator == paste(terms.nuisance, collapse=" + ")) 
		result <- result[-ind.nui]/result[ind.nui]
	} 

	bf <- as.numeric(exp(result@bayesFactor$bf))
	error <- as.numeric(result@bayesFactor$error)
	models <- names(result)$numerator
	
	return(list(bf = bf, error = error, models = models, bf.object = result))
}

.posteriorResultsBayesianAnCova <- function( table = "null" , null.name = "Null model", n.mod.0 = NULL, 
	posterior.probability = NULL, BFmain.0 = NULL, errormain.0 = NULL, models.tab = NULL, 
	BFmodels = NULL, options = NULL, pprior = NULL, terms.nuisance = NULL, all.models = NULL){

	if ( table != "run" & table != "init" & table != "null")
		stop('Mode is not correctly set for posterior.results. Should be "run", "init" or "null".' )

	posterior.results <- list()	
	if ( table == "run") {  
		for (n in 1:n.mod.0) {
						
			ppost <- .clean(posterior.probability[n])
			BF <- .clean(BFmain.0[n] )
			error <- .clean(errormain.0[n]*100)
			if (n==1){
				error <- " "
			}
			
			model.name <- models.tab[n]

			if(n > 1 && length(terms.nuisance) > 0) {
				model.components <- unlist(strsplit(.vf(model.name), split = "+", fixed = TRUE))
				model.components <- sapply(model.components, stringr::str_trim, simplify=FALSE)
				for(i in length(model.components):1){
					if(model.components[i] %in% terms.nuisance ){
						model.components <- model.components[-i]
					} 
				}
				model.name <- .unvf(paste(model.components, collapse=" + "))
			}

			
			BFM <- .clean(BFmodels[n])
			if (length(options$randomFactors) == 0 && length(options$covariates) == 0) {
				r <- list("Models"= model.name,"P(M)" = pprior, "P(M|data)"=ppost,
					"BFM" = BFM,"BF10"=BF, "% error"=error)
			} else {
				r <- list("Models"= model.name,"P(M)" = pprior, "P(M|data)"=ppost,
					"BFM" = BFM,"BF10"=BF, "% error"=error, ".footnotes"=list("p"=list(1)))
			}
			posterior.results[[length(posterior.results)+1]] <- r
		}
	} else if (table == "init") {

		posterior.results[[1]] <- list("Models"=null.name)
		
		for (model in all.models) {
			model.name <- as.character(model)[[3]]
			if(length(terms.nuisance) > 0) {
				model.components <- unlist(strsplit(model.name, split = "+", fixed = TRUE))
				model.components <- sapply(model.components, stringr::str_trim, simplify=FALSE)
				for(i in length(model.components):1){
					if(model.components[i] %in% terms.nuisance ){
						model.components <- model.components[-i]
					} 
				}
				model.name <- paste(model.components, collapse=" + ")
			}
			model.name <- .unvf(model.name)
			if(length(model.name) == 1){
				posterior.results[[length(posterior.results)+1]] <- list("Models" = model.name)
			}
		}
	} else if (table == "null"){
		posterior.results[[1]] <- list("Models" = null.name)	
	}

	return(posterior.results)
}

.effectResultsBayesianAnCova <- function( nmodels = 0, options = NULL, posterior.probability = NULL, n.eff = NULL, 
	n.mod.0 = NULL, match.eff.mod = NULL, match.eff.interactions = NULL, withmain = NULL, effects = NULL, models.tab = NULL, 
	pprior  = NULL, BFmodels  = NULL, BFmain.0  = NULL, errormain.0 = NULL, model.formula = NULL, 
	dataset = NULL, terms.nuisance = NULL, jasp.callback = NULL, n.comp.eff = NULL, models = NULL, effects.tab = NULL){
	if ( nmodels < 0 || round(nmodels,0) != nmodels )
		stop('Number of models should be a positive integer.')
	
	effect.results <- list()
	if(nmodels > 2){
	#Null model and at least two more
				
	# 1. Bottom up effects	(Forward analysis)

		result <- .estimateBayesFactorBayesianAnCova( mode = "bottom", model.formula = model.formula, dataset = dataset, terms.nuisance = terms.nuisance, options = options, jasp.callback = jasp.callback)
		bottom <- result$bf.object
		BFbot <- result$bf
		errorbot <- result$error					
	# 2. Inclusion probabilities and Bayes factors 
		inclusion.probabilities <- rowSums(match.eff.mod * matrix(rep(posterior.probability[], each = n.eff) ,n.eff,n.mod.0))
		prior.probabilities <- rowSums(match.eff.mod)/n.mod.0
		Bayesfactor.inclusion <- (inclusion.probabilities/(1-inclusion.probabilities))/(prior.probabilities/(1- prior.probabilities))
			
	#3. Backward probabilities (use this for more elaborate model)
			
	# Combine match.eff.mod and match.eff.interactions 
	# to select the Full and Reduced model to compare
		complexity.models <- colSums(match.eff.mod) # complexity (no. terms) of each model
		n.include.eff <- rowSums(match.eff.mod)
		Full <- rep(0, n.eff)
		Reduced <-	rep(0, n.eff)
		BF.Backward <-	rep(0, n.eff)
		error.Backward <-	rep(0, n.eff)

		for (e in 1:n.eff) {
			if (n.include.eff[e] == 1) {
			#If the effect only in one model: Should compare with null model
				Full <-	which(match.eff.mod[e,] == 1)
				Reduced <-	which(complexity.models == max(complexity.models[-Full]))					
			} else {
			#Model has to meet the condition that the effect is included 
			#Model shouldnt include a higher order interaction with the effect 
				mno1 <- which(match.eff.interactions [e,] == 0)
				mno2 <- which(match.eff.mod[e,]	> 0)
				selection <- intersect(mno1,mno2)
			# Look for the most complex model which meets these criteria
				Full <- selection[which(complexity.models[selection]==max(complexity.models[selection]))]
			# Look for the most complex model without the effect
				mno3 <- which(match.eff.mod[e,] == 0)
				Reduced <- mno3[which(complexity.models[mno3]==max(complexity.models[mno3]))]
				
			}
			if(Reduced > 1){
				out <- withmain[Full-1]/withmain[Reduced-1]
				print(out)
			} else { 
			#Most complex model without effect may be the null model (Reduced == 1). MM
			#Should then compare against the null
				out <- withmain[Full-1]				
			}
			BF.Backward <- as.numeric(exp(out@bayesFactor$bf))
			error.Backward <- as.numeric(exp(out@bayesFactor$error))
												
			BFbottom <- .clean(BFbot[e])
			errorbottom <- .clean(errorbot[e]*100)
			if(n.comp.eff[e] == 1) {
				ind.model.e <- which(models == effects[e])
				BFbottom <- .clean(BFmain.0[ind.model.e])
				errorbottom <- .clean(errormain.0[ind.model.e]*100)
			}
				
			r <- list("Effects" = as.character(effects.tab[e]),
				"P(incl)" = .clean(prior.probabilities[e]), 
				"P(incl|data)" = .clean(inclusion.probabilities[e]),
				"BF<sub>Inclusion</sub>" = .clean(Bayesfactor.inclusion[e]),
				"BF<sub>Backward</sub>" = .clean(BF.Backward), 
				"% errorB" = .clean(error.Backward),
				"BF<sub>Forward</sub>" = BFbottom, 
				"% errorF" = errorbottom)
				
			effect.results[[length(effect.results)+1]] <- r
		}   
	} else if (nmodels == 2){
	#Null model and one more
		r <- list("Effects" = as.character(models.tab[2]),
			"P(incl)" = .clean(pprior), 
			"P(incl|data)" = .clean(posterior.probability[2]),
			"BF<sub>Inclusion</sub>" = .clean(BFmodels[2]),
			"BF<sub>Backward</sub>" = "",
			"% errorB" = "",
			"BF<sub>Forward</sub>" = .clean(BFmain.0[2]),
			 "% errorF" = .clean(errormain.0[2]*100))
		
		effect.results[[length(effect.results)+1]] <- r
		
	} else if (nmodels == 0){
		for (term in options$modelTerms) {
			if(term$isNuisance ==FALSE){
				effect.results[[length(effect.results)+1]] <- list("Effects"=paste(term$components, collapse=":"))
			}				
		}   
	}
	return(effect.results)
}	
	
	

