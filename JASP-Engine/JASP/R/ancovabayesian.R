AncovaBayesian     <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
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
            
            dataset <-	.readDataSetToEnd(columns.as.numeric = numeric.variables, columns.as.factor = factor.variables, exclude.na.listwise = all.variables)
            
        } else {
            
            dataset <- .readDataSetHeader( columns.as.numeric = c(numeric.variables), columns.as.factor = c(factor.variables) )
        }
    }

	results <- list()
    
    #### META

    meta <- list()
    
    meta[[1]] <- list(name="posterior", type="table")
    meta[[2]] <- list(name="effect", type="table")
    
    results[[".meta"]] <- meta
     
    jasp.callback <- function(...) as.integer(callback())
	
	#### Check for errors
	error.present <- 0
	if (options$dependent != "" && length(options$modelTerms) > 0) {
		errorcheck <- .checkerrors(options, dataset, perform)
		
		error.present <- errorcheck$error.present
		
		specific.error <- errorcheck$specific.error
	}

	####GENERAL SET UP FOR MODELS#####
	
	null.name <- as.character(c("Null model"))

	if (options$dependent != "" && length(options$modelTerms) > 0) {
		
		terms <- options$modelTerms
		terms.as.strings <- c()
		
		
		if ( error.present == 0){
	
			for (term in options$modelTerms) {
				term.as.string <- paste(.v(term$components), collapse=":")
				terms.as.strings <- c(terms.as.strings, term.as.string)
			}
    
			terms.nuisance <- c()
			for (term in options$modelTerms) {
				if(term$isNuisance == TRUE) {
					term.nuisance <- paste(.v(term$components), collapse=":")
					terms.nuisance <- c(terms.nuisance, term.nuisance)
				}
			}
    
			ind.random <- length(options$randomFactors)
			ind.cov <- length(options$covariates)
			random <- .v(options$randomFactors)
			covariate <- .v(options$covariates)
 
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
	
			terms.nuisance <- c()
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
		
		all.models <- BayesFactor::enumerateGeneralModels(model.formula, whichModels="withmain", neverExclude=paste("^",terms.nuisance,"$", sep = ""))
	}
		
		
    #####BUILD Bayesian ANOVA table ##########
	
    posterior <- list()
    posterior[["title"]] <- "Bayesian ANOVA: Model Comparison"
	
    fields <- list(
        list(name="Models", type="string"),
        list(name="P(M)", type="number", format="sf:4;dp:3"),
        list(name="P(M|Y)", type="number", format="sf:4;dp:3"),
        list(name="BFM", type="number", format="sf:4;dp:3", title="BF<sub>M</sub>"),
        list(name="BF10", type="number", format="sf:4;dp:3", title="BF<sub>10</sub>"),
        list(name="% error", type="number", format="sf:4;dp:3")
    )
	
    schema <- list(fields=fields)
    posterior[["schema"]] <- schema
	
	posterior.results <- list()
	
	if (options$dependent != "" && length(options$modelTerms) > 0 ){
		if ( perform == "run" && error.present == 0){
			#build run table
		
			if (length(terms.nuisance)> 0) {
				fields[[1]][[".footnotes"]] <- list(0)
			}
			
			#######ANALYSIS#######
			withmain <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "withmain", neverExclude =    paste("^",terms.nuisance,"$", sep = ""), whichRandom = random,    progress=FALSE, callback=jasp.callback)
		
#something is broken in callback with random effects included: needs to be repaired

			if (length(terms.nuisance) > 0) {
				indw.nui <- which(names(withmain)$numerator == paste(terms.nuisance, collapse=" + "))
				withmain <- withmain[-indw.nui]/withmain[indw.nui]
			}
			
			BFmain <- as.numeric(exp(withmain@bayesFactor$bf))
			errormain <- as.numeric(withmain@bayesFactor$error)
			
			#####PREPARE VECTORS/MATRICES WITHOUT NUISANCE######
			# Prepare for model selection: lists of components of effects and models
			# Modelnames and effect names for table
			# Matrix of effects (of interest -without nuisance) vs models
			
			# Make a list of all models and their components, without nuisance terms, for later use
			
			modelsmain <- names(withmain)$numerator
			
			a <- .modelnames(modelsmain,null.name,terms.nuisance)
			for (n in 1: length(a)){
				assign(names(a)[n], a[[n]])
			}
			
			##PROCESS RESULTS
			BFmain.0	<- c(1,BFmain)
			# prior probability: 1/ number of models
			pprior <- .clean(1/n.mod.0)			#n.mod.0 comes from .modelnames()
			# posterior probability: computed from Bayes factors (only with equal prior probability)
			posterior.probability <- c(BFmain.0)/sum(BFmain.0)
			
			BFmodels <-	(posterior.probability/(1-posterior.probability))/(pprior/(1-pprior))
			errormain.0 <- c(NA,errormain)
			
			#####TABLE####
			
			for (n in 1:n.mod.0) {
				ppost <- .clean(posterior.probability[n])
				BF <- .clean(BFmain.0[n] )
				error <- .clean(errormain.0[n]*100)
				if (n==1){
					error <- " "
				}
				model.name <-	models.tab[n]
				BFM <- .clean(BFmodels[n])
				if (ind.random == 0 && ind.cov == 0) {
					r <- list("Models"= model.name,"P(M)" = pprior, "P(M|Y)"=ppost,
					"BFM" = BFM,"BF10"=BF, "% error"=error)
				} else {
					r <- list("Models"= model.name,"P(M)" = pprior, "P(M|Y)"=ppost,
					"BFM" = BFM,"BF10"=BF, "% error"=error, ".footnotes"=list("p"=list(1)))
				}
				posterior.results[[length(posterior.results)+1]] <- r
			}
		
			footnotes <- list()
			if (length(terms.nuisance) > 0){
				footrand <- paste(.unvf(terms.nuisance), collapse=" and ")
				footnotes <- list(paste("All models include", footrand))
			}
			posterior[["footnotes"]] <- footnotes
	
		} else {
			#build init table
			posterior.results[[1]] <- list("Models"=null.name)
			
			for (model in all.models) {
				nuisance.plus    <- paste(c("", terms.nuisance), collapse=" + ")
				nuisance.alone    <- paste(c(terms.nuisance), collapse=" + ")
				model.name <- as.character(model)[[3]]
				if (length(terms.nuisance) > 0) {
					model.name    <- gsub(nuisance.plus, "", model.name, fixed = TRUE)
					model.name    <- gsub(nuisance.alone, "", model.name, fixed = TRUE)
				}
				model.name <- .unvf(model.name)
				posterior.results[[length(posterior.results)+1]] <- list("Models" = model.name)
			}

			if (error.present > 0){
				#provide errors
				if (specific.error == "interaction nuisance"){
					posterior[["error"]] <- list(errorType="badData", errorMessage="Interactions as nuisance are only allowed when the corresponding main effects are specified as nuisance")
				}
				if (specific.error == "interaction"){
					posterior[["error"]] <- list(errorType="badData", errorMessage="Interactions are only allowed when the corresponding main effects are specified")
				}
				if (specific.error == "levels"){
					posterior[["error"]] <- list(errorType="badData", errorMessage="Factors must have 2 or more levels")
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
					posterior[["error"]] <- list(errorType="badData", errorMessage=paste("After removing cases with missing values, less than 2 levels were observed for: ", factor.names,".",sep=""))
				}
				if (specific.error =="all nuisance"){
					posterior[["error"]] <- list(errorType="badData", errorMessage="All modelterms are specified as nuisance")
				}
			}
		}
		
	} else {
		posterior.results[[1]] <- list("Models" = null.name)
	}
	posterior[["data"]] <- posterior.results


    ###################
    ###EFFECTS TABLE###
    ##################
    
    
    if (options$outputEffects == TRUE) {
        
        # set up table
        
        effect <- list()
        effect[["title"]] <- "Bayesian ANOVA: Analysis of Effects"
        
        fields <- list(
            list(name="Effects", type="string"),
            list(name="P(incl)", type="number", format="sf:4;dp:3"),
            list(name="P(incl|Y)", type="number", format="sf:4;dp:3"),
            list(name="BF<sub>Inclusion</sub>", type="number", format="sf:4;dp:3"),
            list(name="BF<sub>Backward</sub>", type="number", format="sf:4;dp:3"),
            list(name="% errorB", type="number", format="sf:4;dp:3"),
            list(name="BF<sub>Forward</sub>", type="number", format="sf:4;dp:3"),
            list(name="% errorF", type="number", format="sf:4;dp:3")
        )
        
        schema <- list(fields=fields)
        effect[["schema"]] <- schema
        
        if (options$dependent != "" && length(options$modelTerms) > 0) {
            
            effect.results <- list()
            
            ### TABLE when non-run or error 
            if (perform == "init" || error.present > 0) {
                for (term in options$modelTerms) {
                    if(term$isNuisance ==FALSE){
                        effect.results[[length(effect.results)+1]] <- list("Effects"=paste(term$components, collapse=":"))
                    }                
                }
                if (perform == "run" && error.present > 0) {
                    effect[["error"]] <- list(errorType="badData")
                }
            } else {    # if the requirements are met to run the model 
                
                #############PREPARE NAMES AND MATCHES 
                
                b <- .modelsandeffects(modelsmain, terms.nuisance, terms.as.strings)
                for (n in 1: length(b)){
                    assign(names(b)[n], b[[n]])
                }
                
                ###########ANALYSIS###
                
                if (length(models) > 2) { #if null model + at least 2 more
                        
                    # 1. Bottom up effects	(Forward analysis)
                    
                    bottom <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "bottom", 
                                                                                             neverExclude = paste("^",terms.nuisance,"$", sep = ""), whichRandom = random, 
                                                                                             progress=FALSE, callback=jasp.callback)
                    
                    if (length(terms.nuisance) > 0) {
                        indb.nui <- which(names(bottom)$numerator == paste(terms.nuisance, collapse=" + ")) 
                        bottom <- bottom[-indb.nui]/bottom[indb.nui]
                    } 
                    
                    BFbot <- as.numeric(exp(bottom@bayesFactor$bf))
                    errorbot <- as.numeric(bottom@bayesFactor$error)
                    
                    
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
                            Full[e] <-	which(match.eff.mod[e,] == 1)
                            Reduced[e] <-	which(complexity.models == max(complexity.models[-Full[e]]))	
                            
                        } else {
                            #Model has to meet the condition that the effect is included 
                            #Model shouldnt include a higher order interaction with the effect 
                            mno1 <- which(match.eff.interactions [e,] == 0)
                            mno2 <- which(match.eff.mod[e,]	> 0)
                            selection <- intersect(mno1,mno2)
                            # Look for the most complex model which meets these criteria
                            Full[e] <- selection[which(complexity.models[selection]==max(complexity.models[selection]))]
                            # Look for the most complex model without the effect
                            mno3 <- which(match.eff.mod[e,] == 0)
                            Reduced[e] <- mno3[which(complexity.models[mno3]==max(complexity.models[mno3]))]
                        }
                        
                        out <- withmain[Full[e]-1]/withmain[Reduced[e]-1]
                        BF.Backward[e] <- as.numeric(exp(out@bayesFactor$bf))
                        error.Backward[e] <- as.numeric(exp(out@bayesFactor$error))
                    }
                    
                    Fnames <- models[Full]
                    Rnames <- models[Reduced]
                
                } # if length(models) > 2
                
                
                #TABLE#
                
                if (length(models) > 2) { #if null model + at least 2 more
                    
                    for (e in 1:n.eff) {
                                
                        BFtops <- .clean(BF.Backward[e])
                        errortops <- .clean(error.Backward[e])
                        BFbottom <- .clean(BFbot[e])
                        errorbottom <- .clean(errorbot[e]*100)
                                
                        if(n.comp.eff[e] == 1) {
                            ind.model.e <- which(models == effects[e])
                            BFbottom <- .clean(BFmain.0[ind.model.e])
                            errorbottom <- .clean(errormain.0[ind.model.e]*100)
                        }
                        
                        effect.name <- as.character(effects.tab[e])
                        
                        ipb <- .clean(inclusion.probabilities[e])
                        ppb <- .clean(prior.probabilities[e])
                        bfi <- .clean(Bayesfactor.inclusion[e])
                        
                        r <- list("Effects"=effect.name,"P(incl)"= ppb, "P(incl|Y)"= ipb,"BF<sub>Inclusion</sub>" = bfi,
                                            "BF<sub>Backward</sub>"=BFtops, "% errorB"=errortops,"BF<sub>Forward</sub>"=BFbottom, "% errorF"=errorbottom)
                        
                        effect.results[[length(effect.results)+1]] <- r
                    } #for (e in 1:n.eff) 
                    
                
                } # if length(models) > 2
                                
                if (length(models) == 2) { #if null model + only one other model
                
                    BFtops <- ""
                    errortops <- ""
                    BFbottom <- .clean(BFmain.0[2])
                    errorbottom <- .clean(errormain.0[2]*100)
                    effect.name <- as.character(models.tab[2])
                    
                    ipb <- .clean(posterior.probability[2])
                    ppb <- .clean(pprior)
                    bfi <- .clean(BFmodels[2])
                    
                    r <- list("Effects"=effect.name,"P(incl)"= ppb, "P(incl|Y)"= ipb,"BF<sub>Inclusion</sub>" = bfi,
                                        "BF<sub>Backward</sub>"=BFtops, "% errorB"=errortops,"BF<sub>Forward</sub>"=BFbottom, "% errorF"=errorbottom)
                    
                    effect.results[[length(effect.results)+1]] <- r
                    
                }     #if (length(models) == 2)
                
            } #else from (perform == "init" || error.present > 0) 
                
            effect[["data"]] <- effect.results
            
        } # if (options$dependent != "" && length(options$modelTerms) > 0) 
                
    } #if (options$outputEffects == TRUE)
    
    if (TRUE)
        results[["posterior"]] <- posterior
    if (FALSE)	
        results[["null"]] <- null
    if (options$outputEffects)
        results[["effect"]] <- effect
    
    results
    
}#END FUNCTION
    
    
    
    




#########################################################
##########SUPPORTING FUNCTIONS###########################
#########################################################
.checkerrors <- function(options, dataset,perform)    { 
    
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
	
	
    # error message when less than two levels are observed for a factor after deleting NA's
    if(perform == "run"){
    	for (fact in options$fixedFactors) {
    		if (length(unique(dataset[[.v(fact)]])) < 2){
    			error.present <- 1
    			specific.error <- "observed levels"
    		} 
    	}
    }
	
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
    
    list(error.present = error.present, specific.error = specific.error)
}

.modelnames <- function(modelsmain, null.name, terms.nuisance) {
    
    n.mod <- length(modelsmain)
    
    models.tab <- null.name
    models <- .vf(null.name) 
    models.no.null <- c()
    for (i in 1:n.mod) { 
        if (length(terms.nuisance)    == 0) {
            models.tab <-     c(models.tab,as.character(.unvf(modelsmain[i]))) 
            models <-     c(models,as.character(modelsmain[i])) 
            models.no.null <- c(models.no.null, as.character(modelsmain[i]))        
        } else {
            nuisance.plus    <- paste(c("", terms.nuisance), collapse=" + ") 
            mi <- as.character(modelsmain[i])
            mi2    <- gsub(nuisance.plus, "", mi, fixed = TRUE)
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
    
    comp.mod <-    vector("list",n.mod)
    for (i in 1:n.mod) { 
        comp.mod[[i]] <- .decompose(modelsmain[i])[[1]][[1]] # list of components of each model
        n.comp.mod[i] <- length(comp.mod[[i]]) # number of components of each model
        
        if (length(terms.nuisance > 0 )) {    # take the nuisance terms out of the model specification
            a <- .decompose(modelsmain[i])[[1]][[1]]     
            comp.mod[[i]] <-     a[-which(is.element(a, terms.nuisance))]
            n.comp.mod[i] <- length(comp.mod[[i]])
        }
    }
    
    # Create model names and names for table without nuisance effects
    #!!!! watchout,    gave models a different name!!!!!!
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
    effects.tab <-    .unvf(effects)
    
    
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
                TT <-    matrix(0,n.comp.eff[n1],length(interaction.possible)) 
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


