
AnovaBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	 
	if(is.null(base::options()$BFMaxModels)) base::options(BFMaxModels = 50000)
	if(is.null(base::options()$BFpretestIterations)) base::options(BFpretestIterations = 100)
	if(is.null(base::options()$BFapproxOptimizer)) base::options(BFapproxOptimizer = "optim")
	if(is.null(base::options()$BFapproxLimits)) base::options(BFapproxLimits = c(-15,15))
	if(is.null(base::options()$BFprogress)) base::options(BFprogress = interactive())
	if(is.null(base::options()$BFfactorsMax)) base::options(BFfactorsMax = 5) 
	
	numeric.variables <- c(unlist(options$dependent),unlist(options$wlsWeight))
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
	# Currently only effect works, needs posterior estimates and plots of posterior distributions 
	
	meta <- list()
	
	meta[[1]] <- list(name="posterior", type="table")
	meta[[2]] <- list(name="effect", type="table")

	results[[".meta"]] <- meta
	
	
	jasp.callback <- function(...) as.integer(callback())
	
	##############
	### DEFAULT###
	##############
	## This table will have a number of rows equal to the number of MODELS, not 
	## the number of effects. Output shows prior model probability, posterior 
	## model probability, Bayes factor showing change from prior to posterior 
	## model probability, Bayes factor for model versus intercept only model and 
	## percentage of error for model vs intercepr Bayes factor 
	
	# Set up table as a list
	
	posterior <- list()
	posterior[["title"]] <- "Bayesian ANOVA: Model Comparison"
		
	
	fields <- list(
		list(name="Models", type="string", .footnotes = list(0)),
		list(name="P(M)", type="number", format="sf:4;dp:3"),
		list(name="P(M|Y)", type="number", format="sf:4;dp:3"),
		list(name="BF<sub>M</sub>", type="number", format="sf:4;dp:3"),
		list(name="BF<sub>10</sub>", type="number", format="sf:4;dp:3"),
		list(name="% error", type="number", format="sf:4;dp:3")
		
		)
	
	schema <- list(fields=fields)
	posterior[["schema"]] <- schema	

	# set up variable names and formula
	
	if (length(options$modelTerms) > 0) {
	
		terms <- options$modelTerms
		
		if (perform == "run"){	
		
			# error message if a factor has only one level 
			ferror <- 0
			for (fact in options$fixedFactor) {
				levels <- unique(dataset[[.v(fact)]])
				if (length(levels) < 2) {
					ferror <- ferror + 1
				} 
			}		 
			 
			if (ferror > 0) {
				
				posterior[["error"]] <- list(errorType="badData", errorMessage="Factors must have 2 or more levels")
				
				posterior.results <- list()
				r <- list("Models"= "","P(M)" = "", "P(M|Y)"="","BF<sub>M</sub>" = "","BF<sub>10</sub>"="", "% error"="")
				posterior.results[[length(posterior.results)+1]] <- r
				
				ind.random <- length(options$randomFactors)
				
				
			} else {
			
			posterior.results <- try (silent = FALSE, expr = {
				
					terms.as.strings <- c()
				
				for (term in options$modelTerms) {
					term.as.string <- paste(.v(term$components), collapse=":")
					# .v changes from interface to dataframe
					terms.as.strings <- c(terms.as.strings, term.as.string)
				}
				
				
				ind.random <- length(options$randomFactors)
				
				if (ind.random > 0) {
				
					for (rand in options$randomFactors) {
					
						term.as.string <- paste(.v(rand))
						terms.w.rand <- c(terms.as.strings, term.as.string)
					}
					
				} else {
				
					terms.w.rand <- terms.as.strings
				}
				
				rhs <- paste(terms.w.rand, collapse="+")
				model.def <- paste(.v(options$dependent), "~", rhs)
				model.formula <- as.formula(model.def)

				ind.random <- length(options$randomFactors)
			
				##ANALYSIS##
			
				if (ind.random > 0) {
				
					random <- .v(options$randomFactors)
					withmain <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "withmain", neverExclude = random, whichRandom = random, progress=FALSE, callback=jasp.callback)
					indr <- which(names(withmain)$numerator == paste(random, collapse=" + "))
					withmain <- withmain[-indr]/withmain[indr]
					
				} else {
				
					withmain <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "withmain", progress=FALSE, callback=jasp.callback)
				}

				BFmain <- as.numeric(exp(withmain@bayesFactor$bf))
				errormain <- as.numeric(withmain@bayesFactor$error)

					
				##PREPARE VECTORS##
				# - modelnames for table
				# - prepare for component matching to allow customizing

				if (ind.random > 0) {
				
					random.plus	<- paste(.unv(random), collapse=" + ") 
					models <- as.character(paste("Null model (incl.",	random.plus, ")", sep = "" ) )
					
				} else {
				
					models <- as.character(c("Null model"))
				}	
																									
				# all possible models and their components 
				n.mod.all <- length(names(withmain)$numerator)	
				n.comp.mod <- rep(0,n.mod.all)
								
				complist <-	vector("list",n.mod.all)
				for (i in 1:n.mod.all) { 
				
					complist[[i]] <- .decompose(names(withmain)$numerator[i])[[1]][[1]]
					n.comp.mod[i] <- length(complist[[i]])
					
					if (ind.random > 0) {
					
						a <- .decompose(names(withmain)$numerator[i])[[1]][[1]]	 
						complist[[i]] <-	 a[which(a != random)]
						n.comp.mod[i] <- length(complist[[i]])
					}
				}
				
				for (i in 1:n.mod.all) { 
				
					if (ind.random == 0) {
					
						models <-	 c(models,as.character(.unvf(names(withmain)$numerator[i]))) 
						
					} else {
					
						random.plus	<- paste(c("", options$randomFactors), collapse=" + ") 
						mi <- as.character(.unvf(names(withmain)$numerator[i]))
						mi2	<- gsub(random.plus, "", mi, fixed = TRUE)
						models <- c(models, mi2) 
					}
				}
				
				# In case of customized model specification: 
				nocomp <- length(terms.as.strings)
				seq.eff <- seq(1:length(terms.as.strings))
			
				custommtch <- matrix(0,nocomp,n.mod.all)

				for (n1 in 1:nocomp) {
					for (n2 in 1:n.mod.all) {
						abs <- 0
						for (j in 1:n.comp.mod[n2]) {
						
							if (length(complist[[n2]][[j]]) == length(.v(options$modelTerms[[n1]]$component)) ) {
								A <- paste(sort(complist[[n2]][[j]]), sep="", collapse="")
								B <- paste(sort(.v(options$modelTerms[[n1]]$component)), sep="", collapse="")	
								abs	<- abs + (A==B)
							}
						}
						
						if (abs > 0)
							custommtch[n1,n2] <- 1
					}
				}
	
				##PROCESS RESULTS 
				BFmain.c	<- c(1,BFmain)		
				n.models.c <- length(models)
	
				# prior probability: 1/ number of models
				pprior <- .clean(1/n.models.c)	
				# posterior probability: computed from Bayes factors (only with equal prior probability) 
				posterior.probability <- c(BFmain.c)/sum(BFmain.c)
			
				BFmodels <-	(posterior.probability/(1-posterior.probability))/(pprior/(1-pprior))
				BFnull		<- as.numeric(1/1)

				errormain <- c(NA,errormain)
				modelscustom <-	 models
	 
				#TABLE#
				posterior.table <- list()
		
				for (n in 1:n.models.c) {
				
					ppost <- .clean(posterior.probability[n])
					BF <- .clean(BFmain.c[n] )
					error <- .clean(errormain[n]*100)
					
					if (n==1)
						error <- " "
					
					model.name <-	modelscustom[n]
					BFM <- .clean(BFmodels[n])
		
					if (ind.random == 0) {
					
						r <- list("Models"= model.name,"P(M)" = pprior, "P(M|Y)"=ppost,
							"BF<sub>M</sub>" = BFM,"BF<sub>10</sub>"=BF, "% error"=error)
					} else {
					
						r <- list("Models"= model.name,"P(M)" = pprior, "P(M|Y)"=ppost,
							"BF<sub>M</sub>" = BFM,"BF<sub>10</sub>"=BF, "% error"=error, ".footnotes"=list("p"=list(1)))
					}
	
					posterior.table[[length(posterior.table)+1]] <- r
				}

				posterior.table
			})

	}
			if (class(posterior.results) == "try-error") {
				posterior.results <- list()
				r <- list("Models"= "","P(M)" = "", "P(M|Y)"="","BF<sub>M</sub>" = "","BF<sub>10</sub>"="", "% error"="")
				posterior.results[[length(posterior.results)+1]] <- r
			}
	
			posterior[["data"]] <- posterior.results


		 		footnotes <- list()
 				if (ind.random > 0){
 					footrand <- paste(options$randomFactors, collapse=" and ")
 					footnotes <- list(paste("All models include", footrand))
 				} 				
				posterior[["footnotes"]] <- footnotes
		
		
		
		}
	
	}
	
	########################
	####FUTURE TABLE ####
	########################

	if (FALSE) {	
		###	for the second table 
		null <- list()
		null[["title"]] <- options$model
		null[["cases"]] <- options$modelTerms
	
		fields <- list(
			list(name="Models", type="string"),
			list(name="BF<sub>10</sub>", type="number", format="sf:4;dp:3"),
			list(name="% error", type="number", format="sf:4;dp:3") 
		)
	
		schema <- list(fields=fields)
		null[["schema"]] <- schema
	
		if (length(options$modelTerms) > 0) {
			terms <- options$modelTerms
			null.results <- list()
			null.results <- for(i in 1:(n.models.c-1)) {
			r <- list("Models"="","BF<sub>10</sub>"="", "% error"="")
			null.results[[length(null.results)+1]] <- r
		}
			
		if (class(null.results) == "try-error") {
			null.results <- list()
				null.results <- for(i in 1:(n.models.c-1)) {
				r <- list("Models"="","BF<sub>10</sub>"="", "% error"="")
				null.results[[length(null.results)+1]] <- r
			}
		}
	#		null[["cases"]] <- names(models)
			null[["data"]] <- null.results
		}
	}

	###################
	###EFFECTS TABLE###
	##################
	# Now we compute the results for the inclusion/removal of every term in the model
	## this table will have a number of rows equal to the number of effects
	## this table will have columns corresponding to the prior and posterior inclusion
	## probability for the effect, the Bayes factor for inclusion,
	## a "backward Bayes factor" comparing the fullest model with the effect with 
	## the model when the effect would be removed, and 
	## a "forward bayes factor" compring a model with only the indicated effect 
	## with a null model 
	
	if (options$outputEffects == TRUE) {

		# set up table as a list	 
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
		
		if (length(options$modelTerms) > 0) {
	
			if (perform == "run" & ferror == 0) {
		
				effect.results <- try (silent = FALSE, expr = {	
			
					if (length(modelscustom) > 2) {
				
						###ANALYSIS###
						# 1. Bottom up effects	(Forward analysis)
							
						if( ind.random > 0) {
					
							random <- .v(options$randomFactors)
							bottom <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "bottom", neverExclude = random, whichRandom = random, progress=FALSE, callback=jasp.callback)
							indr <- which(names(bottom)$numerator == paste(random, collapse=" + ")) 
							bottom <- bottom[-indr]/bottom[indr]
						
						} else {
					
							bottom <- BayesFactor::generalTestBF(model.formula, dataset, whichModels = "bottom", progress=FALSE, callback=jasp.callback)
						}
				
						BFbot <- as.numeric(exp(bottom@bayesFactor$bf))
						errorbot <- as.numeric(bottom@bayesFactor$error)
			
						effect.names <- as.character(names(bottom)$numerator)
						neff.c <- length(effect.names)
					
						# the random effects are in the model names as well 
						if (ind.random > 0) {
					
							randomv.plus <- paste(c("",random), collapse=" + ") 
							effect.names.r <- effect.names
							effect.names <- gsub(randomv.plus,"",effect.names.r, fixed = TRUE)
						}
		
						# 2. Inclusion probabilities and Bayes factors 
				
						# Extract names of models from BF output 
						model.names.c <- models 
				
						#Match models and effects
						# which effects are in which models 
						match <- cbind(rep(0,neff.c),custommtch)
				
						inclusion.probabilities <- rowSums(match * matrix(rep(posterior.probability[], each = neff.c) ,neff.c,n.models.c))
						prior.probabilities <- rowSums(match)/n.models.c
						Bayesfactor.inclusion <- (inclusion.probabilities/(1-inclusion.probabilities))/(prior.probabilities/(1- prior.probabilities))
								
						#3. Backward probabilities (use this for more elaborate model)
						 
						complist[[n.mod.all+1]] <-	"NULL"
						complist.c <- complist[c(n.mod.all+1,1:n.mod.all)]				
						model.length <- sapply(complist.c, length)
				
				
						# Which interactions models have interactions including the effect
						# and are therefore forbidden to be in the full model
					
						effect.length <- sapply(strsplit(effect.names, ":"), length)
						effect.components <- sapply(strsplit(effect.names, ":"), as.character) 
			
						match2 <- matrix(0,neff.c,n.models.c)
						#match 2 gives for every effect the models which include an interaction in which the effect is included (lower order)
						for(n1 in 1:neff.c) {
					
							for(n2 in 1:n.models.c) {
						
								# length of each model component 
								comp.length <- sapply(complist.c[[n2]], length) 
								# which model components are longer than the effect - higher order interactions
								adm <- which(comp.length>effect.length[n1])
							
								if (length(adm) > 0) {
							
									TT <-	matrix(0,effect.length[n1],length(adm)) 
								
									for (ii in 1:effect.length[n1]) {
										for (jj in 1:length(adm)) {
											#check whether each effect component is one of the components of the higher order interaction(s)
											TT[ii,] <-	length(grep(effect.components[[n1]][ii],complist.c[[n2]][[adm[jj]]]))
										}	
									}
								
									# for each higher order interaction check whether the number of matching components corresponds to the effect length
									# because if only one of the effect compnents is in the higher order model component thwe 
									# do not want it to give a signal 
									if (sum(colSums(TT) == effect.length[n1]) > 0) {
										match2[n1,n2] <- 1
									}
								}
							}
						}
				 
						# Combine match and forbidden to select the Full and Reduced model to compare
						complexity.models <- colSums(match) # complexity (no. terms) of each model
						ninlude <- rowSums(match)
						Full <- rep(0, neff.c)
						Red <-	rep(0, neff.c)
						BF.Backward <-	rep(0, neff.c)
						error.Backward <-	rep(0, neff.c)
					
						for (e in 1:neff.c) {
					 
							if (ninlude[e] == 1) {
						 
								Full[e] <-	which(match[e,]==1)
								Red[e] <-	which(complexity.models ==	max(complexity.models[-Full[e]]))	
							 
							} else {
						 
								# the model has to meet the condition that the effect is included but the model doesnt include a
								# higher order interaction with the model 
								mno1 <- which(match2[e,] ==0)
								mno2 <- which(match[e,]	>0)
								selection <- intersect(mno1,mno2)
					 
								Full[e] <- selection[which(complexity.models[selection]==max(complexity.models[selection]))]
								mno3 <- which(match[e,] == 0)
								Red[e] <- mno3[which(complexity.models[mno3]==max(complexity.models[mno3]))]
							 
							}
						 
							out <- withmain[Full[e]-1]/withmain[Red[e]-1]
							BF.Backward[e] <- as.numeric(exp(out@bayesFactor$bf))
							error.Backward[e] <- as.numeric(exp(out@bayesFactor$error))
						}
				
						Fnames <- model.names.c[Full]
						Rnames <- model.names.c[Red]
			 
			 
						################################
						# For Table
						###################################
						# put effect names back to original
		
			
						effect.names.tab <- rep(0, neff.c)
						for (i in 1:neff.c) { 
					
							effect.names.tab[i] <- .unvf(effect.names[i])
						}
					}
		
						#TABLE#
		
					effect.table <- list()
									
					if (length(modelscustom) > 2) {
			
						for (e in 1:neff.c) {
					
							BFtops <- .clean(BF.Backward[e])
							errortops <- .clean(error.Backward[e])
							BFbottom <- .clean(BFbot[e])
							errorbottom <- .clean(errorbot[e]*100)
					
							effname <- as.character(effect.names.tab[e])
				 
							ipb <- .clean(inclusion.probabilities[e])
							ppb <- .clean(prior.probabilities[e])
							bfi <- .clean(Bayesfactor.inclusion[e])
					
							r <- list("Effects"=effname,"P(incl)"= ppb, "P(incl|Y)"= ipb,"BF<sub>Inclusion</sub>" = bfi,
								"BF<sub>Backward</sub>"=BFtops, "% errorB"=errortops,"BF<sub>Forward</sub>"=BFbottom, "% errorF"=errorbottom)
					
							effect.table[[length(effect.table)+1]] <- r
						}
					}
		
					if (length(modelscustom) == 2) {
			
						BFtops <- ""
						errortops <- ""
						BFbottom <- .clean(BFmain[2])
						errorbottom <- .clean(errormain[2]*100)
						effname <- as.character(modelscustom[2])
			
						ipb <- .clean(posterior.probability[2])
						ppb <- .clean(pprior)
						bfi <- .clean(BFmodels[2])
			
						r <- list("Effects"=effname,"P(incl)"= ppb, "P(incl|Y)"= ipb,"BF<sub>Inclusion</sub>" = bfi,
											"BF<sub>Backward</sub>"=BFtops, "% errorB"=errortops,"BF<sub>Forward</sub>"=BFbottom, "% errorF"=errorbottom)
			
						effect.table[[length(effect.table)+1]] <- r
					}
		
					effect.table
				
				})
			
				if (class(effect.results) == "try-error" | ferror > 0) {
					effect.results <- list()
					r <- list("Effects"= "","P(incl)"= "", "P(incl|Y)"= "","BF<sub>Inclusion</sub>" = "",
												"BF<sub>Backward</sub>"="", "% errorB"="","BF<sub>Forward</sub>"="", "% errorF"="")
					effect.results[[length(effect.results)+1]] <- r
				
				}
			
			#	print("effect.results")
			#	print(effect.results)
			#	print("effect.results end")
		
				effect[["data"]] <- effect.results
			}
		}
	}

	if (TRUE)
		results[["posterior"]] <- posterior
	if (FALSE)	
		results[["null"]] <- null
	if (options$outputEffects)
		results[["effect"]] <- effect

	results
}



		


