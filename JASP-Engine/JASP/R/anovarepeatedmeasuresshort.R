
AnovaRepeatedMeasuresShort <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

    numeric.variables <- c(unlist(options$repeatedMeasuresCells))
    numeric.variables <- numeric.variables[numeric.variables != ""]
    factor.variables <- c(unlist(options$betweenSubjectFactors))
    factor.variables <- factor.variables[factor.variables != ""]

    if (is.null(dataset)) {

        if (perform == "run") {

            dataset <- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=c(numeric.variables, factor.variables))

        } else {

            dataset <- .readDataSetHeader(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
        }
		
	} else {
	
		dataset <- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)	
	}
    
    results <- list()
    
    
    ## META definitions

	.meta <- list(
		list(name="title", type="title"),
		list(name="headerWithinSubjectsEffects", type="h1"),
		list(name="withinSubjectsEffects", type="table"),
		list(name="headerBetweenSubjectsEffects", type="h1"),
		list(name="betweenSubjectsEffects", type="table"),
		list(name="headerSphericity", type="h1"),
		list(name="sphericity", type="table"),
		list(name="headerLevene", type="h1"),
		list(name="levene", type="table"),
		list(name="headerDescriptives", type="h1"),
		list(name="descriptives", type="table")
	)
	
	results[[".meta"]] <- .meta
	
	
	
	## Create Title

	results[["title"]] <- "Repeated Measures ANOVA"
	
	
	status <- .rmAnovaCheck(dataset, options, perform)
	
	
	
	## Perform ANOVA

	model <- NULL
	epsilon <- NULL
	mauchly <- NULL
	epsilonError <- FALSE
	if (perform == "run" && status$ready && status$error == FALSE) {
		
		anovaModel <- .rmAnovaModel(dataset, options, status)
		model <- anovaModel$model
		epsilon <- anovaModel$epsilon
		epsilonError <- anovaModel$epsilonError
		mauchly <- anovaModel$mauchly
		status <- anovaModel$status
	
	}
    
    
        
    ## Create Sphericity Assumption Table
    
    result <- .sphericityTest(dataset, options, perform, epsilon, epsilonError, mauchly, status) 
    
    results[["sphericity"]] <- result$result
	status <- result$status
	epsilonTable <- result$epsilonTable
	
	if (options$miscSphericityTests)
	    results[["headerSphericity"]] <- "Test of Sphericity"



	## Create Within Subjects Effects Table

	result <- .rmAnovaWithinSubjectsTable(dataset, options, perform, model, epsilonTable, epsilonError, status)
	
	results[["withinSubjectsEffects"]] <- result$result
	status <- result$status
	
	results[["headerWithinSubjectsEffects"]] <- "Within Subjects Effects"
	
	
	
	## Create Between Subjects Effects Table

	result <- .rmAnovaBetweenSubjectsTable(dataset, options, perform, model, status)
	
	results[["betweenSubjectsEffects"]] <- result$result
	status <- result$status
	
	results[["headerBetweenSubjectsEffects"]] <- "Between Subjects Effects"
	
	
	
	## Create Levene's Table

	result <- .rmAnovaLevenesTable(dataset, options, perform, status)
	
	results[["levene"]] <- result$result
	status <- result$status
	
	if (options$miscHomogeneityTests)
	    results[["headerLevene"]] <- "Test for Equality of Variances"
	
	
	
	## Create Descriptives Table
	
	result <- .rmAnovaDescriptivesTable(dataset, options, perform, status)
	
	results[["descriptives"]] <- result$result
	status <- result$status
	
	if (!is.null(results[["descriptives"]]))
	    results[["headerDescriptives"]] <- "Descriptives"
	
	
	results
}

.rmAnovaCheck <- function(dataset, options, perform) {

	error <- FALSE
	errorMessage <- NULL
	ready <- "" %in% options$repeatedMeasuresCells == FALSE
	
	if (ready && perform == "run") {
	
		components <- unique(unlist(options$betweenSubjectFactors))
		independentsWithLessThanTwoLevels <- c()
		
		for (component in components) {
		
			column <- dataset[[ .v(component) ]]
			if (length(unique(column)) < 2)
				independentsWithLessThanTwoLevels <- c(independentsWithLessThanTwoLevels, component)
		}
		
		if (length(independentsWithLessThanTwoLevels) > 0) {
		
			error <- TRUE
			if(length(independentsWithLessThanTwoLevels) == 1) {
			    errorMessage <- paste("Factor: <em>", independentsWithLessThanTwoLevels, "</em>, contains less than two levels.<br><br>(Possible only after rows with missing values are excluded)", sep="")
			} else {
			    errorMessage <- paste("Factors: <em>", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""), "</em>, contain less than two levels.<br><br>(Possible only after rows with missing values are excluded)", sep="")
			}
		}
		
		repeatedMeasuresData <- list()
		for(i in options$repeatedMeasuresCells) {
		    repeatedMeasuresData[[i]] <- dataset[[.v(i)]]
		}
		infiniteRM <- unlist(lapply(repeatedMeasuresData,function(x)sum(is.infinite(x)) > 0))
		
		if (!is.null(infiniteRM) && sum(infiniteRM) > 0) {
		
			error <- TRUE
			if(sum(infiniteRM) == 1) {
			    errorMessage <- paste("The repeated measure: <em>", options$repeatedMeasuresCells[infiniteRM], "</em>, contains infinite values.<br><br>(Possible only after rows with infinite values are excluded)", sep="")
			} else {
			    errorMessage <- paste("The repeated measures: <em>", paste(options$repeatedMeasuresCells[infiniteRM], collapse=", "), "</em>, contain infinite values.<br><br>(Possible only after rows with infinite values are excluded)", sep="")
			}
		}
		
		covariatesData <- list()
		for(i in options$covariates) {
		    covariatesData[[i]] <- dataset[[.v(i)]]
		}
		infiniteCov <- unlist(lapply(covariatesData,function(x)sum(is.infinite(x)) > 0))
		
		if (!is.null(infiniteCov) && sum(infiniteCov) > 0) {
		
			error <- TRUE
			if(sum(infiniteCov) == 1) {
			    errorMessage <- paste("The covariate: <em>", options$covariates[infiniteCov], "</em>, contains infinite values.<br><br>(Possible only after rows with infinite values are excluded)", sep="")
			} else {
			    errorMessage <- paste("The covariates: <em>", paste(options$covariates[infiniteCov], collapse=", "), "</em>, contain infinite values.<br><br>(Possible only after rows with infinite values are excluded)", sep="")
			}
		}
		
	}
	
	list(ready=ready, error=error, errorMessage=errorMessage)
}

.rmModelFormula <- function(options) {

    termsRM.base64 <- c()
    termsRM.normal <- c()

    for (term in options$withinModelTerms) {
        
        components <- unlist(term$components)
        termRM.base64 <- paste(.v(components), collapse=":", sep="")
        termRM.normal <- paste(components, collapse=" \u273B ", sep="")
    
        termsRM.base64 <- c(termsRM.base64, termRM.base64)
        termsRM.normal <- c(termsRM.normal, termRM.normal)
    }

    termsBS.base64 <- c()
    termsBS.normal <- c()

    for (term in options$betweenModelTerms) {
    
        components <- unlist(term$components)
        termBS.base64 <- paste(.v(components), collapse=":", sep="")
        termBS.normal <- paste(components, collapse=" \u273B ", sep="")
    
        termsBS.base64 <- c(termsBS.base64, termBS.base64)
        termsBS.normal <- c(termsBS.normal, termBS.normal)
    }
    
    terms.base64 <- list()
    terms.normal <- list()
    terms.base64[[1]] <- termsBS.base64
    terms.normal[[1]] <- termsBS.normal
        
    for (i in 1:length(termsRM.base64)) {
        if (is.null(termsBS.base64)) {
            terms.base64[[i+1]] <- termsRM.base64[i]
            terms.normal[[i+1]] <- termsRM.normal[i]
        } else {
            terms.base64[[i+1]] <- c(termsRM.base64[i], paste(termsRM.base64[i], termsBS.base64, sep = ":"))
            terms.normal[[i+1]] <- c(termsRM.normal[i], paste(termsRM.normal[i], termsBS.normal, sep = " \u273B "))
        }
    }

    main <- paste("(",paste(unlist(terms.base64), collapse=" + "),")", sep="")
    termsBS <- paste("(",paste(termsBS.base64, collapse=" + "),")", sep="")
    errorRM <- paste("Error(",paste("subject/(", termsRM.base64, ")",sep="", collapse=" + "),")",sep="")
    
    if (is.null(termsBS.base64)) {
        model.def <- paste("dependent", "~", paste(main, errorRM, sep=" + "))
    } else {
        model.def <- paste("dependent", "~", paste(main, errorRM, termsBS, sep=" + "))
    }

    list(model.def = model.def, terms.normal = terms.normal, terms.base64 = terms.base64, termsRM.normal = termsRM.normal, termsRM.base64 = termsRM.base64)
}

.rmAnovaModel <- function(dataset, options, status) {
	
	modelDef <- .rmModelFormula(options)
	model.formula <- as.formula(modelDef$model.def)
	
	dataset <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)
	
	options(contrasts=c("contr.sum","contr.poly"))
	
	if (options$sumOfSquares == "type1") {
	
		result <- try(stats::aov(model.formula, data=dataset), silent = TRUE)
		sphericityStatistics <- try(afex::aov.car(model.formula, data=dataset, type= 3, return = "univariate"), silent = TRUE)
	
	} else if (options$sumOfSquares == "type2") {
	
		result <- try(afex::aov.car(model.formula, data=dataset, type= 2, return = "univariate"), silent = TRUE)
		
	} else {
	
		result <- try(afex::aov.car(model.formula, data=dataset, type= 3, return = "univariate"), silent = TRUE)
	}
	
	model <- NULL
	epsilon <- NULL
	mauchly <- NULL
	epsilonError <- FALSE
	
    if (length(class(result)) == 1 && class(result) == "try-error") {
    
        status$error <- TRUE
        status$errorMessage <- .extractErrorMessage(result)
        
    } else {
        
        if (options$sumOfSquares == "type1" && class(sphericityStatistics) != "try-error") {
            
            model <- summary(result)
            epsilon <- sphericityStatistics$sphericity.correction
            mauchly <- sphericityStatistics$mauchly
        
        } else if (options$sumOfSquares == "type1" && class(sphericityStatistics) == "try-error") {
        
            model <- summary(result)
            epsilonError <- TRUE
        
        } else {
            
            model <- result$anova
            epsilon <- result$sphericity.correction
            mauchly <- result$mauchly
        
        }
    }

	list(model = model, epsilon = epsilon, epsilonError = epsilonError, mauchly = mauchly, status = status)
}

.identicalTerms <- function(x,y) {
    
    equalLength <- length(x) == length(y)
    equalTerms <- all(x %in% y)
    
    if(equalLength && equalTerms) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

.rmAnovaBetweenSubjectsTable <- function(dataset, options, perform, model, status) {

    anova <- list()
	    
	anova[["title"]] <- "Between Subjects ANOVA"
	
	fields <- list()
	
	fields[[length(fields) + 1]] <- list(name="case", type="string", title="", combine=TRUE)
    fields[[length(fields) + 1]] <- list(name="SS", type="number", format="sf:4;dp:3", title="Sum of Squares")
	fields[[length(fields) + 1]] <- list(name="df", type="integer")
	fields[[length(fields) + 1]] <- list(name="MS", type="number", format="sf:4;dp:3", title="Mean Square")
	fields[[length(fields) + 1]] <- list(name="F", type="number", format="sf:4;dp:3")
	fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")
		
	if (options$miscEffectSizeEstimates) {
	    
	    if(options$effectSizeEtaSquared) {
	        fields[[length(fields) + 1]] <- list(name="eta", type="number", title="\u03B7\u00B2", format="dp:3")
	    }
	    if(options$effectSizePartialEtaSquared) {
	        fields[[length(fields) + 1]] <- list(name="partialEta", type="number", title="\u03B7\u00B2\u209A", format="dp:3")
	    }
	    if(options$effectSizeOmegaSquared) {
	        fields[[length(fields) + 1]] <- list(name="omega", type="number", title="\u03C9\u00B2", format="dp:3")
	    }
	}
	
	anova[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
    if (options$sumOfSquares == "type1") {
    
        .addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")
						
	} else if (options$sumOfSquares == "type2") {
			
        .addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {
			
        .addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}
    	
	modelDef <- .rmModelFormula(options)
	terms.normal <- modelDef$terms.normal 
    terms.base64 <- modelDef$terms.base64
    termsRM.base64 <- modelDef$termsRM.base64
    
    if (perform == "init" || status$ready == FALSE || status$error) {
    
        anova.rows <- list()
                    
        for (j in .indices(terms.base64[[1]])) {
        
            if (j == 1) {
                newGroup <- TRUE   
            } else {				
                newGroup <- FALSE
            }
            
            row <- list("case"=terms.normal[[1]][j], "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
            anova.rows[[length(anova.rows) + 1]] <- row
                
        }
        
        row <- list("case"="Residual", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
        anova.rows[[length(anova.rows) + 1]] <- row

		anova[["data"]] <- anova.rows
		
		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
    
    } else {
    
        anova.rows <- list()
                
        if (options$sumOfSquares == "type1") {
        
            resultTable <- model[["Error: subject"]][[1]]
                                
            modelTermsResults <- strsplit(gsub(" ", "", rownames(resultTable), fixed = TRUE), ":")
    
            for (j in .indices(terms.base64[[1]])) {
    
                if (j == 1) {
                    newGroup <- TRUE   
                } else {				
                    newGroup <- FALSE
                }
            
                modelTermsCase <- strsplit(terms.base64[[1]],":")[[j]]
                index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                
                SS <- resultTable[index,"Sum Sq"]
                df <- resultTable[index,"Df"]
                MS <- resultTable[index,"Mean Sq"]
                F <- resultTable[index,"F value"]
                p <- resultTable[index,"Pr(>F)"]
            
                row <- list("case"=terms.normal[[1]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
                
                if (options$miscEffectSizeEstimates) {
                    
                    SSt <- sum(resultTable[,"Sum Sq"])
                    SSr <- resultTable["Residuals","Sum Sq"]
                    MSr <- SSr/resultTable["Residuals","Df"]
                                        
                    row[["eta"]] <- SS / SSt
                    row[["partialEta"]] <- SS / (SS + SSr)
                    omega <- (SS - (df * MSr)) / (SSt + MSr)
            
                    if (omega < 0) {
                        row[["omega"]] <- 0
                    } else {
                        row[["omega"]] <- omega
                    }
                    
                }
        
                anova.rows[[length(anova.rows) + 1]] <- row
    
            }
                
            SS <- resultTable["Residuals","Sum Sq"]
            df <- resultTable["Residuals","Df"]
            MS <- resultTable["Residuals","Mean Sq"]
            
            row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
                
            anova.rows[[length(anova.rows) + 1]] <- row
            
        } else {
        
            result <- model
            
            modelTermsResults <- strsplit(rownames(result), ":")
                    
            for (j in .indices(terms.base64[[1]])) {
    
                if (j == 1) {
                    newGroup <- TRUE   
                } else {				
                    newGroup <- FALSE
                }
            
                modelTermsCase <- strsplit(terms.base64[[1]],":")[[j]]
                index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                
                SS <- result[index,"SS"]
                df <- result[index,"num Df"]
                MS <- SS / df
                F <- result[index,"F"]
                p <- result[index,"Pr(>F)"]
            
                row <- list("case"=terms.normal[[1]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)

                if (options$miscEffectSizeEstimates) {
                    
                    modelTermsCases <- strsplit(terms.base64[[1]],":")
                    
                    indices <- c()
                    for (case in .indices(terms.base64[[1]])) {
                        indices <- c(indices, which(unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCases[[case]])))))
                    }
                    
                    SSr <- result[index,"Error SS"]
                    SSt <- sum(result[indices,"SS"]) + SSr
                    MSr <- SSr/result[index,"den Df"]
                                        
                    row[["eta"]] <- SS / SSt
                    row[["partialEta"]] <- SS / (SS + SSr)
                    omega <- (SS - (df * MSr)) / (SSt + MSr)
            
                    if (omega < 0) {
                        row[["omega"]] <- 0
                    } else {
                        row[["omega"]] <- omega
                    }
                    
                }

                anova.rows[[length(anova.rows) + 1]] <- row
    
            }
            
            indexResidual <- 2
            
            SS <- result[indexResidual,"Error SS"]
            df <- result[indexResidual,"den Df"]
            MS <- SS / df
            
            row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
            
            anova.rows[[length(anova.rows) + 1]] <- row    
        
        }
        
		anova[["data"]] <- anova.rows  
    
    }
    
    anova[["footnotes"]] <- as.list(footnotes)
    
    list(result = anova, status = status)
}

.rmAnovaWithinSubjectsTable <- function(dataset, options, perform, model, epsilon, epsilonError, status) {

    anova <- list()
	    
	anova[["title"]] <- "Within Subjects ANOVA"
	
	corrections <- NULL
        
    if (options$miscSphericityCorrections) {
        
        if (options$sphericityNone) {
            corrections <- c(corrections, "None")
        }
        
        if (options$sphericityGreenhouseGeisser) {
            corrections <- c(corrections, "Greenhouse-Geisser")
        }
        
        if (options$sphericityHuynhFeldt) {
            corrections <- c(corrections, "Huynh-Feldt")
        }
    }
	
	fields <- list()
	
	fields[[length(fields) + 1]] <- list(name="case", type="string", title="", combine=TRUE)
	
	if (options$miscSphericityCorrections && !is.null(corrections))
	    fields[[length(fields) + 1]] <- list(name="cor", type="string", title="Sphericity Correction")
	
	fields[[length(fields) + 1]] <- list(name="SS", type="number", format="sf:4;dp:3", title="Sum of Squares")
	
	if (options$miscSphericityCorrections && !is.null(corrections)) {
	    fields[[length(fields) + 1]] <- list(name="df", type="number", format="sf:4;dp:3")
	} else {
	    fields[[length(fields) + 1]] <- list(name="df", type="integer")
	}
	    
	fields[[length(fields) + 1]] <- list(name="MS", type="number", format="sf:4;dp:3", title="Mean Square")
	fields[[length(fields) + 1]] <- list(name="F", type="number", format="sf:4;dp:3")
	fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")
		
	if (options$miscEffectSizeEstimates) {
	    
	    if(options$effectSizeEtaSquared) {
	        fields[[length(fields) + 1]] <- list(name="eta", type="number", title="\u03B7\u00B2", format="dp:3")
	    }
	    if(options$effectSizePartialEtaSquared) {
	        fields[[length(fields) + 1]] <- list(name="partialEta", type="number", title="\u03B7\u00B2\u209A", format="dp:3")
	    }
	    if(options$effectSizeOmegaSquared) {
	        fields[[length(fields) + 1]] <- list(name="omega", type="number", title="\u03C9\u00B2", format="dp:3")
	    }
	}
	
	anova[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
    if (options$sumOfSquares == "type1") {
    
        .addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")
						
	} else if (options$sumOfSquares == "type2") {
			
        .addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {
			
        .addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}
    	
	modelDef <- .rmModelFormula(options)
	terms.normal <- modelDef$terms.normal 
    terms.base64 <- modelDef$terms.base64
    termsRM.base64 <- modelDef$termsRM.base64
    
    if (perform == "init" || status$ready == FALSE || status$error || (options$miscSphericityCorrections && epsilonError)) {
    
        anova.rows <- list()
        
        for (i in 2:length(terms.base64)) {
            
            for (j in .indices(terms.base64[[i]])) {
            
                if (j == 1) {
			        newGroup <- TRUE   
			    } else {				
				    newGroup <- FALSE
			    }

			    if (options$miscSphericityCorrections && !is.null(corrections)) {
			        
			        counter <- 1
			        
			        if (options$sphericityNone) {
                        row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1))
                        anova.rows[[length(anova.rows) + 1]] <- row
                        counter <- counter + 1
			        }
			        
			        if (options$sphericityGreenhouseGeisser) {
			            row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1))
			            anova.rows[[length(anova.rows) + 1]] <- row
			            counter <- counter + 1
			        } 
			        
			        if (options$sphericityHuynhFeldt) {
			            row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1))
			            anova.rows[[length(anova.rows) + 1]] <- row
			            counter <- counter + 1
			        }
			        
			    } else {
			    
			        row <- list("case"=terms.normal[[i]][j], "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
			        anova.rows[[length(anova.rows) + 1]] <- row
			        
			    }
            }
            
            if (options$miscSphericityCorrections && !is.null(corrections)) {
                
                counter <- 1
                
                if (options$sphericityNone) {
                    row <- list("case"="Residual", "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                    anova.rows[[length(anova.rows) + 1]] <- row
                    counter <- counter + 1
                }
                
                if (options$sphericityGreenhouseGeisser) {
                    row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                    anova.rows[[length(anova.rows) + 1]] <- row
                    counter <- counter + 1
                } 
                
                if (options$sphericityHuynhFeldt) {
                    row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                    anova.rows[[length(anova.rows) + 1]] <- row
                    counter <- counter + 1
                }
                
            } else {
            
                row <- list("case"="Residual", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
                anova.rows[[length(anova.rows) + 1]] <- row
                
            }   
		}

		anova[["data"]] <- anova.rows
		
		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

        if (epsilonError)
            anova[["error"]] <- list(errorType="badData", errorMessage="Could not estimate sphericity corrections due to problems with estimating the correction parameters.")
    
    } else {
    
        anova.rows <- list()
        
        if (is.null(corrections))
            corrections <- "empty"
                        
        if (options$sumOfSquares == "type1") {
        
            result <- model
            
            for (i in 2:length(terms.base64)) {
            
                resultTable <- result[[paste("Error: subject", termsRM.base64[[i-1]], sep=":")]][[1]]
                
                modelTermsResults <- strsplit(gsub(" ", "", rownames(resultTable), fixed = TRUE), ":")
        
                for (j in .indices(terms.base64[[i]])) {
        
                    if (j == 1) {
                        newGroup <- TRUE   
                    } else {				
                        newGroup <- FALSE
                    }
                
                    modelTermsCase <- strsplit(terms.base64[[i]],":")[[j]]
                    index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                    
                    SS <- resultTable[index,"Sum Sq"]
                    df <- resultTable[index,"Df"]
                    MS <- resultTable[index,"Mean Sq"]
                    F <- resultTable[index,"F value"]
                    p <- resultTable[index,"Pr(>F)"]
                    dfR <- resultTable["Residuals","Df"]
                    
                    counter <- 0
                    
                    for (cor in corrections) {
                        
                        counter <- counter + 1
                        
                        if (!options$miscSphericityCorrections || cor == "empty") {
                                                        
                            if (!is.null(epsilon) && epsilon[i-1,"p"] < .05) {
                            
                                foot.index <- .addFootnote(footnotes, text="Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05).")
				                row.footnotes <- list(SS=list(foot.index), df=list(foot.index), MS=list(foot.index), F=list(foot.index), p=list(foot.index))
                        
                                row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
                            
                            } else {
                                
                                row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
                            
                            }
                            
                        } else if (cor == "None") {
                        
                            row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = (newGroup && counter == 1))
                            
                        } else if (cor == "Greenhouse-Geisser") {
                        
                            dfGG <- df * epsilon[i-1,"GG"]
                            MSGG <- SS / dfGG
                            dfRGG <- dfR * epsilon[i-1,"GG"]
                            pGG <-  pf(F,dfGG,dfRGG,lower.tail=FALSE)
                        
                            row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"=F, "p"=pGG, ".isNewGroup" = (newGroup && counter == 1))
                            
                        } else if (cor == "Huynh-Feldt") {
                            
                            dfHF <- df * epsilon[i-1,"HF"]
                            MSHF <- SS / dfHF
                            dfRHF <- dfR * epsilon[i-1,"HF"]
                            pHF <- pf(F,dfHF,dfRHF,lower.tail=FALSE)
                            
                            row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"=F, "p"=pHF, ".isNewGroup" = (newGroup && counter == 1))
                        
                        }
                        
                        
                        if (options$miscEffectSizeEstimates) {
                        
                            SSt <- sum(resultTable[,"Sum Sq"])
                            SSr <- resultTable["Residuals","Sum Sq"]
                            MSr <- SSr/resultTable["Residuals","Df"]
                                            
                            row[["eta"]] <- SS / SSt
                            row[["partialEta"]] <- SS / (SS + SSr)
                            omega <- (SS - (df * MSr)) / (SSt + MSr)
                
                            if (omega < 0) {
                                row[["omega"]] <- 0
                            } else {
                                row[["omega"]] <- omega
                            }
                        
				        }
				        
				        anova.rows[[length(anova.rows) + 1]] <- row
                    }
                }
                
                SS <- resultTable["Residuals","Sum Sq"]
                df <- resultTable["Residuals","Df"]
                MS <- resultTable["Residuals","Mean Sq"]
                
                counter <- 0
                
                for (cor in corrections) {
                    
                    counter <- counter + 1
                    
                    if (!options$miscSphericityCorrections || cor == "empty") {
                    
                        row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
                        
                    } else if (cor == "None") {
                        
                        
                        row <- list("case"="Residual", "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                        
                    } else if (cor == "Greenhouse-Geisser") {
                    
                        dfGG <- df * epsilon[i-1,"GG"]
                        MSGG <- SS / dfGG
                    
                        row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                        
                    } else if (cor == "Huynh-Feldt") {
                        
                        dfHF <- df * epsilon[i-1,"HF"]
                        MSHF <- SS / dfHF
                        
                        row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                    
                    }
                    
                    anova.rows[[length(anova.rows) + 1]] <- row
                    
                }
            }
            
        } else {
        
            result <- model
            
            modelTermsResults <- strsplit(rownames(result), ":")
            
            for (i in 2:length(terms.base64)) {
        
                for (j in .indices(terms.base64[[i]])) {
        
                    if (j == 1) {
                        newGroup <- TRUE   
                    } else {				
                        newGroup <- FALSE
                    }
                
                    modelTermsCase <- strsplit(terms.base64[[i]],":")[[j]]
                    index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                    
                    SS <- result[index,"SS"]
                    df <- result[index,"num Df"]
                    MS <- SS / df
                    F <- result[index,"F"]
                    p <- result[index,"Pr(>F)"]
                    dfR <- result[index,"den Df"]
                    
                    counter <- 0
                    
                    for (cor in corrections) {
                    
                        counter <- counter + 1

                        if (!options$miscSphericityCorrections || cor == "empty") {
                        
                            if (!is.null(epsilon) && epsilon[i-1,"p"] < .05) {
                            
                                foot.index <- .addFootnote(footnotes, text="Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05).")
				                row.footnotes <- list(SS=list(foot.index), df=list(foot.index), MS=list(foot.index), F=list(foot.index), p=list(foot.index))
                        
                                row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
                            
                            } else {
                                
                                row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
                            
                            }
                            
                        } else if (cor == "None") {
                        
                            row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = (newGroup && counter == 1))
                            
                        } else if (cor == "Greenhouse-Geisser") {
                        
                            dfGG <- df * epsilon[i-1,"GG"]
                            MSGG <- SS / dfGG
                            dfRGG <- dfR * epsilon[i-1,"GG"]
                            pGG <-  pf(F,dfGG,dfRGG,lower.tail=FALSE)
                        
                            row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"=F, "p"=pGG, ".isNewGroup" = (newGroup && counter == 1))
                            
                        } else if (cor == "Huynh-Feldt") {
                            
                            dfHF <- df * epsilon[i-1,"HF"]
                            MSHF <- SS / dfHF
                            dfRHF <- dfR * epsilon[i-1,"HF"]
                            pHF <- pf(F,dfHF,dfRHF,lower.tail=FALSE)
                            
                            row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"=F, "p"=pHF, ".isNewGroup" = (newGroup && counter == 1))
                        
                        }
                        
                        
                        if (options$miscEffectSizeEstimates) {
                        
                            modelTermsCases <- strsplit(terms.base64[[i]],":")
                            
                            indices <- c()
                            for (case in .indices(terms.base64[[i]])) {
                                indices <- c(indices, which(unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCases[[case]])))))
                            }
                            
                            SSr <- result[index,"Error SS"]
                            SSt <- sum(result[indices,"SS"]) + SSr
                            MSr <- SSr/result[index,"den Df"]
                                            
                            row[["eta"]] <- SS / SSt
                            row[["partialEta"]] <- SS / (SS + SSr)
                            omega <- (SS - (df * MSr)) / (SSt + MSr)
                
                            if (omega < 0) {
                                row[["omega"]] <- 0
                            } else {
                                row[["omega"]] <- omega
                            }
                        
				        }
				        
				        anova.rows[[length(anova.rows) + 1]] <- row
                    
                    }        
                }
                
                modelTermsCase <- unlist(strsplit(termsRM.base64[[i-1]], ":"))
                indexResidual <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                
                SS <- result[indexResidual,"Error SS"]
                df <- result[indexResidual,"den Df"]
                MS <- SS / df
                
                counter <- 0
                
                for (cor in corrections) {
                
                    counter <- counter + 1
                    
                    if (!options$miscSphericityCorrections || cor == "empty") {
                    
                        row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = TRUE)
                        
                    } else if (cor == "None") {
                        
                        
                        row <- list("case"="Residual", "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                        
                    } else if (cor == "Greenhouse-Geisser") {
                    
                        dfGG <- df * epsilon[i-1,"GG"]
                        MSGG <- SS / dfGG
                    
                        row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                        
                    } else if (cor == "Huynh-Feldt") {
                        
                        dfHF <- df * epsilon[i-1,"HF"]
                        MSHF <- SS / dfHF
                        
                        row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewGroup" = (counter == 1))
                    
                    }
                    
                    anova.rows[[length(anova.rows) + 1]] <- row
                    
                }            
            }            
        }
        
		anova[["data"]] <- anova.rows  
    
    }
    
    anova[["footnotes"]] <- as.list(footnotes)
    
    list(result = anova, status = status)
}

.sphericityTest <- function(dataset, options, perform, epsilon, epsilonError, mauchly, status) {
    
    sphericity <- list()
	    
	sphericity[["title"]] <- "Test of Sphericity"
	
	fields <- list(
		list(name="case", type="string", title=""),
		list(name="W", type="number", format="sf:4;dp:3", title="Mauchly's W"),
		list(name="p", type="number", format="dp:3;p:.001"),
		list(name="GG", type="number", format="sf:4;dp:3", title="Greenhouse-Geisser \u03B5"),
		list(name="HF", type="number", format="sf:4;dp:3", title="Huynh-Feldt \u03B5"))
	
	sphericity[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
	modelDef <- .rmModelFormula(options)
    termsRM.base64 <- modelDef$termsRM.base64
    termsRM.normal <- modelDef$termsRM.normal
    
    epsilonTable <- NULL
    
    if (perform == "init" || status$ready == FALSE || status$error || epsilonError) {
    
        sphericity.rows <- list()
        
        for(i in .indices(termsRM.base64)) {
        
            if (i == 1) {
                newGroup <- TRUE                
            } else {
                newGroup <- FALSE
            }
    
            row <- list("case"=termsRM.normal[[i]], "W"=".", "p"=".", "GG"=".", "HF"=".", ".isNewGroup" = newGroup)
		    sphericity.rows[[length(sphericity.rows) + 1]] <- row
            
        }
        
        sphericity[["data"]] <- sphericity.rows
        
        if (status$error)
            sphericity[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
            
        if (epsilonError)
            sphericity[["error"]] <- list(errorType="badData", errorMessage="Could not estimate sphericity parameters due to singularity problems.")
    
    } else {
    
        sphericity.rows <- list()
        
        epsilonTable <- as.data.frame(matrix(0, length(termsRM.base64), 3))
        colnames(epsilonTable) <- c("GG", "HF", "p")
        rownames(epsilonTable) <- termsRM.base64 
        
        if (is.null(epsilon)) {
            modelTermsResults <- list()
        } else {
            modelTermsResults <- strsplit(rownames(epsilon), ":")
        }
        
        for (i in .indices(termsRM.base64)) {
        
            if (i == 1) {
                newGroup <- TRUE                
            } else {
                newGroup <- FALSE
            }
            
            modelTermsCase <- unlist(strsplit(termsRM.base64[[i]],":"))
            index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
            
            if (sum(index) == 0) {
            
                foot.index <- .addFootnote(footnotes, text="The repeated measure only has two levels. When the repeated measure has two levels, the assumption of sphericity is always met.")
				row.footnotes <- list(W=list(foot.index), p=list(foot.index), GG=list(foot.index), HF=list(foot.index))
                
                epsilonTable[i,"GG"] <- 1
                epsilonTable[i,"HF"] <- 1
                epsilonTable[i,"p"] <- 1
                
                row <- list("case"=termsRM.normal[[i]], "W"=1, "p"=.clean(NaN), "GG"=1, "HF"=1, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
                
            } else {
                
                W <- mauchly[index,"Test statistic"]
                p <- mauchly[index,"p-value"]
                GG <- epsilon[index, "GG eps"]
                HF <- epsilon[index, "HF eps"]
                
                if (HF > 1)
                    HF <- 1
                    
                epsilonTable[i,"GG"] <- GG
                epsilonTable[i,"HF"] <- HF
                epsilonTable[i,"p"] <- p
                
                row <- list("case"=termsRM.normal[[i]], "W"=W, "p"=p, "GG"=GG, "HF"=HF, ".isNewGroup" = newGroup)
                
            }
            
		    sphericity.rows[[length(sphericity.rows) + 1]] <- row
            
        }
        
        sphericity[["data"]] <- sphericity.rows
    
    }
    
    sphericity[["footnotes"]] <- as.list(footnotes)
    
    if (options$miscSphericityTests == FALSE)
		return (list(result = NULL, epsilonTable = epsilonTable, status = status))
    
    list(result = sphericity, epsilonTable = epsilonTable, status = status)
}

.rmAnovaLevenesTable <- function(dataset, options, perform, status) {

	if (options$miscHomogeneityTests == FALSE)
		return (list(result=NULL, status=status))
		
	levenes.table <- list()
	
	levenes.table[["title"]] <- "Test for Equality of Variances (Levene's)"
	
	fields <- list(
	    list(name="case", type="string", title=""),
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="df1", type="number", format="dp:0"),
		list(name="df2", type="number", format="dp:0"),
		list(name="p", type="number", format="dp:3;p:.001"))

	levenes.table[["schema"]] <- list(fields=fields)
        
	if (perform == "run" && status$ready && status$error == FALSE && length(options$betweenSubjectFactors) > 0) {
        
        levenes.rows <- list()
        
        for (i in .indices(options$repeatedMeasuresCells)) {
            
            if (i == 1) {
                newGroup <- TRUE                
            } else {
                newGroup <- FALSE
            }            

            interaction <- paste(.v(options$betweenSubjectFactors), collapse=":", sep="")
		    levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", interaction)
		    levene.formula <- as.formula(levene.def)

		    r <- car::leveneTest(levene.formula, dataset, center = "mean")
		
		    row <- list("case"=options$repeatedMeasuresCells[i],"F"=r[1,2], "df1"=r[1,1], "df2"=r[2,1], "p"=r[1,3], ".isNewGroup"=newGroup)
            
            levenes.rows[[length(levenes.rows) + 1]] <- row
            
        }
        
        levenes.table[["data"]] <- levenes.rows
		
	} else {
	
	    levenes.rows <- list()
	    
	    for (i in .indices(options$repeatedMeasuresCells)) {

            if (i == 1) {
                newGroup <- TRUE                
            } else {
                newGroup <- FALSE
            }
            
            row <- list("case"=options$repeatedMeasuresCells[i], "F"=".", "df1"=".", "df2"=".", "p"=".", ".isNewGroup"=newGroup)
            levenes.rows[[length(levenes.rows) + 1]] <- row
            
	    }
	    
		levenes.table[["data"]] <- levenes.rows
	}
	
	if (status$error)
	    levenes.table[["error"]] <- list(error="badData")
	
	list(result=levenes.table, status=status)
}

.rmAnovaDescriptivesTable <- function(dataset, options, perform, status) {

	if (options$miscDescriptives == FALSE)
		return(list(result=NULL, status=status))
    
    rmFactors <- c()
    rmLevels <- list()
    
    for (i in .indices(options$repeatedMeasuresFactors)) {
    
        rmFactors[i] <- options$repeatedMeasuresFactors[[i]]$name
        rmLevels[[i]] <- options$repeatedMeasuresFactors[[i]]$levels
        
    }
    
    bsFactors <- c()
    bsLevels <- list()
    
    for (i in .indices(options$betweenSubjectFactors)) {
    
        bsFactors[i] <- options$betweenSubjectFactors[i]
        bsLevels[[i]] <- levels(dataset[[ .v(options$betweenSubjectFactors[i]) ]])
    
    }
    
    factors <- c(rmFactors, bsFactors)
    lvls <- c(rmLevels, bsLevels)
        
	descriptives.table <- list()
	    
	descriptives.table[["title"]] <- "Descriptives"
		
	fields <- list()
	
	for (variable in factors) {
	
		name <- paste(".", variable, sep="")  # in case variable is "Mean", "SD" or "N"
		fields[[length(fields)+1]] <- list(name=name, type="string", title=variable, combine=TRUE)
		
	}
	
	fields[[length(fields)+1]] <- list(name="Mean", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="SD", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="N", type="number", format="dp:0")
	
	descriptives.table[["schema"]] <- list(fields=fields)
		
	cases <- rev(expand.grid(rev(lvls)))
	
	namez <- unlist(factors)
	column.names <- paste(".", namez, sep="")
	
	if (length(factors) > 0) {
	
	    rows <- list()
        
        if (perform == "run" && status$ready && status$error == FALSE) {
            
            dataset <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)
            
            for (i in 1:dim(cases)[1]) {
	
                row <- list()
            
                for (j in 1:dim(cases)[2])
                    row[[ column.names[[j]] ]] <- as.character(cases[i, j])
            
                sub  <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse=" & ")))
            
                data <- base::subset(dataset, sub, select="dependent")[[1]]
            
                N <- base::length(data)

                row[["N"]] <- N
            
                if (N == 0) {
            
                    row[["Mean"]] <- ""
                    row[["SD"]]   <- ""
            
                } else if (N == 1) {
            
                    row[["Mean"]] <- data
                    row[["SD"]]   <- ""
            
                } else {
            
                    row[["Mean"]] <- base::mean(data)
                    row[["SD"]]   <- stats::sd(data)
                }
                
                print(cases)
                
                if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][[1]]) {
                    row[[".isNewGroup"]] <- TRUE   
                } else {				
                    row[[".isNewGroup"]] <- FALSE
                }
        
                rows[[i]] <- row
            }
            
        } else {
            
            for (i in 1:dim(cases)[1]) {
            
                row <- list()
            
                for (j in 1:dim(cases)[2])
                    row[[ column.names[[j]] ]] <- as.character(cases[i, j])
                
                if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][[1]]) {
                    row[[".isNewGroup"]] <- TRUE   
                } else {				
                    row[[".isNewGroup"]] <- FALSE
                }
        
                rows[[i]] <- row
            }
        }
        
        descriptives.table[["data"]] <- rows		
	} 
	
	if (status$error)
	    descriptives.table[["error"]] <- list(error="badData")
	
	list(result=descriptives.table, status=status)
}
