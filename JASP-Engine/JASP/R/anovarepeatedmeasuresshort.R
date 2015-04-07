
AnovaRepeatedMeasuresShort <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

    numeric.variables <- c(unlist(options$dependent))
    numeric.variables <- numeric.variables[numeric.variables != ""]
    factor.variables <- c(unlist(options$repeatedMeasuresCells),unlist(options$betweenSubjectFactors))
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
		list(name="anova", type="table"),
		list(name="headerSphericity", type="h1"),
		list(name="sphericity", type="table")
	)
	
	results[[".meta"]] <- .meta
	
	
	
	## Create Title

	results[["title"]] <- "Repeated Measures ANOVA"
	
	
	status <- .anovaMixedCheck(dataset, options, perform)
	
	
	
	## Perform ANOVA

	model <- NULL
	epsilon <- NULL
	mauchly <- NULL
	if (perform == "run" && status$ready && status$error == FALSE) {
		
		anovaModel <- .anovaMixedModel(dataset, options, status)
		model <- anovaModel$model
		epsilon <- anovaModel$epsilon
		mauchly <- anovaModel$mauchly
		status <- anovaModel$status
	
	}
    
    
    
    ## Create Sphericity Assumption Table
    
    result <- .sphericityTest(dataset, options, perform, epsilon, mauchly, status) 
    
    results[["sphericity"]] <- result$result
	status <- result$status
	
	if (options$miscSphericityTests)
	    results[["headerSphericity"]] <- "Test of Sphericity"



	## Create ANOVA Table

	result <- .anovaMixedTable(dataset, options, perform, model, epsilon, mauchly, status)
	
	results[["anova"]] <- result$result
	status <- result$status
	
	results
}

.anovaMixedCheck <- function(dataset, options, perform) {

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

.modelMixedFormula <- function(options) {

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

.anovaMixedModel <- function(dataset, options, status) {
	
	modelDef <- .modelMixedFormula(options)
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
	
	epsilon <- NULL
	mauchly <- NULL
	
    if (length(class(result)) == 1 && class(result) == "try-error") {
    
        status$error <- TRUE
        status$errorMessage <- .extractErrorMessage(model)
        
    } else {
        
        if (options$sumOfSquares == "type1" && class(sphericityStatistics) != "try-error") {
            
            model <- summary(result)
            epsilon <- sphericityStatistics$sphericity.correction
            mauchly <- sphericityStatistics$mauchly
        
        } else {
            
            model <- result$anova
            epsilon <- result$sphericity.correction
            mauchly <- result$mauchly
        
        }
    }

	list(model = model, epsilon = epsilon, mauchly = mauchly, status = status)
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

.anovaMixedTable <- function(dataset, options, perform, model, epsilon, mauchly, status) {

    anova <- list()
	    
	anova[["title"]] <- "Repeated Measures ANOVA"
	
	fields <- list()
	
	fields[[length(fields) + 1]] <- list(name="case", type="string", title="", combine=TRUE)
	
	if (options$miscSphericityCorrections)
	    fields[[length(fields) + 1]] <- list(name="cor", type="string", title="Sphericity Correction")
	
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
    	
	modelDef <- .modelMixedFormula(options)
	terms.normal <- modelDef$terms.normal 
    terms.base64 <- modelDef$terms.base64
    termsRM.base64 <- modelDef$termsRM.base64
    
    if (perform == "init" || status$ready == FALSE || status$error) {
    
        anova.rows <- list()
        
        for (i in .indices(terms.base64)) {
            
            if (i == 1) {
			
				anova.rows[[length(anova.rows) + 1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
				
			} else if (i == 2) {
			
				anova.rows[[length(anova.rows) + 1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
				
			}
            
            for (j in .indices(terms.base64[[i]])) {
            
                if (j == 1) {
			        newGroup <- TRUE   
			    } else {				
				    newGroup <- FALSE
			    }

			    if (options$miscSphericityCorrections && i != 1) {
			        
			        row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup, .rowLevel=1)
			        anova.rows[[length(anova.rows) + 1]] <- row
			        
			        if (options$sphericityGreenhouseGeisser) {
			            row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = FALSE, .rowLevel=1)
			            anova.rows[[length(anova.rows) + 1]] <- row
			        } 
			        
			        if (options$sphericityHuynhFeldt) {
			            row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = FALSE, .rowLevel=1)
			            anova.rows[[length(anova.rows) + 1]] <- row
			        }
			        
			    } else {
			    
			        row <- list("case"=terms.normal[[i]][j], "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup, .rowLevel=1)
			        anova.rows[[length(anova.rows) + 1]] <- row
			        
			    }
            }
            
            if (options$miscSphericityCorrections && i != 1) {
                
                row <- list("case"="Residual", "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", ".isNewGroup" = newGroup, .rowLevel=1)
                anova.rows[[length(anova.rows) + 1]] <- row
                
                if (options$sphericityGreenhouseGeisser) {
                    row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", ".isNewGroup" = FALSE, .rowLevel=1)
                    anova.rows[[length(anova.rows) + 1]] <- row
                } 
                
                if (options$sphericityHuynhFeldt) {
                    row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", ".isNewGroup" = FALSE, .rowLevel=1)
                    anova.rows[[length(anova.rows) + 1]] <- row
                }
                
            } else {
            
                row <- list("case"="Residual", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", ".isNewGroup" = FALSE, .rowLevel=1)
                anova.rows[[length(anova.rows) + 1]] <- row
                
            }   
		}

		anova[["data"]] <- anova.rows
		
		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
    
    } else {
    
        anova.rows <- list()
        
        if (options$sumOfSquares == "type1") {
        
            result <- model
            
            for (i in .indices(terms.base64)) {
        
                if (i == 1) {
        
                    anova.rows[[length(anova.rows) + 1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
                    resultTable <- result[["Error: subject"]][[1]]
            
                } else if (i == 2) {
        
                    anova.rows[[length(anova.rows) + 1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
                    resultTable <- result[[paste("Error: subject", termsRM.base64[[i-1]], sep=":")]][[1]]
                
                } else {
                
                    resultTable <- result[[paste("Error: subject", termsRM.base64[[i-1]], sep=":")]][[1]]
                    
                }
                
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
                
                    row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .rowLevel=1)
                    
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
                
                row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", ".isNewGroup" = FALSE, .rowLevel=1)
                
                if (options$miscEffectSizeEstimates) {
                                        
                    row[["eta"]] <- ""
                    row[["partialEta"]] <- ""
                    row[["omega"]] <- ""
                    
				}
                
                anova.rows[[length(anova.rows) + 1]] <- row
            
            }
            
        } else {
        
            result <- model
            
            modelTermsResults <- strsplit(rownames(result), ":")
            
            for (i in .indices(terms.base64)) {
        
                if (i == 1) {
        
                    anova.rows[[length(anova.rows) + 1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
            
                } else if (i == 2) {
        
                    anova.rows[[length(anova.rows) + 1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
                
                } 
        
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
                
                    row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .rowLevel=1)

                    if (options$miscEffectSizeEstimates) {
                        
                        modelTermsCases <- strsplit(terms.base64[[i]],":")
                        
                        indices <- c()
                        for (case in .indices(terms.base64[[i]])) {
	                        indices <- c(indices, which(unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCases[[case]])))))
                        }
                        
                        print(indices)
                        print(result)
                        
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
                
                if (i == 1) {
                
                    indexResidual <- 2
                    
                } else {
                
                    modelTermsCase <- unlist(strsplit(termsRM.base64[[i-1]], ":"))
                    indexResidual <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
                    
                }
                
                SS <- result[indexResidual,"Error SS"]
                df <- result[indexResidual,"den Df"]
                MS <- SS / df
                
                row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", ".isNewGroup" = FALSE, .rowLevel=1)
                
                if (options$miscEffectSizeEstimates) {
                                        
                    row[["eta"]] <- ""
                    row[["partialEta"]] <- ""
                    row[["omega"]] <- ""
                    
				}
                
                anova.rows[[length(anova.rows) + 1]] <- row
            
            }            
        
        }
        
		anova[["data"]] <- anova.rows  
    
    }
    
    anova[["footnotes"]] <- as.list(footnotes)
    
    list(result = anova, status = status)
}

.sphericityTest <- function(dataset, options, perform, epsilon, mauchly, status) {

    if (options$miscSphericityTests == FALSE)
		return (list(result=NULL, status=status))
    
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
	
	modelDef <- .modelMixedFormula(options)
    termsRM.base64 <- modelDef$termsRM.base64
    termsRM.normal <- modelDef$termsRM.normal
    
    if (perform == "init" || status$ready == FALSE || status$error) {
    
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
    
    } else {

        sphericity.rows <- list()
        
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
            
                foot.index <- .addFootnote(footnotes, text="The repeated measure only has two levels (when the repeated measure has two levels, the assumption of sphericity is always met).")
				row.footnotes <- list(W=list(foot.index), p=list(foot.index), GG=list(foot.index), HF=list(foot.index))
                
                row <- list("case"=termsRM.normal[[i]], "W"=1, "p"=.clean(NaN), "GG"=1, "HF"=1, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
                
            } else {
                
                W <- mauchly[index,"Test statistic"]
                p <- mauchly[index,"p-value"]
                GG <- epsilon[index, "GG eps"]
                HF <- epsilon[index, "HF eps"]
                
                if (HF > 1)
                    HF <- 1
                
                row <- list("case"=termsRM.normal[[i]], "W"=W, "p"=p, "GG"=GG, "HF"=HF, ".isNewGroup" = newGroup)
                
            }
            
		    sphericity.rows[[length(sphericity.rows) + 1]] <- row
            
        }
        
        sphericity[["data"]] <- sphericity.rows
    
    }
    
    sphericity[["footnotes"]] <- as.list(footnotes)
    
    list(result = sphericity, status = status)
}
