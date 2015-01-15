
Ancova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	numeric.variables <- c(unlist(options$dependent),unlist(options$covariates),unlist(options$wlsWeight))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors),unlist(options$repeatedMeasures))
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
	
	
	# META definitions

	.meta <- list(
		list(name="anova", type="table"),
		list(name="levene", type="table"),
		list(name="contrasts", type="tables"),
		list(name="posthoc", type="tables"),
		list(name="descriptives", type="table"),
		list(name="profilePlot", type="images")
	)

	results[[".meta"]] <- .meta
	
	
	status <- .anovaCheck(dataset, options, perform)
	


	## Setup Contrasts

	if (perform == "run" && status$ready && status$error == FALSE)
		dataset <- .anovaSetupContrasts(dataset, options)
	


	## Perform ANOVA

	model <- NULL
	singular <- NULL
	if (perform == "run" && status$ready && status$error == FALSE) {
		
		anovaModel <- .anovaModel(dataset, options)
		model <- anovaModel$model
		singular <- anovaModel$singular
	
	}

	## Create ANOVA Table

	result <- .anovaTable(dataset, options, perform, model, status, singular)
	
	results[["anova"]] <- result$result
	status <- result$status
		
		
				
	## Create Contrasts Table
	
	result <- .anovaContrastsTable(dataset, options, perform, model, status)
	
	results[["contrasts"]] <- result$result
	status <- result$status
	

	
	## Create Post Hoc Table
	
	result <- .anovaPostHocTable(dataset, options, perform, status)
	
	results[["posthoc"]] <- result$result
	status <- result$status
	

	
	## Create Descriptives Table
	
	result <- .anovaDescriptivesTable(dataset, options, perform, status)
	
	results[["descriptives"]] <- result$result
	status <- result$status
	


	## Create Levene's Table
	
	result <- .anovaLevenesTable(dataset, options, perform, status)
	
	results[["levene"]] <- result$result
	status <- result$status
	
	
	
	## Create Profile Plots
	
	results[["profilePlot"]] <- .anovaProfilePlot(dataset, options, perform, status)
	
	results
}

.anovaContrastCases <- function(column, contrast.type) {

	levels <- levels(column)
	n.levels <- length(levels)
	
	cases <- list()
	
	if (n.levels == 1) {
	
	    cases[[1]] <- "."
	
	} else if (contrast.type == "deviation") {

		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i + 1], " - ", paste(levels,collapse=", "), sep="")
		
	} else if (contrast.type == "simple") {
	   
		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i+1], " - ", levels[1], sep="")
				   
	} else if (contrast.type == "helmert") {
		
		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i], " - ", paste(levels[-(1:i)], collapse=", "), sep="")
		
	} else if (contrast.type == "repeated") {
		
		for (i in 1:(n.levels-1))
			cases[[i]] <- paste(levels[i], " - ", levels[i+1], sep="")
		
	} else if (contrast.type=="difference") {
		
		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i + 1], " - ", paste(levels[1:i], collapse=", "), sep="")

	} else if (contrast.type == "polynomial") {
		
		poly.names <- c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic", "septic", "octic")
		for (i in 1:(n.levels - 1)) {
			if (i <= 8) {
				cases[[i]] <- poly.names[i]
			} else {
				cases[[i]] <- paste("degree", i, "polynomial", sep=" ")
			}
		}
	} 
		
	cases
}

.anovaCreateContrast <- function (column, contrast.type) {

	levels <- levels(column)
	n.levels <- length(levels)
	
	contr <- NULL
	
	if (contrast.type == "none") {
		
		options(contrasts = c("contr.sum","contr.poly"))
		contr <- NULL
		
	} else if (contrast.type == "deviation") {
		
		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
		
		for (i in 1:n.levels-1) {
			contr[c(1,i+1),i]<- c(1,-1)  
		}
		
	} else if (contrast.type == "simple") {
		
		treatment <- contr.treatment(levels)

		coding <- matrix(rep(1 / n.levels, prod(dim(treatment))), ncol=n.levels - 1)
		contr <- (treatment-coding)*n.levels
				   
	} else if (contrast.type == "helmert") {
		
		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
		
		for (i in 1:(n.levels - 1)) {
		
			k <- 1 / (n.levels - (i - 1))
			contr[i:n.levels,i] <- c(k * (n.levels - i), rep(-k, n.levels - i))
		}
		
	} else if (contrast.type == "repeated") {
		
		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
		
		for (i in 1:n.levels-1) {
		
			contr[1:i,i] <- (n.levels-i)/n.levels
			contr[(i+1):n.levels,i] <- -i/n.levels
		}
		
	} else if (contrast.type=="difference") {
		
		contr <- contr.helmert(levels)

	} else if (contrast.type == "polynomial") {
		
		contr <- contr.poly(levels)
	}
	
	if ( ! is.null(contr))
		dimnames(contr) <- list(NULL, 1:dim(contr)[2])
	
	contr
}

.anovaCheck <- function(dataset, options, perform) {

	error <- FALSE
	errorMessage <- NULL
	ready <- options$dependent != "" && length(options$modelTerms) > 0
	
	if (ready && perform == "run") {
	
		components <- unique(unlist(options$modelTerms))
		independentsWithLessThanTwoLevels <- c()
		
		for (component in components) {
		
			column <- dataset[[ .v(component) ]]
			if (length(unique(column)) < 2)
				independentsWithLessThanTwoLevels <- c(independentsWithLessThanTwoLevels, component)
		}
		
		if (length(independentsWithLessThanTwoLevels) > 0) {
		
			error <- TRUE
			errorMessage <- paste("Factor(s): <em>", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""), "</em>, contain(s) less than two levels.<br><br>(Possibly only after rows with missing values are excluded)", sep="")
		}
		
		if (sum(is.infinite(dataset[[ .v(options$dependent) ]])) > 0) {
		
			error <- TRUE
			errorMessage <- paste("The dependent variable: <em>", options$dependent, "</em>, contains infinite values.<br><br>(Possibly only after rows with infinite values are excluded)", sep="")
		}
		
		if (sum(dataset[[ .v(options$wlsWeights) ]] <= 0) > 0) {
		
			error <- TRUE
			errorMessage <- paste("The variable: <em>", options$wlsWeights, "</em>, contains negative and/or zero values.<br><br>(only positive WLS weights allowed)", sep="")
		}
		
		if (sum(is.infinite(dataset[[ .v(options$wlsWeights) ]])) > 0) {
		
			error <- TRUE
			errorMessage <- paste("The variable: <em>", options$wlsWeights, "</em>, contains infinite values.<br><br>(Possibly only after rows with infinite values are excluded)", sep="")
		}
		
	}
	
	list(ready=ready, error=error, errorMessage=errorMessage)
}

.anovaSetupContrasts <- function(dataset, options) {

	for (contrast in options$contrasts) {

		v <- .v(contrast$variable)
		
		column <- dataset[[v]]
		contrasts(column) <- .anovaCreateContrast(column, contrast$contrast)
		dataset[[v]] <- column			
	}
	
	dataset
}

.reorderModelTerms <- function(options) {
        
    if(length(options$modelTerms) > 0) {
    
        fixedFactors <- list()
        covariates <- list()
    
        k <- 1
        l <- 1
    
    
        for(i in 1:length(options$modelTerms)) {
            if (sum(unlist(options$modelTerms[[i]]$components) %in% options$covariates) > 0) {
                covariates[[k]] <- options$modelTerms[[i]]
                k <- k + 1
            } else {
                fixedFactors[[l]] <- options$modelTerms[[i]]
                l <- l + 1
            }
        }
        
        if(length(covariates) > length(options$covariates)) {
            modelTerms <- options$modelTerms
            interactions <- TRUE
        } else {
            modelTerms <- c(fixedFactors,covariates)
            interactions <- FALSE
        }
    
    } else {
    
        modelTerms <- list()
        interactions <- FALSE
    }	
    
    list(modelTerms = modelTerms, interactions = interactions)
}

.modelFormula <- function(modelTerms, options) {

    dependent.normal <- options$dependent
	dependent.base64 <- .v(options$dependent)
	
	terms.base64 <- c()
	terms.normal <- c()
		        
	for (term in modelTerms) {
        
		components <- unlist(term$components)
		term.base64 <- paste(.v(components), collapse=":", sep="")
		term.normal <- paste(components, collapse="\u2009*\u2009", sep="")

		terms.base64 <- c(terms.base64, term.base64)
		terms.normal <- c(terms.normal, term.normal)
	}
	
	model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))
	
	list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)   
}

.anovaModel <- function(dataset, options) {
	
	reorderModelTerms <-  .reorderModelTerms(options)
	modelTerms <- reorderModelTerms$modelTerms
	
	modelDef <- .modelFormula(modelTerms, options)
	model.formula <- as.formula(modelDef$model.def)
	
	WLS <- NULL
	if ( ! is.null(options$wlsWeights))
		WLS <- dataset[[ .v(options$wlsWeights) ]]
		
	model <- aov(model.formula, dataset, weights=WLS)
	
	modelError <- class(try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = FALSE))) == "try-error"
	errorMessage <- ""
	
	if (modelError) 
	    errorMessage <- .extractErrorMessage(try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = FALSE)))
	
	singular <- FALSE
	if (errorMessage == "singular fit encountered")
	    singular <- TRUE

	list(model = model, singular = singular)
}

.anovaTable <- function(dataset, options, perform, model, status, singular) {
	
	anova <- list()
	
	if (is.null(options$covariates)) {
	
	    if (options$dependent != "") {
	    
	        anova[["title"]] <- paste("ANOVA - ", options$dependent, sep = "")
	    
	    } else {
	    
	        anova[["title"]] <- "ANOVA"
	    
	    }
	    
	} else {
	    
	    if (options$dependent != "") {
	    
	        anova[["title"]] <- paste("ANCOVA - ", options$dependent, sep = "")
	    
	    } else {
	    
	        anova[["title"]] <- "ANCOVA"
	    
	    }	
	}
	
	fields <- list(
		list(name="Cases", type="string"),
		list(name="Sum of Squares", type="number", format="sf:4;dp:3"),
		list(name="df", type="number", format="dp:0"),
		list(name="Mean Square", type="number", format="sf:4;dp:3"),
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
		
	if (options$misc[["effectSizeEstimates"]]) {
	
		fields[[length(fields) + 1]] <- list(name="&eta;&sup2;", type="number", format="dp:3")
		fields[[length(fields) + 1]] <- list(name="&omega;&sup2;", type="number", format="dp:3")
	}
	
	anova[["schema"]] <- list(fields=fields)
	
	reorderModelTerms <-  .reorderModelTerms(options)
	modelTerms <- reorderModelTerms$modelTerms
	
	modelDef <- .modelFormula(modelTerms, options)
    terms.normal <- modelDef$terms.normal
    terms.base64 <- modelDef$terms.base64
	
	footnotes <- .newFootnotes()
    
    if (options$sumOfSquares == "type1") {
    
        .addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")
						
	} else if (options$sumOfSquares == "type2") {
			
        .addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {
			
        .addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}	
	
	
	if (perform == "init" || status$ready == FALSE || status$error) {
	
		anova.rows <- list()

		for (i in .indices(terms.normal)) {
		
		    if(i == 1 || (!is.null(unlist(options$covariates)) && terms.normal[i] == options$covariates[[1]] && !reorderModelTerms$interactions)) {
			    newGroup <- TRUE   
			} else {				
				newGroup <- FALSE
			}

			row <- list("Cases"=terms.normal[i], "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
			anova.rows[[length(anova.rows) + 1]] <- row
		}

		row <- list("Cases"="Residual", "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".", ".isNewGroup" = TRUE)
		anova.rows[[length(anova.rows) + 1]] <- row

		anova[["data"]] <- anova.rows
		
		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
	
	
	} else {
			
		anova.rows <- try (silent = FALSE, expr = {
			
			rows <- list()
					
			if (options$sumOfSquares == "type1") {
			    
				result <- base::tryCatch(stats::anova(model),error=function(e) e, warning=function(w) w)
				
				if (!is.null(result$message) && result$message == "ANOVA F-tests on an essentially perfect fit are unreliable")
				    stop(result$message)	
				    
				SSt <- sum(result[,"Sum Sq"], na.rm = TRUE)
							
			} else if (options$sumOfSquares == "type2") {
			
				result <- car::Anova(model, type=2)
				SSt <- sum(result[,"Sum Sq"], na.rm = TRUE)

			} else if (options$sumOfSquares == "type3") {
			
				result <- car::Anova(model, type=3, singular.ok=TRUE)
				SSt <- sum(result[-1,"Sum Sq"], na.rm = TRUE)
				
			}

			for (i in 1:(length(terms.base64)+1)) {
			
				if (i <= length(terms.base64)) {
					term <- terms.base64[i]
				} else {
					term <- "Residuals"
				}
			
				df <- result[term,"Df"]
				
				if (is.na(df) || df == 0) {
				    SS <- 0
				    df <- 0
					MS <- ""
				} else {
					SS <- result[term,"Sum Sq"]
					MS <- result[term,"Sum Sq"]/result[term,"Df"]
				}
				
				F <- if (is.na(result[term,"F value"])) {""} else { result[term, "F value"] }
				p <- if (is.na(result[term,"Pr(>F)"] )) {""} else { result[term, "Pr(>F)"] }
				
				if(i == 1 || term == "Residuals" || 
				   (!is.null(unlist(options$covariates)) && terms.normal[i] == options$covariates[[1]] && !reorderModelTerms$interactions)) {
				    newGroup <- TRUE   
				} else {				
				    newGroup <- FALSE
				}
							
				if (i <= length(terms.base64)) {
					row <- list("Cases"=terms.normal[i], "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
				} else {
					row <- list("Cases"="Residual", "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"="", "p"="", ".isNewGroup" = newGroup)
				}
			
				if (options$misc[["effectSizeEstimates"]]) {
					MSr <- result["Residuals","Sum Sq"]/result["Residuals","Df"]
				
					if (i <= length(terms.base64)) {
					
						row[["&eta;&sup2;"]] <- SS / SSt
						omega <- (SS - (df * MSr)) / (SSt + MSr)
					
						if (omega < 0) {
							row[["&omega;&sup2;"]] <- 0
						} else {
							row[["&omega;&sup2;"]] <- omega
						}
					
					} else {
					
						row[["&eta;&sup2;"]] <- ""
						row[["&omega;&sup2;"]] <- ""
					}

				}
			
				rows[[length(rows) + 1]] <- row
			}
			
			rows
		})
		
		if (class(anova.rows) == "try-error") {
		
		    errorMessage <- .extractErrorMessage(anova.rows)
						
			if (errorMessage == "NA/NaN/Inf in foreign function call (arg 1)" || errorMessage == "undefined columns selected" ||
			    errorMessage == "ANOVA F-tests on an essentially perfect fit are unreliable") {
				
				errorMessage <- "Residual sums of squares and/or residual degrees of freedom are equal to zero indicating perfect fit.<br><br>(ANOVA F-tests on an essentially perfect fit are unreliable)"
										
			}
		
			status$error <- TRUE
			status$errorMessage <- errorMessage
			
			anova[["error"]] <- list(errorType="badData", errorMessage = errorMessage)

			anova.rows <- list()
			
			for (i in .indices(terms.normal)) {
				row <- list("Cases"=terms.normal[i], "Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
				anova.rows[[length(anova.rows) + 1]] <- row
			}
		}
		
		anova[["data"]] <- anova.rows
        			
		if (singular)
		    .addFootnote(footnotes, text = "Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables", symbol = "<em>Warning.</em>")
							
	}
	
	anova[["footnotes"]] <- as.list(footnotes)
	
	list(result=anova, status=status)
}

.anovaContrastsTable <- function(dataset, options, perform, model, status) {

	no.contrasts <- TRUE

	for (contrast in options$contrasts) {
	
		if (contrast$contrast != "none")
			no.contrasts <- FALSE
	}
	
	if (no.contrasts)
		return(list(result=NULL, status=status))
	
	contrast.tables <- list()
	
	if (perform == "run" && status$ready && status$error == FALSE)
		contrast.summary <- summary.lm(model)[["coefficients"]]
	
	
	for (contrast in options$contrasts) {
	
		if (contrast$contrast != "none") {
		
			variable <- contrast$variable
			contrast.type <- contrast$contrast
			
			contrast.table <- list()
			
			contrast.table[["title"]] <- paste("Contrast", contrast.type, variable, sep=" ")
			
			contrast.table[["schema"]] <- list(fields = list(
				list(name="Comparison", type="string"),
				list(name="Estimate", type="number", format="sf:4;dp:3"),
				list(name="Std. Error", type="number", format="sf:4;dp:3"),
				list(name="t", type="number", format="sf:4;dp:3"),
				list(name="p", type="number", format="dp:3;p:.001")))
			
			v <- .v(variable)

			column <- dataset[[ v ]]
			
			cases <- .anovaContrastCases(column, contrast.type)
			
			if (contrast == "polynomial" && length(cases) > 5)
				cases <- cases[1:5]				
			
			contrast.rows <- list()
			
			if (perform == "init" || status$error || !status$ready) {
			
				for (case in cases)
					contrast.rows[[length(contrast.rows)+1]] <- list(Comparison=case)			
			
			} else {
								
				for (i in .indices(cases)) {
				
					case <- cases[[i]]
										
					nam <- paste(v, i, sep="")
					
					est <- contrast.summary[nam,"Estimate"]
					SE  <- contrast.summary[nam,"Std. Error"]
					t   <- contrast.summary[nam,"t value"]
					p   <- contrast.summary[nam,"Pr(>|t|)"]
					
					if (is.na(p))
						p <- ""
				
					row <- list("Comparison"=case, "Estimate"=est, "Std. Error"=SE, "t"=t, "p"=p)
				
					contrast.rows[[length(contrast.rows)+1]] <- row
				}
			}

			contrast.table[["data"]] <- contrast.rows
			
			if (status$error)
				contrast.table[["error"]] <- list(errorType="badData")
		
			contrast.tables[[length(contrast.tables)+1]] <- contrast.table
		}
	}
	
	list(result=contrast.tables, status=status)
}

.anovaPostHocTable <- function(dataset, options, perform, status) {

	posthoc.variables <- unlist(options$postHocTests[["variables"]])
	
	posthoc.tables <- list()
	
	for (posthoc.var in posthoc.variables) {
	
		posthoc.table <- list()

		posthoc.table[["title"]] <- paste("Post-Hoc Comparisons", posthoc.var, sep=" ")
		
		fields <- list(
			list(name="(I) response", type="string", combine=TRUE),
			list(name="(J) response", type="string"),
			list(name="Mean Difference", type="number", format="sf:4;dp:3"),
			list(name="t", type="number", format="sf:4;dp:3"),
			list(name="df", type="number", format="dp:0"),
			list(name="p", type="number", format="dp:3;p:.001"))
		
		if (options$postHocTests[["holm"]])
			fields[[length(fields) + 1]] <- list(name="p holm", type="number", format="dp:3")
		
		if (options$postHocTests[["bonferroni"]])
			fields[[length(fields) + 1]] <- list(name="p bonferroni", type="number", format="dp:3")
		
		if (options$postHocTests[["hochberg"]])
			fields[[length(fields) + 1]] <- list(name="p hochberg", type="number", format="dp:3")
		
		if (options$postHocTests[["hommel"]])
			fields[[length(fields) + 1]] <- list(name="p hommel", type="number", format="dp:3")
		
		if (options$postHocTests[["benjamini"]])
			fields[[length(fields) + 1]] <- list(name="p benjamini", type="number", format="dp:3")
		
		if (options$postHocTests[["FDR"]])
			fields[[length(fields) + 1]] <- list(name="p FDR", type="number", format="dp:3")
		
		posthoc.table[["schema"]] <- list(fields=fields)

		rows <- list()
		
		if (perform == "run" && status$ready && status$error == FALSE)
			data.split <- split(dataset[[ .v(options$dependent) ]], dataset[[ .v(posthoc.var) ]])
			
			
		variable.levels <- levels(dataset[[ .v(posthoc.var) ]])
		
		ps <- c()
		
		for (i in 1:length(variable.levels)) {
			
			for (j in .seqx(i+1, length(variable.levels))) {
				
				row <- list("(I) response"=variable.levels[[i]], "(J) response"=variable.levels[[j]])
				
				if (perform == "run" && status$ready && status$error == FALSE) {

					r <- try(t.test(data.split[[i]], data.split[[j]]))
					
					if (class(r) == "try-error" || is.na(r$p.value)) {
					
						md <- ""
						t  <- ""
						df <- ""
						p  <- 1
						
						posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Some comparisons could not be performed. Possibly too few samples."))
						
					} else {
					
						md <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
						t  <- as.numeric(r$statistic)
						df <- as.numeric(r$parameter)
						p <- as.numeric(r$p.value)
					}
					
					row[["Mean Difference"]] <- md
					row[["t"]]  <- t
					row[["df"]] <- df
					row[["p"]]  <- p
					
					ps <- c(ps, p)
				}
				
				rows[[length(rows)+1]] <- row
			}
		}
		
		if (perform == "run" && status$ready && status$error == FALSE) {
		
			if (options$postHocTests[["holm"]]) {
		
				ps.adjusted <- p.adjust(ps, method="holm")
				for (i in 1:length(rows))
					rows[[i]][["p holm"]] <- ps.adjusted[[i]]
			}

			if (options$postHocTests[["bonferroni"]]) {
		
				ps.adjusted <- p.adjust(ps, method="bonferroni")
				for (i in 1:length(rows))
					rows[[i]][["p bonferroni"]] <- ps.adjusted[[i]]
			}
		
			if (options$postHocTests[["hochberg"]]) {
		
				ps.adjusted <- p.adjust(ps, method="hochberg")
				for (i in 1:length(rows))
					rows[[i]][["p hochberg"]] <- ps.adjusted[[i]]
			}
		
			if (options$postHocTests[["hommel"]]) {
		
				ps.adjusted <- p.adjust(ps, method="hommel")
				for (i in 1:length(rows))
					rows[[i]][["p hommel"]] <- ps.adjusted[[i]]
			}		
		
			if (options$postHocTests[["benjamini"]]) {
		
				ps.adjusted <- p.adjust(ps, method="BY")
				for (i in 1:length(rows))
					rows[[i]][["p benjamini"]] <- ps.adjusted[[i]]
			}
		
			if (options$postHocTests[["FDR"]]) {
		
				ps.adjusted <- p.adjust(ps, method="fdr")
				for (i in 1:length(rows))
					rows[[i]][["p FDR"]] <- ps.adjusted[[i]]
			}
		}
		
		posthoc.table[["data"]] <- rows
		
		if (status$error)
			posthoc.table[["error"]] <- list(errorType="badData")
		
		posthoc.tables[[length(posthoc.tables)+1]] <- posthoc.table
	}
	
	list(result=posthoc.tables, status=status)
}

.anovaDescriptivesTable <- function(dataset, options, perform, status) {

	if (options$misc$descriptives == FALSE)
		return(list(result=NULL, status=status))

	descriptives.table <- list()
	
	descriptives.table[["title"]] <- "Descriptives"
	
	fields <- list()
	
	for (variable in options$fixedFactors) {
	
		name <- paste(".", variable, sep="")  # in case variable is "Mean", "SD" or "N"
		fields[[length(fields)+1]] <- list(name=name, type="string", title=variable, combine=TRUE)
	}
	
	fields[[length(fields)+1]] <- list(name="Mean", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="SD", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="N", type="number", format="dp:0")
	
	descriptives.table[["schema"]] <- list(fields=fields)
	
	lvls <- list()
	factors <- list()

	for (variable in options$fixedFactors) {
	
		factor <- dataset[[ .v(variable) ]]
		factors[[length(factors)+1]] <- factor
		lvls[[length(lvls)+1]] <- levels(factor)
	}
		
	cases <- expand.grid(rev(lvls))
	
	
	if (length(options$fixedFactors) > 0) {
	
		namez <- rev(unlist(options$fixedFactors))
		column.names <- paste(".", namez, sep="")

		rows <- list()
	
		for (i in 1:dim(cases)[1]) {
	
			row <- list()

			for (j in 1:dim(cases)[2])
				row[[ column.names[[j]] ]] <- as.character(cases[i, j])
				
			if (perform == "run" && status$ready && status$error == FALSE) {
			
				sub  <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse=" & ")))
				
				data <- base::subset(dataset, sub, select=.v(options$dependent))[[1]]
				
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
			}
		
			rows[[i]] <- row
		}
		
		descriptives.table[["data"]] <- rows
	}
	
	if (status$error)
	    descriptives.table[["error"]] <- list(error="badData")
	
	list(result=descriptives.table, status=status)
}

.anovaLevenesTable <- function(dataset, options, perform, status) {

	if (options$misc[["homogeneityTests"]] == FALSE)
		return (list(result=NULL, status=status))
		
	levenes.table <- list()
	
	levenes.table[["title"]] <- "Levene's Test for Homogeneity of Variance"
	
	fields <- list(
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="df1", type="number", format="dp:0"),
		list(name="df2", type="number", format="dp:0"),
		list(name="p", type="number", format="dp:3;p:.001"))

	levenes.table[["schema"]] <- list(fields=fields)

	if (perform == "run" && status$ready && status$error == FALSE) {

		interaction <- paste(.v(options$fixedFactors), collapse=":", sep="")
		levene.def <- paste(.v(options$dependent), "~", interaction)
		levene.formula <- as.formula(levene.def)

		r <- car::leveneTest(levene.formula, dataset, center = "mean")

		levenes.table[["data"]] <- list(list("F"=r[1,2], "df1"=r[1,1], "df2"=r[2,1], "p"=r[1,3]))
		
	} else {
	
		levenes.table[["data"]] <- list(list("F"=".", "df1"=".", "df2"=".", "p"="."))
	}
	
	if (status$error)
	    levenes.table[["error"]] <- list(error="badData")
	
	list(result=levenes.table, status=status)
}


.anovaProfilePlot <- function(dataset, options, perform, status) {

    profilePlotList <- list()
            
    if (perform == "run" && status$ready && options$horizontalAxis != "" && options$dependent != "") {
        
        groupVars <- c(options[["horizontalAxis"]], options[["seperateLines"]], options[["seperatePlots"]])
        groupVars <- groupVars[groupVars != ""]
        groupVarsV <- .v(groupVars)
        dependentV <- .v(options$dependent)
        
        summaryStat <- plyr::ddply(as.data.frame(dataset), groupVarsV, .drop = FALSE,
                                   .fun = function(xx, col) {
                                        c(N = length(xx[[col]]), "dependent" = mean(xx[[col]]), sd = sd(xx[[col]]))  
                                   }, dependentV)   
                
        if ( options[["horizontalAxis"]] != "" ) {
            colnames(summaryStat)[which(colnames(summaryStat) == .v(options[["horizontalAxis"]]))] <- "horizontalAxis"
        }
        
        if ( options[["seperateLines"]] != "" ) {
            colnames(summaryStat)[which(colnames(summaryStat) == .v(options[["seperateLines"]]))] <- "seperateLines"
        }
        
        if ( options[["seperatePlots"]] != "" ) {
            colnames(summaryStat)[which(colnames(summaryStat) == .v(options[["seperatePlots"]]))] <- "seperatePlots"
        }
                
        summaryStat$se <- summaryStat$sd / sqrt(summaryStat$N)

        ciMult <- qt(.95/2 + .5, summaryStat$N - 1)
        summaryStat$ci <- summaryStat$se * ciMult
        summaryStat$ciLower <- summaryStat[,"dependent"] - summaryStat[,"ci"]
        summaryStat$ciUpper <- summaryStat[,"dependent"] + summaryStat[,"ci"]
        
        base_breaks_x <- function(x){
            b <- unique(as.numeric(x))
            d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
            list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 2))#,
#                 ggplot2::scale_x_continuous(breaks=unique(as.numeric(x)),
#                                   labels=levels(x), 
#                                   limits=c(min(as.numeric(x)) - (length(levels(x)) * .1), 
#                                         max(as.numeric(x)) + (length(levels(x)) * .1))))
        }

        base_breaks_y <- function(x, errorBars){
            if (errorBars) {
                ci.pos <- c(x[,"dependent"]-x[,"ci"],x[,"dependent"]+x[,"ci"])
                b <- pretty(ci.pos)
                d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
                list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 2),
                     ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
            } else {
                b <- pretty(x[,"dependent"])
                d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
                list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 2),
                     ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
            }
        }
        
        if (length(groupVars) == 3) {
            subsetPlots <- levels(summaryStat[,"seperatePlots"])
		    nPlots <- length(subsetPlots)
	    } else {
		    nPlots <- 1
	    }
	    	    	    
	    for (i in 1:nPlots) {
	    
	        profilePlot <- list()
	        profilePlot[["title"]] <- ""
	        profilePlot[["width"]] <- options$chartWidth
	        profilePlot[["height"]] <- options$chartHeight
	        profilePlot[["custom"]] <- list(width="chartWidth", height="chartHeight")
				
            if (length(groupVars) == 3) {
                summaryStatSubset <- subset(summaryStat,summaryStat[,"seperatePlots"] == subsetPlots[i])
            } else {
                summaryStatSubset <- summaryStat
            }
            
            if(length(groupVars) == 1) {
            
                p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=horizontalAxis, 
                                              y=dependent,
                                              group=1)) 
            
            } else {
            
                p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=horizontalAxis, 
                                              y=dependent, 
                                              colour=seperateLines, 
                                              group=seperateLines))
            
            } 
                    
            if (options[["errorBars"]]) {
            
                pd <- ggplot2::position_dodge(.15)
                p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, 
                                                            ymax=ciUpper), 
                                                            colour="black", width=.15, position=pd)
            
            } else {
            
                pd <- ggplot2::position_dodge(0)
            
            }
                        
            p <- p + ggplot2::geom_line(position=pd, size = 1.5) + ggplot2::geom_point(position=pd, size=3, shape=21, fill="white") +
                ggplot2::ylab(options[["dependent"]]) +
                ggplot2::xlab(options[["horizontalAxis"]]) +
                ggplot2::labs(colour=options[["seperateLines"]]) +
                ggplot2::theme_bw() +
                ggplot2::theme(legend.justification=c(1,0), legend.position=c(1,0),
                      panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=20),
                      panel.grid.major=ggplot2::element_blank(),
                      axis.title.x = ggplot2::element_text(size=20), axis.title.y = ggplot2::element_text(size=20),
                      axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
                      panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                      plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                      legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                      panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                      legend.title = ggplot2::element_text(size=12),
                      legend.text = ggplot2::element_text(size = 12),
                      axis.ticks = ggplot2::element_line(size = 1.2),
                      axis.ticks.margin = grid::unit(1,"mm"),
                      axis.ticks.length = grid::unit(3, "mm"),
                      plot.margin = grid::unit(c(0,.1,0,0), "cm")) +
                base_breaks_y(summaryStatSubset, options[["errorBars"]]) +
                base_breaks_x(summaryStatSubset[,"horizontalAxis"])
                        
            if (nPlots > 1) {
                p <- p + ggplot2::ggtitle(paste(options[["seperatePlots"]],": ",subsetPlots[i], sep = ""))
            }
        
            image <- .beginSaveImage(options$chartWidth, options$chartHeight)
            print(p)
            content <- .endSaveImage(image)
            
            profilePlot[["data"]] <- content
            
            profilePlotList[[i]] <- profilePlot
            
	    }
    }
    
    profilePlotList
}