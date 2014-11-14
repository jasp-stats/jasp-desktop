
.anovaContrastCases <- function(column, contrast.type) {

	levels <- levels(column)
	n.levels <- length(levels)
	
	cases <- list()
	
	if (contrast.type == "deviation") {

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

.anovaModel <- function(dataset, options) {

	dependent.normal <- options$dependent
	dependent.base64 <- .v(options$dependent)
	
	terms.base64 <- c()
	terms.normal <- c()

	for (term in options$modelTerms) {

		components <- unlist(term$components)
		term.base64 <- paste(.v(components), collapse=":", sep="")
		term.normal <- paste(components, collapse="*", sep="")

		terms.base64 <- c(terms.base64, term.base64)
		terms.normal <- c(terms.normal, term.normal)
	}
	
	model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))
	model.formula <- as.formula(model.def)
	
	WLS <- NULL
	if ( ! is.null(options$wlsWeights))
		WLS <- dataset[[ .v(options$wlsWeights) ]]
		
	model <- aov(model.formula, dataset, weights=WLS)
	singular <- class(try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = FALSE))) == "try-error"
					
	list(model = model, singular = singular)
}

.anovaTable <- function(dataset, options, perform, model, status, singular) {
	
	anova <- list()
	
	anova[["title"]] <- "ANOVA"
	
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
	
	dependent.normal <- options$dependent
	dependent.base64 <- .v(options$dependent)

	#terms <- options$modelTerms
	#fixedFactors <- options$fixedFactors
	
	terms.base64 <- c()
	terms.normal <- c()

	for (term in options$modelTerms) {

		components <- unlist(term$components)
		term.base64 <- paste(.v(components), collapse=":", sep="")
		term.normal <- paste(components, collapse="*", sep="")

		terms.base64 <- c(terms.base64, term.base64)
		terms.normal <- c(terms.normal, term.normal)
	}
	
	if (perform == "init" || status$ready == FALSE || status$error) {
	
		anova.rows <- list()

		for (i in .indices(terms.normal)) {

			row <- list("Cases"=terms.normal[i], "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".")
			anova.rows[[length(anova.rows) + 1]] <- row
		}

		row <- list("Cases"="Residual", "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".")
		anova.rows[[length(anova.rows) + 1]] <- row

		anova[["data"]] <- anova.rows
		
		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
	
	
	} else {
			
		anova.rows <- try (silent = FALSE, expr = {
			
			rows <- list()
					
			if (options$sumOfSquares == "type1") {
			
				result <- stats::anova(model)
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
				
				if (df != 0) {
					SS <- result[term,"Sum Sq"]
					MS <- result[term,"Sum Sq"]/result[term,"Df"]
				} else {
					SS <- 0
					MS <- ""
				}	
				
				F <- if (is.na(result[term,"F value"])) {""} else { result[term, "F value"] }
				p <- if (is.na(result[term,"Pr(>F)"] )) {""} else { result[term, "Pr(>F)"] }
			
				if (i <= length(terms.base64)) {
					row <- list("Cases"=terms.normal[i], "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p)
				} else {
					row <- list("Cases"="Residual", "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"="", "p"="")
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
		
			errorMessage <- as.character(anova.rows)
			status$error <- TRUE
			status$errorMessage <- errorMessage
			
			anova[["error"]] <- list(errorType="badData", errorMessage=gsub("\n", "<br>", as.character(anova.rows)))

			anova.rows <- list()
			
			for (i in .indices(terms.normal)) {
				row <- list("Cases"=terms.normal[i], "Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
				anova.rows[[length(anova.rows) + 1]] <- row
			}
		}
		
		anova[["data"]] <- anova.rows
		
		if (singular) 
			anova[["footnotes"]] <- list("Warning: predictor variables are not all linearly independent (singularity)") 
					
	}
	
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


	fixedFactors <- options$fixedFactors
	
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
			
			if (perform == "init" || status$error) {
			
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
				
			if (perform == "run" && status$ready) {
			
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

	if (perform == "run" && status$ready) {

		interaction <- paste(.v(options$fixedFactors), collapse=":", sep="")
		levene.def <- paste(.v(options$dependent), "~", interaction)
		levene.formula <- as.formula(levene.def)

		r <- car::leveneTest(levene.formula, dataset, center = "mean")

		levenes.table[["data"]] <- list(list("F"=r[1,2], "df1"=r[1,1], "df2"=r[2,1], "p"=r[1,3]))
		
	} else {
	
		levenes.table[["data"]] <- list(list("F"=".", "df1"=".", "df2"=".", "p"="."))
	}
	
	list(result=levenes.table, status=status)
}


Anova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	numeric.variables <- c(unlist(options$dependent),unlist(options$wlsWeight))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors),unlist(options$repeatedMeasures))
	factor.variables <- factor.variables[factor.variables != ""]

	if (is.null(dataset)) {
		
		if (perform == "run") {
		
			dataset <- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=c(numeric.variables, factor.variables))
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
		}
	}
	
	results <- list()
	
	
	# META definitions

	.meta <- list(
		list(name="anova", type="table"),
		list(name="levene", type="table"),
		list(name="contrasts", type="tables"),
		list(name="posthoc", type="tables"),
		list(name="descriptives", type="table")
	)

	results[[".meta"]] <- .meta
	
	
	status <- .anovaCheck(dataset, options, perform)
	


	## Setup Contrasts

	if (status$ready && status$error == FALSE)
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
	
	
	results
}