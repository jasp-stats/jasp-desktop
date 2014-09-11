
Anova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	numeric.variables <- c(unlist(options$dependent),unlist(options$wlsWeight))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors),unlist(options$repeatedMeasures))
	factor.variables <- factor.variables[factor.variables != ""]

	if (is.null(dataset)) {
		
		if (perform == "run") {
			dataset <- .readDataSetToEnd( columns.as.numeric = c(numeric.variables), columns.as.factor = c(factor.variables) )
		} else {
			dataset <- .readDataSetHeader( columns.as.numeric = c(numeric.variables), columns.as.factor = c(factor.variables) )
		}
	}
	
	results <- list()
	
	#######################################
	###              META               ###
	#######################################

	.meta <- list(
		list(name="anova", type="table"),
		list(name="levene", type="table"),
		list(name="contrasts", type="tables"),
		list(name="posthoc", type="tables"),
		list(name="descriptives", type="table")
	)

	results[[".meta"]] <- .meta

	#######################################
	###        CONTRAST FUNCTION        ###
	#######################################
	
	.setContrast <- function (var,con,case=FALSE) {
		levels <- levels(dataset[[ .v(var) ]])
		n.levels <- length(levels)
		
		cases <- list()
		
		if (con == "none") {
			
			options(contrasts = c("contr.sum","contr.poly"))
			
		} else if (con == "deviation") {
			
			contr = matrix(0,nrow = n.levels, ncol = n.levels - 1)
			for(i in 1:n.levels-1){
				contr[c(1,i+1),i]<- c(-1,1)  
			}

			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i + 1]," - ",paste(levels,collapse=", "),sep="")
			}
			
		} else if (con == "simple") {
			
			c <- contr.treatment(levels)

			my.coding <- matrix(rep(1 / n.levels, prod(dim(c))), ncol=n.levels - 1)
			contr <- (c-my.coding)*n.levels
		   
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i+1]," - ",levels[1],sep="")
			}
					   
		} else if (con == "helmert") {
			
			contr = matrix(0,nrow = n.levels, ncol = n.levels - 1)
			for(i in 1:(n.levels - 1)) {
				k <- 1 / (n.levels - (i - 1))
				contr[i:n.levels,i] <- c(k * (n.levels - i), rep(-k, n.levels - i))
			}
			
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i]," - ",paste(levels[-(1:i)],collapse=", "),sep="")
			}
			
		} else if (con == "repeated") {
			
			contr = matrix(0,nrow = n.levels, ncol = n.levels - 1)
			for(i in 1:n.levels-1){
				contr[1:i,i] <- (n.levels-i)/n.levels
				contr[(i+1):n.levels,i] <- -i/n.levels
			}
			
			for(i in 1:(n.levels-1)){
				cases[[i]] <- paste(levels[i]," - ",levels[i+1],sep="")
			}
			
		} else if (con=="difference") {
			
			contr = contr.helmert(levels)
			
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i + 1]," - ",paste(levels[1:i],collapse=", "),sep="")
			}

		} else if (con == "polynomial") {
			
			contr = contr.poly(levels)
			
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- ""
			}	
		} 
		
		if (case==FALSE && con!="none")
			return(contr)
		if (case==TRUE)
			return(cases) 
		
	}
	
	#######################################
	###              ANOVA              ###
	#######################################
	
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
	
	if (length(options$modelTerms) > 0 && options$dependent != "") {

		terms <- options$modelTerms
		fixedFactors <- options$fixedFactors
		model <- NULL
		
		contrast.name <- NULL
		variable <- NULL
		
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
		
		if (perform == "run") {  
			
			anova.results <- try (silent = FALSE, expr = {

				model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))
				model.formula <- as.formula(model.def)
				
				WLS <- NULL
				if (! is.null(options$wlsWeights))
					WLS <- dataset[[ .v(options$wlsWeights) ]]
				
				for (i in .indices(fixedFactors)) {
					
					contrast.name[i] <- options$contrasts[[i]][["contrast"]]
					variable[i] <- options$contrasts[[i]][["variable"]]
					
					if(length(unique(dataset[[ .v(fixedFactors[i]) ]])) > 1) {
						contr <- .setContrast(variable[i], contrast.name[i])
						dimnames(contr)<-NULL
						contrasts(dataset[[ .v(variable[i]) ]]) = contr
					}
				}
				
				model <- aov(model.formula, dataset, weights=WLS)
				
				if (options$sumOfSquares == "type1") {
					
					result <- stats::anova(model)
					SSt <- sum(result[,"Sum Sq"])
					
				} else if (options$sumOfSquares == "type2") {
					
					result <- car::Anova(model, type=2)
					SSt <- sum(result[,"Sum Sq"])
					
				} else if (options$sumOfSquares == "type3") {
					
					result <- car::Anova(model, type=3, singular.ok=TRUE)
					SSt <- sum(result[-1,"Sum Sq"])
					
				}
				
				result.table <- list()

				for (i in 1:(length(terms.base64)+1)) {
					
					if(i <= length(terms.base64)) {
						term <- terms.base64[i]
					} else {
						term <- "Residuals"
					}
					
					df <- result[term,"Df"]
					SS <- result[term,"Sum Sq"]
					MS <- result[term,"Sum Sq"]/result[term,"Df"]
					F <- if (is.na(result[term,"F value"])) {""} else { result[term, "F value"] }
					p <- if (is.na(result[term,"Pr(>F)"] )) {""} else { result[term, "Pr(>F)"] }
					
					if(i <= length(terms.base64)) {
						r <- list("Cases"=terms.normal[i], "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p)
					} else {
						r <- list("Cases"="Residual", "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"="", "p"="")
					}
					
					if (options$misc[["effectSizeEstimates"]]) {
						MSr <- result["Residuals","Sum Sq"]/result["Residuals","Df"]
						
						if(i <= length(terms.base64)) {
							r[["&eta;&sup2;"]] <- SS / SSt
							omega <- (SS - (df * MSr)) / (SSt + MSr)
							
							if(omega < 0) {
								r[["&omega;&sup2;"]] <- 0
							} else {
								r[["&omega;&sup2;"]] <- omega
							}
							
						} else {
							r[["&eta;&sup2;"]] <- ""
							r[["&omega;&sup2;"]] <- ""
						}

					}
					
					result.table[[length(result.table) + 1]] <- r
				}
				
				result.table
				
			})
			
			if (class(anova.results) == "try-error") {
			
				anova[["error"]] <- list(errorType="badData", errorMessage=gsub("\n", "<br>", as.character(anova.results)))

				anova.results <- list()
				
				for (i in .indices(terms.normal)) {
					r <- list("Cases"=terms.normal[i], "Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
					anova.results[[length(anova.results) + 1]] <- r
				}
			}
			
			anova[["data"]] <- anova.results
			
		} else {
		
			anova.results <- list()

			for (i in .indices(terms.normal)) {

				r <- list("Cases"=terms.normal[i], "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".")
				anova.results[[length(anova.results) + 1]] <- r
			}

			r <- list("Cases"="Residual", "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".")
			anova.results[[length(anova.results) + 1]] <- r

			anova[["data"]] <- anova.results		
		}
		
		
		#######################################
		###            CONTRASTS            ###
		#######################################
		
		if (!all(contrast.name == "none")){
			
			contrasts <- list()
			
			result.contrast <- summary.lm(model)[["coefficients"]]
			con <- 1
			for(var in .indices(fixedFactors)) {
				
				if (contrast.name[var] != "none" && length(unique(dataset[[ .v(variable[var]) ]])) > 1) {
					
					contrast <- list()
					
					contrast[["title"]] <- paste("Contrast", contrast.name[var], variable[var], sep=" ")
					
					cases <- .setContrast(variable[var], contrast.name[var], case=TRUE)
					
					fields <- list(
						list(name="Comparison", type="string"),
						list(name="Estimate", type="number", format="sf:4;dp:3"),
						list(name="Std. Error", type="number", format="sf:4;dp:3"),
						list(name="t", type="number", format="sf:4;dp:3"),
						list(name="p", type="number", format="dp:3;p:.001"))
					
					contrast[["schema"]] <- list(fields=fields)
					
					if (perform == "run") {
					
						contrast.results <- list()
						
						for (i in 1:(length(levels(dataset[[ .v(variable[var]) ]])) - 1)) {
							
							name.case <- paste(.v(variable[var]),i,sep="")
							
							est <- result.contrast[name.case,"Estimate"]
							SE <- result.contrast[name.case,"Std. Error"]
							t <- result.contrast[name.case,"t value"]
							p <- if (is.na(result.contrast[name.case,"Pr(>|t|)"])) {""} else { result.contrast[name.case, "Pr(>|t|)"] }
							
							r <- list("Comparison"=cases[[i]], "Estimate"=est, "Std. Error"=SE, "t"=t, "p"=p)
							
							contrast.results[[length(contrast.results) + 1]] <- r
						} 
						
						contrast[["data"]] <- contrast.results  
						
					} else {
						
						contrast.results <- list()
						
						for (i in 1:(length(levels(dataset[[ .v(variable[var]) ]])) - 1)) {
						
							r <- list("Comparison"=cases[[i]], "Estimate"=".", "Std. Error"=".", "t"=".", "p"=".")
							contrast.results[[length(contrast.results) + 1]] <- r
						}
						
						contrast[["data"]] <- contrast.results  
					}
					
					contrasts[[con]] <- contrast
					
					con <- con + 1
				}
			}

			results[["contrasts"]] <- contrasts   
		}
	}
	
	results[["anova"]] <- anova
	
	#######################################
	###         POST-HOC TESTS          ###
	#######################################
	
	if (perform == "run")
	{  
		
		postHoc.var <- unlist(options$postHocTests[["variables"]])
		
		if (length(postHoc.var > 0)) {
			
			postHoc <- list()
			
			for(var in 1:length(postHoc.var)) {
				
				posthoc <- list()
				
				if (length(unique(dataset[[ .v(postHoc.var[var]) ]])) > 1) {
					
					posthoc[["title"]] <- paste("Post-Hoc Comparisons", postHoc.var[var], sep=" ")
					
					fields <- list(
						list(name="(I) response", type="string"),
						list(name="(J) response", type="string"),
						list(name="Mean Difference", type="number", format="sf:4;dp:3"),
						list(name="t", type="number", format="sf:4;dp:3"),
						list(name="df", type="number", format="dp:0"),
						list(name="p", type="number", format="dp:3;p:.001"))
					
					if (options$postHocTests[["holm"]]) {
						fields[[length(fields) + 1]] <- list(name="p holm", type="number", format="dp:3")
					}		
					
					if (options$postHocTests[["bonferroni"]]) {
						fields[[length(fields) + 1]] <- list(name="p bonferroni", type="number", format="dp:3")
					}							
					
					if (options$postHocTests[["hochberg"]]) {
						fields[[length(fields) + 1]] <- list(name="p hochberg", type="number", format="dp:3")
					}	
					
					if (options$postHocTests[["hommel"]]) {
						fields[[length(fields) + 1]] <- list(name="p hommel", type="number", format="dp:3")
					}	
					
					if (options$postHocTests[["benjamini"]]) {
						fields[[length(fields) + 1]] <- list(name="p benjamini", type="number", format="dp:3")
					}	
					
					if (options$postHocTests[["FDR"]]) {
						fields[[length(fields) + 1]] <- list(name="p FDR", type="number", format="dp:3")
					}	
					
					posthoc[["schema"]] <- list(fields=fields)
					
					posthoc.results <- list()
					
					data.split <- split(dataset[[ .v(options$dependent) ]], dataset[[ .v(postHoc.var[var]) ]])
					
					n.test <- length(data.split)*((length(data.split)-1)/2)
					
					count <- 0
					p <- NULL
					
					for (i in 1:length(data.split)) {
						
						for (j in 1:length(data.split)) {
							
							if (i > j) {
								
								count <- count + 1
								
								r <- t.test(data.split[[i]],data.split[[j]])
								
								if (j == 1 | (i == 1 && j == 2))
									var1 <- names(data.split)[i]
								else
									var1 <- "" 
								
								var2 <- names(data.split)[j]
								m	<- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
								t 	 <- as.numeric(r$statistic)
								df	 <- as.numeric(r$parameter)
								p[count] <- as.numeric(r$p.value)
								
								result <- list("(I) response"=var1, "(J) response"=var2, "Mean Difference"=m,
											   t=t, df=df, p=p[count])
								
								posthoc.results[[length(posthoc.results)+1]] <- result  
							}
						}
					}
					
					posthoc[["data"]] <- posthoc.results
					
					if(options$postHocTests[["bonferroni"]] | options$postHocTests[["holm"]] | options$postHocTests[["hochberg"]] |
						   options$postHocTests[["FDR"]] | options$postHocTests[["hommel"]] | options$postHocTests[["benjamini"]])  {
						
						for (i in 1:count) {
							
							if (options$postHocTests[["holm"]]) {
								p.holm <- p.adjust(p, method="holm")[i]
								posthoc[["data"]][[i]][["p holm"]] <- p.holm
							}
							
							if (options$postHocTests[["bonferroni"]]) {
								p.bon <- p.adjust(p, method="bonferroni")[i]
								posthoc[["data"]][[i]][["p bonferroni"]] <- p.bon
							}
							
							if (options$postHocTests[["hochberg"]]) {
								p.hoch <- p.adjust(p, method="hochberg")[i]
								posthoc[["data"]][[i]][["p hochberg"]] <- p.hoch
							}
							
							if (options$postHocTests[["hommel"]]) {
								p.hom <- p.adjust(p, method="hommel")[i]
								posthoc[["data"]][[i]][["p hommel"]] <- p.hom
							}
							
							if (options$postHocTests[["benjamini"]]) {
								p.ben <- p.adjust(p, method="BY")[i]
								posthoc[["data"]][[i]][["p benjamini"]] <- p.ben
							}
							
							if (options$postHocTests[["FDR"]]) {
								p.fdr <- p.adjust(p, method="fdr")[i]
								posthoc[["data"]][[i]][["p FDR"]] <- p.fdr
							}
						}
					}
				}
				
				postHoc[[var]] <- posthoc	  
			}  
			
			results[["posthoc"]] <- postHoc
		}  
	}
	
	#######################################
	###          DESCRIPTIVES           ###
	#######################################
	
	if (perform == "run" && options$misc[["descriptives"]] && length(options$fixedFactors) > 0 && length(options$dependent) > 0) {
		
		descriptives <- list()
		
		descriptives[["title"]] <- "Descriptives"
		
		fields <- list()
		
		for(i in options$fixedFactors) {
			fields[[length(fields)+1]] <-  list(name=i, type="string")
		}
		
		fields[[length(fields)+1]] <- list(name="Mean", type="number", format="sf:4;dp:3")
		fields[[length(fields)+1]] <- list(name="SD", type="number", format="sf:4;dp:3")
		fields[[length(fields)+1]] <- list(name="N", type="number", format="dp:0")
		
		descriptives[["schema"]] <- list(fields=fields)
		
		fixedFactors <- unlist(options$fixedFactors)
		
		data <- list()
		levels <- list()
		
		for(i in 1:length(fixedFactors)) {
			data[[i]] <- factor(dataset[[ .v(fixedFactors[i]) ]])
			levels[[i]] <- levels(data[[i]])
		}
		
		cases <- expand.grid(levels)  
		descr <- by(dataset[[ .v(options$dependent) ]], data, pastecs::stat.desc)
		
		N <- NULL
		mean <- NULL
		sd <- NULL
		
		for(i in 1:nrow(cases)) {
			if(is.null(descr[i][[1]])) {
				N <- c(N, 0)
				mean <- c(mean, NA)
				sd <- c(sd,NA)
			} else {
				N <- c(N, descr[i][[1]][["nbr.val"]])
				mean <- c(mean, descr[i][[1]][["mean"]])
				sd <- c(sd,descr[i][[1]][["std.dev"]])
			}
		}	  
		
		descr <- cbind(cases,N,mean,sd)
		descr <- descr[do.call("order",descr[.indices(fixedFactors)]),]
		
		descriptives.result <- list()
		
		for(i in 1:length(descr[,1])) {
			r <- list()

			for(j in 1:length(fixedFactors)) {
				if(i == 1) {
					r[[fixedFactors[j]]] <- descr[i,j]
				} else if (descr[i,j]!=descr[i-1,j]) {
					r[[fixedFactors[j]]] <- descr[i,j]
				} else 
					r[[fixedFactors[j]]] <- ""
			}  

			r[["Mean"]] <- descr[i,"mean"]
			r[["SD"]] <- descr[i,"sd"]
			r[["N"]] <- descr[i,"N"]
				
			descriptives.result[[length(descriptives.result) +1 ]] <- r
		}
		
		descriptives[["data"]] <- descriptives.result	  
		
		results[["descriptives"]] <- descriptives	  
	}
	
	#######################################
	###          LEVENE'S TEST          ###
	#######################################
	
	if (options$misc[["homogeneityTests"]] && length(options$fixedFactors) > 0 && length(options$dependent) > 0) {
		
		levene <- list()
		
		levene[["title"]] <- "Levene's Test for Homogeneity of Variance"
		
		fields <- list(
			list(name="F", type="number", format="sf:4;dp:3"),
			list(name="df1", type="number", format="dp:0"),
			list(name="df2", type="number", format="dp:0"),
			list(name="p", type="number", format="dp:3;p:.001"))

		levene[["schema"]] <- list(fields=fields)
		
		if (perform == "run") {

			interaction <- paste(.v(options$fixedFactors), collapse=":", sep="")
			levene.def <- paste(.v(options$dependent), "~", interaction)
			levene.formula <- as.formula(levene.def)

			r <- car::leveneTest(levene.formula, dataset, center = "mean")

			levene[["data"]] <- list(list("F"=r[1,2], "df1"=r[1,1], "df2"=r[2,1], "p"=r[1,3]))
		
		} else {
		
			levene[["data"]] <- list(list("F"=".", "df1"=".", "df2"=".", "p"="."))
		
		}
		
		results[["levene"]] <- levene
	}
	
	#print(results)
	results
}