
Anova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
  
	if (is.null(dataset)) {
		
		if (perform == "run") {
		  dataset <- read.dataset.to.end()
		} else {
		  dataset <- read.dataset.header()
		}
	}
  
	results <- list()
  
	#######################################
	###        CONTRAST FUNCTION        ###
	#######################################
  
	.setContrast <- function (var,con,ref,case=FALSE) {
		levels <- sort(levels(dataset[[var]]),decreasing=(ref!="last"))
		n.levels <- length(levels)
		
		cases <- list()
		
		if (con == "none") {
		  
			options(contrasts = c("contr.sum","contr.poly"))
		  
		} else if (con == "simple") {
		  
			c<-contr.treatment(levels)
			my.coding<-matrix(rep(1 / n.levels, prod(dim(c))), ncol=n.levels - 1)
			contr<-(c-my.coding)*n.levels
			  
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i + 1]," - ",paste(levels[-(i + 1)],collapse=", "),sep="")
			}
		  
		} else if (con == "deviation") {
		  
			contr = contr.sum(levels)
		  
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i]," - ",levels[n.levels],sep="")
			}
		  
		} else if (con == "polynomial") {
		  
			contr = contr.poly(levels)
			
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- ""
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
				contr[c(i,i+1),i]<- c(-1,1)  
			}
		  
			for(i in 1:(n.levels-1)){
				cases[[i]] <- paste(levels[i]," - ",levels[i+1],sep="")
			}
		  
		} else if (con=="difference") {
		  
			contr = contr.helmert(levels)
		  
			for(i in 1:(n.levels - 1)) {
				cases[[i]] <- paste(levels[i + 1]," - ",paste(levels[1:i],collapse=", "),sep="")
			}
		  
		}
		
		if (case==FALSE & con!="none")
			return(contr)
		if (case==TRUE)
			return(cases) 
		
	}
  
	#######################################
	###              ANOVA              ###
	#######################################
  
	an0va <- list()
  
	an0va[["title"]] <- "ANOVA"
	an0va[["cases"]] <- I(options$modelTerms)
  
	fields <- list(
		list(name="Cases", type="text"),
		list(name="Sum of Squares", type="number", format="dp:3"),
		list(name="df", type="number", format="dp:0"),
		list(name="Mean Square", type="number", format="dp:3"),
		list(name="F", type="number", format="dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
  
	an0va[["schema"]] <- list(fields=fields)
  
	if (length(options$modelTerms) > 0) {
    
		terms <- options$modelTerms
		fixedFactors <- options$fixedFactors
		model <- NULL
    
		contrast.name <- NULL
		reference <- NULL
		variable <- NULL
    
		if (perform == "run") {  
		
			an0va.results <- try (silent = FALSE, expr = {
        
				model.def <- paste(options$dependent, "~", paste(terms, collapse="+"))
				model.formula <- as.formula(model.def)
			
				for(i in fixedFactors) {
					dataset[[i]] <- as.factor(dataset[[i]])
				}
			
				WLS <- NULL
				if (! is.null(options$wlsWeights))
					WLS <- dataset[[options$wlsWeights]]
			
				for(i in 1:length(fixedFactors)) {
			  
					contrast.name[i] <- options$contrasts[[i]][["contrast"]]
					reference[i] <- options$contrasts[[i]][["reference"]]
					variable[i] <- options$contrasts[[i]][["variable"]]
			  
					contr <- .setContrast(variable[i], contrast.name[i], reference[i])
					dimnames(contr)<-NULL
					contrasts(dataset[[variable[i]]]) = contr
				}
			
				model <- aov(model.formula, dataset, weights=WLS)
			
				if (options$sumOfSquares == "type1") {
			  
					result <- anova(model)
			  
				} else if (options$sumOfSquares == "type2") {
			  
					result <- car::Anova(model, type=2)
			  
				} else if (options$sumOfSquares == "type3") {
			  
					result <- car::Anova(model, type=3)
			  
				}
			
				result.table <- list()
			
				for (term in terms) {
					
					df <- result[term,"Df"]
					SS <- result[term,"Sum Sq"]
					MS <- result[term,"Sum Sq"]/result[term,"Df"]
					F <- if (is.na(result[term,"F value"])) {""} else { result[term, "F value"] }
					p <- if (is.na(result[term,"Pr(>F)"])) {""} else { result[term, "Pr(>F)"] }
			  
					r <- list("Cases"=term, "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p)
			  
					result.table[[length(result.table) + 1]] <- r
				}
			
				result.table
        
			})
      
			if (class(an0va.results) == "try-error") {
				an0va.results <- list()
				an0va.results <- for(i in 1:length(terms)) {
					r <- list("Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
					an0va.results[[length(an0va.results) + 1]] <- r
				}
			}
      
		an0va[["data"]] <- an0va.results
		}
    
    results[["anova"]] <- an0va
    
    
    #######################################
    ###            CONTRASTS            ###
    #######################################
    
		if (!all(contrast.name == "none")){

			contrasts <- list()
			
			if (perform == "run") {    
			
				result.contrast <- summary.lm(model)[["coefficients"]]
			  
				for(var in 1:length(fixedFactors)) {
				
					if (contrast.name[var] != "none" & length(unique(dataset[[variable[var]]])) > 1) {
				  
						contrast <- list()
				  
						contrast[["title"]] <- paste("Test", contrast.name[var],"contrast for variable",variable[var], sep=" ")
				  
						cases <- .setContrast(variable[var], contrast.name[var], reference[var], case=TRUE)
				  				  
						fields <- list(
							list(name="Comparison", type="text"),
							list(name="Estimate", type="number", format="dp:3"),
							list(name="Std. Error", type="number", format="dp:3"),
							list(name="t", type="number", format="dp:3"),
							list(name="p", type="number", format="dp:3;p:.001"))
				  
						contrast[["schema"]] <- list(fields=fields)
				  
						contrast.results <- list()
				  
						for (i in 1:(length(levels(dataset[[variable[var]]])) - 1)) {
					
							name.case <- paste(variable[var],i,sep="")
					
							est <- result.contrast[name.case,"Estimate"]
							SE <- result.contrast[name.case,"Std. Error"]
							t <- result.contrast[name.case,"t value"]
							p <- if (is.na(result.contrast[name.case,"Pr(>|t|)"])) {""} else { result.contrast[name.case, "Pr(>|t|)"] }
					
							r <- list("Comparison"=cases[[i]], "Estimate"=est, "Std. Error"=SE, "t"=t, "p"=p)
					
							contrast.results[[length(contrast.results) + 1]] <- r
						}
				  
						contrast[["data"]] <- contrast.results  
				  
						contrasts[[var]] <- contrast		  
					}             
				}
			}
			
			results[["contrasts"]] <- contrasts   
		}
	}
  
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
        
				if (length(unique(dataset[[postHoc.var[var]]])) > 1) {
          
					posthoc[["title"]] <- paste("Post-Hoc Comparisons", postHoc.var[var], sep=" ")
          
					fields <- list(
						list(name="(I) response", type="text"),
						list(name="(J) response", type="text"),
						list(name="Mean Difference", type="number", format="dp:3"),
						list(name="t", type="number", format="dp:3"),
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
          
					data.split <- split(dataset[[options$dependent]], dataset[[postHoc.var[var]]])
					
					n.test <- length(data.split)*((length(data.split)-1)/2)
					
					count <- 0
					p <- NULL
					
					for (i in 1:length(data.split)) {
            
						for (j in 1:length(data.split)) {
              
							if (i > j) {

								count <- count + 1
							
								r <- t.test(data.split[[i]],data.split[[j]])
				                
								if (j == 1 | (i == 1 & j == 2))
									var1 <- names(data.split)[i]
								else
									var1 <- "" 
																
								var2 <- names(data.split)[j]
								m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
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
      
		results[["postHoc"]] <- postHoc
		}  
	}
	
	print(results)
	results}