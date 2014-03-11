
Anova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset)) {
	
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	an0va <- list()
	
	an0va[["title"]] <- "ANOVA"
	an0va[["cases"]] <- I(options$modelTerms)
	
	fields <- list(
		list(id="Sum of Squares", type="number", format="dp:3"),
		list(id="df", type="number", format="dp:0"),
		list(id="Mean Square", type="number", format="dp:3"),
		list(id="F", type="number", format="dp:3"),
		list(id="p", type="number", format="dp:3;p:.001"))
			
	schema <- list(fields=fields)
	
	an0va[["schema"]] <- schema
	
	if (length(options$modelTerms) > 0) {
	
		terms <- options$modelTerms
		
		if (perform == "run")
		{	
			an0va.results <- try (silent = FALSE, expr = {
				
				model.def <- paste(options$dependent, "~", paste(terms, collapse="+"))
				model.formula <- as.formula(model.def)

				WLS <- NULL
				if ( ! is.null(options$wlsWeights))
					WLS <- dataset[[options$wlsWeights]]
				
				options(contrasts = c("contr.sum","contr.poly"))
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
					
					r <- list("Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p)
					
					result.table[[length(result.table)+1]] <- r
				}

				result.table

			})
				
			if (class(an0va.results) == "try-error") {
				an0va.results <- list()
				an0va.results <- for(i in 1:length(terms)) {
					r <- list("Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
					an0va.results[[length(an0va.results)+1]] <- r
				}
			}
			
			an0va[["data"]] <- an0va.results
		}
		
		results[["anova"]] <- an0va

	}
	
	if (length(options$fixedFactors) > 0) {
		
		contrasts <- list()
		
		for(var in 1:length(options$fixedFactors)){
			
			contrast.name <- options$contrasts[[var]][["contrast"]]
			reference <- options$contrasts[[var]][["reference"]]
			variable <- options$contrasts[[var]][["variable"]]
			
			contrast <- list()
			
			if(contrast.name != "none" & length(unique(dataset[[variable]])) > 1){
					
				contrast[["title"]] <- paste(variable, contrast.name, "contrast", sep=" ")
				  
				levels <- sort(unique(dataset[[variable]]),decreasing=reference!="last")
				n.levels <- length(levels)
        
				cases <- list()
				  
				if(contrast.name=="simple"){
				  
				  c<-contr.treatment(levels)
				  my.coding<-matrix(rep(1/n.levels, prod(dim(c))), ncol=n.levels-1)
				  contr<-c-my.coding
          
				  for(i in 1:(n.levels-1)){
				    cases[[i]] <- paste(levels[i+1]," - ",paste(levels[-(i+1)],collapse=", "),sep="")
				  }
				  
				} else if(contrast.name=="deviation"){
				  
				  contr = contr.sum(levels)
          
				  for(i in 1:(n.levels-1)){
				    cases[[i]] <- paste(levels[i]," - ",levels[n.levels],sep="")
				  }
				  
				} else if(contrast.name=="polynomial"){
				  
				  contr = contr.poly(levels)
				  
				} else if(contrast.name=="helmert"){
				  
				  contr = matrix(0,nrow = n.levels, ncol = n.levels-1)
				  for(i in 1:(n.levels-1)){
				    k <- 1/(n.levels-(i-1))
				    contr[i:n.levels,i] <- c(k*(n.levels-i),rep(-k,n.levels-i))
				  }
          
				  for(i in 1:(n.levels-1)){
				    cases[[i]] <- paste(levels[i]," - ",paste(levels[-(1:i)],collapse=", "),sep="")
				  }
				  
				} else if(contrast.name=="repeated"){
				  
				  contr = matrix(0,nrow = n.levels, ncol = n.levels-1)
				  for(i in 1:n.levels-1){
				    contr[c(i,i+1),i]<- c(-1,1)  
				  }
          
				  for(i in 1:(n.levels-1)){
				    cases[[i]] <- paste(levels[i]," - ",levels[i+1],sep="")
				  }

				  
				} else if(contrast.name=="difference"){
				  
				  rotate <- function(x) t(apply(x, 2, rev))
				  contr = matrix(0,nrow = n.levels, ncol = n.levels-1)
				  for(i in 1:(n.levels-1)){
				    k <- 1/(n.levels-(i-1))
				    contr[i:n.levels,i] <- c(k*(n.levels-i),rep(-k,n.levels-i))
				  }
				  contr <- rotate(rotate(contr))
          
				  for(i in 1:(n.levels-1)){
				    cases[[i]] <- paste(levels[i+1]," - ",paste(levels[1:i],collapse=", "),sep="")
				  }
				  
				}
        
				contrast[["cases"]] <- cases
					
				fields <- list(
					list(id="estimate", type="number", format="sf:4"),
					list(id="t", type="number", format="sf:4"),
					list(id="p", type="number", format="dp:4;p:.001"))

				contrast[["schema"]] <- list(fields=fields)
	
				fixedFactor = as.factor(dataset[[variable]])
						
				contrasts(fixedFactor) = contr
				model.def <- paste(options$dependent, "~", "fixedFactor")
				model.formula <- as.formula(model.def)
				result <- summary(lm(model.formula, dataset))
				
				contrast.results <- list()
				
				for (i in 1:(length(unique(dataset[[variable]]))-1)) {
					
					est <- result$coefficients[i+1,"Estimate"]	
					t <- result$coefficients[i+1,"t value"]	
					p <- if (is.na(result$coefficients[i+1,"Pr(>|t|)"])) {""} else { result$coefficients[i+1,"Pr(>|t|)"] }
						
					r <- list("estimate"=est, "t"=t, "p"=p)
						
					contrast.results[[length(contrast.results)+1]] <- r
				}
				
				
				contrast[["data"]] <- contrast.results
			}
			
			contrasts[[var]] <- contrast
			
		}
		
		results[["contrasts"]] <- contrasts
	}
		
	results
}

