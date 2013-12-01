
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
				
				model <- aov(model.formula, dataset, weights=WLS)
				
				if (options$sumOfSquares == "type1") {
				
					result <- anova(model)
					
				} else if (options$sumOfSquares == "type2") {
				
					result <- car::Anova(model, type=2)

				} else if (options$sumOfSquares == "type3") {
				
					options(contrasts = c("contr.sum","contr.poly"))
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
		
	results
}

