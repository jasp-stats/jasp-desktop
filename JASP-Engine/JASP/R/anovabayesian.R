
AnovaBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	options$covariates <- NULL

	results <- AncovaBayesian(dataset, options, perform, callback)
	results$results$title <- "Bayesian ANOVA"	

return(results)
}


