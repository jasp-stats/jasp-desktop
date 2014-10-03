
AnovaRepeatedMeasuresBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	ready <- "" %in% options$repeatedMeasuresCells == FALSE
	
	if (ready) {
	
		rm.vars <- options$repeatedMeasuresCells
		
	} else {
	
		rm.vars <- c()
	
	}
	
	bt.vars <- options$betweenSubjectFactors
	

	if (is.null(dataset)) {
		
		if (perform == "run" && ready) {
		
			dataset <- .readDataSetToEnd(columns.as.numeric = rm.vars, columns.as.factor = bt.vars)
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric = rm.vars, columns.as.factor = bt.vars)
		}
	}
	
	results <- list()
	
	#######################################
	###			   META			  ###
	#######################################

	.meta <- list(
		list(name="anova", type="table")
	)

	results[[".meta"]] <- .meta
	
  
  #######################################
	###			   ANALYSIS			  ###
	#######################################
  
	rm.factor.names <- c()
	
	for (factor in options$repeatedMeasuresFactors) {
	  
	  rm.factor.names <- c(rm.factor.names, factor$name)
	}
	
	
	anova <- list()
	
	anova[["title"]] <- "Bayesian Repeated Measures ANOVA"
	
	fields <- list(
		list(name="case", type="text", title=""),
		list(name="Sum of Squares", type="number", format="dp:3"),
		list(name="df", type="number", format="dp:0"),
		list(name="Mean Square", type="number", format="dp:3"),
		list(name="F", type="number", format="dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
	
	anova[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	for (factor in options$repeatedMeasuresFactors) {
	
		data[[length(data)+1]] <- list("case"=factor$name)
	}
	
	data[[length(data)+1]] <- list("case"="Residual")
	
	
	if (perform == "run" && ready == TRUE) {

		rm.factors <- options$repeatedMeasuresFactors

		dataset <- .shortToLong(dataset, rm.factors, rm.vars, bt.vars)
		
		print(dataset)
	
	}
	
	
	anova[["data"]] <- data
	
	results[["anova"]] <- anova
	
	results
}