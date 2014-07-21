
AnovaRepeatedMeasuresShort <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	numeric.variables <- c(unlist(options$dependent),unlist(options$wlsWeight))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors),unlist(options$repeatedMeasures))
	factor.variables <- factor.variables[factor.variables != ""]

	if (is.null(dataset)) {
		
		if (perform == "run") {
			dataset <- .readDataSetToEnd(	columns.as.numeric = c(numeric.variables), columns.as.factor = c(factor.variables) )
		} else {
			dataset <- .readDataSetHeader( columns.as.numeric = c(numeric.variables), columns.as.factor = c(factor.variables) )
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
	
	anova <- list()
	
	anova[["title"]] <- "ANOVA"
	
	fields <- list(
		list(name="Cases", type="text"),
		list(name="Sum of Squares", type="number", format="dp:3"),
		list(name="df", type="number", format="dp:0"),
		list(name="Mean Square", type="number", format="dp:3"),
		list(name="F", type="number", format="dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
		
		if (options$misc[["effectSizeEstimates"]]) {
			fields[[length(fields) + 1]] <- list(name="&eta;&sup2;", type="number", format="dp:3")
			fields[[length(fields) + 1]] <- list(name="&omega;&sup2;", type="number", format="dp:3")
		}
	
	anova[["schema"]] <- list(fields=fields)
	
	results[["anova"]] <- anova
	
	results
}