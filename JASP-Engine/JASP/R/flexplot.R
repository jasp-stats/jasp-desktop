#' Flexplot in JASP
#'
#' This function was developed for use in JASP. It takes a dataset as input with user 
#' options and returns a flexplot graphic
#' 
#' @param jaspResults A JASP object
#' @param dataset dataset supplied by JASP
#' @param options a list of options to pass to JASP
#'
#' @return a flexplot graphic. 
#' @export
flexplot = function(jaspResults, dataset, options) {

	### check if they've entered anything	  
  #save(dataset, options, file="~/Documents/JaspResults.Rdat")
	ready <- (options$dependent != "") 
	  
	### read in the dataset 
	if (ready) {
		### read in the dataset
		if (is.null(dataset)){ 
    		dataset = (.readDataSetToEnd(columns=c(options$dependent, options$variables, options$paneledVars)))
    		#save(dataset, ready, options, file="~/Users/Documents/jaspdata.rdat")
		} else {
			return(dataset) 
		}
	}
	  
	### create plots
	.flexPlotRes(jaspResults, formula, dataset, options, ready)
}


.flexPlotRes <- function(jaspResults, formula, dataset, options, ready) {
	#### set up parameters
	flex_Plot <- createJaspPlot(title = "Flexplot",  width = 600, height = 450)
	flex_Plot$dependOn(c("confidence", "dependent", "variables", "paneledVars", "ghostLines"))
	flex_Plot$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")

	#### pre-populate the jasp object
	jaspResults[["flex_Plot"]] <- flex_Plot

	if (!ready){
		return()
	}
   
                #### prepare the data for flexplot
		k = data.frame(matrix(nrow=nrow(dataset), ncol=length(options$variables) + length(options$dependent) + length(options$paneledVars)))
		names(k) = c(options$dependent, options$variables, options$paneledVars)
		variables <- unlist(options$variables)
		panels <- unlist(options$paneledVars)
                if (length(panels)>0){
			vars = c(variables, panels)
		} else {
			vars = variables
		}
                k[,1] = dataset[[.v(options$dependent)]]
		if (length(vars)>0){		#### this statement is necessary to allow histograms
			for (i in 2:(length(vars)+1)){
				k[,i] = dataset[[.v(vars[i-1])]]
			}
		}

                if (length(options$variables)==0){
			formula = as.formula(paste0(options$dependent, "~1"))		
		} else if (length(options$paneledVars)>0){
			formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+"), " | ", paste0(panels, collapse="+")))
		} else {
			formula = as.formula(paste0(options$dependent, "~", paste0(variables, collapse="+")))
		}
		tst = data.frame(x=1:10, y=1:10)
		require(ggplot2)

		#### do a ghost line
		if	(options$ghost){
		  ghost=rgb(195/255,0,0,.3) 
		} else {
		  ghost = NULL
		}

		whiskers = list("Quartiles" = "quartiles",
		                "Standard errors" = "sterr",
		                "Standard deviations" = "stdev")

		linetype = tolower(options$type)

		#save(k, formula, file="/Users/fife/Documents/jaspbroke.rdata")
		jitter = c(options$jitx,options$jity)
		if (linetype == "regression") linetype = "lm"
                plot = flexplot::flexplot(formula, data=k, method=linetype, se=options$confidence, alpha=options$alpha,
		                ghost.line=ghost,
		                spread=whiskers[[options$intervals]],
		                jitter = jitter)
		
		if (options$theme == "JASP"){
                  plot = JASPgraphs::themeJasp(plot)
		} else {
  		theme = list("Black and white"="ggplot2::theme_bw()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
  		             "Minimal" = "ggplot2::theme_minimal()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
  		             "Classic" = "ggplot2::theme_classic()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
  		             "Dark" = "ggplot2::theme_dark() + ggplot2::theme(text=ggplot2::element_text(size=18))")
  		plot = plot + eval(parse(text=theme[[tolower(options$theme)]]))
		}
		
		# #### create flexplot object   
		flex_Plot$plotObject <- plot
		return()   
}


	#### create a table
.printedResults = function(jaspResults, dataset, options, ready){

	if (!is.null(jaspResults[["resultsTable"]])) return()
  	
	# Create Table
	resultsTable <- createJaspTable(title="Flexplot Table")
    resultsTable$dependOn(c("variables", "dependent", "confidence", "type"))
	resultsTable$addCitation("Fife, Dustin A. (2019). Flexplot (Version 0.9.2) [Computer software].")

	resultsTable$showSpecifiedColumnsOnly <- TRUE
  	
	### add columns to table
	resultsTable$addColumnInfo(name = "var",   title = "Variable",   type = "string", combine = TRUE)
	resultsTable$addColumnInfo(name = "mean",   title = "Mean",   type = "string", combine = TRUE)	
	resultsTable$addColumnInfo(name = "median",   title = "Median",   type = "string", combine = TRUE)	
	resultsTable$addColumnInfo(name = "typ",   title = "Type",   type = "string", combine = TRUE)	
	
	#### tell jasp how many rows we expect
	if (ready)
	resultsTable$setExpectedSize(length(options$variables))
	
	for (variable in options$variables){
		#row <- c(variable, options$dependent, options$confidence, options$type)
		resultsTable$addRows(list(
						"var" = variable,
						"mean" = mean(dataset[[.v(variable)]]),
						"median" = median(dataset[[.v(variable)]]), 
						"typ" = mode(dataset[[.v(variable)]])))
	}
	#f <- paste0(options$dependent, "~", paste0(options$variables, collapse="+"))
	#message <- options$variables
	resultsTable$addFootnote(message="hello", symbol="<em>Note.</em>")	
	
	jaspResults[["resultsTable"]] <- resultsTable
}

	#### read in data
.flexReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns=c(options$dependent, options$variables)))
}
