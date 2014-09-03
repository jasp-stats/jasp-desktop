
AnovaRepeatedMeasuresShort <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	ready <- "" %in% options$repeatedMeasuresCells == FALSE
	
	if (ready) {
	
		rm.vars <- options$repeatedMeasuresCells
		
	} else {
	
		rm.vars <- c()
	
	}
	
	bt.vars <- options$betweenSubjectFactors
	

	if (is.null(dataset)) {
		
		if (perform == "run" && ready == TRUE) {
		
			dataset <- .readDataSetToEnd(columns.as.numeric = rm.vars, columns.as.factor = bt.vars, exclude.na.listwise = c(rm.vars, bt.vars))
			
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
	
	anova[["title"]] <- "Repeated Measures ANOVA"
	
	fields <- list(
		list(name="case", type="string", title=""),
		list(name="SS", type="number", format="dp:3", title="Sum of Squares"),
		list(name="df", type="number", format="dp:0"),
		list(name="MS", type="number", format="dp:3", title="Mean Square"),
		list(name="F", type="number", format="dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
	
	anova[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	for (factor.name in rm.factor.names) {
	
		data[[length(data)+1]] <- list("case"=factor.name)
	}
	
	data[[length(data)+1]] <- list("case"="Residual")
	
	
	if (perform == "run" && ready == TRUE) {

		rm.factors <- options$repeatedMeasuresFactors

		dataset <- .shortToLong(dataset, rm.factors, rm.vars, bt.vars)
		
		options(contrasts=c("contr.sum","contr.poly"))
		
		df.rm.factor.names <- paste("F", .v(rm.factor.names), sep="")
		
		bs.error <- paste("(", paste(.v(bt.vars), collapse="*"), ")")
		
		independents <- c(df.rm.factor.names, .v(bt.vars))

		main <- paste("(", paste(independents, collapse="*"), ")", sep="")
		ws.error <- paste("Error(subject/(", paste(df.rm.factor.names, collapse="*"), "))", sep="")
		rhs <- paste(main, ws.error, sep="+")

		if (length(bt.vars) > 0) {
		
			bs.error <- paste("(", paste(.v(bt.vars), collapse="*"), ")", sep="")
			rhs <- paste(rhs, bs.error, sep="+")
		}
		
		f <- paste("dependent", rhs, sep="~")
		
		r <- stats::aov(as.formula(f), data=dataset)
		
		data <- list()

		for (i in 2:length(r)) {
		
			if (i == 2) {
			
				data[[length(data)+1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="")
				
			} else if (i == 3) {
			
				data[[length(data)+1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="")
			}
		
			s <- summary(r[[i]])[[1]]

			for (row.name in row.names(s)) {
		
				row <- s[row.name,]
				
				row.name <- stringr::str_trim(row.name)
				if (row.name == "Residuals") {

					row.name <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Residual"
					data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F="", p="")
					
				} else {
				
					row.name <- paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", .unvf(row.name), sep="")
					data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F=row[["F value"]], p=row[["Pr(>F)"]])
				}
			}
		}
	}
	
	
	anova[["data"]] <- data
	
	anova[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Type I Sum of Squares"))
	
	results[["anova"]] <- anova
	
	results
}