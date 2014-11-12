
AnovaRepeatedMeasuresShort <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	ready <- "" %in% options$repeatedMeasuresCells == FALSE
	
	if (ready) {
	
		rm.vars <- options$repeatedMeasuresCells
		
		
	} else {
	
		rm.vars <- c()
	
	}
	
	bt.vars <- options$betweenSubjectFactors
	print(as.character(bt.vars))
	

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
	
	if (length(rm.factor.names) > 2) {
		
			anova[["error"]] <- list(errorType="badData", errorMessage="More than two repeated measures factors is not supported in this release")
	}
	
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
				
		if (options$sumOfSquares == "type1") {
			
			r <- stats::aov(as.formula(f), data=dataset)
		
			data <- list()

			for (i in 2:length(r)) {
		
				if (i == 2) {
			
					data[[length(data)+1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
				
				} else if (i == 3) {
			
					data[[length(data)+1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
				}
		
				s <- summary(r[[i]])[[1]]
			
				r.names <- row.names(s)

				for (j in .indices(r.names)) {
		
					row.name <- r.names[j]
		
					row <- s[row.name,]
				
					row.name <- stringr::str_trim(row.name)
					
					if (row.name == "Residuals") {

						row.name <- "Residual"
						data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F="", p="", .rowLevel=1)
					
					} else {
				
						row.name <- .unvf(row.name) 
						data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F=row[["F value"]], p=row[["Pr(>F)"]], .rowLevel=1, .isNewGroup=(j==1))
					}
				}
			}
			
			anova[["data"]] <- data
	
			anova[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Type I Sum of Squares"))
		}
		
		
		if (options$sumOfSquares == "type2") {			
				
			r <- try (silent = TRUE, expr = {afex::aov.car(as.formula(f), data=dataset,type= 2,return = "univariate")})
			
			if (class(r) == "try-error") {
			
				errorMessage <- .extractErrorMessage(r)
			}
		}	
		
		if (options$sumOfSquares == "type3") {	
					
			r <- try (silent = TRUE, expr = {afex::aov.car(as.formula(f), data=dataset,type= 3,return = "univariate")})
			
			if (class(r) == "try-error") {
			
			errorMessage <- .extractErrorMessage(r)
			}
		}	
		
		if(class(r) == "try-error"){
		
			anova[["error"]] <- list(errorType="badData", errorMessage="ANOVA could not be performed. Most likely due to too few observations for the interaction.")
		} else {
		
		if (options$sumOfSquares == "type3" | options$sumOfSquares == "type2") {
				
			r <- as.data.frame(r$anova)
			colnames(r) <- c("Sum Sq", "Df", "Error SS", "residualDf",   "F value", "Pr(>F)")  
			
			data <- list()

			for (i in 2:3) {
		
				if (i == 2) {
				
					indNewGroup <- c()
				
		
					data[[length(data)+1]] <- list(case="Between Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)
			
					if(length(bt.vars) == 0){
						s <- r[1,]
				
						s[,"Mean Sq"] <- s[,1]/s[,2]
						s[.v("Residuals"), "Sum Sq"] <- s[1,3]
						s[.v("Residuals"), "Df"] <- s[1,4]
						s[.v("Residuals"), "Mean Sq"] <- s[1,3]/s[1,4]
						s <- s[2,]
					} else {
						s <- r[2:2^length(bt.vars),]
										
						s[,"Mean Sq"] <- s[,1]/s[,2]
						s[.v("Residuals"), "Sum Sq"] <- s[1,3]
						s[.v("Residuals"), "Df"] <- s[1,4]
						s[.v("Residuals"), "Mean Sq"] <- s[1,3]/s[1,4]
						}
						
				} else if (i == 3) {				
		
					data[[length(data)+1]] <- list(case="Within Subjects", SS="", df="", MS="", F="", p="", .isNewGroup=TRUE)

					s <- r[-(1:(2^length(bt.vars))),]
									
					s[ ,"Mean Sq"] <- s[,1]/s[,2]
					
					inc <- 0
					
					if (length(df.rm.factor.names)==1) {
						indicat <- 1
					} else {
						indicat <- length(df.rm.factor.names)+1
						}
									
					for(z in seq_len(indicat)){
						
						i <- z* 2^(length(bt.vars))  + inc
						place <- i+1
												
						nd <- data.frame("Sum Sq"=numeric(0), "Df"=numeric(0), "Error SS"=numeric(0), "residualDf"=numeric(0),   "F value"=numeric(0), "Pr(>F)"=numeric(0), "Mean Sq" = numeric(0))
						colnames(nd) <- c("Sum Sq", "Df", "Error SS", "residualDf",   "F value", "Pr(>F)", "Mean Sq")
						nd[.v("Residuals"), "Sum Sq"] <- s[i,3]
						nd[.v("Residuals"), "Df"] <- s[i,4]
						nd[.v("Residuals"), "Mean Sq"] <- s[i,3]/s[i,4]
						
						insertRow <- function(existingDF, newrow, r){
						# function that inserts a row in a data frame at a specific position	
						rbind(existingDF[1:(r-1),],newrow,existingDF[-(1:(r-1)),])
						}						
						s <- insertRow(s, nd, place)
						inc <- inc + 1
					}
				}

				r.names <- row.names(s)
				resi <- .v("Residuals")
				indNewGroup <- c(1,grep(paste("^",resi, sep = ""), r.names)+1)
					
				for (j in .indices(r.names)) {
			
					row.name <- r.names[j]
					row <- s[row.name,]
				
					row.name <- stringr::str_trim(row.name)
					
					if (grepl(paste("^",resi, sep = ""), row.name)) {
	
						row.name <- "Residual"
						data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F="", p="", .rowLevel=1)
					
					} else {
					
						row.name <- .unvf(row.name)
						
						data[[length(data)+1]] <- list(case=row.name, SS=row[["Sum Sq"]], df=row[["Df"]], MS=row[["Mean Sq"]], F=row[["F value"]], p=row[["Pr(>F)"]], .rowLevel=1, .isNewGroup=(any(j == indNewGroup)))
					}
				}
			}
		}
		
			anova[["data"]] <- data
			
			if (options$sumOfSquares == "type2") {
			
				anova[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Type II Sum of Squares"))
			}
			
			if (options$sumOfSquares == "type3") {
			
				anova[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Type III Sum of Squares"))
			}
		}
	}
			
	results[["anova"]] <- anova
	results
}