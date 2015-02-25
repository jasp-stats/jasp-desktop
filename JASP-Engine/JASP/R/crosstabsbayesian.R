
.crosstabBayesian <- function(dataset, options, perform, analysis) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)
	
	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL
	
	all.vars <- c(unlist(analysis), counts.var)
	
	dataset <- subset(dataset, select=.v(all.vars))
	
	# the following creates a 'groups' list
	# a 'group' represents a combinations of the levels from the layers
	# if no layers are specified, groups is null

	if (length(analysis) >= 3)  # if layers are specified
	{
		lvls <- base::levels(dataset[[ .v(analysis[[3]]) ]])
		
		if (length(lvls) < 2) {
		
			lvls <- ""
			
		} else {
		
			lvls <- c(lvls, "")  # blank means total
		}

		# here we create all combinations of the levels from the layers
		# it is easiest to do this with a data frame
		# at the end we convert this to a list of rows

		groups <- data.frame(lvls, stringsAsFactors=FALSE)
		base::names(groups) <- analysis[[3]]
		
		if (length(analysis) >= 4) {
		
			for (j in 4:length(analysis))
			{
				lvls <- base::levels(dataset[[ .v(analysis[[j]]) ]])
				lvls <- c(lvls, "")  # blank means total
			
				groups <- cbind(rep(lvls, each=dim(groups)[1]), groups, stringsAsFactors=FALSE)
				names(groups)[1] <- analysis[[j]]
			}
		}
		
		# convert all the combinations to a list of rows
		
		groups <- .dataFrameToRowList(groups)
		
	} else {  # if layers are not specified
	
		groups <- NULL
	}
	
	tables <- list()

    ### SETUP COLUMNS COMMON TO BOTH TABLES

	fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}
		
	### SETUP COUNTS TABLE SCHEMA

	counts.table <- list()
	
	counts.table[["title"]] <- "Bayesian Contingency Tables"
	
	counts.fields <- fields
	
	counts.fields[[length(counts.fields)+1]] <- list(name=analysis$rows, type="string", combine=TRUE)

	lvls <- c()
	if (is.factor(dataset[[ .v(analysis$columns) ]] )) {

		lvls <- base::levels(dataset[[ .v(analysis$columns) ]])

	} else  {
	
		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
	}
	
	counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
	
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		if (identical(counts, as.integer(counts)) == FALSE)  # are the counts floating point?
			counts.fp <- TRUE
	}
	
	for (column.name in lvls) {

		private.name <- base::paste(column.name,"[counts]", sep="")
		
		if (counts.fp || options$countsExpected) {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		
		} else {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="integer")
		}
		
		if (options$countsExpected) {
		
			private.name <- base::paste(column.name,"[expected]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		}
	}
	
	# Totals columns
	
	if (counts.fp || options$countsExpected) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]",   title="Total", type="number", format="sf:4;dp:2")	
		
	} else {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]", title="Total", type="integer")
	}

	if (options$countsExpected) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[expected]", title="Total", type="number", format="sf:4;dp:2")
	}

	schema <- list(fields=counts.fields)

	counts.table[["schema"]] <- schema
	

	### SETUP TESTS TABLE SCHEMA

	tests.table <- list()
	
	tests.table[["title"]] <- "Bayesian Contingency Tables Tests"
	
	tests.fields <- fields
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[BF]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[BF]", title="Value", type="number", format="sf:4;dp:3")
	tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")
	
	schema <- list(fields=tests.fields)
	
	tests.table[["schema"]] <- schema
	
	##### Odds ratio
	if (options$oddsRatio) {
		
		oddsratio.table <- list()
		
		oddsratio.table[["title"]] <- "Log Odds Ratio"
		
		oddsratio.fields <- fields
			
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="value[oddsRatio]", title="Odds ratio", type="number", format="sf:4;dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="low[oddsRatio]", title="Lower CI", type="number", format="dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="up[oddsRatio]",  title="Upper CI", type="number", format="dp:3")
		
		schema <- list(fields=oddsratio.fields)
		
		oddsratio.table[["schema"]] <- schema
	}
	
	##### Odds ratio Plots
	
	
	status <- list(error=FALSE)
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		
		if (any(counts < 0) || any(is.infinite(counts)))
			status <- list(error=TRUE, errorMessage="Counts may not contain negative numbers or infinite number")
	}
	


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, .v(analysis$rows), .v(analysis$columns), groups, .v(counts.var))
	
	if (all(dim(group.matrices[[1]]) == c(2,2))) {
	
		isTwoByTwo <- TRUE
	} else {
	
		isTwoByTwo <- FALSE
	}
	
	plots <- list()
	counts.rows <- list()
	tests.rows <- list()
	oddsratio.rows <- list()	
	tests.footnotes <- .newFootnotes()
	oddsratio.footnotes <- .newFootnotes()

	samples <- NULL
	CI <- NULL
	medianSamples <- NULL
	BF <- NULL
	
	for (i in 1:length(group.matrices)) {
	
		group.matrix <- group.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
	
		next.rows <- .crosstabsCreateCountsRows(analysis$rows, group.matrix, options, perform, group, status)
		counts.rows <- c(counts.rows, next.rows)
		
		next.rows <- .crosstabsBayesianCreateTestsRows(analysis$rows, group.matrix, tests.footnotes, options, perform, group, status)
		tests.rows <- c(tests.rows, next.rows)
		BF <- next.rows[[1]]$`value[BF]`
		
		next.rows <- .crosstabsBayesianCreateoddratioRows(analysis$rows, group.matrix, oddsratio.footnotes, options, perform, group, status)
		oddsratio.rows <- c(oddsratio.rows, next.rows[[1]])
		samples <- next.rows$samples
		CI <- next.rows$CI
		medianSamples <- next.rows$medianSamples
		
		plot <- .crosstabsBayesianPlotoddsratio(analysis$rows, group.matrix, options, perform, group, status=status, samples=samples, CI=CI, medianSamples=medianSamples, BF=BF, isTwoByTwo= isTwoByTwo)

		plots <- c(plots, plot)
	}

	counts.table[["data"]] <- counts.rows
	if (status$error)
		counts.table[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

	tables[[1]] <- counts.table
	
	tests.table[["data"]] <- tests.rows
	tests.table[["footnotes"]] <- as.list(tests.footnotes)
	if (status$error)
		tests.table[["error"]] <- list(errorType="badData")
		
	tables[[2]] <- tests.table
	
	if (options$oddsRatio) {
		 
	
		oddsratio.table[["data"]] <- oddsratio.rows 
		oddsratio.table[["footnotes"]] <- as.list(oddsratio.footnotes)
	
		if (status$error)
			oddsratio.table[["error"]] <- list(errorType="badData")
			
		tables[[3]] <- oddsratio.table
	}
	
	list(tables=tables, plots=plots)
}

.crosstabsBayesianCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	
	row[["type[N]"]] <- "N"

		if (perform == "run" && status$error == FALSE) {
	
		row[["value[N]"]] <- base::sum(counts.matrix)
		
	} else {
	
		row[["value[N]"]] <- "."
	}
	
		
	if (options$samplingModel == "poisson") {
	
		if (options$bayesFactorType == "BF10"){
			bfLabel <- "BF\u2081\u2080 Poisson"
			
		} else if (options$bayesFactorType == "BF01"){
			bfLabel <- "BF\u2080\u2081 Poisson"
		
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <- " Log\u2009(\u2009BF\u2080\u2081) Poisson"		
		}
		
		sampleType <- "poisson"
		fixedMargin <- NULL
		
	} else if (options$samplingModel == "jointMultinomial") {
	
		if (options$bayesFactorType == "BF10"){
			bfLabel <- "BF\u2081\u2080 joint multinomial"
			
		} else if (options$bayesFactorType == "BF01"){
			bfLabel <- "BF\u2080\u2081 joint multinomial"
			
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <- "Log\u2009(\u2009BF\u2080\u2081) joint multinomial"
		}
		
		sampleType <- "jointMulti"
		fixedMargin <- NULL
		
	} else if (options$samplingModel =="independentMultinomialRowsFixed") {

		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u2081 independent multinomial"
			
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}
			
				
		} else if (options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}
						 
		} else if(options$hypothesis =="groupTwoGreater") { 
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
			
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}				
		}
				
		sampleType <- "indepMulti"
		fixedMargin <- "rows"
		
	} else if (options$samplingModel=="independentMultinomialColumnsFixed") {
	
		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u2081 independent multinomial"
			
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}
				
		} else if(options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A independent multinomial"
			
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}
							 
		} else if (options$hypothesis=="groupTwoGreater"){
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2080\u2081) independent multinomial"
			}				
		}
			
		sampleType <- "indepMulti"
		fixedMargin <- "cols"
		
	} else if (options$samplingModel=="hypergeometric") {

		if (options$bayesFactorType == "BF10"){
			bfLabel <- "BF\u2081\u2080 hypergeometric"
			
		} else if (options$bayesFactorType == "BF01"){
			bfLabel <- "BF\u2080\u2081 hypergeometric"
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <-"Log\u2009(\u2009BF\u2080\u2081) hypergeometric"
		}
		
		sampleType <- "hypergeom"
		fixedMargin <- NULL
		
	} else {
	
		stop("wtf?")
	}
	
	row[["type[BF]"]] <- bfLabel

	if (perform == "run" && status$error == FALSE) {
	
		BF <- try({

			BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType=sampleType, priorConcentration=options$priorConcentration, fixedMargin=fixedMargin)
			bf1 <- exp(as.numeric(BF@bayesFactor$bf))
			lbf1 <- as.numeric(BF@bayesFactor$bf)
			
		
			if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialColumnsFixed") {
				
				count.matrix <- base::t(counts.matrix)
				a <- options$priorConcentration

				s1 <- count.matrix[1,1]
				f1 <- count.matrix[1,2]

				s2 <- count.matrix[2,1]
				f2 <- count.matrix[2,2]

				p1 ~ stats::beta(a+s1, a+f1)
				p2 ~ stats::beta(a+s2, a+f2)

				N.sim <- 10000
				p1.sim <- stats::rbeta(N.sim, a+s1, a+f1)
				p2.sim <- stats::rbeta(N.sim, a+s2, a+f2)
				prop.consistent <- sum(p1.sim > p2.sim)/N.sim
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
			} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialRowsFixed"){
					
				count.matrix <- counts.matrix
				a <- options$priorConcentration

				s1 <- count.matrix[1,1]
				f1 <- count.matrix[1,2]

				s2 <- count.matrix[2,1]
				f2 <- count.matrix[2,2]

				p1 ~ stats::beta(a+s1, a+f1)
				p2 ~ stats::beta(a+s2, a+f2)

				N.sim <- 10000
				p1.sim <- stats::rbeta(N.sim, a+s1, a+f1)
				p2.sim <- stats::rbeta(N.sim, a+s2, a+f2)
				prop.consistent <- sum(p1.sim > p2.sim)/N.sim
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
			} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialColumnsFixed") {
							
				count.matrix <- base::t(counts.matrix)
				a <- options$priorConcentration

				s1 <- count.matrix[1,1]
				f1 <- count.matrix[1,2]

				s2 <- count.matrix[2,1]
				f2 <- count.matrix[2,2]

				p1 ~ stats::beta(a+s1, a+f1)
				p2 ~ stats::beta(a+s2, a+f2)
		
				N.sim <- 10000
				p1.sim <- stats::rbeta(N.sim, a+s1, a+f1)
				p2.sim <- stats::rbeta(N.sim, a+s2, a+f2)
				prop.consistent <- sum(p2.sim > p1.sim)/N.sim
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				
			} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialRowsFixed"){
					
				count.matrix <- counts.matrix
				a <- options$priorConcentration

				s1 <- count.matrix[1,1]
				f1 <- count.matrix[1,2]

				s2 <- count.matrix[2,1]
				f2 <- count.matrix[2,2]

				p1 ~ stats::beta(a+s1, a+f1)
				p2 ~ stats::beta(a+s2, a+f2)
		
				N.sim <- 10000
				p1.sim <- stats::rbeta(N.sim, a+s1, a+f1)
				p2.sim <- stats::rbeta(N.sim, a+s2, a+f2)
				prop.consistent <- sum(p2.sim > p1.sim)/N.sim
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			}
					
		})
						
		if (class(BF) == "try-error") {

			row[["value[BF]"]] <- .clean(NaN)
			
			if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$samplingModel=="hypergeometric") {
			
				row[["value[BF]"]] <- .clean(NaN)
			
				sup <- .addFootnote(footnotes, "Hypergeometric contingency tables test restricted to  2 x 2 tables")
				row[[".footnotes"]] <- list("value[BF]"=list(sup))	
			
			} else {
			
				error <- .extractErrorMessage(BF)
			
				sup   <- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[BF]"=list(sup))
			
			}
		
		} else if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$hypothesis=="groupOneGreater") {
			
			row[["value[BF]"]] <- .clean(NaN)
			
			sup <- .addFootnote(footnotes, "Proportion test restricted to 2 x 2 tables")
			row[[".footnotes"]] <- list("value[BF]"=list(sup))
		
		} else if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$hypothesis=="groupTwoGreater") {
			
			row[["value[BF]"]] <- .clean(NaN)
			
			sup <- .addFootnote(footnotes, "Proportion test restricted to 2 x 2 tables")
			row[[".footnotes"]] <- list("value[BF]"=list(sup))
		
		} else {
		
			if (options$bayesFactorType == "BF10"){
			
				bf1 <- bf1
			
			} else if (options$bayesFactorType == "BF01"){
			
				bf1 <- 1/bf1
				
			} else if (options$bayesFactorType == "LogBF10") {
			
				bf1 <- lbf1
			}			
		
			row[["value[BF]"]] <- .clean(bf1)
		}
		
	} else {
	
		row[["value[BF]"]] <- "."
	}	

	list(row)
}

.crosstabsBayesianCreateoddratioRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) { 

	row <- list()
	samples <- NULL
	CI <- NULL
	medianSamples <- NULL
	BF <- NULL
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			row[[".isNewGroup"]] <- TRUE
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	if (options$oddsRatio) {
	
		row[["type[oddsRatio]"]] <- "Odds ratio"

		if (perform == "run" && status$error == FALSE) {
		
			if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {

					row[["value[oddsRatio]"]] <- .clean(NaN)
					row[["low[oddsRatio]"]] <- ""
					row[["up[oddsRatio]"]] <-  ""
			
					sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
					row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
				} else if ( options$samplingModel== "hypergeometric") {

				row[["value[oddsRatio]"]] <- .clean(NaN)
				row[["low[oddsRatio]"]] <- ""
				row[["up[oddsRatio]"]] <-  ""
			
				sup <- .addFootnote(footnotes, "Odd ratio for this model not yet implemented")
				row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
			} else {

				OR <- try({
	
					if(options$samplingModel == "poisson"){
						sampleType <- "poisson"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						lambda<-as.data.frame(chi.result,col.names=c("lambda11","lambda21","lambda12","lambda22"))
						odds.ratio<-(lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
			
					} else if (options$samplingModel == "jointMultinomial"){
			
						sampleType <- "jointMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result,col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
				
					} else if (options$samplingModel == "independentMultinomialRowsFixed"){
			
						sampleType <- "indepMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "rows")
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
				
					} else if (options$samplingModel == "independentMultinomialColumnsFixed"){
			
						sampleType <- "indepMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "cols")
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])				
					} 
				})
									
				logOR<-log(odds.ratio)
				samples <- logOR
				BF <- BayesFactor::extractBF(BF)[1, "bf"]
				
				z<-stats::density(logOR)
				#x.mode <- z$x[i.mode <- which.max(z$y)]
				x.median <- stats::median(logOR)
				medianSamples <- x.median
				Sig <- options$oddsRatioCredibleIntervalInterval
				alpha <- (1 - Sig)/2
				x0 <- unname(stats::quantile(logOR, p = alpha))
				x1 <- unname(stats::quantile(logOR, p = (1-alpha)))
				
				CI <- c(x0, x1)
				
				if (class(OR) == "try-error") {
		
					row[["value[oddsRatio]"]] <- .clean(NaN)
		
					error <- .extractErrorMessage(BF)
		
					sup   <- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
		
				} else  {
		
					row[["value[oddsRatio]"]] <- x.median
					row[["low[oddsRatio]"]] <- x0
					row[["up[oddsRatio]"]] <- x1
				}
			}
		}
		
	} else {

		row[["value[oddsRatio]"]] <- "."
	}
		
	list(list(row), samples=samples, CI=CI, medianSamples=medianSamples, BF= BF)
}

.plotPosterior.crosstabs <- function(samples, CI, medianSamples, BF, oneSided= FALSE, iterations= 10000, lwd= 2, cexPoints= 1.5,
 cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addInformation= FALSE, dontPlotData=FALSE, selectedCI= options$oddsRatioCredibleIntervalInterval) {
	
	if (addInformation) {
	
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
		
	} else {
	
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
	}
	
	if (dontPlotData) {
	
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		mtext("log(Odds ratio)", side = 1, cex = cexXlab, line= 2.5)
	
		return()
	}
	
	
	BF10 <- BF
	BF01 <- 1 / BF10
	
	# fit denisty estimator
	fit.posterior <-  logspline::logspline(samples)
	
	# density function posterior
	dposterior <- function(x, oneSided= oneSided, samples= samples){
	
		return(logspline::dlogspline(x, fit.posterior))
	}
	
	
	# set limits plot
	xlim <- vector("numeric", 2)
	
	xlim[1] <- min(-2, quantile(samples, probs = 0.01)[[1]])
	xlim[2] <- max(2, quantile(samples, probs = 0.99)[[1]])
	stretch <- 1.2	

	
	ylim <- vector("numeric", 2)
	
	ylim[1] <- 0
	ylim[2] <- stretch * max(dposterior(x= samples, oneSided= oneSided, samples=samples))
	
	# calculate position of "nice" tick marks and create labels
	xticks <- pretty(xlim)
	yticks <- pretty(ylim)
	xlabels <- formatC(xticks, 1, format= "f")
	ylabels <- formatC(yticks, 1, format= "f")
	
	# compute 95% credible interval & median:
	
	CIlow <- CI[1]
	CIhigh <- CI[2]
	medianPosterior <- medianSamples
	
	
	posteriorLine <- dposterior(x= seq(min(xticks), max(xticks),length.out = 1000), oneSided = oneSided, samples=samples)
	
	xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
	
	plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(seq(min(xticks), max(xticks),length.out = 1000),posteriorLine, lwd= lwd)
		
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext("log(Odds ratio)", side = 1, cex = cexXlab, line= 2.5)	
	
	
	# credible interval
	dmax <- optimize(function(x)dposterior(x,oneSided= oneSided, samples=samples), interval= range(xticks), maximum = TRUE)$objective # get maximum density
	
	# enable plotting in margin
	par(xpd=TRUE)
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
	
	medianText <- formatC(medianPosterior, digits= 3, format="f")
	
	
	if (addInformation) {
		
		# display BF10 value
		offsetTopPart <- 0.06	
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000) {
			BF10t <- formatC(BF10,3, format = "e")
			BF01t <- formatC(BF01,3, format = "e")
		}
		
		if (BF10 < 1000000 & BF01 < 1000000) {
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (oneSided == FALSE) {
			
			text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		
		CIwidth <- selectedCI * 100
		CInumber <- paste(CIwidth, "% CI: [", sep="")
		CIText <- paste(CInumber,  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		medianLegendText <- paste("median =", medianText)
		
		text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
		text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		
		# probability wheel
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		# make sure that colored area is centered		
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel		
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (oneSided == FALSE) {
			
			text(xx, yy, "data|H1", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy, "data|H+", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy, "data|H-", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		# add legend		
		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		
		medianLegendText <- paste("median =", medianText)
	}
	
	mostPosterior <- mean(samples > mean(range(xticks)))
}

.crosstabsBayesianPlotoddsratio <- function(var.name, counts.matrix, options, perform, group, status, medi, samples, CI, medianSamples, BF10, isTwoByTwo) { 


	if (!options$plotPosteriorOddsRatio || !isTwoByTwo)
		return()
	
	OddratioPlots <- list()
	oddsratio.plot <- list()
	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			row[[".isNewGroup"]] <- TRUE
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
    group[group==""] <- "Total"
    #group[group==""] <- names(group)

	if (options$plotPosteriorOddsRatio){
	
		
		if (length(group) == 0) {
		
			oddsratio.plot[["title"]] <- "Odds ratio"
			
		} else if (length(group) > 0) {
			
			layerLevels <- paste(names(group),"=", group)
			layerLevels <- gsub(pattern = " = Total", layerLevels, replacement = "")
			# print(paste(layerLevels, collapse="; "))
			plotTitle <- paste(layerLevels, collapse="; ")
			oddsratio.plot[["title"]] <- plotTitle
		}		
		
		oddsratio.plot[["width"]]  <- 530
		oddsratio.plot[["height"]] <- 400
		#oddsratio.plot[["custom"]] <- list(width="plotWidths", height="plotHeights")
		
		image <- .beginSaveImage(530, 400)
		.plotPosterior.crosstabs(dontPlotData=TRUE,addInformation=options$plotPosteriorOddsRatioAdditionalInfo)
		oddsratio.plot[["data"]] <- .endSaveImage(image) 	
		oddsratio.plot[["status"]] <- "running"
		
		
		if (status$error) {
		
			oddsratio.plot[["error"]] <- list(error="badData", errorMessage=status$errorMessage)
			oddsratio.plot[["status"]] <- "complete"
			OddratioPlots[[length(OddratioPlots)+1]] <- oddsratio.plot
			
			return(OddratioPlots)
		}
		
		if (perform == "run" && status$error == FALSE) {
		
		
			if (! identical(dim(counts.matrix),as.integer(c(2,2)))) {
			
			} else if ( options$samplingModel== "hypergeometric") {
			
				oddsratio.plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: Odd ratio for this model not yet implemented")
				oddsratio.plot[["status"]] <- "complete"
			} else {
			
				if(options$samplingModel== "poisson"){
					sampleType <- "poisson"
					BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
					chi.result <- BayesFactor::posterior(BF, iterations = 10000)
					lambda<-as.data.frame(chi.result,col.names=c("lambda11","lambda21","lambda12","lambda22"))
					odds.ratio<-(lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
	
				} else if (options$samplingModel== "jointMultinomial"){
	
					sampleType <- "jointMulti"
					BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
					chi.result <- BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(chi.result,col.names=c("theta11","theta21","theta12","theta22"))
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
				} else if (options$samplingModel== "independentMultinomialRowsFixed"){
	
					sampleType <- "indepMulti"
					BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "rows")
					chi.result <- BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
				} else if (options$samplingModel== "independentMultinomialColumnsFixed"){
	
					sampleType <- "indepMulti"
					BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "cols")
					chi.result <- BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
				}
				
				
								
				# do this if no values are parsed to plotting function
				if (is.null(samples) && is.null(CI) && is.null(medianSamples)) {
				
					# BF10 <- BayesFactor::extractBF(BF)[1, "bf"]
					logOR <-log(odds.ratio)
					samples <- logOR
					medianSamples <- stats::median(logOR)					
					CI <- options$oddsRatioCredibleIntervalInterval
					Sig <- (1 - CI)/2
					x0 <- unname(stats::quantile(logOR, p = Sig))
					x1 <- unname(stats::quantile(logOR, p = (1-Sig)))
					CI <- c(x0, x1)
				}
				
				
				#image <- .beginSaveImage(530, 400)
				
				# par(mar= c(5, 4.5, 8, 2) + 0.1, xpd=TRUE, cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, las=1)
				# digitsize <- 1.2
				# y.mode <- z$y[i.mode]
				# lim<-max(z$x)-min(z$x)
				# fit<-logspline::logspline(logOR)
				# ylim0 <- c(0,1.1*y.mode )
				# xlow<-unname(stats::quantile(logOR, p =0.0001))
				# xhigh<-unname(stats::quantile(logOR, p =0.9999))
				# xticks <- pretty(c(xlow,xhigh), min.n= 3)
				# 
				# if (length(group) > 0) {
				# 
				# 	plot(1, type="n", ylim=ylim0, xlim=range(xticks),
				# 		axes=F, 
				# 		main =paste(names(group),"=", group), xlab="log(Odds ratio)", ylab="Posterior Density")
				# 
				# } else {
                # 
				# 	plot(1, type="n", ylim=ylim0, xlim=range(xticks),
				# 		axes=F, 
				# 		xlab="log(Odds ratio)", ylab="Posterior Density")
				# }
				# 		
				# plot(function(x)logspline::dlogspline(x, fit), xlim = range(xticks), lwd=2, add=TRUE)
				# axis(1, line=0.3, at=xticks, lab=xticks)
				# axis(2)
				# CI1<-CI*100
				# CI1<-bquote(.(CI1))
				# arrows(x0, 1.07*y.mode, x1, 1.07*y.mode, length = 0.05, angle = 90, code = 3, lwd=2)
				# #text(-1.5, 0.8, expression(log('BFI'[10]) == 22.60),cex=digitsize)
				# text(x.mode,y.mode+(y.mode/4), paste("Median =", round(x.median,digit=3)), cex=digitsize)
				# text(x.mode, y.mode+(y.mode/7), paste(CI1,"%"), cex=digitsize)
				# text(x0, y.mode, round(x0, digits = 3) , cex=digitsize)
				# text(x1, y.mode, round(x1, digits = 3) , cex=digitsize)
				
				#.plotPosterior.crosstabs(samples=samples, CI=CI, medianSamples=medianSamples, BF=BF10, selectedCI= options$oddsRatioCredibleIntervalInterval)
				
				
				if (BF10 == "NaN") {
				
					oddsratio.plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: The Bayes factor is NaN")
					
				}
				 else if (is.infinite(1 / BF10)) {
				
					oddsratio.plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: The Bayes factor is too small")
					
				} else if (is.infinite(BF10)) {
				
					oddsratio.plot[["error"]] <- list(error="badData", errorMessage="Plotting is not possible: BayesFactor is infinite")
					
				} else {
				
					p <- try(silent= FALSE, expr= {
						
							image <- .beginSaveImage(530, 400)
						
							if (options$hypothesis=="groupTwoGreater") {
								oneSided <- "left"
						
							} else if (options$hypothesis=="groupOneGreater") {
								oneSided <- "right"
						
							} else {
								oneSided <- FALSE
							}
								
							.plotPosterior.crosstabs(samples=samples, CI=CI, medianSamples=medianSamples, BF=BF10, selectedCI= options$oddsRatioCredibleIntervalInterval,
									addInformation=options$plotPosteriorOddsRatioAdditionalInfo, oneSided= oneSided)
						
							oddsratio.plot[["data"]] <- .endSaveImage(image)
						}
					)
							
					if (class(p) == "try-error") {
						
						errorMessage <- .extractErrorMessage(p)
						
						if (errorMessage == "not enough data") {
						
								errorMessage <- "Plotting is not possible: The Bayes factor is too small"
						} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
						
							errorMessage <- "Plotting is not possible: The Bayes factor is too small"
						}
						
						oddsratio.plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
					}
				}
				
				oddsratio.plot[["status"]] <- "complete"				
			}
		}
	}
	
	OddratioPlots[[length(OddratioPlots)+1]] <- oddsratio.plot
	OddratioPlots
}
	

CrosstabsBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	layer.variables <- c()

	for (layer in options$layers)
		layer.variables <- c(layer.variables, unlist(layer$variables))

	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL

	factors <- c(unlist(options$rows), unlist(options$columns), layer.variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.factor=factors, columns.as.numeric=counts.var)
		} else {
			dataset <- .readDataSetHeader(columns.as.factor=factors, columns.as.numeric=counts.var)
		}
	}

	results <- list()
	
	### META

	meta <- list()
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="Contingency Tables", type="tables")
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	
	results[["title"]] <- "Bayesian Contingency Tables"	
	
	### CROSS TABS
	
	crosstabs <- list()
	plots <- list()

	if (length(options$rows) > 0 && length(options$columns) > 0)
	{
		rows <- as.vector(options$rows, "character")
		columns <- as.vector(options$columns, "character")

		analyses <- data.frame("columns"=columns, stringsAsFactors=FALSE)
		analyses <- cbind(analyses, "rows"=rep(rows, each=dim(analyses)[1]), stringsAsFactors=FALSE)
		
		for (layer in options$layers)
		{
			layer.vars <- as.vector(layer$variables, "character")
			analyses <- cbind(analyses, rep(layer.vars, each=dim(analyses)[1]), stringsAsFactors=FALSE)
			names(analyses)[dim(analyses)[2]] <- layer$name
		}
		
		analyses <- .dataFrameToRowList(analyses)

		for (analysis in analyses)
		{
			res <- .crosstabBayesian(dataset, options, perform, analysis)
			
			for (table in res$tables)
				crosstabs[[length(crosstabs)+1]] <- table
				
			for (plot in res$plots)
				plots[[length(plots)+1]] <- plot
		}
	
	} else {
	
		crosstabs[[1]] <- list(title = "Bayesian Contingency Tables", cases = list(), schema = list(fields=list()))
	}

	results[["Contingency Tables"]] <- crosstabs
	results[["plots"]] <- plots

	results
}

