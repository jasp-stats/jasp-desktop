
.crosstab <- function(dataset, options, perform, analysis) {

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
	
	counts.table[["title"]] <- "Crosstabs"
	
	counts.fields <- fields
	
	counts.fields[[length(counts.fields)+1]] <- list(name=analysis$rows, type="string", combine=TRUE)
	

	lvls <- c()
	if (is.factor(dataset[[ .v(analysis$columns) ]] )) {

		lvls <- base::levels(dataset[[ .v(analysis$columns) ]])

	} else if (perform == "run") {
	
		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
	}
	

	counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
	
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		if (identical(counts, as.integer(counts)) == FALSE)  # are the counts floating point?
			counts.fp <- TRUE
	}
	
	if (options$countsExpected || options$percentages$row || options$percentages$column || options$percentages$total ) {
	
	
		counts.fields[[length(counts.fields)+1]] <- list(name="type[counts]", title="", type="string")
	
		if (options$countsExpected) {	
			counts.fields[[length(counts.fields)+1]] <- list(name="type[expected]", title="", type="string")
			}
	
		if (options$percentages$row) {

			#counts.fields[[length(counts.fields)+1]] <- list(name="type[counts]", title="", type="string")
			counts.fields[[length(counts.fields)+1]] <- list(name="type[rowproportions]", title="", type="string")
		}

		if (options$percentages$column) {

			#counts.fields[[length(counts.fields)+1]] <- list(name="type[counts]", title="", type="string")
			counts.fields[[length(counts.fields)+1]] <- list(name="type[colproportions]", title="", type="string")
		}

		if (options$percentages$total) {

			#counts.fields[[length(counts.fields)+1]] <- list(name="type[counts]", title="", type="string")
			counts.fields[[length(counts.fields)+1]] <- list(name="type[proportions]", title="", type="string")
		}
	
	}
	
	
	for (column.name in lvls) {

		private.name <- base::paste(column.name,"[counts]", sep="")

		if (counts.fp || options$countsExpected || options$percentages$row || options$percentages$column || options$percentages$total ) {
			
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		
		} else {
		
			#counts.fields[[length(counts.fields)+1]] <- list(name="Count",type="string")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="integer")
		}
		
		if (options$countsExpected) {
			
			private.name <- base::paste(column.name,"[expected]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		}
		
		if (options$percentages$row) {
			
			private.name <- base::paste(column.name,"[rowproportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}
		
		if (options$percentages$column) {
			
			private.name <- base::paste(column.name,"[colproportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}
		
		if (options$percentages$total) {
			
			private.name <- base::paste(column.name,"[proportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}
	}
	
	# Totals columns
	
	if (counts.fp || options$countsExpected || options$percentages$row || options$percentages$column || options$percentages$total) {

	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]",   title="Total", type="number", format="sf:4;dp:2")	
		
	} else {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]", title="Total", type="integer")
	}

	if (options$countsExpected) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[expected]", title="Total", type="number", format="sf:4;dp:2")
	}
	
	if (options$percentages$row) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[rowproportions]", title="Total", type="number", format="dp:1;pc")
	}
	
	if (options$percentages$column) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[colproportions]", title="Total", type="number", format="dp:1;pc")
	}
	
	if (options$percentages$total) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[proportions]", title="Total", type="number", format="dp:1;pc")
	}

	schema <- list(fields=counts.fields)

	counts.table[["schema"]] <- schema
	

	### SETUP TESTS TABLE SCHEMA
	
	if (options$chiSquared || options$chiSquaredContinuityCorrection || options$likelihoodRatio) {

		tests.table <- list()
	
		tests.table[["title"]] <- "Chi-Squared Tests"
	
		tests.fields <- fields
	
		if (options$chiSquared){
	
			tests.fields[[length(tests.fields)+1]] <- list(name="type[chiSquared]", title="", type="string")
			tests.fields[[length(tests.fields)+1]] <- list(name="value[chiSquared]", title="Value", type="number", format="sf:4;dp:3")
			tests.fields[[length(tests.fields)+1]] <- list(name="df[chiSquared]", title="df", type="integer")
			tests.fields[[length(tests.fields)+1]] <- list(name="p[chiSquared]", title="p", type="number", format="dp:3;p:.001")
		}
		
		if (options$chiSquaredContinuityCorrection){	
			tests.fields[[length(tests.fields)+1]] <- list(name="type[chiSquared-cc]", title="", type="string")
			tests.fields[[length(tests.fields)+1]] <- list(name="value[chiSquared-cc]", title="Value", type="number", format="sf:4;dp:3")
			tests.fields[[length(tests.fields)+1]] <- list(name="df[chiSquared-cc]", title="df", type="integer")
			tests.fields[[length(tests.fields)+1]] <- list(name="p[chiSquared-cc]", title="p", type="number", format="dp:3;p:.001")
		}
		
		if (options$likelihoodRatio) {
			tests.fields[[length(tests.fields)+1]] <- list(name="type[likelihood]", title="", type="string")
			tests.fields[[length(tests.fields)+1]] <- list(name="value[likelihood]", title="Value", type="number", format="sf:4;dp:3")
			tests.fields[[length(tests.fields)+1]] <- list(name="df[likelihood]", title="df", type="integer")
			tests.fields[[length(tests.fields)+1]] <- list(name="p[likelihood]", title="p", type="number", format="dp:3;p:.001")
		}

		tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")
		
		if (counts.fp) {
		
			tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="number", format="sf:4;dp:2")
			
		} else {
		
			tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")			
		}
		
		tests.fields[[length(tests.fields)+1]] <- list(name="df[N]", title="df")
		tests.fields[[length(tests.fields)+1]] <- list(name="p[N]", title="p")
	
		schema <- list(fields=tests.fields)
	
		tests.table[["schema"]] <- schema
	
	}
	
	############# Odds ratio
	if (options$oddsRatio) {
		
		oddsratio.table <- list()
		
		oddsratio.table[["title"]] <- "Log Odds ratio"
		
		oddsratio.fields <- fields
			
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="type[oddsRatio]", title="", type="string")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="value[oddsRatio]", title="log(odds ratio)", type="number", format="sf:4;dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="low[oddsRatio]", title="Lower CI", type="number", format="dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="up[oddsRatio]",  title="Upper CI", type="number", format="dp:3")
		
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="type[FisherTest]", title="", type="string")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="value[FisherTest]", title="Odds ratio", type="number", format="sf:4;dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="low[FisherTest]", title="Lower CI", type="number", format="dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="up[FisherTest]",  title="Upper CI", type="number", format="dp:3")
		
		schema <- list(fields=oddsratio.fields)
		
		oddsratio.table[["schema"]] <- schema
	}
	
	
	##### Nominal Table (Symmetric Measures)
	if (options$nominal$contingencyCoefficient|| options$nominal$phiAndCramersV) {
		
		nominal.table <- list()

		nominal.table[["title"]] <- "Nominal"

		nominal.fields <- fields

		if (options$nominal$contingencyCoefficient){

			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[ContCoef]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[ContCoef]", title="Value", type="number", format="sf:4;dp:3")
		}

		if (options$nominal$phiAndCramersV) {
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[PhiCoef]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[PhiCoef]", title="Value", type="number", format="sf:4;dp:3")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[CramerV]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[CramerV]", title="Value", type="number", format="sf:4;dp:3")
		}

		schema <- list(fields=nominal.fields)

		nominal.table[["schema"]] <- schema
	}
	
	##### Ordinal Table
	if (options$ordinal$gamma) {
		
		ordinal.table <- list()
		
		ordinal.table[["title"]] <- "Ordinal Gamma"
		
		ordinal.fields <- fields
			
		#ordinal.fields[[length(ordinal.fields)+1]] <- list(name="type[gammaCoef]", title="", type="string")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="value[gammaCoef]", title="Gamma", type="number", format="sf:4;dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="Sigma[gammaCoef]", title="std. error", type="number", format="dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="low[gammaCoef]", title="Lower CI", type="number", format="dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="up[gammaCoef]",  title="Upper CI", type="number", format="dp:3")
		
		schema <- list(fields=ordinal.fields)
		
		ordinal.table[["schema"]] <- schema
	}
	
	##########Kendall Tau-B table
	
	if (options$ordinal$kendallsTauB) {
		
		kendalls.table <- list()
		
		kendalls.table[["title"]] <- "Kendall's Tau"
		
		kendalls.fields <- fields
			
		#kendalls.fields[[length(kendalls.fields)+1]] <- list(name="type[kTauB]", title="", type="string")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="value[kTauB]", title="Kendall's Tau B", type="number", format="sf:4;dp:3")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="statistic[kTauB]", title="z statistic", type="number", format="dp:3")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="p[kTauB]", title="p", type="number", format="dp:3;p:.001")
		
		
		schema <- list(fields=kendalls.fields)
		
		kendalls.table[["schema"]] <- schema
	}
	
	status <- list(error=FALSE)
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		
		if (any(counts < 0)|| any(is.infinite(counts)))
			status <- list(error=TRUE, errorMessage="Counts may not contain negative numbers or infinities")
	}


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, .v(analysis$rows), .v(analysis$columns), groups, .v(counts.var))
	
	counts.rows <- list()
	tests.rows <- list()
	oddsratio.rows <- list()
	nominal.rows <- list()
	ordinal.rows <- list()
	kendalls.rows <- list()
	
	tests.footnotes <- .newFootnotes()
	oddsratio.footnotes <- .newFootnotes()
	nominal.footnotes <- .newFootnotes()
	ordinal.footnotes <- .newFootnotes()
	kendalls.footnotes <- .newFootnotes()

	for (i in 1:length(group.matrices)) {
	
		group.matrix <- group.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
	
		next.rows <- .crosstabsCreateCountsRows(analysis$rows, group.matrix, options, perform, group, status)
		counts.rows <- c(counts.rows, next.rows)
		
		next.rows <- .crosstabsCreateTestsRows(analysis$rows, group.matrix, tests.footnotes, options, perform, group, status)
		tests.rows <- c(tests.rows, next.rows)
		
		next.rows <- .crosstabsCreateoddratioRows(analysis$rows, group.matrix, oddsratio.footnotes, options, perform, group, status)
		oddsratio.rows <- c(oddsratio.rows, next.rows)
		
		next.rows <- .crosstabsCreateNominalRows(analysis$rows, group.matrix, nominal.footnotes, options, perform, group, status)
		nominal.rows <- c(nominal.rows, next.rows)
		
		next.rows <- .crosstabsCreateOrdinalRows(analysis$rows, group.matrix, ordinal.footnotes, options, perform, group, status)
		ordinal.rows <- c(ordinal.rows, next.rows)
		
		next.rows <- .crosstabsCreateOrdinalTau(analysis$rows, group.matrix, ordinal.footnotes, options, perform, group, status)
		kendalls.rows <- c(kendalls.rows, next.rows)

	}

	counts.table[["data"]] <- counts.rows
	if (status$error)
		counts.table[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
	
	tables[[1]] <- counts.table
	
	if (options$chiSquared || options$chiSquaredContinuityCorrection || options$likelihoodRatio ) {

		tests.table[["data"]] <- tests.rows
		tests.table[["footnotes"]] <- as.list(tests.footnotes)

		if (status$error)
			tests.table[["error"]] <- list(errorType="badData")

		tables[[2]] <- tests.table
	}
	
	if (options$oddsRatio) {
		oddsratio.table[["data"]] <- oddsratio.rows 
		oddsratio.table[["footnotes"]] <- as.list(oddsratio.footnotes)
	
		if (status$error)
			oddsratio.table[["error"]] <- list(errorType="badData")
			
		tables[[3]] <- oddsratio.table
	}
	
	if (options$nominal$contingencyCoefficient || options$nominal$phiAndCramersV) {
	
		nominal.table[["data"]] <- nominal.rows
		nominal.table[["footnotes"]] <- as.list(nominal.footnotes)
		
		if (status$error)
			nominal.table[["error"]] <- list(errorType="badData")
		
		tables[[4]] <- nominal.table
	}
	
	if (options$ordinal$gamma) {
	
		ordinal.table[["data"]] <- ordinal.rows
		ordinal.table[["footnotes"]] <- as.list(ordinal.footnotes)
		
		if (status$error)
			ordinal.table[["error"]] <- list(errorType="badData")
		
		tables[[5]] <- ordinal.table
	}
	
	if (options$ordinal$kendallsTauB) {
	
		kendalls.table[["data"]] <- kendalls.rows
		kendalls.table[["footnotes"]] <- as.list(kendalls.footnotes)
		
		if (status$error)
			kendalls.table[["error"]] <- list(errorType="badData")
		
		tables[[6]] <- kendalls.table
	}

	
	tables
}

.crosstabsCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {

	row <- list()
	row.footnotes <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	row[["type[N]"]] <- "N"
	row[["df[N]"]] <- ""
	row[["p[N]"]] <- ""

	if (perform == "run" && status$error == FALSE) {

		row[["value[N]"]] <- base::sum(counts.matrix)
		
	} else {
	
		row[["value[N]"]] <- "."
	}
	

	if (options$chiSquared) {
	
		row[["type[chiSquared]"]] <- "\u03A7\u00B2"

		if (perform == "run" && status$error == FALSE) {
		
			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix, correct=FALSE)
			})
			
			if (class(chi.result) == "try-error") {

				row[["value[chiSquared]"]] <- .clean(NaN)
				row[["df[chiSquared]"]]<- " "
				row[["p[chiSquared]"]]<- " "
				
				error <- .extractErrorMessage(chi.result)
				
				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"
				
				sup	<- .addFootnote(footnotes, error)
				row.footnotes <- c(row.footnotes, list("value[chiSquared]"=list(sup)))
			
			} else if (is.na(chi.result$statistic)) {
			
				row[["value[chiSquared]"]] <- .clean(NaN)
				row[["df[chiSquared]"]]<- " "
				row[["p[chiSquared]"]]<- " "
				
				message <- "\u03A7\u00B2 could not be calculated"

				warn <- warnings()
				if (length(warn) > 0)
					message <- paste(message, names(warn)[1], sep=" : ")
			
				sup <- .addFootnote(footnotes, message)
				row.footnotes <- c(row.footnotes, list("value[chiSquared]"=list(sup)))
			
			} else {
			
				row[["value[chiSquared]"]] <- unname(chi.result$statistic)
				row[["df[chiSquared]"]] <- unname(chi.result$parameter)
				row[["p[chiSquared]"]] <- unname(chi.result$p.value)
			}
			
		} else {
		
			row[["value[chiSquared]"]] <- "."
			
		}
	}	
	
###############################################	
	
	if (options$chiSquaredContinuityCorrection){	
		row[["type[chiSquared-cc]"]] <- "\u03A7\u00B2 Continuity correction"


		if (perform == "run" && status$error == FALSE) {
		
			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix)
				#row <- list(Method="Pearson's Chi-squared", X2=unname(chi$statistic), df=unname(chi$parameter), p=chi$p.value)
			})
			
			if (class(chi.result) == "try-error") {

				row[["value[chiSquared-cc]"]] <- .clean(NaN)
				row[["df[chiSquared-cc]"]]<- " "
				row[["p[chiSquared-cc]"]]<- " "
				
				error <- .extractErrorMessage(chi.result)
				
				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"
				
				sup	<- .addFootnote(footnotes, error)
				row.footnotes <- c(row.footnotes, list("value[chiSquared-cc]"=list(sup)))
			
			} else if (is.na(chi.result$statistic)) {
			
				row[["value[chiSquared-cc]"]] <- .clean(NaN)
				row[["df[chiSquared-cc]"]]<- " "
				row[["p[chiSquared-cc]"]]<- " "
			
				sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
				row.footnotes <- c(row.footnotes, list("value[chiSquared-cc]"=list(sup)))
			
			} else {
			
				row[["value[chiSquared-cc]"]] <- unname(chi.result$statistic)
				row[["df[chiSquared-cc]"]] <- unname(chi.result$parameter)
				row[["p[chiSquared-cc]"]] <- unname(chi.result$p.value)
			}
			
		} else {
		
			row[["value[chiSquared-cc]"]] <- "."
		}
	}
	
##################################################################

	if (options$likelihoodRatio) {
		
	row[["type[likelihood]"]] <- "Likelihood ratio"


		if (perform == "run" && status$error == FALSE) {
		
			chi.result <- try({

				chi.result <- vcd::assocstats(counts.matrix)
				
			})
			
			if (class(chi.result) == "try-error") {

				row[["value[likelihood]"]] <- .clean(NaN)
				row[["df[likelihood]"]] <- ""
				row[["p[likelihood]"]] <-""
				
				error <- .extractErrorMessage(chi.result)
				
				sup	<- .addFootnote(footnotes, error)
				row.footnotes <- c(row.footnotes, list("value[likelihood]"=list(sup)))
			
			} else {
			
				row[["value[likelihood]"]] <- chi.result$chisq_tests[1]
				row[["df[likelihood]"]] <- chi.result$chisq_tests[3]
				row[["p[likelihood]"]] <- chi.result$chisq_tests[5]
			}
			
		} else {
		
			row[["value[likelihood]"]] <- "."
		}
	}
		
	row[[".footnotes"]] <- row.footnotes

	list(row)
}

.crosstabsCreateNominalRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {
	
	row <- list()
	for (layer in names(group)) {
		
		level <- group[[layer]]
		
		if (level == "") {
			
			row[[layer]] <- "Total"
			
		} else {
			
			row[[layer]] <- level
		}
	}

	if (options$nominal$contingencyCoefficient) {
		 
		 row[["type[ContCoef]"]] <- "Contingency Coefficient"
		 
		 
		if (perform == "run" && status$error == FALSE) {
			 
			 chi.result <- try({
				 
				 chi.result <- vcd::assocstats(counts.matrix)
				 
			})
			
			if (class(chi.result) == "try-error") {
				
				row[["value[ContCoef]"]] <- .clean(NaN)
				
				error <- .extractErrorMessage(chi.result)
				
				sup	<- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[ContCoef]"=list(sup))
				
			} else {
				 
				 row[["value[ContCoef]"]] <- chi.result$contingency
			}
		
		} else {
		
			row[["value[ContCoef]"]] <- "."
		}
		
		}
	
		if (options$nominal$phiAndCramersV) {
		
			row[["type[PhiCoef]"]] <- "Phi-Coefficient"
			
			
			if (perform == "run" && status$error == FALSE) {
			
				chi.result <- try({
				
					chi.result <- vcd::assocstats(counts.matrix)
				})
			
				if (class(chi.result) == "try-error") {
				
					row[["value[PhiCoef]"]] <- .clean(NaN)
				
					error <- .extractErrorMessage(chi.result)
				
					sup	<- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[PhiCoef]"=list(sup))
				
				} else {
					
					row[["value[PhiCoef]"]] <- chi.result$phi
				}
				
			} else {
				
				row[["value[PhiCoef]"]] <- "."
			}
			
		# }
		
		# if (options$nominal$phiAndCramersV) {
			
			row[["type[CramerV]"]] <- "Cramer's V "
			
			
			if (perform == "run" && status$error == FALSE) {
				
				chi.result <- try({
					
					chi.result <- vcd::assocstats(counts.matrix)
					
				})
				
				if (class(chi.result) == "try-error") {
					
					row[["value[CramerV]"]] <- .clean(NaN)
					
					error <- .extractErrorMessage(chi.result)
					
					sup	<- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[CramerV]"=list(sup))
					
				} else {
					
					row[["value[CramerV]"]] <- chi.result$cramer
				}
				
			} else {
				
				row[["value[CramerV]"]] <- "."
			}
			
		}

	 list(row)

	 }
	
.crosstabsCreateOrdinalRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {
	
	row <- list()
	for (layer in names(group)) {
		
		level <- group[[layer]]
		
		if (level == "") {
			
			row[[layer]] <- "Total"
			
		} else {
			
			row[[layer]] <- level
		}
	}
	
	
	if (options$ordinal$gamma) {
		
		#row[["type[gammaCoef]"]] <- "Gamma Coefficient"
		
		
		if (perform == "run" && status$error == FALSE) {
			
			chi.result <- try({
				
				chi.result <- vcdExtra::GKgamma(counts.matrix)
				
			})
			
			if (class(chi.result) == "try-error") {
				
				row[["value[gammaCoef]"]] <- .clean(NaN)
				
				error <- .extractErrorMessage(chi.result)
				
				sup	<- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[gammaCoef]"=list(sup))
				
			} else {
				
				row[["value[gammaCoef]"]] <- chi.result$gamma
				row[["Sigma[gammaCoef]"]] <- chi.result$sigma
				row[["low[gammaCoef]"]] <- chi.result$CI[1]
				row[["up[gammaCoef]"]] <-  chi.result$CI[2]
			}
			
		} else {
			
			 row[["value[gammaCoef]"]] <- "."
			 row[["Sigma[gammaCoef]"]] <- "."
		  	 row[["low[gammaCoef]"]] <- "."
		  	 row[["up[gammaCoef]"]] <-  "."
		}
		
	}
	
	list(row)
	
}

.crosstabsCreateOrdinalTau <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {
	
	row <- list()
	for (layer in names(group)) {
		
		level <- group[[layer]]
		
		if (level == "") {
			
			row[[layer]] <- "Total"
			
		} else {
			
			row[[layer]] <- level
		}
	}
	
	
	if (options$ordinal$kendallsTauB) {
		
		#row[["type[kTauB]"]] <- "Kendall's Tau B"
		
		
		if (perform == "run" && status$error == FALSE) {
			
			chi.result <- try({
			
				count.dat <- stats::ftable(counts.matrix)
				count.dat <- as.data.frame(count.dat)
				Var1<-rep(count.dat[,1],times=count.dat$Freq)
				Var2<-rep(count.dat[,2],times=count.dat$Freq)
				chi.result <- stats::cor.test(as.numeric(Var1), as.numeric(Var2), method="kendall")
				
			})
			
			if (class(chi.result) == "try-error") {
				
				row[["value[kTauB]"]] <- .clean(NaN)
				
				error <- .extractErrorMessage(chi.result)
				
				sup	<- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[kTauB]"=list(sup))
				
			} else {
				
				row[["value[kTauB]"]] <- unname(chi.result$estimate)
				row[["p[kTauB]"]] <- chi.result$p.value
				row[["statistic[kTauB]"]] <- unname(chi.result$statistic)
				
			}
			
		} else {
			
			 row[["value[kTauB]"]] <- "."
			 row[["p[kTauB]"]] <- "."
		  	 row[["statistic[kTauB]"]] <- "."
		  	 
		}
		
	}
	
	list(row)
	
}

.crosstabsCreateoddratioRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) { 

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			#row[[".isNewGroup"]] <- TRUE
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	if (options$oddsRatio ) {
	
		row[["type[oddsRatio]"]] <- "Odds ratio"

		if (perform == "run" && status$error == FALSE) {
		
			if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {

				row[["value[oddsRatio]"]] <- .clean(NaN)
				row[["low[oddsRatio]"]] <- ""
				row[["up[oddsRatio]"]] <-  ""
				
				sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
				row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
				
			} else {
			
				chi.result <- try({

					chi.result <- vcd::oddsratio(counts.matrix)
					LogOR <- chi.result
					CI <- stats::confint(chi.result, level = options$oddsRatioConfidenceIntervalInterval)
					CI.low <- CI[1]
					CI.high <- CI[2]
				})

				if (class(chi.result) == "try-error") {

					row[["value[oddsRatio]"]] <- .clean(NaN)

					error <- .extractErrorMessage(chi.result)

					if (error == "at least one entry of 'x' must be positive")
						error <- "\u03A7\u00B2 could not be calculated, contains no observations"

					sup   <- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))

				} else if (is.na(chi.result)) {

					row[["value[oddsRatio]"]] <- .clean(NaN)

					sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
					row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))

				} else {

					row[["value[oddsRatio]"]] <-LogOR
					row[["low[oddsRatio]"]] <- CI.low
					row[["up[oddsRatio]"]] <- CI.high
				}
	
			}
		}
		 
	} else {
	
		row[["value[oddsRatio]"]] <- "."
	}
	
	if (options$oddsRatio ) {
	
		row[["type[FisherTest]"]] <- "Fisher's exact test "

		if (perform == "run" && status$error == FALSE) {
		
			if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {

				row[["value[FisherTest]"]] <- .clean(NaN)
				row[["low[FisherTest]"]] <- ""
				row[["up[FisherTest]"]] <-  ""
				
				sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
				row[[".footnotes"]] <- list("value[FisherTest]"=list(sup))
				
			} else {
			
				chi.result <- try({

					chi.result <- stats::fisher.test(counts.matrix, conf.level = options$oddsRatioConfidenceIntervalInterval)
					OR <- unname(chi.result$estimate)
					logOR <- log(OR)
					
					CI.low <- chi.result$conf.int[1]
					CI.high <- chi.result$conf.int[2]
					
				})

				if (class(chi.result) == "try-error") {

					row[["value[FisherTest]"]] <- .clean(NaN)

					error <- .extractErrorMessage(chi.result)

					if (error == "at least one entry of 'x' must be positive")
						error <- "\u03A7\u00B2 could not be calculated, contains no observations"

					sup   <- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[FisherTest]"=list(sup))

				} else if (is.na(chi.result)) {

					row[["value[FisherTest]"]] <- .clean(NaN)

					sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
					row[[".footnotes"]] <- list("value[FisherTest]"=list(sup))

				} else {

					row[["value[FisherTest]"]] <- logOR 
					row[["low[FisherTest]"]] <- log(CI.low)
					row[["up[FisherTest]"]] <-  log(CI.high)
				}
	
			}
		}
		 
	} else {
	
		row[["value[FisherTest]"]] <- "."
	}
	
	
	list(row)
}




.crosstabsCreateCountsRows <- function(var.name, counts.matrix, options, perform, group, status) {

	rows <- list()
	row.count <- list()
	row.expected <- list()
	row.rowproportions <- list()
	row.colproportions <- list()
	row.proportions <- list()
	row.count[["type[counts]"]] <- "Count"
	#row.count[["type[proportions]"]] <- "Total Proportions"
	
	
	if (perform == "run" && status$error == FALSE) {
	
		expected.matrix <- try({

			stats::chisq.test(counts.matrix, correct=FALSE)$expected
		})

		if (class(expected.matrix) == "try-error") {

			expected.matrix <- counts.matrix
			expected.matrix[,] <- "&nbsp;"
		}
		
	} else {
	
		expected.matrix <- counts.matrix
	}

######percentages

	if (perform == "run" && status$error == FALSE) {

	rowproportions.matrix <- try({

		base::prop.table(counts.matrix, 1)
	})

	if (class(rowproportions.matrix) == "try-error") {

		rowproportions.matrix <- counts.matrix
		rowproportions.matrix[,] <- "&nbsp;"
	}
		
	} else {
	
		rowproportions.matrix <- counts.matrix
	}
	
	if (perform == "run" && status$error == FALSE) {

	colproportions.matrix <- try({

		base::prop.table(counts.matrix, 2)
	})

	if (class(colproportions.matrix) == "try-error") {

		colproportions.matrix <- counts.matrix
		colproportions.matrix[,] <- "&nbsp;"
	}
		
	} else {
	
		colproportions.matrix <- counts.matrix
	}
	

	if (perform == "run" && status$error == FALSE) {

	proportions.matrix <- try({

		base::prop.table(counts.matrix, margin = NULL)
	})

	if (class(proportions.matrix) == "try-error") {

		proportions.matrix <- counts.matrix
		proportions.matrix[,] <- "&nbsp;"
	}
		
	} else {
	
		proportions.matrix <- counts.matrix
	}
	

	for (i in 1:dim(counts.matrix)[[1]]) {
	
		if (perform == "run" && status$error == FALSE) {

			row <- as.list(counts.matrix[i,])
			names(row) <- base::paste(names(row),"[counts]",	sep="")
			row[["total[counts]"]] <- base::sum(counts.matrix[i,])
			row <- c(row.count, row)
			
			if (options$countsExpected) {
			
				row.expected[["type[expected]"]] <- "Expected Count"

				expected <- as.list(expected.matrix[i,])
				names(expected) <- paste(names(expected),"[expected]",  sep="")
				
				if (class(expected.matrix[1,1]) == "character") {
					expected[["total[expected]"]] <- ""
				} else {
					expected[["total[expected]"]] <- base::sum(expected.matrix[i,])
				}
			
				expected <- c(row.expected, expected)
				row <- c(row, expected)
			}
			
			if (options$percentages$row) {
			
				row.rowproportions[["type[rowproportions]"]] <- " % within row"

				rowproportions <- as.list(rowproportions.matrix[i,])
				names(rowproportions) <- paste(names(rowproportions),"[rowproportions]",  sep="")
				
				if (class(rowproportions.matrix[1,1]) == "character") {
					rowproportions[["total[rowproportions]"]] <- ""
				} else {
					rowproportions[["total[rowproportions]"]] <- base::sum(rowproportions.matrix[i,])
				}
			
				rowproportions <- c(row.rowproportions, rowproportions)
				row <- c(row, rowproportions)
			}
			
			if (options$percentages$column) {
			
				row.colproportions[["type[colproportions]"]] <- " % within column"

				colproportions <- as.list(colproportions.matrix[i,])
				names(colproportions) <- paste(names(colproportions),"[colproportions]",  sep="")
				
				if (class(colproportions.matrix[1,1]) == "character") {
					colproportions[["total[colproportions]"]] <- ""
				} else {
					
					row.sum <- base::margin.table(counts.matrix, 1)
					row.prop <- as.list( base::prop.table(row.sum)) 
					colproportions[["total[colproportions]"]] <- row.prop[[i]]
				}
			
				colproportions <- c(row.colproportions, colproportions)
				row <- c(row, colproportions)
			}
			
			if (options$percentages$total) {
			
				row.proportions[["type[proportions]"]] <- " % of Total"

				proportions <- as.list(proportions.matrix[i,])
				names(proportions) <- paste(names(proportions),"[proportions]",  sep="")
				
				if (class(proportions.matrix[1,1]) == "character") {
					proportions[["total[proportions]"]] <- ""
				} else {
					proportions[["total[proportions]"]] <- base::sum(proportions.matrix[i,])
				}
			
				proportions <- c(row.proportions, proportions)
				row <- c(row, proportions)
			}
		
		} else {
		
			row <- list()
		}
		
		row[[var.name]] <- dimnames(counts.matrix)[[1]][i]
		
		for (layer in names(group)) {
		
			level <- group[[layer]]
			
			if (level == "") {

				row[[layer]] <- "Total"
							
			} else {
			
				row[[layer]] <- level
			}
		}

		if (i == 1 && options$countsExpected == FALSE && options$percentages$row == FALSE && options$percentages$col == FALSE && options$percentages$total == FALSE) {

			row[[".isNewGroup"]] <- TRUE
		}
		
		rows[[length(rows)+1]] <- row
	}
	
	
	if (perform == "run" && status$error == FALSE) {

		row <- apply(counts.matrix, 2, base::sum)
		row <- as.list(row)
		names(row) <- base::paste(names(row),"[counts]",	sep="")
		row[["total[counts]"]] <- base::sum(counts.matrix)
		row <- c(row.count, row)
		
		if (options$countsExpected) {
		
			if (class(expected.matrix[1,1]) == "character") {
				expected <- expected.matrix[1,]
			} else {
				expected <- apply(expected.matrix, 2, base::sum)
			}

			expected <- as.list(expected)
			names(expected) <- paste(names(expected),"[expected]", sep="")

			if (class(expected.matrix[1,1]) == "character") {
				expected[["total[expected]"]] <- ""
			} else {
				expected[["total[expected]"]] <- base::sum(expected.matrix)
			}
			
			expected<-c(row.expected, expected)
			
			row <- c(row,  expected)
		}
		
		if (options$percentages$row) {
		
			if (class(rowproportions.matrix[1,1]) == "character") {
				rowproportions <- rowproportions.matrix[1,]
			} else {
				m <- base::margin.table(counts.matrix, 2)
				rowproportion <- base::prop.table(m)
			}

			rowproportions <- as.list(rowproportion)
			names(rowproportions) <- paste(names(rowproportions),"[rowproportions]", sep="")

			if (class(rowproportions.matrix[1,1]) == "character") {
				rowproportions[["total[rowproportions]"]] <- ""
			} else {
				rowproportions[["total[rowproportions]"]] <- base::sum(rowproportion)
			}
			
			rowproportions<-c(row.rowproportions, rowproportions)
			
			row <- c(row,  rowproportions)
		}
		
		if (options$percentages$column) {
		
			if (class(colproportions.matrix[1,1]) == "character") {
				colproportions <- colproportions.matrix[1,]
			} else {
				colproportion <- apply(colproportions.matrix, 2, base::sum)
			}

			colproportions <- as.list(colproportion)
			names(colproportions) <- paste(names(colproportions),"[colproportions]", sep="")

			if (class(rowproportions.matrix[1,1]) == "character") {
				colproportions[["total[colproportions]"]] <- ""
			} else {
				row.sum <- base::margin.table(counts.matrix, 1)
				row.prop <- base::prop.table(row.sum) 
				colproportions[["total[colproportions]"]] <- base::sum(row.prop)
			}
			
			colproportions<-c(row.colproportions, colproportions)
			
			row <- c(row,  colproportions)
		}
		
		
		if (options$percentages$total) {
		
			if (class(proportions.matrix[1,1]) == "character") {
				proportions <- proportions.matrix[1,]
			} else {
				proportions <- apply(proportions.matrix, 2, base::sum)
			}

			proportions <- as.list(proportions)
			names(proportions) <- paste(names(proportions),"[proportions]", sep="")

			if (class(proportions.matrix[1,1]) == "character") {
				proportions[["total[proportions]"]] <- ""
			} else {
				proportions[["total[proportions]"]] <- base::sum(proportions.matrix)
			}
			
			proportions<-c(row.proportions, proportions)
			
			row <- c(row,  proportions)
		}
		
	} else {
	
		row <- list()
	}
	
	row[[var.name]] <- "Total"
	if (options$countsExpected == FALSE && options$percentages$row == FALSE && options$percentages$col == FALSE && options$percentages$total == FALSE)
		row[[".isNewGroup"]] <- TRUE
	
	for (layer in names(group)) {
	
		level <- group[[layer]]

		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}

	rows[[length(rows)+1]] <- row

	rows
}


.crosstabsCreateGroupMatrices <- function(dataset, rows, columns, groups, counts=NULL) {

	# this creates count matrices for each of the groups

	matrices <- list()

	if (is.null(groups)) {

		ss.dataset <- base::subset(dataset, select=c(rows, columns, counts))

		if (is.null(counts)) {

			ss.table  <- base::table(ss.dataset)
			ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))
			
		} else {
		
			counts <- stats::na.omit(counts)
		
			ss.matrix <- base::tapply(ss.dataset[[counts]], list(ss.dataset[[rows]], ss.dataset[[columns]]), base::sum)
			ss.matrix[is.na(ss.matrix)] <- 0
		}
		
		matrices[[1]] <- ss.matrix
	
	} else {
	
		for (group in groups) {
		
			group <- group[group != ""]

			if (length(group) == 0) {
			
				ss.dataset <- base::subset(dataset, select=c(rows, columns, counts))
			
			} else {

				ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", sep="", collapse="&")
				ss.expression <- base::parse(text=ss.filter.string)
				ss.dataset	  <- base::subset(dataset, select=c(rows, columns, counts), subset=eval(ss.expression))
			}
			
			if (is.null(counts)) {

				ss.table  <- base::table(ss.dataset)
				ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))
			
			} else {
		
				ss.matrix <- base::tapply(ss.dataset[[counts]], list(ss.dataset[[rows]], ss.dataset[[columns]]), base::sum)
			}

			matrices[[length(matrices)+1]] <- ss.matrix
		}
	}
	
	matrices
}



Crosstabs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

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
	meta[[2]] <- list(name="crosstabs", type="tables")
	
	results[[".meta"]] <- meta
	
	results[["title"]] <- "Crosstabs"
	
	
	### CROSS TABS
	
	crosstabs <- list()

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
			tables <- .crosstab(dataset, options, perform, analysis)
			for (table in tables)
				crosstabs[[length(crosstabs)+1]] <- table				
		}
	
	} else {
	
		crosstabs[[1]] <- list(title = "Crosstabs", cases = list(), schema = list(fields=list()))
	}

	results[["crosstabs"]] <- crosstabs

	results
}

