#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

ContingencyTables <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

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

	### TITLE

	results[["title"]] <- "Contingency Tables"

	meta <- list()

	### CROSS TABS

	crosstabs <- list()

	rows    <- as.vector(options$rows,    "character")
	columns <- as.vector(options$columns, "character")

	if (length(rows) == 0)
		rows <- ""

	if (length(columns) == 0)
		columns <- ""

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

		if (!is.null(tables[["counts.table"]])) {

			results[["Counts Table"]] <- tables[["counts.table"]]
			meta[[length(meta)+1]] <- list(name="Counts Table", type="table")

		}
		if (!is.null(tables[["tests.table"]])) {

			results[["Tests Table"]] <- tables[["tests.table"]]
			meta[[length(meta)+1]] <- list(name="Tests Table", type="table")

		}
		if (!is.null(tables[["odds.ratio.table"]])) {

			results[["Odds Ratio Table"]] <- tables[["odds.ratio.table"]]
			meta[[length(meta)+1]] <- list(name="Odds Ratio Table", type="table")

		}
		if (!is.null(tables[["nominal.table"]])) {

			results[["Nominal Table"]] <- tables[["nominal.table"]]
			meta[[length(meta)+1]] <- list(name="Nominal Table", type="table")

		}
		if (!is.null(tables[["ordinal.table"]])) {

			results[["Ordinal Table"]] <- tables[["ordinal.table"]]
			meta[[length(meta)+1]] <- list(name="Ordinal Table", type="table")

		}
		if (!is.null(tables[["kendalls.table"]])) {

			results[["Kendalls Table"]] <- tables[["kendalls.table"]]
			meta[[length(meta)+1]] <- list(name="Kendalls Table", type="table")

		}
	}

	results[[".meta"]] <- meta



	if (perform == "run" || length(options$rows) == 0 || length(options$columns) == 0) {

		list(results=results, status="complete")

	} else {

		list(results=results, status="inited")
	}
}


.crosstab <- function(dataset, options, perform, analysis) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)

	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL

	status <- list(error=FALSE, ready=TRUE)

	if (length(options$rows) == 0 || length(options$columns) == 0) {

		status$ready <- FALSE

	} else {

		all.vars <- c(unlist(analysis), counts.var)
		dataset <- subset(dataset, select=.v(all.vars))
	}

	# the following creates a 'groups' list
	# a 'group' represents a combinations of the levels from the layers
	# if no layers are specified, groups is null

	if (length(analysis) >= 3) {  # if layers are specified

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

	counts.table[["title"]] <- "Contingency Tables"

	counts.fields <- fields

	counts.fields[[length(counts.fields)+1]] <- list(name=analysis$rows, type="string", combine=TRUE)

	lvls <- c()

	if (analysis$columns == "") {

		lvls <- c(".", ". ")

	} else if (is.factor(dataset[[ .v(analysis$columns) ]] )) {

		lvls <- base::levels(dataset[[ .v(analysis$columns) ]])
		if (options$columnOrder == "descending") {
			lvls <- base::rev(lvls)
		} else {
			lvls <- lvls
		}

	} else if (perform == "run") {

		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])

		if (options$columnOrder == "descending") {
			lvls <- base::rev(lvls, decreasing = TRUE)
		} else {
			lvls <- lvls
		}
	}

	counts.fp <- FALSE  # whether the counts are float point or not; changes formatting

	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		if (identical(counts, as.integer(counts)) == FALSE)  # are the counts floating point?
			counts.fp <- TRUE
	}

	if (options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal ) {

		counts.fields[[length(counts.fields)+1]] <- list(name="type[counts]", title="", type="string")

		if (options$countsExpected) {
			counts.fields[[length(counts.fields)+1]] <- list(name="type[expected]", title="", type="string")
		}

		if (options$percentagesRow) {

			counts.fields[[length(counts.fields)+1]] <- list(name="type[row.proportions]", title="", type="string")
		}

		if (options$percentagesColumn) {

			counts.fields[[length(counts.fields)+1]] <- list(name="type[col.proportions]", title="", type="string")
		}

		if (options$percentagesTotal) {

			counts.fields[[length(counts.fields)+1]] <- list(name="type[proportions]", title="", type="string")
		}

	}

	overTitle <- unlist(analysis$columns)
	if (overTitle == "")
		overTitle <- "."

	for (column.name in lvls) {

		private.name <- base::paste(column.name,"[counts]", sep="")

		if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal ) {

			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, overTitle=overTitle, type="number", format="sf:4;dp:2")

		} else {

			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, overTitle=overTitle, type="integer")
		}

		if (options$countsExpected) {

			private.name <- base::paste(column.name,"[expected]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		}

		if (options$percentagesRow) {

			private.name <- base::paste(column.name,"[row.proportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}

		if (options$percentagesColumn) {

			private.name <- base::paste(column.name,"[col.proportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}

		if (options$percentagesTotal) {

			private.name <- base::paste(column.name,"[proportions]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="dp:1;pc")
		}
	}

	# Totals columns

	if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal) {

		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]", title="Total", type="number", format="sf:4;dp:2")

	} else {

		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]", title="Total", type="integer")
	}

	if (options$countsExpected) {

		counts.fields[[length(counts.fields)+1]] <- list(name="total[expected]", title="Total", type="number", format="sf:4;dp:2")
	}

	if (options$percentagesRow) {

		counts.fields[[length(counts.fields)+1]] <- list(name="total[row.proportions]", title="Total", type="number", format="dp:1;pc")
	}

	if (options$percentagesColumn) {

		counts.fields[[length(counts.fields)+1]] <- list(name="total[col.proportions]", title="Total", type="number", format="dp:1;pc")
	}

	if (options$percentagesTotal) {

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
			if (options$VovkSellkeMPR) {
				tests.fields[[length(tests.fields)+1]] <- list(name = "MPR[chiSquared]",
							                                        title = "VS-MPR",
							                                        type = "number",
							                                        format = "sf:4;dp:3")
			}
		}

		if (options$chiSquaredContinuityCorrection){
			tests.fields[[length(tests.fields)+1]] <- list(name="type[chiSquared-cc]", title="", type="string")
			tests.fields[[length(tests.fields)+1]] <- list(name="value[chiSquared-cc]", title="Value", type="number", format="sf:4;dp:3")
			tests.fields[[length(tests.fields)+1]] <- list(name="df[chiSquared-cc]", title="df", type="integer")
			tests.fields[[length(tests.fields)+1]] <- list(name="p[chiSquared-cc]", title="p", type="number", format="dp:3;p:.001")
			if (options$VovkSellkeMPR) {
				tests.fields[[length(tests.fields)+1]] <- list(name = "MPR[chiSquared-cc]",
							                                        title = "VS-MPR",
							                                        type = "number",
							                                        format = "sf:4;dp:3")
			}
		}

		if (options$likelihoodRatio) {
			tests.fields[[length(tests.fields)+1]] <- list(name="type[likelihood]", title="", type="string")
			tests.fields[[length(tests.fields)+1]] <- list(name="value[likelihood]", title="Value", type="number", format="sf:4;dp:3")
			tests.fields[[length(tests.fields)+1]] <- list(name="df[likelihood]", title="df", type="integer")
			tests.fields[[length(tests.fields)+1]] <- list(name="p[likelihood]", title="p", type="number", format="dp:3;p:.001")
			if (options$VovkSellkeMPR) {
				tests.fields[[length(tests.fields)+1]] <- list(name = "MPR[likelihood]",
							                                        title = "VS-MPR",
							                                        type = "number",
							                                        format = "sf:4;dp:3")
			}
		}

		tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")

		if (counts.fp) {

			tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="number", format="sf:4;dp:2")

		} else {

			tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")
		}

		tests.fields[[length(tests.fields)+1]] <- list(name="df[N]", title="df")
		tests.fields[[length(tests.fields)+1]] <- list(name="p[N]", title="p")
		if (options$VovkSellkeMPR) {
			tests.fields[[length(tests.fields)+1]] <- list(name = "MPR[N]",
																										title = "VS-MPR",
																										type = "number",
																										format = "sf:4;dp:3")
		}
		schema <- list(fields=tests.fields)

		tests.table[["schema"]] <- schema

	}

	############# Odds ratio

	if (options$oddsRatio) {

		odds.ratio.table <- list()

		odds.ratio.table[["title"]] <- "Log Odds Ratio"
		ci.label <- paste(100*options$oddsRatioConfidenceIntervalInterval, "% Confidence Intervals", sep="")

		odds.ratio.fields <- fields

		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="type[oddsRatio]", title="", type="string")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="value[oddsRatio]", title="Log Odds Ratio", type="number", format="sf:4;dp:3")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="low[oddsRatio]", title="Lower",overTitle=ci.label, type="number", format="dp:3")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="up[oddsRatio]",  title="Upper",overTitle=ci.label, type="number", format="dp:3")

		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="type[FisherTest]", title="", type="string")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="value[FisherTest]", title="Log Odds Ratio", type="number", format="sf:4;dp:3")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="low[FisherTest]", title="Lower", overTitle=ci.label, type="number", format="dp:3")
		odds.ratio.fields[[length(odds.ratio.fields)+1]] <- list(name="up[FisherTest]",  title="Upper", overTitle=ci.label, type="number", format="dp:3")

		schema <- list(fields=odds.ratio.fields)

		odds.ratio.table[["schema"]] <- schema
	}


	##### Nominal Table (Symmetric Measures)

	if (options$contingencyCoefficient|| options$phiAndCramersV || options$lambda) {

		nominal.table <- list()

		nominal.table[["title"]] <- "Nominal"

		nominal.fields <- fields

		if (options$contingencyCoefficient){

			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[ContCoef]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[ContCoef]", title="Value", type="number", format="sf:4;dp:3")
		}

		if (options$phiAndCramersV) {
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[PhiCoef]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[PhiCoef]", title="Value", type="number", format="sf:4;dp:3")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[CramerV]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[CramerV]", title="Value", type="number", format="sf:4;dp:3")
		}

		if (options$lambda) {
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[LambdaR]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[LambdaR]", title="Value", type="number", format="sf:4;dp:3")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="type[LambdaC]", title="", type="string")
			nominal.fields[[length(nominal.fields)+1]] <- list(name="value[LambdaC]", title="Value", type="number", format="sf:4;dp:3")
		}

		schema <- list(fields=nominal.fields)

		nominal.table[["schema"]] <- schema
	}

	##### Ordinal Table

	if (options$gamma) {

		ordinal.table <- list()

		ordinal.table[["title"]] <- "Ordinal Gamma"
		ci.label <- paste( "95% Confidence Intervals")

		ordinal.fields <- fields

		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="value[gammaCoef]", title="Gamma", type="number", format="sf:4;dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="Sigma[gammaCoef]", title="Standard Error", type="number", format="dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="low[gammaCoef]", title="Lower",overTitle=ci.label, type="number", format="dp:3")
		ordinal.fields[[length(ordinal.fields)+1]] <- list(name="up[gammaCoef]",  title="Upper",overTitle=ci.label, type="number", format="dp:3")
		#ordinal.fields[[length(ordinal.fields)+1]] <- list(name="low[gammaCoef]", title="Lower CI", type="number", format="dp:3")
		#ordinal.fields[[length(ordinal.fields)+1]] <- list(name="up[gammaCoef]",  title="Upper CI", type="number", format="dp:3")

		schema <- list(fields=ordinal.fields)

		ordinal.table[["schema"]] <- schema
	}

	########## Kendall Tau-B table

	if (options$kendallsTauB) {

		kendalls.table <- list()

		kendalls.table[["title"]] <- "Kendall's Tau"

		kendalls.fields <- fields

		#kendalls.fields[[length(kendalls.fields)+1]] <- list(name="type[kTauB]", title="", type="string")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="value[kTauB]", title="Kendall's Tau-b ", type="number", format="sf:4;dp:3")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="statistic[kTauB]", title="Z", type="number", format="dp:3")
		kendalls.fields[[length(kendalls.fields)+1]] <- list(name="p[kTauB]", title="p", type="number", format="dp:3;p:.001")
		if (options$VovkSellkeMPR){
			kendalls.fields[[length(kendalls.fields)+1]] <- list(name = "MPR[kTauB]",
									                                        title = "VS-MPR",
									                                        type = "number",
									                                        format = "sf:4;dp:3")
		}


		schema <- list(fields=kendalls.fields)

		kendalls.table[["schema"]] <- schema
	}

	if (is.null(counts.var) == FALSE) {

		counts <- stats::na.omit(dataset[[ .v(counts.var) ]])

		if (any(counts < 0)|| any(is.infinite(counts))) {

			status$error <- TRUE
			status$errorMessage <- "Counts may not contain negative numbers or infinities"
		}
	}


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, analysis$rows, analysis$columns, groups, counts.var, options$rowOrder=="descending", options$columnOrder=="descending", perform=perform, status=status)

	counts.rows <- list()
	tests.rows <- list()
	odds.ratio.rows <- list()
	nominal.rows <- list()
	ordinal.rows <- list()
	kendalls.rows <- list()

	tests.footnotes <- .newFootnotes()
	odds.ratio.footnotes <- .newFootnotes()
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

		next.rows <- .crosstabsCreateOddsRatioRows(analysis$rows, group.matrix, odds.ratio.footnotes, options, perform, group, status)
		odds.ratio.rows <- c(odds.ratio.rows, next.rows)

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

	tables[["counts.table"]] <- counts.table

	if (options$chiSquared || options$chiSquaredContinuityCorrection || options$likelihoodRatio ) {

		tests.table[["data"]] <- tests.rows
		tests.table[["footnotes"]] <- as.list(tests.footnotes)

		if (status$error)
			tests.table[["error"]] <- list(errorType="badData")

		tables[["tests.table"]] <- tests.table
	}

	if (options$oddsRatio) {
		odds.ratio.table[["data"]] <- odds.ratio.rows
		odds.ratio.table[["footnotes"]] <- as.list(odds.ratio.footnotes)

		if (status$error)
			odds.ratio.table[["error"]] <- list(errorType="badData")

		tables[["odds.ratio.table"]] <- odds.ratio.table
	}

	if (options$contingencyCoefficient || options$phiAndCramersV || options$lambda) {

		nominal.table[["data"]] <- nominal.rows
		nominal.table[["footnotes"]] <- as.list(nominal.footnotes)

		if (status$error)
			nominal.table[["error"]] <- list(errorType="badData")

		tables[["nominal.table"]] <- nominal.table
	}

	if (options$gamma) {

		ordinal.table[["data"]] <- ordinal.rows
		ordinal.table[["footnotes"]] <- as.list(ordinal.footnotes)

		if (status$error)
			ordinal.table[["error"]] <- list(errorType="badData")

		tables[["ordinal.table"]] <- ordinal.table
	}

	if (options$kendallsTauB) {

		kendalls.table[["data"]] <- kendalls.rows
		kendalls.table[["footnotes"]] <- as.list(kendalls.footnotes)

		if (status$error)
			kendalls.table[["error"]] <- list(errorType="badData")

		tables[["kendalls.table"]] <- kendalls.table
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
	row[["MPR[N]"]] <- ""

	if (perform == "run" && status$error == FALSE && status$ready) {

		row[["value[N]"]] <- base::sum(counts.matrix)

	} else {

		row[["value[N]"]] <- "."
	}

	if (options$chiSquared) {

		row[["type[chiSquared]"]] <- "\u03A7\u00B2"

		if (perform == "run" && status$error == FALSE && status$ready) {

			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix, correct=FALSE)
			})

			if (class(chi.result) == "try-error") {

				row[["value[chiSquared]"]] <- .clean(NaN)
				row[["df[chiSquared]"]]<- " "
				row[["p[chiSquared]"]]<- " "
				row[["MPR[chiSquared]"]]<- " "

				error <- .extractErrorMessage(chi.result)

				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"

				sup	<- .addFootnote(footnotes, error)
				row.footnotes[["value[chiSquared]"]]=list(sup)

			} else if (is.na(chi.result$statistic)) {

				row[["value[chiSquared]"]] <- .clean(NaN)
				row[["df[chiSquared]"]]<- " "
				row[["p[chiSquared]"]]<- " "
				row[["MPR[chiSquared]"]]<- " "

				message <- "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros"

				#sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
				#row.footnotes[["value[oddsRatio]"]]=list(sup)

				#warn <- warnings()
				#if (length(warn) > 0)
				#	message <- paste(message, names(warn)[1], sep=" : ")

				sup <- .addFootnote(footnotes, message)
				row.footnotes [["value[chiSquared]"]]=list(sup)

			} else {

				row[["value[chiSquared]"]] <- unname(chi.result$statistic)
				row[["df[chiSquared]"]] <- unname(chi.result$parameter)
				row[["p[chiSquared]"]] <- unname(chi.result$p.value)
				row[["MPR[chiSquared]"]]<- .VovkSellkeMPR(row[["p[chiSquared]"]])
			}

		} else {

			row[["value[chiSquared]"]] <- "."

		}
	}

	if (options$chiSquaredContinuityCorrection) {

		row[["type[chiSquared-cc]"]] <- "\u03A7\u00B2 continuity correction"

		if (perform == "run" && status$error == FALSE && status$ready) {

			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix)
				#row <- list(Method="Pearson's Chi-squared", X2=unname(chi$statistic), df=unname(chi$parameter), p=chi$p.value)
			})

			if (class(chi.result) == "try-error") {

				row[["value[chiSquared-cc]"]] <- .clean(NaN)
				row[["df[chiSquared-cc]"]]<- " "
				row[["p[chiSquared-cc]"]]<- " "
				row[["MPR[chiSquared-cc]"]]<- " "

				error <- .extractErrorMessage(chi.result)

				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"

				sup	<- .addFootnote(footnotes, error)
				row.footnotes [["value[chiSquared-cc]"]]=list(sup)

			} else if (is.na(chi.result$statistic)) {

				row[["value[chiSquared-cc]"]] <- .clean(NaN)
				row[["df[chiSquared-cc]"]]<- " "
				row[["p[chiSquared-cc]"]]<- " "
				row[["MPR[chiSquared-cc]"]]<- " "

				sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros")
				row.footnotes [["value[chiSquared-cc]"]]=list(sup)

			} else {

				row[["value[chiSquared-cc]"]] <- unname(chi.result$statistic)
				row[["df[chiSquared-cc]"]] <- unname(chi.result$parameter)
				row[["p[chiSquared-cc]"]] <- unname(chi.result$p.value)
				row[["MPR[chiSquared-cc]"]]<- .VovkSellkeMPR(row[["p[chiSquared-cc]"]])

			}

		} else {

			row[["value[chiSquared-cc]"]] <- "."
		}
	}

	if (options$likelihoodRatio) {

		row[["type[likelihood]"]] <- "Likelihood ratio"

		if (perform == "run" && status$error == FALSE && status$ready) {

			chi.result <- try({

				chi.result <- vcd::assocstats(counts.matrix)

			})

			if (class(chi.result) == "try-error") {

				row[["value[likelihood]"]] <- .clean(NaN)
				row[["df[likelihood]"]] <- ""
				row[["p[likelihood]"]] <-""
				row[["MPR[likelihood]"]] <-""

				error <- .extractErrorMessage(chi.result)

				sup	<- .addFootnote(footnotes, error)
				row.footnotes[["value[likelihood]"]] = list(sup)

			} else {

				row[["value[likelihood]"]] <- chi.result$chisq_tests[1]
				row[["df[likelihood]"]] <- chi.result$chisq_tests[3]
				row[["p[likelihood]"]] <- chi.result$chisq_tests[5]
				row[["MPR[likelihood]"]] <- .VovkSellkeMPR(row[["p[likelihood]"]])
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

	row.footnotes <- list ()

	if (options$contingencyCoefficient) {

		row[["type[ContCoef]"]] <- "Contingency coefficient"

		if (perform == "run" && status$error == FALSE) {

			 chi.result <- try({

				 chi.result <- vcd::assocstats(counts.matrix)

			})

			if (class(chi.result) == "try-error") {

				row[["value[ContCoef]"]] <- .clean(NaN)

				error <- .extractErrorMessage(chi.result)

				sup	<- .addFootnote(footnotes, error)
				row.footnotes[["value[ContCoef]"]] <- list(sup)

			} else if (is.na(chi.result$contingency)) {

					row[["value[ContCoef]"]] <- .clean(NaN)

					sup <- .addFootnote(footnotes, "Value could not be calculated - At least one row or column contains all zeros")
					row.footnotes[["value[ContCoef]"]] <- list(sup)

			} else {

				 row[["value[ContCoef]"]] <- chi.result$contingency
			}

		} else {

			row[["value[ContCoef]"]] <- "."
		}

	}

	if (options$phiAndCramersV) {

		row[["type[PhiCoef]"]] <- "Phi-coefficient"


		if (perform == "run" && status$error == FALSE) {

			chi.result <- try({

				chi.result <- vcd::assocstats(counts.matrix)
			})

			if (class(chi.result) == "try-error") {

				row[["value[PhiCoef]"]] <- .clean(NaN)

				error <- .extractErrorMessage(chi.result)

				sup	<- .addFootnote(footnotes, error)
				row[["value[PhiCoef]"]] <- list(sup)

			} else if (is.na(chi.result$phi)) {

				row[["value[PhiCoef]"]] <- .clean(NaN)

				sup <- .addFootnote(footnotes, "Value could not be calculated - At least one row or column contains all zeros")
				row.footnotes[["value[PhiCoef]"]] <- list(sup)
				#row.footnotes <- c(row.footnotes, list("value[PhiCoef]"=list(sup)))

			} else {

				row[["value[PhiCoef]"]] <- chi.result$phi
			}

		} else {

			row[["value[PhiCoef]"]] <- "."
		}

	 }

	 if (options$phiAndCramersV) {

		row[["type[CramerV]"]] <- "Cramer's V "

		if (perform == "run" && status$error == FALSE) {

			chi.result <- try({

				chi.result <- vcd::assocstats(counts.matrix)

			})

			if (class(chi.result) == "try-error") {

				row[["value[CramerV]"]] <- .clean(NaN)

				error <- .extractErrorMessage(chi.result)

				sup	<- .addFootnote(footnotes, error)
				row.footnotes[["value[CramerV]"]] <- list(sup)

			} else if (is.na(chi.result$cramer)) {

				row[["value[CramerV]"]] <- .clean(NaN)

				sup <- .addFootnote(footnotes, "Value could not be calculated - At least one row or column contains all zeros")
				row.footnotes[["value[CramerV]"]] <- list(sup)

			} else {

				row[["value[CramerV]"]] <- chi.result$cramer
			}

		} else {

			row[["value[CramerV]"]] <- "."
		}
	}

	 if (options$lambda) {

		row[["type[LambdaR]"]] <- paste("Lambda (", options$rows, "dependent)", sep= " ")

		if (perform == "run" && status$error == FALSE) {

				N <- sum(counts.matrix)
				E1 <- N- max(rowSums(counts.matrix))
				E2 <- sum(apply(counts.matrix,2, function (x) sum(x)-max(x) ))
				lambda<-(E1-E2)/E1
				row[["value[LambdaR]"]] <- lambda

		 if (is.na(lambda)) {

				row[["value[LambdaR]"]] <- .clean(NaN)

				sup <- .addFootnote(footnotes, "Value could not be calculated - At least one row sums or column sums contains all zeros")
				row.footnotes[["value[LambdaR]"]] <- list(sup)


		}
		} else {

			row[["value[LambdaR]"]] <- "."
		}
	}


	if (options$lambda) {

		row[["type[LambdaC]"]] <- paste("Lambda (", options$columns, "dependent)", sep= " ")

		if (perform == "run" && status$error == FALSE) {

				N <- sum(counts.matrix)
				E1 <- N- max(colSums(counts.matrix))
				E2 <- sum(apply(counts.matrix,1, function (x) sum(x)-max(x) ))
				lambda<-(E1-E2)/E1
				print(N)
				print(E1)
				print(E2)
				print(lambda)
				row[["value[LambdaC]"]] <- lambda


		} else {

			row[["value[LambdaC]"]] <- "."
		}
	}



	row[[".footnotes"]] <- row.footnotes
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



	if (options$gamma) {

		row[["type[gammaCoef]"]] <- "Gamma coefficient"


		if (perform == "run" && status$error == FALSE) {

			chi.result <- try({

				chi.result <- vcdExtra::GKgamma(counts.matrix)

			})

			print(chi.result)
			print("we are here")

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

	if (options$kendallsTauB) {

		row[["type[kTauB]"]] <- "Kendall's Tau-b"


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
				if (options$VovkSellkeMPR){
					row[["MPR[kTauB]"]] <- .VovkSellkeMPR(row[["p[kTauB]"]])
				}
				row[["statistic[kTauB]"]] <- unname(chi.result$statistic)

			}

		} else {

			 row[["value[kTauB]"]] <- "."
			 row[["p[kTauB]"]] <- "."
			 if (options$VovkSellkeMPR){
				 row[["MPR[kTauB]"]] <- "."
			 }
		   row[["statistic[kTauB]"]] <- "."
		}

	}

	list(row)

}

.crosstabsCreateOddsRatioRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {

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

	row.footnotes <- list()

	if (options$oddsRatio ) {

		row[["type[oddsRatio]"]] <- "Odds ratio"

		if (perform == "run" && status$error == FALSE) {

			if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {

				row[["value[oddsRatio]"]] <- .clean(NaN)
				row[["low[oddsRatio]"]] <- ""
				row[["up[oddsRatio]"]] <-  ""

				sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
				row.footnotes[["value[oddsRatio]"]]=list(sup)

			} else {

				chi.result <- try({

					chi.result <- vcd::oddsratio(counts.matrix)
					CI <- stats::confint(chi.result, level = options$oddsRatioConfidenceIntervalInterval)
					LogOR <- unname(chi.result$coefficients)
					log.CI.low <- CI[1]
					log.CI.high <- CI[2]
				})

				if (class(chi.result) == "try-error") {

					row[["value[oddsRatio]"]] <- .clean(NaN)

					error <- .extractErrorMessage(chi.result)

					if (error == "at least one entry of 'x' must be positive")
						error <- "\u03A7\u00B2 could not be calculated, contains no observations"

					sup   <- .addFootnote(footnotes, error)
					row.footnotes[["value[oddsRatio]"]]=list(sup)

				} else if (is.na(chi.result)) {

					row[["value[oddsRatio]"]] <- .clean(NaN)

					sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
					row.footnotes[["value[oddsRatio]"]]=list(sup)

				} else {

					row[["value[oddsRatio]"]] <-LogOR
					row[["low[oddsRatio]"]] <- log.CI.low
					row[["up[oddsRatio]"]] <- log.CI.high
				}

				row[["value[oddsRatio]"]] <- .clean(LogOR)
				row[["low[oddsRatio]"]] <- .clean(log.CI.low)
				row[["up[oddsRatio]"]] <-  .clean(log.CI.high)
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
				row.footnotes[["value[FisherTest]"]]=list(sup)

			} else {

				chi.result <- try({

					chi.result <- stats::fisher.test(counts.matrix, conf.level = options$oddsRatioConfidenceIntervalInterval)
					OR <- unname(chi.result$estimate)
					logOR <- log(OR)
					log.CI.low <- log(chi.result$conf.int[1])
					log.CI.high <- log(chi.result$conf.int[2])

				})

				if (class(chi.result) == "try-error") {

					row[["value[FisherTest]"]] <- .clean(NaN)

					error <- .extractErrorMessage(chi.result)

					if (error == "at least one entry of 'x' must be positive")
						error <- "\u03A7\u00B2 could not be calculated, contains no observations"

					sup   <- .addFootnote(footnotes, error)
					row.footnotes[["value[FisherTest]"]]=list(sup)

				} else if (is.na(chi.result)) {

					row[["value[FisherTest]"]] <- .clean(NaN)

					sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
					row.footnotes[["value[FisherTest]"]]=list(sup)

				} else {

					row[["value[FisherTest]"]] <- logOR
					row[["low[FisherTest]"]] <- log.CI.low
					row[["up[FisherTest]"]] <-  log.CI.high
				}

				row[["value[FisherTest]"]] <- .clean(logOR)
				row[["low[FisherTest]"]] <- .clean(log.CI.low)
				row[["up[FisherTest]"]] <-  .clean(log.CI.high)
			}
		}

	} else {

		row[["value[FisherTest]"]] <- "."
	}

	row[[".footnotes"]] <- row.footnotes


	list(row)
}




.crosstabsCreateCountsRows <- function(var.name, counts.matrix, options, perform, group, status) {

	rows <- list()
	row.count <- list()
	row.expected <- list()
	row.row.proportions <- list()
	row.col.proportions <- list()
	row.proportions <- list()
	row.count[["type[counts]"]] <- "Count"
	row.count[["type[proportions]"]] <- "% of Total"


	if (perform == "run" && status$error == FALSE && status$ready) {

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

	if (perform == "run" && status$error == FALSE && status$ready) {

		row.proportions.matrix <- try({

			base::prop.table(counts.matrix, 1)
		})

		if (class(row.proportions.matrix) == "try-error") {

			row.proportions.matrix <- counts.matrix
			row.proportions.matrix[,] <- "&nbsp;"
		}

	} else {

		row.proportions.matrix <- counts.matrix
	}

	if (perform == "run" && status$error == FALSE && status$ready) {

		col.proportions.matrix <- try({

			base::prop.table(counts.matrix, 2)
		})

		if (class(col.proportions.matrix) == "try-error") {

			col.proportions.matrix <- counts.matrix
			col.proportions.matrix[,] <- "&nbsp;"
		}

	} else {

		col.proportions.matrix <- counts.matrix
	}


	if (perform == "run" && status$error == FALSE && status$ready) {

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

		if (perform == "run" && status$error == FALSE && status$ready) {

			row <- as.list(counts.matrix[i,])
			names(row) <- base::paste(names(row),"[counts]",	sep="")
			row[["total[counts]"]] <- base::sum(counts.matrix[i,])
			row <- c(row.count, row)

			if (options$countsExpected) {

				row.expected[["type[expected]"]] <- "Expected count"

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

			if (options$percentagesRow) {

				row.row.proportions[["type[row.proportions]"]] <- " % within row"

				row.proportions <- as.list(row.proportions.matrix[i,])
				row.proportions <- .clean(row.proportions)
				names(row.proportions) <- paste(names(row.proportions),"[row.proportions]",  sep="")

				if (class(row.proportions.matrix[1,1]) == "character") {
					row.proportions[["total[row.proportions]"]] <- ""
				} else {
					row.proportions[["total[row.proportions]"]] <- .clean(base::sum(row.proportions.matrix[i,]))
				}

				row.proportions <- c(row.row.proportions, row.proportions)
				row <- c(row, row.proportions)
			}

			if (options$percentagesColumn) {

				row.col.proportions[["type[col.proportions]"]] <- " % within column"

				col.proportions <- as.list(col.proportions.matrix[i,])
				col.proportions <- .clean(col.proportions)
				names(col.proportions) <- paste(names(col.proportions),"[col.proportions]",  sep="")

				if (class(col.proportions.matrix[1,1]) == "character") {
					col.proportions[["total[col.proportions]"]] <- ""
				} else {

					row.sum <- base::margin.table(counts.matrix, 1)
					row.prop <- as.list( base::prop.table(row.sum))
					row.prop <- .clean(row.prop)
					col.proportions[["total[col.proportions]"]] <- row.prop[[i]]
				}

				col.proportions <- c(row.col.proportions, col.proportions)
				row <- c(row, col.proportions)
			}

			if (options$percentagesTotal) {

				row.proportions[["type[proportions]"]] <- " % of Total"

				proportions <- as.list(proportions.matrix[i,])
				proportions <- .clean(proportions)
				names(proportions) <- paste(names(proportions),"[proportions]",  sep="")

				if (class(proportions.matrix[1,1]) == "character") {
					proportions[["total[proportions]"]] <- ""
				} else {
					proportions[["total[proportions]"]] <- .clean(base::sum(proportions.matrix[i,]))
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

		if (i == 1 && options$countsExpected == FALSE && options$percentagesRow == FALSE && options$percentagesCol == FALSE && options$percentagesTotal == FALSE) {

			row[[".isNewGroup"]] <- TRUE
		}

		rows[[length(rows)+1]] <- row
	}


	if (perform == "run" && status$error == FALSE && status$ready) {

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

		if (options$percentagesRow) {

			if (class(row.proportions.matrix[1,1]) == "character") {
				row.proportions <- row.proportions.matrix[1,]
			} else {
				m <- base::margin.table(counts.matrix, 2)
				rowproportion <- base::prop.table(m)
			}

			row.proportions <- as.list(rowproportion)
			row.proportions <- .clean(row.proportions)
			names(row.proportions) <- paste(names(row.proportions),"[row.proportions]", sep="")

			if (class(row.proportions.matrix[1,1]) == "character") {
				row.proportions[["total[row.proportions]"]] <- ""
			} else {
				row.proportions[["total[row.proportions]"]] <- .clean(base::sum(rowproportion))
			}

			row.proportions<-c(row.row.proportions, row.proportions)

			row <- c(row,  row.proportions)
		}

		if (options$percentagesColumn) {

			if (class(col.proportions.matrix[1,1]) == "character") {
				col.proportions <- col.proportions.matrix[1,]
			} else {
				colproportion <- apply(col.proportions.matrix, 2, base::sum)
			}

			col.proportions <- as.list(colproportion)
			col.proportions <- .clean(col.proportions)
			names(col.proportions) <- paste(names(col.proportions),"[col.proportions]", sep="")

			if (class(row.proportions.matrix[1,1]) == "character") {
				col.proportions[["total[col.proportions]"]] <- ""
			} else {
				row.sum <- base::margin.table(counts.matrix, 1)
				row.prop <- base::prop.table(row.sum)
				col.proportions[["total[col.proportions]"]] <- .clean(base::sum(row.prop))
			}

			col.proportions<-c(row.col.proportions, col.proportions)

			row <- c(row,  col.proportions)
		}


		if (options$percentagesTotal) {

			if (class(proportions.matrix[1,1]) == "character") {
				proportions <- proportions.matrix[1,]
			} else {
				proportions <- apply(proportions.matrix, 2, base::sum)
			}

			proportions <- as.list(proportions)
			proportions <- .clean(proportions)
			names(proportions) <- paste(names(proportions),"[proportions]", sep="")

			if (class(proportions.matrix[1,1]) == "character") {
				proportions[["total[proportions]"]] <- ""
			} else {
				proportions[["total[proportions]"]] <- .clean(base::sum(proportions.matrix))
			}

			proportions <- c(row.proportions, proportions)

			row <- c(row,  proportions)
		}

	} else {

		row <- list()
	}

	row[[var.name]] <- "Total"
	if (options$countsExpected == FALSE && options$percentagesRow == FALSE && options$percentagesCol == FALSE && options$percentagesTotal == FALSE)
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


.crosstabsCreateGroupMatrices <- function(dataset, rows, columns, groups, counts=NULL, rowOrderDescending=FALSE,columnOrderDescending=FALSE, perform="run", status=NULL) {

	# this creates count matrices for each of the groups

	matrices <- list()

	if (is.null(groups)) {

		if (status$ready == FALSE || perform == "init") {

			row.levels <- c(".", ". ")
			col.levels <- c(".", ". ")

			if (rows != "")
				row.levels <- base::levels(dataset[[ .v(rows) ]])
			if (columns != "")
				col.levels <- base::levels(dataset[[ .v(columns) ]])

			ss.matrix <- base::matrix(0, nrow=length(row.levels), ncol=length(col.levels), dimnames=list(row.levels, col.levels))

		} else if (is.null(counts)) {

			ss.dataset <- base::subset(dataset, select=.v(c(rows, columns)))
			ss.table   <- base::table(ss.dataset)
			ss.matrix  <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))

		} else {

			ss.dataset <- base::subset(dataset, select=.v(c(rows, columns, counts)))
			ss.matrix  <- base::tapply(ss.dataset[[ .v(counts) ]], list(ss.dataset[[ .v(rows) ]], ss.dataset[[ .v(columns) ]]), base::sum)
			ss.matrix[is.na(ss.matrix)] <- 0
		}

		if (rowOrderDescending) {
			ss.matrix <- base::apply(ss.matrix, 2, base::rev)
		} else {
			ss.matrix <- ss.matrix
		}

		if (columnOrderDescending) {
			ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
		} else {
			ss.matrix <- ss.matrix
		}

		ss.matrix[base::is.na(ss.matrix)] <- 0

		matrices[[1]] <- ss.matrix

	} else {

		for (group in groups) {

			group <- group[group != ""]

			if (status$ready == FALSE) {

				# do nothing

			} else if (length(group) == 0) {

				ss.dataset <- base::subset(dataset, select=.v(c(rows, columns, counts)))

			} else {

				ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", sep="", collapse="&")
				ss.expression <- base::parse(text=ss.filter.string)
				ss.dataset	  <- base::subset(dataset, select=.v(c(rows, columns, counts)), subset=eval(ss.expression))
			}


			if (status$ready == FALSE) {

				ss.matrix <- base::matrix(c(0,0,0,0), nrow=2, ncol=2)

			} else if (is.null(counts)) {

				ss.table  <- base::table(ss.dataset)
				ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))

			} else {

				ss.matrix <- base::tapply(ss.dataset[[ .v(counts) ]], list(ss.dataset[[ .v(rows) ]], ss.dataset[[ .v(columns) ]]), base::sum)
			}

			ss.matrix[base::is.na(ss.matrix)] <- 0

			if (rowOrderDescending) {
				ss.matrix <- base::apply(ss.matrix, 2, base::rev)
			} else {
				ss.matrix <- ss.matrix
			}

			if (columnOrderDescending) {
				ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
			} else {
				ss.matrix <- ss.matrix
			}

			matrices[[length(matrices)+1]] <- ss.matrix
		}
	}

	matrices
}
