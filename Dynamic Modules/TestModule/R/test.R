testFunc <- function(jaspResults, dataset, options)
{
	
	# the correlation table stored here
	mainTable <- createJaspTable(title = "Transposed Table")
	mainTable$showSpecifiedColumnsOnly <- TRUE

	# I guess we want the transpose
	mainTable$transpose <- TRUE

	# but we don't want overtitles over the "displayed" columns
	#mainTable$transposeWithOvertitle <- FALSE

	# the displayed columns are supposed to be just the number of variable
	mainTable$addColumnInfo(name = "var", title = "", type = "string", overtitle="Variables")

	# now we need to define the "displayed" rows; ideally defining columns with overtitles
	whichtests <- c(TRUE, TRUE, TRUE)#options$pearson, options$spearman, options$kendallsTauB)
	testsTitles <- c("Pearson's rho", "Spearman's r", "Kendall's Tau B")[whichtests]
	tests <- c("pearson", "spearman", "kendall")[whichtests]

	vars  <- options$variables
	vvars <- vars # inside just, this will be actually .v(vars)

	for (vi in seq_along(vvars)){
	  for(ti in seq_along(tests)){
		    mainTable$addColumnInfo(
          name      = paste(vvars[vi], tests[ti], "r", sep = "."),
					title     = testsTitles[ti], type = "number",
					overtitle = vars[vi]) # this overtitle should appear as a first column


	#if(options$reportSignificance)
		mainTable$addColumnInfo(name = paste(vvars[vi], tests[ti], "p.value", sep = "."),
								title = "p-value", type = "pvalue",
								overtitle = vars[vi]) # this overtitle should appear as a first column

	# here will be more option-dependent stuff, but this should be enough as an example
	}
	}

	# the displayed columns will show the variable names
	mainTable$addColumns(list(var = vars))

	# everything else is passed to the table with the computed results...
	# fake example
	mainTable$addColumns(list(contNormal.pearson.r = c(0.2, 0.3, 0.4), contNormal.pearson.p.value = c(0.1, 0.034, 0.001)))

	#mainTable$print()
	jaspResults[['transposeMe']] <- mainTable
}
