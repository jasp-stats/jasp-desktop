customContrasts <- function(jaspResults, dataset, options)
{
	if(is.null(jaspResults[["CustomContrastsTable"]]))
		jaspResults[["CustomContrastsTable"]] <- createCustomConstrastsTable(options$customContrasts)
}

createCustomConstrastsTable <- function(customContrasts)
{
	chosenColumn <- "?"

	if(length(customContrasts) > 0)
		chosenColumn <- customContrasts[[1]]$colName

	table <- createJaspTable(title=chosenColumn, expectedRows=1, expectedColumns=length(customContrasts), dependencies='customContrasts')

	colNames <- sapply(customContrasts, function(column) 
	{
		name <- column$colLabel 
		table$addColumnInfo(name=name); 
		return(name) 
	} );

	colData <- lapply(
		customContrasts, 
		function(column) 
		{ 
			return(column$values)
		}
	);

	names(colData) <- colNames

	table$setData(colData)

	

	return(table);
}