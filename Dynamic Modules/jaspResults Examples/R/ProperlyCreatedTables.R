ProperlyCreatedTables <- function(jaspResults, dataset, options)
{
  ready <- options$weAreReady
  
  .properTable(jaspResults, options, ready)
}

.properTable <- function(jaspResults, options, ready) {
  # first check if we actually want the table, or if it was previously computed
  if (!options$weWantThisTable || !is.null(jaspResults[["table"]]))
    return()
  
  # now create the jaspTable object
  table <- createJaspTable(title="This is our table")
  
  # set dependencies
  table$dependOn(c("weWantThisTable", "weAreReady"))
  
  # set citations
  table$addCitation("This is a citation")
  
  # fix it's position in the output
  table$position <- 1
  
  # set the column specification
  table$addColumnInfo(name="col1", title="First column")
  table$addColumnInfo(name="col2", title="Second column")
  table$addColumnInfo(name="col3", title="Third column")
  table$addColumnInfo(name="col4", title="Fourth column")
  table$addColumnInfo(name="col5", title="Fifth column")
  
  # now we assign the table to jaspResults
  jaspResults[["table"]] <- table
  
  # check if we actually want to calculate anything or if we're done all together
  if (!ready)
    return()
  
  # now in a subfunction we'll go ahead and fill the table up;
  # note that we only have to supply table; it's pass-by-reference, so any change to table is reflected in jaspResults
  .fillProperTable(table)
  
  # done
  return()
}

.fillProperTable <- function(table) {
  for (i in 1:5) {
    table$addRows(list(col1=i, col2=i, col3=i, col4=i, col5=i))
  }
}