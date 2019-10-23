testFootnotesTableFunc <- function(jaspResults, dataset, options)
{
  jaspResults[['table0']] <- createFootnotesTable(options=options)
  jaspResults[['table2']] <- createFootnotesTable(options=options, notUnique=TRUE)
  #jaspResults[['table1']] <- createFootnotesTable(options=options, rowNames=c("b", "c", "a"), colNames=c("c", "a", "c"))
}

createFootnotesTable <- function(numFootnotes=3, rowNames=c("b", "c", "c"), colNames=c("a", "a", "b"), options, notUnique=FALSE)
{
  foo <- sapply(letters[21:23], function(col) paste(letters[24:26], col))
  rownames(foo) <- colnames(foo) <- letters[1:3]
  
  tableFootnotes <- createJaspTable(paste0("Table with Footnotes at rowNames=(", paste0(rowNames, collapse=", "), ") and colNames=(", paste0(colNames, collapse=", "), ")", collapse=""), foo, dependencies="checkbox_0")

  msgNum <- 0

  while(msgNum < numFootnotes)
  {
    msgNum <- msgNum + 1

    if(options$checkbox_0 == notUnique)       tableFootnotes$addFootnote(message = paste0("msg", as.character(msgNum)), rowNames = rowNames[[msgNum]], colNames = colNames[[msgNum]])
    else if(options$checkbox_1 != notUnique)  tableFootnotes$addFootnote(message = "Identical Twins!",                  rowNames = rowNames[[msgNum]], colNames = colNames[[msgNum]])
    else                                      tableFootnotes$addFootnote(message = "Identical Symbols!",                  rowNames = rowNames[[msgNum]], colNames = colNames[[msgNum]], symbol="?")
  }

  return(tableFootnotes);
}