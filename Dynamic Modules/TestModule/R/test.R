testFunc <- function(jaspResults, dataset, options)
{
	
  tab <- createJaspTable("tab")
  foo <- sapply(letters[1:3], function(col) paste(letters[1:3], col))
  rownames(foo) <- colnames(foo) <- letters[1:3]
  
  tab$setData(foo)
  
  tab$addFootnote(message = "bla", rowNames = "b", colNames = "a")
  tab$addFootnote(message = "bla", rowNames = "c", colNames = "a")
  tab$addFootnote(message = "bla", rowNames = "c", colNames = "b")
  jaspResults[['tab']] <- tab
}
