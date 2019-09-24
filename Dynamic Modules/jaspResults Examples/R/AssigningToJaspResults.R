AssigningToJaspResults <- function(jaspResults, dataset, options)
{
  # jaspResults returns its results every X ms, given that it received something new
  # so we should ensure that whatever gets send back is presentable
  
  .poorlyTimedAssignment(jaspResults, options)
  .wellTimedAssignment(jaspResults, options)
}

.poorlyTimedAssignment <- function(jaspResults, options) {
  if (!options$poorTable || !is.null(jaspResults[["poorTable"]])) return()
  
  table <- createJaspTable(title="This table could improve");
  # at this point jaspResults will learn about the table, although it is not properly formatted
  jaspResults[['poorTable']] <- table
  # and if jaspResults happens to emit output it will not look very nice (we fake this by returning)
  return()
  
  table$dependOn("poorTable")
  table$addCitation("A citation")
  
  table$addColumnInfo(name="col1", title="First column")
  table$addColumnInfo(name="col2", title="Second column")
  table$addColumnInfo(name="col3", title="Third column")
  table$addColumnInfo(name="col4", title="Fourth column")
  table$addColumnInfo(name="col5", title="Fifth column")
}

.wellTimedAssignment <- function(jaspResults, options) {
  if (!options$goodTable || !is.null(jaspResults[["goodTable"]])) return()
  
  table <- createJaspTable(title="I am an optimal table");
  
  table$dependOn("goodTable")
  table$addCitation("A citation")
  
  table$addColumnInfo(name="col1", title="First column")
  table$addColumnInfo(name="col2", title="Second column")
  table$addColumnInfo(name="col3", title="Third column")
  table$addColumnInfo(name="col4", title="Fourth column")
  table$addColumnInfo(name="col5", title="Fifth column")
  
  # at this point jaspResults will learn about the table, and it is properly formatted
  jaspResults[['goodTable']] <- table
  # and if jaspResults happens to emit output it will look ok (we fake this by returning)
  return()
}