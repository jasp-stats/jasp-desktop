  roundToPrecision <- function(x) {
    if (is.numeric(x))  signif(round(x, digits=4), digits=4)
    else                x
  }
  
  getTableMismatches <- function(testTable, lookupTable, nRows, nCells, cellNames) {
    
    fullValues <- unlist(testTable)
    testTable <- unlist(lapply(testTable, roundToPrecision))
    attr(testTable, "fullValues") <- fullValues
    attr(testTable, "cellNames") <- cellNames

    fullValues <- unlist(lookupTable)
    lookupTable <- unlist(lapply(lookupTable, roundToPrecision))
    names(lookupTable) <- fullValues
    
    errors <- character(0)
    for (row in 1:nRows) {
      cellRange <- (1 + (row - 1) * nCells):(row * nCells)
      lookupRow <- lookupTable[cellRange]
      for (cell in cellRange) {
        indicesMatch <- which(lookupRow %in% testTable[cell])
        if (length(indicesMatch) > 0)
          lookupRow <- lookupRow[-min(indicesMatch)]
        else
          errors <- c(errors, 
                      paste0("New table value `", attr(testTable, "fullValues")[cell], 
                             "` (col `", attr(testTable, "cellNames")[cell], "`, row ", row, ")",
                             " does not exist in old table"))
      }
      if (length(lookupRow) == 1)
        errors <- c(errors, 
                    paste0("Old table value `", paste0(names(lookupRow), collapse="`, `"),
                           "` does not exist in new table"))
      else if (length(lookupRow) > 1)
        errors <- c(errors, 
                    paste0("Old table values `", paste0(names(lookupRow), collapse="`, `"),
                           "` do not exist in new table"))
    }
    return(errors)
  }

expect_equal_tables <- function(test, ref, label=NULL) {
  if (length(test) == 0) {
    errorMsg <- jasptools:::.getErrorMsgFromLastResults()
    if (! is.null(errorMsg))
      stop(paste("Tried retrieving table data from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
    else
      stop("The new table has no data. Check your unit test", call.=FALSE)
  }
  
  if (is.null(label))
    label <- "New table"

  nRows <- length(test)
  nCells <- length(test[[1]])
  cellNames <- names(unlist(test))
  test <- jasptools:::collapseTable(test)

  if (length(test) == length(ref)) {
    mismatches <- getTableMismatches(test, ref, nRows, nCells, cellNames)
    expect(length(mismatches) == 0, paste0(label, " is not equal to old table:\n", paste0(mismatches, collapse="\n")))
  } else {
    cellDiff <- abs(length(ref) - length(test))
    type <- "cells (possibly footnotes?)"
    if (cellDiff %% nRows == 0) type <- "columns"
    else if (cellDiff %% nCells == 0) type <- "rows"
    
    if (length(test) > length(ref))
      reason <- paste("likely reason: there are one or more new", type ,"in the new table that were not in the old table")
    else
      reason <- paste("likely reason: one or more", type, "are no longer in the new table that were in the old table")
      
    expect(FALSE, paste0(label, " (# cells: ", length(test), ") and old table (# cells: ", length(ref), ") are not of equal length, ", reason))
  }
}