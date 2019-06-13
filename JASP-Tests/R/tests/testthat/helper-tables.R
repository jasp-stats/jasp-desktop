  roundToPrecision <- function(x) {
    if (is.numeric(x))  signif(round(x, digits=4), digits=4)
    else                x
  }
  
  getTableMismatches <- function(testTable, lookupTable, nRows, nCells) {
    
    namesTestTable <- unlist(testTable)
    testTable <- unlist(lapply(testTable, roundToPrecision))
    names(testTable) <- namesTestTable
    
    namesLookupTable <- unlist(lookupTable)
    lookupTable <- unlist(lapply(lookupTable, roundToPrecision))
    names(lookupTable) <- namesLookupTable
    
    errors <- character(0)
    for (row in 1:nRows) {
      cellRange <- (1 + (row - 1) * nCells):(row * nCells)
      lookupRow <- lookupTable[cellRange]
      for (cell in cellRange) {
        indicesMatch <- which(lookupRow %in% testTable[cell])
        if (length(indicesMatch) > 0)
          lookupRow <- lookupRow[-min(indicesMatch)]
        else
          errors <- c(errors, paste0("Value `", names(testTable)[cell], "` in test table (row ", row, ")", " cannot be located in reference table"))
      }
      if (length(lookupRow) > 0)
        errors <- c(errors, paste0("Value(s) `", paste0(names(lookupRow), collapse="`, `"), "` not matched in reference table"))
    }
    return(errors)
  }

expect_equal_tables <- function(test, ref, label=NULL) {
  if (length(test) == 0) {
    errorMsg <- jasptools:::.getErrorMsgFromLastResults()
    if (! is.null(errorMsg))
        stop(paste("Tried retrieving table data from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
  }
  
  if (is.null(label))
    label <- "Test table"
  
  nRows <- length(test)
  nCells <- length(test[[1]])
  
  test <- jasptools:::collapseTable(test)

  if (length(test) == length(ref)) {
    mismatches <- getTableMismatches(test, ref, nRows, nCells)
    expect(length(mismatches) == 0, paste0(label, " is not equal to reference table:\n", paste0(mismatches, collapse="\n")))
  } else {
    expect(FALSE, paste(label, "and reference table are not of equal length, check if the number of columns/rows is still the same"))
  }
}