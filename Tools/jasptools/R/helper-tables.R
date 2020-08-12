roundToPrecision <- function(x) {
  if (is.numeric(x))  signif(round(x, digits=4), digits=4)
  else                x
}

isUnicodeMismatch <- function(mismatch) {
  grepl("<unicode>", mismatch, fixed=TRUE)
}

excludeUnicodeMismatches <- function(mismatches) {
  indicesToRemove <- NULL
  for (i in seq_along(mismatches))
    if (isUnicodeMismatch(mismatches[i]))
      indicesToRemove <- c(indicesToRemove, i)
  
  if (!is.null(indicesToRemove))
    return(mismatches[-indicesToRemove])
  
  return(mismatches)
}

getMismatchesEqualSizeTables <- function(test, ref, nRows, nCols, cellNames) {
  testVec <- tableListToAnnotatedCharacterVector(test, cellNames)
  refVec <- tableListToAnnotatedCharacterVector(ref)
  
  mismatches <- character(0)
  for (row in 1:nRows) {
    
    cellRange <- (1 + (row - 1) * nCols):(row * nCols)
    lookupRow <- refVec[cellRange]
    
    for (cell in cellRange) {
      indicesMatch <- which(lookupRow %in% testVec[cell])
      if (length(indicesMatch) > 0)
        lookupRow <- lookupRow[-min(indicesMatch)]
      else {
        if (isUnicodeMismatch(names(testVec)[cell]))
          next
        
        mismatches <- c(mismatches, 
                        paste0("New table value `", names(testVec)[cell], 
                                "` (col `", attr(testVec, "cellNames")[cell], "`, row ", row, ")",
                                " does not exist in old table"))
      }
    }
    
    if (length(lookupRow) == 1) {
      if (isUnicodeMismatch(names(lookupRow)))
        next
      
      mismatches <- c(mismatches, 
                      paste0("Old table value `", names(lookupRow),
                              "` does not exist in new table"))
    }
    else if (length(lookupRow) > 1) {
      lookupRow <- excludeUnicodeMismatches(lookupRow)
      if (length(lookupRow) == 0)
        next
      
      mismatches <- c(mismatches, 
                      paste0("Old table values `", paste0(names(lookupRow), collapse="`, `"),
                              "` do not exist in new table"))
    }
  
  }
  
  return(paste(mismatches, collapse="\n"))
}
  
getMissingValuesDiffSizeTables <- function(test, ref, cellNames) {
  testVec <- tableListToAnnotatedCharacterVector(test, cellNames)
  refVec <- tableListToAnnotatedCharacterVector(ref)
  
  if (length(testVec) > length(refVec)) {
    searchFor <- testVec
    searchIn <- refVec
  } else {
    searchFor <- refVec
    searchIn <- testVec
  }
  
  missingValues <- character(0)
  for (i in seq_along(searchFor)) {
    
    if (!searchFor[i] %in% searchIn) {
      
      missingValue <- paste0("`", names(searchFor)[i], "`")
      col <- attr(searchFor, "cellNames")[i]
      if (!is.null(col))
        missingValue <- paste0(missingValue, " (col `", col, "`)")
      missingValues <- c(missingValues, missingValue)
      
    } else {
      searchIn <- searchIn[-min(which(searchIn %in% searchFor[i]))]
    }
    
  }
  
  return(paste(missingValues, collapse=", "))
}

tableListToAnnotatedCharacterVector <- function(tableList, cellNames=NULL) {
    fullValues <- unlist(tableList)
    tableVec <- as.character(unlist(lapply(tableList, roundToPrecision)))
    names(tableVec) <- fullValues
    attr(tableVec, "cellNames") <- cellNames
    
    return(tableVec)
}

expect_equal_tables <- function(test, ref, label=NULL) {
  if (is.null(label))
    label <- "New table"
    
  errorMsg <- jaspTools:::.getErrorMsgFromLastResults()
  if (!is.null(errorMsg))
    stop(paste("Tried retrieving table from results, but last run of jasptools exited with an error:\n", errorMsg), call.=FALSE)
    
  if (length(test) == 0)
      stop(paste(label, "has no data. Please check your unit test; is the index path to the table specified correctly?"), call.=FALSE)

  nRows <- length(test)
  nCols <- length(test[[1]])
  cellNames <- names(unlist(test))
  test <- jaspTools:::collapseTable(test)

  if (length(test) == length(ref)) {
    mismatches <- getMismatchesEqualSizeTables(test, ref, nRows, nCols, cellNames)
    expect(mismatches == "", paste0(label, " is not equal to old table:\n", mismatches))
  } else {
    missingValues <- getMissingValuesDiffSizeTables(test, ref, cellNames)
    
    cellDiff <- abs(length(ref) - length(test))
    type <- "cells (possibly footnotes, .isNewGroup, etc?)"
    if (cellDiff %% nRows == 0) type <- "columns"
    else if (cellDiff %% nCols == 0) type <- "rows"

    if (length(test) > length(ref))
      reason <- paste0("likely reason: there are one or more new ", type , " in the new table that were not in the old table.\n",
                       "New table values that are not matched: ", missingValues)
    else
      reason <- paste0("likely reason: one or more ", type, " are no longer in the new table that were in the old table.\n",
                       "Old table values that are not matched: ", missingValues)

    expect(FALSE, paste0(label, " (# cells: ", length(test), ") and old table (# cells: ", length(ref), ") are not of equal length, ", reason))
  }
}