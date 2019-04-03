expect_equal_tables <- function(test, ref, ...) {
  if (length(test) == 0) {
    errorMsg <- jasptools:::.getErrorMsgFromLastResults()
    if (! is.null(errorMsg))
        stop(paste("Tried retrieving table data from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
  }
  test <- jasptools:::collapseTable(test)
  expect_equal(test, ref, tolerance=1e-4, ...)
}
