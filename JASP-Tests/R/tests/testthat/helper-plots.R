expect_equal_plots <- function(test, name, dir) {
  if (length(test) == 0) {
    errorMsg <- jasptools:::.getErrorMsgFromLastResults()
    if (! is.null(errorMsg))
        stop(paste("Tried retrieving plot from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
  }
  vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir)
}
