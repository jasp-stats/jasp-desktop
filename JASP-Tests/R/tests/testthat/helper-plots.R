expect_equal_plots <- function(test, name, dir) {
  if (length(test) == 0) {
    errorMsg <- jasptools:::.getErrorMsgFromLastResults()
    if (! is.null(errorMsg))
        stop(paste("Tried retrieving plot from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
  }

  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")

  vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir)
}
