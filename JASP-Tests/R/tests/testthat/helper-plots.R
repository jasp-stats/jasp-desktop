expect_equal_plots <- function(test, name, dir) {
  errorMsg <- jasptools:::.getErrorMsgFromLastResults()
  if (!is.null(errorMsg))
    stop(paste("Tried retrieving plot from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
    
  if (length(test) == 0)
      stop("The new plot has no data. Please check your unit test; is the index path to the plot specified correctly?", call.=FALSE)

  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")
  
  if (inherits(test, "qgraph")) {
    qq <- test
    test <- function() plot(qq)
  }
  
  vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir)
}
