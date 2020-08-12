expect_equal_plots <- function(test, name, dir) {
  errorMsg <- jaspTools:::.getErrorMsgFromLastResults()
  if (!is.null(errorMsg))
    stop(paste("Tried retrieving plot from results, but last run of jasptools exited with an error:\n", errorMsg), call.=FALSE)
    
  if (length(test) == 0)
      stop("The new plot has no data. Please check your unit test; is the index path to the plot specified correctly?", call.=FALSE)

  skip_if_grob(test)
  skip_if_recordedPlot(test)

  if (inherits(test, "JASPgraphsPlot")) {
    subplots <- test$subplots

    for (i in seq_along(subplots))
      vdiffr::expect_doppelganger(paste(dir, name, "subplot", i, sep="-"), subplots[[i]], path=dir)

  } else {
    if (inherits(test, "qgraph")) {
      qq <- test
      test <- function() plot(qq)
    }
    suppressWarnings(vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir))
  }
}

skip_if_grob <- function(test) {
  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")
}

skip_if_recordedPlot <- function(test) {
  if (inherits(test, "recordedplot"))
    skip("Recorded plots are skipped until the scaling of these plots is fixed")
}
