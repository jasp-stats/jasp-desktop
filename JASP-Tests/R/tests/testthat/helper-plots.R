expect_equal_plots <- function(test, name, dir) {
  errorMsg <- jasptools:::.getErrorMsgFromLastResults()
  if (!is.null(errorMsg))
    stop(paste("Tried retrieving plot from results, but last run of jasptools exited with an error:", errorMsg), call.=FALSE)
    
  if (length(test) == 0)
      stop("The new plot has no data. Please check your unit test; is the index path to the plot specified correctly?", call.=FALSE)

  skip_if_grob(test)

  if (inherits(test, "JASPgraphsPlot")) {
    subplots <- test$subplots
    nms <- names(subplots)
    if (is.null(nms))
      nms <- seq_along(subplots)

    for (i in seq_along(subplots)) {
      skip_if_grob(subplots[[i]]) # This means if one out of 20 plots is a grob, we skip the test!
      vdiffr::expect_doppelganger(paste(dir, name, "subplot", nms[i], sep="-"), subplots[[i]], path=dir)
    }

  } else {
    vdiffr::expect_doppelganger(paste(dir, name, sep="-"), test, path=dir)
  }
}

skip_if_grob <- function(test) {
  if (inherits(test, "grob"))
    skip("Cannot reliably test matrix plots (they fail Windows <-> OSX)")
}
