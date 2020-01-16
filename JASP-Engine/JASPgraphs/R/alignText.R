#' Align strings in two places
#'
#' @param leftSide string that should be left aligned
#' @param rightSide string that should be left aligned and concatenated with 
#' @param device a graphics device
#' @param ... further arguments passed to the device 
#'
#' @details Alignment is done by padding with tabs, so this does not work with plotmath. Consider using unicode instead of platmath.
#' @return a vector of strings
#'
#' @export
alignText <- function(leftSide, rightSide, device = NULL, ...) {
  
  if (length(leftSide) != length(rightSide))
    stop("length(leftSide) != length(rightSide)")

  if (is.null(device)) {
    # check if we're inside JASP, then check for jasptools, then fall back to png
    f <- get0("jaspResultsCalledFromJasp()")
    if (!is.null(f) && f())
      device <- get0("openGrDevice")
    else if (all(c("jasptools", "jaspResults") %in% loadedNamespaces()))
      device <- jaspResults::openGrDevice
    else
      device <- grDevices::png
  }

  if (!is.function(device))
    stop("device should be a function!")
  f <- tempfile()
  device(f, 320, 320)
  on.exit({
    dev.off()
    file.remove(f)
  })
  plot.new()
  tabLength  <- strwidth("\t")
  leftLength <- strwidth(leftSide)
  idx <- leftLength == max(leftLength) # not which.max so this includes equally long strings.
  leftSide[idx] <- paste0(leftSide[idx], "\t")
  noTabs <- ceiling((strwidth(leftSide[idx][1]) - leftLength[!idx]) / tabLength)
  leftSide[!idx] <- paste0(leftSide[!idx], strrep("\t", noTabs))

  return(paste0(leftSide, rightSide))
}