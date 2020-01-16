#' Align strings in two places
#'
#' @param leftSide string that should be left aligned
#' @param rightSide string that should be left aligned and concatenated with 
#' @param prependBy a string to prependBy
#' @param asExpression if TRUE, then the returned string is suitabled to be parsed as expression.
#' @param device a graphics device
#' @param ... further arguments passed to the device.
#'
#' @return a vector of strings
#'
#' @export
alignText <- function(leftSide, rightSide, prependBy = NULL, asExpression = FALSE, device = NULL, ...) {
  
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
  if (asExpression) {
    strwidth(expression(" "))
    strwidth(expression(""))
    
  } else {
    spaceLength  <- strwidth(" ")
    leftLength <- strwidth(leftSide)
    idx <- leftLength == max(leftLength) # not which.max so this includes equally long strings.
    leftSide[idx] <- paste0(leftSide[idx], " ")
    noTabs <- ceiling((strwidth(leftSide[idx][1]) - leftLength[!idx]) / spaceLength)
    leftSide[!idx] <- paste0(leftSide[!idx], strrep(" ", noTabs))
  }

  return(paste0(leftSide, rightSide))
}